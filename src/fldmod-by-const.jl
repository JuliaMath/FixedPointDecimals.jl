# This file includes a manual implementation of the divide-by-constant optimization for
# non-power-of-2 divisors. LLVM automatically includes this optimization, but only for
# smaller integer sizes (<=64-bits on my 64-bit machine).
#
# NOTE: We use LLVM's built-in implementation for Int64 and smaller, to keep the code
# simpler (though the code we produce is identical). We apply this optimization to (U)Int128
# and (U)Int256, which result from multiplying FD{Int64}s and FD{Int128}s.
# Before:
# julia> using FixedPointDecimals, BenchmarkTools
#
# julia> @btime $(FixedDecimal{Int32,3}[rand(Float32) for _ in 1:10000]).^2;
#   25.750 μs (2 allocations: 39.17 KiB)
# 
# julia> @btime $(FixedDecimal{Int64,3}[rand(Float32) for _ in 1:10000]).^2;
#   92.750 μs (2 allocations: 78.17 KiB)
# 
# julia> @btime $(FixedDecimal{Int128,3}[rand(Float32) for _ in 1:10000]).^2;
#   1.892 ms (120698 allocations: 2.45 MiB)
#
# After:
# julia> using FixedPointDecimals, BenchmarkTools
#
# julia> @btime $(FixedDecimal{Int32,3}[rand(Float32) for _ in 1:10000]).^2;
#   16.250 μs (2 allocations: 39.17 KiB)
# 
# julia> @btime $(FixedDecimal{Int64,3}[rand(Float32) for _ in 1:10000]).^2;
#   35.375 μs (2 allocations: 78.17 KiB)
# 
# julia> @btime $(FixedDecimal{Int128,3}[rand(Float32) for _ in 1:10000]).^2;
#   138.917 μs (2 allocations: 156.30 KiB)


"""
    ShouldUseCustomFldmodByConst(::Type{<:MyCustomIntType}) = true
A trait to control opt-in for the custom `fldmod_by_const` implementation. To use this for a
given integer type, you can define this overload for your integer type.
You will also need to implement some parts of the interface below, including _widen().
"""
ShouldUseCustomFldmodByConst(::Type{<:Union{Int128,UInt128}}) = true  # For FD{Int64}
ShouldUseCustomFldmodByConst(::Type{<:Union{Int256,UInt256}}) = true  # For FD{Int128}
ShouldUseCustomFldmodByConst(::Type) = false

@inline function fldmod_by_const(x, y)
    if ShouldUseCustomFldmodByConst(typeof(x))
        # For large Int types, LLVM doesn't optimize well, so we use a custom implementation
        # of fldmod, which extends that optimization to those larger integer types.
        d = fld_by_const(x, Val(y))
        return d, (x - d * y)
    else
        # For other integers, LLVM might be able to correctly optimize away the division, if
        # it knows it's dividing by a const.
        # Since julia 1.8+, fldmod(x,y) automatically optimizes for constant divisors.
        return fldmod(x, y)
    end
end

# Calculate fld(x,y) when y is a Val constant.
# NOTE: This implementation is based on Hacker's Delight, Chapter 10, except that
# we are implementing fld(x, y), whereas that code implements `div(x, y)`. (i.e. we
# always floor-divide, rather than rounding towards zero.)
@inline function fld_by_const(x::T, ::Val{C}) where {T, C}
    # These checks will be compiled away during specialization.
    # While for `*(FixedDecimal, FixedDecimal)`, C will always be a power of 10, these
    # checks allow this function to work for any `C > 0`, in case that's useful in the
    # future.
    if C <= 0
        throw(DomainError(C, "C must be > 0"))
    elseif C == 1
        return x
    elseif ispow2(C)
        # NOTE: Power of 2 divisors must not reach the magic number path below.
        return x >> T(log2(C))
        #return fld(x, C)
    end
    # Calculate the magic number and shift amount, based on Hacker's Delight, Chapter 10.
    magic_number, shift = magicg(typemax(T), C)

    # Now, do a floor-division (shift implements fld, not div):
    wide_result = _widemul(promote(x, magic_number)...)
    result = (wide_result >> shift) % T

    # The raw shift correctly computes fld(x, C) for all x EXCEPT negative exact
    # multiples of C. The magic number m slightly overshoots 2^shift/C, so for
    # negative x, x*m/2^shift is slightly below x/C. When x/C is non-integer this
    # doesn't affect the floor, but when x/C IS an integer the floor drops by 1.
    # We detect this by checking the remainder: normally in [0,C), but exactly C
    # when off-by-one. (For unsigned types, x >= 0 always, so this is a no-op.)
    remainder = x - result * T(C)
    return result + T(remainder == T(C))
end

# Unsigned magic number computation + shift by constant
# See Hacker's delight, equations (26) and (27) from Chapter 10-9.
# (See also the errata on https://web.archive.org/web/20190915025154/http://www.hackersdelight.org/)
# requires nmax >= divisor > 2. divisor must not be a power of 2.
Base.@assume_effects :foldable function magicg(nmax::Unsigned, divisor)
    T = typeof(nmax)
    W = _widen(T)
    d = W(divisor)

    nc = div(W(nmax) + W(1), d) * d - W(1) # largest multiple of d <= nmax, minus 1
    nbits = 8sizeof(nmax)                  # most significant bit
    # shift must be larger than int size because we want the high bits of the wide multiplication
    for p in nbits:2nbits
        e = d - W(1) - rem(W(2)^p - W(1), d)
        if W(2)^p > nc * e         # (27)
            m = div(W(2)^p + e, d) # (26)
            return (m, p)
        end
    end
    _throw_magicg_unreachable(divisor, nmax)
end

# See Hacker's delight, equations (5) and (6) from Chapter 10-4.
# (See also the errata on https://web.archive.org/web/20190915025154/http://www.hackersdelight.org/)
# requires nmax >= divisor > 2. divisor must not be a power of 2.
Base.@assume_effects :foldable function magicg(nmax::Signed, divisor)
    T = typeof(nmax)
    W = _widen(T)
    d = W(divisor)

    nc = div(W(nmax) + W(1), d) * d - W(1) # largest multiple of d <= nmax, minus 1
    nbits = 8sizeof(nmax)                  # most significant bit
    # shift must be larger than int size because we want the high bits of the wide multiplication
    for p in nbits:2nbits
        e = d - rem(W(2)^p, d)
        if W(2)^p > nc * e         # (6)
            m = div(W(2)^p + e, d) # (5)
            return (m, p)
        end
    end
    _throw_magicg_unreachable(divisor, nmax)
end

@noinline function _throw_magicg_unreachable(divisor, nmax)
    error(lazy"""magicg bug: Unreachable reached. divisor=$divisor, nmax=$nmax.
        Please report an issue to https://github.com/JuliaMath/FixedPointDecimals.jl
        """)
end
