# This file includes a manual implementation of the divide-by-constant optimization for
# non-power-of-2 divisors. LLVM automatically includes this optimization, but only for
# smaller integer sizes (<=64-bits on my 64-bit machine).
#
# NOTE: We use LLVM's built-in implementation for Int64 and smaller, to keep the code
# simpler (though the code we produce is identical, verified in tests.) We apply this
# optimization to (U)Int128 and (U)Int256, which result from multiplying FD{Int64}s and
# FD{Int128}s.
# Before:
# julia> @btime for _ in 1:10000 fd = fd * fd end setup = (fd = FixedDecimal{Int32,3}(1.234))
#     54.125 μs (0 allocations: 0 bytes)   # (Unchanged)
#   FixedDecimal{Int32,3}(1700943.280)
#
# julia> @btime for _ in 1:10000 fd = fd * fd end setup = (fd = FixedDecimal{Int64,3}(1.234))
#     174.625 μs (0 allocations: 0 bytes)
#   FixedDecimal{Int64,3}(4230510070790917.029)
#
# julia> @btime for _ in 1:10000 fd = fd * fd end setup = (fd = FixedDecimal{Int128,3}(1.234))
#     2.119 ms (79986 allocations: 1.60 MiB)
#   FixedDecimal{Int128,3}(-66726338547984585007169386718143307.324)
#
# After:
# julia> @btime for _ in 1:10000 fd = fd * fd end setup = (fd = FixedDecimal{Int32,3}(1.234))
#     56.958 μs (0 allocations: 0 bytes)   # (Unchanged)
#   FixedDecimal{Int32,3}(1700943.280)
#
# julia> @btime for _ in 1:10000 fd = fd * fd end setup = (fd = FixedDecimal{Int64,3}(1.234))
#     90.708 μs (0 allocations: 0 bytes)
#   FixedDecimal{Int64,3}(4230510070790917.029)
#
# julia> @btime for _ in 1:10000 fd = fd * fd end setup = (fd = FixedDecimal{Int128,3}(1.234))
#     180.167 μs (0 allocations: 0 bytes)
#   FixedDecimal{Int128,3}(-66726338547984585007169386718143307.324)

"""
    ShouldUseCustomFldmodByConst(::Type{<:MyCustomIntType})) = true
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
# The implementation for fld_by_const was lifted directly from Base.fld(x,y), except that
# it uses `div_by_const` instead of `div`.
fld_by_const(x::T, y::Val{C}) where {T<:Unsigned, C} = div_by_const(x, y)
function fld_by_const(x::T, y::Val{C}) where {T<:Signed, C}
    d = div_by_const(x, y)
    return d - (signbit(x ⊻ C) & (d * C != x))
end

# Unsigned magic number computation + shift by constant
# See Hacker's delight, equations (26) and (27) from Chapter 10-9.
# (See also the errata on https://web.archive.org/web/20190915025154/http://www.hackersdelight.org/)
# requires nmax >= divisor > 2. divisor must not be a power of 2.
Base.@assume_effects :foldable function magicg(nmax::Unsigned, divisor)
    T = typeof(nmax)
    W = _widen(T)
    d = W(divisor)

    nc = div(W(nmax) + W(1), d) * d - W(1)      # largest multiple of d <= nmax, minus 1
    nbits = 8sizeof(nmax) - leading_zeros(nmax) # most significant bit
    # shift must be larger than int size because we want the high bits of the wide multiplication
    for p in nbits-1:2nbits-1
        if W(2)^p > nc * (d - W(1) - rem(W(2)^p - W(1), d))       # (27)
            m = div(W(2)^p + d - W(1) - rem(W(2)^p - W(1), d), d) # (26)
            return (m, p)
        end
    end
    return W(0), 0 # Should never reach here
end

# See Hacker's delight, equations (5) and (6) from Chapter 10-4.
# (See also the errata on https://web.archive.org/web/20190915025154/http://www.hackersdelight.org/)
# requires nmax >= divisor > 2. divisor must not be a power of 2.
Base.@assume_effects :foldable function magicg(nmax::Signed, divisor)
    T = typeof(nmax)
    W = _widen(T)
    d = W(divisor)

    nc = div(W(nmax) + W(1), d) * d - W(1)      # largest multiple of d <= nmax, minus 1
    nbits = 8sizeof(nmax) - leading_zeros(nmax) # most significant bit
    # shift must be larger than int size because we want the high bits of the wide multiplication
    for p in nbits-1:2nbits-1
        if W(2)^p > nc * (d - rem(W(2)^p, d))       # (6)
            m = div(W(2)^p + d - rem(W(2)^p, d), d) # (5)
            return (m, p)
        end
    end
    return W(0), 0 # Should never reach here
end

function div_by_const(x::T, ::Val{C}) where {T, C}
    # These checks will be compiled away during specialization.
    # While for `*(FixedDecimal, FixedDecimal)`, C will always be a power of 10, these
    # checks allow this function to work for any `C > 0`, in case that's useful in the
    # future.
    if C == 1
        return x
    elseif ispow2(C)
        return div(x, C) # Will already do the right thing
    elseif C <= 0
        throw(DomainError("C must be > 0"))
    end
    # Calculate the magic number and shift amount, based on Hacker's Delight, Chapter 10.
    magic_number, shift = magicg(typemax(T), C)

    out = _widemul(promote(x, magic_number)...)
    out >>= shift
    # Adding one as implied by formula (1b) in Hacker's delight, Chapter 10-4.
    return (out % T) + (x < zero(T))
end
