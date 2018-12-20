"""
    calculate_inv_coeff(::Type{T}, C)

Returns the magic numbers needed for `fldmod_by_const(x, Val(C))`: `2^N / C`, or the
`inv_coeff`. That `inv_coeff` has all leading zeros shifted away to maximize precision, so
this also returns how many bits need to be shifted back to undo that optimization, making
the return value: `((2^N / C) << toshift, toshift)`.

Note that for a given `FixedDecimal{T,f}`, `C` will be `10^f`.

REQUIRES:
  - `C` must not be a power of 2.

# Examples
```julia
julia> calculate_inv_coeff(UInt, 100)
(0xa3d70a3d70a3d70b, 6)
```
"""
# This function is marked `@pure` because it has no side-effects and depends on no global
# state. The value will never change for a given (type,precision) pair.
# This allows its result to be const-folded away when called with Const values.
Base.@pure function calculate_inv_coeff(::Type{T}, C) where {T}
    # First, calculate 2^nbits(T)/C
    # We shift away leading zeros to preserve the most precision when we use it to multiply
    # in the next step. At the end, we will shift the final answer back to undo this
    # operation (which is why we need to return `toshift`).
    # Note, also, that we calculate invceoff at double-precision so that the left-shift
    # doesn't leave trailing zeros. We truncate to only the upper-half before returning.
    UT = unsigned(T)
    invcoeff = two_to_the_size_of(widen(UT))÷C
    toshift = _leading_zeros(invcoeff)
    invcoeff = invcoeff << toshift
    # Now, truncate to only the upper half of invcoeff, after we've shifted. Instead of
    # bitshifting, we round to maintain precision. (This is needed to prevent off-by-ones.)
    # -- This is equivalent to `invcoeff = T(invcoeff >> sizeof(T))`, except rounded. --
    invcoeff = _round_to_even(fldmod(invcoeff, typemax(UT))..., typemax(UT)) % T
    return invcoeff, toshift
end
# These are needed to handle Int128, which widens to BigInt, since BigInt doesn't have typemax
two_to_the_size_of(::Type{T}) where {T} = typemax(unsigned(T))
two_to_the_size_of(::Type{BigInt}) = BigInt(2)^256

# This special-purpose leading_zeros is needed to handle Int128, which widens to BigInt
_leading_zeros(x) = leading_zeros(x)
# BigInt doesn't have a concept of "leading zeros", but since we _know_ the value being
# passed here will fit in 256-bits (per two_to_the_size_of), we can pretend this is a
# 256-bit integer, take just the upper half as a UInt128, and count the leading zeros there.
_leading_zeros(x::BigInt) = leading_zeros((x >> 128) % UInt128)


# Splits `x` into `(hi,lo)` and returns them as the corresponding narrowed type
function splitint(x::T) where {T<:Integer}
    NT = narrow(T)
    (x >> nbits(NT)) % NT, x % NT
end

narrow(::Type{Int128}) = Int64
narrow(::Type{Int64}) = Int32
narrow(::Type{Int32}) = Int16
narrow(::Type{Int16}) = Int8
narrow(::Type{UInt128}) = UInt64
narrow(::Type{UInt64}) = UInt32
narrow(::Type{UInt32}) = UInt16
narrow(::Type{UInt16}) = UInt8

nbits(x) = sizeof(x)*8

# Implemenation based on umul32hi, from https://stackoverflow.com/a/22847373/751061
# Compute the upper half of the widened product of two unsigned integers.
# Example: `widemul(0x0020,0x2002) == 0x0004_0040` vs
#          `unsigned_splitmul_upper(0x0020,0x2002) == 0x0004`
@inline function unsigned_splitmul_upper(a::T, b::T) where T<:Unsigned
    # Split operands into halves
    ah,al = splitint(a)
    bh,bl = splitint(b)
    halfT = typeof(ah)
    halfbits = nbits(al)
    # Compute partial products
    p0 = widemul(al, bl);
    p1 = widemul(al, bh);
    p2 = widemul(ah, bl);
    p3 = widemul(ah, bh);
    # Sum partial products
    carry = ((p0 >> halfbits) + (p1%halfT) + (p2%halfT)) >> halfbits;
    return p3 + (p2 >> halfbits) + (p1 >> halfbits) + carry;
end
@inline function splitmul_upper(a::T, b::T) where T<:Unsigned
    return unsigned_splitmul_upper(a,b)
end
@inline function splitmul_upper(a::T, b::T) where T<:Signed
    uresult = unsigned_splitmul_upper(unsigned(a), unsigned(b))
    return signed(uresult) - ((a < 0) ? b : 0) - ((b < 0) ? a : 0)
end
@inline splitmul_upper(a,b) = splitmul_upper(promote(a,b)...)


"""
    div_by_const(x, Val(C))

The idea behind `div_by_const(x, Val(C))` is that we can avoid division entirely if
instead of dividing by `C`, we multiply by `(2^N/C)`, then "divide by" `2^N`.

And if we choose `N` to be `nbits(T)`, we can avoid the "divide by `2^N`" by just taking the
_upper half_ of the result of the multiplication. And so, to do the multiplication, we use
a `splitwidemul` which avoids widening by doing splitting x into upper and lower bits, doing
several smaller multiplies, and then returning the result as two Ts, for the up and lo.

REQUIRES:
  - `C` _must be_ greater than `0`
"""
function div_by_const(x::T, ::Val{C}) where {T, C}
# These checks will be compiled away during specialization.
    if C == 1
        return x
    elseif ispow2(C)
        return div(x,C)
    elseif (C <= 0)
        throw(DomainError("C must be > 0"))
    end
    inv_coeff, toshift = calculate_inv_coeff(T, C)
    up = splitmul_upper(x, inv_coeff)
    out = up    # By keeping only the upper half, we're essentially dividing by 2^nbits(T)
    # This condition will be compiled away during specialization.
    if T <: Signed
        # Because our magic number has a leading one, the result is negative if it's Signed.
        # We add x to give us the positive equivalent.
        out += x
        signshift = (nbits(x)-1)
        signed = T(unsigned(out) >> signshift)  # "unsigned" bitshift (to read top bit)
    end
    out = out >> toshift
    if T <: Signed
        out =  out + signed
    end
    return T(out)
end

"""
    fld_by_const(x, Val(C))

Like `div_by_const`, but for `fld`, not `div` -- fld rounds down for negative numbers.
"""
# This implementation was lifted directly from Base.fld(x,y)
fld_by_const(x::T, y::Val{C}) where {T<:Unsigned, C} = div_by_const(x,y)
function fld_by_const(x::T, y::Val{C}) where {T<:Integer, C}
    d = div_by_const(x,y)
    return d - (signbit(x ⊻ C) & (d * C != x))
end

"""
    manual_mod(x, y, quotient)

Calculate `mod(x,y)` after you've already acquired quotient, the result of `fld(x,y)`.

REQUIRES:
    - `y != -1`
"""
@inline function manual_mod(x::T, y::T, quotient::T) where T<:Integer
    return x - quotient * y
end

"""
    fldmod_by_const(x, Val(C))

Returns `fldmod(x,C)`, implemented efficiently when `C` is a (positive) static Const.

REQUIRES:
  - `C` must be greater than `0`
"""
function fldmod_by_const(x, y::Val{C}) where {C}
    d = fld_by_const(x, y)
    return d, manual_mod(promote(x, C, d)...)
end
