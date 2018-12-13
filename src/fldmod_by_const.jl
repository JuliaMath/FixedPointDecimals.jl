"""
    calculate_inv_coeff(::Type{T}, f)

Returns the magic numbers needed for `fldmod_by_const(x, Val(C))`: `2^N / C`, or the
`inv_coeff`. That `inv_coeff` has all leading zeros shifted away to maximize precision, so
this also returns how many bits need to be shifted back to undo that optimization, making
the return value: `((2^N / C) << toshift, toshift)`.
"""
# This function is marked `@pure` because it has no side-effects and depends on no global
# state. The value will never change for a given (type,precision) pair.
# This allows its result to be const-folded away when called with Const values.
Base.@pure function calculate_inv_coeff(::Type{T}, f) where {T}
    # First, calculate 2^nbits(T)/f
    # We calculate it at double-precision and shift away leading zeros to preserve the most
    # precision. Later, we will shift the final answer back to undo this operation.
    UT = unsigned(T)
    invcoeff = twoToTheSizeOf(UT)÷f
    toshift = _leading_zeros(invcoeff)
    invcoeff = invcoeff << toshift
    # Now, truncate the result to sizeof(T), rounding to maintain precision.
    invcoeff = _round_to_even(fldmod(invcoeff, typemax(UT))..., typemax(UT)) % T
    return invcoeff, toshift
end
# These are needed to handle Int128, which widens to BigInt, since BigInt doesn't have typemax
twoToTheSizeOf(::Type{T}) where {T} = typemax(widen(unsigned(T)))
twoToTheSizeOf(::Union{Type{Int128}, Type{UInt128}}) = BigInt(2)^256
twoToTheSizeOf(::Type{BigInt}) = BigInt(2)^256

# This special-purpose leading_zeros is needed to handle Int128, which widens to BigInt
_leading_zeros(x) = leading_zeros(x)
# BigInt doesn't have a concept of "leading zeros", but since we _know_ the value being
# passed here will fit in 256-bits (per twoToTheSizeOf), we can pretend this is a 256-bit
# integer, take just the upper half as a UInt128, and count the leading zeros there.
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

"""    nbits(x) = sizeof(x)*8 """
nbits(x) = sizeof(x)*8

# Implemenation based on umul32hi, from https://stackoverflow.com/a/22847373/751061
# /* compute the upper half of the widened product of two unsigned integers */
@inline function unsigned_splitmul_upper(a::T, b::T) where T<:Unsigned
    # /* split operands into halves */
    ah,al = splitint(a)
    bh,bl = splitint(b)
    halfT = typeof(ah)
    halfbits = nbits(al)
    # /* compute partial products */
    p0 = widemul(al, bl);
    p1 = widemul(al, bh);
    p2 = widemul(ah, bl);
    p3 = widemul(ah, bh);
    # /* sum partial products */
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
