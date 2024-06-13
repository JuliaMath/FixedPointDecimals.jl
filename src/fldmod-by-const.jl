
const BitInteger256 = Union{UInt256, Int256}

@inline function fldmod_by_const(x, y)
    # For small-to-normal integers, LLVM can correctly optimize away the division, if it
    # knows it's dividing by a const. We cannot call `Base.fldmod` since it's not
    # inlined, so here we have explictly inlined it instead.
    return (fld(x,y), mod(x,y))
end
@inline function fldmod_by_const(x::BitInteger256, y)
    # For large or non-standard Int types, LLVM doesn't optimize
    # well, so we use a custom implementation of fldmod.
    d = fld_by_const(x, Val(y))
    return d, manual_mod(promote(x, y, d)...)
end

# Calculate fld(x,y) when y is a Val constant.
# The implementation for fld_by_const was lifted directly from Base.fld(x,y), except that
# it uses `div_by_const` instead of `div`.
fld_by_const(x::T, y::Val{C}) where {T<:UInt256, C} = div_by_const(x, y)
function fld_by_const(x::T, y::Val{C}) where {T<:Int256, C}
    d = div_by_const(x, y)
    return d - (signbit(x ⊻ C) & (d * C != x))
end

# Calculate `mod(x,y)` after you've already acquired quotient, the result of `fld(x,y)`.
# REQUIRES:
#   - `y != -1`
@inline function manual_mod(x::T, y::T, quotient::T) where T<:BitInteger256
    return x - quotient * y
end

function div_by_const(x::T, ::Val{C}) where {T, C}
    # These checks will be compiled away during specialization.
    # While for `*(FixedDecimal, FixedDecimal)`, C will always be a power of 10, these
    # checks allow this function to work for any `C > 0`, in case that's useful in the
    # future.
    if C == 1
        return x
    elseif ispow2(C)
        return div(x, C)  # Will already do the right thing
    elseif C <= 0
        throw(DomainError("C must be > 0"))
    end
    # Calculate the magic number 2^N/C. Note that this is computed statically, not at
    # runtime.
    inverse_coeff, toshift = calculate_inverse_coeff(T, C)
    # Compute the upper-half of widemul(x, 2^nbits(T)/C).
    # By keeping only the upper half, we're essentially dividing by 2^nbits(T), undoing the
    # numerator of the multiplication, so that the result is equal to x/C.
    out = mul_hi(x, inverse_coeff)
    # This condition will be compiled away during specialization.
    if T <: Signed
        # Because our magic number has a leading one (since we shift all-the-way left), the
        # result is negative if it's Signed. We add x to give us the positive equivalent.
        out += x
        signshift = (nbits(x) - 1)
        isnegative = T(out >>> signshift)  # 1 if < 0 else 0 (Unsigned bitshift to read top bit)
    end
    # Undo the bitshifts used to calculate the invoeff magic number with maximum precision.
    out = out >> toshift
    if T <: Signed
        out =  out + isnegative
    end
    return T(out)
end

Base.@assume_effects :foldable function calculate_inverse_coeff(::Type{T}, C) where {T}
    # First, calculate 2^nbits(T)/C
    # We shift away leading zeros to preserve the most precision when we use it to multiply
    # in the next step. At the end, we will shift the final answer back to undo this
    # operation (which is why we need to return `toshift`).
    # Note, also, that we calculate invcoeff at double-precision so that the left-shift
    # doesn't leave trailing zeros. We truncate to only the upper-half before returning.
    UT = _unsigned(T)
    invcoeff = typemax(_widen(UT)) ÷ C
    toshift = leading_zeros(invcoeff)
    invcoeff = invcoeff << toshift
    # Now, truncate to only the upper half of invcoeff, after we've shifted. Instead of
    # bitshifting, we round to maintain precision. (This is needed to prevent off-by-ones.)
    # -- This is equivalent to `invcoeff = T(invcoeff >> sizeof(T))`, except rounded. --
    invcoeff = _round_to_nearest(fldmod(invcoeff, typemax(UT))..., typemax(UT)) % T
    return invcoeff, toshift
end

function mul_hi(x::T, y::T) where T
    xy = _widemul(x, y)  # support Int256 -> Int512 (!!)
    (xy >> nbits(T)) % T
end

# Annoyingly, Unsigned(T) isn't defined for BitIntegers types:
# https://github.com/rfourquet/BitIntegers.jl/pull/2
_unsigned(x) = unsigned(x)
_unsigned(::Type{Int256}) = UInt256

nbits(x) = sizeof(x) * 8
