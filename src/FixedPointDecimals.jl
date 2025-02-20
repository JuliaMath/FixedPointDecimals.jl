# This file partially based off JuliaMath/FixedPointNumbers.jl
#
# Copyright (c) 2014: Jeff Bezanson and other contributors.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

__precompile__()

module FixedPointDecimals

using Base: checked_abs, checked_add, checked_cld, checked_div, checked_fld,
    checked_mod, checked_mul, checked_neg, checked_rem, checked_sub

export FixedDecimal, RoundThrows

# (Re)export checked_* arithmetic functions
# - Defined in this package:
export checked_rdiv, div_with_overflow
# - Reexported from Base:
export checked_abs, checked_add, checked_cld, checked_div, checked_fld,
    checked_mod, checked_mul, checked_neg, checked_rem, checked_sub

using Base: decompose, BitInteger

import BitIntegers  # For 128-bit _widemul / _widen
import Parsers

# floats that support fma and are roughly IEEE-like
const FMAFloat = Union{Float16, Float32, Float64, BigFloat}

for fn in [:trunc, :floor, :ceil]
    fnname = Symbol(fn, "mul")
    fnname_str = String(fnname)
    opp_fn = fn == :floor ? :ceil : :floor

    @eval begin
        @doc """
            $($fnname_str)(I, x, y) :: I

        Compute `$($fn)(I, x * y)`, returning the result as type `I`. For
        floating point values, this function can be more accurate than
        `$($fn)(x * y)`.
        """ function $fnname end

        $fnname(::Type{I}, x::T, y::T) where {I, T <: Number} = $fn(I, x * y)

        $fnname(::Type{I}, x::Number, y::Number) where I = $fnname(I, promote(x, y)...)
    end

    if fn === :trunc
        # trunc a little different, implement in terms of floor
        @eval function $fnname(::Type{I}, x::T, y::T) where {I, T <: FMAFloat}
            copysign(floormul(I, abs(x), abs(y)), x*y)
        end
    else
        # floor and ceil can be implemented the same way
        @eval function $fnname(::Type{I}, x::T, y::T) where {I, T <: FMAFloat}
            a = x * y
            b = fma(x, y, -a)
            if signbit(b)
                $fn(I, a) - (isinteger(a) ? $opp_fn(I, abs(b)) : zero(I))
            else
                $fn(I, a) + (isinteger(a) ? $fn(I, abs(b)) : zero(I))
            end
        end
    end
end

"""
    FixedDecimal{T <: Integer, f::Int}

A fixed-point decimal type backed by integral type `T`, with `f` digits after
the decimal point stored.
"""
struct FixedDecimal{T <: Integer, f} <: Real
    i::T

    # inner constructor
    function Base.reinterpret(::Type{FixedDecimal{T, f}}, i::Integer) where {T, f}
        n = max_exp10(T)
        if f >= 0 && (n < 0 || f <= n)
            new{T, f}(i % T)
        else
            # Note: introducing a function barrier to improve performance
            # https://github.com/JuliaMath/FixedPointDecimals.jl/pull/30
            _throw_storage_error(f, T, n)
        end
    end
end

@noinline function _throw_storage_error(f, T, n)
    throw(ArgumentError(
        "Requested number of decimal places $f exceeds the max allowed for the " *
        "storage type $T: [0, $n]"
    ))
end

const FD = FixedDecimal

include("parse.jl")

function __init__()
    nt = isdefined(Base.Threads, :maxthreadid) ? Threads.maxthreadid() : Threads.nthreads()
    # Buffers used in parsing when dealing with BigInts, see _divpow10! in parse.jl
    resize!(empty!(_BIGINT_10s), nt)
    resize!(empty!(_BIGINT_Rs), nt)
    return
end

# Custom widemul implementation to avoid the cost of widening to BigInt.
# FD{Int128} operations should widen to 256 bits internally, rather than to a BigInt.
const BitInteger128 = Union{Int128, UInt128}
_widemul(x, y) = _widen(x) * _widen(y)
_widemul(x::Signed,y::Unsigned) = _widen(x) * signed(_widen(y))
_widemul(x::Unsigned,y::Signed) = signed(_widen(x)) * _widen(y)

# Custom widen implementation to avoid the cost of widening to BigInt.
# FD{Int128} operations should widen to 256 bits internally, rather than to a BigInt.
_widen(::Type{Int128}) = BitIntegers.Int256
_widen(::Type{UInt128}) = BitIntegers.UInt256
_widen(t::Type) = widen(t)
_widen(x::T) where {T} = (_widen(T))(x)


(::Type{T})(x::Real) where {T <: FD} = convert(T, x)

floattype(::Type{<:FD{T}}) where {T<:Union{Int8, UInt8, Int16, UInt16}} = Float32
floattype(::Type{<:FD{T}}) where {T<:Integer} = Float64
floattype(::Type{<:FD{BigInt}}) = BigFloat

# basic operators
Base.:-(x::FD{T, f}) where {T, f} = reinterpret(FD{T, f}, -x.i)
Base.abs(x::FD{T, f}) where {T, f} = reinterpret(FD{T, f}, abs(x.i))

Base.:+(x::FD{T, f}, y::FD{T, f}) where {T, f} = reinterpret(FD{T, f}, x.i+y.i)
Base.:-(x::FD{T, f}, y::FD{T, f}) where {T, f} = reinterpret(FD{T, f}, x.i-y.i)

# wide multiplication
function Base.widemul(x::FD{<:Any, f}, y::FD{<:Any, g}) where {f, g}
    i = widemul(x.i, y.i)
    reinterpret(FD{typeof(i), f + g}, i)
end
function Base.widemul(x::FD{T, f}, y::Integer) where {T, f}
    i = widemul(x.i, y)
    reinterpret(FD{typeof(i), f}, i)
end
Base.widemul(x::Integer, y::FD) = widemul(y, x)
# Need to avoid ambiguity:
Base.widemul(x::Bool, y::FD) = widemul(y, x)
# Need to avoid ambiguity:
Base.widemul(x::FD{T, f}, y::Bool) where {T, f} = widemul(x, Int(y))

"""
    _round_to_nearest(quotient, remainder, divisor, ::RoundingMode{M})

Round `quotient + remainder / divisor` to the nearest integer,
given that `0 ≤ remainder < divisor` or `0 ≥ remainder >
divisor`. (This assumption is satisfied by the return value of
`fldmod` in all cases, and the return value of `divrem` in cases where
`divisor` is known to be positive.) The tie is decided depending on
the `RoundingMode`.
"""
function _round_to_nearest(quotient::T,
                           remainder::T,
                           divisor::T,
                           ::RoundingMode{M}=RoundNearest) where {T <: Integer, M}
    halfdivisor = divisor >> 1
    # PERF Note: Only need the last bit to check iseven, and default iseven(Int256)
    # allocates, so we truncate first.
    if iseven((divisor % Int8)) && remainder == halfdivisor
        # `:NearestTiesAway` will tie away from zero, e.g. -8.5 ->
        # -9. `:NearestTiesUp` will always ties towards positive
        # infinity. `:Nearest` will tie towards the nearest even
        # integer.
        if M == :NearestTiesAway
            ifelse(quotient < zero(quotient), quotient, quotient + one(quotient))
        elseif M == :Nearest
            ifelse(iseven(quotient), quotient, quotient + one(quotient))
        else
            quotient + one(quotient)
        end
    elseif abs(remainder) > abs(halfdivisor)
        quotient + one(quotient)
    else
        quotient
    end
end
_round_to_nearest(q, r, d, m=RoundNearest) = _round_to_nearest(promote(q, r, d)..., m)

# In many of our calls to fldmod, `y` is a constant (the coefficient, 10^f). However, since
# `fldmod` is sometimes not being inlined, that constant information is not available to the
# optimizer. We need an inlined version of fldmod so that the compiler can replace expensive
# divide-by-power-of-ten instructions with the cheaper multiply-by-inverse-coefficient.
@inline fldmodinline(x,y) = (fld(x,y), mod(x,y))

# multiplication rounds to nearest even representation
# TODO: can we use floating point to speed this up? after we build a
# correctness test suite.
function Base.:*(x::FD{T, f}, y::FD{T, f}) where {T, f}
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmodinline(_widemul(x.i, y.i), powt)
    reinterpret(FD{T, f}, _round_to_nearest(quotient, remainder, powt))
end

# these functions are needed to avoid InexactError when converting from the
# integer type
Base.:*(x::Integer, y::FD{T, f}) where {T, f} = reinterpret(FD{T, f}, x * y.i)
Base.:*(x::FD{T, f}, y::Integer) where {T, f} = reinterpret(FD{T, f}, x.i * y)

Base.:/(x::FD, y::FD) = checked_rdiv(x, y)
Base.:/(x::Integer, y::FD) = checked_rdiv(x, y)
Base.:/(x::FD, y::Integer) = checked_rdiv(x, y)

# integerification
Base.trunc(x::FD{T, f}) where {T, f} = FD{T, f}(div(x.i, coefficient(FD{T, f})))
Base.floor(x::FD{T, f}) where {T, f} = FD{T, f}(fld(x.i, coefficient(FD{T, f})))

Base.round(fd::FD, ::RoundingMode{:Up}) = ceil(fd)
Base.round(fd::FD, ::RoundingMode{:Down}) = floor(fd)
Base.round(fd::FD, ::RoundingMode{:ToZero}) = trunc(fd)

# TODO: round with number of digits; should be easy
function Base.round(x::FD{T, f},
                    m::Union{RoundingMode{:Nearest},
                             RoundingMode{:NearestTiesUp},
                             RoundingMode{:NearestTiesAway}}=RoundNearest) where {T, f}
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmodinline(x.i, powt)
    FD{T, f}(_round_to_nearest(quotient, remainder, powt, m))
end
function Base.ceil(x::FD{T, f}) where {T, f}
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmodinline(x.i, powt)
    if remainder > 0
        FD{T, f}(quotient + one(quotient))
    else
        FD{T, f}(quotient)
    end
end

"""
    required_precision(n::Integer)

Compute the number of bits of precision needed to represent an integer exactly
as a floating point number.

This is equivalent to counting the number of bits needed to represent the
integer, excluding any trailing zeros.
"""
required_precision(n::Integer) = ndigits(n, base=2) - trailing_zeros(n)

"""
    _apply_exact_float(f, T, x::Real, i::Integer)

Compute `f(T, x, i)::T` but avoiding possible loss of precision from an
intermediate conversion of `i` to a floating point type by instead using a
`BigFloat` with sufficient precision if necessary.
"""
function _apply_exact_float(f, ::Type{T}, x::FMAFloat, i::Integer) where T
    prec = required_precision(i)
    if prec > 53
        setprecision(BigFloat, prec) do
            f(T, x, BigFloat(i))
        end
    else
        f(T, x, Float64(i))
    end
end

_apply_exact_float(f, ::Type{T}, x::Real, i::Integer) where T = f(T, x, i)

for fn in [:trunc, :floor, :ceil]
    @eval (Base.$fn(::Type{TI}, x::FD)::TI) where {TI <: Integer} = $fn(x)

    # round/trunc/ceil/flooring to FD; generic
    @eval function Base.$fn(::Type{FD{T, f}}, x::Real) where {T, f}
        powt = coefficient(FD{T, f})
        # Use machine Float64 if possible, but fall back to BigFloat if we need
        # more precision. 4f bits suffices.
        val = _apply_exact_float($(Symbol(fn, "mul")), T, x, powt)
        reinterpret(FD{T, f}, val)
    end
    # needed to avoid ambiguity with e.g. `floor(::Type{T}, x::Rational)`
    @eval function Base.$fn(::Type{FD{T, f}}, x::Rational) where {T, f}
        reinterpret(FD{T, f}, $fn(T, x * coefficient(FD{T, f})))
    end
end
function Base.round(::Type{TI}, x::FD, m::RoundingMode=RoundNearest) where {TI <: Integer}
    convert(TI, round(x,m))::TI
end
function Base.round(::Type{FD{T, f}}, x::Real, ::RoundingMode{:Nearest}=RoundNearest) where {T, f}
    reinterpret(FD{T, f}, round(T, x * coefficient(FD{T, f})))
end

# needed to avoid ambiguity with `round(::Type{T}, x::Rational{Bool}, ::RoundingMode)`
function Base.round(::Type{FD{T, f}}, x::Rational{Bool}, ::RoundingMode{:Nearest}=RoundNearest) where {T, f}
   reinterpret(FD{T, f}, round(T, x * coefficient(FD{T, f})))
end
function Base.round(::Type{FD{T, f}}, x::Rational{Tr}, ::RoundingMode{:Nearest}=RoundNearest) where {T, f, Tr}
   reinterpret(FD{T, f}, round(T, x * coefficient(FD{T, f})))
end


# conversions and promotions
Base.convert(::Type{FD{T, f}}, x::FD{T, f}) where {T, f} = x  # Converting an FD to itself is a no-op

function Base.convert(::Type{FD{T, f}}, x::Integer) where {T, f}
    C = coefficient(FD{T, f})
    throw_inexact() = throw(InexactError(:convert, FD{T, f}, x))
    typemin(T) <= x <= typemax(T) || throw_inexact()
    xT = convert(T, x)  # This won't throw, since we already checked, above.
    # Perform x * C, and check for overflow. This is cheaper than a widemul, especially for
    # 128-bit T, since those widen into a BigInt.
    v = _mul_checked_overflow(throw_inexact, xT, C)
    return reinterpret(FD{T, f}, v)
end
function Base.convert(::Type{FD{BigInt, f}}, x::Integer) where {f}
    # We specialize on f==0, since julia can't eliminate BigInt multiplication.
    if f == 0
        # If x is already a BigInt, this is a no-op, otherwise we alloc a new BigInt.
        return reinterpret(FD{BigInt, f}, BigInt(x))
    end
    # For the normal case, we multiply by C, which produces a BigInt value.
    C = coefficient(FD{BigInt, f})
    # This can't throw since BigInt and BigFloat can hold any number.
    v = x * C
    return reinterpret(FD{BigInt, f}, v)
end

# x * y - if overflow, report an InexactError(FDT, )
function _mul_checked_overflow(overflow_callback, x, y)
    v, overflow = Base.mul_with_overflow(x, y)
    if overflow
        overflow_callback()
    end
    return v
end

Base.convert(::Type{T}, x::AbstractFloat) where {T <: FD} = round(T, x)

function Base.convert(::Type{FD{T, f}}, x::Rational) where {T, f}
    powt = coefficient(FD{T, f})
    reinterpret(FD{T, f}, T(x * powt))::FD{T, f}
end

function Base.convert(::Type{FD{T, f}}, x::FD{U, g}) where {T, f, U, g}
    if f ≥ g
        # Compute `10^(f - g)` without overflow
        powt = div(coefficient(FD{T, f}), coefficient(FD{U, g}))
        v = _mul_checked_overflow(promote(x.i, powt)...) do
            throw(InexactError(:convert, FD{T, f}, x))
        end
        reinterpret(FD{T, f}, T(v))
    else
        # Compute `10^(g - f)` without overflow
        powt = div(coefficient(FD{U, g}), coefficient(FD{T, f}))
        q, r = divrem(x.i, powt)
        if r == 0
            reinterpret(FD{T, f}, T(q))
        else
            throw(InexactError(:convert, FD{T, f}, x))
        end
    end
end

for remfn in [:rem, :mod, :mod1, :min, :max]
    @eval Base.$remfn(x::T, y::T) where {T <: FD} = reinterpret(T, $remfn(x.i, y.i))
end
# TODO: When we upgrade to a min julia version >=1.4 (i.e Julia 2.0), this block can be
# dropped in favor of three-argument `div`, below.
# The division functions all default to *throwing OverflowError* rather than
# wrapping on integer overflow.
# This decision may be changed in a future release of FixedPointDecimals.
Base.div(x::FD, y::FD) = Base.checked_div(x, y)
Base.fld(x::FD, y::FD) = Base.checked_fld(x, y)
Base.cld(x::FD, y::FD) = Base.checked_cld(x, y)
# There is no checked_fld1, so this is implemented here:
function Base.fld1(x::FD{T,f}, y::FD{T,f}) where {T, f}
    C = coefficient(FD{T, f})
    # Note: fld1() will already throw for divide-by-zero and typemin(T) ÷ -1.
    v, b = Base.Checked.mul_with_overflow(C, fld1(x.i, y.i))
    b && _throw_overflowerr_op(:fld1, x, y)
    return reinterpret(FD{T, f}, v)
end
if VERSION >= v"1.4.0-"
    # div(x.i, y.i) eliminates the scaling coefficient, so we call the FD constructor.
    # We don't need any widening logic, since we won't be multiplying by the coefficient.
    @eval function Base.div(x::FD{T, f}, y::FD{T, f}, r::RoundingMode) where {T<:Integer, f}
        C = coefficient(FD{T, f})
        # Note: The div() will already throw for divide-by-zero and typemin(T) ÷ -1.
        v, b = Base.Checked.mul_with_overflow(C, div(x.i, y.i, r))
        b && _throw_overflowerr_op(:div, x, y)
        return reinterpret(FD{T, f}, v)
    end
end

# --- Checked arithmetic ---

function Base.add_with_overflow(x::T, y::T) where {T<:FD}
    z, b = Base.add_with_overflow(x.i, y.i)
    return (reinterpret(T, z), b)
end

function Base.sub_with_overflow(x::T, y::T) where {T<:FD}
    z, b = Base.sub_with_overflow(x.i, y.i)
    return (reinterpret(T, z), b)
end

function Base.Checked.mul_with_overflow(x::FD{T,f}, y::FD{T,f}) where {T<:Integer,f}
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmodinline(_widemul(x.i, y.i), powt)
    v = _round_to_nearest(quotient, remainder, powt)
    return (reinterpret(FD{T,f}, rem(v, T)), v < typemin(T) || v > typemax(T))
end

# This does not exist in Base so is just part of this package.
# Throws on divide by zero.
@doc """
    div_with_overflow(x::FixedDecimal{T,f}, y::FixedDecimal{T,f})::(FixedDecimal{T,f}, Bool)
        where {T<:Integer, f}

Return the result of div (wrapping on overflow/underflow) and a boolean indicating whether
overflow/underflow did in fact happen. Throws a DivideError on divide-by-zero.
"""
function div_with_overflow(x::FD{T,f}, y::FD{T,f}) where {T<:Integer,f}
    C = coefficient(FD{T, f})
    # This case will break the div call below.
    if y.i == -1 && T <: Signed && hasmethod(typemin, (Type{T},)) && x.i == typemin(T)
        # To perform the div and overflow means reaching the max and adding 1, so typemin.
        return (x, true)
    end
    # Note: The div() will throw for divide-by-zero, that's not an overflow.
    v, b = Base.Checked.mul_with_overflow(C, div(x.i, y.i))
    return (reinterpret(FD{T,f}, v), b)
end

# Does not exist in Base.Checked, so just exists in this package.
@doc """
    FixedPointDecimals.fld_with_overflow(x::FD, y::FD)::Tuple{FD,Bool}

Calculates the largest integer less than or equal to `x / y`, checking for overflow errors
where applicable, returning the result and a boolean indicating whether overflow occured.
Throws a DivideError on divide-by-zero.

The overflow protection may impose a perceptible performance penalty.

See also:
- `Base.checked_fld`.
"""
function fld_with_overflow(x::FD{T,f}, y::FD{T,f}) where {T<:Integer,f}
    C = coefficient(FD{T, f})
    # This case will break the fld call below.
    if y.i == -1 && T <: Signed && hasmethod(typemin, (Type{T},)) && x.i == typemin(T)
        # To fld and overflow means reaching the max and adding 1, so typemin (x).
        return (x, true)
    end
    # Note: The fld() will already throw for divide-by-zero, that's not an overflow.
    v, b = Base.Checked.mul_with_overflow(C, fld(x.i, y.i))
    return (reinterpret(FD{T, f}, v), b)
end

"""
    FixedPointDecimals.rdiv_with_overflow(x::FD, y::FD)::Tuple{FD,Bool}

Calculates `x / y`, checking for overflow errors where applicable, returning the result
and a boolean indicating whether overflow occured. Throws a DivideError on divide-by-zero.

The overflow protection may impose a perceptible performance penalty.

See also:
- `Base.checked_rdiv`.
"""
function rdiv_with_overflow(x::FD{T,f}, y::FD{T,f}) where {T<:Integer,f}
    powt = coefficient(FD{T, f})
    # No multiplication can reach the typemax/typemin of a wider type, thus no typemin / -1.
    quotient, remainder = fldmod(_widemul(x.i, powt), y.i)
    # quotient is necessarily not typemax/typemin. x.i * powt cannot reach typemax/typemin
    # of the widened type and y.i is an integer. Thus the following call cannot overflow.
    v = _round_to_nearest(quotient, remainder, y.i)
    return (reinterpret(FD{T,f}, rem(v, T)), v < typemin(T) || v > typemax(T))
end

# These functions allow us to perform division with integers outside of the range of the
# FixedDecimal.
function rdiv_with_overflow(x::Integer, y::FD{T, f}) where {T<:Integer, f}
    powt = coefficient(FD{T, f})
    powtsq = _widemul(powt, powt)
    # No multiplication can reach the typemax/typemin of a wider type, thus no typemin / -1.
    quotient, remainder = fldmod(_widemul(x, powtsq), y.i)
    # Same deal as previous overload as to why this will not overload. Note that all
    # multiplication operations were widemuls.
    v = _round_to_nearest(quotient, remainder, y.i)
    return (reinterpret(FD{T,f}, rem(v, T)), v < typemin(T) || v > typemax(T))
end
function rdiv_with_overflow(x::FD{T, f}, y::Integer) where {T<:Integer, f}
    if y == -1 && T <: Signed && hasmethod(typemin, (Type{T},)) && x.i == typemin(T)
        # typemin / -1 for signed integers wraps, giving typemin (x) again.
        return (x, true)
    end

    quotient, remainder = fldmod(x.i, y)
    # It is impossible for both the quotient to be typemax/typemin AND remainder to be
    # non-zero because y is an integer. Thus the following call cannot overflow.
    v = _round_to_nearest(quotient, remainder, y)
    return (reinterpret(FD{T, f}, v), false)
end

# Does not exist in Base.Checked, so just exists in this package.
"""
    FixedPointDecimals.ceil_with_overflow(x::FD)::Tuple{FD,Bool}

Calculate the nearest integral value of the same type as x that is greater than or equal
to x, returning it and a boolean indicating whether overflow has occurred.

The overflow protection may impose a perceptible performance penalty.
"""
function ceil_with_overflow(x::FD{T,f}) where {T<:Integer,f}
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmodinline(x.i, powt)
    return if remainder > 0
        # Could overflow when powt is 1 (f is 0) and x/x.i is typemax.
        v, add_overflowed = Base.Checked.add_with_overflow(quotient, one(quotient))
        # Could overflow when x is close to typemax (max quotient) independent of f.
        backing, mul_overflowed = Base.Checked.mul_with_overflow(v, powt)
        (reinterpret(FD{T, f}, backing), add_overflowed || mul_overflowed)
    else
        (FD{T, f}(quotient), false)
    end
end

# Does not exist in Base.Checked, so just exists in this package.
"""
    FixedPointDecimals.floor_with_overflow(x::FD)::Tuple{FD,Bool}

Calculate the nearest integral value of the same type as x that is less than or equal
to x, returning it and a boolean indicating whether overflow has occurred.

The overflow protection may impose a perceptible performance penalty.
"""
function floor_with_overflow(x::FD{T, f}) where {T, f}
    powt = coefficient(FD{T, f})
    # Won't underflow, powt is an integer.
    quotient = fld(x.i, powt)
    # When we convert it back to the backing format it might though. Occurs when
    # the integer part of x is at its maximum.
    backing, overflowed = Base.Checked.mul_with_overflow(quotient, powt)
    return (reinterpret(FD{T, f}, backing), overflowed)
end

# Does not exist in Base.Checked, so just exists in this package.
"""
    FixedPointDecimals.round_with_overflow(x::FD, mode=RoundNearest)::Tuple{FD,Bool}

Calculate the nearest integral value of the same type as x, breaking ties using the
specified RoundingModes, returning it and a boolean indicating whether overflow has
occurred.

The overflow protection may impose a perceptible performance penalty.
"""
round_with_overflow(fd::FD, ::RoundingMode{:Up}) = ceil_with_overflow(fd)
round_with_overflow(fd::FD, ::RoundingMode{:Down}) = floor_with_overflow(fd)
# trunc cannot overflow.
round_with_overflow(fd::FD, ::RoundingMode{:ToZero}) = (trunc(fd), false)
function round_with_overflow(
    x::FD{T, f},
    m::Union{
        RoundingMode{:Nearest},
        RoundingMode{:NearestTiesUp},
        RoundingMode{:NearestTiesAway}
    }=RoundNearest,
) where {T, f}
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmodinline(x.i, powt)
    v = _round_to_nearest(quotient, remainder, powt, m)
    backing, overflowed = Base.Checked.mul_with_overflow(v, powt)
    (reinterpret(FD{T, f}, backing), overflowed)
end

Base.checked_add(x::FD, y::FD) = Base.checked_add(promote(x, y)...)
Base.checked_sub(x::FD, y::FD) = Base.checked_sub(promote(x, y)...)
Base.checked_mul(x::FD, y::FD) = Base.checked_mul(promote(x, y)...)
Base.checked_div(x::FD, y::FD) = Base.checked_div(promote(x, y)...)
Base.checked_cld(x::FD, y::FD) = Base.checked_cld(promote(x, y)...)
Base.checked_fld(x::FD, y::FD) = Base.checked_fld(promote(x, y)...)
Base.checked_rem(x::FD, y::FD) = Base.checked_rem(promote(x, y)...)
Base.checked_mod(x::FD, y::FD) = Base.checked_mod(promote(x, y)...)

Base.checked_add(x::FD, y) = Base.checked_add(promote(x, y)...)
Base.checked_add(x, y::FD) = Base.checked_add(promote(x, y)...)
Base.checked_sub(x::FD, y) = Base.checked_sub(promote(x, y)...)
Base.checked_sub(x, y::FD) = Base.checked_sub(promote(x, y)...)
Base.checked_mul(x::FD, y) = Base.checked_mul(promote(x, y)...)
Base.checked_mul(x, y::FD) = Base.checked_mul(promote(x, y)...)
Base.checked_div(x::FD, y) = Base.checked_div(promote(x, y)...)
Base.checked_div(x, y::FD) = Base.checked_div(promote(x, y)...)
Base.checked_cld(x::FD, y) = Base.checked_cld(promote(x, y)...)
Base.checked_cld(x, y::FD) = Base.checked_cld(promote(x, y)...)
Base.checked_fld(x::FD, y) = Base.checked_fld(promote(x, y)...)
Base.checked_fld(x, y::FD) = Base.checked_fld(promote(x, y)...)
Base.checked_rem(x::FD, y) = Base.checked_rem(promote(x, y)...)
Base.checked_rem(x, y::FD) = Base.checked_rem(promote(x, y)...)
Base.checked_mod(x::FD, y) = Base.checked_mod(promote(x, y)...)
Base.checked_mod(x, y::FD) = Base.checked_mod(promote(x, y)...)

function Base.checked_add(x::T, y::T) where {T<:FD}
    z, b = Base.Checked.add_with_overflow(x, y)
    b && Base.Checked.throw_overflowerr_binaryop(:+, x, y)
    return z
end
function Base.checked_sub(x::T, y::T) where {T<:FD}
    z, b = Base.Checked.sub_with_overflow(x, y)
    b && Base.Checked.throw_overflowerr_binaryop(:-, x, y)
    return z
end
function Base.checked_mul(x::FD{T,f}, y::FD{T,f}) where {T<:Integer,f}
    z, b = Base.Checked.mul_with_overflow(x, y)
    b && Base.Checked.throw_overflowerr_binaryop(:*, x, y)
    return z
end
# Checked division functions
for divfn in [:div, :fld, :cld]
    @eval function Base.$(Symbol("checked_$divfn"))(x::FD{T,f}, y::FD{T,f}) where {T<:Integer,f}
        C = coefficient(FD{T, f})
        # Note: The div() will already throw for divide-by-zero and typemin(T) ÷ -1.
        v, b = Base.Checked.mul_with_overflow(C, $divfn(x.i, y.i))
        b && _throw_overflowerr_op($(QuoteNode(divfn)), x, y)
        return reinterpret(FD{T, f}, v)
    end
end
for remfn in [:rem, :mod]
    # rem and mod already check for divide-by-zero and typemin(T) ÷ -1, so nothing to do.
    @eval Base.$(Symbol("checked_$remfn"))(x::T, y::T) where {T <: FD} = $remfn(x, y)
end

@noinline _throw_overflowerr_op(op, x::T, y::T) where T = throw(OverflowError("$op($x, $y) overflowed for type $T"))

function Base.checked_neg(x::T) where {T<:FD}
    r = -x
    # Simplify the compiler's job, no need to call the FD,Int comparison
    (x.i<0) & (r.i<0) && Base.Checked.throw_overflowerr_negation(x)
    return r
end
function Base.checked_abs(x::FD)
    # Simplify the compiler's job, no need to call the FD,Int comparison
    r = ifelse(x.i<0, -x, x)
    r.i<0 || return r
    _throw_overflow_abs(x)
end
if VERSION >= v"1.8.0-"
    @noinline _throw_overflow_abs(x) =
        throw(OverflowError(LazyString("checked arithmetic: cannot compute |x| for x = ", x, "::", typeof(x))))
else
    @noinline _throw_overflow_abs(x) =
        throw(OverflowError("checked arithmetic: cannot compute |x| for x = $x"))
end

# We introduce a new function for this since Base.Checked only supports integers, and ints
# don't have a decimal division operation.
"""
    FixedPointDecimals.checked_rdiv(x::FD, y::FD) -> FD

Calculates `x / y`, checking for overflow errors where applicable.

The overflow protection may impose a perceptible performance penalty.

See also:
- `Base.checked_div` for truncating division.
"""
checked_rdiv(x::FD, y::FD) = checked_rdiv(promote(x, y)...)

function checked_rdiv(x::FD{T,f}, y::FD{T,f}) where {T<:Integer,f}
    (z, b) = rdiv_with_overflow(x, y)
    b && Base.Checked.throw_overflowerr_binaryop(:/, x, y)
    return z
end

# These functions allow us to perform division with integers outside of the range of the
# FixedDecimal.
function checked_rdiv(x::Integer, y::FD{T, f}) where {T<:Integer, f}
    (z, b) = rdiv_with_overflow(x, y)
    b && Base.Checked.throw_overflowerr_binaryop(:/, x, y)
    return z
end
function checked_rdiv(x::FD{T, f}, y::Integer) where {T<:Integer, f}
    (z, b) = rdiv_with_overflow(x, y)
    b && Base.Checked.throw_overflowerr_binaryop(:/, x, y)
    return z
end


# --------------------------

Base.convert(::Type{AbstractFloat}, x::FD) = convert(floattype(typeof(x)), x)
function Base.convert(::Type{TF}, x::FD{T, f}) where {TF <: AbstractFloat, T, f}
    convert(TF, x.i / coefficient(FD{T, f}))::TF
end

function Base.convert(::Type{TF}, x::FD{T, f}) where {TF <: BigFloat, T, f}
    convert(TF, BigInt(x.i) / BigInt(coefficient(FD{T, f})))::TF
end

function Base.convert(::Type{TI}, x::FD{T, f}) where {TI <: Integer, T, f}
    isinteger(x) || throw(InexactError(:convert, TI, x))
    convert(TI, div(x.i, coefficient(FD{T, f})))::TI
end

function Base.convert(::Type{TR}, x::FD{T, f}) where {TR <: Rational, T, f}
    convert(TR, x.i // coefficient(FD{T, f}))::TR
end

(::Type{T})(x::FD) where {T<:Union{AbstractFloat,Integer,Rational}} = convert(T, x)
# Need to avoid ambiguity:
Bool(x::FD) = convert(Bool, x)

Base.promote_rule(::Type{FD{T, f}}, ::Type{<:Integer}) where {T, f} = FD{T, f}
Base.promote_rule(::Type{<:FD}, ::Type{TF}) where {TF <: AbstractFloat} = TF
Base.promote_rule(::Type{<:FD{T}}, ::Type{Rational{TR}}) where {T, TR} = Rational{promote_type(T, TR)}

# TODO: decide if these are the right semantics;
# right now we pick the bigger int type and the bigger decimal point
function Base.promote_rule(::Type{FD{T, f}}, ::Type{FD{U, g}}) where {T, f, U, g}
    FD{promote_type(T, U), max(f, g)}
end

# The default `zero` and `one` call `convert`, which is expensive, so we call reinterpret.
Base.zero(::Type{FD{T, f}}) where {T, f} = reinterpret(FD{T, f}, zero(T))
Base.one(::Type{FD{T, f}}) where {T, f} = reinterpret(FD{T, f}, coefficient(FD{T, f}))

# comparison
Base.:(==)(x::T, y::T) where {T <: FD} = x.i == y.i
Base.:(<)(x::T, y::T) where {T <: FD} = x.i < y.i
Base.:(<=)(x::T, y::T) where {T <: FD} = x.i <= y.i

# In the case where the FD types are different, or where one is an Integer, add
# custom overloads, rather than relying on the default promotions, to avoid throwing
# InexactError from the promotions. This lets us do e.g. `FD{Int8,2}(0) < 2`, without the
# promotion to FD{Int8,2}(2) throwing an InexactError.
for comp_op in (:(==), :(<), :(<=))
    @eval function Base.$comp_op(x::FD{T1,f1}, y::FD{T2,f2}) where {T1<:Integer, f1, T2<:Integer, f2}
        if f1 == f2
            # If the precisions are the same, even if the types are different, we can compare
            # the integer values directly. e.g. Int8(2) == Int16(2) is true.
            return $comp_op(x.i, y.i)
        else
            # Promote the integers to the larger type, and then scale them to the same
            # precision. If the scaling operation ends up overflowing, we know that they aren't
            # equal, because we know that the less precise value wasn't even representable in
            # the more precise type, so they cannot be equal.
            newFD = promote_type(FD{T1,f1}, FD{T2,f2})
            xi, yi = promote(x.i, y.i)
            if f1 > f2
                C = coefficient(newFD) ÷ coefficient(FD{T2,f2})
                yi, wrapped = Base.mul_with_overflow(yi, C)
                if wrapped
                    is_underflow = (y.i < 0)
                    if !is_underflow
                        # Whether we're computing `==`, `<` or `<=`, if y overflowed, it
                        # means it's bigger than x.
                        return $(comp_op == :(==)) ? false : true
                    else  # underflow
                        # If y is negative, then y is definitely less than x, since y is so
                        # small, it doesn't even fit in y's type.
                        return false
                    end
                end
            else
                C = coefficient(newFD) ÷ coefficient(FD{T1,f1})
                xi, wrapped = Base.mul_with_overflow(xi, C)
                if wrapped
                    is_underflow = (x.i < 0)
                    if !is_underflow
                        # Whether we're computing `==`, `<` or `<=`, if x overflowed, it
                        # means it's bigger than y, so this is false.
                        return false
                    else  # underflow
                        # If x is negative, then x is definitely less than y, since x is so
                        # small, it doesn't even fit in y's type.
                        return $(comp_op == :(==)) ? false : true
                    end
                end
            end
            return $comp_op(xi, yi)
        end
    end
    @eval function Base.$comp_op(x::Integer, y::FD{T,f}) where {T<:Integer, f}
        if f == 0
            # If the precisions are the same, even if the types are different, we can
            # compare the integer values directly. e.g. Int8(2) == Int16(2) is true.
            return $comp_op(x, y.i)
        else
            if !(x isa T)
                if x > typemax(T)
                    # If x is too big to fit in T, then we know already that it's bigger
                    # than y, so not equal and not less than.
                    return false
                elseif x < typemin(T)
                    # Similarly, if too small, it's definitely less than y (and not equal).
                    return $(comp_op == :(==)) ? false : true
                end
            end
            # Now it's safe to truncate x down to y's type.
            xi = x % T
            xi, wrapped = Base.mul_with_overflow(xi, coefficient(FD{T,f}))
            if wrapped
                is_underflow = (x < 0)
                if !is_underflow
                    # Whether we're computing `==`, `<` or `<=`, if x overflowed, it means it's
                    # bigger than y, so this is false.
                    return false
                else  # underflow
                    # If x is negative, then x is definitely less than y, since x is so
                    # small, it doesn't even fit in y's type.
                    return $(comp_op == :(==)) ? false : true
                end
            end
            return $comp_op(xi, y.i)
        end
    end
    @eval function Base.$comp_op(x::FD{T,f}, y::Integer) where {T<:Integer, f}
        if f == 0
            # If the precisions are the same, even if the types are different, we can
            # compare the integer values directly. e.g. Int8(2) == Int16(2) is true.
            return $comp_op(x.i, y)
        else
            if !(y isa T)
                if y > typemax(T)
                    # If y is too big to fit in T, then we know already that x is smaller
                    # than y. So not equal, but definitely x < y.
                    return $(comp_op == :(==)) ? false : true
                elseif y < typemin(T)
                    # Similarly, if y is too small, definitely x > y (and not equal).
                    return false
                end
            end
            # Now it's safe to truncate x down to y's type.
            yi = y % T
            yi, wrapped = Base.mul_with_overflow(yi, coefficient(FD{T,f}))
            if wrapped
                is_underflow = (y < 0)
                if !is_underflow
                    # Whether we're computing `==`, `<` or `<=`, if y overflowed, it means it's
                    # bigger than x.
                    return $(comp_op == :(==)) ? false : true
                else  # underflow
                    # If y is negative, then y is definitely less than x, since y is so
                    # small, it doesn't even fit in y's type.
                    return false
                end
            end
            return $comp_op(x.i, yi)
        end
    end
end

# predicates and traits
Base.isinteger(x::FD{T, f}) where {T, f} = rem(x.i, coefficient(FD{T, f})) == 0
Base.typemin(::Type{FD{T, f}}) where {T, f} = reinterpret(FD{T, f}, typemin(T))
Base.typemax(::Type{FD{T, f}}) where {T, f}= reinterpret(FD{T, f}, typemax(T))
Base.eps(::Type{T}) where {T <: FD} = reinterpret(T, 1)
Base.eps(x::FD) = eps(typeof(x))
Base.floatmin(::Type{T}) where {T <: FD} = eps(T)
Base.floatmax(::Type{T}) where {T <: FD} = typemax(T)

# printing
function Base.print(io::IO, x::FD{T, 0}) where T
    print(io, x.i)
end
function Base.print(io::IO, x::FD{T, f}) where {T, f}
    iscompact = get(io, :compact, false)

    # note: a is negative if x.i == typemin(x.i)
    s, a = sign(x.i), abs(x.i)
    integer, fractional = divrem(a, coefficient(x))
    integer = abs(integer)  # ...but since f > 0, this is positive
    fractional = abs(fractional)

    if s == -1
        print(io, "-")
    end
    fractionchars = lpad(abs(fractional), f, "0")
    if iscompact
        fractionchars = rstrip(fractionchars, '0')
        if isempty(fractionchars)
            fractionchars = "0"
        end
    end
    print(io, integer, '.', fractionchars)
end

function Base.show(io::IO, x::FD{T, f}) where {T, f}
    iscompact = get(io, :compact, false)
    if !iscompact
        print(io, "FixedDecimal{$T,$f}(")
    end
    print(io, x)
    if !iscompact
        print(io, ')')
    end
end

"""
    max_exp10(T)

The highest value of `x` which does not result in an overflow when evaluating `T(10)^x`. For
types of `T` that do not overflow -1 will be returned.

NOTE: This function is expensive, since it contains a while-loop, but it is actually
      computing a constant value for types, so it really only needs to be run once per type.
      We achieve this by `@eval`ing new methods in a loop, below. Users can do this
      themselves to add more "frozen" methods for custom Integer types:
      ```julia
      @eval FixedPointDecimals.max_exp10(::Type{CustomIntType}) = \$(max_exp10(CustomIntType))
      ```
      This function does not have or depend on any side-effects.
"""
function max_exp10(::Type{T}) where {T <: Integer}
    W = _widen(T)
    type_max = W(typemax(T))

    powt = one(W)
    ten = W(10)
    exponent = 0

    while type_max > powt
        powt *= ten
        exponent += 1
    end

    exponent - 1
end

max_exp10(::Type{BigInt}) = -1

# Freeze the evaluation for BitInteger types, since max_exp10() is too compilicated to get
# optimized away by the compiler during const-folding. (We can't freeze for user-defined
# types because we don't know what they are yet.)
for T in Base.BitInteger_types
    @eval max_exp10(::Type{$T}) = $(max_exp10(T))
end

"""
    coefficient(::Type{FD{T, f}}) -> T

Compute `10^f` as an Integer without overflow. Note that overflow will not occur for any
constructable `FD{T, f}`.
"""
coefficient(::Type{FD{T, f}}) where {T, f} = T(10)^f
coefficient(fd::FD{T, f}) where {T, f} = coefficient(FD{T, f})
value(fd::FD) = fd.i

# for generic hashing
Base.decompose(fd::FD) = decompose(Rational(fd))

end  # module
