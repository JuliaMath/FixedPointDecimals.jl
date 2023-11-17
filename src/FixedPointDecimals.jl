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

export FixedDecimal, RoundThrows

using Base: decompose, BitInteger
import Parsers

# floats that support fma and are roughly IEEE-like
const FMAFloat = Union{Float16, Float32, Float64, BigFloat}

for fn in [:trunc, :floor, :ceil]
    fnname = Symbol(fn, "mul")
    opp_fn = fn == :floor ? :ceil : :floor

    @eval begin
        @doc """
            $($fnname)(I, x, y) :: I

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
    if iseven(divisor) && remainder == halfdivisor
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
    quotient, remainder = fldmodinline(widemul(x.i, y.i), powt)
    reinterpret(FD{T, f}, _round_to_nearest(quotient, remainder, powt))
end

# these functions are needed to avoid InexactError when converting from the
# integer type
Base.:*(x::Integer, y::FD{T, f}) where {T, f} = reinterpret(FD{T, f}, T(x * y.i))
Base.:*(x::FD{T, f}, y::Integer) where {T, f} = reinterpret(FD{T, f}, T(x.i * y))

function Base.:/(x::FD{T, f}, y::FD{T, f}) where {T, f}
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmod(widemul(x.i, powt), y.i)
    reinterpret(FD{T, f}, T(_round_to_nearest(quotient, remainder, y.i)))
end

# These functions allow us to perform division with integers outside of the range of the
# FixedDecimal.
function Base.:/(x::Integer, y::FD{T, f}) where {T, f}
    powt = coefficient(FD{T, f})
    powtsq = widemul(powt, powt)
    quotient, remainder = fldmod(widemul(x, powtsq), y.i)
    reinterpret(FD{T, f}, T(_round_to_nearest(quotient, remainder, y.i)))
end

function Base.:/(x::FD{T, f}, y::Integer) where {T, f}
    quotient, remainder = fldmod(x.i, y)
    reinterpret(FD{T, f}, T(_round_to_nearest(quotient, remainder, y)))
end

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
end
function Base.round(::Type{TI}, x::FD, m::RoundingMode=RoundNearest) where {TI <: Integer}
    convert(TI, round(x,m))::TI
end
function Base.round(::Type{FD{T, f}}, x::Real, ::RoundingMode{:Nearest}=RoundNearest) where {T, f}
    reinterpret(FD{T, f}, round(T, x * coefficient(FD{T, f})))
end

# needed to avoid ambiguity
function Base.round(::Type{FD{T, f}}, x::Rational, ::RoundingMode{:Nearest}=RoundNearest) where {T, f}
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
    v, overflow = Base.mul_with_overflow(xT, C)
    if overflow
        throw_inexact()
    end
    reinterpret(FD{T, f}, v)
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
        reinterpret(FD{T, f}, T(widemul(x.i, powt)))
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
for divfn in [:div, :fld, :fld1, :cld]
    # div(x.i, y.i) eliminates the scaling coefficient, so we call the FD constructor.
    # We don't need any widening logic, since we won't be multiplying by the coefficient.
    @eval Base.$divfn(x::T, y::T) where {T <: FD} = T($divfn(x.i, y.i))
end
if VERSION >= v"1.4.0-"
    # div(x.i, y.i) eliminates the scaling coefficient, so we call the FD constructor.
    # We don't need any widening logic, since we won't be multiplying by the coefficient.
    Base.div(x::T, y::T, r::RoundingMode) where {T <: FD} = T(div(x.i, y.i, r))
end

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
    W = widen(T)
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

end
