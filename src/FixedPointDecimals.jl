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

using Compat: lastindex, something

import Compat: floatmin, floatmax

import Base: reinterpret, zero, one, abs, sign, ==, <, <=, +, -, /, *, div, rem, divrem,
             fld, mod, fldmod, fld1, mod1, fldmod1, isinteger, typemin, typemax,
             print, show, string, convert, parse, promote_rule, min, max,
             trunc, round, floor, ceil, eps, float, widemul, decompose

const BitInteger = Union{Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64,
                         UInt64, Int128, UInt128}

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

(::Type{T})(x::Real) where {T <: FD} = convert(T, x)

floattype(::Type{<:FD{T}}) where {T<:Union{Int8, UInt8, Int16, UInt16}} = Float32
floattype(::Type{<:FD{T}}) where {T<:Integer} = Float64
floattype(::Type{<:FD{BigInt}}) = BigFloat

# basic operators
-(x::FD{T, f}) where {T, f} = reinterpret(FD{T, f}, -x.i)
abs(x::FD{T, f}) where {T, f} = reinterpret(FD{T, f}, abs(x.i))

+(x::FD{T, f}, y::FD{T, f}) where {T, f} = reinterpret(FD{T, f}, x.i+y.i)
-(x::FD{T, f}, y::FD{T, f}) where {T, f} = reinterpret(FD{T, f}, x.i-y.i)

# wide multiplication
Base.@pure function widemul(x::FD{<:Any, f}, y::FD{<:Any, g}) where {f, g}
    i = widemul(x.i, y.i)
    reinterpret(FD{typeof(i), f + g}, i)
end
Base.@pure function widemul(x::FD{T, f}, y::Integer) where {T, f}
    i = widemul(x.i, y)
    reinterpret(FD{typeof(i), f}, i)
end
Base.@pure widemul(x::Integer, y::FD) = widemul(y, x)

"""
    _round_to_even(quotient, remainder, divisor)

Round `quotient + remainder / divisor` to the nearest even integer, given that
`0 ≤ remainder < divisor` or `0 ≥ remainder > divisor`. (This assumption is
satisfied by the return value of `fldmod` in all cases, and the return value of
`divrem` in cases where `divisor` is known to be positive.)
"""
function _round_to_even(quotient::T, remainder::T, divisor::T) where {T <: Integer}
    halfdivisor = divisor >> 1
    if iseven(divisor) && remainder == halfdivisor
        ifelse(iseven(quotient), quotient, quotient + one(quotient))
    elseif abs(remainder) > abs(halfdivisor)
        quotient + one(quotient)
    else
        quotient
    end
end
_round_to_even(q, r, d) = _round_to_even(promote(q, r, d)...)

# multiplication rounds to nearest even representation
# TODO: can we use floating point to speed this up? after we build a
# correctness test suite.
function *(x::FD{T, f}, y::FD{T, f}) where {T, f}
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmod(widemul(x.i, y.i), powt)
    reinterpret(FD{T, f}, _round_to_even(quotient, remainder, powt))
end

# these functions are needed to avoid InexactError when converting from the
# integer type
*(x::Integer, y::FD{T, f}) where {T, f} = reinterpret(FD{T, f}, T(x * y.i))
*(x::FD{T, f}, y::Integer) where {T, f} = reinterpret(FD{T, f}, T(x.i * y))

function /(x::FD{T, f}, y::FD{T, f}) where {T, f}
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmod(widemul(x.i, powt), y.i)
    reinterpret(FD{T, f}, T(_round_to_even(quotient, remainder, y.i)))
end

# These functions allow us to perform division with integers outside of the range of the
# FixedDecimal.
function /(x::Integer, y::FD{T, f}) where {T, f}
    powt = coefficient(FD{T, f})
    powtsq = widemul(powt, powt)
    quotient, remainder = fldmod(widemul(x, powtsq), y.i)
    reinterpret(FD{T, f}, T(_round_to_even(quotient, remainder, y.i)))
end

function /(x::FD{T, f}, y::Integer) where {T, f}
    quotient, remainder = fldmod(x.i, y)
    reinterpret(FD{T, f}, T(_round_to_even(quotient, remainder, y)))
end

# integerification
trunc(x::FD{T, f}) where {T, f} = FD{T, f}(div(x.i, coefficient(FD{T, f})))
floor(x::FD{T, f}) where {T, f} = FD{T, f}(fld(x.i, coefficient(FD{T, f})))

# TODO: round with number of digits; should be easy
function round(x::FD{T, f}, ::RoundingMode{:Nearest}=RoundNearest) where {T, f}
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmod(x.i, powt)
    FD{T, f}(_round_to_even(quotient, remainder, powt))
end
function ceil(x::FD{T, f}) where {T, f}
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmod(x.i, powt)
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
required_precision(::Integer)

# https://github.com/JuliaLang/julia/pull/27908
if VERSION < v"0.7.0-beta.183"
    required_precision(n::Integer) = ndigits(n, 2) - trailing_zeros(n)
else
    required_precision(n::Integer) = ndigits(n, base=2) - trailing_zeros(n)
end

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
    @eval ($fn(::Type{TI}, x::FD)::TI) where {TI <: Integer} = $fn(x)

    # round/trunc/ceil/flooring to FD; generic
    @eval function $fn(::Type{FD{T, f}}, x::Real) where {T, f}
        powt = coefficient(FD{T, f})
        # Use machine Float64 if possible, but fall back to BigFloat if we need
        # more precision. 4f bits suffices.
        val = _apply_exact_float($(Symbol(fn, "mul")), T, x, powt)
        reinterpret(FD{T, f}, val)
    end
end
function round(::Type{TI}, x::FD, ::RoundingMode{:Nearest}=RoundNearest) where {TI <: Integer}
    convert(TI, round(x))::TI
end
function round(::Type{FD{T, f}}, x::Real, ::RoundingMode{:Nearest}=RoundNearest) where {T, f}
    reinterpret(FD{T, f}, round(T, x * coefficient(FD{T, f})))
end

# needed to avoid ambiguity
function round(::Type{FD{T, f}}, x::Rational, ::RoundingMode{:Nearest}=RoundNearest) where {T, f}
    reinterpret(FD{T, f}, round(T, x * coefficient(FD{T, f})))
end

# conversions and promotions
function convert(::Type{FD{T, f}}, x::Integer) where {T, f}
    reinterpret(FD{T, f}, T(widemul(x, coefficient(FD{T, f}))))
end

convert(::Type{T}, x::AbstractFloat) where {T <: FD} = round(T, x)

function convert(::Type{FD{T, f}}, x::Rational) where {T, f}
    powt = coefficient(FD{T, f})
    reinterpret(FD{T, f}, T(x * powt))::FD{T, f}
end

function convert(::Type{FD{T, f}}, x::FD{U, g}) where {T, f, U, g}
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
    @eval $remfn(x::T, y::T) where {T <: FD} = reinterpret(T, $remfn(x.i, y.i))
end
for divfn in [:div, :fld, :fld1]
    @eval $divfn(x::T, y::T) where {T <: FD} = $divfn(x.i, y.i)
end

convert(::Type{AbstractFloat}, x::FD) = convert(floattype(typeof(x)), x)
function convert(::Type{TF}, x::FD{T, f}) where {TF <: AbstractFloat, T, f}
    convert(TF, x.i / coefficient(FD{T, f}))::TF
end

function convert(::Type{TF}, x::FD{T, f}) where {TF <: BigFloat, T, f}
    convert(TF, BigInt(x.i) / BigInt(coefficient(FD{T, f})))::TF
end

function convert(::Type{TI}, x::FD{T, f}) where {TI <: Integer, T, f}
    isinteger(x) || throw(InexactError(:convert, TI, x))
    convert(TI, div(x.i, coefficient(FD{T, f})))::TI
end

function convert(::Type{TR}, x::FD{T, f}) where {TR <: Rational, T, f}
    convert(TR, x.i // coefficient(FD{T, f}))::TR
end

(::Type{T})(x::FD) where {T<:Union{AbstractFloat,Integer,Rational}} = convert(T, x)

promote_rule(::Type{FD{T, f}}, ::Type{<:Integer}) where {T, f} = FD{T, f}
promote_rule(::Type{<:FD}, ::Type{TF}) where {TF <: AbstractFloat} = TF
promote_rule(::Type{<:FD}, ::Type{Rational{TR}}) where {TR} = Rational{TR}

# TODO: decide if these are the right semantics;
# right now we pick the bigger int type and the bigger decimal point
Base.@pure function promote_rule(::Type{FD{T, f}}, ::Type{FD{U, g}}) where {T, f, U, g}
    FD{promote_type(T, U), max(f, g)}
end

# comparison
==(x::T, y::T) where {T <: FD} = x.i == y.i
 <(x::T, y::T) where {T <: FD} = x.i  < y.i
<=(x::T, y::T) where {T <: FD} = x.i <= y.i

# predicates and traits
isinteger(x::FD{T, f}) where {T, f} = rem(x.i, coefficient(FD{T, f})) == 0
typemin(::Type{FD{T, f}}) where {T, f} = reinterpret(FD{T, f}, typemin(T))
typemax(::Type{FD{T, f}}) where {T, f}= reinterpret(FD{T, f}, typemax(T))
eps(::Type{T}) where {T <: FD} = reinterpret(T, 1)
eps(x::FD) = eps(typeof(x))
floatmin(::Type{T}) where {T <: FD} = eps(T)
floatmax(::Type{T}) where {T <: FD} = typemax(T)

# printing
function print(io::IO, x::FD{T, 0}) where T
    print(io, x.i)
end
function print(io::IO, x::FD{T, f}) where {T, f}
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

function show(io::IO, x::FD{T, f}) where {T, f}
    iscompact = get(io, :compact, false)
    if !iscompact
        print(io, "FixedDecimal{$T,$f}(")
    end
    print(io, x)
    if !iscompact
        print(io, ')')
    end
end

# parsing

"""
    RoundThrows

Raises an `InexactError` if any rounding is necessary.
"""
const RoundThrows = RoundingMode{:Throw}()

function parse(::Type{FD{T, f}}, str::AbstractString, mode::RoundingMode=RoundNearest) where {T, f}
    if !(mode in [RoundThrows, RoundNearest, RoundToZero])
        throw(ArgumentError("Unhandled rounding mode $mode"))
    end

    # Parse exponent information
    exp_index = something(findfirst(==('e'), str), 0)
    if exp_index > 0
        exp = parse(Int, str[(exp_index + 1):end])
        sig_end = exp_index - 1
    else
        exp = 0
        sig_end = lastindex(str)
    end

    # Remove the decimal place from the string
    sign = T(first(str) == '-' ? -1 : 1)
    dec_index = something(findfirst(==('.'), str), 0)
    sig_start = sign < 0 ? 2 : 1
    if dec_index > 0
        int_str = str[sig_start:(dec_index - 1)] * str[(dec_index + 1):sig_end]
        exp -= sig_end - dec_index
    else
        int_str = str[sig_start:sig_end]
    end

    # Split the integer string into the value we can represent inside the FixedDecimal and
    # the remaining digits we'll use during rounding
    int_end = lastindex(int_str)
    pivot = int_end + exp - (-f)

    a = rpad(int_str[1:min(pivot, int_end)], pivot, '0')
    b = lpad(int_str[max(pivot, 1):int_end], int_end - pivot + 1, '0')

    # Parse the strings
    val = isempty(a) ? T(0) : sign * parse(T, a)
    if !isempty(b) && any(!isequal('0'), b[2:end])
        if mode == RoundThrows
            throw(InexactError(:parse, FD{T, f}, str))
        elseif mode == RoundNearest
            val += sign * parse_round(T, b, mode)
        end
    end

    reinterpret(FD{T, f}, val)
end

function parse_round(::Type{T}, fractional::AbstractString, ::RoundingMode{:Nearest}) where T
    # Note: parsing each digit individually ensures we don't run into an OverflowError
    digits = Int8[parse(Int8, d) for d in fractional]
    for i in length(digits):-1:2
        if digits[i] > 5 || digits[i] == 5 && isodd(digits[i - 1])
            if i - 1 == 1
                return T(1)
            else
                digits[i - 1] += 1
            end
        end
    end
    return T(0)
end


"""
    max_exp10(T)

The highest value of `x` which does not result in an overflow when evaluating `T(10)^x`. For
types of `T` that do not overflow -1 will be returned.
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

"""
    coefficient(::Type{FD{T, f}}) -> T

Compute `10^f` as an Integer without overflow. Note that overflow will not occur for any
constructable `FD{T, f}`.
"""
coefficient(::Type{FD{T, f}}) where {T, f} = T(10)^f
coefficient(fd::FD{T, f}) where {T, f} = coefficient(FD{T, f})
value(fd::FD) = fd.i

# for generic hashing
decompose(fd::FD) = decompose(Rational(fd))

end
