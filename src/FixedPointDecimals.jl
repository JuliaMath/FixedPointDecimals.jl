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

using Compat

import Base: reinterpret, zero, one, abs, sign, ==, <, <=, +, -, /, *, div, rem, divrem,
             fld, mod, fldmod, fld1, mod1, fldmod1, isinteger, typemin, typemax,
             realmin, realmax, print, show, string, convert, parse, promote_rule, min, max,
             trunc, round, floor, ceil, eps, float, widemul

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

        $fnname{I, T <: Number}(::Type{I}, x::T, y::T) = $fn(I, x * y)

        $fnname{I}(::Type{I}, x::Number, y::Number) = $fnname(I, promote(x, y)...)
    end

    if fn === :trunc
        # trunc a little different, implement in terms of floor
        @eval function $fnname{I, T <: FMAFloat}(::Type{I}, x::T, y::T)
            copysign(floormul(I, abs(x), abs(y)), x*y)
        end
    else
        # floor and ceil can be implemented the same way
        @eval function $fnname{I, T <: FMAFloat}(::Type{I}, x::T, y::T)
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
immutable FixedDecimal{T <: Integer, f} <: Real
    i::T

    # inner constructor
    function Base.reinterpret{T, f}(::Type{FixedDecimal{T, f}}, i::Integer)
        n = max_exp10(T)
        if f >= 0 && (n < 0 || f <= n)
            new{T, f}(i % T)
        else
            throw(ArgumentError(
                "Requested number of decimal places $f exceeds the max allowed for the " *
                "storage type $T: [0, $(max_exp10(T))]"
            ))
        end
    end
end

const FD = FixedDecimal

floattype{T<:Union{Int8, UInt8, Int16, UInt16}, f}(::Type{FD{T, f}}) = Float32
floattype{T<:Integer, f}(::Type{FD{T, f}}) = Float64
floattype{f}(::Type{FD{BigInt, f}}) = BigFloat

# basic operators
-{T, f}(x::FD{T, f}) = reinterpret(FD{T, f}, -x.i)
abs{T, f}(x::FD{T, f}) = reinterpret(FD{T, f}, abs(x.i))

+{T, f}(x::FD{T, f}, y::FD{T, f}) = reinterpret(FD{T, f}, x.i+y.i)
-{T, f}(x::FD{T, f}, y::FD{T, f}) = reinterpret(FD{T, f}, x.i-y.i)

# wide multiplication
Base.@pure function widemul{T, f, U, g}(x::FD{T, f}, y::FD{U, g})
    i = widemul(x.i, y.i)
    reinterpret(FD{typeof(i), f + g}, i)
end
Base.@pure function widemul{T, f}(x::FD{T, f}, y::Integer)
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
function _round_to_even{T <: Integer}(quotient::T, remainder::T, divisor::T)
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
function *{T, f}(x::FD{T, f}, y::FD{T, f})
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmod(widemul(x.i, y.i), powt)
    reinterpret(FD{T, f}, _round_to_even(quotient, remainder, powt))
end

# these functions are needed to avoid InexactError when converting from the
# integer type
*{T, f}(x::Integer, y::FD{T, f}) = reinterpret(FD{T, f}, T(x * y.i))
*{T, f}(x::FD{T, f}, y::Integer) = reinterpret(FD{T, f}, T(x.i * y))

function /{T, f}(x::FD{T, f}, y::FD{T, f})
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmod(widemul(x.i, powt), y.i)
    reinterpret(FD{T, f}, T(_round_to_even(quotient, remainder, y.i)))
end

# These functions allow us to perform division with integers outside of the range of the
# FixedDecimal.
function /{T, f}(x::Integer, y::FD{T, f})
    powt = coefficient(FD{T, f})
    powtsq = widemul(powt, powt)
    quotient, remainder = fldmod(widemul(x, powtsq), y.i)
    reinterpret(FD{T, f}, T(_round_to_even(quotient, remainder, y.i)))
end

function /{T, f}(x::FD{T, f}, y::Integer)
    quotient, remainder = fldmod(x.i, y)
    reinterpret(FD{T, f}, T(_round_to_even(quotient, remainder, y)))
end

# integerification
trunc{T, f}(x::FD{T, f}) = FD{T, f}(div(x.i, coefficient(FD{T, f})))
floor{T, f}(x::FD{T, f}) = FD{T, f}(fld(x.i, coefficient(FD{T, f})))

# TODO: round with number of digits; should be easy
function round{T, f}(x::FD{T, f}, ::RoundingMode{:Nearest}=RoundNearest)
    powt = coefficient(FD{T, f})
    quotient, remainder = fldmod(x.i, powt)
    FD{T, f}(_round_to_even(quotient, remainder, powt))
end
function ceil{T, f}(x::FD{T, f})
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
required_precision(n::Integer) = ndigits(n, 2) - trailing_zeros(n)

"""
    _apply_exact_float(f, T, x::Real, i::Integer)

Compute `f(T, x, i)::T` but avoiding possible loss of precision from an
intermediate conversion of `i` to a floating point type by instead using a
`BigFloat` with sufficient precision if necessary.
"""
function _apply_exact_float{T}(f, ::Type{T}, x::FMAFloat, i::Integer)
    prec = required_precision(i)
    if prec > 53
        setprecision(BigFloat, prec) do
            f(T, x, BigFloat(i))
        end
    else
        f(T, x, Float64(i))
    end
end

_apply_exact_float{T}(f, ::Type{T}, x::Real, i::Integer) = f(T, x, i)

for fn in [:trunc, :floor, :ceil]
    @eval $fn{TI <: Integer}(::Type{TI}, x::FD)::TI = $fn(x)

    # round/trunc/ceil/flooring to FD; generic
    @eval function $fn{T, f}(::Type{FD{T, f}}, x::Real)
        powt = coefficient(FD{T, f})
        # Use machine Float64 if possible, but fall back to BigFloat if we need
        # more precision. 4f bits suffices.
        val = _apply_exact_float($(Symbol(fn, "mul")), T, x, powt)
        reinterpret(FD{T, f}, val)
    end
end
function round{TI <: Integer}(::Type{TI}, x::FD, ::RoundingMode{:Nearest}=RoundNearest)::TI
    round(x)
end
function round{T, f}(::Type{FD{T, f}}, x::Real, ::RoundingMode{:Nearest}=RoundNearest)
    reinterpret(FD{T, f}, round(T, x * coefficient(FD{T, f})))
end

# needed to avoid ambiguity
function round{T, f}(::Type{FD{T, f}}, x::Rational, ::RoundingMode{:Nearest}=RoundNearest)
    reinterpret(FD{T, f}, round(T, x * coefficient(FD{T, f})))
end

# conversions and promotions
function convert{T, f}(::Type{FD{T, f}}, x::Integer)
    reinterpret(FD{T, f}, T(widemul(x, coefficient(FD{T, f}))))
end

convert{T <: FD}(::Type{T}, x::AbstractFloat) = round(T, x)

function convert{T, f}(::Type{FD{T, f}}, x::Rational)::FD{T, f}
    powt = coefficient(FD{T, f})
    reinterpret(FD{T, f}, T(x * powt))
end

function convert{T, f, U, g}(::Type{FD{T, f}}, x::FD{U, g})
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
    @eval $remfn{T <: FD}(x::T, y::T) = reinterpret(T, $remfn(x.i, y.i))
end
for divfn in [:div, :fld, :fld1]
    @eval $divfn{T <: FD}(x::T, y::T) = $divfn(x.i, y.i)
end

convert(::Type{AbstractFloat}, x::FD) = convert(floattype(typeof(x)), x)
function convert{TF <: AbstractFloat, T, f}(::Type{TF}, x::FD{T, f})::TF
    x.i / coefficient(FD{T, f})
end

function convert{TF <: BigFloat, T, f}(::Type{TF}, x::FD{T, f})::TF
    BigInt(x.i) / BigInt(coefficient(FD{T, f}))
end

function convert{TI <: Integer, T, f}(::Type{TI}, x::FD{T, f})::TI
    isinteger(x) || throw(InexactError(:convert, TI, x))
    div(x.i, coefficient(FD{T, f}))
end

convert{TR <: Rational, T, f}(::Type{TR}, x::FD{T, f})::TR = x.i // coefficient(FD{T, f})

promote_rule{T, f, TI <: Integer}(::Type{FD{T, f}}, ::Type{TI}) = FD{T, f}
promote_rule{T, f, TF <: AbstractFloat}(::Type{FD{T, f}}, ::Type{TF}) = TF
promote_rule{T, f, TR}(::Type{FD{T, f}}, ::Type{Rational{TR}}) = Rational{TR}

# TODO: decide if these are the right semantics;
# right now we pick the bigger int type and the bigger decimal point
Base.@pure promote_rule{T, f, U, g}(::Type{FD{T, f}}, ::Type{FD{U, g}}) =
    FD{promote_type(T, U), max(f, g)}

# comparison
=={T <: FD}(x::T, y::T) = x.i == y.i
 <{T <: FD}(x::T, y::T) = x.i  < y.i
<={T <: FD}(x::T, y::T) = x.i <= y.i

# predicates and traits
isinteger{T, f}(x::FD{T, f}) = rem(x.i, coefficient(FD{T, f})) == 0
typemin{T, f}(::Type{FD{T, f}}) = reinterpret(FD{T, f}, typemin(T))
typemax{T, f}(::Type{FD{T, f}}) = reinterpret(FD{T, f}, typemax(T))
eps{T <: FD}(::Type{T}) = reinterpret(T, 1)
eps(x::FD) = eps(typeof(x))
realmin{T <: FD}(::Type{T}) = eps(T)
realmax{T <: FD}(::Type{T}) = typemax(T)

# printing
function print{T}(io::IO, x::FD{T, 0})
    print(io, x.i)
end
function print{T, f}(io::IO, x::FD{T, f})
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

function show{T, f}(io::IO, x::FD{T, f})
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

function parse{T, f}(::Type{FD{T, f}}, str::AbstractString, mode::RoundingMode=RoundNearest)
    if !(mode in [RoundThrows, RoundNearest, RoundToZero])
        throw(ArgumentError("Unhandled rounding mode $mode"))
    end

    # Parse exponent information
    exp_index = findfirst(equalto('e'), str)
    if exp_index > 0
        exp = parse(Int, str[(exp_index + 1):end])
        sig_end = exp_index - 1
    else
        exp = 0
        sig_end = endof(str)
    end

    # Remove the decimal place from the string
    sign = T(first(str) == '-' ? -1 : 1)
    dec_index = findfirst(equalto('.'), str)
    sig_start = sign < 0 ? 2 : 1
    if dec_index > 0
        int_str = str[sig_start:(dec_index - 1)] * str[(dec_index + 1):sig_end]
        exp -= sig_end - dec_index
    else
        int_str = str[sig_start:sig_end]
    end

    # Split the integer string into the value we can represent inside the FixedDecimal and
    # the remaining digits we'll use during rounding
    int_end = endof(int_str)
    pivot = int_end + exp - (-f)

    a = rpad(int_str[1:min(pivot, int_end)], pivot, '0')
    b = lpad(int_str[max(pivot, 1):int_end], int_end - pivot + 1, '0')

    # Parse the strings
    val = isempty(a) ? T(0) : sign * parse(T, a)
    if !isempty(b) && any(collect(b[2:end]) .!= '0')
        if mode == RoundThrows
            throw(InexactError(:parse, FD{T, f}, str))
        elseif mode == RoundNearest
            val += sign * parse_round(T, b, mode)
        end
    end

    reinterpret(FD{T, f}, val)
end

function parse_round{T}(::Type{T}, fractional::AbstractString, ::RoundingMode{:Nearest})
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
function max_exp10{T <: Integer}(::Type{T})
    applicable(typemax, T) || return -1
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

"""
    coefficient(::Type{FD{T, f}}) -> T

Compute `10^f` as an Integer without overflow. Note that overflow will not occur for any
constructable `FD{T, f}`.
"""
coefficient{T, f}(::Type{FD{T, f}}) = T(10)^f
coefficient{T, f}(fd::FD{T, f}) = coefficient(FD{T, f})
value(fd::FD) = fd.i

end
