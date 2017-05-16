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
             trunc, round, floor, ceil, eps, float, widemul, exp10

import Base.Checked: checked_mul

const IEEEFloat = Union{Float16, Float32, Float64}

for fn in [:trunc, :floor, :ceil]
    fnname = Symbol(fn, "mul")

    @eval begin
        @doc """
            $($fnname)(x, y) :: Integer

        Compute `$($fn)(x * y)`. For floating point values, this function can
        be more accurate than `$($fn)(x * y)`. The boundary behavior of this
        function (e.g. at large values of `x`, `y`) is untested and possibly
        incorrect.
        """ function $fnname end

        $fnname{T <: Number}(x::T, y::T) = $fn(x * y)

        $fnname(x::Number, y::Number) = $fnname(promote(x, y)...)
    end

    if fn === :trunc
        # trunc a little different, implement in terms of floor
        @eval function $fnname{T <: IEEEFloat}(x::T, y::T)
            copysign(floormul(abs(x), abs(y)), x*y)
        end
    else
        # floor and ceil can be implemented the same way
        @eval function $fnname{T <: IEEEFloat}(x::T, y::T)
            a = x * y
            b = fma(x, y, -a)
            ifelse(isinteger(a), a + $fn(b), $fn(a))
        end
    end
end

"""
    FixedDecimal{I <: Integer, f::Int}

A fixed-point decimal type backed by integral type `I`, with `f` digits after
the decimal point stored.
"""
immutable FixedDecimal{T <: Integer, f} <: Real
    i::T

    # internal constructor
    Base.reinterpret{T, f}(::Type{FixedDecimal{T, f}}, i::Integer) =
        new{T, f}(i % T)
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

function _round_to_even(quotient, remainder, powt)
    if powt == 1
        quotient
    else
        halfpowt = powt >> 1
        if remainder == halfpowt
            ifelse(iseven(quotient), quotient, quotient + one(quotient))
        elseif remainder < halfpowt
            quotient
        else
            quotient + one(quotient)
        end
    end
end

# multiplication rounds to nearest even representation
# TODO: can we use floating point to speed this up? after we build a
# correctness test suite.
function *{T, f}(x::FD{T, f}, y::FD{T, f})
    powt = T(10)^f
    quotient, remainder = fldmod(Base.widemul(x.i, y.i), powt)
    reinterpret(FD{T, f}, _round_to_even(quotient, remainder, powt))
end

# these functions are needed to avoid InexactError when converting from the
# integer type
*{T, f}(x::Integer, y::FD{T, f}) = reinterpret(FD{T, f}, T(x * y.i))
*{T, f}(x::FD{T, f}, y::Integer) = reinterpret(FD{T, f}, T(x.i * y))

# TODO. this is probably wrong sometimes.
function /{T, f}(x::FD{T, f}, y::FD{T, f})
    powt = T(10)^f
    quotient, remainder = divrem(x.i, y.i)
    reinterpret(FD{T, f}, quotient * powt + round(T, remainder / y.i * powt))
end

# these functions are needed to avoid InexactError when converting from the integer type
function /{T, f}(x::Integer, y::FD{T, f})
    powt = T(10)^f
    xi, yi = checked_mul(x, powt), y.i
    quotient, remainder = divrem(xi, yi)
    reinterpret(FD{T, f}, quotient * powt + round(T, remainder / yi * powt))
end

function /{T, f}(x::FD{T, f}, y::Integer)
    powt = T(10)^f
    xi, yi = x.i, checked_mul(y, powt)
    quotient, remainder = divrem(xi, yi)
    reinterpret(FD{T, f}, quotient * powt + round(T, remainder / yi * powt))
end

# integerification
trunc{T, f}(x::FD{T, f}) = FD{T, f}(div(x.i, T(10)^f))
floor{T, f}(x::FD{T, f}) = FD{T, f}(fld(x.i, T(10)^f))
# TODO: round with number of digits; should be easy
function round{T, f}(x::FD{T, f}, ::RoundingMode{:Nearest}=RoundNearest)
    powt = T(10)^f
    quotient, remainder = fldmod(x.i, powt)
    FD{T, f}(_round_to_even(quotient, remainder, powt))
end
function ceil{T, f}(x::FD{T, f})
    powt = T(10)^f
    quotient, remainder = fldmod(x.i, powt)
    if remainder > 0
        FD{T, f}(quotient + one(quotient))
    else
        FD{T, f}(quotient)
    end
end

for fn in [:trunc, :floor, :ceil]
    @eval $fn{TI <: Integer}(::Type{TI}, x::FD)::TI = $fn(x)

    # round/trunc/ceil/flooring to FD; generic
    # TODO. we may need to check overflow and boundary conditions here.
    @eval function $fn{T, f}(::Type{FD{T, f}}, x::Real)
        powt = T(10)^f
        val = trunc(T, $(Symbol(fn, "mul"))(x, powt))
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
function round{T, f}(::Type{FD{T, f}}, x::Rational,
                     ::RoundingMode{:Nearest}=RoundNearest)
    reinterpret(FD{T, f}, round(T, T(10)^f * x))
end

# conversions and promotions
convert{T, f}(::Type{FD{T, f}}, x::Integer) =
    reinterpret(FD{T, f}, round(T, Base.widemul(T(x), T(10)^f)))

convert{T <: FD}(::Type{T}, x::AbstractFloat) = round(T, x)

function convert{T, f}(::Type{FD{T, f}}, x::Rational)::FD{T, f}
    powt = T(10)^f
    num::T, den::T = numerator(x), denominator(x)
    g = gcd(powt, den)
    powt = div(powt, g)
    den = div(den, g)
    reinterpret(FD{T, f}, powt * num) / FD{T, f}(den)
end

function convert{T, f, U, g}(::Type{FD{T, f}}, x::FD{U, g})
    if f ≥ g
        reinterpret(FD{T, f}, convert(T, Base.widemul(T(10)^(f-g), x.i)))
    else
        sf = T(10)^(g - f)
        q, r = divrem(x.i, sf)
        if r ≠ 0
            throw(InexactError())
        else
            reinterpret(FD{T, f}, convert(T, q))
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
convert{TF <: AbstractFloat, T, f}(::Type{TF}, x::FD{T, f})::TF =
    x.i / TF(10)^f

function convert{TI <: Integer, T, f}(::Type{TI}, x::FD{T, f})::TI
    isinteger(x) || throw(InexactError())
    div(x.i, T(10)^f)
end

convert{TR<:Rational,T,f}(::Type{TR}, x::FD{T, f})::TR =
    x.i // T(10)^f

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
isinteger{T, f}(x::FD{T, f}) = rem(x.i, T(10)^f) == 0
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
    exp_index = findfirst(str, 'e')
    if exp_index > 0
        exp = parse(Int, str[(exp_index + 1):end])
        sig_end = exp_index - 1
    else
        exp = 0
        sig_end = endof(str)
    end

    # Remove the decimal place from the string
    sign = T(first(str) == '-' ? -1 : 1)
    dec_index = findfirst(str, '.')
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
            throw(InexactError())
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

The highest value of `x` which does not result in an overflow when evaluating `T(10)^x`.
"""
function max_exp10{T <: Integer}(::Type{T})
    length(digits(typemax(T))) - 1
end

"""
    exp10(::Type{FD{T, f}})

Compute `10^f` as an Integer without overflow. The resulting type will be an integer of type
T or wider.
"""
@generated function exp10{T <: Integer, f}(::Type{FD{T, f}})
    P = T
    while P != BigInt && f > max_exp10(P)
        P = widen(P)
    end
    quote
        $(P(10)^f)
    end
end

coefficient{T, f}(::Type{FD{T, f}}) = exp10(FD{T, f})
coefficient{T, f}(fd::FD{T, f}) = coefficient(FD{T, f})
value(fd::FD) = fd.i

end
