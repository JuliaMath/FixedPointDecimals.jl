using Parsers
using Parsers: AbstractConf, Result

"""
    RoundThrows

Raises an `InexactError` if any rounding is necessary.
"""
const RoundThrows = RoundingMode{:Throw}()

# make our own conf struct to avoid specializing Parsers.typeparser on each unique precision value
struct FixedDecimalConf{T<:Integer} <: AbstractConf{T}
    f::Int
end
# This overload says that when parsing a FixedDecimal type, use our new custom FixedDecimalConf type
Parsers.conf(::Type{FixedDecimal{T,f}}, opts::Parsers.Options, kw...) where {T<:Integer,f} = FixedDecimalConf{T}(f)
# Because the value returned from our `typeparser` isn't a FixedDecimal, we overload here to show we're returning an integer type
Parsers.returntype(::Type{FixedDecimal{T,f}}) where {T,f} = T
# This overload allows us to take the Result{IntegerType} returned from typeparser and turn it into a FixedDecimal Result
function Parsers.result(FD::Type{FixedDecimal{T,f}}, res::Parsers.Result{T}) where {T,f}
    return Parsers.invalid(res.code) ? Result{FD}(res.code, res.tlen) :
        Result{FD}(res.code, res.tlen, reinterpret(FD, res.val))
end
# Tell Parsers that we can use our custom typeparser and not rely on Base.tryparse
Parsers.supportedtype(::Type{<:FixedDecimal}) = true

const OPTIONS_ROUND_NEAREST = Parsers.Options(rounding=RoundNearest)
const OPTIONS_ROUND_TO_ZERO = Parsers.Options(rounding=RoundToZero)
const OPTIONS_ROUND_THROWS = Parsers.Options(rounding=nothing)

# TODO: a lookup table per type would be faster
@inline _shift(n::T, decpos) where {T} = T(10)^decpos * n

const _BIGINT1 = BigInt(1)
const _BIGINT2 = BigInt(2)
const _BIGINT10 = BigInt(10)
const _BIGINT_10s = BigInt[] # buffer for "remainders" in _divpow10!, accessed via `Parsers.access_threaded`
const _BIGINT_Rs = BigInt[]  # buffer for "remainders" in _divpow10!, accessed via `Parsers.access_threaded`

for T in (Base.BitSigned_types..., Base.BitUnsigned_types...)
    let bytes = Tuple(codeunits(string(typemax(T))))
        # The number of digits an integer of type T can hold
        @eval _maxintdigits(::Type{$T}) = $(length(bytes))
    end
end

# All `v`s are non-negative
function _unsafe_convert_int(::Type{T}, v::V) where {T<:Integer,V<:Integer}
    return sizeof(T) > sizeof(V) ? T(v) :
           sizeof(T) < sizeof(V) ? unsafe_trunc(T, v) :
           Base.bitcast(T, v)
end
_unsafe_convert_int(::Type{T}, v::BigInt) where {T<:Integer} = unsafe_trunc(T, v)
_unsafe_convert_int(::Type{T}, v::T) where {T<:Integer} = v

function _check_overflows(::Type{T}, v::BigInt, neg::Bool) where {T<:Integer}
    return neg ? -v < typemin(T) : v > typemax(T)
end
function _check_overflows(::Type{T}, v::V, neg::Bool) where {T<:Integer,V<:Union{UInt64,UInt128}}
    return sizeof(T) <= sizeof(V) && (neg ? v > _unsafe_convert_int(V, typemax(T)) + one(V) : v > typemax(T))
end
_check_overflows(::Type{T}, v::T, neg::Bool) where {T <: Integer} = false

# `x = div(x, 10^pow, mode)`; may set code |= INEXACT for RoundThrows
# x is non-negative, pow is >= 1
# `!` to signal we mutate bigints in-place
function _divpow10!(x::T, code, pow, mode::RoundingMode) where {T}
    return div(x, _shift(one(T), pow), mode), code
end
function _divpow10!(x::T, code, pow, ::RoundingMode{:Throw}) where {T}
    q, r = divrem(x, _shift(one(T), pow))
    r == 0 || (code |= Parsers.INEXACT)
    return q, code
end
function _divpow10!(x::BigInt, code, pow, ::RoundingMode{:Nearest})
    # adapted from https://github.com/JuliaLang/julia/blob/112554e1a533cebad4cb0daa27df59636405c075/base/div.jl#L217
    @inbounds r = Parsers.access_threaded(() -> (@static VERSION > v"1.5" ? BigInt(; nbits=256) : BigInt()), _BIGINT_Rs)  # we must not yield here!
    @inbounds y = Parsers.access_threaded(() -> (@static VERSION > v"1.5" ? BigInt(; nbits=256) : BigInt()), _BIGINT_10s) # we must not yield here!
    Base.GMP.MPZ.set!(y, _BIGINT10)             # y = 10
    Base.GMP.MPZ.pow_ui!(y, pow)                # y = y^pow
    Base.GMP.MPZ.tdiv_qr!(x, r, x, y)           # x, r = divrem(x, y)
    Base.GMP.MPZ.tdiv_q!(y, _BIGINT2)           # y = div(y, 2)
    iseven(x) && Base.GMP.MPZ.add!(y, _BIGINT1) # y = y + iseven(x)
    if r >= y
        Base.GMP.MPZ.add!(x, _BIGINT1)          # x = x + (r >= y)
    end
    return x, code
end
function _divpow10!(x::BigInt, code, pow, ::RoundingMode{:ToZero})
    @inbounds y = Parsers.access_threaded(() -> (@static VERSION > v"1.5" ? BigInt(; nbits=256) : BigInt()), _BIGINT_10s) # we must not yield here!
    Base.GMP.MPZ.set!(y, _BIGINT10) # y = 10
    Base.GMP.MPZ.pow_ui!(y, pow)    # y = y^pow
    Base.GMP.MPZ.tdiv_q!(x, y)      # x = div(x, y)
    return x, code
end

function _divpow10!(x::BigInt, code, pow, ::RoundingMode{:Throw})
    @inbounds y = Parsers.access_threaded(() -> (@static VERSION > v"1.5" ? BigInt(; nbits=256) : BigInt()), _BIGINT_10s) # we must not yield here!
    Base.GMP.MPZ.set!(y, _BIGINT10)   # y = 10
    Base.GMP.MPZ.pow_ui!(y, pow)      # y = y^pow
    Base.GMP.MPZ.tdiv_qr!(x, y, x, y) # x, y = divrem(x, y)
    y == 0 || (code |= Parsers.INEXACT)
    return x, code
end

# Rescale the digits we accumulated so far into the the a an integer representing the decimal
# Note the 2nd argument `FloatType` is used by Parsers.jl for _float_ parsing, but we can ignore in the fixed decimal case
@inline function Parsers.scale(
    conf::FixedDecimalConf{T}, ::Parsers.FloatType, digits::V, exp, neg, code, ndigits, f::F, options::Parsers.Options
) where {T,V,F}
    rounding = something(options.rounding, RoundThrows)
    # Positive: how many trailing zeroes we need to add to our integer
    # Negative: how many digits are past our precision (we need to handle them in rounding)
    decimal_shift = conf.f + exp
    # Number of digits we need to accumulate including any trailigng zeros or digits past our precision
    backing_integer_digits = ndigits + decimal_shift
    may_overflow = backing_integer_digits == _maxintdigits(T)
    if iszero(ndigits)
        # all digits are zero
        i = zero(T)
    # The backing_integer_digits == 0 case is handled in the `else` (it means
    # that all the digits are passed the precision but we might get `1` from rounding)
    elseif backing_integer_digits < 0
        # All digits are past our precision, no overflow possible, but we might get an inexact
        i = zero(T)
        (rounding === RoundThrows) && (code |= Parsers.INEXACT)
    elseif neg && (T <: Unsigned)
        # Unsigned types can't represent negative numbers
        i = _unsafe_convert_int(T, digits)
        code |= Parsers.INVALID
    elseif backing_integer_digits > _maxintdigits(T)
        i = _unsafe_convert_int(T, digits)
        # The number of digits to accumulate is larger than the capacity of T, we overflow
        # We don't check for inexact here because we already have an error
        code |= Parsers.OVERFLOW
    else
        if decimal_shift > 0
            r = _unsafe_convert_int(T, digits)
            i = _shift(r, decimal_shift)
            may_overflow && (r >= i) && (code |= Parsers.OVERFLOW)
        elseif decimal_shift < 0
            if rounding === RoundNearest
                r, code = _divpow10!(digits, code, -decimal_shift, RoundNearest)
            elseif rounding === RoundToZero
                r, code = _divpow10!(digits, code, -decimal_shift, RoundToZero)
            else
                r, code = _divpow10!(digits, code, -decimal_shift, RoundThrows)
            end
            # Now that the digits were rescaled we can check for overflow
            # can happen e.g. if digits were unsigned ints and out type is signed
            may_overflow && _check_overflows(T, r, neg) && (code |= Parsers.OVERFLOW)
            i = _unsafe_convert_int(T, r)
        else
            may_overflow && _check_overflows(T, digits, neg) && (code |= Parsers.OVERFLOW)
            i = _unsafe_convert_int(T, digits)
        end
    end
    out = ifelse(neg, -i, i)
    return (out, code)
end

# If we only saw integer digits and not fractional or exponent digits, we just call scale with exp of 0
# To handle type conversions and overflow checks etc.
@inline function Parsers.noscale(conf::FixedDecimalConf{T}, digits::Integer, neg::Bool, code, ndigits, f::F, options::Parsers.Options) where {T,F}
    FT = Parsers.FLOAT64 # not used by FixedDecimal parser
    exp = 0
    return Parsers.scale(conf, FT, digits, exp, neg, code, ndigits, f, options)
end

# This hooks into the floating point parsing machinery from Parsers.jl, where we also accumulate
# all the digits and note the effective exponent before we do "scaling" -- for FixedDecimals,
# the scaling means padding the backing integer with zeros or rounding them as necessary.
# We overloaded the "scale" and "noscale" methods to produce backing integers for FixedDecimals.
# We return a value of T -- i.e. the _integer_ backing the FixedDecimal, the reintrpret needs to happen later
@inline function Parsers.typeparser(conf::FixedDecimalConf{T}, source, pos, len, b, code, pl, options::Parsers.Options) where {T<:Integer}
    if !(options.rounding in (nothing, RoundNearest, RoundToZero, RoundThrows))
        throw(ArgumentError("Unhandled rounding mode $(options.rounding)"))
    end

    startpos = pos
    # begin parsing
    neg = b == UInt8('-')
    if neg || b == UInt8('+')
        pos += 1
        Parsers.incr!(source)
        if Parsers.eof(source, pos, len)
            code |= Parsers.INVALID | Parsers.EOF
            x = zero(T)
            @goto done
        end
        b = Parsers.peekbyte(source, pos)
    else
        # Check if the input is empty
        if Parsers.eof(source, pos, len)
            code |= Parsers.INVALID | Parsers.EOF
            x = zero(T)
            @goto done
        end
    end

    if (b - UInt8('0')) <= 0x09 || b == options.decimal
        x, code, pos = Parsers.parsedigits(conf, source, pos, len, b, code, options, UInt64(0), neg, startpos, true, 0, nothing)
    else
        x = zero(T)
        code |= Parsers.INVALID
    end
    @label done
    return pos, code, Parsers.PosLen(pl.pos, pos - pl.pos), x
end

function _base_parse(::Type{FD{T, f}}, source::AbstractString, mode::RoundingMode=RoundNearest) where {T, f}
    if !(mode in (RoundThrows, RoundNearest, RoundToZero))
        throw(ArgumentError("Unhandled rounding mode $mode"))
    end

    bytes = codeunits(source)
    options = mode === RoundNearest ? OPTIONS_ROUND_NEAREST :
        mode === RoundToZero ? OPTIONS_ROUND_TO_ZERO :
        OPTIONS_ROUND_THROWS
    res = Parsers.xparse2(FD{T, f}, bytes, 1, length(bytes), options)
    return res
end

function Base.tryparse(::Type{FD{T, f}}, source::AbstractString, mode::RoundingMode=RoundNearest) where {T, f}
    isempty(source) && return nothing
    res = _base_parse(FD{T, f}, source, mode)
    # If we didn't reach eof, there was some garbage at the end of the string after something that looked like a number
    return (Parsers.eof(res.code) && Parsers.ok(res.code)) ? res.val : nothing
end

function Base.parse(::Type{FD{T, f}}, source::AbstractString, mode::RoundingMode=RoundNearest) where {T, f}
    isempty(source) && throw(ArgumentError("Empty input is not allowed"))
    res = _base_parse(FD{T, f}, source, mode)
    Parsers.inexact(res.code) && throw(InexactError(:parse, FD{T, f}, source))
    Parsers.overflow(res.code) && throw(OverflowError("overflow parsing $(repr(source)) as $(FD{T, f})"))
    # If we didn't reach eof, there was some garbage at the end of the string after something that looked like a number
    (!Parsers.eof(res.code) || Parsers.invalid(res.code)) && throw(ArgumentError("cannot parse $(repr(source)) as $(FD{T, f})"))
    return res.val
end
