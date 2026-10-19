"""
    RoundThrows

Raises an `InexactError` if any rounding is necessary.
"""
const RoundThrows = RoundingMode{:Throw}()

# Status flags reported by the parser
const _OK       = 0x00
const _INVALID  = 0x01  # malformed input, or a negative value for an unsigned type
const _OVERFLOW = 0x02  # the value doesn't fit the backing integer type
const _INEXACT  = 0x04  # rounding was needed but the rounding mode is `RoundThrows`

_pow10(::Type{T}, n) where {T} = T(10)^n
for T in (Base.BitSigned_types..., Base.BitUnsigned_types..., UInt256)
    let bytes = Tuple(codeunits(string(typemax(T))))
        # The number of digits an integer of type T can hold. Not defined for `UInt256`, which is
        # only here for the lookup table: the parser doesn't support it as a backing type.
        T === UInt256 || @eval _maxintdigits(::Type{$T}) = $(length(bytes))
        # Lookup table of the powers of 10 that fit in T
        pow10s = Tuple(T(10)^i for i in 0:length(bytes)-1)
        @eval @inline function _pow10(::Type{$T}, n)
            # The parser never asks for an `n` outside the table, but fall back to `^` to be safe
            return 0 <= n < $(length(pow10s)) ? @inbounds($pow10s[n + 1]) : $T(10)^n
        end
    end
end

# All `v`s are non-negative
function _unsafe_convert_int(::Type{T}, v::V) where {T<:Integer,V<:Integer}
    return sizeof(T) > sizeof(V) ? T(v) :
           sizeof(T) < sizeof(V) ? unsafe_trunc(T, v) :
           Base.bitcast(T, v)
end
_unsafe_convert_int(::Type{T}, v::T) where {T<:Integer} = v

function _check_overflows(::Type{T}, v::V, neg::Bool) where {T<:Integer,V<:Union{UInt64,UInt128,UInt256}}
    return sizeof(T) <= sizeof(V) && (neg ? v > _unsafe_convert_int(V, typemax(T)) + one(V) : v > typemax(T))
end
_check_overflows(::Type{T}, v::T, neg::Bool) where {T <: Integer} = false

# `div(x, 10^pow)` and `divrem(x, 10^pow)`
_div_pow10(x::T, pow) where {T} = div(x, _pow10(T, pow))
_divrem_pow10(x::T, pow) where {T} = divrem(x, _pow10(T, pow))

# BitIntegers' generic 256-bit division is slow, so for the `UInt256` accumulator we divide by
# `10^pow` in steps of at most `10^19`, each a schoolbook long division over 64-bit limbs.
function _div_u64(x::UInt256, d::UInt64)
    q = zero(UInt256)
    r = UInt64(0)
    for i in 3:-1:0
        cur = (UInt128(r) << 64) | ((x >> (64 * i)) % UInt64)
        if cur < d
            # Skips the division for the leading zero limbs
            r = cur % UInt64
        else
            qi = div(cur, d) % UInt64  # fits since `r < d`
            r = (cur % UInt64) - qi * d
            q |= UInt256(qi) << (64 * i)
        end
    end
    return q
end
function _div_pow10(x::UInt256, pow)
    while pow > 0
        k = min(pow, max_exp10(UInt64))
        x = _div_u64(x, _pow10(UInt64, k))
        pow -= k
    end
    return x
end
function _divrem_pow10(x::UInt256, pow)
    q = _div_pow10(x, pow)
    return q, x - q * _pow10(UInt256, pow)
end

# `x = div(x, 10^pow, mode)`; may set code |= INEXACT for RoundThrows
# x is non-negative, pow is >= 1
_divpow10(x, code, pow, ::RoundingMode{:ToZero}) = _div_pow10(x, pow), code
function _divpow10(x::T, code, pow, mode::RoundingMode{:Nearest}) where {T}
    q, r = _divrem_pow10(x, pow)
    return _round_to_nearest(q, r, _pow10(T, pow), mode), code
end
function _divpow10(x, code, pow, ::RoundingMode{:Throw})
    q, r = _divrem_pow10(x, pow)
    r == 0 || (code |= _INEXACT)
    return q, code
end

# Rescale the accumulated digits into the integer backing a `FixedDecimal{T,f}`.
# The parsed value is `(-1)^neg * digits * 10^exp`, where `digits` has `ndigits` significant
# digits (`ndigits == 0` iff `digits == 0`).
@inline function _scale(::Type{T}, f::Int, digits::V, exp::Int, neg::Bool, ndigits::Int, rounding::RoundingMode) where {T,V}
    code = _OK
    # Positive: how many trailing zeroes we need to add to our integer
    # Negative: how many digits are past our precision (we need to handle them in rounding)
    decimal_shift = f + exp
    # Number of digits we need to accumulate including any trailing zeros or digits past our precision
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
        (rounding === RoundThrows) && (code |= _INEXACT)
    elseif neg && (T <: Unsigned)
        # Unsigned types can't represent negative numbers
        i = zero(T)
        code |= _INVALID
    elseif backing_integer_digits > _maxintdigits(T)
        # The number of digits to accumulate is larger than the capacity of T, we overflow
        # We don't check for inexact here because we already have an error
        i = zero(T)
        code |= _OVERFLOW
    else
        if decimal_shift > 0
            # `digits` has fewer than `_maxintdigits(T)` digits here, so it fits in `T`
            r = _unsafe_convert_int(T, digits)
            if may_overflow
                i, overflowed = Base.mul_with_overflow(r, _pow10(T, decimal_shift))
                overflowed && (code |= _OVERFLOW)
            else
                i = _pow10(T, decimal_shift) * r
            end
        elseif decimal_shift < 0
            if -decimal_shift > _maxaccdigits(V)
                # Only possible when `digits` has one more digit than `_maxaccdigits(V)` and all of
                # them are past our precision. `10^-decimal_shift` doesn't fit in `V`, but
                # `digits < 10^-decimal_shift / 2`, so the result always rounds to zero.
                r = zero(V)
                (rounding === RoundThrows) && (code |= _INEXACT)
            else
                r, code = _divpow10(digits, code, -decimal_shift, rounding)
            end
            # Now that the digits were rescaled we can check for overflow
            # can happen e.g. if digits were unsigned ints and out type is signed
            may_overflow && _check_overflows(T, r, neg) && (code |= _OVERFLOW)
            i = _unsafe_convert_int(T, r)
        else
            may_overflow && _check_overflows(T, digits, neg) && (code |= _OVERFLOW)
            i = _unsafe_convert_int(T, digits)
        end
    end
    out = ifelse(neg, -i, i)
    return (out, code)
end

@inline _isdigit(b::UInt8) = (b - UInt8('0')) <= 0x09
@inline _isblank(b::UInt8) = (b == UInt8(' ')) | (b == UInt8('\t'))

# How many decimal digits the digit accumulator can hold without overflowing
_maxaccdigits(::Type{V}) where {V<:Union{UInt64,UInt128,UInt256}} = max_exp10(V)

# `acc * 10^n + d`
@inline _push_digits(acc::V, n::Int, d::UInt8) where {V} = acc * _pow10(V, n) + d

# `acc * 10^n + d` and whether it overflowed, assuming `10^n` itself fits in `V`
@inline function _push_digits_checked(acc::V, n::Int, d::UInt8) where {V<:Union{UInt64,UInt128}}
    x, o1 = Base.mul_with_overflow(acc, _pow10(V, n))
    x, o2 = Base.add_with_overflow(x, V(d))
    return x, o1 | o2
end

# Exponents are saturated at this magnitude, far past anything that could fit any `T`. On
# 64-bit it is also past the exponent implied by the position of the decimal point in any
# input that fits in memory (so that e.g. "0.<10^9 zeros>1e1000000001" still parses as 1);
# on 32-bit that only holds for inputs shorter than about 10^8 bytes.
# Small enough that `10 * _MAXEXP + 9`, plus such an implied exponent, doesn't overflow an `Int`.
const _MAXEXP = Int === Int64 ? 100_000_000_000_000_000 : 100_000_000

# Parse `bytes[pos:last]` as the backing integer of a `FixedDecimal{T,f}`, returning `(value, code)`.
#
# Accepted syntax: optional ASCII spaces or tabs, an optional sign, decimal digits with an
# optional decimal point (at least one digit is required), an optional exponent
# (`e` or `E`, an optional sign, and at least one digit), and optional spaces or tabs.
# `value` is meaningless unless `code == _OK`.
function _parse_fixed(::Type{T}, f::Int, bytes::AbstractVector{UInt8}, pos::Int, last::Int, rounding::RoundingMode) where {T<:Integer}
    @inbounds begin
        while pos <= last && _isblank(bytes[pos])
            pos += 1
        end
        pos > last && return (zero(T), _INVALID)
        b = bytes[pos]
        neg = b == UInt8('-')
        if neg || b == UInt8('+')
            pos += 1
        end
    end
    return _parse_mantissa(T, f, bytes, pos, last, neg, rounding, UInt64(0), 0, 0, 0, false, false, false)
end

# Accumulate the significant digits into `acc`. Trailing zeros are counted in `zeros` instead of
# being multiplied in right away, so e.g. "1.000000" never needs a wide accumulator.
#
# Only the first `_maxintdigits(T) + 1` significant digits can affect the result exactly: any more
# means either overflow, or digits past the rounding position. Past that point we only track
# whether any dropped digit was nonzero (`sticky`), and count the dropped digits in `zeros`.
#
# When `acc` would overflow, we continue in a wider accumulator. Each width is a separate
# specialization, which keeps the common `UInt64` case free of type instability.
# Thanks to the dropped digits, a `UInt256` is only ever needed for 128-bit `T`.
function _parse_mantissa(
    ::Type{T}, f::Int, bytes::AbstractVector{UInt8}, pos::Int, last::Int, neg::Bool, rounding::RoundingMode,
    acc::V, ndigits::Int, zeros::Int, nfrac::Int, seen_point::Bool, seen_digit::Bool, sticky::Bool,
) where {T<:Integer,V}
    @inbounds while pos <= last
        if zeros == 0 && ndigits > 0
            # Fast path for a run of digits after the first significant one: take up to 19 digits
            # at a time in a UInt64, which also avoids slow 128-bit arithmetic for every digit.
            # (Zeros get multiplied in here rather than deferred, which is fine.)
            maxk = min(19, _maxaccdigits(V) - ndigits, _maxintdigits(T) + 1 - ndigits)
            chunk, k = _take_digits(bytes, pos, last, maxk)
            if k > 0
                acc = acc * _pow10(V, k) + chunk
                ndigits += k
                nfrac += k * seen_point
                pos += k
                continue
            end
        end
        b = bytes[pos]
        if _isdigit(b)
            seen_digit = true
            nfrac += seen_point
            d = b - UInt8('0')
            if d == 0x00
                # Leading zeros are not significant, trailing zeros are deferred
                zeros += ndigits > 0
            elseif ndigits + zeros + 1 > _maxintdigits(T) + 1
                sticky = true
                zeros += 1
            # (the first check is a compile-time constant, so e.g. `T == Int64` never compiles the
            # widening to a `UInt256` accumulator)
            elseif _maxaccdigits(V) < _maxintdigits(T) + 1 && ndigits + zeros + 1 > _maxaccdigits(V)
                # One more digit may still fit, e.g. most 20-digit numbers fit in a UInt64
                if ndigits + zeros == _maxaccdigits(V)
                    acc2, overflowed = _push_digits_checked(acc, zeros + 1, d)
                    if !overflowed
                        acc = acc2
                        ndigits += zeros + 1
                        zeros = 0
                        pos += 1
                        continue
                    end
                end
                return @noinline _parse_mantissa(T, f, bytes, pos, last, neg, rounding, _widen(acc), ndigits, zeros, nfrac - seen_point, seen_point, seen_digit, sticky)
            else
                # N.B. `zeros == 0` whenever `ndigits == 0`
                acc = zeros == 0 ? acc * V(10) + d : _push_digits(acc, zeros + 1, d)
                ndigits += zeros + 1
                zeros = 0
            end
        elseif b == UInt8('.') && !seen_point
            seen_point = true
        else
            break
        end
        pos += 1
    end
    seen_digit || return (zero(T), _INVALID)
    if sticky
        # Stand in for the dropped digits with a `1` right after the first `_maxintdigits(T) + 1`
        # digits. That's past the rounding position, and it is nonzero just like the dropped digits.
        n = _maxintdigits(T) + 1 - ndigits + 1
        return _parse_exponent(T, f, bytes, pos, last, neg, rounding, _push_sticky(T, acc, n), ndigits + n, zeros - n - nfrac)
    end
    return _parse_exponent(T, f, bytes, pos, last, neg, rounding, acc, ndigits, zeros - nfrac)
end

# Read up to `maxk` consecutive digits from `bytes[pos:last]`, returning their value and count
@inline function _take_digits(bytes::AbstractVector{UInt8}, pos::Int, last::Int, maxk::Int)
    chunk = UInt64(0)
    k = 0
    @inbounds while k < maxk && pos + k <= last
        d = bytes[pos + k] - UInt8('0')
        d <= 0x09 || break
        chunk = chunk * UInt64(10) + d
        k += 1
    end
    return chunk, k
end

# `acc * 10^n + 1` in an accumulator that can hold `_maxintdigits(T) + 2` digits
@noinline function _push_sticky(::Type{T}, acc, n::Int) where {T}
    if _maxintdigits(T) + 2 <= _maxaccdigits(UInt64)
        return _push_digits(UInt64(acc), n, 0x01)
    elseif _maxintdigits(T) + 2 <= _maxaccdigits(UInt128)
        return _push_digits(UInt128(acc), n, 0x01)
    else
        return _push_digits(UInt256(acc), n, 0x01)
    end
end

# Parse the optional exponent and trailing whitespace, then scale the digits.
# `exp` is the exponent implied by the deferred zeros and the decimal point position.
@inline function _parse_exponent(
    ::Type{T}, f::Int, bytes::AbstractVector{UInt8}, pos::Int, last::Int, neg::Bool, rounding::RoundingMode,
    acc, ndigits::Int, exp::Int,
) where {T<:Integer}
    @inbounds begin
        if pos <= last && (bytes[pos] | 0x20) == UInt8('e')
            pos += 1
            pos > last && return (zero(T), _INVALID)
            b = bytes[pos]
            eneg = b == UInt8('-')
            if eneg || b == UInt8('+')
                pos += 1
            end
            (pos <= last && _isdigit(bytes[pos])) || return (zero(T), _INVALID)
            e = 0
            while pos <= last
                b = bytes[pos]
                _isdigit(b) || break
                e = min(10e + (b - UInt8('0')), _MAXEXP)
                pos += 1
            end
            exp += ifelse(eneg, -e, e)
        end
        while pos <= last && _isblank(bytes[pos])
            pos += 1
        end
        pos <= last && return (zero(T), _INVALID)
    end
    return _scale(T, f, acc, exp, neg, ndigits, rounding)
end

function _check_rounding_mode(mode::RoundingMode)
    if !(mode in (RoundThrows, RoundNearest, RoundToZero))
        throw(ArgumentError("Unhandled rounding mode $mode"))
    end
end

function _base_parse(::Type{FD{T, f}}, source::AbstractString, mode::RoundingMode) where {T, f}
    # Strings with non-UTF-8 code units (e.g. UTF-16) are transcoded first
    codeunit(source) === UInt8 || return _base_parse(FD{T, f}, String(source), mode)
    _check_rounding_mode(mode)
    bytes = codeunits(source)
    return _parse_fixed(T, Int(f), bytes, 1, length(bytes), mode)
end

function Base.tryparse(::Type{FD{T, f}}, source::AbstractString, mode::RoundingMode=RoundNearest) where {T, f}
    val, code = _base_parse(FD{T, f}, source, mode)
    return code == _OK ? reinterpret(FD{T, f}, val) : nothing
end

function Base.parse(::Type{FD{T, f}}, source::AbstractString, mode::RoundingMode=RoundNearest) where {T, f}
    isempty(source) && throw(ArgumentError("Empty input is not allowed"))
    val, code = _base_parse(FD{T, f}, source, mode)
    code & _INEXACT != 0 && throw(InexactError(:parse, FD{T, f}, source))
    code & _OVERFLOW != 0 && throw(OverflowError("overflow parsing $(repr(source)) as $(FD{T, f})"))
    code & _INVALID != 0 && throw(ArgumentError("cannot parse $(repr(source)) as $(FD{T, f})"))
    return reinterpret(FD{T, f}, val)
end
