# Print the floating-point number with precision high enough that rounding never occurs
function float_string(val::AbstractFloat)
    # Note: This function could simply be `@sprintf("%.325f", val)` once the following
    # issue is solved: https://github.com/JuliaLang/julia/issues/22137
    dp = 325  # Number of decimal places to print `nextfloat(0.0)` without rounding
    int = trunc(BigInt, val)
    if abs(int) > 0
        dp -= ndigits(int)
    end
    @eval @sprintf($("%.$(dp)f"), $val)
end

# FixedDecimal methods which perform an alternative method of trunc, floor, and ceil which
# primarily use the string representation of the floating-point. These `*_alt` methods exist
# to ensure the accuracy of the packages mathematical methods for trunc, floor, and ceil.

function integer_alt(::Type{T}, dp::Integer, val::AbstractFloat) where {T<:Integer}
    # Note: Use a precision larger than the value can represent so that `sprintf` doesn't
    # perform any rounding.
    str = float_string(val)
    sign = T(first(str) == '-' ? -1 : 1)
    decimal = coalesce(findfirst(==('.'), str), 0)
    int_start = sign < 0 ? 2 : 1
    int_end = decimal + dp
    v = parse(T, str[int_start:(decimal - 1)] * str[(decimal + 1):int_end])
    r = T(any(d -> d != '0', str[(int_end + 1):end]))  # Remaining digits
    return (sign, v, r)
end

function trunc_alt(::Type{FD{T,f}}, val::AbstractFloat) where {T<:Integer, f}
    s, v, r = integer_alt(T, f, val)
    reinterpret(FD{T,f}, copysign(v, s))
end

function floor_alt(::Type{FD{T,f}}, val::AbstractFloat) where {T<:Integer, f}
    s, v, r = integer_alt(T, f, val)
    reinterpret(FD{T,f}, copysign(v + (s < 0 ? r : zero(T)), s))
end

function ceil_alt(::Type{FD{T,f}}, val::AbstractFloat) where {T<:Integer, f}
    s, v, r = integer_alt(T, f, val)
    reinterpret(FD{T,f}, copysign(v + (s > 0 ? r : zero(T)), s))
end
