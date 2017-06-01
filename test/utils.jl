# Format a floating-point number as a string with a specific precision. Similar to
# `@sprintf("%.$(dp)f", val)`.

if VERSION < v"0.6-"
    function float_string(val::AbstractFloat, dp::Integer)
        # On Julia 0.5 positive 0.0 can have extra bits. No other integer floating-point
        # seems to have this problem.
        isequal(val, 0.0) && return rpad("0.", dp, '0')
        format = "%.$(dp)f"
        @eval @sprintf($("%.$(dp)f"), nextfloat(0.0))
        @eval begin
            let buffer = Array{UInt8}(100 + $dp)
                ccall((:snprintf, :libc), Int, (Ptr{UInt8}, Csize_t, Cstring, Cdouble), buffer, length(buffer), $format, $val)
                unsafe_string(pointer(buffer))
            end
        end
    end
else
    function float_string(val::AbstractFloat, dp::Integer)
        buffer = Array{UInt8}(100 + dp)
        ccall((:snprintf, :libc), Int, (Ptr{UInt8}, Csize_t, Cstring, Cdouble), buffer, length(buffer), "%.$(dp)f", val)
        unsafe_string(pointer(buffer))
    end
end

# FixedDecimal methods which perform an alternative method of trunc, floor, and ceil which
# primarily use the string representation of the floating-point. These `*_alt` methods exist
# to ensure the accuracy of the packages mathematical methods for trunc, floor, and ceil.

function integer_alt{T<:Integer}(::Type{T}, dp::Integer, val::AbstractFloat)
    # Note: Use a precision larger than the value can represent so that `sprintf` doesn't
    # perform any rounding.
    # TODO: Ideally we could be using just be using `@sprintf("%.325f", val)` once this
    # issue is fixed: https://github.com/JuliaLang/julia/issues/22137
    str = float_string(val, 325)  # 325 digits is large enough for `nextfloat(0.0)`
    sign = T(first(str) == '-' ? -1 : 1)
    decimal = findfirst(str, '.')
    int_start = sign < 0 ? 2 : 1
    int_end = decimal + dp
    v = parse(T, str[int_start:(decimal - 1)] * str[(decimal + 1):int_end])
    r = T(any(d -> d != '0', str[(int_end + 1):end]))  # Remaining digits
    return (sign, v, r)
end

function trunc_alt{T<:Integer,f}(::Type{FD{T,f}}, val::AbstractFloat)
    s, v, r = integer_alt(T, f, val)
    reinterpret(FD{T,f}, copysign(v, s))
end

function floor_alt{T<:Integer,f}(::Type{FD{T,f}}, val::AbstractFloat)
    s, v, r = integer_alt(T, f, val)
    reinterpret(FD{T,f}, copysign(v + (s < 0 ? r : zero(T)), s))
end

function ceil_alt{T<:Integer,f}(::Type{FD{T,f}}, val::AbstractFloat)
    s, v, r = integer_alt(T, f, val)
    reinterpret(FD{T,f}, copysign(v + (s > 0 ? r : zero(T)), s))
end
