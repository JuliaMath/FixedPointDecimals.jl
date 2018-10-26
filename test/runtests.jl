using FixedPointDecimals
import FixedPointDecimals: FD, value
using Compat
using Compat.Test
using Compat.Printf
using Base.Checked: checked_mul

include("utils.jl")

const SFD2 = FixedDecimal{Int16, 2}
const SFD4 = FixedDecimal{Int16, 4}
const FD1 = FixedDecimal{Int, 1}
const FD2 = FixedDecimal{Int, 2}
const FD3 = FixedDecimal{Int, 3}
const FD4 = FixedDecimal{Int, 4}
const WFD2 = FixedDecimal{Int128, 2}
const WFD4 = FixedDecimal{Int128, 4}

const CONTAINER_TYPES = Base.BitInteger_types  # Integer concrete subtypes which are bits

# these arrays should be kept sorted manually
const keyvalues = Dict(
    FD2 => [typemin(FD2),  # near minimum range
            FD2(-289.64),  # randomly generated
            FD2(-1),
            FD2(-0.01),    # near zero
            FD2(0),
            FD2(0.01),
            FD2(0.14),     # fraction-like
            FD2(0.33),
            FD2(0.5),
            FD2(1),        # one
            FD2(592.57),   # randomly generated
            typemax(FD2)], # near maximum range
    WFD4 => [typemin(WFD4),
             reinterpret(WFD4, -157030247204331916472131926508185768261),
             reinterpret(WFD4, -64628160301714851880492570261792833470),
             reinterpret(WFD4, -11679287782747983139362515984380939763),
             WFD4(-1),
             WFD4(-0.0001),
             WFD4(0),
             WFD4(0.0001),
             WFD4(0.01),
             WFD4(1),
             reinterpret(WFD4, 164435910993133062409572187012743929911),
             typemax(WFD4)])

# Floating point values written as integer strings. Useful for testing behaviours of
# trunc, floor, and ceil.
const INTS = Dict(
    v => replace(@sprintf("%.200f", v), "." => "")
    for v in [
        1.22,
        1.23,
        1.51,
        2.2,
        2.3,
    ]
)
const smaller_than_decimal = [1.22, 1.23, 2.3]
const bigger_than_decimal = [1.51, 2.2]


# numbers that may cause overflow
islarge(x) = x == typemin(x) || abs(x) > 1000

# numbers that can never cause overflow
issmall(x) = -1 < x ≤ 1

function parse_int(::Type{FD{T, f}}, val::AbstractString; ceil::Bool=false) where {T, f}
    reinterpret(FD{T, f}, parse(T, val[1:(f + 1)]) + T(ceil))
end

@testset "FixedPointDecimals" begin

# Basic tests for the methods created above
@testset "alt" begin
    @test trunc_alt(FD2, 0.0) == FD2(0)
    @test floor_alt(FD2, 0.0) == FD2(0)
    @test ceil_alt(FD2,  0.0) == FD2(0)

    @test trunc_alt(FD2, 2.149) == FD2(2.14)
    @test floor_alt(FD2, 2.149) == FD2(2.14)
    @test ceil_alt(FD2,  2.149) == FD2(2.15)

    @test trunc_alt(FD2, -2.149) == FD2(-2.14)
    @test floor_alt(FD2, -2.149) == FD2(-2.15)
    @test ceil_alt(FD2,  -2.149) == FD2(-2.14)

    @test trunc_alt(FD2, nextfloat(0.0)) == FD2(0)
    @test floor_alt(FD2, nextfloat(0.0)) == FD2(0)
    @test ceil_alt(FD2,  nextfloat(0.0)) == FD2(0.01)

    @test trunc_alt(FD2, prevfloat(0.0)) == FD2(0)
    @test floor_alt(FD2, prevfloat(0.0)) == FD2(-0.01)
    @test ceil_alt(FD2,  prevfloat(0.0)) == FD2(0)
end

@testset "max_exp10" begin
    @test FixedPointDecimals.max_exp10(Int8) == 2
    @test FixedPointDecimals.max_exp10(Int64) == 18
    @test FixedPointDecimals.max_exp10(Int128) == 38
    @test FixedPointDecimals.max_exp10(UInt8) == 2
    @test FixedPointDecimals.max_exp10(UInt64) == 19
    @test FixedPointDecimals.max_exp10(UInt128) == 38
    @test FixedPointDecimals.max_exp10(BigInt) == -1

    for T in CONTAINER_TYPES
        x = FixedPointDecimals.max_exp10(T)
        @test T(10)^x == widen(T(10))^x
    end

    @testset "custom integer types" begin
        @eval begin
            primitive type Int24 <: Integer 24 end
            Base.typemax(::Type{Int24}) = 2^24
            Base.widen(::Type{Int24}) = Int32
        end

        @test FixedPointDecimals.max_exp10(Int24) == 7

        # Note: we're just pretending that this is unbounded
        @eval primitive type IntUnbounded <: Integer 256 end
        @test_throws MethodError FixedPointDecimals.max_exp10(IntUnbounded)
    end
end

# ensure that the coefficient multiplied by the highest and lowest representable values of
# the container type do not result in overflow.
@testset "coefficient" begin
    @testset "overflow $T" for T in CONTAINER_TYPES
        f = FixedPointDecimals.max_exp10(T)
        powt = FixedPointDecimals.coefficient(FD{T, f})
        @test powt % 10 == 0
        @test checked_mul(widen(powt), typemax(T)) == widemul(powt, typemax(T))
        @test checked_mul(widen(powt), typemin(T)) == widemul(powt, typemin(T))
    end
end

@testset "constructor" begin
    @testset "invalid $T" for T in CONTAINER_TYPES
        f = FixedPointDecimals.max_exp10(T) + 1
        @test_throws ArgumentError reinterpret(FD{T,f}, 0)
        @test_throws ArgumentError reinterpret(FD{T,-1}, 0)
    end
end

@testset "conversion" begin
    @testset for x in keyvalues[FD2]
        @testset for T in [Rational{Int128}, WFD2, WFD4]
            @test convert(FD2, convert(T, x)) == x
            @test T(x) == convert(T, x)
        end
        if 0 ≤ abs(x) < 2
            @testset for T in [SFD2, SFD4, FD4]
                @test convert(FD2, convert(T, x)) == x
            end
        end
    end

    @testset "to float" begin
        # Convert the rational 5//7 into a FixedDecimal with as much precision as we can
        # without using BigInt.
        T = Int128
        f = FixedPointDecimals.max_exp10(T)
        powt = FixedPointDecimals.coefficient(FD{T,f})
        val = T(trunc(BigInt, widemul(5//7, powt)))

        fd = reinterpret(FD{T,f}, val)
        @test convert(Float64, fd) != convert(BigFloat, fd)
        @test convert(Float64, fd) == T(val) / T(powt)
        @test convert(BigFloat, fd) == BigInt(val) / BigInt(powt)
    end

    @testset "to rational" begin
        fd = reinterpret(FD2, 25)
        @test convert(Rational, fd) == 1//4
    end

    @testset "invalid" begin
        @test_throws InexactError convert(FD2, FD4(0.0001))
        @test_throws InexactError convert(FD4, typemax(FD2))
        @test_throws InexactError convert(SFD2, typemax(FD2))
        @test_throws InexactError convert(FD2, 1//3)
        @test_throws InexactError convert(FD{Int8,1}, 1//4)
    end

    @testset "limits of $T" for T in CONTAINER_TYPES
        max_exp = FixedPointDecimals.max_exp10(T)
        f = max_exp
        powt = widen(FixedPointDecimals.coefficient(FD{T,f}))

        # Smallest positive integer which is out-of-bounds for the FD
        x = max_exp - f + 1
        oob = T(10)^(x > 0 ? x : 0)

        # ideally we would just use `typemax(T)` but due to precision issues with
        # floating-point its possible the closest float will exceed `typemax(T)`.
        # Note: we should be doing `trunc(T, ...)` but truncating a BigFloat can be
        # problematic (https://github.com/JuliaLang/julia/issues/21914)
        max_int = trunc(BigInt, prevfloat(typemax(T) / powt) * powt)
        min_int = trunc(BigInt, nextfloat(typemin(T) / powt) * powt)

        @test max_int <= typemax(T)
        @test value(convert(FD{T,f}, max_int / powt)) == max_int
        @test min_int >= typemin(T)
        @test value(convert(FD{T,f}, min_int / powt)) == min_int

        @test convert(FD{T,f}, typemax(T) // powt) == reinterpret(FD{T,f}, typemax(T))
        @test convert(FD{T,f}, typemin(T) // powt) == reinterpret(FD{T,f}, typemin(T))

        @test_throws InexactError convert(FD{T,f}, oob)

        # Converting to a floating-point
        fd = reinterpret(FD{T,f}, typemax(T))
        @test convert(Float32, fd)  == Float32(typemax(T) / powt)
        @test convert(Float64, fd)  == Float64(typemax(T) / powt)
        @test convert(BigFloat, fd) == BigInt(typemax(T)) / powt

        fd = reinterpret(FD{T,f}, typemin(T))
        @test convert(Float32, fd)  == Float32(typemin(T) / powt)
        @test convert(Float64, fd)  == Float64(typemin(T) / powt)
        @test convert(BigFloat, fd) == BigInt(typemin(T)) / powt

        # Converting to a rational
        fd = reinterpret(FD{T,f}, typemax(T))
        @test convert(Rational, fd)  == typemax(T) // powt

        fd = reinterpret(FD{T,f}, typemin(T))
        @test convert(Rational, fd)  == typemin(T) // powt

        # The following tests require that the number of decimal places allow for
        # `-10 < x < 10` where x is a FD{T,f}. Needed to test `convert(::FD, ::Integer)`.
        max_int = typemax(T) ÷ powt * powt
        min_int = typemin(T) ÷ powt * powt

        @test convert(FD{T,f}, max_int ÷ powt) == reinterpret(FD{T,f}, max_int)
        @test convert(FD{T,f}, min_int ÷ powt) == reinterpret(FD{T,f}, min_int)

        @test_throws InexactError convert(FD{T,f}, max_int ÷ powt + oob)
        @test_throws InexactError convert(FD{T,f}, min_int ÷ powt - oob)  # Overflows with Unsigned
    end

    @testset "limits from $U to $T" for T in CONTAINER_TYPES, U in CONTAINER_TYPES
        f = FixedPointDecimals.max_exp10(T)
        g = FixedPointDecimals.max_exp10(U)
        powt = div(
            FixedPointDecimals.coefficient(FD{T, f}),
            FixedPointDecimals.coefficient(FD{U, g}),
        )

        val = typemax(U)
        expected = widemul(typemax(U), powt)

        # Mixed usage of signed and unsigned types makes testing with typemin hard.
        if f >= g && expected <= typemax(T)
            @test convert(FD{T,f}, reinterpret(FD{U,g}, val)) == reinterpret(FD{T,f}, expected)
        else
            @test_throws InexactError convert(FD{T,f}, reinterpret(FD{U,g}, val))
        end
    end
end

@testset "promotion" begin
    @test 1//10 + FD2(0.1) === 1//5
    @test 0.1 + FD2(0.1) === 0.2
    @test 1 + FD2(0.1) === FD2(1.1)
    @test FD2(0.1) + FD4(0.0001) === FD4(0.1001)
    @test WFD2(0.1) + FD4(0.0001) === WFD4(0.1001)
end

@testset "float" begin
    @test float(-one(SFD2)) === -1.0f0
    @test float(zero(SFD2)) === 0.0f0
    @test float(one(SFD2)) === 1.0f0
    @test float(-one(FD2)) === -1.0
    @test float(zero(FD2)) === 0.0
    @test float(one(FD2)) === 1.0
end

@testset "comparison" begin
    @testset for T in [FD2, WFD4]
        @testset for (i, x) in enumerate(keyvalues[T])
            @test x == x
            @testset for y in keyvalues[T][i+1:end]
                @test x ≠ y
                @test x < y
                @test x ≤ y
                @test y ≠ x
                @test y > x
                @test y ≥ x
            end
        end
    end
end

@testset "traits" begin
    @testset "zero, one" begin
        @test FD2(0) == zero(FD2)
        @test FD2(42.42) + FD2(0) == FD2(42.42)
        @test FD2(1) == one(FD2)
        @test FD2(42.42) * FD2(1) == FD2(42.42)
    end

    @testset "eps, floatmin, floatmax" begin
        @test floatmin(FD2) == eps(FD2) == FD2(0.01)
        @test eps(FD2(1.11)) == FD2(0.01)
        for x in keyvalues[FD2]
            if x ≠ typemax(FD2)
                @test x + eps(x) > x
            end
            if x ≠ typemin(FD2)
                @test x - eps(x) < x
                if x ≠ 0
                    @test floatmin(FD2) ≤ abs(x) ≤ floatmax(FD2)
                end
            end
        end
    end
end

@testset "addition" begin
    @test FD2(0) + FD2(0) == FD2(0)
    @test FD2(1.11) + FD2(2.22) == FD2(3.33)
    @test FD2(0.01) + FD2(0.01) == FD2(0.02)
    @test FD2(0.01) + FD2(-0.01) == FD2(0)

    # overflow
    @test typemax(FD2) + eps(FD2) == typemin(FD2)
end

@testset "subtraction" begin
    for x in keyvalues[FD2]
        @test x - x == 0
        for y in keyvalues[FD2]
            @test x + y - y == x
            @test y + x - y == x
        end
    end
end

@testset "multiply" begin
    @testset "with integer, $T" for T in [FD2, WFD4]
        for x in keyvalues[T]
            @test 1 * x == x * 1 == x
            @test one(x) * x == x * one(x) == x
            @test (-1) * x == x * (-1) == -x
            @test 2 * x == x + x == (one(x) + one(x)) * x
        end
    end

    @testset "binary" begin
        @test FD2(0.33) * FD2(1.00) == FD2(0.33)
        @test FD2(0.33) * FD2(3.00) == FD2(0.99)
        @test FD2(0.33) * FD2(0.50) == FD2(0.16)
        @test FD2(0.33) * FD2(0.33) == FD2(0.11)
        @test FD2(0.67) * FD2(0.67) == FD2(0.45)
    end

    @testset "key values $T" for T in [FD2, WFD4]
        totest = [(x, y) for x in keyvalues[T] for y in keyvalues[T] if
                  issmall(x) || issmall(y) || (!islarge(x) && !islarge(y))]
        @testset for (x, y) in totest
            # test the multiplication result is correctly rounded
            @test x * y == round(typeof(x), Base.widemul(x, y))
        end
        @test prod(keyvalues[T]) == 0
    end

    @testset "without promotion" begin
        @test_throws InexactError FD{Int8,1}(20)
        @test 20 * FD{Int8,1}(0.1) == FD{Int8,1}(2.0)
        @test FD{Int8,1}(0.1) * 20 == FD{Int8,1}(2.0)
    end

    @testset "limits of $T" for T in CONTAINER_TYPES
        f = FixedPointDecimals.max_exp10(T)
        scalar = convert(FD{T,f}, 1 // 10)  # 0.1

        # Since multiply will round the result we'll make sure our value does not
        # always rounds down.
        max_int = typemax(T) - (typemax(T) % 10)
        min_int = typemin(T) - (typemin(T) % 10)

        @test reinterpret(FD{T,f}, max_int) * scalar ==
            reinterpret(FD{T,f}, div(max_int, 10))
        @test reinterpret(FD{T,f}, min_int) * scalar ==
            reinterpret(FD{T,f}, div(min_int, 10))
    end
end

@testset "division" begin
    @testset "division by 1" begin
        @testset for x in keyvalues[FD2]
            @test x / one(x) == x

            # signed integers using two's complement have one additional negative value
            if x < 0 && x == typemin(x)
                @test_throws InexactError x / -one(x)
            else
                @test x / -one(x) == -x
            end
        end
    end

    @testset "division by 2" begin
        # even targets
        for x in FD2[-0.02, 0, 0.02, 1.00]
            for y in [2x-eps(x), 2x, 2x+eps(x)]
                @test y / 2 == y / 2one(y) == x == y * FD2(0.5)
            end
        end

        # odd targets
        for x in FD2[-0.01, 0.01, 1.01]
            y = 2x
            @test y / 2 == y / 2one(x) == x == y * FD2(0.5)
        end

        # big numbers
        for T in [SFD2, SFD4, FD2, FD4, WFD2, WFD4]
            @test typemin(T) / 2 * 2 == typemin(T)
            @test (typemax(T) / 2 - eps(T)) * 2 == typemax(T) - eps(T)
        end
    end

    @testset "division by 3" begin
        @test FD2(10) / 3 == FD2(3.33)
        @test FD2(20) / 3 == FD2(6.67)
        @test FD2(-1.50) / 3 == FD2(-0.50)
        @test FD2(-20) / 3 == FD2(-6.67)

        # should work with big numbers
        @test typemin(FD2) / 3 < 0
        @test typemax(FD2) / 3 > 0
    end

    @testset "reciprocal $x" for x in filter(!iszero, keyvalues[FD2])
        # convert each keyvalue to rational to take an exact reciprocal
        # and check that it rounds to the in-type reciprocal.
        r = 1/Rational{BigInt}(x)
        @test round(FD2, r) == 1/x
    end

    @testset "divide $x by 0" for x in keyvalues[FD2]
        @test_throws DivideError x/FD2(0)
    end

    @testset "rounding" begin
        # RoundNearest: 1.27 / 2 == 0.635 rounds up to 0.64
        @test FD2(1.27) / FD2(2) == FD2(0.64)
        @test FD2(-1.27) / FD2(2) == FD2(-0.64)
        @test FD2(1.27) / 2 == FD2(0.64)
        @test FD2(-1.27) / 2 == FD2(-0.64)
        @test 127 / FD2(200) == FD2(0.64)
        @test -127 / FD2(200) == FD2(-0.64)

        # RoundNearest: 1.29 / 2 == 0.645 rounds down to 0.64
        @test FD2(1.29) / FD2(2) == FD2(0.64)
        @test FD2(-1.29) / FD2(2) == FD2(-0.64)
        @test FD2(1.29) / 2 == FD2(0.64)
        @test FD2(-1.29) / 2 == FD2(-0.64)
        @test 129 / FD2(200) == FD2(0.64)
        @test -129 / FD2(200) == FD2(-0.64)

        # Use of Float or BigFloat internally should not change the calculated result
        @test round(Int, 109 / 200 * 100) == 55
        @test round(Int, BigInt(109) / 200 * 100) == 54  # Correct

        x = FD2(1.09)
        y = FD2(200)
        for T in [FD2, Int8, Int128, BigInt]
            @test x / T(2) == FD2(0.54)
            @test T(109) / y == FD2(0.54)
        end
    end

    @testset "without promotion" begin
        @test_throws InexactError FD{Int8,1}(20)
        @test Int8(20) / FD{Int8,1}(2) == FD{Int8,1}(10.0)
        @test FD{Int8,1}(2) / Int8(20) == FD{Int8,1}(0.1)
    end

    @testset "limits" begin
        @test_throws InexactError Int8(1) / FD{Int8,2}(0.4)
        @test_throws InexactError FD{Int8,2}(1) / FD{Int8,2}(0.4)
    end

    @testset "limits of $T" for T in CONTAINER_TYPES
        max_exp = FixedPointDecimals.max_exp10(T)
        f = max_exp
        scalar = convert(FD{T,f}, 1 // 10)  # 0.1

        # Should be outside of the bounds of a FD{T,f}
        x = T(10)
        @test_throws InexactError FD{T,f}(x)

        # Since multiply will round the result we'll make sure our value always
        # rounds down.
        max_int = typemax(T) - (typemax(T) % 10)
        min_int = typemin(T) - (typemin(T) % 10)
        max_fd = reinterpret(FD{T,f}, max_int)
        min_fd = reinterpret(FD{T,f}, min_int)

        @test (max_fd * scalar) / scalar == max_fd
        @test (min_fd * scalar) / scalar == min_fd
        @test max_fd / x == reinterpret(FD{T,f}, div(max_int, x))
        @test min_fd / x == reinterpret(FD{T,f}, div(min_int, x))
    end
end

@testset "abs, sign" begin
    @testset for T in [FD2, WFD4]
        for x in keyvalues[T]
            @test sign(x)^2 ∈ [0, 1]
            @test abs(abs(x)) == abs(x)
            @test abs(x) * sign(x) == x
            @test abs(x) == abs(-x)
            if x ≠ typemin(x)
                @test abs(x) ≥ 0
                @test sign(x) == -sign(-x)
            end
            @test (abs(x) == 0) === (x == 0)
        end
    end
end

@testset "isinteger" begin
    # Note: Test cannot be used unless we can construct `FD{Int8,6}`
    # @testset "overflow" begin
    #     # Note: After overflow `Int8(10)^6 == 64`
    #     @test !isinteger(reinterpret(FD{Int8,6}, 64))  # 0.000064
    # end

    @testset "limits of $T" for T in CONTAINER_TYPES
        f = FixedPointDecimals.max_exp10(T)

        max_fd = typemax(FD{T,f})
        min_fd = typemin(FD{T,f})

        @test !isinteger(max_fd)
        @test isinteger(trunc(max_fd))

        @test isinteger(min_fd) == (min_fd == zero(min_fd))
        @test isinteger(trunc(min_fd))
    end
end

@testset "round" begin
    @testset "to Int" begin
        @test round(Int, FD2(-0.51)) === -1
        @test round(Int, FD2(-0.50)) === 0
        @test round(Int, FD2(-0.49)) === 0
        @test round(Int, FD2(0.50)) === 0
        @test round(Int, FD2(0.51)) === 1
        @test round(Int, FD2(1.50)) === 2
    end

    @testset "rounding invariant $x" for x in filter(!islarge, keyvalues[FD2])
        @test isinteger(round(x))
        @test x - FD2(1//2) ≤ round(x) ≤ x + FD2(1//2)
        if x - FD2(1//2) == round(x) || x + FD2(1//2) == round(x)
            @test iseven(convert(Int, 100round(x)))
        end
        @testset "to Int" for T in [Int64, Int32]
            @test round(T, x) == round(x)
        end
        # to FD1
        @test x - FD2(1//20) ≤ round(FD1, x) ≤ x + FD2(1//20)
        # to FD2
        @test x == round(FD2, x)
    end

    @testset "limits of $T" for T in CONTAINER_TYPES
        f = FixedPointDecimals.max_exp10(T)
        powt = FixedPointDecimals.coefficient(FD{T,f})

        # Ideally we would just use `typemax(T)` but due to precision issues with
        # floating-point its possible the closest float will exceed `typemax(T)`.
        # Additionally, when the division results in a `BigFloat` we need to first truncate
        # to a `BigInt` before we can truncate the type we want.
        max_int = T(trunc(BigInt, prevfloat(typemax(T) / powt) * powt))
        min_int = T(trunc(BigInt, nextfloat(typemin(T) / powt) * powt))

        @test round(FD{T,f}, max_int / powt) == reinterpret(FD{T,f}, max_int)
        @test round(FD{T,f}, min_int / powt) == reinterpret(FD{T,f}, min_int)

        @test round(FD{T,f}, typemax(T) // powt) == reinterpret(FD{T,f}, typemax(T))
        @test round(FD{T,f}, typemin(T) // powt) == reinterpret(FD{T,f}, typemin(T))

        # Note: rounding away from zero will result in an exception.
        max_int = typemax(T)
        min_int = typemin(T)

        max_dec = max_int / powt
        min_dec = min_int / powt

        if round(T, max_dec) == trunc(T, max_dec)
            @test round(reinterpret(FD{T,f}, max_int)) == FD{T,f}(round(T, max_dec))
        else
            @test_throws InexactError round(reinterpret(FD{T,f}, max_int))
        end

        if round(T, min_dec) == trunc(T, min_dec)
            @test round(reinterpret(FD{T,f}, min_int)) == FD{T,f}(round(T, min_dec))
        else
            @test_throws InexactError round(reinterpret(FD{T,f}, min_int))
        end
    end
end

@testset "trunc" begin
    @test trunc(Int, FD2(0.99)) === 0
    @test trunc(Int, FD2(-0.99)) === 0
    @test trunc(Int, FD2(1)) === 1
    @test trunc(Int, FD2(-1)) === -1
    @test trunc(typemax(FD2)) ≤ typemax(FD2)
    @test trunc(Int, typemax(FD2)) ≤ typemax(FD2)
    @test trunc(typemin(FD2)) ≥ typemin(FD2)
    @test trunc(Int, typemin(FD2)) ≥ typemin(FD2)
    @test trunc(eps(FD2)) == 0
    @test trunc(-eps(FD2)) == 0

    @testset "truncate invariant" for x in keyvalues[FD2]
        @test isinteger(trunc(x))
        if x ≠ typemin(FD2)
            @test abs(x) - 1 < abs(trunc(x)) ≤ abs(x)
        else
            @test abs(trunc(x)) > 0
        end

        # to FD1
        @test isinteger(Base.widemul(10, FD2(trunc(FD1, x))))
        @test abs(FD2(trunc(FD1, x))) ≥ 0
    end

    @testset "truncate precision" begin
        for x in smaller_than_decimal
            @test trunc(FD2, x) ≠ trunc(FD3, x)
            @test trunc(FD2, x) == FD2(x - 0.01)
            @test trunc(FD3, x) == FD3(x - 0.001)

            for f in 0:18
                @test trunc(FD{Int64, f}, x) == parse_int(FD{Int64, f}, INTS[x])
            end
            for f in 0:200
                @test trunc(FD{BigInt, f}, x) == parse_int(FD{BigInt, f}, INTS[x])
            end
        end

        for x in bigger_than_decimal
            exactval = FD3(x)
            for f in 3:14
                @test trunc(FD{Int64, f}, x) == exactval
            end
            for f in 0:18
                @test trunc(FD{Int64, f}, x) == parse_int(FD{Int64, f}, INTS[x])
            end
        end
    end

    @testset "limits of $T" for T in CONTAINER_TYPES
        f = FixedPointDecimals.max_exp10(T)
        powt = FixedPointDecimals.coefficient(FD{T,f})

        # When converting from typemax to a floating-point it is possible that due to
        # precision issues that the closest possible float will exceed the typemax.
        max_float = prevfloat(convert(AbstractFloat, typemax(FD{T,f})))
        min_float = nextfloat(convert(AbstractFloat, typemin(FD{T,f})))

        @test trunc(FD{T,f}, max_float) == trunc_alt(FD{T,f}, max_float)
        @test trunc(FD{T,f}, min_float) == trunc_alt(FD{T,f}, min_float)

        @test trunc(reinterpret(FD{T,f}, typemax(T))) == FD{T,f}(div(typemax(T), powt))
        @test trunc(reinterpret(FD{T,f}, typemin(T))) == FD{T,f}(div(typemin(T), powt))
    end
end

# eps that works for integers too
epsi(::Type{T}) where T <: Integer = one(T)::T
epsi(::Type{T}) where T = eps(T)

@testset "floor, ceil" begin
    @testset for x in filter(!islarge, keyvalues[FD2])
        @test floor(x) ≤ x < floor(x) + 1
        @test ceil(x) - 1 < x ≤ ceil(x)
        @test isinteger(floor(x))
        @test isinteger(ceil(x))

        @testset for T in [Int32, Int64, FD1, FD2, FD4, WFD2, WFD4]
            @test floor(T, x) ≤ x < floor(T, x) + epsi(T)
            @test ceil(T, x) - epsi(T) < x ≤ ceil(T, x)
        end
    end

    @testset "floor, ceil precision" begin
        for x in smaller_than_decimal
            @test floor(FD2, x) != floor(FD3, x)
            @test floor(FD2, x) == FD2(x - 0.01)
            @test floor(FD3, x) == FD3(x - 0.001)

            for f in 0:18
                @test floor(FD{Int64, f}, x) == parse_int(FD{Int64, f}, INTS[x])
            end

            @test ceil(FD3, x) == ceil(FD4, x) == FD4(x)
        end

        for x in bigger_than_decimal
            @test ceil(FD2, x) ≠ ceil(FD3, x)
            @test ceil(FD2, x) == FD2(x + 0.01)
            @test ceil(FD3, x) == FD3(x + 0.001)

            for f in 0:18
                @test ceil(FD{Int64, f}, x) == parse_int(FD{Int64, f}, INTS[x], ceil=true)
            end

            @test floor(FD3, x) == floor(FD4, x) == FD4(x)
        end
    end

    @testset "limits of $T" for T in CONTAINER_TYPES
        f = FixedPointDecimals.max_exp10(T)
        powt = FixedPointDecimals.coefficient(FD{T,f})

        # When converting from typemax to a floating-point it is possible that due to
        # precision issues that the closest possible float will exceed the typemax.
        max_float = prevfloat(convert(AbstractFloat, typemax(FD{T,f})))
        min_float = nextfloat(convert(AbstractFloat, typemin(FD{T,f})))

        @test floor(FD{T,f}, max_float) == floor_alt(FD{T,f}, max_float)
        @test floor(FD{T,f}, min_float) == floor_alt(FD{T,f}, min_float)

        @test ceil(FD{T,f}, max_float) == ceil_alt(FD{T,f}, max_float)
        @test ceil(FD{T,f}, min_float) == ceil_alt(FD{T,f}, min_float)

        # Note: rounding away from zero will result in an exception.
        max_int = typemax(T)
        min_int = typemin(T)

        max_dec = max_int / powt
        min_dec = min_int / powt

        @test floor(reinterpret(FD{T,f}, max_int)) == FD{T,f}(floor(T, max_dec))
        if floor(T, min_dec) == trunc(T, min_dec)
            @test floor(reinterpret(FD{T,f}, min_int)) == FD{T,f}(floor(T, min_dec))
        else
            @test_throws InexactError floor(reinterpret(FD{T,f}, min_int))
        end

        if ceil(T, max_dec) == trunc(T, max_dec)
            @test ceil(reinterpret(FD{T,f}, max_int)) == FD{T,f}(ceil(T, max_dec))
        else
            @test_throws InexactError ceil(reinterpret(FD{T,f}, max_int))
        end
        @test ceil(reinterpret(FD{T,f}, min_int)) == FD{T,f}(ceil(T, min_dec))
    end
end

@testset "print" begin
    @test string(FD2(1.00)) == "1.00"
    @test string(FD2(1.23)) == "1.23"
    @test string(FD2(42.40)) == "42.40"
    @test string(FD2(-42.40)) == "-42.40"
    @test string(FD2(-0.01)) == "-0.01"
    @test string(FD2(0)) == "0.00"
    @test string(FixedDecimal{Int,0}(123.4)) == "123"

    # Displaying a decimal could be incorrect when using a decimal place precision which is
    # close to or at the limit for our storage type.
    @testset "limits of $T" for T in CONTAINER_TYPES
        f = FixedPointDecimals.max_exp10(T)

        function fmt(val, f)
            str = string(val)
            neg = ""
            if str[1] == '-'
                neg = "-"
                str = str[2:end]
            end
            return string(neg, str[1], ".", rpad(str[2:end], f, '0'))
        end

        @test string(reinterpret(FD{T,f}, typemax(T))) == fmt(typemax(T), f)
        @test string(reinterpret(FD{T,f}, typemin(T))) == fmt(typemin(T), f)
    end
end

@testset "show" begin
    @testset "compact" begin
        @test sprintcompact(FD2(1.00)) == "1.0"
        @test sprintcompact(FD2(1.23)) == "1.23"
        @test sprintcompact(FD2(42.40)) == "42.4"
        @test sprintcompact(FD2(-42.40)) == "-42.4"
        @test sprintcompact(FD2(-0.01)) == "-0.01"
        @test sprintcompact(FD2(0)) == "0.0"

        @test repr(typemin(FixedDecimal{Int64, 2})) ==
              "FixedDecimal{Int64,2}(-92233720368547758.08)"
        @test repr(typemax(FixedDecimal{Int64, 2})) ==
              "FixedDecimal{Int64,2}(92233720368547758.07)"
        @test repr(typemin(FixedDecimal{Int32, 2})) ==
              "FixedDecimal{Int32,2}(-21474836.48)"
        @test repr(typemax(FixedDecimal{Int32, 2})) ==
              "FixedDecimal{Int32,2}(21474836.47)"
    end
end

@testset "string" begin
    for x in keyvalues[FD2]
        if 0 ≤ abs(x) < 1000
            @test Core.eval(@__MODULE__, Meta.parse(string(x))) == x
        end
    end
end

@testset "parse_round" begin
    @test FixedPointDecimals.parse_round(Int, "44", RoundNearest) == 0
    @test FixedPointDecimals.parse_round(Int, "45", RoundNearest) == 0
    @test FixedPointDecimals.parse_round(Int, "46", RoundNearest) == 1
    @test FixedPointDecimals.parse_round(Int, "54", RoundNearest) == 0
    @test FixedPointDecimals.parse_round(Int, "55", RoundNearest) == 1
    @test FixedPointDecimals.parse_round(Int, "56", RoundNearest) == 1

    # Handle a number of digits that exceeds the storage capacity of Int128
    @test FixedPointDecimals.parse_round(Int8, "9"^40, RoundNearest) == 1
end

@testset "parse" begin
    # Note: the underscore used in the reinterpreted integer is used to indicate the decimal
    # place.
    @testset "decimal position" begin
        @test parse(FD2, "123")   == reinterpret(FD2, 123_00)
        @test parse(FD2, "0.123") == reinterpret(FD2, 0_12)
        @test parse(FD2, ".123")  == reinterpret(FD2, 0_12)
        @test parse(FD2, "1.23")  == reinterpret(FD2, 1_23)
        @test parse(FD2, "12.3")  == reinterpret(FD2, 12_30)
        @test parse(FD2, "123.")  == reinterpret(FD2, 123_00)
        @test parse(FD2, "123.0") == reinterpret(FD2, 123_00)

        @test parse(FD2, "-123")   == reinterpret(FD2, -123_00)
        @test parse(FD2, "-0.123") == reinterpret(FD2, -0_12)
        @test parse(FD2, "-.123")  == reinterpret(FD2, -0_12)
        @test parse(FD2, "-1.23")  == reinterpret(FD2, -1_23)
        @test parse(FD2, "-12.3")  == reinterpret(FD2, -12_30)
        @test parse(FD2, "-123.")  == reinterpret(FD2, -123_00)
        @test parse(FD2, "-123.0") == reinterpret(FD2, -123_00)
    end

    @testset "scientific notation" begin
        @test parse(FD4, "12e0")   == reinterpret(FD4, 00012_0000)
        @test parse(FD4, "12e3")   == reinterpret(FD4, 12000_0000)
        @test parse(FD4, "12e-3")  == reinterpret(FD4, 00000_0120)
        @test parse(FD4, "1.2e0")  == reinterpret(FD4, 00001_2000)
        @test parse(FD4, "1.2e3")  == reinterpret(FD4, 01200_0000)
        @test parse(FD4, "1.2e-3") == reinterpret(FD4, 00000_0012)
        @test parse(FD4, "1.2e-4") == reinterpret(FD4, 00000_0001)

        @test parse(FD4, "-12e0")   == reinterpret(FD4, -00012_0000)
        @test parse(FD4, "-12e3")   == reinterpret(FD4, -12000_0000)
        @test parse(FD4, "-12e-3")  == reinterpret(FD4, -00000_0120)
        @test parse(FD4, "-1.2e0")  == reinterpret(FD4, -00001_2000)
        @test parse(FD4, "-1.2e3")  == reinterpret(FD4, -01200_0000)
        @test parse(FD4, "-1.2e-3") == reinterpret(FD4, -00000_0012)

        @test parse(FD2, "999e-1") == reinterpret(FD2, 99_90)
        @test parse(FD2, "999e-2") == reinterpret(FD2, 09_99)
        @test parse(FD2, "999e-3") == reinterpret(FD2, 01_00)
        @test parse(FD2, "999e-4") == reinterpret(FD2, 00_10)
        @test parse(FD2, "999e-5") == reinterpret(FD2, 00_01)
        @test parse(FD2, "999e-6") == reinterpret(FD2, 00_00)

        @test parse(FD2, "-999e-1") == reinterpret(FD2, -99_90)
        @test parse(FD2, "-999e-2") == reinterpret(FD2, -09_99)
        @test parse(FD2, "-999e-3") == reinterpret(FD2, -01_00)
        @test parse(FD2, "-999e-4") == reinterpret(FD2, -00_10)
        @test parse(FD2, "-999e-5") == reinterpret(FD2, -00_01)
        @test parse(FD2, "-999e-6") == reinterpret(FD2, -00_00)

        @test parse(FD4, "9"^96 * "e-100") == reinterpret(FD4, 0_001)
    end

    @testset "round to nearest" begin
        @test parse(FD2, "0.444") == reinterpret(FD2, 0_44)
        @test parse(FD2, "0.445") == reinterpret(FD2, 0_44)
        @test parse(FD2, "0.446") == reinterpret(FD2, 0_45)
        @test parse(FD2, "0.454") == reinterpret(FD2, 0_45)
        @test parse(FD2, "0.455") == reinterpret(FD2, 0_46)
        @test parse(FD2, "0.456") == reinterpret(FD2, 0_46)

        @test parse(FD2, "-0.444") == reinterpret(FD2, -0_44)
        @test parse(FD2, "-0.445") == reinterpret(FD2, -0_44)
        @test parse(FD2, "-0.446") == reinterpret(FD2, -0_45)
        @test parse(FD2, "-0.454") == reinterpret(FD2, -0_45)
        @test parse(FD2, "-0.455") == reinterpret(FD2, -0_46)
        @test parse(FD2, "-0.456") == reinterpret(FD2, -0_46)

        @test parse(FD2, "0.009")  == reinterpret(FD2,  0_01)
        @test parse(FD2, "-0.009") == reinterpret(FD2, -0_01)

        @test parse(FD4, "1.5e-4") == reinterpret(FD4, 0_0002)
    end

    @testset "round to zero" begin
        @test parse(FD2, "0.444", RoundToZero) == reinterpret(FD2, 0_44)
        @test parse(FD2, "0.445", RoundToZero) == reinterpret(FD2, 0_44)
        @test parse(FD2, "0.446", RoundToZero) == reinterpret(FD2, 0_44)
        @test parse(FD2, "0.454", RoundToZero) == reinterpret(FD2, 0_45)
        @test parse(FD2, "0.455", RoundToZero) == reinterpret(FD2, 0_45)
        @test parse(FD2, "0.456", RoundToZero) == reinterpret(FD2, 0_45)

        @test parse(FD2, "-0.444", RoundToZero) == reinterpret(FD2, -0_44)
        @test parse(FD2, "-0.445", RoundToZero) == reinterpret(FD2, -0_44)
        @test parse(FD2, "-0.446", RoundToZero) == reinterpret(FD2, -0_44)
        @test parse(FD2, "-0.454", RoundToZero) == reinterpret(FD2, -0_45)
        @test parse(FD2, "-0.455", RoundToZero) == reinterpret(FD2, -0_45)
        @test parse(FD2, "-0.456", RoundToZero) == reinterpret(FD2, -0_45)

        @test parse(FD2, "0.009", RoundToZero)  == reinterpret(FD2, 0_00)
        @test parse(FD2, "-0.009", RoundToZero) == reinterpret(FD2, 0_00)

        @test parse(FD4, "1.5e-4", RoundToZero) == reinterpret(FD4, 0_0001)
    end

    @testset "round throws" begin
        @test parse(FD2, "0.44", RoundThrows)  == reinterpret(FD2, 0_44)
        @test parse(FD2, "0.440", RoundThrows) == reinterpret(FD2, 0_44)

        @test_throws InexactError parse(FD2, "0.444", RoundThrows)
        @test_throws InexactError parse(FD2, "0.445", RoundThrows)
        @test_throws InexactError parse(FD2, "0.446", RoundThrows)
        @test_throws InexactError parse(FD2, "0.454", RoundThrows)
        @test_throws InexactError parse(FD2, "0.455", RoundThrows)
        @test_throws InexactError parse(FD2, "0.456", RoundThrows)

        @test_throws InexactError parse(FD2, "-0.444", RoundThrows)
        @test_throws InexactError parse(FD2, "-0.445", RoundThrows)
        @test_throws InexactError parse(FD2, "-0.446", RoundThrows)
        @test_throws InexactError parse(FD2, "-0.454", RoundThrows)
        @test_throws InexactError parse(FD2, "-0.455", RoundThrows)
        @test_throws InexactError parse(FD2, "-0.456", RoundThrows)

        @test_throws InexactError parse(FD2, "0.009", RoundThrows)
        @test_throws InexactError parse(FD2, "-0.009", RoundThrows)

        @test_throws InexactError parse(FD4, "1.5e-4", RoundThrows)
    end

    @testset "invalid" begin
        @test_throws OverflowError parse(FD4, "1.2e100")
        @test_throws ArgumentError parse(FD4, "foo")
        @test_throws ArgumentError parse(FD4, "1.2.3")
        @test_throws ArgumentError parse(FD4, "1.2", RoundUp)
    end

    @testset "hashing" begin
        fd1 = FixedDecimal{Int, 4}(2.5)
        fd2 = FixedDecimal{Int, 5}(2.5)
        fd3 = FixedDecimal{Int, 4}(3.5)

        @test hash(fd1) == hash(fd2)
        @test hash(fd1) != hash(fd3)
        @test hash(fd1) != hash(fd1.i)
        @test hash(FD2(1//10)) == hash(1//10)
        @test hash(FD2(1//10)) ≠ hash(0.1)
    end
end

end  # global testset
