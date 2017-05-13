using FixedPointDecimals
import FixedPointDecimals: FD
using Base.Test
using Compat

const SFD2 = FixedDecimal{Int16, 2}
const SFD4 = FixedDecimal{Int16, 4}
const FD1 = FixedDecimal{Int, 1}
const FD2 = FixedDecimal{Int, 2}
const FD3 = FixedDecimal{Int, 3}
const FD4 = FixedDecimal{Int, 4}
const WFD2 = FixedDecimal{Int128, 2}
const WFD4 = FixedDecimal{Int128, 4}

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
const INT_2_2 = "22000000000000001776356839400250464677"  # 2.2
const INT_2_3 = "22999999999999998223643160599749535322"  # 2.3


# numbers that may cause overflow
islarge(x) = x == typemin(x) || abs(x) > 1000

# numbers that can never cause overflow
issmall(x) = -1 < x ≤ 1

function parse_int{T, f}(::Type{FD{T, f}}, val::AbstractString; ceil::Bool=false)
    reinterpret(FD{T, f}, parse(T, val[1:(f + 1)]) + T(ceil))
end

@testset "conversion" begin
    @testset for x in keyvalues[FD2]
        @testset for T in [Rational{Int128}, WFD2, WFD4]
            @test convert(FD2, convert(T, x)) == x
        end
        if 0 ≤ abs(x) < 2
            @testset for T in [SFD2, SFD4, FD4]
                @test convert(FD2, convert(T, x)) == x
            end
        end
    end

    @test_throws InexactError convert(FD2, FD4(0.0001))
    @test_throws InexactError convert(FD4, typemax(FD2))
    @test_throws InexactError convert(SFD2, typemax(FD2))
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

    @testset "eps, realmin, realmax" begin
        @test realmin(FD2) == eps(FD2) == FD2(0.01)
        @test eps(FD2(1.11)) == FD2(0.01)
        for x in keyvalues[FD2]
            if x ≠ typemax(FD2)
                @test x + eps(x) > x
            end
            if x ≠ typemin(FD2)
                @test x - eps(x) < x
                if x ≠ 0
                    @test realmin(FD2) ≤ abs(x) ≤ realmax(FD2)
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
end

@testset "division" begin
    @testset "division by 1" begin
        @testset for x in keyvalues[FD2]
            @test x / one(x) == x
            @test x / -one(x) == -x
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
        @test trunc(FD2, 2.3) != trunc(FD3, 2.3)
        @test trunc(FD2, 2.3) == FD2(2.29)
        @test trunc(FD3, 2.3) == FD3(2.299)

        for f in 0:12
            trunc(FD{Int64, f}, 2.3) == parse_int(FD{Int64, f}, INT_2_3)
        end
    end
end

# eps that works for integers too
epsi{T <: Integer}(::Type{T})::T = 1
epsi{T}(::Type{T}) = eps(T)

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
        @test floor(FD2, 2.3) != floor(FD3, 2.3)
        @test floor(FD2, 2.3) == FD2(2.29)
        @test floor(FD3, 2.3) == FD3(2.299)

        for f in 0:12
            floor(FD{Int64, f}, 2.3) == parse_int(FD{Int64, f}, INT_2_3)
        end

        @test ceil(FD2, 2.2) != ceil(FD3, 2.2)
        @test ceil(FD2, 2.2) == FD2(2.21)
        @test ceil(FD3, 2.2) == FD3(2.201)

        for f in 0:12
            ceil(FD{Int64, f}, 2.2) == parse_int(FD{Int64, f}, INT_2_2, ceil=true)
        end
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
end

@testset "show" begin
    @testset "compact" begin
        @test sprint(showcompact, FD2(1.00)) == "1.0"
        @test sprint(showcompact, FD2(1.23)) == "1.23"
        @test sprint(showcompact, FD2(42.40)) == "42.4"
        @test sprint(showcompact, FD2(-42.40)) == "-42.4"
        @test sprint(showcompact, FD2(-0.01)) == "-0.01"
        @test sprint(showcompact, FD2(0)) == "0.0"

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
            @test eval(parse(string(x))) == x
        end
    end
end
