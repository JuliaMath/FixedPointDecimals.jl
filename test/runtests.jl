using FixedPointDecimals
using Base.Test

const SFD2 = FixedDecimal{Int16, 2}
const SFD4 = FixedDecimal{Int16, 4}
const FD2 = FixedDecimal{Int, 2}
const FD4 = FixedDecimal{Int, 4}
const WFD2 = FixedDecimal{Int128, 2}
const WFD4 = FixedDecimal{Int128, 4}

const keyvalues = [typemin(FD2),
                   FD2(-0.01),
                   FD2(0),
                   FD2(0.01),
                   FD2(1),
                   typemax(FD2)]

@testset "conversion" begin
    @testset for x in keyvalues
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
    for (i, x) in enumerate(keyvalues)
        @test x == x
        for y in keyvalues[i+1:end]
            @test x ≠ y
            @test x < y
            @test x ≤ y
            @test y ≠ x
            @test y > x
            @test y ≥ x
        end
    end
end

@testset "zero, one" begin
    @test FD2(0) == zero(FD2)
    @test FD2(42.42) + FD2(0) == FD2(42.42)
    @test FD2(1) == one(FD2)
    @test FD2(42.42) * FD2(1) == FD2(42.42)
end

@testset "eps, realmin, realmax" begin
    @test realmin(FD2) == eps(FD2) == FD2(0.01)
    @test eps(FD2(1.11)) == FD2(0.01)
    for x in keyvalues
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

@testset "addition" begin
    @test FD2(0) + FD2(0) == FD2(0)
    @test FD2(1.11) + FD2(2.22) == FD2(3.33)
    @test FD2(0.01) + FD2(0.01) == FD2(0.02)
    @test FD2(0.01) + FD2(-0.01) == FD2(0)

    # overflow
    @test typemax(FD2) + eps(FD2) == typemin(FD2)
end

@testset "subtraction" begin
    for x in keyvalues
        @test x - x == 0
        for y in keyvalues
            @test x + y - y == x
            @test y + x - y == x
        end
    end
end

@testset "multiplication" begin
    @testset "with integer" begin
        for x in keyvalues
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
end

@testset "division by 1" begin
    @testset for x in keyvalues
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

@testset "abs, sign" begin
    for x in keyvalues
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
end

@testset "floor, ceil" begin
    @testset for x in keyvalues
        if 0 ≤ abs(x) < 1000
            @test floor(x) ≤ x < floor(x) + 1
            @test ceil(x) - 1 < x ≤ ceil(x)
            @test isinteger(floor(x))
            @test isinteger(ceil(x))
        end
    end
end

@testset "show" begin
    @testset "compact" begin
        @test sprint(showcompact, FD2(1.00)) == "1.0"
        @test sprint(showcompact, FD2(1.23)) == "1.23"
        @test sprint(showcompact, FD2(42.40)) == "42.4"
        @test sprint(showcompact, FD2(-42.40)) == "-42.4"
        @test sprint(showcompact, FD2(-0.01)) == "-0.01"
        @test sprint(showcompact, FD2(0)) == "0.0"
        for x in keyvalues
            if 0 ≤ abs(x) < 1000
                @test eval(parse(string(x))) == x
            end
        end

        @test string(typemin(FixedDecimal{Int64, 2})) ==
              "FixedDecimal{Int64,2}(-92233720368547758.08)"
        @test string(typemax(FixedDecimal{Int64, 2})) ==
              "FixedDecimal{Int64,2}(92233720368547758.07)"
        @test string(typemin(FixedDecimal{Int32, 2})) ==
              "FixedDecimal{Int32,2}(-21474836.48)"
        @test string(typemax(FixedDecimal{Int32, 2})) ==
              "FixedDecimal{Int32,2}(21474836.47)"
    end
end
