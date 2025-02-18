const SFD2 = FixedDecimal{Int16, 2}
const SFD4 = FixedDecimal{Int16, 4}
const FD1 = FixedDecimal{Int, 1}
const FD2 = FixedDecimal{Int, 2}
const FD3 = FixedDecimal{Int, 3}
const FD4 = FixedDecimal{Int, 4}
const WFD2 = FixedDecimal{Int128, 2}
const WFD4 = FixedDecimal{Int128, 4}
const UFD2 = FixedDecimal{UInt, 2}
const UWFD2 = FixedDecimal{UInt128, 2}

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

@testset "equality between types" begin
    @test FD{Int8, 0}(1) == FD{Int8, 2}(1)
    @test FD{Int8, 0}(0) != FD{Int8, 2}(1)
    # Note: this doesn't throw inexact error
    @test FD{Int8, 0}(2) != FD{Int8, 2}(1)  # FD{Int8,2}(2) doesn't fit

    @test FD{Int8, 0}(1) == FD{Int16, 1}(1)
    @test FD{Int8, 0}(2) != FD{Int16, 1}(1)
    # Note: this doesn't throw inexact error
    @test FD{Int8, 0}(4) != FD{Int16, 4}(1)  # FD{Int16,4}(4) doesn't fit

    # Integer == FD
    @test 1 == FD{Int8, 2}(1)
    @test 2 != FD{Int8, 2}(1)
    @test FD{Int8, 2}(1) == 1
    @test FD{Int8, 2}(1) != 2
    @test 1 == FD{Int8, 0}(1) != 2

    @test typemax(Int16) !== FD{Int8, 0}(1)
    @test typemax(Int16) !== FD{Int8, 2}(1)
    @test typemin(Int16) !== FD{Int8, 0}(1)
    @test typemin(Int16) !== FD{Int8, 2}(1)
    @test FD{Int8, 0}(1) != typemax(Int16)
    @test FD{Int8, 2}(1) != typemax(Int16)
    @test FD{Int8, 0}(1) != typemin(Int16)
    @test FD{Int8, 2}(1) != typemin(Int16)

    @test typemax(Int16) !== FD{Int8, 0}(-1)
    @test typemax(Int16) !== FD{Int8, 2}(-1)
    @test typemin(Int16) !== FD{Int8, 0}(-1)
    @test typemin(Int16) !== FD{Int8, 2}(-1)
    @test FD{Int8, 0}(-1) != typemax(Int16)
    @test FD{Int8, 2}(-1) != typemax(Int16)
    @test FD{Int8, 0}(-1) != typemin(Int16)
    @test FD{Int8, 2}(-1) != typemin(Int16)

    @test typemax(Int16) !== FD{Int8, 0}(0)
    @test typemax(Int16) !== FD{Int8, 2}(0)
    @test typemin(Int16) !== FD{Int8, 0}(0)
    @test typemin(Int16) !== FD{Int8, 2}(0)
    @test FD{Int8, 0}(0) != typemax(Int16)
    @test FD{Int8, 2}(0) != typemax(Int16)
    @test FD{Int8, 0}(0) != typemin(Int16)
    @test FD{Int8, 2}(0) != typemin(Int16)

    # less precision allows smaller and bigger numbers
    @test typemin(FD{Int8, 1}) != typemin(FD{Int8,2})
    @test typemax(FD{Int8, 1}) != typemax(FD{Int8,2})
end
@testset "inequality between types" begin
    @test FD{Int8, 0}(1) <= FD{Int8, 2}(1)
    @test FD{Int8, 0}(0) < FD{Int8, 2}(1)
    # Note: this doesn't throw inexact error
    @test FD{Int8, 0}(2) >= FD{Int8, 2}(1)  # FD{Int8,2}(2) doesn't fit
    @test FD{Int8, 2}(1) < FD{Int8, 0}(2)  # FD{Int8,2}(2) doesn't fit

    @test FD{Int8, 0}(1) <= FD{Int16, 1}(1)
    @test FD{Int8, 0}(2) > FD{Int16, 1}(1)
    # Note: this doesn't throw inexact error
    @test FD{Int8, 0}(4) > FD{Int16, 4}(1)  # FD{Int16,4}(4) doesn't fit
    @test FD{Int8, 0}(4) >= FD{Int16, 4}(1)  # FD{Int16,4}(4) doesn't fit
    @test FD{Int16, 4}(1) < FD{Int8, 0}(4)  # FD{Int16,4}(4) doesn't fit

    # less precision allows smaller numbers
    @test typemin(FD{Int8, 1}) <  typemin(FD{Int8,2})
    @test typemin(FD{Int8, 1}) <= typemin(FD{Int8,2})
    @test typemin(FD{Int8, 2}) >  typemin(FD{Int8,1})
    @test typemin(FD{Int8, 2}) >= typemin(FD{Int8,1})
    # less precision allows bigger numbers
    @test typemax(FD{Int8, 1}) >  typemax(FD{Int8,2})
    @test typemax(FD{Int8, 1}) >= typemax(FD{Int8,2})
    @test typemax(FD{Int8, 2}) <  typemax(FD{Int8,1})
    @test typemax(FD{Int8, 2}) <= typemax(FD{Int8,1})

    @test !(typemin(FD{Int8, 2}) <= typemin(FD{Int8,1}))
    @test !(typemin(FD{Int8, 1}) >= typemin(FD{Int8,2}))
    @test !(typemin(FD{Int8, 1}) >  typemin(FD{Int8,2}))
    @test !(typemin(FD{Int8, 1}) >= typemin(FD{Int8,2}))
    @test !(typemin(FD{Int8, 2}) <  typemin(FD{Int8,1}))
    @test !(typemin(FD{Int8, 2}) <= typemin(FD{Int8,1}))
    @test !(typemax(FD{Int8, 1}) <  typemax(FD{Int8,2}))
    @test !(typemax(FD{Int8, 1}) <= typemax(FD{Int8,2}))
    @test !(typemax(FD{Int8, 2}) >  typemax(FD{Int8,1}))
    @test !(typemax(FD{Int8, 2}) >= typemax(FD{Int8,1}))

    @testset "Integer and FD" begin
        @test 1 <= FD{Int8, 2}(1) <= 1
        @test 1 >= FD{Int8, 2}(1) >= 1
        @test 2 > FD{Int8, 2}(1)
        @test FD{Int8, 2}(1) < 2
        @test 2 >= FD{Int8, 2}(1)
        @test FD{Int8, 2}(1) <= 2
        @test 1 <= FD{Int8, 0}(1) < 2

        # negatives
        @test -1 <= FD{Int8, 2}(-1) <= -1
        @test -1 >= FD{Int8, 2}(-1) >= -1
        @test -2 < FD{Int8, 2}(-1)
        @test FD{Int8, 2}(-1) > -2
        @test -2 <= FD{Int8, 2}(-1)
        @test FD{Int8, 2}(-1) >= -2
        @test -1 <= FD{Int8, 0}(-1) < 2

        # Same types
        @test typemax(Int8) > FD{Int8, 0}(1) > typemin(Int8)
        @test typemax(Int8) > FD{Int8, 2}(1) > typemin(Int8)
        @test typemin(Int8) < FD{Int8, 0}(1) < typemax(Int8)
        @test typemin(Int8) < FD{Int8, 2}(1) < typemax(Int8)
        @test !(typemax(Int8) < FD{Int8, 0}(1) < typemin(Int8))
        @test !(typemax(Int8) < FD{Int8, 2}(1) < typemin(Int8))
        @test !(typemin(Int8) > FD{Int8, 0}(1) > typemax(Int8))
        @test !(typemin(Int8) > FD{Int8, 2}(1) > typemax(Int8))

        @test typemax(Int8) > FD{Int8, 0}(-1) > typemin(Int8)
        @test typemax(Int8) > FD{Int8, 2}(-1) > typemin(Int8)
        @test typemin(Int8) < FD{Int8, 0}(-1) < typemax(Int8)
        @test typemin(Int8) < FD{Int8, 2}(-1) < typemax(Int8)
        @test !(typemax(Int8) < FD{Int8, 0}(-1) < typemin(Int8))
        @test !(typemax(Int8) < FD{Int8, 2}(-1) < typemin(Int8))
        @test !(typemin(Int8) > FD{Int8, 0}(-1) > typemax(Int8))
        @test !(typemin(Int8) > FD{Int8, 2}(-1) > typemax(Int8))

        # Different types
        @test typemax(Int16) > FD{Int8, 0}(1) > typemin(Int16)
        @test typemax(Int16) > FD{Int8, 2}(1) > typemin(Int16)
        @test typemin(Int16) < FD{Int8, 0}(1) < typemax(Int16)
        @test typemin(Int16) < FD{Int8, 2}(1) < typemax(Int16)
        @test !(typemax(Int16) < FD{Int8, 0}(1) < typemin(Int16))
        @test !(typemax(Int16) < FD{Int8, 2}(1) < typemin(Int16))
        @test !(typemin(Int16) > FD{Int8, 0}(1) > typemax(Int16))
        @test !(typemin(Int16) > FD{Int8, 2}(1) > typemax(Int16))

        @test typemax(Int16) > FD{Int8, 0}(-1) > typemin(Int16)
        @test typemax(Int16) > FD{Int8, 2}(-1) > typemin(Int16)
        @test typemin(Int16) < FD{Int8, 0}(-1) < typemax(Int16)
        @test typemin(Int16) < FD{Int8, 2}(-1) < typemax(Int16)
        @test !(typemax(Int16) < FD{Int8, 0}(-1) < typemin(Int16))
        @test !(typemax(Int16) < FD{Int8, 2}(-1) < typemin(Int16))
        @test !(typemin(Int16) > FD{Int8, 0}(-1) > typemax(Int16))
        @test !(typemin(Int16) > FD{Int8, 2}(-1) > typemax(Int16))

        @test typemax(Int16) >= FD{Int8, 0}(0) >= typemin(Int16)
        @test typemax(Int16) >= FD{Int8, 2}(0) >= typemin(Int16)
        @test typemin(Int16) <= FD{Int8, 0}(0) <= typemax(Int16)
        @test typemin(Int16) <= FD{Int8, 2}(0) <= typemax(Int16)
        @test !(typemax(Int16) <= FD{Int8, 0}(-1) <= typemin(Int16))
        @test !(typemax(Int16) <= FD{Int8, 2}(-1) <= typemin(Int16))
        @test !(typemin(Int16) >= FD{Int8, 0}(-1) >= typemax(Int16))
        @test !(typemin(Int16) >= FD{Int8, 2}(-1) >= typemax(Int16))
    end
end

@testset "128-bit conversion correctness" begin
    # Force the bits for these tests
    F64D2 = FixedDecimal{Int64, 2}
    UF64D2 = FixedDecimal{UInt64, 2}

    @testset "Convert from 64-bit to 128-bit" begin
        @test convert(WFD2, 1).i === Int128(100)
        @test convert(UWFD2, 1).i === UInt128(100)
        @test convert(WFD2, -1).i === Int128(-100)
        @test_throws InexactError(:convert, UWFD2, -1) convert(UWFD2, -1)
        @test convert(WFD2, UInt64(1)).i === Int128(100)
        @test convert(UWFD2, UInt64(1)).i === UInt128(100)
        @test convert(WFD2, typemax(Int64)).i === Int128(922337203685477580700)
        @test convert(UWFD2, typemax(Int64)).i === UInt128(922337203685477580700)
        @test convert(WFD2, typemin(Int64)).i === Int128(-922337203685477580800)
        @test_throws InexactError(:convert, UWFD2, typemin(Int64)) convert(UWFD2, typemin(Int64))
        @test convert(WFD2, typemax(UInt64)).i === Int128(1844674407370955161500)
        @test convert(UWFD2, typemax(UInt64)).i === UInt128(1844674407370955161500)
    end

    @testset "Convert from 128-bit to 128-bit" begin
        @test convert(WFD2, Int128(1)).i === Int128(100)
        @test convert(UWFD2, Int128(1)).i === UInt128(100)
        @test convert(WFD2, UInt128(1)).i === Int128(100)
        @test convert(UWFD2, UInt128(1)).i === UInt128(100)
        @test convert(WFD2, Int128(-1)).i === Int128(-100)
        @test_throws InexactError(:convert, UWFD2, Int128(-1)) convert(UWFD2, Int128(-1))
        @test_throws InexactError(:convert, WFD2, typemax(Int128)) convert(WFD2, typemax(Int128))
        @test_throws InexactError(:convert, UWFD2, typemax(Int128)) convert(UWFD2, typemax(Int128))
        @test_throws InexactError(:convert, WFD2, typemin(Int128)) convert(WFD2, typemin(Int128))
        @test_throws InexactError(:convert, UWFD2, typemin(Int128)) convert(UWFD2, typemin(Int128))
        @test_throws InexactError(:convert, WFD2, typemax(UInt128)) convert(WFD2, typemax(UInt128))
        @test_throws InexactError(:convert, UWFD2, typemax(UInt128)) convert(UWFD2, typemax(UInt128))
    end

    @testset "Convert from 128-bit to 64-bit" begin
        @test convert(F64D2, Int128(1)).i === Int64(100)
        @test convert(UF64D2, Int128(1)).i === UInt64(100)
        @test convert(F64D2, UInt128(1)).i === Int64(100)
        @test convert(UF64D2, UInt128(1)).i === UInt64(100)
        @test convert(F64D2, Int128(-1)).i === Int64(-100)
        @test_throws InexactError(:convert, UF64D2, Int128(-1)) convert(UF64D2, Int128(-1))
        @test_throws InexactError(:convert, F64D2, typemax(Int128)) convert(F64D2, typemax(Int128))
        @test_throws InexactError(:convert, UF64D2, typemax(Int128)) convert(UF64D2, typemax(Int128))
        @test_throws InexactError(:convert, F64D2, typemin(Int128)) convert(F64D2, typemin(Int128))
        @test_throws InexactError(:convert, UF64D2, typemin(Int128)) convert(UF64D2, typemin(Int128))
        @test_throws InexactError(:convert, F64D2, typemax(UInt128)) convert(F64D2, typemax(UInt128))
        @test_throws InexactError(:convert, UF64D2, typemax(UInt128)) convert(UF64D2, typemax(UInt128))
    end

    @testset "Convert from BigInt to 128-bit" begin
        @test convert(WFD2, BigInt(1)).i === Int128(100)
        @test convert(UWFD2, BigInt(1)).i === UInt128(100)
        @test convert(WFD2, BigInt(-1)).i === Int128(-100)
        @test_throws InexactError(:convert, UWFD2, BigInt(-1)) convert(UWFD2, BigInt(-1))
        @test_throws InexactError(:convert, FD2, BigInt(typemax(Int128))) convert(FD2, BigInt(typemax(Int128)))
    end

    @testset "Convert from 128-bit to BigInt" begin
        @test convert(FixedDecimal{BigInt,2}, Int128(1)).i == BigInt(100)
        @test convert(FixedDecimal{BigInt,2}, UInt128(1)).i == BigInt(100)
        @test convert(FixedDecimal{BigInt,2}, Int128(-1)).i == BigInt(-100)
        @test convert(FixedDecimal{BigInt,2}, typemax(UInt128)).i == BigInt(typemax(UInt128))*100

        @test convert(FixedDecimal{BigInt, 1}, Int128(1)).i == BigInt(10)
        @test convert(FixedDecimal{BigInt, 0}, Int128(1)).i == BigInt(1)
    end

    @testset "Convert from Big* to BigInt" begin
        @test convert(FixedDecimal{BigInt,2}, BigInt(1)).i == BigInt(100)
        @test convert(FixedDecimal{BigInt,2}, BigFloat(1)).i == BigInt(100)
        @test convert(FixedDecimal{BigInt,2}, BigFloat(1.5)).i == BigInt(150)

        @test convert(FixedDecimal{BigInt, 1}, BigInt(1)).i == BigInt(10)
        @test convert(FixedDecimal{BigInt, 0}, BigInt(1)).i == BigInt(1)
    end
end

module PerfTests
Base.Experimental.@optlevel 2  # For perf tests

using Test
using FixedPointDecimals

const SFD2 = FixedDecimal{Int16, 2}
const SFD4 = FixedDecimal{Int16, 4}
const F64D2 = FixedDecimal{Int64, 2}
const UF64D2 = FixedDecimal{UInt64, 2}
const WFD2 = FixedDecimal{Int128, 2}
const WFD3 = FixedDecimal{Int128, 3}
const UFD2 = FixedDecimal{UInt, 2}
const UWFD2 = FixedDecimal{UInt128, 2}

# These perf tests only make sense on 64-bit architectures, since 32-bit architectures
# use BigInts to implement Int128, so there will be allocations for many of these tests.
@static if Int === Int64
@testset "128-bit conversion performance" begin
    # Baseline cases
    @test @allocated(convert(F64D2, Int8(-1))) == 0
    @test @allocated(convert(F64D2, UInt8(1))) == 0
    @test @allocated(convert(SFD2, Int8(-1))) == 0

    # Int 128 cases
    @test @allocated(convert(WFD2, 1)) == 0
    @test @allocated(convert(WFD2, 1)) == 0
    @test @allocated(convert(UWFD2, 1)) == 0
    @test @allocated(convert(WFD2, -1)) == 0
    @test @allocated(convert(WFD2, UInt(1))) == 0
    @test @allocated(convert(UWFD2, UInt(1))) == 0

    @test @allocated(convert(WFD2, Int128(1))) == 0
    @test @allocated(convert(UWFD2, Int128(1))) == 0
    @test @allocated(convert(WFD2, UInt128(1))) == 0
    @test @allocated(convert(UWFD2, UInt128(1))) == 0
    @test @allocated(convert(WFD2, Int128(-1))) == 0

    @test @allocated(convert(F64D2, Int128(1))) == 0
    @test @allocated(convert(UF64D2, Int128(1))) == 0
    @test @allocated(convert(F64D2, UInt128(1))) == 0
    @test @allocated(convert(UF64D2, UInt128(1))) == 0
    @test @allocated(convert(F64D2, Int128(-1))) == 0
end

@testset "128-bit FD to FD conversion performance" begin
    @test @allocated(convert(WFD3, WFD2(1))) == 0
    @test @allocated(convert(SFD4, SFD2(1))) == 0
    @test @allocated(convert(SFD4, WFD2(1))) == 0
end

@testset "128-bit FD to FD conversion performance" begin
    @test @allocated(convert(WFD3, WFD2(1))) == 0
    @test @allocated(convert(SFD4, SFD2(1))) == 0
    @test @allocated(convert(SFD4, WFD2(1))) == 0
end

end  # if @static Int === Int64

@testset "BigInt conversion performance" begin
    b = BigInt(2)
    # Special-cased f=1 to not allocate for BigInt => FD conversion
    @test @allocated(convert(FixedDecimal{BigInt, 0}, b)) == 0
end

end  # module PerfTests

@testset "promotion" begin
    @test 1//10 + FD2(0.1) === 1//5
    @test 0.1 + FD2(0.1) === 0.2
    @test 1 + FD2(0.1) === FD2(1.1)
    @test FD2(0.1) + FD4(0.0001) === FD4(0.1001)
    @test WFD2(0.1) + FD4(0.0001) === WFD4(0.1001)

    # promotion with Rational
    # see https://github.com/JuliaMath/FixedPointDecimals.jl/issues/73
    r = Rational{Int8}(1//1)
    fd = FixedDecimal{Int128,4}(2.5806)
    @test (r + fd) isa Rational{Int128}
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
                @test_throws OverflowError x / -one(x)
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

    @testset "limits: with_overflow math" begin
        using FixedPointDecimals: rdiv_with_overflow, fld_with_overflow

        # Easy to reason about cases of overflow:
        @test Base.Checked.add_with_overflow(FD{Int8,2}(1), FD{Int8,2}(1)) == (FD{Int8,2}(-0.56), true)
        @test Base.Checked.add_with_overflow(FD{Int8,2}(1), FD{Int8,2}(1)) == (FD{Int8,2}(-0.56), true)
        @test Base.Checked.add_with_overflow(FD{Int8,2}(1), FD{Int8,2}(0.4)) == (FD{Int8,2}(-1.16), true)
        @test Base.Checked.sub_with_overflow(FD{Int8,2}(1), FD{Int8,2}(-1)) == (FD{Int8,2}(-0.56), true)
        @test Base.Checked.sub_with_overflow(FD{Int8,2}(-1), FD{Int8,2}(0.4)) == (FD{Int8,2}(1.16), true)
        @test Base.Checked.mul_with_overflow(FD{Int8,2}(1.2), FD{Int8,2}(1.2)) == (FD{Int8,2}(-1.12), true)

        @test div_with_overflow(FD{Int8,2}(1), FD{Int8,2}(0.5)) == (FD{Int8,2}(-0.56), true)
        @test div_with_overflow(typemin(FD{Int32,0}), FD{Int32,0}(-1)) == (typemin(FD{Int32,0}), true)
        @test div_with_overflow(FD{Int16,1}(1639), FD{Int16,1}(0.5)) == (FD{Int16,1}(-3275.6), true)

        @test rdiv_with_overflow(Int8(1), FD{Int8,2}(0.7)) == (FD{Int8,2}(-1.13), true)
        @test rdiv_with_overflow(FD{Int16,2}(165), FD{Int16,2}(0.5)) == (FD{Int16,2}(-325.36), true)
        @test rdiv_with_overflow(FD{Int16,2}(-165), FD{Int16,2}(0.5)) == (FD{Int16,2}(325.36), true)
        @test rdiv_with_overflow(typemin(FD{Int64,8}), Int32(-1)) == (typemin(FD{Int64,8}), true)
        @test rdiv_with_overflow(typemin(FD{Int64,0}), FD{Int64,0}(-1)) == (typemin(FD{Int64,0}), true)
        @test rdiv_with_overflow(typemin(FD{Int8,2}), FD{Int8,2}(-1)) == (typemin(FD{Int8,2}), true)
        @test rdiv_with_overflow(typemin(FD{Int8,2}), FD{Int8,2}(-0.01)) == (FD{Int8,2}(0), true)

        @test fld_with_overflow(FD{Int8,2}(-1), FD{Int8,2}(0.9)) == (FD{Int8,2}(0.56), true)
        @test fld_with_overflow(typemin(FD{Int64,0}), FD{Int64,0}(-1)) == (typemin(FD{Int64,0}), true)
        @test fld_with_overflow(FD{Int8,1}(7), FD{Int8,1}(0.5)) == (FD{Int8,1}(-11.6), true)
        @test FixedPointDecimals.fld_with_overflow(typemin(FD{Int8,2}), FD{Int8,2}(-0.01)) == (typemin(FD{Int8,2}), true)

        @testset "with_overflow math corner cases" begin
            @testset for I in (Int128, UInt128, Int8, UInt8), f in (0,2)
            T = FD{I, f}
                issigned(I) = signed(I) === I

                @test Base.Checked.add_with_overflow(typemax(T), eps(T)) == (typemax(T) + eps(T), true)
                issigned(I) && @test Base.Checked.add_with_overflow(typemin(T), -eps(T)) == (typemin(T) + -eps(T), true)
                @test Base.Checked.add_with_overflow(typemax(T), T(1)) == (typemax(T) + 1, true)
                @test Base.Checked.add_with_overflow(T(1), typemax(T)) == (typemax(T) + 1, true)

                @test Base.Checked.sub_with_overflow(typemin(T), eps(T)) == (typemin(T) - eps(T), true)
                issigned(I) && @test Base.Checked.sub_with_overflow(typemax(T), -eps(T)) == (typemax(T) - -eps(T), true)
                @test Base.Checked.sub_with_overflow(typemin(T), T(1)) == (typemin(T) - 1, true)
                if issigned(I) && 2.0 <= typemax(T)
                    @test Base.Checked.sub_with_overflow(T(-2), typemax(T)) == (-2 -typemax(T), true)
                end

                @test Base.Checked.mul_with_overflow(typemax(T), typemax(T)) == (typemax(T) * typemax(T), true)
                issigned(I) && @test Base.Checked.mul_with_overflow(typemin(T), typemax(T)) == (typemin(T) * typemax(T), true)
                if 2.0 <= typemax(T)
                    @test Base.Checked.mul_with_overflow(typemax(T), T(2)) == (typemax(T) * 2, true)
                    @test Base.Checked.mul_with_overflow(T(2), typemax(T)) == (2 * typemax(T), true)
                    issigned(I) && @test Base.Checked.mul_with_overflow(typemin(T), T(2)) == (typemin(T) * 2, true)
                    issigned(I) && @test Base.Checked.mul_with_overflow(T(2), typemin(T)) == (2 * typemin(T), true)
                end

                if f > 0
                    @test div_with_overflow(typemax(T), eps(T))[2]
                    issigned(I) && @test div_with_overflow(typemin(T), eps(T))[2]
                    issigned(I) && @test div_with_overflow(typemax(T), -eps(T))[2]

                    issigned(I) && @test_throws DivideError div_with_overflow(typemax(T), T(0))
                    issigned(I) && @test_throws DivideError div_with_overflow(typemin(T), T(0))
                    issigned(I) && @test div_with_overflow(typemin(T), -eps(T))[2]

                    @test fld_with_overflow(typemax(T), eps(T))[2]
                    issigned(I) && @test fld_with_overflow(typemin(T), eps(T))[2]
                    issigned(I) && @test fld_with_overflow(typemax(T), -eps(T))[2]
                end

                @test_throws DivideError rdiv_with_overflow(typemax(T), T(0))
                @test_throws DivideError rdiv_with_overflow(typemin(T), T(0))
                @test_throws DivideError rdiv_with_overflow(eps(T), T(0))
                @test_throws DivideError rdiv_with_overflow(-eps(T), T(0))

                @test_throws DivideError fld_with_overflow(typemax(T), T(0))
                @test_throws DivideError fld_with_overflow(typemin(T), T(0))
                @test_throws DivideError fld_with_overflow(eps(T), T(0))
                @test_throws DivideError fld_with_overflow(-eps(T), T(0))
            end
        end

        @testset "non-overflowing with_overflow math" begin
            @test Base.Checked.add_with_overflow(FD{Int8,1}(1), FD{Int8,1}(1.1)) == (FD{Int8,1}(2.1), false)
            @test Base.Checked.add_with_overflow(FD{Int8,1}(1.1), FD{Int8,1}(1)) == (FD{Int8,1}(2.1), false)
            @test Base.Checked.add_with_overflow(FD{Int64,8}(30.123), FD{Int64,8}(30)) == (FD{Int64,8}(60.123), false)
            @test Base.Checked.add_with_overflow(FD{Int64,8}(30.123), FD{Int64,8}(-50)) == (FD{Int64,8}(-19.877), false)

            @test Base.Checked.sub_with_overflow(FD{Int16,2}(3), FD{Int16,2}(2.5)) == (FD{Int16,1}(0.5), false)
            @test Base.Checked.sub_with_overflow(FD{Int16,2}(2.5), FD{Int16,2}(3)) == (FD{Int16,1}(-0.5), false)
            @test Base.Checked.sub_with_overflow(FD{Int32,4}(10.11), FD{Int32,4}(2)) == (FD{Int32,4}(8.11), false)
            @test Base.Checked.sub_with_overflow(FD{Int32,4}(10.11), FD{Int32,4}(-2)) == (FD{Int32,4}(12.11), false)

            @test Base.Checked.mul_with_overflow(FD{Int64,6}(4), FD{Int64,6}(2.22)) == (FD{Int64,6}(8.88), false)
            @test Base.Checked.mul_with_overflow(FD{Int64,6}(2.22), FD{Int64,6}(4)) == (FD{Int64,6}(8.88), false)
            @test Base.Checked.mul_with_overflow(FD{Int128,14}(10), FD{Int128,14}(20.1)) == (FD{Int128,14}(201), false)
            @test Base.Checked.mul_with_overflow(FD{Int128,30}(10.1), FD{Int128,30}(1)) == (FD{Int128,30}(10.1), false)

            @test div_with_overflow(FD{Int64,6}(4), FD{Int64,6}(2)) == (FD{Int64,6}(2), false)
            @test div_with_overflow(FD{Int32,6}(4), FD{Int32,6}(2.1)) == (FD{Int32,6}(1), false)
            @test div_with_overflow(FD{Int128,14}(10), FD{Int128,14}(20.1)) == (FD{Int128,14}(0), false)
            @test div_with_overflow(FD{Int128,30}(10.1), FD{Int128,30}(1)) == (FD{Int128,30}(10), false)
            @test div_with_overflow(typemin(FD{Int32,8}(1)), FD{Int32,8}(-1)) == (21, false)

            @test rdiv_with_overflow(Int8(1), FD{Int8,2}(0.8)) == (FD{Int8,2}(1.25), false)
            @test rdiv_with_overflow(FD{Int64,8}(5), FD{Int64,8}(2)) == (FD{Int64,8}(2.5), false)
            @test rdiv_with_overflow(FD{Int64,8}(5), FD{Int64,8}(0.5)) == (FD{Int64,8}(10), false)
            @test rdiv_with_overflow(FD{Int128,0}(20000), Int32(5000)) == (FD{Int128,0}(4), false)

            @test fld_with_overflow(typemax(FD{Int128,38}), FD{Int128,38}(1)) == (FD{Int128,38}(1), false)
            @test fld_with_overflow(FD{Int64,8}(20.5), FD{Int64,8}(2.1)) == (FD{Int64,8}(9), false)
            @test fld_with_overflow(FD{Int8,0}(-5), FD{Int8,0}(-1)) == (FD{Int8,0}(5), false)
            @test fld_with_overflow(FD{Int8,2}(0.99), FD{Int8,2}(0.5)) == (FD{Int8,2}(1), false)
            @test fld_with_overflow(typemin(FD{Int8,2}), FD{Int8,2}(-1)) == (FD{Int8,2}(1), false)
        end
    end

    @testset "limits: overflow checked math" begin
        # Easy to reason about cases of overflow:
        @test_throws OverflowError Base.checked_add(FD{Int8,2}(1), FD{Int8,2}(1))
        @test_throws OverflowError Base.checked_add(FD{Int8,2}(1), 1)
        @test_throws OverflowError Base.checked_add(FD{Int8,2}(1), FD{Int8,2}(0.4))

        @test_throws OverflowError Base.checked_sub(FD{Int8,2}(1), FD{Int8,2}(-1))
        @test_throws OverflowError Base.checked_sub(1, FD{Int8,2}(-1))
        @test_throws OverflowError Base.checked_sub(FD{Int8,2}(-1), FD{Int8,2}(0.4))

        @test_throws OverflowError Base.checked_mul(FD{Int8,2}(1.2), FD{Int8,2}(1.2))
        @test_throws OverflowError Base.checked_mul(FD{Int8,1}(12), 2)
        @test_throws OverflowError Base.checked_mul(FD{Int8,0}(120), 2)
        @test_throws OverflowError Base.checked_mul(120, FD{Int8,0}(2))

        @test_throws OverflowError Base.checked_div(FD{Int8,2}(1), FD{Int8,2}(0.5))
        @test_throws OverflowError Base.checked_div(1, FD{Int8,2}(0.5))
        @test_throws OverflowError Base.checked_div(FD{Int8,2}(1), FD{Int8,2}(0.4))

        @testset "checked_rdiv" begin
            using FixedPointDecimals: checked_rdiv

            @test checked_rdiv(Int8(1), FD{Int8,2}(0.8)) == FD{Int8,2}(1.25)
            @test_throws OverflowError checked_rdiv(Int8(1), FD{Int8,2}(0.7))
        end

        # Rounds down to -2
        @test_throws OverflowError Base.checked_fld(FD{Int8,2}(-1), FD{Int8,2}(0.9))
        # Rounds up to 2
        @test_throws OverflowError Base.checked_cld(FD{Int8,2}(1),  FD{Int8,2}(0.9))

        # Rem and Mod only throw DivideError and nothing more. They can't overflow, since
        # they can only return smaller values than the arguments.
        @test_throws DivideError Base.checked_rem(FD{Int8,2}(-1), FD{Int8,2}(0))
        @test_throws DivideError Base.checked_mod(FD{Int8,2}(-1), FD{Int8,2}(0))

        @test_throws OverflowError Base.checked_abs(typemin(FD{Int8,2}))
        @test_throws OverflowError Base.checked_neg(typemin(FD{Int8,2}))
        @test Base.checked_abs(typemax(FD{Int8,2})) == FD{Int8,2}(1.27)
        @test Base.checked_neg(typemax(FD{Int8,2})) == FD{Int8,2}(-1.27)

        @testset "checked math corner cases" begin
            @testset for I in (Int128, UInt128, Int8, UInt8), f in (0,2)
            T = FD{I, f}
                issigned(I) = signed(I) === I

                @test_throws OverflowError Base.checked_add(typemax(T), eps(T))
                issigned(I) && @test_throws OverflowError Base.checked_add(typemin(T), -eps(T))
                @test_throws OverflowError Base.checked_add(typemax(T), 1)
                @test_throws OverflowError Base.checked_add(1, typemax(T))

                @test_throws OverflowError Base.checked_sub(typemin(T), eps(T))
                issigned(I) && @test_throws OverflowError Base.checked_sub(typemax(T), -eps(T))
                @test_throws OverflowError Base.checked_sub(typemin(T), 1)
                if issigned(I) && 2.0 <= typemax(T)
                    @test_throws OverflowError Base.checked_sub(-2, typemax(T))
                end

                @test_throws OverflowError Base.checked_mul(typemax(T), typemax(T))
                issigned(I) && @test_throws OverflowError Base.checked_mul(typemin(T), typemax(T))
                if 2.0 <= typemax(T)
                    @test_throws OverflowError Base.checked_mul(typemax(T), 2)
                    @test_throws OverflowError Base.checked_mul(2, typemax(T))
                    issigned(I) && @test_throws OverflowError Base.checked_mul(typemin(T), 2)
                    issigned(I) && @test_throws OverflowError Base.checked_mul(2, typemin(T))
                end

                if f > 0
                    @test_throws OverflowError Base.checked_div(typemax(T), eps(T))
                    issigned(I) && @test_throws OverflowError Base.checked_div(typemin(T), eps(T))
                    issigned(I) && @test_throws OverflowError Base.checked_div(typemax(T), -eps(T))

                    issigned(I) && @test_throws DivideError Base.checked_div(typemax(T), T(0))
                    issigned(I) && @test_throws DivideError Base.checked_div(typemin(T), T(0))
                    issigned(I) && @test_throws DivideError Base.checked_div(typemin(T), -eps(T))
                end

                if f > 0
                    @test_throws OverflowError Base.checked_fld(typemax(T), eps(T))
                    issigned(I) && @test_throws OverflowError Base.checked_fld(typemin(T), eps(T))
                    issigned(I) && @test_throws OverflowError Base.checked_fld(typemax(T), -eps(T))

                    @test_throws OverflowError Base.checked_cld(typemax(T), eps(T))
                    issigned(I) && @test_throws OverflowError Base.checked_cld(typemin(T), eps(T))
                    issigned(I) && @test_throws OverflowError Base.checked_cld(typemax(T), -eps(T))
                end

                issigned(I) && @test_throws OverflowError Base.checked_abs(typemin(T))
                issigned(I) && @test_throws OverflowError Base.checked_neg(typemin(T))
            end
        end

        @testset "checked math promotions" begin
            x = FD{Int8,1}(1)
            y = FD{Int64,1}(2)
            @testset for op in (
                Base.checked_add, Base.checked_sub, Base.checked_mul, Base.checked_div,
                Base.checked_cld, Base.checked_fld, Base.checked_rem, Base.checked_mod,
                FixedPointDecimals.checked_rdiv,
            )
                @test op(x, y) === op(FD{Int64,1}(1), y)
                @test op(y, x) === op(y, FD{Int64,1}(1))

                @test op(x, 2) === op(x, FD{Int8,1}(2))
                @test op(2, x) === op(FD{Int8,1}(2), x)
            end
        end
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

@testset "truncating div" begin
    @testset "div by 1" begin
        @testset for x in keyvalues[FD2]
            @test x ÷ one(x) === trunc(x)

            # signed integers using two's complement have one additional negative value
            if x < 0 && trunc(x) === typemin(x)
                @test_throws InexactError x ÷ -one(x)
            else
                @test x ÷ -one(x) === -trunc(x)
            end
        end
    end
    @testset "div by 2" begin
        @testset for x in keyvalues[FD2]
            @test x ÷ 2one(x) === x ÷ 2 === FD2(x.i ÷ FD2(2).i)
        end
    end
    @testset "return types" begin
        @test div(2one(FD2), 3) isa FD2
        @test one(FD2) ÷ one(FD2) isa FD2

        # Promotion to bigger type
        @test one(FD4) ÷ one(FD2) isa FD4
        @test one(FD2) ÷ one(FD4) isa FD4

        @test one(FD{Int32, 2}) ÷ one(FD{Int64, 6}) isa FD{Int64, 6}
    end

    @testset "div with rounding modes" begin
        if VERSION >= v"1.4.0-"
            @testset for x in keyvalues[FD2]
                # TODO: Test RoundFromZero -- https://github.com/JuliaLang/julia/issues/34519
                for R in (RoundToZero, RoundUp, RoundDown, RoundNearest, RoundNearestTiesAway)
                    @test div(x, 2one(x), R) === div(x, 2, R) === FD2(div(x.i, FD2(2).i, R))
                end
            end
        end
        @testset for x in keyvalues[FD2], f in (fld, cld, fld1, div)
            @test f(x, 2one(x)) === f(x, 2) === FD2(f(x.i, FD2(2).i))
        end
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

@testset "overflow" begin
    T = FD{Int8, 1}
    @testset "addition" begin
        @test typemax(T) + eps(T) == typemin(T)
        @test typemin(T) + (-eps(T)) == typemax(T)
    end

    @testset "subtraction" begin
        @test typemin(T) - eps(T) == typemax(T)
        @test typemax(T) - (-eps(T)) == typemin(T)
    end

    @testset "multiplication" begin
        @test typemax(T) * 2 == T(-0.2)
        @test typemin(T) * 2 == T(0)
    end

    @testset "division" begin
        @test_throws OverflowError typemax(T) / T(0.5)
        @test_throws OverflowError typemin(T) / T(0.5)
    end

    @testset "truncating division" begin
        @test_throws OverflowError typemax(T) ÷ T(0.5)
        @test_throws OverflowError typemin(T) ÷ T(0.5)
        @test_throws OverflowError typemax(T) ÷ eps(T)
        @test_throws OverflowError typemin(T) ÷ eps(T)

        @test_throws OverflowError div(typemax(T), T(0.5), RoundUp)
        @test_throws OverflowError div(typemin(T), T(0.5), RoundUp)
        @test_throws OverflowError div(typemax(T), eps(T), RoundUp)
        @test_throws OverflowError div(typemin(T), eps(T), RoundUp)
    end

    @testset "fld / cld" begin
        @test_throws OverflowError fld(typemax(T), T(0.5))
        @test_throws OverflowError fld(typemin(T), T(0.5))
        @test_throws OverflowError fld(typemax(T), eps(T))
        @test_throws OverflowError fld(typemin(T), eps(T))

        @test_throws OverflowError fld1(typemax(T), T(0.5))
        @test_throws OverflowError fld1(typemin(T), T(0.5))
        @test_throws OverflowError fld1(typemax(T), eps(T))
        @test_throws OverflowError fld1(typemin(T), eps(T))

        @test_throws OverflowError cld(typemax(T), T(0.5))
        @test_throws OverflowError cld(typemin(T), T(0.5))
        @test_throws OverflowError cld(typemax(T), eps(T))
        @test_throws OverflowError cld(typemin(T), eps(T))
    end

    @testset "abs / neg" begin
        @test abs(typemin(T)) == typemin(T)
        @test -(typemin(T)) == typemin(T)
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

    # Is alias for `ceil`.
    @testset "up" begin
        @test round(Int, FD2(-0.51), RoundUp) === 0
        @test round(Int, FD2(-0.50), RoundUp) === 0
        @test round(Int, FD2(-0.49), RoundUp) === 0
        @test round(Int, FD2(0.50), RoundUp) === 1
        @test round(Int, FD2(0.51), RoundUp) === 1
        @test round(Int, FD2(1.50), RoundUp) === 2
    end

    # Is alias for `floor`.
    @testset "down" begin
        @test round(Int, FD2(-0.51), RoundDown) === -1
        @test round(Int, FD2(-0.50), RoundDown) === -1
        @test round(Int, FD2(-0.49), RoundDown) === -1
        @test round(Int, FD2(0.50), RoundDown) === 0
        @test round(Int, FD2(0.51), RoundDown) === 0
        @test round(Int, FD2(1.50), RoundDown) === 1
    end

    # Is alias for `trunc`.
    @testset "to zero" begin
        @test round(Int, FD2(-0.51), RoundToZero) === 0
        @test round(Int, FD2(-0.50), RoundToZero) === 0
        @test round(Int, FD2(-0.49), RoundToZero) === 0
        @test round(Int, FD2(0.50), RoundToZero) === 0
        @test round(Int, FD2(0.51), RoundToZero) === 0
        @test round(Int, FD2(1.50), RoundToZero) === 1
    end

    @testset "tie away" begin
        @test round(Int, FD2(-0.51), RoundNearestTiesAway) === -1
        @test round(Int, FD2(-0.50), RoundNearestTiesAway) === -1
        @test round(Int, FD2(-0.49), RoundNearestTiesAway) === 0
        @test round(Int, FD2(0.50), RoundNearestTiesAway) === 1
        @test round(Int, FD2(0.51), RoundNearestTiesAway) === 1
        @test round(Int, FD2(1.50), RoundNearestTiesAway) === 2
    end

    @testset "tie up" begin
        @test round(Int, FD2(-0.51), RoundNearestTiesUp) === -1
        @test round(Int, FD2(-0.50), RoundNearestTiesUp) === 0
        @test round(Int, FD2(-0.49), RoundNearestTiesUp) === 0
        @test round(Int, FD2(0.50), RoundNearestTiesUp) === 1
        @test round(Int, FD2(0.51), RoundNearestTiesUp) === 1
        @test round(Int, FD2(1.50), RoundNearestTiesUp) === 2
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

@testset "round_with_overflow" begin
    using FixedPointDecimals: round_with_overflow

    FD642 = FixedDecimal{Int64,2}
    FD643 = FixedDecimal{Int64,3}

    # Is alias for `ceil`.
    @testset "up" begin
        @test round_with_overflow(FD642(-0.51), RoundUp) === (FD642(0), false)
        @test round_with_overflow(FD642(-0.50), RoundUp) === (FD642(0), false)
        @test round_with_overflow(FD642(-0.49), RoundUp) === (FD642(0), false)
        @test round_with_overflow(FD642(0.50), RoundUp) === (FD642(1), false)
        @test round_with_overflow(FD642(0.51), RoundUp) === (FD642(1), false)
        @test round_with_overflow(FD642(1.50), RoundUp) === (FD642(2), false)
        @test round_with_overflow(typemin(FD642), RoundUp) ===
            (parse(FD642, "-92233720368547758"), false)

        @testset "overflowing" begin
            @test round_with_overflow(typemax(FD642), RoundUp) ===
                (parse(FD642, "-92233720368547757.16"), true)
            @test round_with_overflow(parse(FD642, "92233720368547758.01"), RoundUp) ===
                (parse(FD642, "-92233720368547757.16"), true)
        end
    end

    # Is alias for `floor`.
    @testset "down" begin
        @test round_with_overflow(FD642(-0.51), RoundDown) === (FD642(-1), false)
        @test round_with_overflow(FD642(-0.50), RoundDown) === (FD642(-1), false)
        @test round_with_overflow(FD642(-0.49), RoundDown) === (FD642(-1), false)
        @test round_with_overflow(FD642(0.50), RoundDown) === (FD642(0), false)
        @test round_with_overflow(FD642(0.51), RoundDown) === (FD642(0), false)
        @test round_with_overflow(FD642(1.50), RoundDown) === (FD642(1), false)
        @test round_with_overflow(typemax(FD642), RoundDown) ===
            (parse(FD642, "92233720368547758"), false)

        @testset "overflowing" begin
            @test round_with_overflow(typemin(FD642), RoundDown) ===
                (parse(FD642, "92233720368547757.16"), true)
            @test round_with_overflow(parse(FD642, "-92233720368547758.01"), RoundDown) ===
                (parse(FD642, "92233720368547757.16"), true)
        end
    end

    # Is alias for `trunc`.
    @testset "to zero" begin
        @test round_with_overflow(FD642(-0.51), RoundToZero) === (FD642(0), false)
        @test round_with_overflow(FD642(-0.50), RoundToZero) === (FD642(0), false)
        @test round_with_overflow(FD642(-0.49), RoundToZero) === (FD642(0), false)
        @test round_with_overflow(FD642(0.50), RoundToZero) === (FD642(0), false)
        @test round_with_overflow(FD642(0.51), RoundToZero) === (FD642(0), false)
        @test round_with_overflow(FD642(1.50), RoundToZero) === (FD642(1), false)

        @test round_with_overflow(typemin(FD642), RoundToZero) ===
            (parse(FD642, "-92233720368547758"), false)
        @test round_with_overflow(typemax(FD642), RoundToZero) ===
            (parse(FD642, "92233720368547758"), false)

        # Cannot overflow.
    end

    @testset "tie away" begin
        @test round_with_overflow(FD642(-0.51), RoundNearestTiesAway) === (FD642(-1), false)
        @test round_with_overflow(FD642(-0.50), RoundNearestTiesAway) === (FD642(-1), false)
        @test round_with_overflow(FD642(-0.49), RoundNearestTiesAway) === (FD642(0), false)
        @test round_with_overflow(FD642(0.50), RoundNearestTiesAway) === (FD642(1), false)
        @test round_with_overflow(FD642(0.51), RoundNearestTiesAway) === (FD642(1), false)
        @test round_with_overflow(FD642(1.50), RoundNearestTiesAway) === (FD642(2), false)

        @test round_with_overflow(typemin(FD642), RoundNearestTiesAway) ===
            (parse(FD642, "-92233720368547758"), false)
        @test round_with_overflow(typemax(FD642), RoundNearestTiesAway) ===
            (parse(FD642, "92233720368547758"), false)

        @testset "overflowing" begin
            # For max, FD642 has fractional .07 so use FD643 which has .807.
            @test round_with_overflow(typemin(FD643), RoundNearestTiesAway) ===
                (parse(FD643, "9223372036854775.616"), true)
            @test round_with_overflow(typemax(FD643), RoundNearestTiesAway) ===
                (parse(FD643, "-9223372036854775.616"), true)

            @test round_with_overflow(parse(FD643, "9223372036854775.5"), RoundNearestTiesAway) ===
                (parse(FD643, "-9223372036854775.616"), true)
            @test round_with_overflow(parse(FD643, "-9223372036854775.5"), RoundNearestTiesAway) ===
                (parse(FD643, "9223372036854775.616"), true)
        end
    end

    @testset "tie up" begin
        @test round_with_overflow(FD642(-0.51), RoundNearestTiesUp) === (FD642(-1), false)
        @test round_with_overflow(FD642(-0.50), RoundNearestTiesUp) === (FD642(0), false)
        @test round_with_overflow(FD642(-0.49), RoundNearestTiesUp) === (FD642(0), false)
        @test round_with_overflow(FD642(0.50), RoundNearestTiesUp) === (FD642(1), false)
        @test round_with_overflow(FD642(0.51), RoundNearestTiesUp) === (FD642(1), false)
        @test round_with_overflow(FD642(1.50), RoundNearestTiesUp) === (FD642(2), false)

        @test round_with_overflow(typemin(FD642), RoundNearestTiesUp) ===
            (parse(FD642, "-92233720368547758"), false)
        @test round_with_overflow(typemax(FD642), RoundNearestTiesUp) ===
            (parse(FD642, "92233720368547758"), false)

        # For max, FD642 has fractional .07 so use FD643 which has .807.
        @test round_with_overflow(parse(FD643, "-9223372036854775.5"), RoundNearestTiesUp) ===
            (FD643(-9223372036854775), false)

        @testset "overflowing" begin
            @test round_with_overflow(typemin(FD643), RoundNearestTiesUp) ===
                (parse(FD643, "9223372036854775.616"), true)
            @test round_with_overflow(typemax(FD643), RoundNearestTiesUp) ===
                (parse(FD643, "-9223372036854775.616"), true)

            @test round_with_overflow(parse(FD643, "9223372036854775.5"), RoundNearestTiesUp) ===
                (parse(FD643, "-9223372036854775.616"), true)
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

@testset "floor_with_overflow" begin
    using FixedPointDecimals: floor_with_overflow

    @testset "non-overflowing" begin
        @test floor_with_overflow(FD{Int8,2}(1.02)) == (FD{Int8,2}(1), false)
        @test floor_with_overflow(FD{Int8,2}(-0.02)) == (FD{Int8,2}(-1), false)
        @test floor_with_overflow(FD{Int8,2}(-1)) == (FD{Int8,2}(-1), false)

        @test floor_with_overflow(FD{Int16,1}(5.2)) == (FD{Int16,1}(5), false)
        @test floor_with_overflow(FD{Int16,1}(-5.2)) == (FD{Int16,1}(-6), false)

        @test floor_with_overflow(typemax(FD{Int32,0})) == (typemax(FD{Int32,0}), false)
        @test floor_with_overflow(typemin(FD{Int32,0})) == (typemin(FD{Int32,0}), false)

        @test floor_with_overflow(FD{Int64,8}(40.054672)) == (FD{Int64,8}(40), false)
        @test floor_with_overflow(FD{Int64,8}(-40.054672)) == (FD{Int64,8}(-41), false)
        @test floor_with_overflow(FD{Int64,8}(-92233720368)) ==
            (FD{Int64,8}(-92233720368), false)

        @test floor_with_overflow(typemax(FD{Int128,18})) ==
            (FD{Int128,18}(170141183460469231731), false)
        @test floor_with_overflow(FD{Int128,18}(-400.0546798232)) ==
            (FD{Int128,18}(-401), false)
    end

    @testset "overflowing" begin
        @test floor_with_overflow(typemin(FD{Int8,2})) == (FD{Int8,2}(0.56), true)
        @test floor_with_overflow(FD{Int8,2}(-1.02)) == (FD{Int8,2}(0.56), true)

        @test floor_with_overflow(typemin(FD{Int16,3})) == (FD{Int16,3}(32.536), true)
        @test floor_with_overflow(FD{Int16,3}(-32.111)) == (FD{Int16,3}(32.536), true)

        @test floor_with_overflow(typemin(FD{Int32,1})) == (FD{Int32,1}(214748364.6), true)
        @test floor_with_overflow(FD{Int32,1}(-214748364.7)) ==
            (FD{Int32,1}(214748364.6), true)

        @test floor_with_overflow(typemin(FD{Int64,8})) ==
            (parse(FD{Int64,8}, "92233720368.09551616"), true)
        @test floor_with_overflow(FD{Int64,8}(-92233720368.5)) ==
            (parse(FD{Int64,8}, "92233720368.09551616"), true)

        @test floor_with_overflow(typemin(FD{Int128,2})) ==
            (parse(FD{Int128,2}, "1701411834604692317316873037158841056.56"), true)
        @test floor_with_overflow(parse(FD{Int128,2}, "-1701411834604692317316873037158841057.27")) ==
            (parse(FD{Int128,2}, "1701411834604692317316873037158841056.56"), true)
    end
end

@testset "ceil_with_overflow" begin
    using FixedPointDecimals: ceil_with_overflow

    @testset "non-overflowing" begin
        @test ceil_with_overflow(FD{Int8,2}(-1.02)) == (FD{Int8,2}(-1), false)
        @test ceil_with_overflow(FD{Int8,2}(-0.02)) == (FD{Int8,2}(0), false)
        @test ceil_with_overflow(FD{Int8,2}(0.49)) == (FD{Int8,2}(1), false)
        @test ceil_with_overflow(FD{Int8,2}(1)) == (FD{Int8,2}(1), false)

        @test ceil_with_overflow(FD{Int16,1}(5.2)) == (FD{Int16,1}(6), false)
        @test ceil_with_overflow(FD{Int16,1}(-5.2)) == (FD{Int16,1}(-5), false)

        @test ceil_with_overflow(typemax(FD{Int32,0})) == (typemax(FD{Int32,0}), false)
        @test ceil_with_overflow(typemin(FD{Int32,0})) == (typemin(FD{Int32,0}), false)

        @test ceil_with_overflow(FD{Int64,8}(40.054672)) == (FD{Int64,8}(41), false)
        @test ceil_with_overflow(FD{Int64,8}(-40.054672)) == (FD{Int64,8}(-40), false)
        @test ceil_with_overflow(FD{Int64,8}(-92233720368)) ==
            (FD{Int64,8}(-92233720368), false)
        @test ceil_with_overflow(FD{Int64,8}(92233720368)) ==
            (FD{Int64,8}(92233720368), false)

        @test ceil_with_overflow(typemin(FD{Int128,18})) ==
            (FD{Int128,18}(-170141183460469231731), false)
        @test ceil_with_overflow(FD{Int128,18}(-400.0546798232)) ==
            (FD{Int128,18}(-400), false)
    end

    @testset "overflowing" begin
        @test ceil_with_overflow(typemax(FD{Int8,2})) == (FD{Int8,2}(-0.56), true)
        @test ceil_with_overflow(FD{Int8,2}(1.02)) == (FD{Int8,2}(-0.56), true)

        @test ceil_with_overflow(typemax(FD{Int16,3})) == (FD{Int16,3}(-32.536), true)
        @test ceil_with_overflow(FD{Int16,3}(32.111)) == (FD{Int16,3}(-32.536), true)

        @test ceil_with_overflow(typemax(FD{Int32,1})) == (FD{Int32,1}(-214748364.6), true)
        @test ceil_with_overflow(FD{Int32,1}(214748364.7)) ==
            (FD{Int32,1}(-214748364.6), true)

        @test ceil_with_overflow(typemax(FD{Int64,8})) ==
            (parse(FD{Int64,8}, "-92233720368.09551616"), true)
        @test ceil_with_overflow(FD{Int64,8}(92233720368.5)) ==
            (parse(FD{Int64,8}, "-92233720368.09551616"), true)

        @test ceil_with_overflow(typemax(FD{Int128,2})) ==
            (parse(FD{Int128,2}, "-1701411834604692317316873037158841056.56"), true)
        @test ceil_with_overflow(parse(FD{Int128,2}, "1701411834604692317316873037158841057.27")) ==
            (parse(FD{Int128,2}, "-1701411834604692317316873037158841056.56"), true)
    end
end

@testset "type stability" begin
    # Test that basic operations are type stable for all the basic integer types.
    fs = [0, 1, 2, 7, 16, 38]  # To save time, don't test all possible combinations.
    @testset for T in (CONTAINER_TYPES..., BigInt,)
        maxF = FixedPointDecimals.max_exp10(T)
        frange = filter(f->f<=maxF, fs)
        # Unary operations
        @testset for f in frange
            @test @inferred(zero(FD{T,f}(1))) === FD{T,f}(0)
            @test @inferred(one(FD{T,f}(1))) === FD{T,f}(1)
            @test @inferred(ceil(FD{T,f}(1))) === FD{T,f}(1)
            @test @inferred(round(FD{T,f}(1))) === FD{T,f}(1)
            @test @inferred(abs(FD{T,f}(1))) === FD{T,f}(1)
            @test @inferred(FD{T,f}(1)^2) === FD{T,f}(1)
            @test @inferred(typemax(FD{T,f})) isa FD{T,f}
        end
        # Binary operations
        @testset for (f1,f2) in Iterators.product(frange, frange)
            fmax = max(f1,f2)
            @test @inferred(FD{T,f1}(1) + FD{T,f2}(0)) === FD{T,fmax}(1)
            @test @inferred(FD{T,f1}(1) - FD{T,f2}(0)) === FD{T,fmax}(1)
            @test @inferred(FD{T,f1}(1) * FD{T,f2}(1)) === FD{T,fmax}(1)
            @test @inferred(FD{T,f1}(1) / FD{T,f2}(1)) === FD{T,fmax}(1)
            @test @inferred(FD{T,f1}(1) ÷ FD{T,f2}(1)) === FD{T,fmax}(1)
            @test @inferred(max(FD{T,f1}(1), FD{T,f2}(0))) === FD{T,fmax}(1)
            @test @inferred(min(FD{T,f1}(1), FD{T,f2}(0))) === FD{T,fmax}(0)
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
        @test sprint(show, FD2(1.00), context=:compact=>true) == "1.0"
        @test sprint(show, FD2(1.23), context=:compact=>true) == "1.23"
        @test sprint(show, FD2(42.40), context=:compact=>true) == "42.4"
        @test sprint(show, FD2(-42.40), context=:compact=>true) == "-42.4"
        @test sprint(show, FD2(-0.01), context=:compact=>true) == "-0.01"
        @test sprint(show, FD2(0), context=:compact=>true) == "0.0"

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

@testset "parsing" begin
    @time include(joinpath(pkg_path, "test", "parse_tests.jl"))
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


@testset "ambiguities" begin
    # Unit tests for the methods added to resolve Aqua-detected ambiguities.
    @test widemul(true, FD3(1.5)) == FD3(1.5)
    @test widemul(FD3(1.5), true) == FD3(1.5)
    @test trunc(FD3, 4//3) == FD3(1.333)
    @test floor(FD3, 4//3) == FD3(1.333)
    @test ceil(FD3, 4//3) == FD3(1.334)
    @test round(FD3, true) == FD3(1.000)
    @test round(FD3, 4//3) == FD3(1.333)
    @test round(FD3, 1 // 1) == FD3(1)
    @test round(FD3, BigInt(1) // 1) == FD3(1)
    @test round(FD3, true // true) == FD3(1)
    @test Bool(FixedDecimal{Int,4}(1))
end
