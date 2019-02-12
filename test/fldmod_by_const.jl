using Test
using FixedPointDecimals

@testset "fldmod_by_const" begin
    vals = [2432, 100, 0x1, Int32(10000), typemax(Int64), typemax(Int16), 8, Int64(2)^32]
    for a_base in vals
        # Only test negative numbers on `a`, since fldmod_by_const requires b > 0.
        @testset for (a, b, f) in Iterators.product((a_base, -a_base), vals, (unsigned, signed))
            a, b = promote(f(a), f(b))
            @test FixedPointDecimals.fldmod_by_const(a, Val(b)) == fldmod(a, b)
        end
    end
end

@testset "div_by_const" begin
    vals = [2432, 100, 0x1, Int32(10000), typemax(Int64), typemax(Int16), 8, Int64(2)^32]
    for a_base in vals
        # Only test negative numbers on `a`, since div_by_const requires b > 0.
        @testset for (a, b, f) in Iterators.product((a_base, -a_base), vals, (unsigned, signed))
            a, b = promote(f(a), f(b))
            @test FixedPointDecimals.div_by_const(a, Val(b)) == a ÷ b
        end
    end
end

@testset "splitmul_upper" begin
    vals = [0xA3D70A3D70A3D70B, 0x6666666666666666, typemax(Int16), typemax(Int16),
            typemax(Int64), typemax(Int64)]
    @testset for (a, b, f) in Iterators.product(vals, vals, (unsigned, signed))
        a, b = promote(f(a), f(b))
        @test (FixedPointDecimals.splitmul_upper(a, b) ==
               FixedPointDecimals.splitint(widemul(a, b))[1])
    end
end

@testset "narrow(::Type{T})" begin
    types = (Int128, Int64, Int32, Int16)
    narrowed = (Int64, Int32, Int16, Int8)
    @testset for (T,N) in zip(types, narrowed)
        @test FixedPointDecimals.narrow(T) == N
        @test FixedPointDecimals.narrow(unsigned(T)) == unsigned(N)
    end
end
