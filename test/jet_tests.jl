using Test
using JET
using FixedPointDecimals

@testset "JET: FD multiplication" begin
    @testset for T in (Int8, Int16, Int32, Int64, Int128)
        max_f = FixedPointDecimals.max_exp10(T)
        @testset for f in (0:max_f)
            FD = FixedDecimal{T, f}
            a = FD(1)
            b = reinterpret(FD, typemax(T))
            @testset let T=T, f=f, a=a, b=b
                @test a * b == b
                @test_opt a * b
                @test_call a * b
            end
        end
    end
end
