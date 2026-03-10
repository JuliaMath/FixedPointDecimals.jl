using Test
using JET
using FixedPointDecimals

@testset "JET test_opt: FD multiplication" begin
    @testset for T in (Int8, Int16, Int32, Int64, Int128)
        max_f = FixedPointDecimals.max_exp10(T)
        @testset for f in (0, 1, max_f ÷ 2, max_f)
            FD = FixedDecimal{T, f}
            a = reinterpret(FD, T(1))
            b = reinterpret(FD, T(2))
            @testset let T=T, f=f
                @test_opt a * b
            end
        end
    end
end
