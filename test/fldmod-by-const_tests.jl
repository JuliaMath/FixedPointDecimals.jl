using Test
using FixedPointDecimals

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

@testset "fldmod_by_const" begin
    vals = [2432, 100, 0x1, Int32(10000), typemax(Int64), typemax(Int16), 8, Int64(2)^32]
    for a_base in vals
        # Only test negative numbers on `a`, since fldmod_by_const requires b > 0.
        @testset for (a, b, f) in Iterators.product((a_base, -a_base), vals, (unsigned, signed))
            a, b = promote(f(a), f(b))
            @test FixedPointDecimals.fldmod_by_const(a, b) == fldmod(a, b)
        end
    end
end

# We don't actually use fldmod_by_const with 8-bit ints, but they're useful because
# we can exhaustively test every possible combination, to increase our confidence in
# the implementation.
@testset "flmdod_by_const - exhaustive 8-bit" begin
    @testset for T in (Int8, UInt8)
        @testset for x in typemin(T) : typemax(T)
            @testset for y in typemin(T) : typemax(T)
                y == 0 && continue
                y == -1 && continue
                @testset "fldmod($x, $y)" begin
                    @test fldmod(x, y) == FixedPointDecimals.fldmod_by_const(x, y)
                end
            end
        end
    end
end

# 64-bit FD multiplication uses the custom fldmod_by_const implementation.
# Test a few various different cases to try to ensure it works correctly.
@testset "fixed decimal multiplication - 64-bit" begin
    @testset for P in (0,1,2,3,4)
        @testset for T in (Int64, UInt64)
            FD = FixedDecimal{T,P}

            function test_multiplies_correctly(fd, x)
                big = FixedDecimal{BigInt, P}(fd)
                big_mul = big * x
                # This might overflow: ...
                mul = fd * x
                @testset "$fd * $x" begin
                    # ... so we truncate big to the same size
                    @test big_mul.i % T == mul.i % T
                end
            end
            num_tests = 2<<11
            # Add one to avoid powers-of-2 to get hopefully better tests
            epsilon = (typemax(FD) ÷ num_tests) + eps(FD)
            @testset for v in typemin(FD) : epsilon : typemax(FD)
                test_multiplies_correctly(v, typemin(T))
                test_multiplies_correctly(v, -1)
                test_multiplies_correctly(v, -eps(FD))
                test_multiplies_correctly(v, 0)
                test_multiplies_correctly(v, eps(FD))
                test_multiplies_correctly(v, 1)
                test_multiplies_correctly(v, 2)
                test_multiplies_correctly(v, 3)
                test_multiplies_correctly(v, typemax(T))
            end
        end
    end
end

@testset "fixed decimal multiplication - 128-bit" begin
    @testset for P in 0:37
        @testset for T in (Int128, UInt128)
            FD = FixedDecimal{T,P}

            function test_multiplies_correctly(fd, x)
                big = FixedDecimal{BigInt, P}(fd)
                big_mul = big * x
                # This might overflow: ...
                mul = fd * x
                @testset "$fd * $x" begin
                    # ... so we truncate big to the same size
                    @test big_mul.i % T == mul.i % T
                end
            end
            vals = FD[
                typemin(FD), typemax(FD),
                typemin(FD) + eps(FD), typemax(FD) - eps(FD),
                0.0, eps(FD), 0.1, 1.0, 2.0,
                typemax(FD) ÷ 2,
            ]
            if T <: Signed
                append!(vals, vals.*-1)
            end
            @testset for v in vals
                test_multiplies_correctly(v, typemin(T))
                test_multiplies_correctly(v, -1)
                test_multiplies_correctly(v, -eps(FD))
                test_multiplies_correctly(v, 0)
                test_multiplies_correctly(v, eps(FD))
                test_multiplies_correctly(v, 1)
                test_multiplies_correctly(v, 2)
                test_multiplies_correctly(v, 3)
                test_multiplies_correctly(v, typemax(T))
            end
        end
    end
end
