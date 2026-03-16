using Test
using FixedPointDecimals

@testset "fld_by_const" begin
    # Divisors covering each branch in fld_by_const:
    #   C == 1 (identity), ispow2(C) (shift path), else (magic number path)
    # Also: powers of 10 (actual FixedDecimal divisors), small odd divisors, large divisor
    divisors = [1, 2, 4, 8, Int64(2)^32,    # C==1 and ispow2 paths
                3, 5, 7, 9, 10, 100, 1000,  # magic number path
                Int64(10)^9, Int64(10)^18,  # large powers of 10
                1000000007, typemax(Int16)] # large odd divisors
    x_vals(T, C) = T[
        zero(T), one(T), -one(T),
        C, -C, T(C - 1), T(-(C - 1)),    # near and at exact multiples
        typemax(T), typemin(T) + one(T), # type boundaries
    ]
    for C_base in divisors
        for f in (unsigned, signed)
            C = f(C_base)
            T = typeof(C)
            for x in x_vals(T, C)
                @testset let T=T, C=C, x=x
                    @test FixedPointDecimals.fld_by_const(x, Val(C)) == fld(x, C)
                end
            end
        end
    end
    @testset "Int128/UInt128" begin
        for T in (Int128, UInt128)
            for C in (T(3), T(10), T(1000), T(10)^9, T(10)^18)
                for x in x_vals(T, C)
                    @testset let T=T, C=C, x=x
                        @test FixedPointDecimals.fld_by_const(x, Val(C)) == fld(x, C)
                    end
                end
            end
        end
    end
end

@testset "fldmod_by_const" begin
    divisors = [1, 2, 4, 8, Int64(2)^32,
                3, 5, 7, 9, 10, 100, 1000,
                Int64(10)^9, Int64(10)^18,
                1000000007, typemax(Int16)]
    x_vals(T, C) = T[
        zero(T), one(T), -one(T),
        C, -C, T(C - 1), T(-(C - 1)),
        typemax(T), typemin(T) + one(T),
    ]
    for C_base in divisors
        for f in (unsigned, signed)
            C = f(C_base)
            T = typeof(C)
            for x in x_vals(T, C)
                @testset let T=T, C=C, x=x
                    @test FixedPointDecimals.fldmod_by_const(x, C) == fldmod(x, C)
                end
            end
        end
    end
    @testset "Int128/UInt128" begin
        for T in (Int128, UInt128)
            for C in (T(3), T(10), T(1000), T(10)^9, T(10)^18)
                for x in x_vals(T, C)
                    @testset let T=T, C=C, x=x
                        @test FixedPointDecimals.fldmod_by_const(x, C) == fldmod(x, C)
                    end
                end
            end
        end
    end
end

# We don't actually use fldmod_by_const with 8-bit ints, but they're useful because
# we can exhaustively test every possible combination, to increase our confidence in
# the implementation.
@testset "fldmod_by_const - exhaustive 8-bit" begin
    for T in (Int8, UInt8)
        for x in typemin(T) : typemax(T)
            for y in typemin(T) : typemax(T)
                y == 0 && continue
                y == -1 && x == typemin(T) && continue
                @testset let x=x, y=y, T=T
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
