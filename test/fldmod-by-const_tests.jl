using Test
using FixedPointDecimals

@testset "calculate_inverse_coeff signed" begin
    using FixedPointDecimals: calculate_inverse_coeff

    # The correct magic number here comes from investigating the following native code
    # produced on an m2 aarch64 macbook pro:
    # @code_native ((x)->fld(x,100))(2)
    #   ...
    #         mov     x8, #55051
    #         movk    x8, #28835, lsl #16
    #         movk    x8, #2621, lsl #32
    #         movk    x8, #41943, lsl #48
    # Where:
    # julia> 55051 | 28835 << 16 | 2621 << 32 | 41943 << 48
    # -6640827866535438581
    @test calculate_inverse_coeff(Int64, 100) == (-6640827866535438581, 6)

    # Same for the tests below:

    # (LLVM's magic number is shifted one bit less, then they shift by 2, instead of 3,
    #  but the result is the same.)
    @test calculate_inverse_coeff(Int64, 10) == (7378697629483820647 << 1, 3)

    @test calculate_inverse_coeff(Int64, 1) == (1, 0)
end

@testset "calculate_inverse_coeff signed 4" begin
    using FixedPointDecimals: calculate_inverse_coeff

    # Same here, our magic number is shifted 2 bits more than LLVM's
    @test calculate_inverse_coeff(UInt64, 100) == (0xa3d70a3d70a3d70b, 6)

    @test calculate_inverse_coeff(UInt64, 1) == (UInt64(0x1), 0)
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
