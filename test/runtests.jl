using FixedPointDecimals
using FixedPointDecimals: FD, value
using Test
using Printf
using Base.Checked: checked_mul
using Parsers

pkg_path = pkgdir(FixedPointDecimals)
include(joinpath(pkg_path, "test", "utils.jl"))

@testset "FixedPointDecimals" begin
    include("FixedDecimal.jl")
    isdefined(Base, Symbol("@assume_effects")) && include("fldmod-by-const_tests.jl")
end

