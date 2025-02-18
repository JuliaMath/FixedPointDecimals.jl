using Test, FixedPointDecimals
@testset "Aqua" begin
    using Aqua
    Aqua.test_all(FixedPointDecimals)
end
