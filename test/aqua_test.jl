@testset "Aqua" begin
    using Aqua
    Aqua.test_all(FixedPointDecimals; stale_deps=false, ambiguities=false)
    Aqua.test_stale_deps(FixedPointDecimals; ignore=[:Aqua])
    Aqua.test_ambiguities(FixedPointDecimals)
end
