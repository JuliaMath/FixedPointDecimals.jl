@testset "Aqua" begin
    using Aqua
    # For some reason, running the ambiguities test as part of `test_all`
    # complains about something in BitIntegers.  But running it separately is
    # OK.
    Aqua.test_all(FixedPointDecimals; ambiguities=false)
    Aqua.test_ambiguities(FixedPointDecimals)
end
