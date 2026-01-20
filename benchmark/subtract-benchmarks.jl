import PkgBenchmark
function PkgBenchmark._run(b::PkgBenchmark.BenchmarkTools.BenchmarkDiff, p::PkgBenchmark.BenchmarkTools.Parameters = b.params;
    prog = nothing, verbose::Bool = false, pad = "", hierarchy = [], kwargs...)
    res = BenchmarkTools.run_result(b, p; kwargs...)[1]
    if prog != nothing
        indent = 0
        ProgressMeter.next!(prog; showvalues = [map(id -> ("  "^(indent += 1) * "[$(id[2])/$(id[3])]", id[1]), hierarchy)...])
    end
    return res
end
