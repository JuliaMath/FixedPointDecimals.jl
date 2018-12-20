module FixedPointDecimals_RunBench

using Pkg

Pkg.activate(@__DIR__)
using PkgBenchmark, BenchmarkTools, Statistics

const N = 1_000

import Base: -, /
function -(a::BenchmarkTools.TrialEstimate, b::BenchmarkTools.TrialEstimate)
    ttol = max(params(a).time_tolerance, params(b).time_tolerance)
    mtol = max(params(a).memory_tolerance, params(b).memory_tolerance)
    p = BenchmarkTools.Parameters(params(a); time_tolerance = ttol, memory_tolerance = mtol)
    return BenchmarkTools.TrialEstimate(p, -(time(a), time(b)), -(gctime(a), gctime(b)),
                                        -(memory(a), memory(b)), -(allocs(a), allocs(b)))
end
function /(a::BenchmarkTools.TrialEstimate, b::Int)
    ttol = params(a).time_tolerance / b
    mtol = params(a).memory_tolerance / b
    p = BenchmarkTools.Parameters(params(a); time_tolerance = ttol, memory_tolerance = mtol)
    return BenchmarkTools.TrialEstimate(p, time(a)/b, gctime(a)/b,
                                        memory(a)/b, allocs(a)/b)
end

function postprocess(results::BenchmarkGroup)
    for (op, op_group) in results.data
        op_results = op_group.data
        for (type, type_group) in op_results
            benchresults = type_group.data
            if op == "identity"
                # For :identity, bench and base are identical so we don't want to subtract.
                op_results[type] = median(benchresults["bench"]) / N
            else
                op_results[type] = median(benchresults["bench"])/N - median(benchresults["base"])/N
            end
        end
    end
    results
end

bench_results = withenv("BENCH_NUM_ITERS"=>string(N)) do
    benchmarkpkg("FixedPointDecimals"; postprocess=postprocess)
end

export_markdown(joinpath(@__DIR__, "results.md"), bench_results)

end
