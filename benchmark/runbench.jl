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
                                        round(memory(a)/b), round(allocs(a)/b))
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
function postprocess_no_div(results::BenchmarkGroup)
    for (op, op_group) in results.data
        op_results = op_group.data
        for (type, type_group) in op_results
            benchresults = type_group.data
            if op == "identity"
                # For :identity, bench and base are identical so we don't want to subtract.
                op_results[type] = median(benchresults["bench"])
            else
                op_results[type] = median(benchresults["bench"]) - median(benchresults["base"])
            end
        end
    end
    results
end


function runbench()
    rm(joinpath(@__DIR__, "tune.json"))  # Remove the existing tune.json file.
    bench_results = withenv("BENCH_NUM_ITERS"=>string(N)) do
        benchmarkpkg("FixedPointDecimals"; postprocess=postprocess)
    end

    export_markdown(joinpath(@__DIR__, "results.md"), bench_results)
    return bench_results
end

function judgebench(target::Union{String, BenchmarkConfig}, baseline::Union{String, BenchmarkConfig},
            postprocess_fn=postprocess_no_div)
    try rm(joinpath(@__DIR__, "tune.json")) catch end  # Remove the existing tune.json file.
    bench_results = withenv("BENCH_NUM_ITERS"=>string(N)) do
        if postprocess_fn != nothing
            judge("FixedPointDecimals", target, baseline; f=identity, postprocess=postprocess_fn)
        else
            judge("FixedPointDecimals", target, baseline)
        end
    end
    export_markdown(joinpath(@__DIR__, "judge.md"), bench_results)
    return bench_results
end
function judgebench(baseline::Union{String, BenchmarkConfig})
    judgebench(BenchmarkConfig(), baseline)
end

end
