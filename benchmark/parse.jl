# Benchmarks for `parse`/`tryparse` of FixedDecimals.
#
# Usage (from the package root):
#     julia --project=benchmark -e 'using Pkg; Pkg.develop(path="."); Pkg.instantiate()'
#     julia --project=benchmark benchmark/parse.jl
using BenchmarkTools
using FixedPointDecimals
using FixedPointDecimals: RoundThrows
using Printf
using Random

const SUITE = BenchmarkGroup()

const CASES = [
    # (name, type, input, rounding mode)
    ("FD{Int64,2} short",          FixedDecimal{Int64,2},    "1.23",                    RoundNearest),
    ("FD{Int64,2} negative",       FixedDecimal{Int64,2},    "-123.45",                 RoundNearest),
    ("FD{Int64,2} zero",           FixedDecimal{Int64,2},    "0",                       RoundNearest),
    ("FD{Int64,2} integer",        FixedDecimal{Int64,2},    "123456789",               RoundNearest),
    ("FD{Int64,8} long",           FixedDecimal{Int64,8},    "92233720368.54775807",    RoundNearest),
    ("FD{Int32,4} short",          FixedDecimal{Int32,4},    "1234.5678",               RoundNearest),
    ("FD{UInt64,2} short",         FixedDecimal{UInt64,2},   "1.23",                    RoundNearest),
    ("FD{Int128,18} near typemax", FixedDecimal{Int128,18},  "170141183460469231731.687303715884105727", RoundNearest),
    ("FD{Int128,2} 30 digits",     FixedDecimal{Int128,2},   "1234567890123456789012345678.90", RoundNearest),
    ("FD{Int64,4} 60 digits",      FixedDecimal{Int64,4},    "9"^60 * "e-50",          RoundNearest),
    ("FD{Int64,4} exp neg",        FixedDecimal{Int64,4},    "1.2e-3",                  RoundNearest),
    ("FD{Int64,4} exp pos",        FixedDecimal{Int64,4},    "12e3",                    RoundNearest),
    ("FD{Int64,2} round nearest",  FixedDecimal{Int64,2},    "0.455",                   RoundNearest),
    ("FD{Int64,2} round to zero",  FixedDecimal{Int64,2},    "0.455",                   RoundToZero),
    ("FD{Int64,2} round throws",   FixedDecimal{Int64,2},    "0.450",                   RoundThrows),
    ("FD{Int64,2} whitespace",     FixedDecimal{Int64,2},    "  1.23  ",                RoundNearest),
]

const INVALID_CASES = [
    # (name, type, input, rounding mode); benchmarked with `tryparse` only
    ("invalid foo",      FixedDecimal{Int64,4}, "foo",   RoundNearest),
    ("invalid 1.2.3",    FixedDecimal{Int64,4}, "1.2.3", RoundNearest),
    ("overflow 1e100",   FixedDecimal{Int64,4}, "1e100", RoundNearest),
    ("inexact (throws)", FixedDecimal{Int64,2}, "0.455", RoundThrows),
]

SUITE["parse"] = BenchmarkGroup()
SUITE["tryparse"] = BenchmarkGroup()
# N.B. Interpolating a type into `@benchmarkable` makes it a `DataType` variable, so the
# benchmark would be dominated by dynamic dispatch. `Base.Fix1` stores it as `Type{T}` instead.
for (name, T, s, mode) in CASES
    p, tp = Base.Fix1(parse, T), Base.Fix1(tryparse, T)
    SUITE["parse"][name] = @benchmarkable $p($s, $mode)
    SUITE["tryparse"][name] = @benchmarkable $tp($s, $mode)
end
for (name, T, s, mode) in INVALID_CASES
    tp = Base.Fix1(tryparse, T)
    SUITE["tryparse"][name] = @benchmarkable $tp($s, $mode)
end

function random_decimal_strings(rng, n)
    return [
        string(rand(rng, ("", "-")), rand(rng, 0:99_999), ".", lpad(rand(rng, 0:9_999), rand(rng, 1:4), '0'))
        for _ in 1:n
    ]
end
const STRS = random_decimal_strings(Xoshiro(1234), 100_000)
SUITE["throughput"] = BenchmarkGroup()
SUITE["throughput"]["parse.(FD{Int64,4}, 10^5 strings)"] = @benchmarkable parse.(FixedDecimal{Int64,4}, $STRS)

function report(results)
    println("# FixedPointDecimals parse benchmarks")
    println("# Julia $(VERSION), FixedPointDecimals $(pkgversion(FixedPointDecimals)), $(Sys.cpu_info()[1].model)")
    println("# median time | allocations | memory")
    for group in ("parse", "tryparse", "throughput")
        for (name, trial) in sort(collect(results[group]); by=first)
            m = median(trial)
            @printf("%-9s %-42s %12s %6d allocs %10s\n", group, name,
                BenchmarkTools.prettytime(time(m)), allocs(m), BenchmarkTools.prettymemory(memory(m)))
        end
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    tune!(SUITE)
    report(run(SUITE))
end
