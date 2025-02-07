# Decimal Representation Comparisons
#
# This benchmark compares the performance of several numeric representations, over various
# numeric operations (+,-,*,/,÷...) on large arrays of numbers, in order to guide
# decision-making about how to represent fixed-decimal numbers.
#
# It compares fixed-decimal types against the builtin Int and Float types of various sizes.
# The output is written to a .csv file in the same directory as this file.

# TODO: remove this file once BenchmarkTools has a built-in solution for diffing two
# @benchmarkable runs
using Pkg
Pkg.activate(@__DIR__)

using FixedPointDecimals
using Random
using BenchmarkTools, Statistics

# Define a parent BenchmarkGroup to contain our suite
const SUITE = BenchmarkGroup()
const N = parse(Int, get(ENV, "BENCH_NUM_ITERS", "1000"))

benchtypes = [
    FixedPointDecimals.FixedDecimal{Int32,  2},
    FixedPointDecimals.FixedDecimal{Int64,  2},
    FixedPointDecimals.FixedDecimal{Int128, 2},
]

identity1(a,_) = a
allops = (*, /, +, ÷, identity1)

prettytype(::Type{FixedPointDecimals.FixedDecimal{T,f}}) where {T,f} = "FD{$T,$f}"
prettytype(::Type{FixedPointDecimals.FixedDecimal{T,f}}) where {T<:Union{Int32,Int64},f} = "FD{ $T,$f}"
opname(f) = string(Symbol(f))
opname(f::typeof(identity1)) = "identity"

# --------- Define benchmark functions -------------
# Some care is taken here to prevent the compiler from optimizing away the operations:
#  - Marked @noinline so the constants we pass in aren't available to the optimizer.
#  - We take `a` and `out` as parameters so that their values aren't available when
#      compiling this function.
#  - `out` is a Ref{T} so that this function will have side effects. We use an output
#      parameter instead of returning the value directly  so that it will play nicely with
#      the `@benchmark` macro which returns the benchmark results as an object.
#  - `T` and `op` _should_ be available as compile-time constants, since we don't want to be
#      measuring the time it takes to read from global variables.
@noinline function benchmark(::Type{T}, op, a::T, n, out::Ref{T}) where {T}
    for _ in 1:n
        tmp = op(a,a)
        out[] += tmp
        a += one(T)
    end
end

@noinline function baseline(::Type{T}, a::T, n, out::Ref{T}) where {T}
    for _ in 1:n
        tmp = a
        out[] += tmp
        a += one(T)
    end
end

# Define the benchmark structure
for op in allops
    SUITE[opname(op)] = BenchmarkGroup()
    for T in benchtypes
        SUITE[opname(op)][prettytype(T)] = BenchmarkGroup(["base", "bench"])
    end
end

for op in allops
    println()
    println("$op")
    for T in benchtypes
        print("$T ")

        initial_value = zero(T)
        a = one(T)

        # For some reason this is necessary to eliminate mysterious "1 allocation"
        fbase = @eval (out::Ref{$T})->baseline($T, $a, $N, out)
        fbench = @eval (out::Ref{$T})->benchmark($T, $op, $a, $N, out)

        # Run the benchmark
        outbase = Ref(initial_value)
        SUITE[opname(op)][prettytype(T)]["base"] = @benchmarkable $fbase($outbase) evals=1 setup=($outbase[]=$initial_value)
        outbench = Ref(initial_value)
        SUITE[opname(op)][prettytype(T)]["bench"] = @benchmarkable $fbench($outbench) evals=1 setup=($outbench[]=$initial_value)
    end
end
