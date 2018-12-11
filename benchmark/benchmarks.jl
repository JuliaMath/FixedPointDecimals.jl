# Decimal Representation Comparisons
#
# This benchmark compares the performance of several numeric representations, over various
# numeric operations (+,-,*,/,÷...) on large arrays of numbers, in order to guide
# decision-making about how to represent fixed-decimal numbers.
#
# It compares fixed-decimal types against the builtin Int and Float types of various sizes.
# The output is written to a .csv file in the same directory as this file.

using FixedPointDecimals
using Random
using BenchmarkTools, Statistics

# # TODO: remove this file once BenchmarkTools has a built-in solution for diffing two
# # @benchmarkable runs
# include("subtract-benchmarks.jl")

# Define a parent BenchmarkGroup to contain our suite
const SUITE = BenchmarkGroup()

decimal_precision = 2

# Express that data through the various types. Round it for integers.
fd_FixedPointDecimal_types = [
    FixedPointDecimals.FixedDecimal{Int32, decimal_precision},
    FixedPointDecimals.FixedDecimal{Int64, decimal_precision},
    FixedPointDecimals.FixedDecimal{Int128, decimal_precision},
]
inttypes = [Int32,Int64,Int128]
floattypes = [Float32,Float64]
bigtypes = [BigInt, BigFloat]

alltypes = (inttypes..., bigtypes..., floattypes..., fd_FixedPointDecimal_types...,)

identity1(a,_) = a
allops = (*, /, +, ÷, identity1)

# Category for the results output CSV
category(::Type{<:Union{inttypes...}}) = "Int"
category(::Type{<:Union{floattypes...}}) = "Float"
category(::Type{<:Union{bigtypes...}}) = "Big"
category(::Type{<:FixedPointDecimals.FixedDecimal}) = "FixedDecimal"
type(T::Type) = "$T"
type(T::Type{<:Union{Int32, Int64}}) = "  $T"
type(T::Type{Int128}) = " $T"
type(::Type{FixedPointDecimals.FixedDecimal{T,f}}) where {T,f} = "FD{$T,$f}"
type(::Type{FixedPointDecimals.FixedDecimal{T,f}}) where {T<:Union{Int32,Int64},f} = "FD{ $T,$f}"
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
    for T in alltypes
        SUITE[opname(op)][type(T)] = BenchmarkGroup(["diff"])
    end
end

for op in allops
    println()
    println("$op")
    for T in alltypes
        print("$T ")

        N = 1 # _000 #_000
        initial_value = zero(T)
        a = one(T)

        # For some reason this is necessary to eliminate mysterious "1 allocation"
        fbase = @eval (out::Ref{$T})->baseline($T, $a, $N, out)
        fbench = @eval (out::Ref{$T})->benchmark($T, $op, $a, $N, out)

        # Run the benchmark
        outbase = Ref(initial_value)
        bbase = @benchmarkable $fbase($outbase) evals=1 setup=($outbase[]=$initial_value)
        outbench = Ref(initial_value)
        bbench = @benchmarkable $fbench($outbench) evals=1 setup=($outbench[]=$initial_value)
        bdiff = bbench - bbase
        SUITE[opname(op)][type(T)]["diff"] = bdiff
    end
end
