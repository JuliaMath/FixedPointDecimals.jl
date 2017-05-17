# FixedPointDecimals

[![Travis Build Status](https://travis-ci.org/JuliaMath/FixedPointDecimals.jl.svg?branch=master)](https://travis-ci.org/JuliaMath/FixedPointDecimals.jl)
[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/edir9h23fs98jfjc/branch/master?svg=true)](https://ci.appveyor.com/project/omus/fixedpointdecimals-jl)
[![coveralls](https://coveralls.io/repos/github/JuliaMath/FixedPointDecimals.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/JuliaMath/FixedPointDecimals.jl?branch=master)
[![codecov.io](https://codecov.io/github/JuliaMath/FixedPointDecimals.jl/coverage.svg?branch=master)](https://codecov.io/github/JuliaMath/FixedPointDecimals.jl?branch=master)

Provides the fixed-point decimal type `FixedDecimal` allowing for exact representations of
decimal numbers. These numbers are useful in financial calculations where interactions
between decimal numbers are required to be exact.

This library defines the type `FixedDecimal{T <: Integer, f}` as a subtype of `Real`. The
parameter `T` is the underlying machine representation and `f` is the number of decimal
places which can be stored.

For example, `FixedDecimal{Int8, 2}` allows you to a decimal number with up to 2 fractional
digits. All `FixedDecimal{Int8, 2}` numbers `x` must satisfy

```
-1.28 = -128/10² ≤ x ≤ 127/10² = 1.27
```

because the range of `Int8` is from -128 to 127.

In general `FixedDecimal{T <: Integer, f}` numbers `y` must satisfy:

```
typemin(T)/10ᶠ ≤ y ≤ typemax(T)/10ᶠ
```

## Usage

```julia
julia> using FixedPointDecimals

julia> 2.2 / 10
0.22000000000000003

julia> FixedDecimal{Int,2}(2.2) / 10
FixedDecimal{Int64,2}(0.22)

julia> 0.1 + 0.2
0.30000000000000004

julia> FixedDecimal{Int,1}(0.1) + FixedDecimal{Int,1}(0.2)
FixedDecimal{Int64,1}(0.3)
```
