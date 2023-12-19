# FixedPointDecimals

[![CI](https://github.com/JuliaMath/FixedPointDecimals.jl/workflows/CI/badge.svg)](https://github.com/JuliaMath/FixedPointDecimals.jl/actions?query=workflow%3ACI)
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

### Arithmetic details: Overflow and checked math

_NOTE: This section applies to FixedPointDecimals v0.5+._

By default, all arithmetic operations on FixedDecimals, except division, **will silently overflow** following the standard behavior for bit integer types in Julia. For example:
```julia
julia> FixedDecimal{Int8,2}(1.0) + FixedDecimal{Int8,2}(1.0)
FixedDecimal{Int8,2}(-0.56)

julia> -FixedDecimal{Int8,2}(-1.28)  # negative typemin wraps to typemin again
FixedDecimal{Int8,2}(-1.28)

julia> abs(FixedDecimal{Int8,2}(-1.28))  # negative typemin wraps to typemin again
FixedDecimal{Int8,2}(-1.28)
```

*Note that **division** on FixedDecimals will throw OverflowErrors on overflow, and will not wrap. This decision may be reevaluated in a future breaking version change release of FixedDecimals. Please keep this in mind.*

In most applications dealing with `FixedDecimals`, you will likely want to use the **checked arithmetic** operations instead. These operations will _throw an OverflowError_ on overflow or underflow, rather than silently wrapping. For example:
```julia
julia> Base.checked_mul(FixedDecimal{Int8,2}(1.2), FixedDecimal{Int8,2}(1.2))
ERROR: OverflowError: 1.20 * 1.20 overflowed for type FixedDecimal{Int8, 2}

julia> Base.checked_add(FixedDecimal{Int8,2}(1.2), 1)
ERROR: OverflowError: 1.20 + 1.00 overflowed for type FixedDecimal{Int8, 2}

julia> Base.checked_div(Int8(1), FixedDecimal{Int8,2}(0.5))
ERROR: OverflowError: 1.00 ÷ 0.50 overflowed for type FixedDecimal{Int8, 2}
```

**Checked division:** Note that `checked_div` performs truncating, integer division. Julia Base does not provide a function to perform checked *decimal* division (`/`), so we provide one in this package, `FixedPointDecimals.checked_rdiv`. However, as noted above, the default division arithmetic operators will throw on overflow anyway.

Here are all the checked arithmetic operations supported by `FixedDecimal`s:
- `Base.checked_add(x,y)`
- `Base.checked_sub(x,y)`
- `Base.checked_mul(x,y)`
- `Base.checked_div(x,y)`
- `FixedPointDecimals.checked_rdiv(x,y)`
- `Base.checked_cld(x,y)`
- `Base.checked_fld(x,y)`
- `Base.checked_rem(x,y)`
- `Base.checked_mod(x,y)`
- `Base.checked_neg(x)`
- `Base.checked_abs(x)`

### Conversions, Promotions, and Inexact Errors.

Note that arithmetic operations will _promote_ all arguments to the same FixedDecimal type
before performing the operation. If you are promoting a non-FixedDecimal _number_ to a FixedDecimal, there is always a chance that the Number will not fit in the FD type. In that case, the conversion will throw an exception. Here are some examples:
```julia
julia> FixedDecimal{Int8,2}(2)  # 200 doesn't fit in Int8
ERROR: InexactError: convert(FixedDecimal{Int8, 2}, 2)

julia> FixedDecimal{Int8,2}(1) + 2  # Same here: 2 is promoted to FD{Int8,2}(2)
ERROR: InexactError: convert(FixedDecimal{Int8, 2}, 2)

julia> FixedDecimal{Int8,2}(1) + FixedDecimal{Int8,1}(2)  # Promote to the higher-precision type again throws.
ERROR: InexactError: convert(FixedDecimal{Int8, 2}, 2.0)
```

