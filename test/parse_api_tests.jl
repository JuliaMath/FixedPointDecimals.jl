using Test
using FixedPointDecimals
using FixedPointDecimals: RoundThrows
using Random

# Outcome of `parse` as a string: the backing integer, or the kind of exception thrown.
function parse_outcome(::Type{FD}, s, mode=RoundNearest) where {FD}
    try
        return string(parse(FD, s, mode).i)
    catch e
        e isa InexactError && return "Inexact"
        e isa OverflowError && return "Overflow"
        e isa ArgumentError && return "Argument"
        rethrow()
    end
end

# Compare `parse`/`tryparse` against the reference, logging the first few mismatches.
# Returns the updated mismatch count.
function check_against_reference(::Type{FD}, s, mode, nbad) where {FD}
    got, expected = parse_outcome(FD, s, mode), reference_outcome(FD, s, mode)
    got == expected && tryparse_agrees(FD, s, mode) && return nbad
    nbad < 10 && @error "mismatch" FD s mode got expected
    return nbad + 1
end

# `tryparse` must return `nothing` exactly when `parse` throws, and agree with it otherwise.
function tryparse_agrees(::Type{FD}, s, mode=RoundNearest) where {FD}
    res = tryparse(FD, s, mode)
    out = parse_outcome(FD, s, mode)
    return isnothing(res) ? out in ("Inexact", "Overflow", "Argument") : out == string(res.i)
end

# An exact reference implementation of `parse` using `Rational{BigInt}` arithmetic, returning
# the same kind of outcome as `parse_outcome`. Only practical for moderate exponents.
const VALID_SYNTAX = r"^[ \t]*([+-]?)([0-9]*)(?:\.([0-9]*))?(?:[eE]([+-]?[0-9]+))?[ \t]*\z"
function reference_outcome(::Type{FixedDecimal{T,f}}, s, mode=RoundNearest) where {T,f}
    m = match(VALID_SYNTAX, s)
    m === nothing && return "Argument"
    sign, int, frac, exp = m.captures
    frac = something(frac, "")
    isempty(int) && isempty(frac) && return "Argument"
    neg = sign == "-"
    # Clamping the exponent doesn't change the outcome, as long as it's large compared to the digits
    elim = length(s) + 100
    e = Int(clamp(exp === nothing ? 0 : parse(BigInt, exp), -elim, elim)) - length(frac) + f
    mag = parse(BigInt, int * frac * "0")  # trailing "0" so that empty digit strings parse
    scaled = e >= 0 ? (mag * big(10)^e // 10) : (mag // (10 * big(10)^-e))
    # The order of these checks mirrors `FixedPointDecimals._scale`
    iszero(scaled) && return "0"
    if scaled < 1//10
        return mode === RoundThrows ? "Inexact" : "0"
    end
    neg && T <: Unsigned && return "Argument"
    scaled >= big(10)^ndigits(typemax(T)) && return "Overflow"
    r = mode === RoundToZero ? trunc(BigInt, scaled) : round(BigInt, scaled, RoundNearest)
    mode === RoundThrows && r != scaled && return "Inexact"
    r = neg ? -r : r
    typemin(T) <= r <= typemax(T) || return "Overflow"
    return string(r)
end

@testset "edge cases" begin
    FD2 = FixedDecimal{Int64,2}
    cases = [
        # whitespace: ASCII spaces and tabs around the number are allowed, nothing else
        " 1.5" => "150", "1.5 " => "150", "\t1.5" => "150", "1.5\t" => "150", "  1.5  " => "150",
        " " => "Argument", "\t" => "Argument", "1 .5" => "Argument", "1. 5" => "Argument",
        "\n1.5" => "Argument", "1.5\n" => "Argument", "\r1.5" => "Argument", "1.5\r" => "Argument",
        "- 1" => "Argument", "1 e2" => "Argument", "1e 2" => "Argument",
        # signs
        "+" => "Argument", "-" => "Argument", "+.5" => "50", "-.5" => "-50", "--1" => "Argument",
        "+-1" => "Argument", "-+1" => "Argument", "1-" => "Argument", "1+" => "Argument",
        # decimal marks
        "." => "Argument", ".." => "Argument", ".-" => "Argument", ".1." => "Argument",
        "1.." => "Argument", "0." => "0", "-0." => "0", "00" => "0", ".5" => "50", "5." => "500",
        # exponents
        "1E5" => "10000000", "1e+2" => "10000", "1.e2" => "10000", ".5e1" => "500", "1e01" => "1000",
        "1e-01" => "10", "1e0000000000000000000000000000000002" => "10000", "1e" => "Argument",
        "1e+" => "Argument", "1e-" => "Argument", "-.e" => "Argument", "e1" => "Argument",
        "E1" => "Argument", "1e+-1" => "Argument", "1e1.0" => "Argument", "1ee1" => "Argument",
        "1e2e3" => "Argument",
        # zeros and huge exponents
        "-0" => "0", "0e-99999" => "0", "1e-99999" => "0", "1e-9223372036854775808" => "0",
        "1e-99999999999999999999999" => "0", "1e18" => "Overflow", "1e300" => "Overflow",
        "0e300" => "0", "0.0e300" => "0",
        # leading and trailing zeros
        "0"^45 * "1.5" => "150", "0." * "0"^45 * "1" => "0", "1" * "0"^400 * "e-400" => "100",
        "1." * "0"^400 => "100", "0"^400 => "0",
        # junk
        "1,000.5" => "Argument", "1_000" => "Argument", "\0" => "Argument", "1.5\0" => "Argument",
        "\0" * "1.5" => "Argument", "é" => "Argument", "1.5é" => "Argument", "NaN" => "Argument",
        "Inf" => "Argument", "-Inf" => "Argument", "0x10" => "Argument", "0b1" => "Argument",
        "\"1.5\"" => "Argument", "1.5;" => "Argument", "1.5," => "Argument", "1.2.3" => "Argument",
        "foo" => "Argument", "1.5foo" => "Argument", "" => "Argument",
    ]
    @testset "$(repr(s))" for (s, expected) in cases
        @test parse_outcome(FD2, s) == expected
        @test tryparse_agrees(FD2, s)
        if length(s) < 100 && !occursin(r"[eE][+-]?[0-9]{6}", s)
            @test reference_outcome(FD2, s) == expected
        end
    end

    # Behaviour that deliberately differs from v0.6.5 (which parsed through Parsers.jl v2)
    @testset "changes from v0.6.5" begin
        # A number needs at least one digit (v0.6.5 accepted these as 0, but rejected ".")
        for s in ("-.", "+.", ".e0", "-.e1", "+.E5", " -. ")
            @test parse_outcome(FD2, s) == "Argument"
        end
        # No limit on the exponent (v0.6.5 rejected exponents past Float64's range)
        @test parse_outcome(FD2, "1e400") == "Overflow"
        @test parse_outcome(FD2, "1e9223372036854775807") == "Overflow"
        @test parse_outcome(FD2, "1e99999999999999999999") == "Overflow"
        @test parse_outcome(FD2, "0e400") == "0"
        @test parse_outcome(FD2, "0.0e99999999999999999999") == "0"
        @test parse_outcome(FD2, "1" * "0"^400 * "e-400") == "100"
        # Malformed input is always an `ArgumentError`, even when a prefix of it would round or overflow
        @test parse_outcome(FD2, "1.234x", RoundThrows) == "Argument"
        @test parse_outcome(FD2, "1e100x") == "Argument"
        @test parse_outcome(FixedDecimal{Int8,1}, "-5571,50") == "Argument"
        # `f`/`F` are not exponent markers (v0.6.5 accepted Float32-literal style "1.5f0", like Parsers.jl v2)
        for s in ("1.5f0", "1.5F0", "1f5", "1.5f-1")
            @test parse_outcome(FD2, s) == "Argument"
        end
        # Overflow when scaling by a power of 10 is detected (v0.6.5 silently wrapped around)
        @test parse_outcome(FixedDecimal{UInt8,0}, "9e2") == "Overflow"
        @test parse_outcome(FixedDecimal{UInt8,0}, "90e1") == "Overflow"
        @test parse_outcome(FixedDecimal{UInt8,0}, "900") == "Overflow"
        @test parse_outcome(FixedDecimal{UInt8,2}, "6") == "Overflow"
        @test parse_outcome(FixedDecimal{UInt8,2}, "2.5") == "250"
        @test parse_outcome(FixedDecimal{UInt8,2}, "2.6") == "Overflow"
        @test parse_outcome(FixedDecimal{Int16,0}, "4e4") == "Overflow"
        @test parse_outcome(FixedDecimal{Int16,0}, "-4e4") == "Overflow"
        @test parse_outcome(FixedDecimal{Int16,0}, "3e4") == "30000"
        @test parse_outcome(FixedDecimal{Int16,0}, "-3e4") == "-30000"
        @test parse_outcome(FixedDecimal{Int32,6}, "+5509.") == "Overflow"
        @test parse_outcome(FixedDecimal{UInt64,19}, "9.0") == "Overflow"
        @test parse_outcome(FixedDecimal{UInt64,19}, "1.8") == "18000000000000000000"
        @test parse_outcome(FixedDecimal{Int128,38}, "8.5055") == "Overflow"
        @test parse_outcome(FixedDecimal{UInt128,38}, "7") == "Overflow"
        @test parse_outcome(FixedDecimal{UInt128,38}, "3") == "300000000000000000000000000000000000000"
        @test parse_outcome(FixedDecimal{UInt128,38}, "4") == "Overflow"
    end

    @testset "rounding of tiny values" begin
        @test parse_outcome(FD2, "1e-99999", RoundToZero) == "0"
        @test parse_outcome(FD2, "1e-99999", RoundThrows) == "Inexact"
        @test parse_outcome(FD2, "0e-99999", RoundThrows) == "0"
        @test parse_outcome(FD2, "0.005") == "0"   # ties to even
        @test parse_outcome(FD2, "0.015") == "2"   # ties to even
        @test parse_outcome(FD2, "0.0050000000000000000000000001") == "1"
        @test parse_outcome(FD2, "0.0049999999999999999999999999") == "0"
        @test parse_outcome(FD2, "-0.0050000000000000000000000001") == "-1"
    end

    @testset "unsigned" begin
        U1 = FixedDecimal{UInt8,1}
        @test parse_outcome(U1, "-0") == "0"
        @test parse_outcome(U1, "-0.0") == "0"
        @test parse_outcome(U1, "-0e5") == "0"
        @test parse_outcome(U1, "-1") == "Argument"
        @test parse_outcome(U1, "-0.01") == "Argument"
        @test parse_outcome(U1, "-0.01", RoundToZero) == "Argument"
        @test parse_outcome(U1, "-0.05") == "Argument"
        @test parse_outcome(U1, "25.5") == "255"
        @test parse_outcome(U1, "25.6") == "Overflow"
        @test parse_outcome(U1, "25.54") == "255"
        @test parse_outcome(U1, "25.55") == "Overflow"
    end

    @testset "boundaries" begin
        @test parse_outcome(FixedDecimal{Int8,2}, "1.275") == "Overflow"
        @test parse_outcome(FixedDecimal{Int8,2}, "1.2749999999999999999999999999999999999999999") == "127"
        @test parse_outcome(FixedDecimal{Int8,2}, "-1.285") == "-128"
        @test parse_outcome(FixedDecimal{Int8,2}, "-1.2851") == "Overflow"
        @test parse_outcome(FixedDecimal{Int64,0}, "9223372036854775807") == "9223372036854775807"
        @test parse_outcome(FixedDecimal{Int64,0}, "9223372036854775808") == "Overflow"
        @test parse_outcome(FixedDecimal{Int64,0}, "-9223372036854775808") == "-9223372036854775808"
        @test parse_outcome(FixedDecimal{Int64,0}, "-9223372036854775809") == "Overflow"
        @test parse_outcome(FixedDecimal{UInt64,0}, "18446744073709551615") == "18446744073709551615"
        @test parse_outcome(FixedDecimal{UInt64,0}, "18446744073709551616") == "Overflow"
        @test parse_outcome(FixedDecimal{UInt128,0}, string(typemax(UInt128))) == string(typemax(UInt128))
        @test parse_outcome(FixedDecimal{UInt128,0}, string(big(typemax(UInt128)) + 1)) == "Overflow"
        @test parse_outcome(FixedDecimal{Int128,0}, string(typemin(Int128))) == string(typemin(Int128))
        @test parse_outcome(FixedDecimal{Int128,0}, string(big(typemin(Int128)) - 1)) == "Overflow"
        @test parse_outcome(FixedDecimal{Int128,38}, "1.70141183460469231731687303715884105727") == string(typemax(Int128))
        @test parse_outcome(FixedDecimal{Int128,38}, "1.70141183460469231731687303715884105728") == "Overflow"
        @test parse_outcome(FixedDecimal{Int128,38}, "-1.70141183460469231731687303715884105728") == string(typemin(Int128))
        @test parse_outcome(FixedDecimal{Int64,2}, "9"^20) == "Overflow"
        # Digits past the 39th must still take part in rounding
        @test parse_outcome(FixedDecimal{Int64,0}, "0." * "4"^40 * "9") == "0"
        @test parse_outcome(FixedDecimal{Int64,0}, "0.5" * "0"^60 * "1") == "1"
        @test parse_outcome(FixedDecimal{Int64,0}, "0.5" * "0"^60) == "0"
        @test parse_outcome(FixedDecimal{Int64,4}, "9"^60 * "e-50") == "100000000000000"
    end

    # Inputs around the widths of the UInt64/UInt128/UInt256 digit accumulators
    @testset "accumulator widths vs. reference" begin
        digit_strings = String[]
        for base in (string(typemax(UInt64)), string(typemax(UInt128)))
            n = length(base)
            for s in (base, string(parse(BigInt, base) + 1), string(parse(BigInt, base) - 1), "9"^(n - 1), "9"^n, "1" * "0"^(n - 1),
                      "5" * "0"^(n - 1), "5" * "0"^(n - 2) * "1", "4" * "9"^(n - 1), "1" * "0"^(n - 2) * "1")
                push!(digit_strings, s)
            end
        end
        inputs = String[]
        for d in digit_strings, k in (0, 1, 5, 18, 19, 20, 21, 37, 38, 39, 40)
            k <= length(d) && push!(inputs, k == 0 ? d : string(d[1:end-k], '.', d[end-k+1:end]))
            push!(inputs, "0." * d, "-0." * d, d * "e-$k", "-" * d * "e-$k", d * "0"^k)
        end
        types = (Int8, UInt8, Int64, UInt64, Int128, UInt128)
        nbad = 0
        for s in inputs, T in types, f in unique((0, 1, 2, ndigits(typemax(T)) - 1)), mode in (RoundNearest, RoundToZero, RoundThrows)
            FD = FixedDecimal{T,f}
            nbad = check_against_reference(FD, s, mode, nbad)
        end
        @test nbad == 0
    end

    # Long inputs exercise dropping the digits past `_maxintdigits(T) + 1` (keeping a sticky digit)
    @testset "long inputs vs. reference (fuzz)" begin
        rng = Random.Xoshiro(20261015)
        types = (Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128)
        function randmantissa(rng)
            # A tie-ish pattern: some leading digits, then "5" or "4999..", then many zeros or nines,
            # then maybe a nonzero digit far past the rounding position
            lead = join(rand(rng, '0':'9', rand(rng, 0:45)))
            mid = rand(rng, ("5", "4", "49", "50", "9", "0", ""))
            fill = rand(rng, ('0', '9'))^rand(rng, 0:50)
            tail = rand(rng, ("", "", "1", "5", "9"))
            digits = lead * mid * fill * tail
            isempty(digits) && (digits = "0")
            k = rand(rng, 0:length(digits))
            m = k == length(digits) ? digits : string(digits[1:k], '.', digits[k+1:end])
            return rand(rng, ("", "-")) * m * (rand(rng, Bool) ? "" : "e$(rand(rng, -60:60))")
        end
        nbad = 0
        for _ in 1:4000
            s = randmantissa(rng)
            T = rand(rng, types)
            f = rand(rng, 0:ndigits(typemax(T))-1)
            mode = rand(rng, (RoundNearest, RoundToZero, RoundThrows))
            FD = FixedDecimal{T,f}
            nbad = check_against_reference(FD, s, mode, nbad)
        end
        @test nbad == 0
    end

    @testset "string types" begin
        @test parse(FD2, SubString("xx1.25yy", 3, 6)) == FD2(1.25)
        @test tryparse(FD2, SubString("xx1.25yy", 3, 6)) == FD2(1.25)
        @test_throws ArgumentError parse(FD2, SubString("xx1.25yy", 3, 7))  # trailing junk
        @test isnothing(tryparse(FD2, SubString("xx1.25yy", 2, 6)))         # leading junk
        @test parse(FD2, Test.GenericString(" -1.25 ")) == FD2(-1.25)
        @test isnothing(tryparse(FD2, Test.GenericString("1.25x")))
    end

    @testset "rounding modes" begin
        @test_throws ArgumentError parse(FD2, "1.5", RoundUp)
        @test_throws ArgumentError tryparse(FD2, "1.5", RoundDown)
        @test_throws ArgumentError parse(FD2, "1.5", RoundNearestTiesAway)
    end
end

# The corpus records the outcomes of v0.6.5. Every line must match the exact reference
# implementation, and the only allowed differences from v0.6.5 are the deliberate changes
# tested in "changes from v0.6.5" above.
@testset "golden corpus" begin
    path = joinpath(@__DIR__, "data", "parse_golden.tsv")
    modes = Dict("N" => RoundNearest, "Z" => RoundToZero, "T" => RoundThrows)
    types = Dict(string(T) => T for T in (Int8, UInt8, Int16, Int32, Int64, UInt64, Int128, UInt128))
    isnumber(x) = occursin(r"^-?[0-9]+$", x)
    nfail = 0
    ncases = 0
    changed = Dict{String,Int}()
    for line in eachline(path)
        startswith(line, '#') && continue
        tstr, fstr, mstr, input, old = split(line, '\t')
        FD = FixedDecimal{types[tstr], parse(Int, fstr)}
        s = unescape_string(input)
        mode = modes[mstr]
        ncases += 1
        got = parse_outcome(FD, s, mode)
        ok = got == reference_outcome(FD, s, mode) && tryparse_agrees(FD, s, mode)
        if got != old
            m = match(VALID_SYNTAX, s)
            valid = m !== nothing && !(isempty(m[2]) && isempty(something(m[3], "")))
            reason = !valid && got == "Argument" ? "malformed input" :
                valid && old == "Argument" && got in ("0", "Overflow") ? "large exponent" :
                valid && isnumber(old) && got == "Overflow" ? "wraparound" : nothing
            if reason === nothing
                ok = false
            else
                changed[reason] = get(changed, reason, 0) + 1
            end
        end
        if !ok
            nfail += 1
            nfail <= 20 && @error "golden mismatch" FD s mode old got reference_outcome(FD, s, mode)
        end
    end
    @info "Golden corpus: $ncases cases, outcomes changed from v0.6.5" changed
    @test ncases > 20_000
    @test nfail == 0
end
