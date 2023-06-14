using Test
import Parsers
using FixedPointDecimals
using FixedPointDecimals: _maxintdigits

# Compat with test contexts introduced in 1.9
if VERSION >= v"1.9"
    var"@testctx" = Test.var"@testset"
else
    macro testctx(ex)
        esc(ex)
    end
end

const DELIM=';';
const GROUP=',';
const DECIM='.';
const OPTIONS_NEAREST = Parsers.Options(groupmark=GROUP, decimal=DECIM, delim=DELIM, quoted=true, rounding=RoundNearest)
const OPTIONS_TOZERO = Parsers.Options(groupmark=GROUP, decimal=DECIM, delim=DELIM, quoted=true, rounding=RoundToZero)
const OPTIONS_THROWS = Parsers.Options(groupmark=GROUP, decimal=DECIM, delim=DELIM, quoted=true, rounding=nothing)
function test_decimal_parser(::Type{T}, f, buf::AbstractString; ok=true, inexact=false, expect=nothing, options = OPTIONS_NEAREST) where {T}
    delim = options.delim.token
    b = get(codeunits(buf), 1, 0xff)
    pl = Parsers.PosLen(1, length(buf))
    conf = FixedPointDecimals.FixedDecimalConf{T}(f)
    (pos, code, _, val) = Parsers.typeparser(conf, codeunits(buf), 1, length(buf), b, Parsers.SUCCESS, pl, options)
    (pos_io, code_io, _, val_io) = Parsers.typeparser(conf, IOBuffer(buf), 1, length(buf), b, Parsers.SUCCESS, pl, options)
    @testctx let _ctx="T=$T, f=$f, input=$(repr(string(buf))), code=$(Parsers.text(code)), len=$(length(buf)), pos=$pos, delim=$(repr(Char(delim))), groupmark=$(repr(options.groupmark)), decimal=$(repr(Char(options.decimal))), mode=$(options.rounding), expect=$(repr(expect))"
        ends_on_delim = !isempty(buf) && UInt8(last(buf)) == UInt8(delim)
        # We parsed the value (un)successfully as expected
        @test Parsers.ok(code) == ok
        @test Parsers.ok(code_io) == ok
        # We marked the value as inexact as expected
        @test Parsers.inexact(code) == inexact
        @test Parsers.inexact(code_io) == inexact
        # We read all valid bytes, i.e. our current `pos` peeks at one past our input, or the delimiter (which is not a part of the number)
        ok && (@test pos == (length(buf) + (UInt8(last(buf)) != UInt8(delim))))
        ok && (@test pos_io == (length(buf) + (UInt8(last(buf)) != UInt8(delim))))
        # We read all valid bytes and which means that we reached EOF (recovery from invalid input is a job for `Parsers.xparse`)
        ok && (@test Parsers.eof(code) == !ends_on_delim)
        ok && (@test Parsers.eof(code_io) == !ends_on_delim)
        isnothing(expect) || (@test val == expect)
        isnothing(expect) || (@test val_io == expect)
        res = Parsers.xparse(FixedDecimal{T,f}, buf, 1, length(buf), options)
        # Even if the input was not valid, we read it entirely and reached EOF or delim
        @test Parsers.eof(res.code) == !ends_on_delim
        @test Parsers.ok(res.code) == ok
        if ok
            @test res.val.i == expect # The integer from our typeparser matches the integer from our FixedDecimal
        else
            @test Parsers.invalid(res.code) # We parsed the value (un)successfully as expected
        end
    end
    return nothing
end

insertchar(str, i, c) = str[1:i-1] * c * str[i:end]

const SignedIntegerTypes = (Int8, Int16, Int32, Int64, Int128)
const UnsignedIntegerTypes = (UInt8, UInt16, UInt32, UInt64, UInt128)
const IntegerTypes = (SignedIntegerTypes..., UnsignedIntegerTypes...)

@testset "_check_overflows" begin
    @testset "$T" for T in SignedIntegerTypes
        @testset "$V" for V in (UInt64, UInt128, BigInt)
            @test !FixedPointDecimals._check_overflows(T, V(0), true)
            @test !FixedPointDecimals._check_overflows(T, V(0), false)

            @test !FixedPointDecimals._check_overflows(T, V(1), true)
            @test !FixedPointDecimals._check_overflows(T, V(1), false)

            if V === BigInt || typemax(T) == typemax(V)
                @test !FixedPointDecimals._check_overflows(T, V(typemax(T) - 1), true)
                @test !FixedPointDecimals._check_overflows(T, V(typemax(T) - 1), false)

                @test !FixedPointDecimals._check_overflows(T, V(typemax(T)), true)
                @test !FixedPointDecimals._check_overflows(T, V(typemax(T)), false)
            end

            if V === BigInt || typemax(T) < typemax(V)
                @test !FixedPointDecimals._check_overflows(T, V(V(typemax(T)) + 1), true)
                @test FixedPointDecimals._check_overflows(T, V(V(typemax(T)) + 1), false)

                @test FixedPointDecimals._check_overflows(T, V(V(typemax(T)) + 2), true)
                @test FixedPointDecimals._check_overflows(T, V(V(typemax(T)) + 2), false)
            end

            if V !== BigInt && typemax(T) > typemax(V)
                @test !FixedPointDecimals._check_overflows(T, typemax(V), true)
                @test !FixedPointDecimals._check_overflows(T, typemax(V), false)
            end
        end
    end

    @testset "$T" for T in UnsignedIntegerTypes
        @testset "$V" for V in (UInt64, UInt128, BigInt)
            @test !FixedPointDecimals._check_overflows(T, V(0), true)
            @test !FixedPointDecimals._check_overflows(T, V(0), false)
            @test !FixedPointDecimals._check_overflows(T, V(1), false)

            if V === BigInt
                @test FixedPointDecimals._check_overflows(T, V(1), true)
                @test FixedPointDecimals._check_overflows(T, V(typemax(T) - 1), true)
                @test FixedPointDecimals._check_overflows(T, V(typemax(T)), true)
                @test FixedPointDecimals._check_overflows(T, V(V(typemax(T)) + 1), true)
                @test FixedPointDecimals._check_overflows(T, V(V(typemax(T)) + 2), true)
            end

            if V === BigInt || typemax(T) == typemax(V)
                @test !FixedPointDecimals._check_overflows(T, V(typemax(T) - 1), false)
                @test !FixedPointDecimals._check_overflows(T, V(typemax(T)), false)
            end

            if V === BigInt || typemax(T) < typemax(V)
                @test FixedPointDecimals._check_overflows(T, V(V(typemax(T)) + 1), false)
                @test FixedPointDecimals._check_overflows(T, V(V(typemax(T)) + 2), false)
            end

            if V !== BigInt && typemax(T) > typemax(V)
                @test !FixedPointDecimals._check_overflows(T, typemax(V), false)
            end
        end
    end
end

T = Int64
f = 8
@testset "xparse" begin
@testset "1.0" begin
    @testset "$T" for T in IntegerTypes
        @testset "$f" for f in 0:(_maxintdigits(T) - 1)
            expected_value = T(10) ^ f
            for test_case in [
                    "1",
                    "1.",
                    "1e0",
                    "1.0",
                    "1.e0",
                    "1.000",
                    "1.0E0",
                    "1.000e0",
                    "1.000e-0",
                    "1.000e+0",
                    "0.100e+1",
                    "10.000e-1",
                    ("0" ^ 99) * "1.0e0",
                    "1." * ("0" ^ 99) * "e0",
                    "1" * ("0" ^ 99) * "e-99",
                    "0." * ("0" ^ 98) * "1E+99",
                    ("0" ^ 99) * "1." * ("0" ^ 99) * "e0",
                ]
                @assert BigFloat(test_case) == 1.0
                test_decimal_parser(T, f, test_case, expect=expected_value)
                test_decimal_parser(T, f, "+" * test_case, expect=expected_value)
                if typemin(T) < 0
                    test_decimal_parser(T, f, "-" * test_case, expect=-expected_value)
                end
            end
        end
    end
end;

@testset "0.0" begin
    @testset "$T" for T in IntegerTypes
        @testset "$f" for f in 0:(_maxintdigits(T) - 1)
            expected_value = 0
            for test_case in [
                    "0",
                    ".0",
                    "0.",
                    "0e0",
                    "0.0",
                    ".0e0",
                    "0.e0",
                    "0.000",
                    "0.0e0",
                    "0.000e0",
                    "0000.e0",
                    "0.000e-0",
                    "0.000e+0",
                    "0.000e-99",
                    "0.000e+99",
                ]
                @assert BigFloat(test_case) == 0.0
                test_decimal_parser(T, f, test_case, expect=expected_value)
                test_decimal_parser(T, f, "+" * test_case, expect=expected_value)
                test_decimal_parser(T, f, "-" * test_case, expect=-expected_value)
            end
        end
    end
end;

@testset "typemax" begin
    @testset "$T" for T in IntegerTypes
        expected_value = typemax(T)
        @testset "$f" for f in 0:(_maxintdigits(T) - 1)
            f_comp = (_maxintdigits(T) - f)
            for test_case in [
                    # f = 2, T = Int8 | "1.27"
                    insertchar("$(typemax(T))", f_comp+1, "."),
                    # f = 2, T = Int8 | "01.27"
                    "0" * insertchar("$(typemax(T))", f_comp+1, "."),
                    # f = 2, T = Int8 | "1.27e0"
                    insertchar("$(typemax(T))", f_comp+1, ".") * "e0",
                    # f = 2, T = Int8 | "0.00127e+3"
                    "0." * "0" ^ (f) * "$(typemax(T))" * "e+$(_maxintdigits(T))",
                    # f = 2, T = Int8 | "12700e-4"
                    "$(typemax(T))" * "0" ^ f * "e-$(2f)",
                ]
                test_decimal_parser(T, f, test_case, expect=expected_value)
                test_decimal_parser(T, f, "+" * test_case, expect=expected_value)
            end
        end
    end
end;

@testset "typemin" begin
    @testset "$T" for T in IntegerTypes
        expected_value = typemin(T)
        expected_value == zero(T) && continue
        val_no_sign = replace(string(expected_value), "-" => "")
        maybesign = expected_value < zero(T) ? "-" : ""
        @testset "$f" for f in 0:(_maxintdigits(T) - 1)
            f_comp = (_maxintdigits(T) - f)
            for test_case in [
                    # f = 2, T = Int8 | "-1.28"
                    maybesign * insertchar("$(val_no_sign)", f_comp+1, "."),
                    # f = 2, T = Int8 | "-01.28"
                    maybesign * "0" * insertchar("$(val_no_sign)", f_comp+1, "."),
                    # f = 2, T = Int8 | "-1.28e0"
                    maybesign * insertchar("$(val_no_sign)", f_comp+1, ".") * "e0",
                    # f = 2, T = Int8 | "-0.00128e+3"
                    maybesign * "0." * "0" ^ (f) * "$(val_no_sign)" * "e+$(_maxintdigits(T))",
                    # f = 2, T = Int8 | "-12800e-4"
                    maybesign * "$(val_no_sign)" * "0" ^ f * "e-$(2f)",
                ]
                test_decimal_parser(T, f, test_case, expect=expected_value)
            end
        end
    end
end;

@testset "RoundingMode{:Nearest} " begin
    options = OPTIONS_NEAREST
    @testset "$T" for T in IntegerTypes
        for (base_str, expected_value) in [
           ("00499", 0),
           ("004" * ("9" ^ 99), 0),
           ("00500", 0),
           ("005" * ("0" ^ 99) * "1", 1),
           ("00501", 1),
           ("01499", 1),
           ("014" * ("9" ^ 99), 1),
           ("01500", 2),
           ("015" * ("0" ^ 99) * "1", 2),
           ("01501", 2),
        ]
            # f = 1 | "00.501"
            test_decimal_parser(T, 1, insertchar(base_str, 2, "."); expect=expected_value, options)
            # f = 1 | "005.01e-1"
            test_decimal_parser(T, 1, insertchar(base_str * "e-1", 3, "."); expect=expected_value, options)
            # f = 1 | "0050.1e-2"
            test_decimal_parser(T, 1, insertchar(base_str * "e-2", 4, "."); expect=expected_value, options)
            if typemin(T) < 0
                # f = 1 | "-00.501"
                test_decimal_parser(T, 1, "-" * insertchar(base_str, 2, "."); expect=-expected_value, options)
                # f = 1 | "-005.01e-1"
                test_decimal_parser(T, 1, "-" * insertchar(base_str * "e-1", 3, "."); expect=-expected_value, options)
                # f = 1 | "-0050.1e-1"
                test_decimal_parser(T, 1, "-" * insertchar(base_str * "e-2", 4, "."); expect=-expected_value, options)
            end
        end
        test_decimal_parser(T, 0, ".5"; expect=0, options)
        test_decimal_parser(T, 0, "0"; expect=0, options)
        test_decimal_parser(T, 0, "1"; expect=1, options)
    end
end;

@testset "RoundingMode{:ToZero} " begin
    options = OPTIONS_TOZERO
    @testset "$T" for T in IntegerTypes
        for (base_str, expected_value) in [
           ("00001", 0),
           ("000" * ("0" ^ 99) * "1", 0),
           ("009", 0),
           ("009" * ("9" ^ 99), 0),
           ("01001", 1),
           ("010" * ("0" ^ 99) * "1", 1),
           ("019", 1),
           ("019" * ("9" ^ 99), 1),
        ]
            # f = 1 | "0.1999..."
            test_decimal_parser(T, 1, insertchar(base_str, 2, "."); expect=expected_value, options)
            # f = 1 | "01.999...e-1"
            test_decimal_parser(T, 1, insertchar(base_str * "e-1", 3, "."); expect=expected_value, options)
            # f = 1 | "019.99...e-2"
            test_decimal_parser(T, 1, insertchar(base_str * "e-2", 4, "."); expect=expected_value, options)
            if typemin(T) < 0
                # f = 1 | "-0.1999.."
                test_decimal_parser(T, 1, "-" * insertchar(base_str, 2, "."); expect=-expected_value, options)
                # f = 1 | "-01.999..e-1"
                test_decimal_parser(T, 1, "-" * insertchar(base_str * "e-1", 3, "."); expect=-expected_value, options)
                # f = 1 | "-019.99..e-2"
                test_decimal_parser(T, 1, "-" * insertchar(base_str * "e-2", 4, "."); expect=-expected_value, options)
            end
        end
        test_decimal_parser(T, 0, ".5"; expect=0, options)
        test_decimal_parser(T, 0, "0"; expect=0, options)
        test_decimal_parser(T, 0, "1"; expect=1, options)
    end
end;

@testset "RoundingMode{:Throw} " begin
    options = OPTIONS_THROWS
    @testset "$T" for T in IntegerTypes
        for (base_str, expected_value) in [
           ("0.000", 0),
           ("0.100", 1),
           ("9.100e0", 91),
           ("0.900" * ("0" ^ 99), 9),
        ]
            test_decimal_parser(T, 1, base_str; expect=expected_value, options)
            if typemin(T) < 0
                test_decimal_parser(T, 1, "-" * base_str; expect=-expected_value, options)
            end
        end
        for base_str in [
            ("0.0100"),
            ("0.0001"),
            ("0.000" * ("0" ^ 99) * "1"),
            ("1.000" * ("0" ^ 99) * "1"),
         ]
            test_decimal_parser(T, 1, base_str; ok=false, inexact=true, options)
            if typemin(T) < 0
                test_decimal_parser(T, 1, "-" * base_str; ok=false, inexact=true, options)
            end
        end
    end
end

@testset "invalid" begin
    @testset "$T" for T in IntegerTypes
        for base_str in [
                "",
                "a",
                ";",
                # ".",
                "e",
                "E",
                ",0",
                "10,",
                "10,,0",
                "10,,",
                "10,.",
                "e0",
                "E0",
                # ".e0",
                # ".E0",
                "10ea",
                "10,e0",
            ]

            test_decimal_parser(T, 1, base_str, ok=false)
            test_decimal_parser(T, 1, "+" * base_str, ok=false)
            test_decimal_parser(T, 1, "-" * base_str, ok=false)
        end
    end
    @test Parsers.invalid(Parsers.xparse(FixedDecimal{T,f}, "10.0,0"; groupmark=GROUP, delim=DELIM, decimal=DECIM, quoted=true).code)
    @test Parsers.invalid(Parsers.xparse(FixedDecimal{T,f}, "10.0e1.0"; groupmark=GROUP, delim=DELIM, decimal=DECIM, quoted=true).code)
    @test Parsers.invalid(Parsers.xparse(FixedDecimal{T,f}, "10,e0"; groupmark=GROUP, delim=DELIM, decimal=DECIM, quoted=true).code)
end


@testset "overflow" begin
    @testset "typemax" begin
        @testset "$T" for T in IntegerTypes
            one_past_max = BigInt(typemax(T)) + 1
            @testset "$f" for f in 0:(_maxintdigits(T) - 1)
                f_comp = (_maxintdigits(T) - f)
                for test_case in [
                        # f = 2, T = Int8 | "1.28"
                        insertchar("$one_past_max", f_comp+1, "."),
                        # f = 2, T = Int8 | "01.28"
                        "0" * insertchar("$one_past_max", f_comp+1, "."),
                        # f = 2, T = Int8 | "1.28e0"
                        insertchar("$one_past_max", f_comp+1, ".") * "e0",
                        # f = 2, T = Int8 | "0.00128e+3"
                        "0." * "0" ^ (f) * "$one_past_max" * "e+$(_maxintdigits(T))",
                        # f = 2, T = Int8 | "12800e-4"
                        "$one_past_max" * "0" ^ f * "e-$(2f)",
                    ]
                    test_decimal_parser(T, f, test_case, ok=false)
                    test_decimal_parser(T, f, "+" * test_case, ok=false)
                end
            end
        end
    end

    @testset "typemin" begin
        @testset "$T" for T in IntegerTypes
            abs_one_under_min = lpad(string(abs(BigInt(typemin(T)) - 1)), _maxintdigits(T), "0")
            @testset "$f" for f in 0:(_maxintdigits(T) - 1)
                f_comp = (_maxintdigits(T) - f)
                for test_case in [
                        # f = 2, T = Int8 | "1.29"
                        insertchar("$abs_one_under_min", f_comp+1, "."),
                        # f = 2, T = Int8 | "01.29"
                        "0" * insertchar("$abs_one_under_min", f_comp+1, "."),
                        # f = 2, T = Int8 | "1.29e0"
                        insertchar("$abs_one_under_min", f_comp+1, ".") * "e0",
                        # f = 2, T = Int8 | "0.00129e+3"
                        "0." * "0" ^ (f) * "$abs_one_under_min" * "e+$(_maxintdigits(T))",
                        # f = 2, T = Int8 | "12900e-4"
                        "$abs_one_under_min" * "0" ^ f * "e-$(2f)",
                    ]
                    test_decimal_parser(T, f, "-" * test_case, ok=false)
                end
            end
        end
    end
end

@testset "misc" begin
    @testset "$T negative zero" for T in UnsignedIntegerTypes
        test_decimal_parser(T, 2, "-0.0000000", expect=0)
    end

    @testset "$T groupmarks" for T in IntegerTypes
        test_decimal_parser(T, 2, "1,2,7.0e-2", expect=127)
        test_decimal_parser(T, 2, "12,7.0e-2", expect=127)
        test_decimal_parser(T, 2, "1,27.0e-2", expect=127)
    end

    @test Parsers.xparse(FixedDecimal{Int,8}, "2", groupmark=',', delim=',', decimal='.', quoted=true).val == FixedDecimal{Int,8}(2)
    @test Parsers.xparse(FixedDecimal{Int,4}, "\" 2000.0 \"", groupmark=',', delim=',', decimal='.', quoted=true).val == FixedDecimal{Int,4}(2000)
    @test Parsers.xparse(FixedDecimal{Int,4}, "\" 2,000.0 \"", groupmark=',', delim=',', decimal='.', quoted=true).val == FixedDecimal{Int,4}(2000)
end

@testset "misc options" begin
    for (dec, grp, del) in (('.', "", ','), (',', " ", ';'), ('.', ",", '|'))
        test_options = Parsers.Options(groupmark=isempty(grp) ? nothing : only(grp), decimal=dec, delim=del, quoted=true, rounding=RoundNearest)
        for T in IntegerTypes
            for test_case in [
                    "123$(dec)123",
                    "123$(dec)123$(del)",
                    "1$(grp)2$(grp)3$(dec)123",
                    "1$(grp)2$(grp)3$(grp)1$(grp)2$(grp)3e-3$(del)",
                ]
                test_decimal_parser(T, 0, test_case, expect=123, options=test_options)
                test_decimal_parser(T, 0, "+" * test_case, expect=123, options=test_options)
                if typemin(T) < 0
                    test_decimal_parser(T, 0, "-" * test_case, expect=-123, options=test_options)
                end
            end
        end
    end
end
end # @testset "xparse"

@testset "parse" begin
    # Note: the underscore used in the reinterpreted integer is used to indicate the decimal
    # place.
    @testset "decimal position" begin
        @test parse(FD2, "123")   == reinterpret(FD2, 123_00)
        @test parse(FD2, "0.123") == reinterpret(FD2, 0_12)
        @test parse(FD2, ".123")  == reinterpret(FD2, 0_12)
        @test parse(FD2, "1.23")  == reinterpret(FD2, 1_23)
        @test parse(FD2, "12.3")  == reinterpret(FD2, 12_30)
        @test parse(FD2, "123.")  == reinterpret(FD2, 123_00)
        @test parse(FD2, "123.0") == reinterpret(FD2, 123_00)

        @test parse(FD2, "-123")   == reinterpret(FD2, -123_00)
        @test parse(FD2, "-0.123") == reinterpret(FD2, -0_12)
        @test parse(FD2, "-.123")  == reinterpret(FD2, -0_12)
        @test parse(FD2, "-1.23")  == reinterpret(FD2, -1_23)
        @test parse(FD2, "-12.3")  == reinterpret(FD2, -12_30)
        @test parse(FD2, "-123.")  == reinterpret(FD2, -123_00)
        @test parse(FD2, "-123.0") == reinterpret(FD2, -123_00)
    end

    @testset "scientific notation" begin
        @test parse(FD4, "12e0")   == reinterpret(FD4, 00012_0000)
        @test parse(FD4, "12e3")   == reinterpret(FD4, 12000_0000)
        @test parse(FD4, "12e-3")  == reinterpret(FD4, 00000_0120)
        @test parse(FD4, "1.2e0")  == reinterpret(FD4, 00001_2000)
        @test parse(FD4, "1.2e3")  == reinterpret(FD4, 01200_0000)
        @test parse(FD4, "1.2e-3") == reinterpret(FD4, 00000_0012)
        @test parse(FD4, "1.2e-4") == reinterpret(FD4, 00000_0001)

        @test parse(FD4, "-12e0")   == reinterpret(FD4, -00012_0000)
        @test parse(FD4, "-12e3")   == reinterpret(FD4, -12000_0000)
        @test parse(FD4, "-12e-3")  == reinterpret(FD4, -00000_0120)
        @test parse(FD4, "-1.2e0")  == reinterpret(FD4, -00001_2000)
        @test parse(FD4, "-1.2e3")  == reinterpret(FD4, -01200_0000)
        @test parse(FD4, "-1.2e-3") == reinterpret(FD4, -00000_0012)

        @test parse(FD2, "999e-1") == reinterpret(FD2, 99_90)
        @test parse(FD2, "999e-2") == reinterpret(FD2, 09_99)
        @test parse(FD2, "999e-3") == reinterpret(FD2, 01_00)
        @test parse(FD2, "999e-4") == reinterpret(FD2, 00_10)
        @test parse(FD2, "999e-5") == reinterpret(FD2, 00_01)
        @test parse(FD2, "999e-6") == reinterpret(FD2, 00_00)

        @test parse(FD2, "-999e-1") == reinterpret(FD2, -99_90)
        @test parse(FD2, "-999e-2") == reinterpret(FD2, -09_99)
        @test parse(FD2, "-999e-3") == reinterpret(FD2, -01_00)
        @test parse(FD2, "-999e-4") == reinterpret(FD2, -00_10)
        @test parse(FD2, "-999e-5") == reinterpret(FD2, -00_01)
        @test parse(FD2, "-999e-6") == reinterpret(FD2, -00_00)

        @test parse(FD4, "9"^96 * "e-100") == reinterpret(FD4, 0_001)
    end

    @testset "round to nearest" begin
        @test parse(FD2, "0.444") == reinterpret(FD2, 0_44)
        @test parse(FD2, "0.445") == reinterpret(FD2, 0_44)
        @test parse(FD2, "0.446") == reinterpret(FD2, 0_45)
        @test parse(FD2, "0.454") == reinterpret(FD2, 0_45)
        @test parse(FD2, "0.455") == reinterpret(FD2, 0_46)
        @test parse(FD2, "0.456") == reinterpret(FD2, 0_46)

        @test parse(FD2, "-0.444") == reinterpret(FD2, -0_44)
        @test parse(FD2, "-0.445") == reinterpret(FD2, -0_44)
        @test parse(FD2, "-0.446") == reinterpret(FD2, -0_45)
        @test parse(FD2, "-0.454") == reinterpret(FD2, -0_45)
        @test parse(FD2, "-0.455") == reinterpret(FD2, -0_46)
        @test parse(FD2, "-0.456") == reinterpret(FD2, -0_46)

        @test parse(FD2, "0.009")  == reinterpret(FD2,  0_01)
        @test parse(FD2, "-0.009") == reinterpret(FD2, -0_01)

        @test parse(FD4, "1.5e-4") == reinterpret(FD4, 0_0002)
    end

    @testset "round to zero" begin
        @test parse(FD2, "0.444", RoundToZero) == reinterpret(FD2, 0_44)
        @test parse(FD2, "0.445", RoundToZero) == reinterpret(FD2, 0_44)
        @test parse(FD2, "0.446", RoundToZero) == reinterpret(FD2, 0_44)
        @test parse(FD2, "0.454", RoundToZero) == reinterpret(FD2, 0_45)
        @test parse(FD2, "0.455", RoundToZero) == reinterpret(FD2, 0_45)
        @test parse(FD2, "0.456", RoundToZero) == reinterpret(FD2, 0_45)

        @test parse(FD2, "-0.444", RoundToZero) == reinterpret(FD2, -0_44)
        @test parse(FD2, "-0.445", RoundToZero) == reinterpret(FD2, -0_44)
        @test parse(FD2, "-0.446", RoundToZero) == reinterpret(FD2, -0_44)
        @test parse(FD2, "-0.454", RoundToZero) == reinterpret(FD2, -0_45)
        @test parse(FD2, "-0.455", RoundToZero) == reinterpret(FD2, -0_45)
        @test parse(FD2, "-0.456", RoundToZero) == reinterpret(FD2, -0_45)

        @test parse(FD2, "0.009", RoundToZero)  == reinterpret(FD2, 0_00)
        @test parse(FD2, "-0.009", RoundToZero) == reinterpret(FD2, 0_00)

        @test parse(FD4, "1.5e-4", RoundToZero) == reinterpret(FD4, 0_0001)
    end

    @testset "round throws" begin
        @test parse(FD2, "0.44", RoundThrows)  == reinterpret(FD2, 0_44)
        @test parse(FD2, "0.440", RoundThrows) == reinterpret(FD2, 0_44)

        @test_throws InexactError parse(FD2, "0.444", RoundThrows)
        @test_throws InexactError parse(FD2, "0.445", RoundThrows)
        @test_throws InexactError parse(FD2, "0.446", RoundThrows)
        @test_throws InexactError parse(FD2, "0.454", RoundThrows)
        @test_throws InexactError parse(FD2, "0.455", RoundThrows)
        @test_throws InexactError parse(FD2, "0.456", RoundThrows)

        @test_throws InexactError parse(FD2, "-0.444", RoundThrows)
        @test_throws InexactError parse(FD2, "-0.445", RoundThrows)
        @test_throws InexactError parse(FD2, "-0.446", RoundThrows)
        @test_throws InexactError parse(FD2, "-0.454", RoundThrows)
        @test_throws InexactError parse(FD2, "-0.455", RoundThrows)
        @test_throws InexactError parse(FD2, "-0.456", RoundThrows)

        @test_throws InexactError parse(FD2, "0.009", RoundThrows)
        @test_throws InexactError parse(FD2, "-0.009", RoundThrows)
        @test_throws InexactError parse(FD4, "1.5e-4", RoundThrows)
    end

    @testset "invalid" begin
        @test_throws OverflowError parse(FD4, "1.2e100")
        @test_throws ArgumentError parse(FD4, "foo")
        @test_throws ArgumentError parse(FD4, "1.2.3")
        @test_throws ArgumentError parse(FD4, "1.2", RoundUp)
    end
end # @testset "parse"

@testset "tryparse" begin
    # Note: the underscore used in the reinterpreted integer is used to indicate the decimal
    # place.
    @testset "decimal position" begin
        @test tryparse(FD2, "123")   == reinterpret(FD2, 123_00)
        @test tryparse(FD2, "0.123") == reinterpret(FD2, 0_12)
        @test tryparse(FD2, ".123")  == reinterpret(FD2, 0_12)
        @test tryparse(FD2, "1.23")  == reinterpret(FD2, 1_23)
        @test tryparse(FD2, "12.3")  == reinterpret(FD2, 12_30)
        @test tryparse(FD2, "123.")  == reinterpret(FD2, 123_00)
        @test tryparse(FD2, "123.0") == reinterpret(FD2, 123_00)

        @test tryparse(FD2, "-123")   == reinterpret(FD2, -123_00)
        @test tryparse(FD2, "-0.123") == reinterpret(FD2, -0_12)
        @test tryparse(FD2, "-.123")  == reinterpret(FD2, -0_12)
        @test tryparse(FD2, "-1.23")  == reinterpret(FD2, -1_23)
        @test tryparse(FD2, "-12.3")  == reinterpret(FD2, -12_30)
        @test tryparse(FD2, "-123.")  == reinterpret(FD2, -123_00)
        @test tryparse(FD2, "-123.0") == reinterpret(FD2, -123_00)
    end

    @testset "scientific notation" begin
        @test tryparse(FD4, "12e0")   == reinterpret(FD4, 00012_0000)
        @test tryparse(FD4, "12e3")   == reinterpret(FD4, 12000_0000)
        @test tryparse(FD4, "12e-3")  == reinterpret(FD4, 00000_0120)
        @test tryparse(FD4, "1.2e0")  == reinterpret(FD4, 00001_2000)
        @test tryparse(FD4, "1.2e3")  == reinterpret(FD4, 01200_0000)
        @test tryparse(FD4, "1.2e-3") == reinterpret(FD4, 00000_0012)
        @test tryparse(FD4, "1.2e-4") == reinterpret(FD4, 00000_0001)

        @test tryparse(FD4, "-12e0")   == reinterpret(FD4, -00012_0000)
        @test tryparse(FD4, "-12e3")   == reinterpret(FD4, -12000_0000)
        @test tryparse(FD4, "-12e-3")  == reinterpret(FD4, -00000_0120)
        @test tryparse(FD4, "-1.2e0")  == reinterpret(FD4, -00001_2000)
        @test tryparse(FD4, "-1.2e3")  == reinterpret(FD4, -01200_0000)
        @test tryparse(FD4, "-1.2e-3") == reinterpret(FD4, -00000_0012)

        @test tryparse(FD2, "999e-1") == reinterpret(FD2, 99_90)
        @test tryparse(FD2, "999e-2") == reinterpret(FD2, 09_99)
        @test tryparse(FD2, "999e-3") == reinterpret(FD2, 01_00)
        @test tryparse(FD2, "999e-4") == reinterpret(FD2, 00_10)
        @test tryparse(FD2, "999e-5") == reinterpret(FD2, 00_01)
        @test tryparse(FD2, "999e-6") == reinterpret(FD2, 00_00)

        @test tryparse(FD2, "-999e-1") == reinterpret(FD2, -99_90)
        @test tryparse(FD2, "-999e-2") == reinterpret(FD2, -09_99)
        @test tryparse(FD2, "-999e-3") == reinterpret(FD2, -01_00)
        @test tryparse(FD2, "-999e-4") == reinterpret(FD2, -00_10)
        @test tryparse(FD2, "-999e-5") == reinterpret(FD2, -00_01)
        @test tryparse(FD2, "-999e-6") == reinterpret(FD2, -00_00)

        @test tryparse(FD4, "9"^96 * "e-100") == reinterpret(FD4, 0_001)
    end

    @testset "round to nearest" begin
        @test tryparse(FD2, "0.444") == reinterpret(FD2, 0_44)
        @test tryparse(FD2, "0.445") == reinterpret(FD2, 0_44)
        @test tryparse(FD2, "0.446") == reinterpret(FD2, 0_45)
        @test tryparse(FD2, "0.454") == reinterpret(FD2, 0_45)
        @test tryparse(FD2, "0.455") == reinterpret(FD2, 0_46)
        @test tryparse(FD2, "0.456") == reinterpret(FD2, 0_46)

        @test tryparse(FD2, "-0.444") == reinterpret(FD2, -0_44)
        @test tryparse(FD2, "-0.445") == reinterpret(FD2, -0_44)
        @test tryparse(FD2, "-0.446") == reinterpret(FD2, -0_45)
        @test tryparse(FD2, "-0.454") == reinterpret(FD2, -0_45)
        @test tryparse(FD2, "-0.455") == reinterpret(FD2, -0_46)
        @test tryparse(FD2, "-0.456") == reinterpret(FD2, -0_46)

        @test tryparse(FD2, "0.009")  == reinterpret(FD2,  0_01)
        @test tryparse(FD2, "-0.009") == reinterpret(FD2, -0_01)

        @test tryparse(FD4, "1.5e-4") == reinterpret(FD4, 0_0002)
    end

    @testset "round to zero" begin
        @test tryparse(FD2, "0.444", RoundToZero) == reinterpret(FD2, 0_44)
        @test tryparse(FD2, "0.445", RoundToZero) == reinterpret(FD2, 0_44)
        @test tryparse(FD2, "0.446", RoundToZero) == reinterpret(FD2, 0_44)
        @test tryparse(FD2, "0.454", RoundToZero) == reinterpret(FD2, 0_45)
        @test tryparse(FD2, "0.455", RoundToZero) == reinterpret(FD2, 0_45)
        @test tryparse(FD2, "0.456", RoundToZero) == reinterpret(FD2, 0_45)

        @test tryparse(FD2, "-0.444", RoundToZero) == reinterpret(FD2, -0_44)
        @test tryparse(FD2, "-0.445", RoundToZero) == reinterpret(FD2, -0_44)
        @test tryparse(FD2, "-0.446", RoundToZero) == reinterpret(FD2, -0_44)
        @test tryparse(FD2, "-0.454", RoundToZero) == reinterpret(FD2, -0_45)
        @test tryparse(FD2, "-0.455", RoundToZero) == reinterpret(FD2, -0_45)
        @test tryparse(FD2, "-0.456", RoundToZero) == reinterpret(FD2, -0_45)

        @test tryparse(FD2, "0.009", RoundToZero)  == reinterpret(FD2, 0_00)
        @test tryparse(FD2, "-0.009", RoundToZero) == reinterpret(FD2, 0_00)

        @test tryparse(FD4, "1.5e-4", RoundToZero) == reinterpret(FD4, 0_0001)
    end

    @testset "round throws" begin
        @test tryparse(FD2, "0.44", RoundThrows)  == reinterpret(FD2, 0_44)
        @test tryparse(FD2, "0.440", RoundThrows) == reinterpret(FD2, 0_44)

        @test isnothing(tryparse(FD2, "0.444", RoundThrows))
        @test isnothing(tryparse(FD2, "0.445", RoundThrows))
        @test isnothing(tryparse(FD2, "0.446", RoundThrows))
        @test isnothing(tryparse(FD2, "0.454", RoundThrows))
        @test isnothing(tryparse(FD2, "0.455", RoundThrows))
        @test isnothing(tryparse(FD2, "0.456", RoundThrows))

        @test isnothing(tryparse(FD2, "-0.444", RoundThrows))
        @test isnothing(tryparse(FD2, "-0.445", RoundThrows))
        @test isnothing(tryparse(FD2, "-0.446", RoundThrows))
        @test isnothing(tryparse(FD2, "-0.454", RoundThrows))
        @test isnothing(tryparse(FD2, "-0.455", RoundThrows))
        @test isnothing(tryparse(FD2, "-0.456", RoundThrows))

        @test isnothing(tryparse(FD2, "0.009", RoundThrows))
        @test isnothing(tryparse(FD2, "-0.009", RoundThrows))
        @test isnothing(tryparse(FD4, "1.5e-4", RoundThrows))
    end

    @testset "invalid" begin
        @test isnothing(tryparse(FD4, "1.2e100"))
        @test isnothing(tryparse(FD4, "foo"))
        @test isnothing(tryparse(FD4, "1.2.3"))
        @test_throws ArgumentError tryparse(FD4, "1.2", RoundUp)
    end
end # @testset "tryparse"
