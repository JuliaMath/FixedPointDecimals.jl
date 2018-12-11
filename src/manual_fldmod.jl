narrow(::Type{Int128}) = Int64
narrow(::Type{Int64}) = Int32
narrow(::Type{Int32}) = Int16
narrow(::Type{Int16}) = Int8
narrow(::Type{UInt128}) = UInt64
narrow(::Type{UInt64}) = UInt32
narrow(::Type{UInt32}) = UInt16
narrow(::Type{UInt16}) = UInt8

twoToTheSizeOf(::Type{T}) where {T} = typemax(T)
twoToTheSizeOf(::Type{BigInt}) = BigInt(2)^256

_leading_zeros(x) = leading_zeros(x)
_leading_zeros(x::BigInt) = leading_zeros((x >> 128) % UInt128)

Base.@pure function calculate_inv_coeff(::Type{T}, f) where {T}
    # First, calculate 2^nbits(T)/f
    # We calculate it at double-precision and shift away leading zeros to preserve the most
    # precision. Later, we will shift the final answer back to undo this operation.
    UT = unsigned(T)
    WUT = widen(UT)
    invcoeff = twoToTheSizeOf(WUT)÷f
    @show invcoeff
    toshift = _leading_zeros(invcoeff)
    @show toshift
    invcoeff = invcoeff << toshift
    # Now, truncate the result to sizeof(T), rounding to maintain precision.
    invcoeff = _round_to_even(fldmod(invcoeff, typemax(UT))..., typemax(UT)) % T
    return invcoeff, toshift
end
##calculate_inv_coeff(Int64, 100)
#@inline function imul_splitwidemul(a::T,b::T) where {T}
#    TT = Tuple{T,T}
#    LLVM.Interop.@asmcall("""
#        movq   \$3, %rax
#        imulq  \$2
#        movq   %rdx, \$1
#        movq   %rax, \$0
#    """, "=r,=r,r,r", TT, Tuple{T,unsigned(T)}, a, unsigned(b))
#end
#imul_splitwidemul(a,b) = imul_splitwidemul(promote(signed.((a,b))...)...)
#imul_splitwidemul(99, 0xA3D70A3D70A3D70B)

function splitint(x::T) where {T<:Integer}
    NT = narrow(T)
    NT(x >> nbits(NT)), x % NT
end

nbits(::Type{T}) where {T} = sizeof(T)*8
nbits(x::T) where {T} = nbits(T)

# /* compute the upper 32 bits of the product of two unsigned 32-bit integers */
function unsigned_splitmul_upper(a::T, b::T) where T<:Unsigned
    # /* split operands into halves */
    ah,al = splitint(a)
    bh,bl = splitint(b)
    halfT = typeof(ah)
    halfbits = nbits(al)
    # /* compute partial products */
    p0 = widemul(al, bl);
    p1 = widemul(al, bh);
    p2 = widemul(ah, bl);
    p3 = widemul(ah, bh);
    # /* sum partial products */
    cy = ((p0 >> halfbits) + (p1%halfT) + (p2%halfT)) >> halfbits;
    return p3 + (p2 >> halfbits) + (p1 >> halfbits) + cy;
end
function splitmul_upper(a::T, b::T) where T<:Unsigned
    return unsigned_splitmul_upper(a,b)
end
function splitmul_upper(a::T, b::T) where T<:Signed
    return signed(unsigned_splitmul_upper(unsigned(a), unsigned(b))) - ((a < 0) ? b : 0) - ((b < 0) ? a : 0);
end
#splitmul_upper(a,b) = splitmul_upper(promote(a,b)...)
#a,b = unsigned(0xA3D70A3D70A3D70B), (0x2000000000000000 % UInt64)
#splitmul_upper(a,b)
#splitint(widemul(a,b))
#a,b = signed(0xA3D70A3D70A3D70B), signed(0x2000000000000000 % UInt64)
#splitmul_upper(a,b)
#splitint(widemul(a,b))


function manual_div_by_const(x::T, ::Val{C}) where {T, C}
    inv_coeff, toshift = calculate_inv_coeff(T, C)
    up = splitmul_upper(x, inv_coeff)
    out = up    # By keeping only the upper half, we're essentially dividing by 2^nbits(T)
    out += x  # Because this _specific_ magic number has a leading one, the result is negative. We add x to give us the positive equivalent.
    signshift = (nbits(x)-1)
    signed = T(unsigned(out) >> signshift)  # "unsigned" bitshift (to read top bit)
    out = out >> toshift
    out =  out + signed     # ; leaq    (%rdx,%rax), %rax
    return T(out)
end
#println()
#manual_div_by_const(2432, Val(100))
#manual_div_by_const(-2400, Val(100))

#abstract type AbstractDivVal <: Integer end
#struct DivConst{C} <: AbstractDivVal  end
#struct DivVal{T} <: AbstractDivVal
#    v::T
#end
#struct DivValXY{X,Y} <: Integer
#    x::X
#end
#Base.promote(x::DivVal{T}, y::DivConst{C}) where {T,C} = DivValXY{T,C}(x.v), DivValXY{T,C}(x.v)
#div(x::DivValXY{T,C}, y::DivValXY{T,C}) where {T,C} = manual_div_by_const(x.x, Val(C))
#
#div(DivValXY{Int64,100}(253), DivValXY{Int64,100}(253))
#fld(DivValXY{Int64,100}(253), DivValXY{Int64,100}(253))

function fld_by_const(x::T, y::Val{C}) where {T<:Integer, C}
    d = manual_div_by_const(x,y)
    return d - (signbit(x ⊻ C) & (d * C !=x))
end

@inline function manual_mod(x::T, y::T, quotient::T) where T<:Integer
    y == -1 && return T(0)   # avoid potential overflow in fld
    return x - quotient * y
end

function manual_fldmod_by_const(x,y::Val{C}) where {C}
    d = fld_by_const(x, y)
    d, manual_mod(promote(x, C, d)...)
end
#manual_fldmod_by_const(2432, Val(100))
#manual_fldmod_by_const(-2499, Val(100))
