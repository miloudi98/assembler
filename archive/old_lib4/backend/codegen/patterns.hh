#ifndef __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_PATTERNS_HH__
#define __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_PATTERNS_HH__

#include "lib/common/base.hh"
#include "lib/backend/ir.hh"

namespace fiska::assembler::backend {

//=====================================================================================================================
// X86 operand class identifier concept.
//=====================================================================================================================
template <typename T> struct is_x86_op_class_t : std::false_type {};
template <typename T> concept is_x86_op_class = is_x86_op_class_t<T>::value;

template <auto... args>
struct r;

template <auto... args>
struct m;

template <auto... args>
struct i;

template <auto... args>
struct mo;

template <is_x86_op_class... OpClass>
struct AnyOf;


template <is_x86_op_class... OpClass>
struct is_x86_op_class_t<AnyOf<OpClass...>> : std::true_type {};

template <auto... args> 
struct is_x86_op_class_t<r<args...>> : std::true_type {};

template <auto... args> 
struct is_x86_op_class_t<m<args...>> : std::true_type {};

template <auto... args>
struct is_x86_op_class_t<i<args...>> : std::true_type {};

template <auto... args>
struct is_x86_op_class_t<mo<args...>> : std::true_type {};


template <>
struct r<> {
    static auto match(IRX86Op::Ref op) -> i1 { return op.r(); }
};

template <BW bit_width>
struct r<bit_width> {
    static auto match(IRX86Op::Ref op) -> i1 {
        return op.r() 
            and op.as_r().bw_ == bit_width
            and op.as_r().kind() == RK::Gp;
    }
};

template <RK kind>
struct r<kind> {
    static auto match(IRX86Op::Ref op) -> i1 {
        return op.r()
            and op.as_r().kind() == kind;
    }
};

template <BW bit_width, RI id>
struct r<bit_width, id> {
    static auto match(IRX86Op::Ref op) -> i1 {
        return op.r()
            and op.as_r().bw_ == bit_width
            and op.as_r().ri_ == id;
    }
};

template <RI id>
struct r<id> {
    static auto match(IRX86Op::Ref op) -> i1 {
        return op.r() and op.as_r().ri_ == id;
    }
};

template <>
struct m<> {
    static auto match(IRX86Op::Ref op) -> i1 { return op.m(); }
};

template <BW bit_width>
struct m<bit_width> {
    static auto match(IRX86Op::Ref op) -> i1 {
        return op.m() and op.as_m().bw_ == bit_width;
    }
};

template <>
struct i<> {
    static auto match(IRX86Op::Ref op) -> i1 { return op.i(); }
};

template <BW bit_width>
struct i<bit_width> {
    static auto match(IRX86Op::Ref op) -> i1 {
        return op.i() and op.as_i().bw_ == bit_width;
    }
};

template <>
struct mo<> {
    static auto match(IRX86Op::Ref op) -> i1 { return op.mo(); }
};

template <BW bit_width>
struct mo<bit_width> {
    static auto match(IRX86Op::Ref op) -> i1 {
        return op.mo() and op.as_mo().bw_ == bit_width;
    }
};

template <is_x86_op_class... IRX86OpClass>
struct AnyOf {
    static auto match(IRX86Op::Ref op) -> i1 {
        return (IRX86OpClass::match(op) or ...);
    }
};

//=====================================================
// Patterns.
//=====================================================
// [[intel]]
// r8 — One of the byte general-purpose registers: AL, CL, DL, BL, AH, CH, DH, BH, BPL, SPL, DIL, and SIL; or
// one of the byte registers (R8B - R15B) available when using REX.R and 64-bit mode
using r8 = r<BW::B8>;
// [[intel]]
// r16 — One of the word general-purpose registers: AX, CX, DX, BX, SP, BP, SI, DI; or one of the word registers
// (R8-R15) available when using REX.R and 64-bit mode.
using r16 = r<BW::B16>;
// [[intel]]
// r32 — One of the doubleword general-purpose registers: EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI; or one of
// the doubleword registers (R8D - R15D) available when using REX.R in 64-bit mode.
using r32 = r<BW::B32>;
// [[intel]]
// r64 — One of the quadword general-purpose registers: RAX, RBX, RCX, RDX, RDI, RSI, RBP, RSP, R8–R15.
// These are available when using REX.R and 64-bit mode.
using r64 = r<BW::B64>;
// [[intel]]
// m8 — A byte operand in memory.
using m8 = m<BW::B8>;
// [[intel]]
// m16 — A word operand in memory.
using m16 = m<BW::B16>;
// [[intel]]
// m32 — A doubleword operand in memory. 
using m32 = m<BW::B32>;
// [[intel]]
// m64 — A memory quadword operand in memory.
using m64 = m<BW::B64>;
// r/m — Register or memory operand.
using rm = AnyOf<r<>, m<>>;
// [[intel]]
// r/m8 — A byte operand that is either the contents of a byte general-purpose register (AL, CL, DL, BL, AH, CH,
// DH, BH, BPL, SPL, DIL, and SIL) or a byte from memory. Byte registers R8B - R15B are available using REX.R
// in 64-bit mode.
using rm8 = AnyOf<r8, m8>;
// [[intel]]
// r/m16 — A word general-purpose register or memory operand used for instructions whose operand-size
// attribute is 16 bits. The word general-purpose registers are: AX, CX, DX, BX, SP, BP, SI, DI. The contents of
// memory are found at the address provided by the effective address computation. Word registers R8W - R15W
// are available using REX.R in 64-bit mode.
using rm16 = AnyOf<r16, m16>;
// [[intel]]
// r/m32 — A doubleword general-purpose register or memory operand used for instructions whose operand-
// size attribute is 32 bits. The doubleword general-purpose registers are: EAX, ECX, EDX, EBX, ESP, EBP, ESI,
// EDI. The contents of memory are found at the address provided by the effective address computation.
// Doubleword registers R8D - R15D are available when using REX.R in 64-bit mode.
using rm32 = AnyOf<r32, m32>;
// [[intel]]
// r/m64 — A quadword general-purpose register or memory operand used for instructions whose operand-size
// attribute is 64 bits when using REX.W. Quadword general-purpose registers are: RAX, RBX, RCX, RDX, RDI,
// RSI, RBP, RSP, R8–R15; these are available only in 64-bit mode. The contents of memory are found at the
// address provided by the effective address computation.
using rm64 = AnyOf<r64, m64>;
// [[intel]]
// moffs8, moffs16, moffs32, moffs64 — A simple memory variable (memory offset) of type byte, word, or
// doubleword used by some variants of the MOV instruction. The actual address is given by a simple offset
// relative to the segment base. No ModR/M byte is used in the instruction.
//
// "The number shown with moffs indicates its size, which is determined by the address-size attribute of the instruction."
// I am not sure whether the last sentence is correct. From my understanding, the number shown with moffs (i.e the '8' in moffs8)
// indicated the number of bits we are reading from memory. The address-size attribute of the instruction doesn't affect the size
// of the data read. Maybe I am simply misunderstanding this.
using moffs8 = mo<BW::B8>;
using moffs16 = mo<BW::B16>;
using moffs32 = mo<BW::B32>;
using moffs64 = mo<BW::B64>;
// [[intel]]
// imm8 — An immediate byte value. The imm8 symbol is a signed number between –128 and +127 inclusive.
// For instructions in which imm8 is combined with a word or doubleword operand, the immediate value is sign-
// extended to form a word or doubleword. The upper byte of the word is filled with the topmost bit of the
// immediate value.
using imm8 = i<BW::B8>;
// [[intel]]
//imm16 — An immediate word value used for instructions whose operand-size attribute is 16 bits. This is a
//number between –32,768 and +32,767 inclusive.
using imm16 = i<BW::B16>;
// [[intel]]
// imm32 — An immediate doubleword value used for instructions whose operand-size attribute is 32
// bits. It allows the use of a number between +2,147,483,647 and –2,147,483,648 inclusive.
using imm32 = i<BW::B32>;
// [[intel]]
// imm64 — An immediate quadword value used for instructions whose operand-size attribute is 64 bits.
// The value allows the use of a number between +9,223,372,036,854,775,807 and –
// 9,223,372,036,854,775,808 inclusive.
using imm64 = i<BW::B64>;
// [[intel]]
// Sreg — A segment register. The segment register bit assignments are ES = 0, CS = 1, SS = 2, DS = 3, FS = 4,
// and GS = 5
using sreg = r<RK::Seg>;
// Cr — A control register. Control registers are Cr0 -- Cr15. 
using cr = r<RK::Ctrl>;
// Dbg — A debug register. Debug registers are Dbg0 -- Dbg15. 
using dbg = r<RK::Dbg>;
// Rax, Eax, Ax, Al — Patterns for parts of the register Rax. We create patterns for them since
// they are used a lot.
using rax = r<BW::B64, RI::Rax>;
using eax = r<BW::B32, RI::Rax>;
using ax = r<BW::B16, RI::Rax>;
using al = r<BW::B8, RI::Rax>;
// rexophobe_byte_regs — Byte registers that are not encodable when a REX prefix is present.
using rexophobe_byte_regs = AnyOf<
    r<RI::Rah>,
    r<RI::Rch>,
    r<RI::Rdh>,
    r<RI::Rbh>
>;
// rexophile_byte_regs — Byte Registers that require a rex prefix to be encoded.
using rexophile_byte_regs = AnyOf<
    r<BW::B8, RI::Rsp>, r<BW::B8, RI::Rbp>,
    r<BW::B8, RI::Rsi>, r<BW::B8, RI::Rdi>
>;

} // namespace fiska::assembler::backend

#endif // __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_PATTERNS_HH__
