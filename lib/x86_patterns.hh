#ifndef __X86_ASSEMBLER_LIB_X86_PATTERNS_HH__
#define __X86_ASSEMBLER_LIB_X86_PATTERNS_HH__

#include "lib/core.hh"
#include "lib/x86_utils.hh"

namespace fiska::x86::patterns {

// Forward declarations.
struct any;

// Base class of all X86OpClasses.
struct X86OpClass {};

template <typename T>
concept IsX86OpClass = std::derived_from<T, X86OpClass>;

template <typename T>
concept IsAnyX86OpClass = std::same_as<std::remove_cvref_t<T>, any>;
    
//====================================================================
// Alternative.
//====================================================================
template <IsX86OpClass... X86Ops>
struct Alt : X86OpClass {
    static constexpr auto match(X86Op::Ref op) -> i1 {
        return (X86Ops::match(op) or ...);
    }

    static auto instances() -> X86Op::List {
        X86Op::List ret;

        auto add_x86op_instances = [&]<typename OpClass>() {
            X86Op::List instances = OpClass::instances();
            ret.insert(ret.end(), instances.begin(), instances.end());
        };

        (add_x86op_instances.template operator()<X86Ops>(), ...);
        return ret;
    }
};

//====================================================================
// Wild card matching anything.
//====================================================================
struct any : X86OpClass {
    static constexpr auto match(X86Op::Ref op) -> i1 {
        return true;
    }
};

//=====================================================
// Register classes.
//=====================================================
template <auto... args>
struct r;

template <BW bit_width>
struct r<bit_width> : X86OpClass {
    static constexpr auto match(X86Op::Ref op) -> i1 {
        return op.is<Reg>() 
            and op.as<Reg>().bw_ == bit_width
            and op.as<Reg>().kind() == RK::Gp;
    }

    // All general purpose registers with a given |bit_width|.
    static auto instances() -> X86Op::List {
        X86Op::List ret;
        
        for (u32 ri = 0; ri < AsCtx::knum_regs; ++ri) {
            RI rid = RI(ri);

            if (rk_of_ri(rid) != RK::Gp) { continue; }
            if (not rgs::contains(AsCtx::kbw_of_ri[+rid], bit_width)) { continue; }

            ret.push_back(X86Op{Reg{bit_width, rid}});
        }

        return ret;
    }
};

template <RK kind>
struct r<kind> : X86OpClass {
    static constexpr auto match(X86Op::Ref op) -> i1 {
        return op.is<Reg>()
            and op.as<Reg>().kind() == kind;
    }

    static auto instances() -> X86Op::List {
        X86Op::List ret;
        
        for (u32 rid = 0; rid < AsCtx::knum_regs; ++rid) {
            if (rk_of_ri(RI(rid)) != kind) { continue; }

            for (BW w : AsCtx::kbw_of_ri[rid]) {
                ret.push_back(X86Op{Reg{w, RI(rid)}});
            }
        }

        return ret;
    }
};

template <BW bit_width, RI id>
struct r<bit_width, id> : X86OpClass {
    static constexpr auto match(X86Op::Ref op) -> i1 {
        return op.is<Reg>()
            and op.as<Reg>().bw_ == bit_width
            and op.as<Reg>().id_ == id;
    }

    static auto instances() -> X86Op::List {
        return { X86Op{ Reg{bit_width, id} } };
    }
};

template <RI id>
struct r<id> : X86OpClass {
    static constexpr auto match(X86Op::Ref op) -> i1 {
        return op.is<Reg>() and op.as<Reg>().id_ == id;
    }

    static auto instances() -> X86Op::List {
        X86Op::List ret;

        for (BW w : AsCtx::kbw_of_ri[+id]) {
            ret.push_back(X86Op{Reg{w, id}});
        }

        return ret;
    }
};

//=====================================================
// Memory classes.
//=====================================================
template <auto... args>
struct m;

template <BW bit_width>
struct m<bit_width> : X86OpClass {
    static constexpr auto match(X86Op::Ref op) -> i1 {
        return op.is<Mem>() and op.as<Mem>().bw_ == bit_width;
    }

    static auto instances() -> X86Op::List {
        using enum MemIndexScale;

        X86Op::List ret;
        X86Op::List base_regs = r<BW::B64>::instances();
        X86Op::List index_regs = r<BW::B64>::instances();
        
        // It is not possible for the register Rsp to serve as an index
        // register, so we remove it.
        std::erase_if(index_regs, [](X86Op::Ref op) { return op.as<Reg>().id_ == RI::Rsp; });

        // Generate a random displacement. The returned displacement can either be
        // 8 or 32-bit wide.
        auto random_disp = [&]() {
            i1 return_8bit_disp = AsCtx::k8bit_rand_int(AsCtx::kgen) & 1;
            if (return_8bit_disp) {
                return i64(AsCtx::k8bit_rand_int(AsCtx::kgen));
            }
            return i64(AsCtx::k32bit_rand_int(AsCtx::kgen));
        };

        // Generate |MK::BaseDisp| memory references.
        for (X86Op::Ref breg : base_regs) {
            ret.push_back(X86Op{
                Mem::make(bit_width, breg.as<Reg>(), std::nullopt, std::nullopt, random_disp())
            });
        }

        // Generate |MK::BaseIndexDisp| memory references.
        for (const auto& [breg, ireg] : vws::cartesian_product(base_regs, index_regs)) {
            for (MemIndexScale s : {One, Two, Four, Eight}) {
                ret.push_back(X86Op{
                    Mem::make(bit_width, breg.as<Reg>(), ireg.as<Reg>(), s, random_disp())
                });
            }
        }

        // Generate |MK::IndexDisp| memory references.
        for (X86Op::Ref ireg : index_regs) {
            for (MemIndexScale s : {One, Two, Four, Eight}) {
                ret.push_back(X86Op{
                    Mem::make(bit_width, std::nullopt, ireg.as<Reg>(), s, random_disp())
                });
            }
        }

        // Generate |MK::DispOnly| memory references.
        // We will generate 5 of them. The choice of 10 is arbitrary.
        for (u8 i = 0; i < 5; ++i) {
            ret.push_back(X86Op{
                Mem::make(bit_width, std::nullopt, std::nullopt, std::nullopt, random_disp())
            });
        }

        // Generate memory references with |Rip| as the base register.
        // We will generate 5 of them. The choice of 10 is arbitrary.
        // The only difference between the memory references generated here will be the 
        // displacement.
        Reg rip{BW::B64, RI::Rip};
        for (u8 i = 0; i < 5; ++i) {
            ret.push_back(X86Op{
                Mem::make(bit_width, rip, std::nullopt, std::nullopt, random_disp())
            });
        }

        return ret;
    }
};

template <IsX86OpClass auto breg_class, IsX86OpClass auto ireg_class> 
struct m<breg_class, ireg_class> : X86OpClass {
    static constexpr auto match(X86Op::Ref op) -> i1 {
        constexpr i1 breg_is_any = IsAnyX86OpClass<decltype(breg_class)>; 
        constexpr i1 ireg_is_any = IsAnyX86OpClass<decltype(ireg_class)>;

        i1 breg_match = breg_is_any or (op.is<Mem>() and op.as<Mem>().base_reg_ and breg_class.match(X86Op{*op.as<Mem>().base_reg_}));
        i1 ireg_match = ireg_is_any or (op.is<Mem>() and op.as<Mem>().index_reg_ and ireg_class.match(X86Op{*op.as<Mem>().index_reg_}));

        return breg_match and ireg_match;
    }
};

//=====================================================
// Immediate classes.
//=====================================================
template <auto... args>
struct i;

template <BW bit_width>
struct i<bit_width> : X86OpClass {
    static constexpr auto match(X86Op::Ref op) -> i1 {
        return op.is<Imm>() 
            and op.as<Imm>().bw_ == bit_width;
    }

    static auto instances() -> X86Op::List {
        X86Op::List ret;

        auto random_bit_width_wide_imm = [&]() {
            switch (bit_width) {
            case BW::B0:
            case BW::B24:
                unreachable();

            case BW::B8:
                return i64(AsCtx::k8bit_rand_int(AsCtx::kgen));
            case BW::B16:
                return i64(AsCtx::k16bit_rand_int(AsCtx::kgen));
            case BW::B32:
                return i64(AsCtx::k32bit_rand_int(AsCtx::kgen));
            case BW::B64:
                return i64(AsCtx::k64bit_rand_int(AsCtx::kgen));
            } // switch
            unreachable();
        };

        // Generate immediates that are |bit_width| wide.
        // We will generate 5 of them. The choice of 5 is arbitrary. It should
        // be enough to test our system.
        for (u8 i = 0; i < 5; ++i) {
            ret.push_back(X86Op {
                Imm{bit_width, random_bit_width_wide_imm()}
            });
        }

        return ret;
    }
};

//=====================================================
// Memory offset classes.
//=====================================================
template <auto... args>
struct mo;

template <BW bit_width>
struct mo<bit_width> : X86OpClass {
    static constexpr auto match(X86Op::Ref op) -> i1 {
        return op.is<Moffs>() 
            and op.as<Moffs>().bw_ == bit_width;
    }

    static auto instances() -> X86Op::List {
        X86Op::List ret;

        // Generate random memory offsets all of which are 64 bit wide.
        // We will generate 5 of them. The choice of 5 is arbitrary. It should
        // be enough to test our system.
        for (u8 i = 0; i < 5; ++i) {
            ret.push_back(X86Op {
                Moffs{bit_width, AsCtx::k64bit_rand_int(AsCtx::kgen)}
            });
        }

        return ret;
    }
};

//=====================================================
// X86 Operand Patterns.
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
// [[intel]]
// r/m8 — A byte operand that is either the contents of a byte general-purpose register (AL, CL, DL, BL, AH, CH,
// DH, BH, BPL, SPL, DIL, and SIL) or a byte from memory. Byte registers R8B - R15B are available using REX.R
// in 64-bit mode.
using rm8 = Alt<r8, m8>;
// [[intel]]
// r/m16 — A word general-purpose register or memory operand used for instructions whose operand-size
// attribute is 16 bits. The word general-purpose registers are: AX, CX, DX, BX, SP, BP, SI, DI. The contents of
// memory are found at the address provided by the effective address computation. Word registers R8W - R15W
// are available using REX.R in 64-bit mode.
using rm16 = Alt<r16, m16>;
// [[intel]]
// r/m32 — A doubleword general-purpose register or memory operand used for instructions whose operand-
// size attribute is 32 bits. The doubleword general-purpose registers are: EAX, ECX, EDX, EBX, ESP, EBP, ESI,
// EDI. The contents of memory are found at the address provided by the effective address computation.
// Doubleword registers R8D - R15D are available when using REX.R in 64-bit mode.
using rm32 = Alt<r32, m32>;
// [[intel]]
// r/m64 — A quadword general-purpose register or memory operand used for instructions whose operand-size
// attribute is 64 bits when using REX.W. Quadword general-purpose registers are: RAX, RBX, RCX, RDX, RDI,
// RSI, RBP, RSP, R8–R15; these are available only in 64-bit mode. The contents of memory are found at the
// address provided by the effective address computation.
using rm64 = Alt<r64, m64>;
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
// ne_byte_rgs_with_rex — Byte registers that are not encodable when a REX prefix is present.
using ne_byte_regs_with_rex = Alt<
    r<RI::Rah>,
    r<RI::Rch>,
    r<RI::Rdh>,
    r<RI::Rbh>
>;
// byte_regs_requiring_rex — Byte Registers that require a rex prefix to be encoded.
using byte_regs_requiring_rex = Alt<
    r<BW::B8, RI::Rsp>, r<BW::B8, RI::Rbp>,
    r<BW::B8, RI::Rsi>, r<BW::B8, RI::Rdi>
>;
// regs_requiring_rex — Registers that require a REX prefix to be encoded.
using regs_requiring_rex = Alt<
    r<BW::B8, RI::Rsp>, r<BW::B8, RI::Rbp>,
    r<BW::B8, RI::Rsi>, r<BW::B8, RI::Rdi>,

    r<RI::R8>, r<RI::R9>, r<RI::R10>,
    r<RI::R11>, r<RI::R12>, r<RI::R13>,
    r<RI::R14>, r<RI::R15>,

    r<RI::Cr8>, r<RI::Cr9>, r<RI::Cr10>,
    r<RI::Cr11>, r<RI::Cr12>, r<RI::Cr13>,
    r<RI::Cr14>, r<RI::Cr15>,

    r<RI::Dbg8>, r<RI::Dbg9>, r<RI::Dbg10>,
    r<RI::Dbg11>, r<RI::Dbg12>, r<RI::Dbg13>,
    r<RI::Dbg14>, r<RI::Dbg15>
>;

// gprs_requiring_rex — General purpose registers that require a REX prefix to be encoded.
using gprs_requiring_rex = Alt<
    r<BW::B8, RI::Rsp>, r<BW::B8, RI::Rbp>,
    r<BW::B8, RI::Rsi>, r<BW::B8, RI::Rdi>,

    r<RI::R8>, r<RI::R9>, r<RI::R10>,
    r<RI::R11>, r<RI::R12>, r<RI::R13>,
    r<RI::R14>, r<RI::R15>
>;
// m_requiring_rex — Memory references that require a REX prefix to be encoded.
using m_requiring_rex = Alt<
    m<gprs_requiring_rex{}, any{}>,
    m<any{}, gprs_requiring_rex{}>
>;
// rm_requiring_rex — Either a general purpose register or a memory operand requiring a REX prefix to be encoded.
using rm_requiring_rex = Alt<gprs_requiring_rex, m_requiring_rex>;

}  // namespace fiska::x86::patterns

#endif  // __X86_ASSEMBLER_LIB_X86_PATTERNS_HH__
