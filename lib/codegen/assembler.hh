#ifndef __X86_LIB_CODEGEN_ASSEMBLER_HH__
#define __X86_LIB_CODEGEN_ASSEMBLER_HH__

#include "lib/support/core.hh"
#include "lib/codegen/patterns.hh"
#include "lib/x86/common.hh"

#include <bit>

namespace fiska::x86::codegen {

// Instruction operand encoding.
enum struct OpEn {
    MR,
    RM,
    FD,
    TD,
    OI,
    MI,
    I,
    ZO,
};

//=====================================================================================================================
// Opcode type.
//=====================================================================================================================
template <u8... byte>
requires (sizeof...(byte) <= 3)
struct OpCode {
    static constexpr auto value() -> u64 {
        constexpr std::array bytes = {byte...};

        switch (sizeof...(byte)) {
        case 1: return bytes[0];
        case 2: return bytes[0] | bytes[1] << 8;
        case 3: return bytes[0] | bytes[1] << 8 | bytes[2] << 16;
        default: unreachable();
        }
        unreachable();
    }
};

//=====================================================================================================================
// Instruction Buffer containing the encoding of a given x86 instruction.
//=====================================================================================================================
struct InstrBuf {
    using Storage = ByteVec;
    Storage storage_ = Storage(IRX86Op::kMaxInstrLength, 0);

    auto curr_off() -> u8 { return u8(storage_.size()); }

    auto add(u64 qword) -> InstrBuf& {
        if (std::bit_width(qword) <= 8) {
            add(BW::B8, qword);
        } else if (std::bit_width(qword) <= 16) {
            add(BW::B16, qword);
        } else if (std::bit_width(qword) <= 24) {
            add(BW::B24, qword);
        } else if (std::bit_width(qword) <= 32) {
            add(BW::B32, qword);
        } else {
            add(BW::B64, qword);
        }
        return *this;
    }

    auto add(BW bw, u64 qword) -> InstrBuf& {
        switch (bw) {
        case BW::Invalid:
            break;

        case BW::B8:
            storage_.push_back(u8(qword >> 0) & 0xff);
            break;
        case BW::B16:
            storage_.push_back(u8(qword >> 0) & 0xff);
            storage_.push_back(u8(qword >> 8) & 0xff);
            break;
        case BW::B24:
            storage_.push_back(u8(qword >> 0) & 0xff);
            storage_.push_back(u8(qword >> 8) & 0xff);
            storage_.push_back(u8(qword >> 16) & 0xff);
            break;
        case BW::B32:
            storage_.push_back(u8(qword >> 0) & 0xff);
            storage_.push_back(u8(qword >> 8) & 0xff);
            storage_.push_back(u8(qword >> 16) & 0xff);
            storage_.push_back(u8(qword >> 24) & 0xff);
            break;
        case BW::B64:
            storage_.push_back(u8(qword >> 0) & 0xff);
            storage_.push_back(u8(qword >> 8) & 0xff);
            storage_.push_back(u8(qword >> 16) & 0xff);
            storage_.push_back(u8(qword >> 24) & 0xff);
            storage_.push_back(u8(qword >> 32) & 0xff);
            storage_.push_back(u8(qword >> 40) & 0xff);
            storage_.push_back(u8(qword >> 48) & 0xff);
            storage_.push_back(u8(qword >> 56) & 0xff);
            break;
        } // switch
        return *this;
    }
};

//=====================================================================================================================
// [[intel]]
// /digit — A digit between 0 and 7 indicates that the ModR/M byte of the instruction uses only the r/m (register
// or memory) operand. The reg field contains the digit that provides an extension to the instruction's opcode
//=====================================================================================================================
enum struct SlashDigit : u8 {
    Zero = 0,
    One = 1,
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7
};

//=====================================================================================================================
// Emitters templated on the Instruction encoding format.
//=====================================================================================================================
// Supress warnings from bit-fields narrowings. We are not aware of 
// any way to supress them other than removing the -Wconversion flag
// from the code in question.
GCC_DIAG_IGNORE_PUSH(-Wconversion)

template <OpEn encoding, auto... args>
struct Emitter;

template <>
struct Emitter<OpEn::MR> {
    static auto emit(u64 opcode, IRX86Op::ListRef ops, OpSz opsz) -> ByteVec {
        assert(ops.size() == 2 and (ops[0].rm() and ops[1].r()));

        IRX86Op::Ref rm = ops[0];
        IRX86Op::Ref r = ops[1];

        ModRM modrm {
            .rm = rm.ndx(),
            .reg = r.ndx(),
            .mod = rm.m() ? rm.as_m().mod() : IRX86Op::kModReg
        };

        Rex rex {
            .b = rm.r() and rm.as_r().need_ext(),
            .x = 0,
            .r = r.as_r().need_ext(),
            .w = opsz == OpSz::B64,
        };
        if (rm.m()) {
            rex.b = IRReg::need_ext(rm.as_m().brid_);
            rex.x = IRReg::need_ext(rm.as_m().irid_);
        }

        i1 rex_required = rex.has_set_bits()
            or byte_regs_requiring_rex::match(rm)
            or byte_regs_requiring_rex::match(r);

        InstrBuf buf;
        // Operand size override to 16-bits. We always assume we are in 64-bit mode.
        if (opsz == OpSz::B16) {
            buf.add(BW::B8, IRX86Op::k16bitOpSzOverridePrefix);
        }

        if (rex_required) {
            buf.add(BW::B8, rex.raw());
        }

        buf.add(opcode)
           .add(BW::B8, modrm.raw());

        if (rm.m() and rm.as_m().need_sib()) {
            buf.add(BW::B8, rm.as_m().sib());
        }

        if (rm.m() and rm.as_m().disp_bw() != BW::Invalid) {
            rm.reloc_info_.instr_offset_ = buf.curr_off();
            buf.add(rm.as_m().disp_bw(), u64(rm.as_m().disp_));
        }

        return buf.storage_;
    }
};

template <>
struct Emitter<OpEn::RM> {
    static auto emit(u64 opcode, IRX86Op::ListRef ops, OpSz opsz) -> ByteVec {
        assert(ops.size() == 2 and (ops[1].rm() and ops[0].r()));

        // Reverse the operators and use the same emitter from OpEn::MR.
        return Emitter<OpEn::MR>::emit(opcode, {ops[1], ops[0]}, opsz);
    }
};

template <>
struct Emitter<OpEn::FD> {
    static auto emit(u64 opcode, IRX86Op::ListRef ops, OpSz opsz) -> ByteVec {
        assert(ops.size() == 2 and (ops[0].r() and ops[1].mo()));

        Rex rex {
            .w = opsz == OpSz::B64
        };
        i1 rex_required = rex.has_set_bits();

        InstrBuf buf;
        // Operand size override to 16-bits. We always assume we are in 64-bit mode.
        if (opsz == OpSz::B16) {
            buf.add(BW::B8, IRX86Op::k16bitOpSzOverridePrefix);
        }

        if (rex_required) {
            buf.add(BW::B8, rex.raw());
        }

        buf.add(opcode);
        ops[1].reloc_info_.instr_offset_ = buf.curr_off();
        buf.add(BW::B64, u64(ops[1].as_mo().addr_));

        return buf.storage_;
    }
};

template <>
struct Emitter<OpEn::TD> {
    static auto emit(u64 opcode, IRX86Op::ListRef ops, OpSz opsz) -> ByteVec {
        assert(ops.size() == 2 and (ops[1].r() and ops[0].mo()));

        // Reverse the operators and use the same emitter from OpEn::FD.
        return Emitter<OpEn::FD>::emit(opcode, {ops[1], ops[0]}, opsz);
    }
};

template <>
struct Emitter<OpEn::OI> {
    static auto emit(u64 opcode, IRX86Op::ListRef ops, OpSz opsz) -> ByteVec {
        assert(ops.size() == 2 and (ops[0].r() and ops[1].i()));

        IRX86Op::Ref reg = ops[0];
        IRX86Op::Ref imm = ops[1];

        Rex rex {
            .b = reg.as_r().need_ext(),
            .w = opsz == OpSz::B64 
        };
        i1 rex_required = rex.has_set_bits() or byte_regs_requiring_rex::match(reg);

        // Add the register index in the opcode. 
        // It is a bit awkward since we chose to pack all the bytes of an 
        // opcode into one single quadword instead of storing them in say a vector.
        if (std::bit_width(opcode) <= 8) {
            opcode |= reg.ndx();
        } else if (std::bit_width(opcode) <= 16) {
            opcode |= u64(reg.ndx() << 8);
        } else if (std::bit_width(opcode) <= 24) {
            opcode |= u64(reg.ndx() << 16);
        } else {
            unreachable();
        }

        InstrBuf buf;
        // Operand size override to 16-bits. We always assume we are in 64-bit mode.
        if (opsz == OpSz::B16) {
            buf.add(BW::B8, IRX86Op::k16bitOpSzOverridePrefix);
        }

        if (rex_required) {
            buf.add(BW::B8, rex.raw());
        }

        buf.add(opcode);
        imm.reloc_info_.instr_offset_ = buf.curr_off();
        buf.add(imm.as_i().bw_, u64(imm.as_i().value_));

        return buf.storage_;
    }
};

template <SlashDigit slash_digit>
struct Emitter<OpEn::MI, slash_digit> {
    static auto emit(u64 opcode, IRX86Op::ListRef ops, OpSz opsz) -> ByteVec {
        assert(ops.size() == 2 and (ops[0].rm() and ops[1].i()));

        IRX86Op::Ref rm = ops[0];

        ModRM modrm {
            .rm = rm.ndx(),
            .reg = +slash_digit,
            .mod = rm.m() ? rm.as_m().mod() : IRX86Op::kModReg
        };

        Rex rex {
            .b = rm.r() and rm.as_r().need_ext(),
            .w = opsz == OpSz::B64 
        };

        if (rm.m()) {
            rex.b = IRReg::need_ext(rm.as_m().brid_);
            rex.x = IRReg::need_ext(rm.as_m().irid_);
        }
        i1 rex_required = rex.has_set_bits() or byte_regs_requiring_rex::match(rm);

        InstrBuf buf;
        // Operand size override to 16-bits. We always assume we are in 64-bit mode.
        if (opsz == OpSz::B16) {
            buf.add(BW::B8, IRX86Op::k16bitOpSzOverridePrefix);
        }
        if (rex_required) {
            buf.add(BW::B8, rex.raw());
        }

        buf.add(opcode)
           .add(BW::B8, modrm.raw());

        if (rm.m() and rm.as_m().need_sib()) {
            buf.add(BW::B8, rm.as_m().sib());
        }

        if (rm.m() and rm.as_m().disp_bw() != BW::Invalid) {
            rm.reloc_info_.instr_offset_ = buf.curr_off();
            buf.add(rm.as_m().disp_bw(), u64(rm.as_m().disp_));
        }
        // Immediate
        ops[1].reloc_info_.instr_offset_ = buf.curr_off();
        buf.add(ops[1].as_i().bw_, u64(ops[1].as_i().value_));
        return buf.storage_;
    }
};

template <>
struct Emitter<OpEn::I> {
    static auto emit(u64 opcode, IRX86Op::ListRef ops, OpSz opsz) -> ByteVec {
        assert(ops.size() == 2 and (ops[0].r() and ops[1].i()));

        Rex rex {
            .w = opsz == OpSz::B64
        };
        i1 rex_required = rex.has_set_bits() or byte_regs_requiring_rex::match(ops[0]);

        InstrBuf buf;
        // Operand size override to 16-bits. We always assume we are in 64-bit mode.
        if (opsz == OpSz::B16) {
            buf.add(BW::B8, IRX86Op::k16bitOpSzOverridePrefix);
        }
        if (rex_required) {
            buf.add(BW::B8, rex.raw());
        }

        buf.add(opcode);
        ops[1].reloc_info_.instr_offset_ = buf.curr_off();
        buf.add(ops[1].as_i().bw_, u64(ops[1].as_i().value_));
        return buf.storage_;
    }
};

template <>
struct Emitter<OpEn::ZO> {
    static auto emit(u64 opcode, IRX86Op::ListRef ops, OpSz opsz) -> ByteVec {
        assert(ops.empty());
        return InstrBuf{}.add(opcode).storage_;
    }
};

//=====================================================================================================================
// Instruction Expression.
// Each instruction expression holds the opcode alongside the pattern
// as well as the instruction encoding format.
//=====================================================================================================================
template <typename OpCode, IsX86PatternClass Pattern, typename Emitter>
struct InstrExpr {
    using pattern = Pattern;
    using opcode = OpCode;
    using emitter = Emitter;
};

template <X86Mnemonic mnemonic, typename... InstrExpr>
struct InstrExprList {
    static constexpr X86Mnemonic mmic = mnemonic;

    // TODO(miloudi): Instead of returning true and or false return a tuple called MatchInfo containing
    // the rex flags and the op size override.
    // X86Ops should be passed by Refs. and when emitting the instruction All x86Ops should contain the patch offset
    // so that we know how to patch the op should it need to be patched.
    static auto match(IRX86Op::ListRef ops) -> i1 {
        return (std::get<Pat<>::match_idx>(InstrExpr::pattern::match(ops)) or ...);
    }

    static auto emit(IRX86Op::ListRef ops) -> ByteVec {
        ByteVec ret;

        auto found_match = [&]<typename InstructionExpr>() {
            auto [is_match, opsz] = InstructionExpr::pattern::match(ops);
            if (is_match) {
                ret = InstructionExpr::emitter::emit(
                    InstructionExpr::opcode::value(), ops, opsz);
            }
            return is_match;
        };

        std::ignore = (found_match.template operator()<InstrExpr>() or ...);
        return ret;
    }
};

}  // namespace fiska::x86::codegen

#endif // __X86_LIB_CODEGEN_ASSEMBLER_HH__
