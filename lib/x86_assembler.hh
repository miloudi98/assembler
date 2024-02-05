#ifndef __X86_ASSEMBLER_LIB_X86_ASSEMBLER_HH__
#define __X86_ASSEMBLER_LIB_X86_ASSEMBLER_HH__

#include "lib/core.hh"
#include "lib/x86_patterns.hh"
#include "lib/x86_utils.hh"

namespace fiska::x86 {

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

// Instruction Buffer
struct InstructionBuf {
    u8 buf_[/*max instruction length=*/15]{};
    u8 sz_{};

    // TODO(miloudi): This doens't belong here.
    static constexpr u8 kb16_opsz_prefix = 0x66;

    void append(u8 byte) {
        buf_[sz_++] = byte;
    }

    void append(BW bw, u64 qword) {
        switch (bw) {
        case BW::B0:
            break;
        case BW::B8: {
            append(u8(qword >> 0) & 0xff);
            break;
        }
        case BW::B16: {
            append(u8(qword >> 0) & 0xff);
            append(u8(qword >> 8) & 0xff);
            break;
        }
        case BW::B24: {
            append(u8(qword >> 0) & 0xff);
            append(u8(qword >> 8) & 0xff);
            append(u8(qword >> 16) & 0xff);
            break;
        }
        case BW::B32: {
            append(u8(qword >> 0) & 0xff);
            append(u8(qword >> 8) & 0xff);
            append(u8(qword >> 16) & 0xff);
            append(u8(qword >> 24) & 0xff);
            break;
        }
        case BW::B64: {
            append(u8(qword >> 0) & 0xff);
            append(u8(qword >> 8) & 0xff);
            append(u8(qword >> 16) & 0xff);
            append(u8(qword >> 24) & 0xff);
            append(u8(qword >> 32) & 0xff);
            append(u8(qword >> 40) & 0xff);
            append(u8(qword >> 48) & 0xff);
            append(u8(qword >> 56) & 0xff);
            break;
        }
        } // switch
    }

    void append_opcode(u64 opcode) {
        if (utils::fits_in_u8(i64(opcode))) {
            append(BW::B8, opcode);

        } else if(utils::fits_in_u16(i64(opcode))) {
            append(BW::B16, opcode);

        } else if (utils::fits_in_u24(i64(opcode))) {
            append(BW::B24, opcode);
        } else {
            unreachable();
        }
    }

    auto empty() -> i1 { return sz_ == 0; }

    auto operator==(const InstructionBuf& other) const -> i1 {
        return sz_ == other.sz_ and std::memcmp(buf_, other.buf_, sz_) == 0;
    }
};

// Base class for all patterns.
struct X86PatternClass {};

template <typename T>
concept IsX86PatternClass = std::derived_from<T, X86PatternClass>;

enum struct Rex_W : i1 {
    Yes = true,
    No = false
};

enum struct B16OpSz : i1 {
    Yes = true,
    No = false
};

//=====================================================
// X86 Operand patterns.
//=====================================================
template <Rex_W with_rex_w, B16OpSz with_b16_opsz_override, patterns::IsX86OpClass... OpClass> 
struct Pat : X86PatternClass {
    static constexpr auto has_rex_w(X86Instruction::OpListRef op_list) -> i1 { return +with_rex_w; }

    static constexpr auto has_b16_opsz_override(X86Instruction::OpListRef op_list) -> i1 { return +with_b16_opsz_override; }

    static constexpr auto match(X86Instruction::OpListRef op_list) -> i1 {
        if (sizeof...(OpClass) != op_list.size()) { return false; }
        if (sizeof...(OpClass) == 0) { return true; }

        u32 op_idx = 0;
        return (OpClass::match(op_list[op_idx++]) and ...);
    }

    static auto instances() -> Vec<X86Op::List> {
        Vec<X86Op::List> ret;

        auto is_illegal_pat = [&](X86Op::ListRef op_list) {
            return rgs::any_of(op_list, patterns::rm_requiring_rex::match)
                and rgs::any_of(op_list, patterns::ne_byte_regs_with_rex::match);
        };

        for (const auto& t : vws::cartesian_product(OpClass::instances()...)) {
            X86Op::List op_list = std::apply([](auto&&... args) { return X86Op::List{FWD(args)...}; } , t);
            if (is_illegal_pat(op_list)) { continue; }
            ret.push_back(std::move(op_list));
        }
        return ret;
    }
};

// Matches the list of x86 operand against multiple patterns.
template <IsX86PatternClass... Pattern>
struct Or : X86PatternClass {
    static constexpr auto has_rex_w(X86Instruction::OpListRef op_list) -> i1 {
        return ((Pattern::match(op_list) and Pattern::has_rex_w(op_list)) or ...);
    }

    static constexpr auto has_b16_opsz_override(X86Instruction::OpListRef op_list) -> i1 {
        return ((Pattern::match(op_list) and Pattern::has_b16_opsz_override(op_list)) or ...);
    }

    static constexpr auto match(X86Instruction::OpListRef op_list) -> i1 {
        return (Pattern::match(op_list) or ...);
    }

    static auto instances() -> Vec<X86Op::List> {
        Vec<X86Op::List> ret;

        auto add_pattern_instances = [&]<typename Pat>() {
            Vec<X86Op::List> pattern_instances = Pat::instances();
            ret.insert(ret.end(), pattern_instances.begin(), pattern_instances.end());
        };

        (add_pattern_instances.template operator()<Pattern>(), ...);
        return ret;
    }
};

//====================================================================
// [[intel]]
// /digit — A digit between 0 and 7 indicates that the ModR/M byte of the instruction uses only the r/m (register
// or memory) operand. The reg field contains the digit that provides an extension to the instruction's opcode
//====================================================================
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

//====================================================================
// Emitters templated on the Instruction encoding format.
//====================================================================
// Supress warnings from bit-fields narrowings. We are not aware of 
// any way to supress them other than removing the -Wconversion flag
// from the code in question.
GCC_DIAG_IGNORE_PUSH(-Wconversion)

template <OpEn encoding, auto... args>
struct Emitter;

template <>
struct Emitter<OpEn::MR> {
    static constexpr auto emit(u64 opcode, X86Instruction::OpListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
        assert(op_list.size() == 2 and (op_list[0].is<Reg, Mem>() and op_list[1].is<Reg>()));

        X86Op::Ref rm = op_list[0];
        X86Op::Ref r = op_list[1];

        ModRM modrm {
            .rm = rm.modrm_encoding(),
            .reg = r.modrm_encoding(),
            .mod = rm.is<Mem>() ? rm.as<Mem>().mod_ : Mem::kmod_reg
        };

        Rex rex {
            .b = rm.is<Reg>() and rm.as<Reg>().requires_ext_,
            .x = 0,
            .r = r.as<Reg>().requires_ext_,
            .w = has_rex_w,
        };
        if (rm.is<Mem>()) {
            rex.b = rm.as<Mem>().base_reg_ and rm.as<Mem>().base_reg_->requires_ext_;
            rex.x = rm.as<Mem>().index_reg_ and rm.as<Mem>().index_reg_->requires_ext_;
        }
        rex.required = rex.w or rex.b or rex.x or rex.r
            or patterns::byte_regs_requiring_rex::match(rm)
            or patterns::byte_regs_requiring_rex::match(r);

        InstructionBuf out{};

        // Operand size override to 16-bits. We always assume we are in 64-bit mode.
        if (has_b16_opsz_override) { out.append(InstructionBuf::kb16_opsz_prefix); }
            
        // Only output the first byte of Rex as the second byte only contains unrelated
        // metadata.
        if (rex.required) { out.append(BW::B8, rex.raw); }

        out.append_opcode(opcode);
        out.append(modrm.raw);

        // Sib byte.
        if (rm.is<Mem>() and rm.as<Mem>().sib_) {
            out.append(rm.as<Mem>().sib_.value());
        }

        // Displacement.
        if (rm.is<Mem>() and rm.as<Mem>().disp_bw_ != BW::B0) {
            out.append(rm.as<Mem>().disp_bw_, u64(rm.as<Mem>().disp_));
        }

        return out;
    }
};

template <>
struct Emitter<OpEn::RM> {
    static constexpr auto emit(u64 opcode, X86Instruction::OpListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
        assert(op_list.size() == 2 and (op_list[1].is<Reg, Mem>() and op_list[0].is<Reg>()));

        // Reverse the operators and use the same emitter from OpEn::MR.
        X86Instruction::OpList reversed_op_list = X86InstructionOperands({op_list[1], op_list[0]});

        return Emitter<OpEn::MR>::emit(opcode, reversed_op_list, has_rex_w, has_b16_opsz_override);
    }
};

template <>
struct Emitter<OpEn::FD> {
    static constexpr auto emit(u64 opcode, X86Instruction::OpListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
        assert(op_list.size() == 2 and (op_list[0].is<Reg>() and op_list[1].is<Moffs>()));

        Rex rex {
            .w = has_rex_w,
        };
        rex.required = rex.w;

        InstructionBuf out{};

        // Operand size override to 16-bits. We always assume we are in 64-bit mode.
        if (has_b16_opsz_override) { out.append(InstructionBuf::kb16_opsz_prefix); }

        // Only output the first byte of Rex as the second byte only contains unrelated
        // metadata.
        if (rex.required) { out.append(BW::B8, rex.raw); }

        out.append_opcode(opcode);
        out.append(BW::B64, u64(op_list[1].as<Moffs>().addr_));

        return out;
    }
};

template <>
struct Emitter<OpEn::TD> {
    static constexpr auto emit(u64 opcode, X86Instruction::OpListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
        assert(op_list.size() == 2 and (op_list[1].is<Reg>() and op_list[0].is<Moffs>()));

        // Reverse the operators and use the same emitter from OpEn::MR.
        X86Instruction::OpList reversed_op_list = X86InstructionOperands({op_list[1], op_list[0]});

        return Emitter<OpEn::FD>::emit(opcode, reversed_op_list, has_rex_w, has_b16_opsz_override);
    }
};

template <>
struct Emitter<OpEn::OI> {
    static constexpr auto emit(u64 opcode, X86Instruction::OpListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
        assert(op_list.size() == 2 and (op_list[0].is<Reg>() and op_list[1].is<Imm>()));

        const Reg& reg = op_list[0].as<Reg>();
        const Imm& imm = op_list[1].as<Imm>();

        Rex rex {
            .b = reg.requires_ext_,
            .w = has_rex_w
        };
        rex.required = rex.w or rex.b or patterns::byte_regs_requiring_rex::match(op_list[0]);

        // Add the register index in the opcode. 
        // It is a bit awkward since we chose to pack all the bytes of an 
        // opcode into one single quadword instead of storing them in say a vector.
        if (utils::fits_in_u8(i64(opcode))) {
            opcode |= reg.idx_;
        } else if (utils::fits_in_u16(i64(opcode))) {
            opcode |= u64(reg.idx_ << 8);
        } else if (utils::fits_in_u24(i64(opcode))) {
            opcode |= u64(reg.idx_ << 16);
        } else {
            unreachable();
        }

        InstructionBuf out{};

        // Operand size override to 16-bits. We always assume we are in 64-bit mode.
        if (has_b16_opsz_override) { out.append(InstructionBuf::kb16_opsz_prefix); }

        // Only output the first byte of Rex as the second byte only contains unrelated
        // metadata.
        if (rex.required) { out.append(BW::B8, rex.raw); }

        out.append_opcode(opcode);
        out.append(imm.bw_, u64(imm.value_));

        return out;
    }
};

template <SlashDigit slash_digit>
struct Emitter<OpEn::MI, slash_digit> {
    static constexpr auto emit(u64 opcode, X86Instruction::OpListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
        assert(op_list.size() == 2 and (op_list[0].is<Reg, Mem>() and op_list[1].is<Imm>()));

        X86Op::Ref rm = op_list[0];

        ModRM modrm {
            .rm = rm.modrm_encoding(),
            .reg = +slash_digit,
            .mod = rm.is<Mem>() ? rm.as<Mem>().mod_ : Mem::kmod_reg
        };

        Rex rex {
            .b = rm.is<Reg>() and rm.as<Reg>().requires_ext_,
            .w = has_rex_w
        };
        if (rm.is<Mem>()) {
            rex.b = rm.as<Mem>().base_reg_ and rm.as<Mem>().base_reg_->requires_ext_;
            rex.x = rm.as<Mem>().index_reg_ and rm.as<Mem>().index_reg_->requires_ext_;
        }
        rex.required = rex.w or rex.b or rex.x or patterns::byte_regs_requiring_rex::match(rm);

        InstructionBuf out{};

        // Operand size override to 16-bits. We always assume we are in 64-bit mode.
        if (has_b16_opsz_override) { out.append(InstructionBuf::kb16_opsz_prefix); }

        // Only output the first byte of Rex as the second byte only contains unrelated
        // metadata.
        if (rex.required) { out.append(BW::B8, rex.raw); }

        out.append_opcode(opcode);
        out.append(modrm.raw);

        // Sib byte.
        if (rm.is<Mem>() and rm.as<Mem>().sib_) {
            out.append(*rm.as<Mem>().sib_);
        }

        // Displacement.
        if (rm.is<Mem>() and rm.as<Mem>().disp_bw_ != BW::B0) {
            out.append(rm.as<Mem>().disp_bw_, u64(rm.as<Mem>().disp_));
        }

        // Immediate
        out.append(op_list[1].as<Imm>().bw_, u64(op_list[1].as<Imm>().value_));

        return out;
    }
};

template <>
struct Emitter<OpEn::I> {
    static constexpr auto emit(u64 opcode, X86Instruction::OpListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
        assert(op_list.size() == 2 and (op_list[0].is<Reg>() and op_list[1].is<Imm>()));

        Rex rex {
            .w = has_rex_w
        };
        rex.required = rex.w or patterns::byte_regs_requiring_rex::match(op_list[0]);

        InstructionBuf out{};

        // Operand size override to 16-bits. We always assume we are in 64-bit mode.
        if (has_b16_opsz_override) { out.append(InstructionBuf::kb16_opsz_prefix); }
        // Only output the first byte of Rex as the second byte only contains unrelated
        // metadata.
        if (rex.required) { out.append(BW::B8, rex.raw); }

        out.append_opcode(opcode);
        out.append(op_list[1].as<Imm>().bw_, u64(op_list[1].as<Imm>().value_));

        return out;
    }
};

template <>
struct Emitter<OpEn::ZO> {
    static constexpr auto emit(u64 opcode, X86Instruction::OpListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
        assert(op_list.empty());

        InstructionBuf out{};
        out.append_opcode(opcode);
        return out;
    }
};

GCC_DIAG_IGNORE_POP();

//====================================================================
// Opcode type.
//====================================================================
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

//====================================================================
// Instruction Expression.
// Each instruction expression holds the opcode alongside the pattern
// as well as the instruction encoding format.
//====================================================================
template <typename OpCode, IsX86PatternClass Pattern, typename Emitter>
struct InstrExpr {
    using pattern = Pattern;
    using opcode = OpCode;
    using emitter = Emitter;
};

template <X86IK instruction_kind, typename... InstrExpr>
struct InstrExprList {
    static constexpr X86IK ik = instruction_kind;

    static constexpr auto match(X86Instruction::OpListRef op_list) -> i1 {
        return ((InstrExpr::pattern::match(op_list)) or ...);
    }

    static constexpr auto emit(X86Instruction::OpListRef op_list) -> InstructionBuf {
        InstructionBuf out{};

        auto found_match = [&]<typename InstructionExpr>() {
            if (InstructionExpr::pattern::match(op_list)) {
                out = InstructionExpr::emitter::emit(
                    InstructionExpr::opcode::value(),
                    op_list,
                    InstructionExpr::pattern::has_rex_w(op_list),
                    InstructionExpr::pattern::has_b16_opsz_override(op_list)
                );
                return true;
            }
            return false;
        };

        std::ignore = (found_match.template operator()<InstrExpr>() or ...);
        return out;
    }

    static auto instances() -> Vec<X86Op::List> {
        return Or<typename InstrExpr::pattern...>::instances();
    }
};

} // namespace fiska

#endif  // __X86_ASSEMBLER_LIB_X86_ASSEMBLER_HH__
