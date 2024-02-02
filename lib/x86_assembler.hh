#ifndef __X86_ASSEMBLER_LIB_X86_ASSEMBLER_HH__
#define __X86_ASSEMBLER_LIB_X86_ASSEMBLER_HH__

#include "lib/core.hh"
#include "lib/x86_patterns.hh"
#include "lib/x86_utils.hh"

namespace fiska::x86 {

// Instruction Buffer
struct InstructionBuf {
    u8 buf_[/*max instruction length=*/15]{};
    u8 sz_{};

    static constexpr u8 kb16_opsz_prefix = 0x66;

    void append(u8 byte);
    void append(BW bw, u64 qword);
    void append_opcode(u64 opcode);

    auto empty() -> i1 { return sz_ == 0; }
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
    static constexpr auto has_rex_w(X86Op::ListRef op_list) -> i1 { return +with_rex_w; }

    static constexpr auto has_b16_opsz_override(X86Op::ListRef op_list) -> i1 { return +with_b16_opsz_override; }

    static constexpr auto match(X86Op::ListRef op_list) -> i1 {
        if (sizeof...(OpClass) != op_list.size()) { return false; }

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
    static constexpr auto has_rex_w(X86Op::ListRef op_list) -> i1 {
        return ((Pattern::match(op_list) and Pattern::has_rex_w(op_list)) or ...);
    }

    static constexpr auto has_b16_opsz_override(X86Op::ListRef op_list) -> i1 {
        return ((Pattern::match(op_list) and Pattern::has_b16_opsz_override(op_list)) or ...);
    }

    static constexpr auto match(X86Op::ListRef op_list) -> i1 {
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
// Emitters templated on the Instruction encoding format.
//====================================================================
// Supress warnings from bit-fields narrowings. We are not aware of 
// any way to supress them other than removing the -Wconversion flag
// from the code in question.
GCC_DIAG_IGNORE_PUSH(-Wconversion)

template <OpEn encoding>
struct Emitter;

template <>
struct Emitter<OpEn::MR> {
    static constexpr auto emit(u64 opcode, X86Op::ListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
        assert(op_list.size() == 2 and (op_list[0].is<Reg, Mem>() and op_list[1].is<Reg>()));

        X86Op::Ref rm = op_list[0];
        X86Op::Ref r = op_list[1];

        ModRM modrm {
            .rm = rm.modrm_encoding(),
            .reg = r.modrm_encoding(),
            .mod = rm.is<Mem>() ? rm.as<Mem>().mod_ : Mem::kmod_reg
        };

        Rex rex {
            .b = rm.is<Reg>() and rm.as<Reg>().requires_ext(),
            .x = 0,
            .r = r.as<Reg>().requires_ext(),
            .w = has_rex_w,
        };
        if (rm.is<Mem>()) {
            rex.b = rm.as<Mem>().base_reg_ and rm.as<Mem>().base_reg_->requires_ext();
            rex.x = rm.as<Mem>().index_reg_ and rm.as<Mem>().index_reg_->requires_ext();
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
    static constexpr auto emit(u64 opcode, X86Op::ListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
        assert(op_list.size() == 2 and (op_list[1].is<Reg, Mem>() and op_list[0].is<Reg>()));

        // Reverse the operators and use the same emitter from OpEn::MR.
        X86Op::List reversed_op_list(op_list.rbegin(), op_list.rend());

        return Emitter<OpEn::MR>::emit(opcode, reversed_op_list, has_rex_w, has_b16_opsz_override);
    }
};

template <>
struct Emitter<OpEn::FD> {
    static constexpr auto emit(u64 opcode, X86Op::ListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
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
    static constexpr auto emit(u64 opcode, X86Op::ListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
        assert(op_list.size() == 2 and (op_list[1].is<Reg>() and op_list[0].is<Moffs>()));

        // Reverse the operators and use the same emitter from OpEn::MR.
        X86Op::List reversed_op_list(op_list.rbegin(), op_list.rend());

        return Emitter<OpEn::FD>::emit(opcode, reversed_op_list, has_rex_w, has_b16_opsz_override);
    }
};

template <>
struct Emitter<OpEn::OI> {
    static constexpr auto emit(u64 opcode, X86Op::ListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
        assert(op_list.size() == 2 and (op_list[0].is<Reg>() and op_list[1].is<Imm>()));

        const Reg& reg = op_list[0].as<Reg>();
        const Imm& imm = op_list[1].as<Imm>();

        Rex rex {
            .b = reg.requires_ext(),
            .w = has_rex_w
        };
        rex.required = rex.w or rex.b or patterns::byte_regs_requiring_rex::match(op_list[0]);

        // Add the register index in the opcode. 
        // It is a bit awkward since we chose to pack all the bytes of an 
        // opcode into one single quadword instead of storing them in say a vector.
        if (utils::fits_in_u8(i64(opcode))) {
            opcode |= reg.index();
        } else if (utils::fits_in_u16(i64(opcode))) {
            opcode |= u64(reg.index() << 8);
        } else if (utils::fits_in_u24(i64(opcode))) {
            opcode |= u64(reg.index() << 16);
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

template <>
struct Emitter<OpEn::MI> {
    static constexpr auto emit(u64 opcode, X86Op::ListRef op_list, i1 has_rex_w, i1 has_b16_opsz_override) -> InstructionBuf {
        assert(op_list.size() == 2 and (op_list[0].is<Reg, Mem>() and op_list[1].is<Imm>()));

        X86Op::Ref rm = op_list[0];

        ModRM modrm {
            .rm = rm.modrm_encoding(),
            .mod = rm.is<Mem>() ? rm.as<Mem>().mod_ : Mem::kmod_reg
        };

        Rex rex {
            .b = rm.is<Reg>() and rm.as<Reg>().requires_ext(),
            .w = has_rex_w
        };
        if (rm.is<Mem>()) {
            rex.b = rm.as<Mem>().base_reg_ and rm.as<Mem>().base_reg_->requires_ext();
            rex.x = rm.as<Mem>().index_reg_ and rm.as<Mem>().index_reg_->requires_ext();
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
            out.append(rm.as<Mem>().sib_.value());
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

template <typename... InstrExpr>
struct InstrExprList {
    static constexpr auto match(X86Op::ListRef op_list) -> i1 {
        return ((InstrExpr::pattern::match(op_list)) or ...);
    }

    static constexpr auto emit(X86Op::ListRef op_list) -> InstructionBuf {
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
