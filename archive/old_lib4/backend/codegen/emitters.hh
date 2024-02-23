#ifndef __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_EMITTERS_HH__
#define __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_EMITTERS_HH__

#include "lib/common/base.hh"
#include "lib/common/x86/types.hh"
#include "lib/common/elf.hh"
#include "lib/backend/ir.hh"
#include "lib/backend/codegen/patterns.hh"

namespace fiska::assembler::backend {
//=====================================================================================================================
// X86 op class pattern identifier concept.
//=====================================================================================================================
template <typename T>
struct is_x86_op_class_pattern_t : std::false_type {};

template <typename T>
concept is_x86_op_class_pattern = is_x86_op_class_pattern_t<T>::value;

template <OpSz opsz, is_x86_op_class... IROpClass>
struct Pat;

template <OpSz opsz, is_x86_op_class... IROpClass>
struct is_x86_op_class_pattern_t<Pat<opsz, IROpClass...>> : std::true_type {};

template <is_x86_op_class_pattern... Pattern>
struct Or;

template <is_x86_op_class_pattern... Pattern>
struct is_x86_op_class_pattern_t<Or<Pattern...>> : std::true_type {};

// X86 operand patterns.
template <OpSz opsz = OpSz::Default, is_x86_op_class... IROpClass>
struct Pat {
    static auto match(IRX86Op::ListRef ops) -> Pair<i1, OpSz> {
        u8 op_idx = 0;
        i1 success = (sizeof...(IROpClass) == 0) or 
            (sizeof...(IROpClass) == ops.size() and (IROpClass::match(ops[op_idx++]) and ...));

        return { success, opsz };
    }
};

template <is_x86_op_class_pattern... Pattern>
struct Or {
    static auto match(IRX86Op::ListRef ops) -> Pair<i1, OpSz> {
        OpSz matched_opsz{};

        auto pattern_match = [&]<typename Pat>() {
            auto [success, opsz] = Pat::match(ops);
            matched_opsz = opsz;
            return success;
        };

        i1 success = (pattern_match.template operator()<Pattern>() or ...);
        return { success, matched_opsz };
    }
};

namespace detail {

template <is_x86_op_class... IROpClass>
auto match(IRX86Op::ListRef ops) -> i1 {
    u8 op_idx = 0;
    return (sizeof...(IROpClass) == 0) or 
           (sizeof...(IROpClass) == ops.size() and (IROpClass::match(ops[op_idx++]) and ...));
}

template <is_x86_op_class... IROpClass>
auto match_any(std::same_as<IRX86Op::Ref> auto&&... ops) -> i1 {
    auto match_op = [&]<typename OpClass>() -> i1 {
        return (OpClass::match(FWD(ops)) or ...);
    };
    return (match_op.template operator()<IROpClass>() or ...);
}

} // namespace detail

// Bytecode containing details for potential patching.
struct X86ByteCode {
    struct Relocation {
        StrRef symbol_name_;
        i64 addend_{};
        u8 type_{};
        u8 i_offset_{};

        auto valid() const -> i1 { return not symbol_name_.empty(); }
    };

    u8 bytecode_[X86Info::kMaxInstrLength]{};
    u8 bytecode_sz_{};
    Vec<Relocation> rels_;

    auto begin() const -> const u8* { return bytecode_; }
    auto end() const -> const u8* { return bytecode_ + size(); }
    auto size() const -> u8 { return bytecode_sz_; }
    auto empty() const -> i1 { return bytecode_sz_ == 0; }
    auto add(BW bw, u64 qword) -> void;
    auto add(u64 qword) -> void;
    auto add_relocation(IRX86Op::Ref op) -> void;
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
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode {
        assert(( detail::match<rm, r<>>(ops) ));

        IRX86Op::Ref rm = ops[0];
        IRX86Op::Ref r = ops[1];

        ModRM modrm {
            .rm = rm.ndx(),
            .reg = r.ndx(),
            .mod = rm.mod()
        };

        Rex rex {
            .b = X86Info::register_req_ext(rm.r() ? rm.as_r().ri_ : rm.as_m().bri_),
            .x = rm.m() and X86Info::register_req_ext(rm.as_m().iri_),
            .r = r.as_r().req_ext(),
            .w = opsz == OpSz::B64
        };

        X86ByteCode out{};
        if (opsz == OpSz::B16) {
            out.add(BW::B8, X86Info::k16bitOpSzOverridePrefix);
        }
        if (rex.has_set_bits() or detail::match_any<rexophile_byte_regs>(rm, r)) {
            out.add(BW::B8, rex.raw());
        }
        out.add(opcode);
        out.add(BW::B8, modrm.raw());

        if (rm.m() and rm.as_m().need_sib()) {
            out.add(BW::B8, rm.as_m().sib());
        }

        if (rm.m() and rm.as_m().disp_bw() != BW::Invalid) {
            if (rm.req_reloc()) {
                out.add_relocation(rm);
            }
            out.add(rm.as_m().disp_bw(), rm.req_reloc() * u64(rm.as_m().disp_.addend_));
        }

        return out;
    }
};

template <>
struct Emitter<OpEn::RM> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode {
        assert(( detail::match<r<>, rm>(ops) ));
        // Reverse the operators and use the same emitter from OpEn::MR.
        return Emitter<OpEn::MR>::emit(opcode, {ops[1], ops[0]}, opsz);
    }
};

template <>
struct Emitter<OpEn::FD> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode {
        assert(( detail::match<r<>, mo<>>(ops) ));

        IRX86Op::Ref mo = ops[1];

        Rex rex {
            .w = opsz == OpSz::B64
        };

        X86ByteCode out{};
        if (opsz == OpSz::B16) {
            out.add(BW::B8, X86Info::k16bitOpSzOverridePrefix);
        }
        if (rex.has_set_bits()) {
            out.add(BW::B8, rex.raw());
        }
        out.add(opcode);

        if (mo.req_reloc()) {
            out.add_relocation(mo);
        }
        out.add(BW::B64, mo.req_reloc() * u64(mo.as_mo().addr_.addend_));
        return out;
    }
};

template <>
struct Emitter<OpEn::TD> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode {
        assert(( detail::match<mo<>, r<>>(ops) ));
        // Reverse the operators and use the same emitter from OpEn::FD.
        return Emitter<OpEn::FD>::emit(opcode, {ops[1], ops[0]}, opsz);
    }
};

template <>
struct Emitter<OpEn::OI> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode {
        assert(( detail::match<r<>, i<>>(ops) ));

        IRX86Op::Ref reg = ops[0];
        IRX86Op::Ref imm = ops[1];

        Rex rex {
            .b = reg.as_r().req_ext(),
            .w = opsz == OpSz::B64 
        };

        // Include the register index in the opcode. 
        // It is a bit awkward since we chose to pack all the bytes of the 
        // opcode into one single quadword instead of storing them in a collection that we
        // can index into.
        //
        // 1 byte opcode.
        if (std::bit_width(opcode) <= 8) {
            opcode |= reg.ndx();

        // 2 byte opcode.
        } else if (std::bit_width(opcode) <= 16) {
            opcode |= u64(reg.ndx() << 8);

        // 3 byte opcode.
        } else if (std::bit_width(opcode) <= 24) {
            opcode |= u64(reg.ndx() << 16);

        // An opcode can not exceed 3 bytes in size.
        } else {
            unreachable();
        }

        X86ByteCode out{};
        // Operand size override to 16-bits. We always assume we are in 64-bit mode.
        if (opsz == OpSz::B16) {
            out.add(BW::B8, X86Info::k16bitOpSzOverridePrefix);
        }
        if (rex.has_set_bits() or detail::match_any<rexophile_byte_regs>(reg)) {
            out.add(BW::B8, rex.raw());
        }
        out.add(opcode);
        if (imm.req_reloc()) {
            out.add_relocation(imm);
        }
        out.add(imm.as_i().bw_, imm.req_reloc() * u64(imm.as_i().value_.addend_));

        return out;
    }
};

template <SlashDigit slash_digit>
struct Emitter<OpEn::MI, slash_digit> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode {
        assert(( detail::match<rm, i<>>(ops) ));

        IRX86Op::Ref rm = ops[0];
        IRX86Op::Ref imm = ops[1];

        ModRM modrm {
            .rm = rm.ndx(),
            .reg = +slash_digit,
            .mod = rm.mod()
        };

        Rex rex {
            .b = X86Info::register_req_ext(rm.r() ? rm.as_r().ri_ : rm.as_m().bri_),
            .x = rm.m() and X86Info::register_req_ext(rm.as_m().iri_),
            .w = opsz == OpSz::B64 
        };

        X86ByteCode out{};
        if (opsz == OpSz::B16) {
            out.add(BW::B8, X86Info::k16bitOpSzOverridePrefix);
        }
        if (rex.has_set_bits() or detail::match_any<rexophile_byte_regs>(rm)) {
            out.add(BW::B8, rex.raw());
        }
        out.add(opcode);
        out.add(BW::B8, modrm.raw());

        if (rm.m() and rm.as_m().need_sib()) {
            out.add(BW::B8, rm.as_m().sib());
        }

        // Mem displacement.
        if (rm.m() and rm.as_m().disp_bw() != BW::Invalid) {
            if (rm.req_reloc()) {
                out.add_relocation(rm);
            }
            out.add(rm.as_m().disp_bw(), (not rm.req_reloc()) * u64(rm.as_m().disp_.addend_));
        }

        // Immediate.
        if (imm.req_reloc()) {
            out.add_relocation(imm);
        }
        // BUG: we should use |not operadn.req_reloc()| in our arithmetic and not the negation of that.
        // It's fixed here but this bug is still in some other parts of the emitter you need to fix it.
        out.add(imm.as_i().bw_, (not imm.req_reloc()) * u64(imm.as_i().value_.addend_));

        return out;
    }
};

template <>
struct Emitter<OpEn::I> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode {
        assert(( detail::match<r<>, i<>>(ops) ));

        IRX86Op::Ref r = ops[0];
        IRX86Op::Ref imm = ops[1];

        Rex rex {
            .w = opsz == OpSz::B64
        };

        X86ByteCode out{};
        if (opsz == OpSz::B16) {
            out.add(BW::B8, X86Info::k16bitOpSzOverridePrefix);
        }
        if (rex.has_set_bits() or detail::match_any<rexophile_byte_regs>(r)) {
            out.add(BW::B8, rex.raw());
        }
        out.add(opcode);

        // Immediate.
        if (imm.req_reloc()) {
            out.add_relocation(imm);
        }
        out.add(imm.as_i().bw_, (not imm.req_reloc()) * u64(imm.as_i().value_.addend_));

        return out;
    }
};

template <>
struct Emitter<OpEn::ZO> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode {
        assert(ops.empty());
        return X86ByteCode{};
    }
};

//=====================================================================================================================
// Emitter identifier concept.
//=====================================================================================================================
template <typename T>
struct is_emitter_t : std::false_type {};

template <OpEn encoding, auto... args> 
struct is_emitter_t<Emitter<encoding, args...>> : std::true_type {};

template <typename T>
concept is_emitter = is_emitter_t<T>::value;


//=====================================================================================================================
// Instruction Expression.
// Each instruction expression holds the opcode alongside the pattern
// as well as the instruction encoding format.
//=====================================================================================================================
template <is_opcode Opcode, is_x86_op_class_pattern Pattern, is_emitter Emitter>
struct InstrExpr {
    using pattern = Pattern;
    using opcode = Opcode;
    using emitter = Emitter;
};

//=====================================================================================================================
// InstrExpr identifer concept
//=====================================================================================================================
template <typename T> 
struct is_instr_expr_t : std::false_type {};

template <is_opcode OpCode, is_x86_op_class_pattern Pattern, is_emitter Emitter>
struct is_instr_expr_t<InstrExpr<OpCode, Pattern, Emitter>> : std::true_type {};

template <typename T>
concept is_instr_expr = is_instr_expr_t<T>::value;

template <X86Mnemonic mmic, is_instr_expr... InstrExpr>
struct InstrExprList {
    static constexpr X86Mnemonic mnemonic = mmic;

    static auto emit(IRX86Op::ListRef ops) -> X86ByteCode {
        X86ByteCode out{};

        auto found_match = [&]<typename InstructionExpr>() {
            auto [is_match, opsz] = InstructionExpr::pattern::match(ops);
            if (not is_match) { return false; }

            out = InstructionExpr::emitter::emit(InstructionExpr::opcode::value(), ops, opsz);
            return true;
        };

        std::ignore = (found_match.template operator()<InstrExpr>() or ...);
        return out;
    }
};

} // namespace fiska::assembler::backend

#endif // __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_EMITTERS_HH__
