#ifndef __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_EMITTERS_HH__
#define __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_EMITTERS_HH__

#include "lib/common/base.hh"
#include "lib/common/x86/types.hh"
#include "lib/backend/ir.hh"
#include "lib/backend/codegen/patterns.hh"

namespace fiska::assembler::backend {

// Bytecode containing details for potential patching.
struct X86ByteCode {
    struct Relocation {
        StrRef symbol_name_;
        u8 i_offset_{};
    };

    u8 bytecode_[X86Info::kMaxInstrLength]{};
    u8 bytecode_sz_{};
    Vec<Relocation> rels_;

    auto begin() -> u8* { return bytecode_; }
    auto end() -> u8* { return bytecode_ + size(); }
    auto size() const -> u8 { return bytecode_sz_; }
    auto add(BW bw, u64 qword) -> void;
    auto add(u64 qword) -> void;
    auto add_relocation(StrRef symbol_name) -> void;
};

template <OpSz operand_size = OpSz::Default, is_x86_op_class... IROpClass>
struct Pattern {
    using MatchInfo = std::tuple<i1, OpSz>;

    static constexpr u8 match_idx = 0;
    static constexpr u8 opsz_idx = 1;
    static constexpr OpSz opsz = operand_size;

    static auto match(IRX86Op::ListRef ops) -> MatchInfo {
        constexpr u8 ops_length = sizeof...(IROpClass);

        if (ops_length == 0) { return { true, opsz }; }
        if (ops_length != ops.size()) { return { false, opsz }; }

        u8 op_idx = 0;
        return { (IROpClass::match(ops[op_idx++]) and ...), opsz };

    }
};

template <IsX86PatternClass... Pattern>
struct Or : X86PatternClass {
    using MatchInfo = Pat<>::MatchInfo;

    static auto match(IRX86Op::ListRef ops) -> MatchInfo {
        MatchInfo match_info{};

        auto pattern_match = [&]<typename X86Pat>() {
            auto [is_match, opsz] = X86Pat::match(ops);
            if (is_match) {
                std::get<Pat<>::match_idx>(match_info) = true;
                std::get<Pat<>::opsz_idx>(match_info) = opsz;
            }
            return is_match;
        };

        std::ignore = (pattern_match.template operator()<Pattern>() or ...);
        return match_info;
    }
};

template <OpEn encoding, auto... args>
struct Emitter;

template <>
struct Emitter<OpEn::MR> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode;
};

template <>
struct Emitter<OpEn::RM> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode;
};

template <>
struct Emitter<OpEn::FD> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode;
};

template <>
struct Emitter<OpEn::TD> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode;
};

template <>
struct Emitter<OpEn::OI> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode;
};

template <SlashDigit slash_digit>
struct Emitter<OpEn::MI, slash_digit> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode;
};

template <>
struct Emitter<OpEn::I> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode;
};

template <>
struct Emitter<OpEn::ZO> {
    static auto emit(u32 opcode, IRX86Op::ListRef ops, OpSz opsz) -> X86ByteCode;
};

} // namespace fiska::assembler::backend

#endif // __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_EMITTERS_HH__
