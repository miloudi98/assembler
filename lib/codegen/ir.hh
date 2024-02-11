#ifndef __X86_ASSEMBLER_LIB_CODEGEN_IR_HH__
#define __X86_ASSEMBLER_LIB_CODEGEN_IR_HH__

#include "lib/support/core.hh"
#include "lib/x86/common.hh"
#include "lib/front_end/parser.hh"

namespace fiska::x86::codegen {

struct IRReg {
    BW bw_ = BW::Invalid;
    RI id_ = RI::Invalid;

    auto ndx() const -> u8;
    auto kind() const -> RK;
    auto need_ext() const -> i1;

    static auto need_ext(RI id) -> i1; 
    static auto kind(RI id) -> RK;
    static auto ndx(RI id) -> u8;
};

struct IRMem {
    enum struct Scale : i8 {
        Invalid = -1,
        One = 0,
        Two = 1,
        Four = 2,
        Eight = 3
    };

    BW bw_ = BW::Invalid;
    RI brid_ = RI::Invalid;
    RI irid_ = RI::Invalid;
    Scale scale_ = Scale::Invalid;
    i32 disp_{};

    auto disp_bw() const -> BW;
    auto kind() const -> MK;
    auto mod() const -> u8;
    auto need_sib() const -> i1;
    auto sib() const -> u8;
    auto ndx() const -> u8;
};

struct IRImm {
    BW bw_ = BW::Invalid;
    i64 value_{};
};

struct IRMoffs {
    BW bw_ = BW::Invalid;
    i64 addr_{};
};

struct RelocInfo {
    StrRef reloc_sym_name_;
    i1 must_reloc_{};
    u8 reloc_type_{};
    u8 instruction_offset_{};
};

template <typename T>
concept IsIRX86Op = OneOf<T, IRReg, IRMem, IRMoffs, IRImm>;

struct IRX86Op {
    static constexpr u8 kModReg = 0b11;
    static constexpr u8 kMaxInstrLength = 15;
    static constexpr u8 k16bitOpSzOverridePrefix = 0x66;
    static constexpr u8 kSibMarker = 0b100;
    static constexpr u8 kModMem = 0b00;
    static constexpr u8 kModMemDisp8 = 0b01;
    static constexpr u8 kModMemDisp32 = 0b10;
    static constexpr u8 kNoIndexRegInSib = 0b100;
    static constexpr u8 kNoBaseRegInSib = 0b101;

    using Inner = std::variant<
        std::monostate,
        IRReg,
        IRMem,
        IRImm,
        IRMoffs
    >;
    using Ref = const IRX86Op&;
    using List = Vec<IRX86Op>;
    using ListRef = const List&;

    Inner inner_{};
    mutable RelocInfo reloc_info_{};

    /*implicit*/IRX86Op(const Inner& i) : inner_(i) {}
    /*implicit*/IRX86Op(const Inner& i, const RelocInfo& ri) :
        inner_(i), reloc_info_(ri) {}

    template <IsIRX86Op... Ts>
    [[nodiscard]] auto is() const -> i1 { return (std::holds_alternative<Ts>(inner_) or ...); }

    template <IsIRX86Op T>
    [[nodiscard]] auto as() -> T& { return std::get<T>(inner_); }

    template <IsIRX86Op T>
    [[nodiscard]] auto as() const -> const T& { return std::get<T>(inner_); }

    // Helper functions used extensively.
    [[nodiscard]] auto r() const -> i1 { return is<IRReg>(); }
    [[nodiscard]] auto m() const -> i1 { return is<IRMem>(); }
    [[nodiscard]] auto rm() const -> i1 { return is<IRReg, IRMem>(); }
    [[nodiscard]] auto mo() const -> i1 { return is<IRMoffs>(); }
    [[nodiscard]] auto i() const -> i1 { return is<IRImm>(); }

    [[nodiscard]] auto as_r() const -> const IRReg& { return as<IRReg>(); }
    [[nodiscard]] auto as_m() const -> const IRMem& { return as<IRMem>(); }
    [[nodiscard]] auto as_mo() const -> const IRMoffs& { return as<IRMoffs>(); }
    [[nodiscard]] auto as_i() const -> const IRImm& { return as<IRImm>(); }

    // Return the operand's r/m or reg value in the ModRM byte.
    [[nodiscard]] auto ndx() const -> u8;

};

struct IRX86Instr {
    using Ref = const IRX86Instr&;

    X86Mnemonic mmic_ = X86Mnemonic::Invalid;
    IRX86Op::List ops_;
};

struct IRProc {
    StrRef name_;
    Vec<IRX86Instr> body_;
};

struct IRVar {
    StrRef name_;
    ByteVec data_;
};

struct IRBuilder {
    using IRObject = std::variant<IRProc, IRVar>;

    Vec<IRObject> ir_objects_;
    utils::StringMap<Str> sym_sct_info_;

    auto lower_x86_instr_expr(fe::X86InstrExpr* x86_instr_expr) -> IRX86Instr;
    auto link_sym_name_to_section(StrRef sym_name, StrRef sct_name) -> void;
    auto build(fe::Expr::ListRef ast_);
};

} // namespace fiska::x86::codegen

#endif // __X86_ASSEMBLER_LIB_CODEGEN_IR_HH__
