#ifndef __X86_ASSEMBLER_LIB_BACKEND_IR_HH__
#define __X86_ASSEMBLER_LIB_BACKEND_IR_HH__

#include "lib/common/x86/types.hh"
#include "lib/common/base.hh"
#include "lib/common/support.hh"
#include "lib/frontend/ast.hh"
#include "lib/frontend/sema.hh"

namespace fiska::assembler::backend {

struct IRValue {
    StrRef symbol_name_;
    i64 addend_{};

    auto req_reloc() const -> i1 { return symbol_name_.empty(); }
    auto has_value() const -> i1 { return addend_ != 0; }
};

struct IRReg {
    BW bw_ = BW::Invalid;
    RI ri_ = RI::Invalid;

    auto ndx() const -> u8;
    auto kind() const -> RK;
    auto req_ext() const -> i1;
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
    RI bri_ = RI::Invalid;
    RI iri_ = RI::Invalid;
    Scale scale_ = Scale::Invalid;
    IRValue disp_{};

    auto disp_bw() const -> BW;
    auto kind() const -> MK;
    auto mod() const -> u8;
    auto need_sib() const -> i1;
    auto sib() const -> u8;
    auto ndx() const -> u8;
};

struct IRImm {
    BW bw_ = BW::Invalid;
    IRValue value_{};
};

struct IRMoffs {
    BW bw_ = BW::Invalid;
    IRValue addr_{};
};

// REMINDER: Relocations are going to be produced by... the emitter. The X86op has all the information 
// required to produce a patchable relocation. so DON'T ADD the patch offset member variable to the IRX86Op.
struct IRX86Op {
    using Ref = const IRX86Op&;
    using List = Vec<IRX86Op>;
    using ListRef = const List&;
    using ValueType = std::variant<
        std::monostate,
        IRReg,
        IRMem,
        IRImm,
        IRMoffs
    >;

    ValueType inner_{};

    IRX86Op() = default;
    /*implicit*/IRX86Op(const ValueType& i) : inner_(i) {}

    template <OneOf<IRReg, IRMem, IRImm, IRMoffs>... Ts>
    [[nodiscard]] auto is() const -> i1 { return (std::holds_alternative<Ts>(inner_) or ...); }

    template <OneOf<IRReg, IRMem, IRImm, IRMoffs> T>
    [[nodiscard]] auto as() -> T& { return std::get<T>(inner_); }

    template <OneOf<IRReg, IRMem, IRImm, IRMoffs> T>
    [[nodiscard]] auto as() const -> const T& { return std::get<T>(inner_); }

    // Helper functions used extensively.
    [[nodiscard]] auto r() const -> i1 { return is<IRReg>(); }
    [[nodiscard]] auto m() const -> i1 { return is<IRMem>(); }
    [[nodiscard]] auto i() const -> i1 { return is<IRImm>(); }
    [[nodiscard]] auto mo() const -> i1 { return is<IRMoffs>(); }
    [[nodiscard]] auto rm() const -> i1 { return is<IRReg, IRMem>(); }

    [[nodiscard]] auto as_r() const -> const IRReg& { return as<IRReg>(); }
    [[nodiscard]] auto as_m() const -> const IRMem& { return as<IRMem>(); }
    [[nodiscard]] auto as_i() const -> const IRImm& { return as<IRImm>(); }
    [[nodiscard]] auto as_mo() const -> const IRMoffs& { return as<IRMoffs>(); }
};

struct IRX86Instr {
    X86Mnemonic mnemonic_ = X86Mnemonic::Invalid;
    Vec<IRX86Op> ops_;
};

struct IRProc {
    Vec<IRX86Instr> body_;
};

struct IRVar {
    BW type_ = BW::Invalid;
    ByteVec data_;
};

struct IRSymbol {
    using ValueType = std::variant<
        std::monostate,
        IRProc,
        IRVar
    >;

    StrRef name_;
    StrRef section_;
    // TODO: We'll probably need this.
    // Span span_;
    ValueType inner_{};

    IRSymbol() = default;
    /*implicit*/IRSymbol(ValueType i) : inner_(std::move(i)) {}

    template <OneOf<IRProc, IRVar>... Ts>
    [[nodiscard]] auto is() const -> i1 { return (std::holds_alternative<Ts>(inner_) or ...); }

    template <OneOf<IRProc, IRVar> T>
    [[nodiscard]] auto as() -> T& { return std::get<T>(inner_); }

    template <OneOf<IRProc, IRVar> T>
    [[nodiscard]] auto as() const -> const T& { return std::get<T>(inner_); }
};

// Entry point for lowering the AST.
auto lower(Ctx* ctx, const Vec<Box<frontend::Expr>>& ast, frontend::SemaDone token) -> Vec<IRSymbol>;

} // namespace fiska::assembler::backend

#endif // __X86_ASSEMBLER_LIB_BACKEND_IR_HH__
