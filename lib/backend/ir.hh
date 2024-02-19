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

    [[nodiscard]] auto ndx() const -> u8 { return r() ? as_r().ndx() : as_m().ndx(); }
    [[nodiscard]] auto mod() const -> u8 { return r() ? X86Info::kModReg : as_m().mod(); }

    [[nodiscard]] auto req_reloc() const -> i1;
    [[nodiscard]] auto reloc_symbol_name() const -> StrRef;
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

    enum struct Kind {
        Invalid,

        Proc,
        Var
    };

    Kind kind_ = Kind::Invalid;
    StrRef name_;
    StrRef section_;
    Span span_;
    ValueType inner_{};

    IRSymbol() = default;
    /*implicit*/IRSymbol(ValueType i) : kind_(kind_of_symbol(i)), inner_(std::move(i)) {}

    template <OneOf<IRProc, IRVar>... Ts>
    [[nodiscard]] auto is() const -> i1 { return (std::holds_alternative<Ts>(inner_) or ...); }

    template <OneOf<IRProc, IRVar> T>
    [[nodiscard]] auto as() -> T& { return std::get<T>(inner_); }

    template <OneOf<IRProc, IRVar> T>
    [[nodiscard]] auto as() const -> const T& { return std::get<T>(inner_); }

    [[nodiscard]] auto kind() const -> Kind {
        return kind_of_symbol(inner_);
    }

    [[nodiscard]] static auto kind_of_symbol(const ValueType& value) -> Kind {
        if (std::holds_alternative<IRProc>(value)) { return Kind::Proc; }
        if (std::holds_alternative<IRVar>(value)) { return Kind::Var; }
        unreachable();
    }
};

// Entry point for lowering the AST.
auto lower(Ctx* ctx, const Vec<Box<frontend::Expr>>& ast, frontend::SemaDone token) -> Vec<IRSymbol>;

} // namespace fiska::assembler::backend

// Support formatting |IRValue|s.
template <>
struct fmt::formatter<fiska::assembler::backend::IRValue> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    template <typename FormatContext>
    auto format(const fiska::assembler::backend::IRValue irvalue, FormatContext& ctx) const -> decltype(ctx.out()) {
        return fmt::format_to(ctx.out(), "[ '{}', {} ]", irvalue.symbol_name_, irvalue.addend_);
    }
};

// Support formatting register ids.
template <>
struct fmt::formatter<fiska::assembler::backend::IRX86Op> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    template <typename FormatContext>
    auto format(const fiska::assembler::backend::IRX86Op op, FormatContext& ctx) const -> decltype(ctx.out()) {
        if (op.r()) {
            return fmt::format_to(ctx.out(), "IRReg {{ {}, {} }}", op.as_r().bw_, op.as_r().ri_); 
        }

        if (op.m()) {
            return fmt::format_to(ctx.out(),
                "IRMem {{ {}, {}, {}, {} }}",
                op.as_m().bw_, op.as_m().bri_, +op.as_m().scale_,
                op.as_m().disp_
            );
        }

        if (op.i()) {
            return fmt::format_to(ctx.out(),
                "IRImm {{ {}, {} }}", op.as_i().bw_, op.as_i().value_);
        }

        if (op.mo()) {
            return fmt::format_to(ctx.out(),
                "IRMoffs {{ {}, {} }}", op.as_mo().bw_, op.as_mo().addr_);
        }

        unreachable();
    }
};

#endif // __X86_ASSEMBLER_LIB_BACKEND_IR_HH__
