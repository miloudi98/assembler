#ifndef __X86_ASSEMBLER_LIB_FRONTEND_AST_HH__
#define __X86_ASSEMBLER_LIB_FRONTEND_AST_HH__

#include "lib/common/base.hh"
#include "lib/common/x86/types.hh"
#include "lib/frontend/token.hh"
#include "lib/common/support.hh"

namespace fiska::assembler::frontend {

struct Expr;

struct RegisterExpr {
    BW bw_ = BW::Invalid;
    RI ri_ = RI::Invalid;
};

struct MemExpr {
    BW bw_ = BW::Invalid;
    RI bri_ = RI::Invalid;
    RI iri_ = RI::Invalid;
    Box<Expr> scale_ = nullptr;
    Box<Expr> disp_ = nullptr;
};

struct ImmExpr {
    BW bw_ = BW::Invalid;
    Box<Expr> value_ = nullptr;
};

struct MoffsExpr {
    BW bw_ = BW::Invalid;
    Box<Expr> addr_ = nullptr;
};

struct IntExpr {
    i64 value_{};
};

struct LabelExpr {
    StrRef name_;
};

struct BinaryOpExpr {
    Tok op_{};
    Box<Expr> lhs_ = nullptr;
    Box<Expr> rhs_ = nullptr;
};

struct UnaryOpExpr {
    Tok op_{};
    Box<Expr> inner_ = nullptr;
};

struct X86InstrExpr {
    X86Mnemonic mnemonic_ = X86Mnemonic::Invalid;
    Vec<Box<Expr>> ops_;
};

struct ProcExpr {
    StrRef name_;
    Vec<Box<Expr>> body_;
};

struct VarExpr {
    BW type_ = BW::Invalid;
    StrRef name_;
    Box<Expr> value_;
};

struct StrExpr {
    StrRef lit_;
};

struct ArrayExpr {
    Vec<Box<Expr>> values_;
};

struct Expr {
    NOT_COPYABLE(Expr);

    using InnerType = std::variant<
        std::monostate,
        RegisterExpr,
        MemExpr,
        ImmExpr,
        MoffsExpr,
        IntExpr,
        LabelExpr,
        BinaryOpExpr,
        UnaryOpExpr,
        X86InstrExpr,
        ProcExpr,
        VarExpr,
        StrExpr,
        ArrayExpr
    >;

    enum struct Kind {
        Invalid,

        Register,
        Mem,
        Imm,
        Moffs,
        Int,
        Label,
        BinaryOp,
        UnaryOp,
        X86Instr,
        Proc,
        Var,
        Str,
        Array,
    };


    Kind kind_ = Kind::Invalid;
    StrRef section_;
    InnerType inner_{};
    Span span_{};

    Expr() = default;
    /* implicit */Expr(InnerType expr) :
        kind_(kind_of_expr(expr)), inner_(std::move(expr)) {}
    Expr(Expr&& o) : 
        kind_(o.kind_), inner_(std::move(o.inner_)), span_(o.span_) {}
    Expr& operator=(Expr&& o) {
        kind_ = o.kind_;
        inner_ = std::move(o.inner_);
        span_ = o.span_;
        return *this;
    }

    template <typename T>
    auto is() const -> i1 { return std::holds_alternative<T>(inner_); }

    template <typename T>
    auto as() const -> const T& { return std::get<T>(inner_); }

    // Expr::Kind of an AST node.
    static auto kind_of_expr(const InnerType& expr) -> Kind {
        if (std::holds_alternative<RegisterExpr>(expr)) { return Kind::Register; }
        if (std::holds_alternative<MemExpr>(expr)) { return Kind::Mem; }
        if (std::holds_alternative<ImmExpr>(expr)) { return Kind::Imm; }
        if (std::holds_alternative<MoffsExpr>(expr)) { return Kind::Moffs; }
        if (std::holds_alternative<IntExpr>(expr)) { return Kind::Int; }
        if (std::holds_alternative<LabelExpr>(expr)) { return Kind::Label; }
        if (std::holds_alternative<BinaryOpExpr>(expr)) { return Kind::BinaryOp; }
        if (std::holds_alternative<UnaryOpExpr>(expr)) { return Kind::UnaryOp; }
        if (std::holds_alternative<X86InstrExpr>(expr)) { return Kind::X86Instr; }
        if (std::holds_alternative<ProcExpr>(expr)) { return Kind::Proc; }
        if (std::holds_alternative<VarExpr>(expr)) { return Kind::Var; }
        if (std::holds_alternative<StrExpr>(expr)) { return Kind::Str; }
        if (std::holds_alternative<ArrayExpr>(expr)) { return Kind::Array; }
        unreachable();
    }
};

} // namespace fiska::assembler::frontend

// Support formatting AST nodes.
template <>
struct fmt::formatter<fiska::assembler::frontend::Expr::Kind> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    template <typename FormatContext>
    auto format(const fiska::assembler::frontend::Expr::Kind ek, FormatContext& ctx) const -> decltype(ctx.out()) {
        using fiska::assembler::frontend::Expr;

        auto str_of_ek = [&] {
            switch (ek) {
                case Expr::Kind::Register: return "RegisterExpr";
                case Expr::Kind::Mem: return "MemExpr";
                case Expr::Kind::Imm: return "ImmExpr";
                case Expr::Kind::Moffs: return "MoffsExpr";
                case Expr::Kind::Int: return "IntExpr";
                case Expr::Kind::Label: return "LabelExpr";
                case Expr::Kind::BinaryOp: return "BinaryOpExpr";
                case Expr::Kind::UnaryOp: return "UnaryOpExpr";
                case Expr::Kind::X86Instr: return "X86InstrExpr";
                case Expr::Kind::Proc: return "ProcExpr";
                case Expr::Kind::Var: return "VarExpr";
                case Expr::Kind::Str: return "StrExpr";
                case Expr::Kind::Array: return "ArrayExpr";
                case Expr::Kind::Invalid: return "InvalidExpr";
            } // switch
            unreachable();
        }();
        return fmt::format_to(ctx.out(), "{}", str_of_ek);
    }
};

#endif // __X86_ASSEMBLER_LIB_FRONTEND_AST_HH__
