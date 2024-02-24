#ifndef __X86_ASSEMBLER_LIB_FRONTEND_AST_HH__
#define __X86_ASSEMBLER_LIB_FRONTEND_AST_HH__

#include "lib/common/base.hh"
#include "lib/common/x86/types.hh"
#include "lib/common/support.hh"
#include "lib/frontend/ctx.hh"
#include "lib/frontend/token.hh"

namespace fiska::assembler::frontend {

struct Expr {
    // For an enumeration whose underlying type is not fixed, the underlying type is an integral type that can represent all the enumerator values
    // defined in the enumeration. If no integral type can represent all the enumerator values, the enumeration is ill-formed. 
    // It is implementation-defined which integral type is used as the underlying type except that the underlying type shall not be larger 
    // than int unless the value of an enumerator cannot fit in an int or unsigned int. 
    // If the enumerator-list is empty, the underlying type is as if the enumeration had a single enumerator with value 0.
    enum struct Kind {
        Invalid,

        // Expr
        Int,
        Label,
        BinaryOp,
        UnaryOp,
        X86Inst,
        Str,
        Array,

        // X86 Operands
        Register,
        Mem,
        Imm,
        Moffs,

        // Symbols
        Proc,
        Var,
    };

    Kind kind_ = Kind::Invalid;
    Span span_{};

    Expr(Kind k) : kind_(k) {}
    Expr(Kind k, Span s) : kind_(k), span_(s) {}
    virtual ~Expr() = default;

    // Forbid allocating an expression without the owner context.
    void* operator new(usz sz, Ctx* ctx) {
        auto expr = static_cast<Expr*>(::operator new(sz));
        ctx->exprs_.push_back(expr);
        return expr;
    }
    void* operator new(usz sz) = delete;
};

struct X86Op : Expr {
    X86Op(Kind k) : Expr(k) {}

    static auto classof(const Expr* e) -> i1 {
        return (e->kind_ >= Kind::Register) and (e->kind_ <= Kind::Moffs);
    }
};

struct Symbol : Expr {
    Symbol(Kind k, Span s) : Expr(k, s) {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ >= Kind::Proc;
    }
};

struct IntExpr : Expr {
    i64 value_{};

    IntExpr(i64 v, Span s) :
        Expr(Expr::Kind::Int, s), value_(v)
    {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ == Kind::Int;
    }
};

struct LabelExpr : Expr {
    StrRef name_;

    LabelExpr(StrRef str, Span s) :
        Expr(Expr::Kind::Label, s), name_(str)
    {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ == Kind::Label;
    }
};

struct BinaryOpExpr : Expr {
    Tok op_{};
    Expr* lhs_{};
    Expr* rhs_{};

    BinaryOpExpr(Tok op, Expr* lhs, Expr* rhs, Span s) :
        Expr(Expr::Kind::BinaryOp, s), op_(op), lhs_(lhs),
        rhs_(rhs)
    {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ == Kind::BinaryOp;
    }
};

struct UnaryOpExpr : Expr {
    Tok op_{};
    Expr* inner_{};

    UnaryOpExpr(Tok op, Expr* i, Span s) :
        Expr(Expr::Kind::UnaryOp, s), op_(op), inner_(i)
    {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ == Kind::UnaryOp;
    }
};

struct StrExpr : Expr {
    StrRef str_{};

    StrExpr(StrRef str, Span s) :
        Expr(Expr::Kind::Str, s), str_(str)
    {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ == Kind::Str;
    }
};

struct ArrayExpr : Expr {
    Vec<Expr*> elements_;

    ArrayExpr(Span s) : Expr(Expr::Kind::Array, s) {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ == Kind::Array;
    }
};

struct X86Inst : Expr {
    X86Mnemonic mnemonic_ = X86Mnemonic::Invalid;
    Vec<X86Op*> ops_;

    X86Inst() : Expr(Kind::X86Inst) {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ == Kind::X86Inst;
    }
};

struct RegisterOp : X86Op {
    BW bw_ = BW::Invalid;
    RI ri_ = RI::Invalid;

    RegisterOp() :
        X86Op(Expr::Kind::Register)
    {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ == Kind::Register;
    }
};

struct MemOp : X86Op {
    BW bw_ = BW::Invalid;
    RI bri_ = RI::Invalid;
    RI iri_ = RI::Invalid;
    Expr* scale_{};
    Expr* disp_{};

    MemOp() : X86Op(Expr::Kind::Mem) {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ == Kind::Mem;
    }
};

struct ImmOp : X86Op {
    BW bw_ = BW::Invalid;
    Expr* value_{};

    ImmOp() :
        X86Op(Expr::Kind::Imm)
    {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ == Kind::Imm;
    }
};

struct MoffsOp : X86Op {
    BW bw_ = BW::Invalid;
    Expr* addr_{};

    MoffsOp() : X86Op(Expr::Kind::Moffs)
    {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ == Kind::Moffs;
    }
};

struct Proc : Symbol {
    StrRef name_;
    Vec<X86Inst*> body_;

    Proc(StrRef name, Span s) : Symbol(Expr::Kind::Proc, s), name_(name) {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ == Kind::Proc;
    }
};

struct Var : Symbol {
    StrRef label_;
    Expr* rvalue_;

    Var(Span s, StrRef l, Expr* rv) : 
        Symbol(Expr::Kind::Var, s), label_(l), rvalue_(rv) 
    {}

    static auto classof(const Expr* e) -> i1 {
        return e->kind_ == Kind::Var;
    }
};

struct Section {
    StrRef name_;
    Vec<Symbol*> symbols_;

    Section(StrRef n) : name_(n) {} 
};

// Simple casting mechanism.
template <typename To>
auto cast(const Expr* from) -> const To* {
    if (To::classof(from)) {
        return static_cast<const To*>(from);
    }
    return static_cast<const To*>(nullptr);
}

} // namespace fiska::assembler::frontend

// Support formatting tokens.
template <>
struct fmt::formatter<fiska::assembler::frontend::Expr::Kind> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    template <typename FormatContext>
    auto format(const fiska::assembler::frontend::Expr::Kind ek, FormatContext& ctx) const -> decltype(ctx.out()) {
        using fiska::assembler::frontend::Expr;

        auto str_of_ek = [&] {
            switch (ek) {
                case Expr::Kind::Invalid: return "<Invalid>";
                case Expr::Kind::Int: return "<Int>";
                case Expr::Kind::Label: return "<Label>";
                case Expr::Kind::BinaryOp: return "<BinaryOp>";
                case Expr::Kind::UnaryOp: return "<UnaryOp>";
                case Expr::Kind::X86Inst: return "<X86Inst>";
                case Expr::Kind::Str: return "<Str>";
                case Expr::Kind::Array: return "<Array>";
                case Expr::Kind::Register: return "<Register>";
                case Expr::Kind::Mem: return "<Mem>";
                case Expr::Kind::Imm: return "<Imm>";
                case Expr::Kind::Moffs: return "<Moffs>";
                case Expr::Kind::Proc: return "<Proc>";
                case Expr::Kind::Var: return "<Var>";
            } // switch
            unreachable();
        }();
        return fmt::format_to(ctx.out(), "{}", str_of_ek);
    }
};

#endif // __X86_ASSEMBLER_LIB_FRONTEND_AST_HH__
