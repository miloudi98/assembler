#ifndef __X86_ASSEMBLER_LIB_FRONT_END_PARSER_HH__
#define __X86_ASSEMBLER_LIB_FRONT_END_PARSER_HH__

#include "lib/support/core.hh"
#include "lib/x86/common.hh"
#include "lib/front_end/lexer.hh"

namespace fiska::x86::fe {

struct Ctx;
struct Expr {
    using List = Vec<Expr*>;
    using ListRef = const List&;

    enum struct Kind {
        Invalid, 

        Proc,
        Type,
        RegLit,
        MemRefLit,
        ImmLit,
        MoffsLit,
        IntLit,
        Label,

        BinaryOp,
        UnaryOp,

        X86Instr,
    };

    Kind kind_ = Kind::Invalid;
    Str section_;

    Expr(Kind kind) : kind_(kind) {}
    virtual ~Expr() = default;

    // Create an expression and bind it to the global context.
    void* operator new(usz sz, Ctx* ctx, StrRef section = "");
    // Disallow creating expressions with no global context.
    void* operator new(usz sz) = delete;
};

// Register expression.
struct RegLitExpr : Expr {
    BW bw_ = BW::Invalid;
    RI id_ = RI::Invalid;

    RegLitExpr() : 
        Expr(Expr::Kind::RegLit) {}
};

struct MemRefLitExpr : Expr {
    BW bw_ = BW::Invalid;
    RI brid_ = RI::Invalid;
    RI irid_ = RI::Invalid;
    i8 scale_{};

    MemRefLitExpr() : Expr(Expr::Kind::MemRefLit) {}
};

struct ImmLitExpr : Expr {
    BW bw_ = BW::Invalid;
    Expr* value_ = nullptr;

    ImmLitExpr() : Expr(Expr::Kind::ImmLit) {}
};

struct MoffsLitExpr : Expr {
    BW bw_ = BW::Invalid;
    Expr* addr_ = nullptr;

    MoffsLitExpr() : Expr(Expr::Kind::MoffsLit) {}
};

struct IntLitExpr : Expr {
    i64 value_{};

    IntLitExpr(i64 v) : 
        Expr(Expr::Kind::IntLit), value_(v) {}
};

struct LabelExpr : Expr {
    StrRef name_;

    LabelExpr(StrRef name) :
        Expr(Expr::Kind::Label), name_(name) {}
};

struct BinaryOpExpr : Expr {
    Expr* lhs_ = nullptr;
    Expr* rhs_ = nullptr;
    TK op_ = TK::Invalid;

    BinaryOpExpr(TK op, Expr* lhs, Expr* rhs) : 
        Expr(Expr::Kind::BinaryOp), lhs_(lhs), rhs_(rhs), op_(op) {}
};

struct UnaryOpExpr : Expr {
    TK op_ = TK::Invalid;
    Expr* inner_ = nullptr;

    UnaryOpExpr() : Expr(Expr::Kind::UnaryOp) {}
    UnaryOpExpr(TK o, Expr* i) :
        Expr(Expr::Kind::UnaryOp), op_(o), inner_(i) {}
};

struct X86InstrExpr : Expr {
    X86Mnemonic mmic_ = X86Mnemonic::Invalid;
    Vec<Expr*> ops_;

    X86InstrExpr() : Expr(Expr::Kind::X86Instr) {}
};

struct ProcExpr : Expr {
    StrRef name_;
    Vec<X86InstrExpr*> body_;

    ProcExpr() : Expr(Expr::Kind::Proc) {}
};

struct VarExpr : Expr {
    StrRef name_;
    Expr* value_;
};

struct Parser {
    Ctx* ctx_{};
    u16 fid_{};
    TokStream::Iterator tok_stream_it_;
    Str curr_section_;

    explicit Parser(Ctx* ctx, u16 fid);

    auto next_tok() -> void;
    auto tok() -> const Tok&;
    auto peek_tok(i32 idx = 0) -> const Tok&;
    auto peek_tok_kind(i32 idx = 0) -> TK { return peek_tok(idx).kind_; }
    auto peek_tok_str(i32 idx = 0) -> StrRef { return peek_tok(idx).str_; }

    auto parse_top_level_expr() -> Expr*;
    auto parse_file() -> Expr::List;
    auto parse_proc() -> ProcExpr*;
    auto parse_var_expr() -> VarExpr*;
    auto parse_x86_instr_expr() -> X86InstrExpr*;
    auto parse_expr(i8 prec = 0) -> Expr*;
    auto perform_constant_folding(Expr*) -> Expr*;
    auto parse_i64(StrRef str) -> i64;

    static auto prefix_prec_of_tok_kind(TK tk) -> i8;
    static auto infix_prec_of_tok_kind(TK tk) -> i8;
    static auto parse_i64() -> i64;

    // Helper methods.
    auto at(std::same_as<TK> auto... tk) -> i1 {
        return ((tok().kind_ == tk) or ...);
    }

    auto consume(std::same_as<TK> auto... tk) -> i1 {
        if (not at(tk...)) { return false; }
        next_tok();
        return true;
    }

    // TODO(miloudi): Have proper error handling please.
    auto expect_any(std::same_as<TK> auto... tk) -> void {
        assert(consume(tk...));
    }

    auto expect_all(std::same_as<TK> auto... tk) -> void {
        assert((consume(tk) and ...));
    }

    auto expect(TK tk) -> void { expect_all(tk); }

    auto match(std::same_as<TK> auto... tk) -> i1 {
        i32 idx = 0;
        return ((peek_tok_kind(idx++) == tk) and ...);
    }

};


} // namespace fiska::x86::fe

#endif // __X86_ASSEMBLER_LIB_FRONT_END_PARSER_HH__
