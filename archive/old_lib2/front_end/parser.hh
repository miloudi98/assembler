#ifndef __X86_ASSEMBLER_LIB_PARSER_HH__
#define __X86_ASSEMBLER_LIB_PARSER_HH__

#include "lib/core.hh"
#include "lib/x86_utils.hh"
#include "lib/front_end/support.hh"
#include "lib/front_end/lexer.hh"

namespace fiska::x86::fe {

// Expression kind.
enum struct ExprKind {
    Invalid, 

    Proc,
    Type,
    RegLit,
    MemRefLit,
    IntLit,
    Label,

    BinaryOp,
    UnaryOp,

    X86Op,
    X86Instruction,
};

struct Ctx;
struct Expr {
    using List = Vec<Expr*>;
    using ListRef = const List&;

    ExprKind kind_ = ExprKind::Invalid;
    // TODO(miloudi): Add this in the constructor.
    StrRef sct_name_;

    Expr(ExprKind kind) : kind_(kind) {}
    virtual ~Expr() = default;

    // Create an expression and bind it to the global context.
    void* operator new(usz sz, Ctx* ctx);
    // Disallow creating expressions with no global context.
    void* operator new(usz sz) = delete;
};

struct TypeExpr : Expr {
    BW bw_ = BW::Invalid;
    i1 is_ptr_ = false;

    TypeExpr() : Expr(ExprKind::Type) {}
};

struct RegLitExpr : Expr {
    RI id_ = RI::Invalid;

    RegLitExpr(RI rid) : 
        Expr(ExprKind::RegLit), id_(rid) {}
};

struct MemRefLitExpr : Expr {
    BW bw_ = BW::Invalid;
    RI brid_ = RI::Invalid;
    RI irid_ = RI::Invalid;
    Mem::Scale scale_ = Mem::Scale::Invalid;
    Expr* disp_ = nullptr;

    MemRefLitExpr() : Expr(ExprKind::MemRefLit) {}
};

struct IntLitExpr : Expr {
    i64 value_{};

    IntLitExpr(i64 value) : 
        Expr(ExprKind::IntLit), value_(value) {}
};

struct LabelExpr : Expr {
    StrRef name_;

    LabelExpr(StrRef name) :
        Expr(ExprKind::Label), name_(name) {}
};

struct BinaryOpExpr : Expr {
    Expr* lhs_ = nullptr;
    Expr* rhs_ = nullptr;
    TK op_ = TK::Invalid;

    BinaryOpExpr(TK op, Expr* lhs, Expr* rhs) : 
        Expr(ExprKind::BinaryOp), lhs_(lhs), rhs_(rhs), op_(op) {}

    // TODO: Implement the evaluate function.
    // Takes a binaryopexpr and spits a flattened binaryopexpr.
    // Same thing for unaryExpr; Maybe validate the expressions there as well.
    auto evaluate() -> Expr*;
};

struct UnaryOpExpr : Expr {
    Expr* inner_ = nullptr;
    TK op_ = TK::Invalid;

    UnaryOpExpr(TK op, Expr* inner) : 
        Expr(ExprKind::UnaryOp), inner_(inner), op_(op) {}
};

struct X86OpExpr : Expr {
    TypeExpr* type_ = nullptr;
    Expr* op_ = nullptr;
    
    X86OpExpr(TypeExpr* type, Expr* op) : 
        Expr(ExprKind::X86Op), type_(type), op_(op) {} 
};

struct X86InstructionExpr : Expr {
    X86IK kind_ = X86IK::Invalid;
    Vec<X86OpExpr*> operands_;

    X86InstructionExpr(X86IK kind, Vec<X86OpExpr*> operands) :
        Expr(ExprKind::X86Instruction), kind_(kind), operands_(std::move(operands)) {}
};

struct ProcExpr : Expr {
    StrRef name_;
    Vec<X86Instruction*> body_;

    ProcExpr() : Expr(ExprKind::Proc) {}
};

struct Parser {
    Ctx* ctx_{};
    u16 fid_{};
    TokStream::Iterator tok_stream_it_;
    StrRef curr_section_{};

    explicit Parser(Ctx* ctx, u16 fid);

    auto next_tok() -> void;
    auto tok() -> const Tok&;
    auto peek_tok(i32 idx = 0) -> const Tok&;
    auto peek_tk(i32 idx = 0) -> TK { return peek_tok(idx).kind_; }
    auto peek_tstr(i32 idx = 0) -> StrRef { return peek_tok(idx).str_; }

    auto parse_section() -> void;
    auto parse_proc() -> ProcExpr*;
    auto parse_type() -> TypeExpr*;
    auto parse_reg_lit() -> RegLitExpr*;
    auto parse_mem_ref_lit() -> MemRefLitExpr*;
    auto parse_int_lit() -> IntLitExpr*;
    // TODO(miloudi): This default value might be a source of hard bugs.
    auto parse_expr(i8 prec = 0) -> Expr*;
    auto parse_x86_instruction() -> X86InstructionExpr*;
    auto parse_x86_op_expr() -> X86OpExpr*;

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
        return ((peek_tok(idx++).kind_ == tk) and ...);
    }

};


} // namespace fiska::fe

#endif // __X86_ASSEMBLER_LIB_PARSER_HH__


// TODO(miloudi): This should not inherit from Expr. It has nothing to do with it.
//struct X86OpExpr : Expr {
//    X86OpK ty_{};
//
//    X86OpExpr(X86OpK kind) : ty_(kind) {}
//    virtual ~X86OpExpr() = default;
//};
//
//struct RegExpr : X86OpExpr {
//    x86::BW bw_ = x86::BW::B0;
//    x86::RI id_ = x86::RI::Invalid;
//
//    RegExpr() : X86OpExpr(X86OpK::RegExpr) {}
//};
//
//struct MemRefExpr : X86OpExpr {
//    x86::BW bw_ = x86::BW::B0;
//    x86::RI breg_id_ = x86::RI::Invalid;
//    x86::RI ireg_id_ = x86::RI::Invalid;
//    x86::MemIndexScale scale_ = x86::MemIndexScale::Invalid;
//    std::variant<i64, StrRef> disp_{};
//    u8 patch_offset_{};
//
//    MemRefExpr() : X86OpExpr(X86OpK::MemRefExpr) {}
//};
//
//struct MoffsExpr : X86OpExpr {
//    x86::BW bw_ = x86::BW::B0;
//    i64 addr_{};
//
//    MoffsExpr() : X86OpExpr(X86OpK::MoffsExpr) {}
//};
//
//struct ImmExpr : X86OpExpr {
//    x86::BW bw_ = x86::BW::B0;
//    i64 value_{};
//
//    ImmExpr() : X86OpExpr(X86OpK::ImmExpr) {}
//};
//
//// X86Instruction Expression.
//struct X86InstructionExpr {
//    using List = Vec<X86InstructionExpr>;
//    using ListRef = const List&;
//    using Ref = const X86InstructionExpr&;
//
//    x86::X86IK kind_{};
//    Vec<X86OpExpr*> operands_{};
//};
//
//struct ProcExpr : Expr {
//    StrRef name_{};
//    X86InstructionExpr::List body_{};
//
//    ProcExpr() : Expr(EK::ProcExpr) {}
//};
//
// Global context for the entire front end.
//struct Ctx {
//    // List of files loaded.
//    Vec<Box<File>> files_{};
//    // Tokens of each file loaded.
//    Vec<TokStream> tok_streams_{};
//    // Ast nodes allocated.
//    Vec<Expr*> ast_{};
//    // Instruction operand expressions allocated.
//    Vec<X86OpExpr*> instruction_ops_{};
//    // Pool of interned strings.
//    StringInterner str_pool_{};
//
//    // Load file to memory.
//    auto load_file(const fs::path& path) -> File*;
//    // Return the file with fid |fid|.
//    auto get_file(u16 fid) -> File*;
//
//    // Deallocate all ast nodes and interned strings.
//    ~Ctx() { 
//        rgs::for_each(ast_, [](Expr* expr) { delete expr; });
//    }
//};

//struct Lexer {
//    Ctx* ctx_{};
//    TokStream& tok_stream_;
//    u16 fid_{};
//    char c_{};
//    const char* curr_{};
//    const char* end_{};
//
//    explicit Lexer(Ctx* ctx, u16 fid) : 
//        ctx_(ctx), tok_stream_(ctx->tok_streams_[fid]),
//        fid_(fid), curr_(ctx->files_[fid]->data()),
//        end_(curr_ + ctx->files_[fid]->size())
//    {
//        // Initialize the first character.
//        next_c();
//        // Initialize the first token.
//        next_tok();
//    }
//
//    auto file_start() -> const char*;
//    auto eof() -> i1;
//    auto starts_ident(char c) -> i1;
//    auto continues_ident(char c) -> i1;
//    auto next_c() -> void;
//    auto peek_c(i32 idx = 0) -> char;
//    auto next_tok() -> void;
//    auto next_tok_helper(Tok* tok) -> void;
//    auto lex_ident(Tok* tok) -> void;
//    auto lex_num(Tok* tok) -> void;
//    auto lex_str_lit(Tok* tok) -> void;
//    auto skip_whitespace() -> void;
//    auto lex_line_comment() -> void;
//    auto tok() -> Tok&;
//    auto curr_offset() -> u32;
//};
//
    //auto parse_proc_expr() -> ProcExpr*;
    //auto parse_x86_instruction_expr() -> X86InstructionExpr;
    //auto parse_x86_op_expr() -> X86OpExpr*;
    //auto parse_num() -> i64;
    //auto parse_mem_index_scale() -> x86::MemIndexScale;
    //auto parse_expr() -> Expr*;
    //
//auto fiska::fe::Parser::parse_proc_expr() -> ProcExpr* {
//    auto proc = new (ctx_) ProcExpr;
//    
//    expect_all(TK::Fn, TK::Ident);
//
//    proc->name_ = ctx_->str_pool_.save(peek_tok(-1).str_);
//
//    expect(TK::LBrace);
//    while (not at(TK::RBrace)) {
//        proc->body_.push_back(parse_x86_instruction_expr());
//    }
//    expect(TK::RBrace);
//
//    return proc;
//}
//
//auto fiska::fe::Parser::parse_x86_instruction_expr() -> X86InstructionExpr {
//    expect_all(TK::Mnemonic, TK::LParen);
//
//    x86::X86IK ik = utils::strmap_get(mnemonics, peek_tok(-2).str_);
//
//    Vec<X86OpExpr*> ops{};
//    while (not at(TK::RParen)) {
//        ops.push_back(parse_x86_op_expr());
//        consume(TK::Comma);
//    }
//    expect_all(TK::RParen, TK::SemiColon);
//
//    return X86InstructionExpr{ik, ops};
//}
//
//auto fiska::fe::Parser::parse_x86_op_expr() -> X86OpExpr* {
//    using x86::BW;
//    using x86::RI;
//
//    // Register.
//    if (match(TK::BitWidth, TK::Reg)) {
//        expect_all(TK::BitWidth, TK::Reg);
//
//        auto reg = new (ctx_) RegExpr;
//        reg->bw_ = utils::strmap_get(bws, peek_tok(-2).str_);
//        reg->id_ = utils::strmap_get(reg_ids, peek_tok(-1).str_);
//
//        return reg;
//    }
//
//    // Immediate.
//    if (match(TK::BitWidth, TK::Num) 
//        or match(TK::BitWidth, TK::Plus, TK::Num) 
//        or match(TK::BitWidth, TK::Minus, TK::Num))
//    {
//        expect(TK::BitWidth);
//
//        auto imm = new (ctx_) ImmExpr;
//        imm->bw_ = utils::strmap_get(bws, peek_tok(-1).str_);;
//        imm->value_ = parse_num();
//
//        return imm;
//    }
//
//    // Memory reference.
//    if (match(TK::At, TK::BitWidth, TK::LBracket)) {
//        expect_all(TK::At, TK::BitWidth);
//
//        auto mem_ref = new (ctx_) MemRefExpr;
//        mem_ref->bw_ = utils::strmap_get(bws, peek_tok(-1).str_);
//
//        expect(TK::LBracket);
//        if (consume(TK::Reg)) {
//            mem_ref->breg_id_ = utils::strmap_get(reg_ids, peek_tok(-1).str_);
//        }
//        expect(TK::RBracket);
//
//        expect(TK::LBracket);
//        if (consume(TK::Num)) {
//            mem_ref->scale_ = parse_mem_index_scale();
//
//            // assert that an index register exists.
//            assert(match(TK::RBracket, TK::LBracket, TK::Reg));
//        }
//        expect(TK::RBracket);
//
//        expect(TK::LBracket);
//        if (consume(TK::Reg)) {
//            mem_ref->ireg_id_ = utils::strmap_get(reg_ids, peek_tok(-1).str_);
//
//            // assert that a scale exists.
//            assert(peek_tk(-3) == TK::Num);
//        }
//        expect(TK::RBracket);
//
//        // Displacement.
//        if (at(TK::Plus, TK::Minus)) {
//            // Position independant memory reference.
//            if (match(TK::Plus, TK::Ident)) {
//                expect_all(TK::Plus, TK::Ident);
//                mem_ref->disp_ = peek_tok(-1).str_;
//
//            } else {
//                mem_ref->disp_ = parse_num();
//            }
//        }
//
//        return mem_ref;
//    }
//
//    // Absolute Memory offset.
//    if (match(TK::At, TK::BitWidth, TK::Num)
//        or match (TK::At, TK::BitWidth, TK::Ident))
//    {
//        expect_all(TK::At, TK::BitWidth);
//
//        auto moffs = new (ctx_) MoffsExpr;
//        moffs->bw_ = utils::strmap_get(bws, peek_tok(-1).str_);
//        moffs->addr_ = parse_num();
//        return moffs;
//    }
//
//    unreachable("Invalid x86Operand.");
//}
//
//// Credit to llvm: https://llvm.org/doxygen/StringRef_8cpp_source.html
//auto fiska::fe::Parser::parse_num() -> i64 {
//    i1 has_sign = consume(TK::Plus, TK::Minus);
//    expect(TK::Num);
//
//    StrRef num_lxm = peek_tok(-1).str_;
//
//    u8 radix{};
//    if (num_lxm.starts_with("0x")) { radix = 16; num_lxm.remove_prefix(2); }
//    else if (num_lxm.starts_with("0b")) { radix = 2; num_lxm.remove_prefix(2); }
//    else { radix = 10; }
//
//    StrRef curr = num_lxm;
//    u64 ret{};
//
//    while (not curr.empty()) {
//        u8 ord{};
//        if (curr[0] >= '0' and curr[0] <= '9') {
//            ord = u8(curr[0] - '0');
//        } else if (curr[0] >= 'a' and curr[0] <= 'z') {
//            ord = u8(curr[0] - 'a');
//        } else if (curr[0] >= 'A' and curr[0] <= 'Z') {
//            ord = u8(curr[0] - 'A');
//        } else {
//            break;
//        }
//
//        if (ord >= radix) { break; }
//
//        u64 old_ret = ret;
//        ret = ret * radix + ord;
//        
//        // Overflow.
//        if ((ret / radix) != old_ret) { break; }
//
//        curr.remove_prefix(1);
//    }
//
//    i1 is_negative = has_sign and peek_tok(-2).kind_ == TK::Minus;
//
//    assert( (not (is_negative and static_cast<i64>(-ret) > 0) ) and curr.empty(),
//            "Invalid number encountered: '{}{}'.",
//                has_sign ? str_of_tk(peek_tok(-2).kind_) : "",
//                num_lxm
//    );
//
//    return static_cast<i64>(is_negative ? -ret : ret);
//}
//
//auto fiska::fe::Parser::parse_mem_index_scale() -> x86::MemIndexScale {
//    i64 scale = parse_num();
//    assert((is<1LL, 2LL, 4LL, 8LL>(scale)), "Invalid scale encountered: '{}'.", scale);
//
//    return x86::MemIndexScale(scale);
//}

