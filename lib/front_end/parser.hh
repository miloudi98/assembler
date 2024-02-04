#ifndef __X86_ASSEMBLER_LIB_PARSER_HH__
#define __X86_ASSEMBLER_LIB_PARSER_HH__

#include "lib/core.hh"
#include "lib/x86_utils.hh"
#include "lib/front_end/support.hh"

namespace fiska::fe {

struct Ctx;

// Token kind.
enum struct TK {
    // One character tokens.
    // '('
    LParen,
    // ')'
    RParen,
    // '{'
    LBrace,
    // '}'
    RBrace,
    // '['
    LBracket,
    // ']'
    RBracket,
    // '@'
    At,
    // ';'
    SemiColon,
    // ':'
    Colon,
    // ','
    Comma,
    // '+'
    Plus,
    // '-'
    Minus,
    // '/'
    Slash,

    // Multi-character tokens.
    // Identifier
    Ident,
    // Number
    Num,
    // Bit width. (e.g. 'b8', 'b16', 'b32')
    BitWidth,
    // keyword 'fn'
    Fn,
    // x86 Registers.
    Reg,
    // x86 mnemonic. (e.g 'mov', 'addcx')
    Mnemonic,

    // End of file.
    Eof,
};
auto str_of_tk(TK tk) -> StrRef;

struct Tok {
    TK kind_{};
    StrRef str_{};
    Location loc_{};
};

struct TokStream {
    using Storage = Vec<Tok>;
    using Iterator = Vec<Tok>::iterator;
    Storage storage_;

    auto alloc_tok() -> Tok* { return &storage_.emplace_back(); }
    auto back() -> Tok& { return storage_.back(); }
    auto begin() -> Iterator { return storage_.begin(); }
    auto end() -> Iterator { return storage_.end(); }
};

// Expression kind.
enum struct EK {
    ProcExpr,
};

struct Expr {
    EK kind_{};

    Expr(EK kind) : kind_(kind) {}
    virtual ~Expr() = default;

    // Create an expression and bind it to the global context.
    void* operator new(usz sz, Ctx* ctx);
    // Disallow creating expressions with no global context.
    void* operator new(usz sz) = delete;
};

struct ProcExpr : Expr {
    StrRef name_{};
    x86::X86Instruction::List instructions_{};

    ProcExpr() : Expr(EK::ProcExpr) {}
};

// Global context for the entire front end.
struct Ctx {
    // List of files loaded.
    Vec<Box<File>> files_{};
    // Tokens of each file loaded.
    Vec<TokStream> tok_streams_{};
    // Ast nodes allocated.
    Vec<Expr*> ast_{};
    // Pool of interned strings.
    StringInterner str_pool_{};

    // Load file to memory.
    auto load_file(const fs::path& path) -> File*;
    // Return the file with fid |fid|.
    auto get_file(u16 fid) -> File*;

    // Deallocate all ast nodes and interned strings.
    ~Ctx() { 
        rgs::for_each(ast_, [](Expr* expr) { delete expr; });
    }
};

struct Lexer {
    Ctx* ctx_{};
    TokStream& tok_stream_;
    u16 fid_{};
    char c_{};
    const char* curr_{};
    const char* end_{};

    explicit Lexer(Ctx* ctx, u16 fid) : 
        ctx_(ctx), tok_stream_(ctx->tok_streams_[fid]),
        fid_(fid), curr_(ctx->files_[fid]->data()),
        end_(curr_ + ctx->files_[fid]->size())
    {
        // Initialize the first character.
        next_c();
        // Initialize the first token.
        next_tok();
    }

    auto file_start() -> const char*;
    auto eof() -> i1;
    auto starts_ident(char c) -> i1;
    auto continues_ident(char c) -> i1;
    auto next_c() -> void;
    auto peek_c(i32 idx = 0) -> char;
    auto next_tok() -> void;
    auto next_tok_helper(Tok* tok) -> void;
    auto lex_ident(Tok* tok) -> void;
    auto lex_num(Tok* tok) -> void;
    auto skip_whitespace() -> void;
    auto lex_line_comment() -> void;
    auto tok() -> Tok&;
    auto curr_offset() -> u32;
};

struct Parser {
    Ctx* ctx_{};
    u16 fid_{};
    TokStream::Iterator tok_stream_it_;

    explicit Parser(Ctx* ctx, u16 fid) :
        ctx_(ctx), fid_(fid)
    {
        Lexer lxr{ctx, fid};
        while (lxr.tok().kind_ != TK::Eof) { lxr.next_tok(); }

        for (auto it = ctx_->tok_streams_[fid_].begin(); it != ctx_->tok_streams_[fid_].end(); ++it) {
            fmt::print("<{}>\n", str_of_tk((*it).kind_));
        }

        tok_stream_it_ = ctx_->tok_streams_[fid_].begin();
    } 

    auto next_tok() -> void;
    auto tok() -> const Tok&;
    auto peek_tok(i32 idx = 0) -> const Tok&;
    auto parse_proc_expr() -> ProcExpr*;
    auto parse_x86_instruction() -> x86::X86Instruction;
    auto parse_x86_op() -> x86::X86Op;


    auto at(std::same_as<TK> auto... tk) -> i1 {
        return ((tok().kind_ == tk) or ...);
    }

    auto consume(std::same_as<TK> auto... tk) -> i1 {
        if (not at(tk...)) { return false; }
        next_tok();
        return true;
    }

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
