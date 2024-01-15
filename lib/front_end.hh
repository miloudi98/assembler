#ifndef __X86_ASSEMBLER_X86_FRONT_END_HH__
#define __X86_ASSEMBLER_X86_FRONT_END_HH__

#include "lib/core.hh"
#include "lib/x86_core.hh"

namespace fiska {

enum struct BW : u16 {
    B8 = 8,
    B16 = 16,
    B32 = 32,
    B64 = 64
};

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
    // Line comment (e.g '// Line comment')
    LComment,

    Eof,
};

struct Tok {
    TK kind_{};
    StrRef str_{};
    Location loc_{};
};

struct Expr {
};

struct ProcExpr : public Expr {
};

struct TokStream {
    using Storage = Vec<Tok>;
    using iterator = Vec<Tok>::iterator;
    Storage storage_;

    auto allocate() -> Tok* { return &storage_.emplace_back(); }
    auto begin() -> iterator { return storage_.begin(); }
    auto end() -> iterator { return storage_.end(); }
};

struct Module {
    StringInterner strings_;
    TokStream tokens_;
    Vec<Expr*> ast_;
    Vec<ProcExpr*> procs_;
    Context* ctx_;

    Module(const Module&) = delete;
    Module(Module&&) = delete;
    Module& operator=(const Module&) = delete;
    Module& operator=(Module&&) = delete;
};


struct Lexer {
    const char* curr_{};
    const char* end_{};
    char c_{};
    u16 fid_{};
    Module* mod_{};

    explicit Lexer(const File& file, Module* mod)
        : curr_(file.data()),
        end_(file.data() + file.size()),
        fid_(file.fid_), 
        mod_(mod)
    {}

    template <typename... Cs>
    requires (std::same_as<Cs, char> and ...)
    auto at(Cs... chars) -> bool {
        return ((c_ == chars) or ...);
    }

    auto eof() -> bool { return curr_ >= end_; }
    auto tok() -> Tok&;
    // A lookahead of '0' means that you are peeking the current char which is
    // the same thing as |c_|. |peek_c(1)| would yield the char right after |c_|.
    auto peek_c(u32 lookahead) -> char;
    auto file_offset() -> u32;
    void next_c();
    void next_tok();
    void next_tok_helper();
    void lex_comment();
};

}  // namespace fiska

#endif
