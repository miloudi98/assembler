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
    Lparen,
    // ')'
    Rparen,
    // '{'
    Lbrace,
    // '}'
    Rbrace,
    // '['
    Lbracket,
    // ']'
    Rbracket,
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
    Register,
    // x86 mnemonic. (e.g 'mov', 'addcx')
    Mnemonic,

    Eof,
};

struct Tok {
    TK kind_{};
    StrRef str_{};
    Location loc{};
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
    TokStream& tokens_;
    Context* ctx_;

    explicit Lexer(const File& file, TokStream& tokens)
        : curr_(file.code_.data()),
        end_(file.code_.data() + file.code_.size()),
        fid_(file.fid_), 
        tokens_(tokens)
    {}
};

}  // namespace fiska

#endif
