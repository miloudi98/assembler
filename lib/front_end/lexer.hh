#ifndef __X86_ASSEMBLER_LIB_FRONT_END_LEXER_HH__
#define __X86_ASSEMBLER_LIB_FRONT_END_LEXER_HH__

#include "lib/support/core.hh"
#include "lib/support/fe_utils.hh"
#include "lib/x86/common.hh"

namespace fiska::x86::fe {

// Token kind.
enum struct TK {
    Invalid,

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
    // '*'
    Star,
    // '='
    Eq,

    // Multi-character tokens.
    // Identifier
    Ident,
    // String literal.
    StrLit,
    // Number
    Num,
    // Bit width. (e.g. 'b8', 'b16', 'b32')
    BitWidth,
    // keyword 'let'
    Let,
    // keyword 'fn'
    Fn,
    // keyword 'section'
    Section,
    // x86 Registers.
    Reg,
    // x86 mnemonic. (e.g 'mov', 'addcx')
    Mnemonic,
    // End of file.
    Eof,

    Unknown,
};

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

struct Ctx;
struct Lexer {
    Ctx* ctx_{};
    TokStream& tok_stream_;
    u16 fid_{};
    char c_{};
    const char* curr_{};
    const char* end_{};

    explicit Lexer(Ctx* ctx, u16 fid);  

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
    auto lex_str_lit(Tok* tok) -> void;
    auto skip_whitespace() -> void;
    auto lex_line_comment() -> void;
    auto tok() -> Tok&;
    auto curr_offset() -> u32;

    static auto str_of_tk(TK tk) -> StrRef;
    static const utils::StringMap<TK> keywords;
    static const utils::StringMap<X86Mnemonic> mnemonics;
    static const utils::StringMap<RI> rids;
    static const utils::StringMap<BW> bws;
};


}  // namespace fiska::x86::fe

#endif // __X86_ASSEMBLER_LIB_FRONT_END_LEXER_HH__
