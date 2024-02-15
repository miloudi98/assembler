#ifndef __X86_ASSEMBLER_LIB_FRONT_END_LEXER_HH__
#define __X86_ASSEMBLER_LIB_FRONT_END_LEXER_HH__

#include "lib/support/core.hh"
#include "lib/x86/common.hh"
#include "lib/front_end/ctx.hh"

namespace fiska::x86::fe {

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

    // TODO: Remove everything from here it's not needed anymore.
    static auto str_of_tk(TK tk) -> StrRef;
    static auto bw_of_str(StrRef bw) -> BW;
    static auto rid_of_str(StrRef rid) -> RI;
    static auto mmic_of_str(StrRef mmic) -> X86Mnemonic;
    static const utils::StringMap<TK> keywords;
    static const utils::StringMap<X86Mnemonic> mmics;
    static const utils::StringMap<RI> rids;
    static const utils::StringMap<BW> bws;
};

}  // namespace fiska::x86::fe

#endif // __X86_ASSEMBLER_LIB_FRONT_END_LEXER_HH__
