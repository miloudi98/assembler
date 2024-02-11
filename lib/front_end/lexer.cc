#include "lib/front_end/lexer.hh"
#include "lib/support/fe_utils.hh"
#include "lib/support/core.hh"
#include "lib/front_end/ctx.hh"

using fiska::x86::fe::TK;
using fiska::x86::BW;
using fiska::x86::RI;
using fiska::x86::X86Mnemonic;

const utils::StringMap<TK> fiska::x86::fe::Lexer::keywords = {
    {"fn", TK::Fn}, {"let", TK::Let}, {"section", TK::Section},

    {"b8", TK::BitWidth}, {"b16", TK::BitWidth}, {"b32", TK::BitWidth}, {"b64", TK::BitWidth},

    {"rax", TK::Reg}, {"rcx", TK::Reg}, {"rdx", TK::Reg}, {"rbx", TK::Reg}, {"rsp", TK::Reg}, {"rbp", TK::Reg},
    {"rsi", TK::Reg}, {"rdi", TK::Reg}, {"rip", TK::Reg}, {"r8", TK::Reg}, {"r9", TK::Reg}, {"r10", TK::Reg},
    {"r11", TK::Reg}, {"r12", TK::Reg}, {"r13", TK::Reg}, {"r14", TK::Reg}, {"r15", TK::Reg},

    {"es", TK::Reg}, {"cs", TK::Reg}, {"ss", TK::Reg}, {"ds", TK::Reg}, {"fs", TK::Reg}, {"gs", TK::Reg},

    {"cr0", TK::Reg}, {"cr1", TK::Reg}, {"cr2", TK::Reg}, {"cr3", TK::Reg}, {"cr4", TK::Reg},
    {"cr5", TK::Reg}, {"cr6", TK::Reg}, {"cr7", TK::Reg}, {"cr8", TK::Reg}, {"cr9", TK::Reg},
    {"cr10", TK::Reg}, {"cr11", TK::Reg}, {"cr12", TK::Reg}, {"cr13", TK::Reg}, {"cr14", TK::Reg},
    {"cr15", TK::Reg},

    {"dbg0", TK::Reg}, {"dbg1", TK::Reg}, {"dbg2", TK::Reg}, {"dbg3", TK::Reg}, {"dbg4", TK::Reg},
    {"dbg5", TK::Reg}, {"dbg6", TK::Reg}, {"dbg7", TK::Reg}, {"dbg8", TK::Reg}, {"dbg9", TK::Reg},
    {"dbg10", TK::Reg}, {"dbg11", TK::Reg}, {"dbg12", TK::Reg}, {"dbg13", TK::Reg}, {"dbg14", TK::Reg},
    {"dbg15", TK::Reg},

    {"mov", TK::Mnemonic}, {"add", TK::Mnemonic}, {"adc", TK::Mnemonic}, {"syscall", TK::Mnemonic},
};

const utils::StringMap<X86Mnemonic> fiska::x86::fe::Lexer::mnemonics = {
    {"mov", X86Mnemonic::Mov},
    {"add", X86Mnemonic::Add},
    {"adc", X86Mnemonic::Adc},
    {"syscall", X86Mnemonic::Syscall},
};

const utils::StringMap<RI> reg_ids = {
    {"rax", RI::Rax}, {"rcx", RI::Rcx}, {"rdx", RI::Rdx}, {"rbx", RI::Rbx}, {"rsp", RI::Rsp}, {"rbp", RI::Rbp},
    {"rsi", RI::Rsi}, {"rdi", RI::Rdi}, {"rip", RI::Rip}, {"r8", RI::R8}, {"r9", RI::R9}, {"r10", RI::R10},
    {"r11", RI::R11}, {"r12", RI::R12}, {"r13", RI::R13}, {"r14", RI::R14}, {"r15", RI::R15}, {"rah", RI::Rah},
    {"Rch", RI::Rch}, {"Rdh", RI::Rdh}, {"Rbh", RI::Rbh},

    {"es", RI::Es}, {"cs", RI::Cs}, {"ss", RI::Ss}, {"ds", RI::Ds}, {"fs", RI::Fs}, {"gs", RI::Gs},

    {"cr0", RI::Cr0}, {"cr1", RI::Cr1}, {"cr2", RI::Cr2}, {"cr3", RI::Cr3}, {"cr4", RI::Cr4},
    {"cr5", RI::Cr5}, {"cr6", RI::Cr6}, {"cr7", RI::Cr7}, {"cr8", RI::Cr8}, {"cr9", RI::Cr9},
    {"cr10", RI::Cr10}, {"cr11", RI::Cr11}, {"cr12", RI::Cr12}, {"cr13", RI::Cr13}, {"cr14", RI::Cr14}, 
    {"cr15", RI::Cr15},

    {"dbg0", RI::Dbg0}, {"dbg1", RI::Dbg1}, {"dbg2", RI::Dbg2}, {"dbg3", RI::Dbg3}, {"dbg4", RI::Dbg4},
    {"dbg5", RI::Dbg5}, {"dbg6", RI::Dbg6}, {"dbg7", RI::Dbg7}, {"dbg8", RI::Dbg8}, {"dbg9", RI::Dbg9},
    {"dbg10", RI::Dbg10}, {"dbg11", RI::Dbg11}, {"dbg12", RI::Dbg12}, {"dbg13", RI::Dbg13}, {"dbg14", RI::Dbg14},
    {"dbg15", RI::Dbg15},
};

const utils::StringMap<BW> fiska::x86::fe::Lexer::bws = {
    {"b8", BW::B8}, {"b16", BW::B16}, {"b32", BW::B32}, {"b64", BW::B64},
};

auto fiska::x86::fe::Lexer::str_of_tk(TK tk) -> StrRef {
    switch (tk) {
    case TK::LParen: return "(";
    case TK::RParen: return ")";
    case TK::LBrace: return "{";
    case TK::RBrace: return "}";
    case TK::LBracket: return "[";
    case TK::RBracket: return "]";
    case TK::At: return "@";
    case TK::SemiColon: return ";";
    case TK::Colon: return ":";
    case TK::Comma: return ",";
    case TK::Plus: return "+";
    case TK::Minus: return "-";
    case TK::Slash: return "/";
    case TK::Star: return "*";
    case TK::Eq: return "=";
    case TK::StrLit: return "STR_LIT";
    case TK::Ident: return "IDENTIFIER";
    case TK::Section: return "IDENTIFIER";
    case TK::Num: return "NUMBER";
    case TK::BitWidth: return "BIT_WIDTH";
    case TK::Fn: return "FN";
    case TK::Let: return "LET";
    case TK::Reg: return "REG";
    case TK::Mnemonic: return "MNEMONIC";
    case TK::Unknown: return "UNKNOWN";
    case TK::Invalid: return "INVALID";
    case TK::Eof: return "EOF";
    } // switch
    unreachable();
}

fiska::x86::fe::Lexer::Lexer(Ctx* ctx, u16 fid) :
    ctx_(ctx), tok_stream_(ctx->tok_streams_[fid]),
    fid_(fid), curr_(ctx->files_[fid]->data()),
    end_(curr_ + ctx->files_[fid]->size())
{
    // Initialize the first character.
    next_c();
    // Initialize the first token.
    next_tok();
}

auto fiska::x86::fe::Lexer::file_start() -> const char* {
    return ctx_->files_[fid_]->data();
}

auto fiska::x86::fe::Lexer::eof() -> i1 {
    return curr_ >= end_;
}

auto fiska::x86::fe::Lexer::starts_ident(char c) -> i1 {
    return c == '_' or std::isalpha(c);
}

auto fiska::x86::fe::Lexer::continues_ident(char c) -> i1 {
    return std::isalnum(c);
}

auto fiska::x86::fe::Lexer::next_c() -> void {
    if (eof()) {
        c_ = 0;
        return;
    }
    c_ = *curr_++;
}

auto fiska::x86::fe::Lexer::peek_c(i32 idx) -> char {
    if (not (curr_ + idx >= curr_ and curr_ + idx < end_)) { return 0; }
    return *(curr_ + idx);
}

auto fiska::x86::fe::Lexer::skip_whitespace() -> void {
    while (c_ and std::isspace(c_)) { next_c(); }
}

auto fiska::x86::fe::Lexer::lex_line_comment() -> void {
    // Eat '//'.
    next_c();
    next_c();

    while (not eof() and c_ != '\n') { next_c(); }
}

auto fiska::x86::fe::Lexer::lex_num(Tok* tok) -> void {
    do {
        next_c();
    } while (std::isalnum(c_));

    tok->kind_ = TK::Num;
    tok->str_ = ctx_->str_pool_.save( StrRef{file_start() + tok->loc_.pos_, curr_offset() - tok->loc_.pos_} );
    tok->loc_.len_ = u32(tok->str_.size());
}

auto fiska::x86::fe::Lexer::lex_str_lit(Tok* tok) -> void {
    // Eat the opening '"'.
    next_c();
    while (not eof() and c_ != '"') { next_c(); }
    // Eat the closing '"';
    assert(c_ == '"', "Missing enclosing '\"' in the string literal.");
    next_c();

    tok->kind_ = TK::StrLit;
    tok->str_ = ctx_->str_pool_.save( StrRef{file_start() + tok->loc_.pos_, curr_offset() - tok->loc_.pos_} );
    tok->loc_.len_ = u32(tok->str_.size());
}

auto fiska::x86::fe::Lexer::lex_ident(Tok* tok) -> void {
    do {
        next_c();
    } while (continues_ident(c_));

    StrRef lxm{ file_start() + tok->loc_.pos_, curr_offset() - tok->loc_.pos_};
    tok->str_ = lxm;
    tok->kind_ = keywords.contains(lxm)
                ? utils::strmap_get(keywords, lxm)
                : TK::Ident;
    tok->loc_.len_ = u32(tok->str_.size());
}

auto fiska::x86::fe::Lexer::next_tok() -> void {
    // The reason we need to separate this function into two parts is because
    // we might recurse in the process of constructing a token and we don't
    // want to allocate a token everytime we recurse.
    // For instance, when we encounter the start of a comment (i.e '/'),
    // we skip all following characters until a new line is reached.
    // Once that's completed we recurse to get the next token.
    Tok* tok = tok_stream_.alloc_tok();
    tok->loc_.fid_ = fid_;
    next_tok_helper(tok);
}

auto fiska::x86::fe::Lexer::next_tok_helper(Tok* tok) -> void {
    skip_whitespace();

    tok->loc_.pos_ = curr_offset();

    switch (c_) {
    case 0: {
        next_c();
        tok->kind_ = TK::Eof;
        break;
    }
    case ',': {
        next_c();
        tok->kind_ = TK::Comma;
        break;
    }
    case ':': {
        next_c();
        tok->kind_ = TK::Colon;
        break;
    }
    case '=': {
        next_c();
        tok->kind_ = TK::Eq;
        break;
    }
    case ';': {
        next_c();
        tok->kind_ = TK::SemiColon;
        break;
    }
    case '(': {
        next_c();
        tok->kind_ = TK::LParen;
        break;
    }
    case ')': {
        next_c();
        tok->kind_ = TK::RParen;
        break;
    }
    case '{': {
        next_c();
        tok->kind_ = TK::LBrace;
        break;
    }
    case '}': {
        next_c();
        tok->kind_ = TK::RBrace;
        break;
    }
    case '[': {
        next_c();
        tok->kind_ = TK::LBracket;
        break;
    }
    case ']': {
        next_c();
        tok->kind_ = TK::RBracket;
        break;
    }
    case '@': {
        next_c();
        tok->kind_ = TK::At;
        break;
    }
    case '+': {
        next_c();
        tok->kind_ = TK::Plus;
        break;
    }
    case '-': {
        next_c();
        tok->kind_ = TK::Minus;
        break;
    }
    case '/': {
        if (peek_c() == '/') {
            lex_line_comment();
            return next_tok_helper(tok);
        } else {
            next_c();
            tok->kind_ = TK::Slash;
        }
        break;
    }
    case '"': {
        lex_str_lit(tok);
        break;
    }
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9': {
        lex_num(tok);
        break;
    }
    default: {
        if (starts_ident(c_)) {
            lex_ident(tok);
            break;
        }

        next_c();
        tok->kind_ = TK::Unknown;
    }
    } // switch
}

auto fiska::x86::fe::Lexer::tok() -> Tok& {
    return tok_stream_.back();
}

auto fiska::x86::fe::Lexer::curr_offset() -> u32 {
    assert(curr_ > file_start(), "u32 overflow detected.");

    return u32(curr_ - file_start() - 1);
}
