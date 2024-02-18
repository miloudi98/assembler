#include "lib/frontend/lexer.hh"
#include "lib/common/base.hh"
#include "lib/common/support.hh"

namespace fiska::assembler::frontend {
namespace {

// Forward declarations.
struct Lexer;
auto next_tok_helper(Lexer*, Tok*) -> void;
auto next(Lexer*) -> void;

struct Lexer {
    const char* cur_{};
    const char* end_{};
    Ctx* ctx_{};
    u16 fid_{};
    char c_{};
    TokStream tok_stream_;

    explicit Lexer(Ctx* ctx, u16 fid) :
        cur_(ctx->files_[fid]->data()),
        end_(ctx->files_[fid]->data() + ctx->files_[fid]->size()),
        ctx_(ctx), fid_(fid)
    {
        // Initialize |c_| with the first character of the file.
        next(this);
    }
};

const utils::StringMap<TK> kKeywords = {
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
};

constexpr auto starts_ident(char c) -> i1 {
    return c == '_' or std::isalpha(static_cast<u8>(c));
}

constexpr auto continues_ident(char c) -> i1 {
    return c == '_' or std::isalnum(static_cast<u8>(c));
}

auto file_start(Lexer* lxr) -> const char* {
    return lxr->ctx_->files_[lxr->fid_]->data();
}

auto curr_offset(Lexer* lxr) -> u32 {
    assert(lxr->cur_ >= file_start(lxr) + 1, "u32 overflow detected.");
    return u32(lxr->cur_ - file_start(lxr) - 1);
}

auto eof(Lexer* lxr) -> i1 { 
    return lxr->cur_ >= lxr->end_;
}

auto next(Lexer* lxr) -> void {
    lxr->c_ = eof(lxr) ? 0 : *lxr->cur_;
    lxr->cur_ += not eof(lxr);
}

auto peek(Lexer* lxr, i32 idx = 0) -> char {
    assert(lxr->cur_ + idx >= lxr->cur_ and lxr->cur_ + idx < lxr->end_, "Index out of bounds.");
    return *(lxr->cur_ + idx);
}

auto skip_whitespace(Lexer* lxr) -> void {
    while (lxr->c_ and std::isspace(lxr->c_)) { next(lxr); }
}

auto next_tok(Lexer* lxr) -> void {
    Tok* tok = lxr->tok_stream_.alloc();
    tok->span_.fid_ = lxr->fid_;
    next_tok_helper(lxr, tok);
}

auto lex_line_comment(Lexer* lxr) -> void {
    // Eat '//'.
    next(lxr);
    next(lxr);

    while (not eof(lxr) and lxr->c_ != '\n') { next(lxr); }
}

auto lex_str_lit(Lexer* lxr, Tok* tok) -> void {
    // Eat the opening '"'.
    next(lxr);

    while (not eof(lxr) and lxr->c_ != '"') { next(lxr); }

    tok->kind_ = TK::StrLit;
    tok->str_ = lxr->ctx_->str_pool_.save( StrRef{file_start(lxr) + tok->span_.pos_, curr_offset(lxr) - tok->span_.pos_} );
    tok->span_.len_ = u16(tok->str_.size());

    // Eat the closing '"' or emit an error.
    if (lxr->c_ != '"') {
        Diagnostic{lxr->ctx_, "Missing enclosing '\"'.", lxr->tok_stream_.back().span_};
    }
    next(lxr);

}

auto lex_num(Lexer* lxr, Tok* tok) -> void {
    do {
        next(lxr);
    } while (std::isalnum(static_cast<u8>(lxr->c_)));

    tok->kind_ = TK::Num;
    tok->str_ = lxr->ctx_->str_pool_.save( StrRef{file_start(lxr) + tok->span_.pos_, curr_offset(lxr) - tok->span_.pos_} );
    tok->span_.len_ = u16(tok->str_.size());
}

auto lex_ident(Lexer* lxr, Tok* tok) -> void {
    do {
        next(lxr);
    } while (continues_ident(lxr->c_));

    tok->str_ = lxr->ctx_->str_pool_.save(StrRef{file_start(lxr) + tok->span_.pos_, curr_offset(lxr) - tok->span_.pos_});
    tok->kind_ = kKeywords.contains(tok->str_) ? utils::strmap_get(kKeywords, tok->str_) : TK::Ident;
    tok->span_.len_ = u16(tok->str_.size());
}

auto next_tok_helper(Lexer* lxr, Tok* tok) -> void {
    skip_whitespace(lxr);

    tok->span_.pos_ = curr_offset(lxr);

    switch (lxr->c_) {
    case 0: {
        next(lxr);
        tok->kind_ = TK::Eof;
        break;
    }
    case ',': {
        next(lxr);
        tok->kind_ = TK::Comma;
        tok->span_.len_ = 1;
        break;
    }
    case ':': {
        next(lxr);
        tok->kind_ = TK::Colon;
        tok->span_.len_ = 1;
        break;
    }
    case '=': {
        next(lxr);
        tok->kind_ = TK::Eq;
        tok->span_.len_ = 1;
        break;
    }
    case ';': {
        next(lxr);
        tok->kind_ = TK::SemiColon;
        tok->span_.len_ = 1;
        break;
    }
    case '(': {
        next(lxr);
        tok->kind_ = TK::LParen;
        tok->span_.len_ = 1;
        break;
    }
    case ')': {
        next(lxr);
        tok->kind_ = TK::RParen;
        tok->span_.len_ = 1;
        break;
    }
    case '{': {
        next(lxr);
        tok->kind_ = TK::LBrace;
        tok->span_.len_ = 1;
        break;
    }
    case '}': {
        next(lxr);
        tok->kind_ = TK::RBrace;
        tok->span_.len_ = 1;
        break;
    }
    case '[': {
        next(lxr);
        tok->kind_ = TK::LBracket;
        tok->span_.len_ = 1;
        break;
    }
    case ']': {
        next(lxr);
        tok->kind_ = TK::RBracket;
        tok->span_.len_ = 1;
        break;
    }
    case '@': {
        next(lxr);
        tok->kind_ = TK::At;
        tok->span_.len_ = 1;
        break;
    }
    case '+': {
        next(lxr);
        tok->kind_ = TK::Plus;
        tok->span_.len_ = 1;
        break;
    }
    case '-': {
        next(lxr);
        tok->kind_ = TK::Minus;
        tok->span_.len_ = 1;
        break;
    }
    case '/': {
        if (peek(lxr) == '/') {
            lex_line_comment(lxr);
            return next_tok_helper(lxr, tok);
        } else {
            next(lxr);
            tok->span_.len_ = 1;
            tok->kind_ = TK::Slash;
        }
        break;
    }
    case '"': {
        lex_str_lit(lxr, tok);
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
        lex_num(lxr, tok);
        break;
    }
    default: {
        if (starts_ident(lxr->c_)) {
            lex_ident(lxr, tok);
            break;
        }

        next(lxr);
        tok->kind_ = TK::Unknown;
        tok->span_.len_ = 1;
    }
    } // switch
}

} // namespace
} // namespace fiska::assembler::frontend

auto fiska::assembler::frontend::lex(Ctx* ctx, u16 fid) -> TokStream {
    Lexer lxr(ctx, fid);
    while (lxr.tok_stream_.empty() or lxr.tok_stream_.back().kind_ != TK::Eof) {
        next_tok(&lxr);
    }
    return std::move(lxr.tok_stream_);
}
