#include "lib/frontend/lexer.hh"
#include "lib/frontend/ctx.hh"
#include "lib/common/base.hh"
#include "lib/common/support.hh"

namespace fiska::assembler::frontend {
namespace {
    
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

struct Lexer {
    struct SectionIdentTag {};

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
        next();
    }

    auto file_data() const -> const char* {
        return ctx_->files_[fid_]->data();
    }

    auto cur_offset() const -> u32 {
        assert(cur_ >= file_data() + 1, "Unsigned overflow.");
        return u32(cur_ - file_data() - 1);
    }
    
    auto eof() const -> i1 { return cur_ >= end_; }

    auto next() -> void {
        c_ = eof() ? 0 : *cur_;
        cur_ += not eof();
    }

    auto peek(i32 idx = 0) const -> char {
        assert(cur_ + idx >= cur_ and cur_ + idx < end_, "Index out of bounds.");
        return *(cur_ + idx);
    }

    auto tok() -> Tok& {
        assert(not tok_stream_.empty());
        return tok_stream_.back();
    }
    
    auto last_tok_lexed() -> Tok {
        if (tok_stream_.size() < 2) { return Tok{}; }
        return tok_stream_[i32(tok_stream_.size()) - 2];
    }

    auto skip_whitespace() -> void {
        while (c_ and std::isspace(static_cast<u8>(c_))) {
            next();
        }
    }

    auto lex_line_comment() -> void {
        // Eat '//'.
        next();
        next();
        while (not eof() and c_ != '\n') { 
            next();
        }
    }

    auto lex_num() -> void {
        do {
            next();
        } while (std::isalnum(static_cast<u8>(c_)));
        tok().kind_ = TK::Num;
        tok().str_ = ctx_->string_pool_.save( StrRef{file_data() + tok().span_.pos_, cur_offset() - tok().span_.pos_} );
    }

    auto lex_ident() -> void {
        do {
            next();
        } while (continues_ident(c_));
        tok().str_ = ctx_->string_pool_.save(StrRef{file_data() + tok().span_.pos_, cur_offset() - tok().span_.pos_});
        tok().kind_ = kKeywords.contains(tok().str_) ? utils::strmap_get(kKeywords, tok().str_) : TK::Ident;
    }

    auto lex_ident(SectionIdentTag) -> void {
        do {
            next();
        } while (continues_ident(c_) or c_ == '.');
        tok().str_ = ctx_->string_pool_.save(StrRef{file_data() + tok().span_.pos_, cur_offset() - tok().span_.pos_});
        tok().kind_ = kKeywords.contains(tok().str_) ? utils::strmap_get(kKeywords, tok().str_) : TK::Ident;
    }

    auto lex_str_lit() -> void {
        // Eat the opening '"'.
        next();
        while (not eof() and c_ != '"') { next(); }
        tok().kind_ = TK::StrLit;
        tok().str_ = ctx_->string_pool_.save( StrRef{file_data() + tok().span_.pos_, cur_offset() - tok().span_.pos_} );
        // Eat the closing '"' or emit an error.
        assert(c_ == '"');
        next();
    }

    auto next_tok() -> void {
        tok_stream_.alloc();
        next_tok_impl();
    }

    auto next_tok_impl() -> void {
        skip_whitespace();

        tok().span_.fid_ = fid_;
        tok().span_.pos_ = cur_offset();

        switch (c_) {
        case 0: {
            next();
            tok().kind_ = TK::Eof;
            break;
        }
        case ',': {
            next();
            tok().kind_ = TK::Comma;
            break;
        }
        case ':': {
            next();
            tok().kind_ = TK::Colon;
            break;
        }
        case '=': {
            next();
            tok().kind_ = TK::Eq;
            break;
        }
        case ';': {
            next();
            tok().kind_ = TK::SemiColon;
            break;
        }
        case '(': {
            next();
            tok().kind_ = TK::LParen;
            break;
        }
        case ')': {
            next();
            tok().kind_ = TK::RParen;
            break;
        }
        case '{': {
            next();
            tok().kind_ = TK::LBrace;
            break;
        }
        case '}': {
            next();
            tok().kind_ = TK::RBrace;
            break;
        }
        case '[': {
            next();
            tok().kind_ = TK::LBracket;
            break;
        }
        case ']': {
            next();
            tok().kind_ = TK::RBracket;
            break;
        }
        case '@': {
            next();
            tok().kind_ = TK::At;
            break;
        }
        case '+': {
            next();
            tok().kind_ = TK::Plus;
            break;
        }
        case '-': {
            next();
            tok().kind_ = TK::Minus;
            break;
        }
        case '/': {
            if (peek() == '/') {
                lex_line_comment();
                next_tok_impl();
                return;
            } else {
                next();
                tok().kind_ = TK::Slash;
            }
            break;
        }
        case '"': {
            lex_str_lit();
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
            lex_num();
            break;
        }
        default: {
            if (starts_ident(c_)) {
                lex_ident();
                break;
            }
            if (c_ == '.' and last_tok_lexed().kind_ == TK::Section) {
                lex_ident(SectionIdentTag{});
                break;
            }

            next();
            tok().kind_ = TK::Unknown;
        }
        } // switch

        // Set the length of the token.
        tok().span_.len_ = (not eof()) * u16(cur_offset() - tok().span_.pos_ - 1);
        tok().span_.len_ += tok().has_single_char_spelling();
    }
};

} // namespace
} // namespace fiska::assembler::frontend

auto fiska::assembler::frontend::lex(Ctx* ctx, u16 fid) -> TokStream {
    Lexer lxr(ctx, fid);
    do {
        lxr.next_tok();
    } while (lxr.tok().kind_ != TK::Eof);

    return std::move(lxr.tok_stream_);
}
