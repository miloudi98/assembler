#include "lib/front_end.hh"

#include "lib/core.hh"

namespace {

// Below are the characters considered to be whitespace.
// \n: New line.
// \t: Tab.
// \r: Carriage return.
// \f: Form feed.
// \v: vertical tab.
// \a: terminal ring bell.
// \b: backspace.
// ' ': regular space.
constexpr auto is_white_space(char c) -> bool {
    return c == '\n' or c == '\t'
        or c == '\f' or c == '\a'
        or c == '\r' or c == '\v'
        or c == '\b' or c == ' ';
}

constexpr auto is_digit(char c) -> bool {
    return c >= '0' and c <= '9';
}

constexpr auto is_hex_digit(char c) -> bool {
    return is_digit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

void skip_white_space(fiska::Lexer& lxr) {
    while (is_white_space(lxr.c_)) {
        lxr.next_c();
    }
}

}  // namespace

auto fiska::Lexer::file_offset() -> u32 {
    return u32(curr_ - mod_->ctx_->get_file(fid_)->data());
}

auto fiska::Lexer::tok() -> Tok& {
    assert(mod_->tokens_.storage_.size() >= 1,
            "No token is allocated. Run 'tokens_.allocate()' before accessing the token");
    return mod_->tokens_.storage_.back();
}

void fiska::Lexer::next_c() {
    c_ = eof() ? 0 : *curr_++;
}

auto fiska::Lexer::peek_c(u32 lookahead) -> char {
    return curr_ + lookahead >= end_ ? 0 : *(curr_ + lookahead);
}


void fiska::Lexer::next_tok() {
    mod_->tokens_.allocate();
    // The reason we need to separate this function into two parts is because
    // we might recurse in the process of constructing a token and we don't
    // want to allocate a token everytime we recurse.
    // For example, When we encounter the beginning of a comment (i.e '/'), we
    // will ignore all the subsequent characters until a new line is found. Once that's done
    // we will recurse to get the next token we are interested in.
    next_tok_helper();
}

void fiska::Lexer::next_tok_helper() {
    skip_white_space(*this);

    tok().loc_.pos_ = file_offset();

    switch (c_) {
    case ',': {
        next_c();
        tok().kind_ = TK::Comma;
        break;
    }
    case ':': {
        next_c();
        tok().kind_ = TK::Colon;
        break;
    }
    case ';': {
        next_c();
        tok().kind_ = TK::SemiColon;
        break;
    }
    case '(': {
        next_c();
        tok().kind_ = TK::LParen;
        break;
    }
    case ')': {
        next_c();
        tok().kind_ = TK::RParen;
        break;
    }
    case '{': {
        next_c();
        tok().kind_ = TK::LBrace;
        break;
    }
    case '}': {
        next_c();
        tok().kind_ = TK::RBrace;
        break;
    }
    case '[': {
        next_c();
        tok().kind_ = TK::LBracket;
        break;
    }
    case ']': {
        next_c();
        tok().kind_ = TK::RBracket;
        break;
    }
    case '@': {
        next_c();
        tok().kind_ = TK::At;
        break;
    }
    case '+': {
        next_c();
        tok().kind_ = TK::Plus;
        break;
    }
    case '-': {
        next_c();
        tok().kind_ = TK::Minus;
        break;
    }
    case '/': {
        // Eat the first '/'
        next_c();
        if (not at('/')) {
            todo("Compile error. Expected a '/' after the first '/' to begin a line comment");
        }
        // Eat the second '/'
        next_c();
        lex_comment();
        return next_tok_helper();
    }
    case '0': {
        char cc = peek_c(1);
        if (is_digit(cc)) {
            todo("Compile error. Leading zeros in non-hex digits is not supported "
                    "Hex digits must be preceded with a '0x' prefix");
        }
        // Encountered a '0x' prefix.
        if (cc == 'x') {
            // Remove the '0x' prefix.
            next_c();
            next_c();
            lex_hex_digit();
            break;
        }
        if (cc and *cc == 'x') {
            // Remove the '0x' prefix before lexing the hex digit.
            NextChar();
            NextChar();

            LexHexDigit();
            break;
        }
        [[fallthrough]];
    }
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9': {
        LexDigit();
        break;
    }
    case '\0': {
        next_c();
        tok().kind_ = TK::Eof;
        break;
    }
    }  // switch
}

void fiska::Lexer::lex_comment() {
    while (not at('\n')) {
        next_c();
    }
}

void fiska::Lexer::lex_hex_digit() {
}
