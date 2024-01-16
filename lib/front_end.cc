#include "lib/front_end.hh"

#include "lib/core.hh"

#include <cctype>

namespace {

using TK = fiska::TK;

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

constexpr auto is_ident_start(char c) -> bool {
    return std::isalpha(c) or c == '_';
}

constexpr auto is_ident_cont(char c) -> bool {
    return std::isalnum(c) or c == '_';
}

void skip_white_space(fiska::Lexer& lxr) {
    while (is_white_space(lxr.c_)) {
        lxr.next_c();
    }
}

const utils::StringMap<TK> keywords = {
    {"fn", TK::Fn},

    {"b8", TK::BitWidth}, {"b16", TK::BitWidth}, {"b32", TK::BitWidth}, {"b64", TK::BitWidth},

    {"rax", TK::Reg}, {"rcx", TK::Reg}, {"rdx", TK::Reg}, {"rbx", TK::Reg}, {"rsp", TK::Reg}, {"rbp", TK::Reg},
    {"rsi", TK::Reg}, {"rdi", TK::Reg}, {"rip", TK::Reg}, {"r8", TK::Reg}, {"r9", TK::Reg}, {"r10", TK::Reg},
    {"r11", TK::Reg}, {"r12", TK::Reg}, {"r13", TK::Reg}, {"r14", TK::Reg}, {"r15", TK::Reg},

    {"es", TK::Reg}, {"cs", TK::Reg}, {"ss", TK::Reg}, {"ds", TK::Reg}, {"fs", TK::Reg}, {"gs", TK::Reg},

    {"cr0", TK::Reg}, {"cr1", TK::Reg}, {"cr2", TK::Reg}, {"cr3", TK::Reg}, {"cr4", TK::Reg},
    {"cr5", TK::Reg}, {"cr6", TK::Reg}, {"cr7", TK::Reg}, {"cr8", TK::Reg}, {"cr9", TK::Reg},
    {"cr10", TK::Reg}, {"cr11", TK::Reg}, {"cr12", TK::Reg}, {"cr13", TK::Reg}, {"cr14", TK::Reg}, {"cr15", TK::Reg},

    {"dbg0", TK::Reg}, {"dbg1", TK::Reg}, {"dbg2", TK::Reg}, {"dbg3", TK::Reg}, {"dbg4", TK::Reg},
    {"dbg5", TK::Reg}, {"dbg6", TK::Reg}, {"dbg7", TK::Reg}, {"dbg8", TK::Reg}, {"dbg9", TK::Reg},
    {"dbg10", TK::Reg}, {"dbg11", TK::Reg}, {"dbg12", TK::Reg}, {"dbg13", TK::Reg}, {"dbg14", TK::Reg}, {"dbg15", TK::Reg},

    {"mov", TK::Mnemonic}
};

const utils::StringMap<RI> registers = {
    {"rax", RI::Rax}, {"rcx", RI::Rcx}, {"rdx", RI::Rdx}, {"rbx", RI::Rbx}, {"rsp", RI::Rsp}, {"rbp", RI::Rbp},
    {"rsi", RI::Rsi}, {"rdi", RI::Rdi}, {"rip", RI::Rip}, {"r8", RI::R8}, {"r9", RI::R9}, {"r10", RI::R10},
    {"r11", RI::R11}, {"r12", RI::R12}, {"r13", RI::R13}, {"r14", RI::R14}, {"r15", RI::R15},

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

}  // namespace

auto fiska::Lexer::file_offset() -> u32 {
    auto f = mod_->ctx_->get_file(fid_);
    return u32(curr_ - f->data() - 1);
}

auto fiska::Lexer::tok() -> Tok& {
    assert(mod_->tokens_.storage_.size() >= 1,
            "No token is allocated. Run 'tokens_.allocate()' before accessing the token");
    return mod_->tokens_.storage_.back();
}

void fiska::Lexer::next_c() {
    c_ = eof() ? 0 : *curr_++;
}

auto fiska::Lexer::peek_c() -> char {
    return eof() ? 0 : *curr_;
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
    tok().loc_.fid_ = fid_;

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
        if (c_ != '/') {
            error(EK::UnexpectedChar, "Expected a line comment after '/', but found '{}'.", c_);
        }
        // Eat the second '/'
        next_c();
        lex_comment();
        return next_tok_helper();
    }
    case '0': {
        char cc = peek_c();
        if (is_digit(cc)) {
            error(EK::UnexpectedChar, "Leading zeros are not allowed in a decimal number. "
                    "Hex numbers must be preceded with a '0x' prefix.");
        }
        // Encountered a '0x' prefix.
        if (cc == 'x') {
            lex_hex_digit();
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
        lex_digit();
        break;
    }
    case '\0': {
        next_c();
        tok().kind_ = TK::Eof;
        break;
    }
    default: {
        if (ident_start(c_)) {
            lex_ident();
            break;
        }
        error(EK::UnrecognizedChar, "Expected thet start of an identifier but found '{}' instead.", c_);
    }
    }  // switch
    tok().loc_.len_ = u32(file_offset() - tok().loc_.pos_);
}

void fiska::Lexer::lex_comment() {
    while (c_ != '\n') { next_c(); }
}

void fiska::Lexer::lex_hex_digit() {
    const char* hex_num_start = curr_ - 1;
    // remove the '0x' prefix.
    next_c();
    next_c();
    while (is_hex_digit(c_)) { next_c(); }

    tok().kind_ = TK::Num;
    tok().str_ = mod_->strings_.save(StrRef{hex_num_start, u32(curr_ - hex_num_start - 1)});
}

void fiska::Lexer::lex_digit() {
    const char* num_start = curr_ - 1;
    while (is_digit(c_)) { next_c(); }

    tok().kind_ = TK::Num;
    tok().str_ = mod_->strings_.save(StrRef{num_start, u32(curr_ - num_start - 1)});
}

void fiska::Lexer::lex_ident() {
    const char* ident_start = curr_ - 1;
    while (is_ident_cont(c_)) { next_c(); }

    tok().str_ = mod_->strings_.save(StrRef{ident_start, u32(curr_ - ident_start - 1)});

    if (auto k = keywords.find(tok().str_); k != keywords.end()) {
        tok().kind_ = k->second;
    } else {
        tok().kind_ = TK::Ident;
    }
}

auto fiska::Tok::spelling() -> StrRef {
    switch (kind_) {
    case TK::LParen: return "<(>";
    case TK::RParen: return "<)>";
    case TK::LBrace: return "<{>";
    case TK::RBrace: return "<}>";
    case TK::LBracket: return "<[>";
    case TK::RBracket: return "<]>";
    case TK::At: return "<@>";
    case TK::SemiColon: return "<;>";
    case TK::Colon: return "<:>";
    case TK::Comma: return "<,>";
    case TK::Plus: return "<+>";
    case TK::Minus: return "<->";
    case TK::Ident: return "<IDENTIFIER>";
    case TK::Num: return "<NUMBER>";
    case TK::BitWidth: return "<BIT_WIDTH>";
    case TK::Fn: return "<FN>";
    case TK::Reg: return "<REG>";
    case TK::Mnemonic: return "<MNEMONIC>";
    case TK::Eof: return "<EOF>";
    } // switch
    unreachable();
}

void fiska::Lexer::lex_file_into_module(File* file, Module* mod) {
    Lexer lxr{file, mod};
    while (lxr.tok().kind_ != TK::Eof) { lxr.next_tok(); }
}

auto fiska::Parser::tok() -> Tok& {
    return *current_tok_it_;
}

void fiska::Parser::next_tok() {
    if (tok().kind_ == TK::Eof) { return; }
    curr_tok_it_++;
}

auto fiska::Parser::parse_proc_expr() -> ProcExpr* {
    todo("to be implemented");
}

auto fiska::Parser::parse_x86_instruction() -> X86Instruction* {
    todo("to be implemented");
}

