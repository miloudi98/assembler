#include "lib/front_end/parser.hh"

#include <cctype>

#include "lib/core.hh"

namespace {

using fiska::fe::TK;
using fiska::x86::X86IK;
using fiska::x86::RI;
using fiska::x86::BW;

const utils::StringMap<TK> keywords = {
    {"fn", TK::Fn},

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

    {"mov", TK::Mnemonic}, {"add", TK::Mnemonic}, {"adc", TK::Mnemonic}
};

const utils::StringMap<X86IK> mnemonics = {
    {"mov", X86IK::Mov},
    {"add", X86IK::Add},
    {"adc", X86IK::Adc}
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

const utils::StringMap<BW> bws = {
    {"b8", BW::B8}, {"b16", BW::B16}, {"b32", BW::B32}, {"b64", BW::B64},
};

// Credit to llvm: https://llvm.org/doxygen/StringRef_8cpp_source.html
// Returns |std::nullopt| if an overflow occurs.
[[nodiscard]] auto i64_of_str(StrRef str, i1 is_negative = false) -> Opt<i64> {
    assert(not (str.starts_with('+') or str.starts_with('-')));

    u8 radix = str.starts_with("0x") ? 16 : 10;
    if (str.starts_with("0x")) { str.remove_prefix(2); }

    StrRef curr_str = str;
    u64 ret = 0;

    while (not curr_str.empty()) {
        u8 ord = 0;

        if (curr_str[0] >= '0' and curr_str[0] <= '9') {
            ord = u8(curr_str[0] - '0');
        } else if (curr_str[0] >= 'a' and curr_str[0] <= 'z') {
            ord = u8(curr_str[0] - 'a' + 10);
        } else if (curr_str[0] >= 'A' and curr_str[0] <= 'Z') {
            ord = u8(curr_str[0] - 'A' + 10);
        } else {
            return std::nullopt;
        }

        if (ord >= radix) {
            return std::nullopt;
        }

        u64 old_ret = ret;
        ret = ret * radix + ord;

        // overflow detected.
        if ((ret / radix) < old_ret) {
            return std::nullopt;
        }

        curr_str.remove_prefix(1);
    }

    // check if negative number is not too large to 
    // fit in an i64.
    if (is_negative and static_cast<i64>(-ret) > 0) {
        return std::nullopt;
    }

    return static_cast<i64>(is_negative ? -ret : ret);
}

} // namespace

auto fiska::fe::str_of_tk(TK tk) -> StrRef {
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
    case TK::Ident: return "IDENTIFIER";
    case TK::Num: return "NUMBER";
    case TK::BitWidth: return "BIT_WIDTH";
    case TK::Fn: return "FN";
    case TK::Reg: return "REG";
    case TK::Mnemonic: return "MNEMONIC";
    case TK::Eof: return "EOF";
    } // switch
    unreachable();
}
auto fiska::fe::Ctx::load_file(const fs::path& path) -> File* {
    auto file = std::make_unique<File>(
        u16(files_.size()),
        path,
        utils::load_file(path)
    );

    files_.push_back(std::move(file));
    // Create a token stream entry for the file.
    tok_streams_.emplace_back();

    return files_.back().get();
}

auto fiska::fe::Ctx::get_file(u16 fid) -> File* {
    assert(fid < files_.size(), "Invalid fid '{}' encountered.", fid);
    return files_[fid].get();
}

auto fiska::fe::Lexer::file_start() -> const char* {
    return ctx_->files_[fid_]->data();
}

auto fiska::fe::Lexer::eof() -> i1 {
    return curr_ >= end_;
}

auto fiska::fe::Lexer::starts_ident(char c) -> i1 {
    return c == '_' or std::isalpha(c);
}

auto fiska::fe::Lexer::continues_ident(char c) -> i1 {
    return std::isalnum(c);
}

auto fiska::fe::Lexer::next_c() -> void {
    if (eof()) {
        c_ = 0;
        return;
    }
    c_ = *curr_++;
}

auto fiska::fe::Lexer::peek_c(i32 idx) -> char {
    if (not (curr_ + idx >= curr_ and curr_ + idx < end_)) { return 0; }
    return *(curr_ + idx);
}

auto fiska::fe::Lexer::skip_whitespace() -> void {
    while (std::isspace(c_)) { next_c(); }
}

auto fiska::fe::Lexer::lex_line_comment() -> void {
    // Eat '//'.
    next_c();
    next_c();

    while (c_ != '\n') { next_c(); }
}

auto fiska::fe::Lexer::lex_num(Tok* tok) -> void {
    do {
        next_c();
    } while (std::isalnum(c_));

    tok->kind_ = TK::Num;
    tok->str_ = ctx_->str_pool_.save( StrRef{file_start() + tok->loc_.pos_, curr_offset() - tok->loc_.pos_} );
    tok->loc_.len_ = u32(tok->str_.size());
}

auto fiska::fe::Lexer::lex_ident(Tok* tok) -> void {
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

auto fiska::fe::Lexer::next_tok() -> void {
    // The reason we need to separate this function into two parts is because
    // we might recurse in the process of constructing a token and we don't
    // want to allocate a token everytime we recurse.
    // For instance, when we encounter the start of a comment (i.e '/'),
    // we skip all following characters until a new line is reached.
    // Once that's completed we recurse to get the next token.
    next_tok_helper(tok_stream_.alloc_tok());
}

auto fiska::fe::Lexer::next_tok_helper(Tok* tok) -> void {
    skip_whitespace();

    tok->loc_.fid_ = fid_;
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
        todo("Handle unrecognized characters '{}'.", c_);
    }
    } // switch
}

auto fiska::fe::Lexer::tok() -> Tok& {
    return tok_stream_.back();
}

auto fiska::fe::Lexer::curr_offset() -> u32 {
    assert(curr_ > file_start(), "u32 overflow detected.");

    return u32(curr_ - file_start() - 1);
}

auto fiska::fe::Expr::operator new(usz sz, Ctx* ctx) -> void* {
    auto expr = static_cast<Expr*>(::operator new(sz)); 
    ctx->ast_.push_back(expr);
    return expr;
}

auto fiska::fe::Parser::parse_proc_expr() -> ProcExpr* {
    auto proc = new (ctx_) ProcExpr;
    
    expect_all(TK::Fn, TK::Ident, TK::LBrace);

    proc->name_ = ctx_->str_pool_.save(peek_tok(-2).str_);

    while (not at(TK::RBrace)) {
        proc->instructions_.push_back(parse_x86_instruction());
    }

    expect(TK::RBrace);

    return proc;
}

auto fiska::fe::Parser::parse_x86_instruction() -> x86::X86Instruction {
    expect_all(TK::Mnemonic, TK::LParen);

    X86IK ik = utils::strmap_get(mnemonics, peek_tok(-2).str_);

    x86::X86Op::List op_list;
    while (not at(TK::RParen)) {
        op_list.push_back(parse_x86_op());
        consume(TK::Comma);
    }
    expect_all(TK::RParen, TK::SemiColon);
    
    return x86::X86Instruction {
        ik,
        x86::X86InstructionOperands(std::move(op_list))
    };
}

auto fiska::fe::Parser::parse_x86_op() -> x86::X86Op {
    using x86::MemIndexScale;
    using x86::Reg;

    // Register.
    if (match(TK::BitWidth, TK::Reg)) {
        expect_all(TK::BitWidth, TK::Reg);
        return x86::X86Op{ 
            x86::Reg(
                utils::strmap_get(bws, peek_tok(-2).str_),
                utils::strmap_get(reg_ids, peek_tok(-1).str_)
            )
        };
    }
    // Immediate.
    if (match(TK::BitWidth, TK::Num) 
        or match(TK::BitWidth, TK::Plus, TK::Num) 
        or match(TK::BitWidth, TK::Minus, TK::Num))
    {
        expect(TK::BitWidth);
        i1 has_sign = consume(TK::Plus, TK::Minus);
        expect(TK::Num);

        return x86::X86Op {
            x86::Imm {
                utils::strmap_get(bws, peek_tok(-2 - has_sign).str_),
                // TODO(miloudi): Handle invalid numbers.
                i64_of_str(peek_tok(-1).str_, /*is_negative=*/has_sign and peek_tok(-2).kind_ == TK::Minus).value()
            }
        };
    }
    // Memory reference.
    if (match(TK::At, TK::BitWidth, TK::LBracket)) {
        expect_all(TK::At, TK::BitWidth, TK::LBracket);

        BW bw = utils::strmap_get(bws, peek_tok(-2).str_);
        Opt<Reg> breg{};
        Opt<Reg> ireg{};
        MemIndexScale scale{};
        i64 disp{};

        if (consume(TK::Reg)) {
            breg = Reg(
                x86::BW::B64,
                utils::strmap_get(reg_ids, peek_tok(-1).str_)
            );
        }
        expect(TK::RBracket);

        // scale + index register.
        if (consume(TK::LBracket)) {
            expect_all(TK::Num, TK::RBracket, TK::LBracket, TK::Reg, TK::RBracket);

            scale = MemIndexScale(i64_of_str(peek_tok(-5).str_).value());
            ireg = Reg(
                x86::BW::B64,
                utils::strmap_get(reg_ids, peek_tok(-2).str_)
            );
        }

        // displacement.
        if (consume(TK::Plus, TK::Minus)) {
            expect(TK::Num);

            disp = i64_of_str(peek_tok(-1).str_, /*is_negative=*/peek_tok(-2).kind_ == TK::Minus).value();
        }

        return x86::X86Op {
            x86::Mem::make(bw, breg, ireg, scale, disp)
        };
    }
    // Absolute Memory offset.
    if (match(TK::At, TK::BitWidth, TK::Num)) {
        expect_all(TK::At, TK::BitWidth, TK::Num);

        return x86::X86Op {
            x86::Moffs {
                utils::strmap_get(bws, peek_tok(-2).str_),
                i64_of_str(peek_tok(-1).str_).value()
            }
        };
    }

    todo("Handle wrong syntax");
}

auto fiska::fe::Parser::tok() -> const Tok& {
    return *tok_stream_it_;
}

auto fiska::fe::Parser::next_tok() -> void {
    ++tok_stream_it_;
}

auto fiska::fe::Parser::peek_tok(i32 idx) -> const Tok& {
    auto ptok = tok_stream_it_ + idx;
    // Bounds checking.
    assert(ctx_->tok_streams_[fid_].begin() <= ptok and ptok < ctx_->tok_streams_[fid_].end());
    return *ptok;
}

