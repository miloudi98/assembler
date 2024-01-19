#include "lib/front_end.hh"

#include "lib/core.hh"

#include <cctype>

namespace {

using namespace fiska;

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

const utils::StringMap<BW> bit_widths = {
    {"b8", BW::B8}, {"b16", BW::B16}, {"b32", BW::B32}, {"b64", BW::B64},
};

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

    {"mov", TK::Mnemonic}
};

const utils::StringMap<RI> x86_registers = {
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

const utils::StringMap<X86IK> x86_instruction_kinds = {
    {"mov", X86IK::Mov}
};

template <IsX86Op T, IsX86Op U>
auto operator>>=(const Opt<T>& me, const Opt<U>& you) -> Opt<X86Op> {
    if (me) { return X86Op(me.value()); }
    return you ? X86Op(you.value()) : Opt<X86Op>{std::nullopt};
}

template <IsX86Op T>
auto operator>>=(const Opt<T>& me, const Opt<X86Op>& you) -> Opt<X86Op> {
    if (me) { return X86Op(me.value()); }
    return you;
}

// Credit to llvm: https://llvm.org/doxygen/StringRef_8cpp_source.html
// Returns |std::nullopt| if an overflow occurs.
auto i64_of_str(StrRef str) -> Opt<i64> {
    bool is_negative = str.starts_with('-');

    if (str.starts_with('+') or str.starts_with('-')) {
        str.remove_prefix(1);
    }

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
            // Not going to happen because during the lexing of a number, we immediately stop
            // when we encounter a character that does not constitute a valid digit in the base used.
            unreachable();
        }

        // Not going to happen because during the lexing of a number, we immediately stop
        // when we encounter a character that does not constitute a valid digit in the base used.
        if (ord >= radix) {
            unreachable();
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

auto i64_of_str_unchecked(StrRef str) -> i64 {
    return i64_of_str(str).value();
}

auto scale_of_i64(i64 scale) -> Mem::Scale {
    switch (scale) {
    case 1: return Mem::Scale::One;
    case 2: return Mem::Scale::Two;
    case 4: return Mem::Scale::Four;
    case 8: return Mem::Scale::Eight;
    default: unreachable("Illegal raw scale '{}' passed to |scale_of_i64|.", scale);
    } // switch
}

auto concat_str_refs(std::same_as<StrRef> auto... strs) -> Str {
    Str ret{};

    auto helper = [&](StrRef str_ref) {
        ret += Str{str_ref};
    };

    (helper(strs), ...);
    return ret;
}

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
            tok().loc_.len_ = u32(file_offset() - tok().loc_.pos_);
            Error err {
                .kind_ = ErrKind::UnexpectedChar,
                .ctx_ = mod_->ctx_,
                .loc_ = tok().loc_,
                .data_ = { .c_ =  c_ }
            };
            report_error(err,  "Expected a line comment after '/', but found '{}'.", c_);
        }
        // Eat the second '/'
        next_c();
        lex_comment();
        return next_tok_helper();
    }
    case '0': {
        char cc = peek_c();
        if (is_digit(cc)) {
            // Eat the '0' and then error out. This is to make sure the error message displays
            // the correct unexpected char which is 'cc' and not '0'.
            next_c();

            tok().loc_.len_ = u32(file_offset() - tok().loc_.pos_);
            Error err {
                .kind_ = ErrKind::UnexpectedChar,
                .ctx_ = mod_->ctx_,
                .loc_ = tok().loc_,
                .data_ = { .c_ = c_ }
            };
            report_error(err,  "Leading zeros are not allowed in a decimal number. "
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
        if (is_ident_start(c_)) {
            lex_ident();
            break;
        }
        // Eat the unrecognized char in order to show the right error message to the user.
        next_c();

        tok().loc_.len_ = u32(file_offset() - tok().loc_.pos_);
        Error err {
            .kind_ = ErrKind::UnrecognizedChar, 
            .ctx_ = mod_->ctx_,
            .loc_ = tok().loc_,
            .data_ = { .c_ = c_ }
        };
        report_error(err, "Expected thet start of an identifier but found '{}' instead.", c_);
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

    // Report overflow.
    if (not i64_of_str(tok().str_)) {
        tok().loc_.len_ = u32(file_offset() - tok().loc_.pos_);
        Error err {
            .kind_ = ErrKind::NumberOverflow,
            .ctx_ = mod_->ctx_,
            .loc_ = tok().loc_,
            .data_ = { .overflowed_num_ = tok().str_ }
        };
        report_error(err, "Number too big to fit in 64-bits");
    }
}

void fiska::Lexer::lex_digit() {
    const char* num_start = curr_ - 1;
    while (is_digit(c_)) { next_c(); }

    tok().kind_ = TK::Num;
    tok().str_ = mod_->strings_.save(StrRef{num_start, u32(curr_ - num_start - 1)});

    // Report overflow.
    if (not i64_of_str(tok().str_)) {
        tok().loc_.len_ = u32(file_offset() - tok().loc_.pos_);
        Error err {
            .kind_ = ErrKind::NumberOverflow,
            .ctx_ = mod_->ctx_,
            .loc_ = tok().loc_,
            .data_ = { .overflowed_num_ = tok().str_ }
        };
        report_error(err, "Number too big to fit in 64-bits.");
    }
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

auto fiska::Tok::spelling(TK kind) -> StrRef {
    switch (kind) {
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


void* fiska::Expr::operator new(usz sz, Module* mod) {
    auto expr = static_cast<Expr*>(::operator new(sz));
    mod->ast_.push_back(expr);

    if (expr->kind_ == ExprKind::ProcExpr) {
        mod->procs_.push_back(static_cast<ProcExpr*>(expr));
    }
    return expr;
}

fiska::Module::~Module() {
    rgs::for_each(ast_, [](Expr* expr) { delete expr; });
}

auto fiska::Parser::tok() -> const Tok& {
    return *curr_tok_it_;
}

auto fiska::Parser::prev(u32 lookbehind) -> const Tok& {
    assert(curr_tok_it_ - mod_->tokens_.begin() >= 1 + lookbehind,
            "Attempting to access the previously consumed token when no tokens were consumed.");
    return *(curr_tok_it_ - 1 - lookbehind);
}

auto fiska::Parser::peek_tok(u32 lookahead) -> const Tok& {
    if (curr_tok_it_ + lookahead >= mod_->tokens_.end()) {
        // Return the TK::Eof token.
        return mod_->tokens_.storage_.back();
    }
    return *(curr_tok_it_ + lookahead);
}

void fiska::Parser::next_tok() {
    // Keep returning TK::Eof when we are out of tokens.
    if (tok().kind_ == TK::Eof) { return; }
    curr_tok_it_++;
}

auto fiska::Parser::parse_proc_expr() -> Expr* {
    expect(TK::Fn, TK::Ident, TK::LBrace);
    StrRef func_name = prev(1).str_;

    Vec<Box<X86Instruction>> instructions;
    while (not at(TK::RBrace)) {
        instructions.push_back(Box<X86Instruction>{parse_x86_instruction()});
    }
    expect(TK::RBrace);

    return new (mod_) ProcExpr(func_name, std::move(instructions));
}

auto fiska::Parser::parse_x86_instruction() -> X86Instruction* {
    expect(TK::Mnemonic, TK::LParen);
    X86IK instruction_kind = strmap_get(x86_instruction_kinds, prev(1).str_);

    switch (instruction_kind) {
    case X86IK::Mov: {
        X86Op dst = parse_x86_operand();
        expect(TK::Comma);
        X86Op src = parse_x86_operand();
        expect(TK::RParen, TK::SemiColon);

        return new Mov(dst, src);
    }
    } // switch

    unreachable();
}

template <OneOf<Reg, Imm, Mem, Moffs> Type>
auto fiska::Parser::try_parse_x86_operand() -> Opt<Type> {
    static_assert(false and "The parser is not implemented for the type");
    unreachable();
}


template <>
auto fiska::Parser::try_parse_x86_operand<Reg>() -> Opt<Reg> {
    // Not a |Reg| operand.
    if (not match_next_toks(TK::BitWidth, TK::Reg)) { return std::nullopt; }

    expect(TK::BitWidth, TK::Reg);

    return Reg {
        .bit_width_ = utils::strmap_get(bit_widths, prev(1).str_),
        .id_ = utils::strmap_get(x86_registers, prev().str_)
    };
}

template <>
auto fiska::Parser::try_parse_x86_operand<Imm>() -> Opt<Imm> {
    // Not an |Imm| operand.
    if (not (match_next_toks(TK::BitWidth, TK::Num)
        or match_next_toks(TK::BitWidth, TK::Plus, TK::Num)
        or match_next_toks(TK::BitWidth, TK::Minus, TK::Num)))
    {
        return std::nullopt;
    }

    expect(TK::BitWidth);
    bool has_sign = consume(TK::Plus, TK::Minus);
    expect(TK::Num);

    return Imm {
        .bit_width_ = utils::strmap_get(bit_widths, prev(1).str_),
        // SAFETY: This will never panic because we check for overflow when lexing the
        // number.
        .inner_ = i64_of_str_unchecked(has_sign ? concat_str_refs(prev(1).str_, prev().str_) : prev().str_)
    };
}

template <>
auto fiska::Parser::try_parse_x86_operand<Moffs>() -> Opt<Moffs> {
    // Not a |Moffs| operand.
    if (not match_next_toks(TK::At, TK::BitWidth, TK::Num)) { return std::nullopt; }
    
    expect(TK::At, TK::BitWidth, TK::Num);

    return Moffs { 
        .bit_width_ = utils::strmap_get(bit_widths, prev(1).str_),
        // SAFETY: This will never panic because we check for overflow when lexing the 
        // number.
        .inner_ = i64_of_str_unchecked(prev().str_)
    };
}

template <>
auto fiska::Parser::try_parse_x86_operand<Mem>() -> Opt<Mem> {
    // Not a memory reference.
    if (not match_next_toks(TK::At, TK::BitWidth, TK::LBracket)) { return std::nullopt; }

    expect(TK::At, TK::BitWidth);

    BW bit_width = utils::strmap_get(bit_widths, prev().str_);
    Opt<Reg> base_reg{std::nullopt};
    Opt<Reg> index_reg{std::nullopt};
    Opt<Mem::Scale> scale{std::nullopt};
    Opt<i64> mem_disp{std::nullopt};

    expect(TK::LBracket);
    if (consume(TK::Reg)) {
        base_reg = Reg {
            // All registers used to address memory in 64-bit mode are 64-bit wide.
            .bit_width_ = BW::B64,
            .id_ = utils::strmap_get(x86_registers, prev().str_)
        };
    }
    expect(TK::RBracket);

    // Has scale and index
    if (match_next_toks(TK::LBracket, TK::Num, TK::RBracket)) {
        expect(TK::LBracket, TK::Num, TK::RBracket, TK::LBracket, TK::Reg, TK::RBracket);
        index_reg = Reg {
            // All registers used to address memory in 64-bit mode are 64-bit wide.
            .bit_width_ = BW::B64,
            .id_ = utils::strmap_get(x86_registers, prev(1).str_)
        };

        // Illegal memory reference scale.
        i64 raw_scale = i64_of_str_unchecked(prev(4).str_);
        if (not is<1LL, 2LL, 4LL, 8LL>(raw_scale)) {
            Error err {
                .kind_ = ErrKind::IllegalValue,
                .ctx_ = mod_->ctx_,
                .loc_ = prev(4).loc_,
                .data_ = { .illegal_num_ = raw_scale }
            };
            report_error(err, "'{}' is not a legal index scale. Valid scales are '[1, 2, 4, 8]'.", raw_scale);
        }
        scale = scale_of_i64(raw_scale);
    }

    // Has displacement
    if (consume(TK::Plus, TK::Minus) and consume(TK::Num)) {
        mem_disp = i64_of_str_unchecked(concat_str_refs(prev(1).str_, prev().str_));

        if (not utils::fits_in_b32(mem_disp.value())) {
            Location sign_and_number_merged_loc {
                .pos_ = prev(1).loc_.pos_,
                .len_ = prev().loc_.pos_ + prev().loc_.len_ - prev(1).loc_.pos_,
                .fid_ = fid_,
            };
            Error err {
                .kind_ = ErrKind::IllegalValue,
                .ctx_ = mod_->ctx_,
                .loc_ = sign_and_number_merged_loc,
                .data_ = { .illegal_num_ = mem_disp.value() }
            };
            report_error(err, "memory reference displacement can't exceed 32-bits.");
        }
    }

    // Ill formed memory reference encountered.
    if (not (base_reg or index_reg or mem_disp)) {
        todo("Ill-formed memory reference. User error message is still not yet implemented");
    }

    return Mem {
        .bit_width_ = bit_width,
        .base_reg_ = base_reg,
        .index_reg_ = index_reg,
        .scale_ = scale,
        .disp_ = mem_disp
    };
}

auto fiska::Parser::parse_x86_operand() -> X86Op {
    Opt<X86Op> op = try_parse_x86_operand<Reg>()
        >>= try_parse_x86_operand<Imm>()
        >>= try_parse_x86_operand<Mem>()
        >>= try_parse_x86_operand<Moffs>();

    // What error should we report?
    assert(op.has_value(),
            "Parse error. TODO: report the error properly instead of asserting");

    return op.value();
}


void fiska::Parser::parse_file_into_module(File* file, Module* mod) {
    Parser p{file, mod};
    while (p.tok().kind_ != TK::Eof) {
        p.parse_proc_expr();
    }
}
