#include "lib/front_end.hh"

#include "lib/core.hh"

#include <cctype>
#include <algorithm>

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
[[nodiscard]] auto i64_of_str(StrRef str) -> Opt<i64> {
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

[[nodiscard]] auto i64_of_str_or_report_error(Context* ctx, Location loc, StrRef str) -> i64 {
    Opt<i64> integer = i64_of_str(str);
    // Signed overflow.
    // Occurs when numbers are lower than i64::MIN.
    if (not integer) {
        Error err {
            .kind_ = ErrKind::NumberOverflow,
            .ctx_ = ctx,
            .loc_ = loc,
            .data_ = { .overflowed_num_ = str }
        };
        report_error(err,
                "Integer too big to fit in 64-bits. 64-bit integers are within [{}, {}].",
                std::numeric_limits<i64>::min(), std::numeric_limits<i64>::max());

    }

    return integer.value();
}

[[nodiscard]] auto scale_of_i64(i64 scale) -> Mem::Scale {
    switch (scale) {
    case 1: return Mem::Scale::One;
    case 2: return Mem::Scale::Two;
    case 4: return Mem::Scale::Four;
    case 8: return Mem::Scale::Eight;
    default: unreachable("Illegal raw scale '{}' passed to |scale_of_i64|.", scale);
    } // switch
}

[[nodiscard]] auto concat_str_refs(std::same_as<StrRef> auto... strs) -> Str {
    Str ret{};

    auto helper = [&](StrRef str_ref) {
        ret += Str{str_ref};
    };

    (helper(strs), ...);
    return ret;
}

template <typename Container>
requires requires (const Container& container) {
    {container.begin()};
    {container.end()};
}
auto operator+=(ModulePrinter::UTF32Str& me, const Container& other) -> ModulePrinter::UTF32Str {
    me.insert(me.end(), other.begin(), other.end());
    return me;
}

template <typename T>
consteval auto is_top_level_expr() -> bool { return OneOf<T, ProcExpr>; }

}  // namespace

auto fiska::str_of_bw(BW bit_width) -> StrRef {
    switch (bit_width) {
    case BW::B8: return "b8";
    case BW::B16: return "b16";
    case BW::B24: return "b24";
    case BW::B32: return "b32";
    case BW::B64: return "b64";
    } // switch

    unreachable();
}

auto fiska::str_of_ri(RI rid) -> StrRef {
    auto ret = std::find_if(
        x86_registers.begin(),
        x86_registers.end(),
        [rid](const auto& p) { return p.second == rid; }
    );

    assert(ret != x86_registers.end(), "Forgot to update the |x86_registers| mapping.");
    return ret->first;
}

auto fiska::str_of_rk(RK rkind) -> StrRef {
    switch (rkind) {
    case RK::Gp: return "General Purpose";
    case RK::Seg: return "Segment";
    case RK::Ctrl: return "Control";
    case RK::Dbg: return "Debug";
    } // switch
    unreachable();
}

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

        // Unrecognized char.
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

    Location loc = tok().loc_;
    loc.len_ = u32(file_offset() - tok().loc_.pos_);

    // Check for overflow.
    std::ignore = i64_of_str_or_report_error(mod_->ctx_, loc, tok().str_);
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
        report_error(err, "Integer too big to fit in 64-bits.");
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

void fiska::Lexer::lex_file_into_module(File* file, Module* mod) {
    Lexer lxr{file, mod};
    while (lxr.tok().kind_ != TK::Eof) { lxr.next_tok(); }
}

auto fiska::X86Op::bit_width() const -> BW {
    if (is<Reg>())   { return as<Reg>().bit_width_;   }
    if (is<Mem>())   { return as<Mem>().bit_width_;   }
    if (is<Imm>())   { return as<Imm>().bit_width_;   }
    if (is<Moffs>()) { return as<Moffs>().bit_width_; }
    unreachable("Unrecognized x86 operand.");
}

auto fiska::X86Op::modrm_mod() const -> u8 {
    using enum RI;
    assert((is<Reg, Mem>()), "Trying to encode a 'Memory offset' or an 'Immediate' in the ModRm.");

    if (is<Reg>()) { return Mem::kmod_reg; }

    const Mem& mem = as<Mem>();

    auto mod_based_on_disp = [](i64 disp) {
        return utils::fits_in_b8(disp) 
            ? Mem::kmod_mem_disp8 
            : Mem::kmod_mem_disp32;
    };

    switch (mem.kind()) {
    case MK::BaseIndexDisp: {
        assert(mem.base_reg_.has_value());

        assert(not ::is<Rip>(mem.base_reg_->id_), 
                "Rip can't used as a memory reference base register "
                "when an index register is present.");

        [[fallthrough]];
    }
    case MK::BaseDisp: {
        if (::is<Rbp, R13>(mem.base_reg_->id_)) { return mod_based_on_disp(mem.disp_.value_or(0)); }
        if (not mem.disp_ or ::is<Rip>(mem.base_reg_->id_)) { return Mem::kmod_mem; }
        return mod_based_on_disp(mem.disp_.value());
    }
    case MK::IndexDisp:
    case MK::DispOnly: {
        return Mem::kmod_mem;
    }
    } // switch 
    unreachable();
}

auto fiska::X86Op::modrm_encoding() const -> u8 {
    using enum RI;

    assert((is<Reg, Mem>()), "Trying to encode a 'Memory offset' or an 'Immediate' in the ModRm.");

    if (is<Reg>()) { return as<Reg>().index(); }

    const Mem& mem = as<Mem>();
    switch (mem.kind()) {
    case MK::BaseDisp: {
        assert(mem.base_reg_.has_value());

        return ::is<Rsp, R12>(mem.base_reg_->id_) 
            ? Mem::ksib_marker
            : mem.base_reg_->index();
    }
    case MK::BaseIndexDisp:
    case MK::IndexDisp: 
    case MK::DispOnly: {
        return Mem::ksib_marker;
    }
    } // switch
    unreachable();
}

void* fiska::Expr::operator new(usz sz, Module* mod, bool is_top_level_expr) {
    auto expr = static_cast<Expr*>(::operator new(sz));
    mod->ast_.push_back(expr);

    if (is_top_level_expr) {
        mod->top_level_exprs_.push_back(expr);
    }
    return expr;
}

void* fiska::X86Instruction::operator new(usz sz, ProcExpr* enclosing_proc) {
    auto instr = static_cast<X86Instruction*>(::operator new(sz));
    enclosing_proc->instructions_.push_back(instr);
    return instr;
}

auto fiska::X86Instruction::str_of_encoding() const -> Str {
    Str ret{};
    ret += "[";
    for (auto [idx, byte] : encoding_ | vws::enumerate) {
        ret += fmt::format("{:#04x}{}",
                byte,
                u32(idx) == encoding_.size() - 1 ? "" : ", ");
    }
    ret += "]";
    return ret;
}

fiska::ProcExpr::~ProcExpr() {
    rgs::for_each(instructions_, [](X86Instruction* instr) { delete instr; });
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

    auto proc_expr = new (mod_, is_top_level_expr<ProcExpr>()) ProcExpr;

    Vec<X86Instruction*> instructions;
    while (not at(TK::RBrace)) {
        instructions.push_back(parse_x86_instruction(proc_expr));
    }
    expect(TK::RBrace);

    proc_expr->instructions_ = std::move(instructions);
    proc_expr->func_name_ = func_name;

    return proc_expr;
}

auto fiska::Parser::parse_x86_instruction(ProcExpr* enclosing_proc) -> X86Instruction* {
    expect(TK::Mnemonic, TK::LParen);
    X86IK instruction_kind = strmap_get(x86_instruction_kinds, prev(1).str_);

    switch (instruction_kind) {
    case X86IK::Mov: {
        X86Op dst = parse_x86_operand();
        expect(TK::Comma);
        X86Op src = parse_x86_operand();
        expect(TK::RParen, TK::SemiColon);

        return new (enclosing_proc) Mov(
                dst,
                src,
                Assembler::encode<X86IK::Mov>({dst, src})
            );
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
        .inner_ = i64_of_str_or_report_error(
                mod_->ctx_,
                has_sign ? prev().loc_.merge(prev(1).loc_) : prev().loc_,
                has_sign ? concat_str_refs(Tok::spelling(prev(1).kind_), prev().str_) : prev().str_)
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
        .inner_ = i64_of_str(prev().str_).value()
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
    // Has base register.
    if (consume(TK::Reg)) {
        base_reg = Reg {
            // All registers used to address memory in 64-bit mode are 64-bit wide.
            .bit_width_ = BW::B64,
            .id_ = utils::strmap_get(x86_registers, prev().str_)
        };
    }
    expect(TK::RBracket);

    // Has scale and index
    if (match_next_toks(TK::LBracket)) {
        expect(TK::LBracket, TK::Num, TK::RBracket, TK::LBracket, TK::Reg, TK::RBracket);
        index_reg = Reg {
            // All registers used to address memory in 64-bit mode are 64-bit wide.
            .bit_width_ = BW::B64,
            .id_ = utils::strmap_get(x86_registers, prev(1).str_)
        };

        // Illegal memory reference scale.
        i64 raw_scale = i64_of_str(prev(4).str_).value();
        if (not is<1LL, 2LL, 4LL, 8LL>(raw_scale)) {
            Error err {
                .kind_ = ErrKind::IllegalValue,
                .ctx_ = mod_->ctx_,
                .loc_ = prev(4).loc_,
                .data_ = { .illegal_lxm_ = prev(4).str_ }
            };
            report_error(err, "'{}' is not a legal index scale. Valid scales are '[1, 2, 4, 8]'.", raw_scale);
        }
        scale = scale_of_i64(raw_scale);
    }

    // Has displacement
    if (at(TK::Plus, TK::Minus)) {
        expect_either(TK::Plus, TK::Minus);
        expect(TK::Num);

        Str mem_disp_lxm = concat_str_refs(Tok::spelling(prev(1).kind_), prev().str_);

        mem_disp = i64_of_str_or_report_error(
                mod_->ctx_,
                prev().loc_.merge(prev(1).loc_),
                mem_disp_lxm
        );

        if (not utils::fits_in_b32(mem_disp.value())) {
            Error err {
                .kind_ = ErrKind::IllegalValue,
                .ctx_ = mod_->ctx_,
                .loc_ = prev().loc_.merge(prev(1).loc_),
                .data_ = { .illegal_lxm_ = mem_disp_lxm }
            };
            report_error(err, "memory reference displacement can't exceed 32-bits.");
        }
    }

    // Ill formed memory reference encountered.
    if (not (base_reg or index_reg or mem_disp)) {
        // Keep merging the locations until we find an '@' designating
        // the start of the memory reference expression.
        Location loc{prev().loc_};
        u32 idx = 1;
        while (prev(idx).kind_ != TK::At) { loc = loc.merge(prev(idx++).loc_); }
        // Merge the '@' token.
        loc = loc.merge(prev(idx).loc_);

        Error err {
            .kind_ = ErrKind::IllegalStmt,
            .ctx_ = mod_->ctx_,
            .loc_ = loc
        };
        // TODO(miloudi): Add an example of the memory reference syntax to the user.
        report_error(err, "Invalid memory reference.");
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

    // TODO(miloudi): Error handling should probably not be here since we don't really have
    // any context of what instruction we're currently parsing. This limits the amount of
    // information we can give in an error message.
    // It's good enough for now, but this needs to change later.
    if (not op) {
        Location loc{tok().loc_};
        // Keep merging the location of the lexemes until we find a stoping token.
        // A stoping token is chosen arbitrarly here. Having keyword tokens (e.g 'fn', 'mnemonic')
        // as stopping tokens seems like a good heuristic.
        // Report the whole set of tokens as an invalid start of a known operand.
        while (not at(TK::Fn, TK::Mnemonic, TK::Comma, TK::SemiColon, TK::Eof)) {
            loc = loc.merge(tok().loc_);
            next_tok();
        }

        Error err {
            .kind_ = ErrKind::IllegalStmt,
            .ctx_ = mod_->ctx_,
            .loc_ = loc
        };
        report_error(err, "Expression does not start any of the known operands "
                "[ 'Register', 'Immediate', 'Memory reference', 'Memory offset' ].");
    }

    return op.value();
}


void fiska::Parser::parse_file_into_module(File* file, Module* mod) {
    Parser p{file, mod};
    while (p.tok().kind_ != TK::Eof) { p.parse_proc_expr(); }
}

auto fiska::ModulePrinter::line_prefix(bool is_last) -> UTF32Str {
    assert(indent_ >= 1);
    u32 num_spaces = (indent_ - 1) << 1;

    UTF32Str ret(num_spaces, ' ');

    auto must_cont_line = [&](auto&& p) {
        return is<bleft_corn_cont, vline>(std::get<1>(p))
            and u64(std::get<0>(p)) < ret.size();
    };

    for (auto [idx, _] : prev_line_prefix_ | vws::enumerate | vws::filter(must_cont_line)) {
        ret[u32(idx)] = vline;
    }

    ret.push_back(is_last ? bleft_corn : bleft_corn_cont);
    ret.push_back(dash);
    prev_line_prefix_ = std::move(ret);

    return prev_line_prefix_;
}

void fiska::ModulePrinter::print_module_helper(Module* mod) {
    out_ += fmt::format("{} <{}>\n",
            c("Module", fg(red)),
            c(mod->name_, fg(cadet_blue)));

    indent_++;
    for (auto [expr_idx, expr] : mod->top_level_exprs_ | vws::enumerate) {
        print(expr, /*is_last=*/u32(expr_idx) == mod->top_level_exprs_.size() - 1);
    }
    indent_--;

    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> converter;
    fmt::print("{}\n", converter.to_bytes(std::move(out_)));
}

void fiska::ModulePrinter::print_module(Module* mod) {
    ModulePrinter printer{};
    printer.print_module_helper(mod);
}

void fiska::ModulePrinter::print(Expr* expr, bool is_last) {
    out_ += line_prefix(is_last);
    switch (expr->kind_) {
    case ExprKind::ProcExpr: {
        auto proc_expr = static_cast<ProcExpr*>(expr);
        out_ += fmt::format("{} <{}>\n",
                c("Proc", bold | fg(red)),
                c(proc_expr->func_name_, fg(cadet_blue)));

        indent_++;
        for (auto [instr_idx, instr] : proc_expr->instructions_ | vws::enumerate) {
            print(instr, /*is_last=*/u32(instr_idx) == proc_expr->instructions_.size() - 1);
        }
        indent_--;
        break;
    }
    } // switch
}

void fiska::ModulePrinter::print(X86Instruction* instruction, bool is_last) {
    out_ += line_prefix(is_last);
    switch (instruction->kind_) {
    case X86IK::Mov: {
        auto mov = static_cast<Mov*>(instruction);
        out_ += c("Mov", fg(dark_cyan));
        out_ += fmt::format(" {}\n", mov->str_of_encoding());

        indent_++;
        print(mov->dst_, /*is_last=*/false);
        print(mov->src_, /*is_last=*/true);
        indent_--;
        break;
    }
    } // switch
}

void fiska::ModulePrinter::print(const X86Op& op, bool is_last) {
    auto c_cyan = [&](const auto& value) { return c(value, fg(cyan)); };
    
    out_ += line_prefix(is_last);

    indent_++;
    if (op.is<Reg>()) {
        const Reg& reg = op.as<Reg>();

        out_ += c("Reg\n", fg(dark_cyan));
        out_ += line_prefix(/*is_last=*/true);
        out_ += fmt::format(" Id: {}, Kind: {}, Bit width: {} \n",
                c_cyan(str_of_ri(reg.id_)),
                c_cyan(str_of_rk(reg.kind())),
                c_cyan(str_of_bw(reg.bit_width_)));
    }

    if (op.is<Mem>()) {
        const Mem& mem = op.as<Mem>();

        out_ += c("Mem\n", fg(dark_cyan));
        out_ += line_prefix(/*is_last*/true);
        out_ += fmt::format(" Base: {}, Index: {}, Scale: {}, Disp: {}, Bit width: {} \n",
                c_cyan(mem.base_reg_ ? str_of_ri(mem.base_reg_.value().id_) : "None"),
                c_cyan(mem.index_reg_ ? str_of_ri(mem.index_reg_.value().id_) : "None"),
                c_cyan(mem.scale_ ? std::to_string(1 << +mem.scale_.value()) : "None"s),
                c_cyan(mem.disp_ ? std::to_string(mem.disp_.value()) : "None"s),
                c_cyan(str_of_bw(mem.bit_width_)));

    }

    if (op.is<Imm>()) {
        const Imm& imm = op.as<Imm>();
            
        out_ += c("Immediate\n", fg(dark_cyan));
        out_ += line_prefix(/*is_last*/true);
        out_ += fmt::format(" Value: {}, Bit width: {}\n",
                c_cyan(std::to_string(imm.as<i64>())),
                c_cyan(str_of_bw(imm.bit_width_)));
    }

    if (op.is<Moffs>()) {
        const Moffs& moffs = op.as<Moffs>();

        out_ += c("Moffset\n", fg(dark_cyan));
        out_ += line_prefix(/*is_last*/true);
        out_ += fmt::format(" Address: {}, Bit width: {} \n",
                fmt::format(fg(cyan), "{:#04x}", moffs.as<u64>()),
                c_cyan(str_of_bw(moffs.bit_width_)));
    }
    indent_--;
}

auto fiska::ByteStream::append(BW bw, u64 qword) -> ByteStream& {
    switch (bw) {
    case BW::B8: {
        out_.push_back(u8(qword >> 0) & 0xff);
        break;
    }
    case BW::B16: {
        out_.push_back(u8(qword >> 0) & 0xff);
        out_.push_back(u8(qword >> 8) & 0xff);
        break;
    }
    case BW::B24: {
        out_.push_back(u8(qword >> 0) & 0xff);
        out_.push_back(u8(qword >> 8) & 0xff);
        out_.push_back(u8(qword >> 16) & 0xff);
        break;
    }
    case BW::B32: {
        out_.push_back(u8(qword >> 0) & 0xff);
        out_.push_back(u8(qword >> 8) & 0xff);
        out_.push_back(u8(qword >> 16) & 0xff);
        out_.push_back(u8(qword >> 24) & 0xff);
        break;
    }
    case BW::B64: {
        out_.push_back(u8(qword >> 0) & 0xff);
        out_.push_back(u8(qword >> 8) & 0xff);
        out_.push_back(u8(qword >> 16) & 0xff);
        out_.push_back(u8(qword >> 24) & 0xff);
        out_.push_back(u8(qword >> 32) & 0xff);
        out_.push_back(u8(qword >> 40) & 0xff);
        out_.push_back(u8(qword >> 48) & 0xff);
        out_.push_back(u8(qword >> 56) & 0xff);
        break;
    }
    } // switch
    return *this;
}

auto fiska::ByteStream::append(ByteVec byte_vec) -> ByteStream& {
    out_.insert(out_.end(), byte_vec.begin(), byte_vec.end());
    return *this;
}

namespace {

// X86 Instruction patterns.

using r8 = r<BW::B8>;
using r16 = r<BW::B16>;
using r32 = r<BW::B32>;
using r64 = r<BW::B64>;
using sreg = r<BW::B16, RK::Seg>;

using m8 = m<BW::B8>;
using m16 = m<BW::B16>;
using m32 = m<BW::B32>;
using m64 = m<BW::B64>;

template <BW w>
using rm = Any<r<w>, m<w>>;

using rm8 = rm<BW::B8>;
using rm16 = rm<BW::B16>;
using rm32 = rm<BW::B32>;
using rm64 = rm<BW::B64>;

using moffs8 = mo<BW::B8>;
using moffs16 = mo<BW::B16>;
using moffs32 = mo<BW::B32>;
using moffs64 = mo<BW::B64>;

using imm8 = i<BW::B8>;
using imm16 = i<BW::B16>;
using imm32 = i<BW::B32>;
using imm64 = i<BW::B64>;

// TODO(miloudi): Check whether ctrl registers are really 32 bits.
// I'm pretty sure they are 32 bits, but it's better to provide a reference
// here in the code.
using cr = r<BW::B64, RK::Ctrl>;

// TODO(miloudi): Check whether dbg registers are 32 bits.
// Same thing here, it's good to have a reference here confirming
// the size of debug registers.
using dbg = r<BW::B64, RK::Dbg>;

} // namespace

template <>
void fiska::Assembler::register_instruction<X86IK::Mov>() {
    using enum BW;
    using enum RI;
    using enum OpEn;

    Vec<InstrExpr>& mov = git_[X86IK::Mov];

    // 0x88 MOV r/m8, r8 -- MR
    mov.push_back({
        {0x88},
        Pat<rm8, r8>{},
        Emitter<MR>{}
    });

    // 0x89 MOV r/m16, r16 -- MR
    // 0x89 MOV r/m32, r32 -- MR
    // 0x89 MOV r/m64, r64 -- MR
    mov.push_back({
        {0x89},
        Or<
            Pat<rm16, r16>,
            Pat<rm32, r32>,
            Pat<rm64, r64>
        >{},
        Emitter<MR>{}
    });

    // 0x8A MOV r8, r/m8 -- RM
    mov.push_back({
        {0x8a},
        Pat<r8, rm8>{},
        Emitter<RM>{}
    });

    // 0x8B MOV r16, r/m16 -- RM
    // 0x8B MOV r32, r/m32 -- RM
    // 0x8B MOV r64, r/m64 -- RM
    mov.push_back({
        {0x8b},
        Or<
            Pat<r16, rm16>,
            Pat<r32, rm32>,
            Pat<r64, rm64>
        >{},
        Emitter<RM>{}
    });

    // 0x8C MOV r/m16, Sreg       -- MR
    // 0x8C MOV r16/r32/m16, Sreg -- MR
    // 0x8C MOV r64/m16, Seg      -- MR
    mov.push_back({
        {0x8c},
        Pat<Any<rm16, r32, r64>, sreg>{},
        Emitter<MR>{}
    });

    // 0x8E MOV Sreg, r/m16 -- RM
    // 0x8E MOV Sreg, r/m64 -- RM
    mov.push_back({
        {0x8e},
        Pat<sreg, Any<rm16, rm64>>{},
        Emitter<RM>{}
    });

    // 0xA0 MOV AL, moffs8   -- FD
    mov.push_back({
        {0xa0},
        Pat<r<B8, Rax>, moffs8>{},
        Emitter<FD>{}
    });

    // 0xA1 MOV AX, moffs16  -- FD
    // 0xA1 MOV EAX, moffs32 -- FD
    // 0xA1 MOV RAX, moffs64 -- FD
    mov.push_back({
        {0xa1},
        Or<
            Pat<r<B16, Rax>, moffs16>,
            Pat<r<B32, Rax>, moffs32>,
            Pat<r<B64, Rax>, moffs64>
        >{},
        Emitter<FD>{}
    });

    // 0xA2 MOV moffs8, AL -- TD
    mov.push_back({
        {0xa2},
        Pat<moffs8, r<B8, Rax>>{},
        Emitter<TD>{}
    });

    // 0xA3 MOV moffs16, AX  -- TD
    // 0xA3 MOV moffs32, EAX -- TD
    // 0xA3 MOV moffs64, RAX -- TD
    mov.push_back({
        {0xa3},
        Or<
            Pat<moffs16, r<B16, Rax>>,
            Pat<moffs32, r<B32, Rax>>,
            Pat<moffs64, r<B64, Rax>>
        >{},
        Emitter<TD>{}
    });

    // 0xB0 MOV r8, imm8 -- OI
    mov.push_back({
        {0xb0},
        Pat<r8, imm8>{},
        Emitter<OI>{}
    });

    // 0xB8 MOV r16, imm16 -- OI
    // 0xB8 MOV r32, imm32 -- OI
    // 0xB8 MOV r64, imm64 -- OI
    mov.push_back({
        {0xb8},
        Or<
            Pat<r16, imm16>,
            Pat<r32, imm32>,
            Pat<r64, imm64>
        >{},
        Emitter<OI>{}
    });

    // 0xC6 MOV r/m8, imm8 -- MI
    mov.push_back({
        {0xc6},
        Pat<rm8, imm8>{},
        Emitter<MI>{}
    });

    // 0xC7 MOV r/m16, imm16 -- MI
    // 0xC7 MOV r/m32, imm32 -- MI
    // 0xC7 MOV r/m64, imm32 -- MI
    mov.push_back({
        {0xc7},
        Or<
            Pat<rm16, imm16>,
            Pat<rm32, imm32>,
            Pat<rm64, imm32>
        >{},
        Emitter<MI>{}
    });

    // 0x0F 0x20 MOV r64, CR0-CR8 -- MR
    mov.push_back({
        {0x0f, 0x20},
        Pat<r64, ctrl>{},
        Emitter<MR>{}
    });

    // 0x0F 0x22 MOV CR0-CR8, r64 -- RM
    mov.push_back({
        {0x0f, 0x22},
        Pat<ctrl, r64>{},
        Emitter<RM>{}
    });

    // 0x0F 0x21 MOV r64, DR0-DR7 -- MR 
    mov.push_back({
        {0x0f, 0x21},
        Pat<r64, dbg>{},
        Emitter<MR>{}
    });

    // 0x0F 0x23 MOV DR0-DR7, r64 -- RM
    mov.push_back({
        {0x0f, 0x23},
        Pat<dbg, r64>{},
        Emitter<RM>{}
    });
}

template <X86IK ik>
auto encode(Span<const X86Op> ops) -> Opt<ByteVec> {
    if (not git_.contains(ik)) { unreachable("Unsupported instruction."); }

    for (const InstrExpr& instr_expr : git_[ik]) {
        if (not instr_expr.match(ops)) { continue; }
        return instr_expr.emit(ops);
    }

    return std::nullopt;
}
