#ifndef __X86_ASSEMBLER_X86_FRONT_END_HH__
#define __X86_ASSEMBLER_X86_FRONT_END_HH__

#include "lib/core.hh"
#include "lib/x86_core.hh"

#include <cstring>
#include <string_view>

namespace fiska {

struct Module;

enum struct X86IK {
    Mov,
};

enum struct BW : u16 {
    B8 = 8,
    B16 = 16,
    B32 = 32,
    B64 = 64
};

// Register Ids.
// Never change the declaration order or the values of this enumeration.
enum struct RI {
    Rax = 0,  Es = 16, Cr0 = 24,  Dbg0 = 40, 
    Rcx = 1,  Cs = 17, Cr1 = 25,  Dbg1 = 41, 
    Rdx = 2,  Ss = 18, Cr2 = 26,  Dbg2 = 42, 
    Rbx = 3,  Ds = 19, Cr3 = 27,  Dbg3 = 43, 
    Rsp = 4,  Fs = 20, Cr4 = 28,  Dbg4 = 44, 
    Rbp = 5,  Gs = 21, Cr5 = 29,  Dbg5 = 45, 
    Rip = 5,
    Rsi = 6,           Cr6 = 30,  Dbg6 = 46, 
    Rdi = 7,           Cr7 = 31,  Dbg7 = 47, 
    R8 = 8,            Cr8 = 32,  Dbg8 = 48,  
    R9 = 9,            Cr9 = 33,  Dbg9 = 49,
    R10 = 10,          Cr10 = 34, Dbg10 = 50,
    R11 = 11,          Cr11 = 35, Dbg11 = 51,
    R12 = 12,          Cr12 = 36, Dbg12 = 52,
    R13 = 13,          Cr13 = 37, Dbg13 = 53,
    R14 = 14,          Cr14 = 38, Dbg14 = 54,
    R15 = 15,          Cr15 = 39, Dbg15 = 55,
};

enum struct RK {
    Gp, Seg, Ctrl, Dbg
};

enum struct MK {
    BaseDisp,
    BaseIndexDisp,
    IndexDisp,
    DispOnly
};

enum struct TK {
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

    // Multi-character tokens.
    // Identifier
    Ident,
    // Number
    Num,
    // Bit width. (e.g. 'b8', 'b16', 'b32')
    BitWidth,
    // keyword 'fn'
    Fn,
    // x86 Registers.
    Reg,
    // x86 mnemonic. (e.g 'mov', 'addcx')
    Mnemonic,

    Eof,
};

enum struct ExprKind {
    ProcExpr,
};

// Error kind.
// This enum will decide what short message will be shown to the user when 
// encountering an error during compilation.
enum struct ErrKind {
    UnrecognizedChar,
    UnexpectedChar,
    UnexpectedTok,
    NumberOverflow,
    IllegalValue,
};

struct Tok {
    TK kind_{};
    StrRef str_{};
    Location loc_{};

    static auto spelling(TK kind) -> StrRef;
};

struct Error {
    ErrKind kind_{};
    Context* ctx_{};
    Location loc_{};
    union {
        char c_;
        TK tok_kind_;
        StrRef overflowed_num_;
    } data_{};
};

struct Reg {
    BW bit_width_{};
    RI id_{};

    auto index() -> u8 { return +id_ & 0x7; }
    auto requires_extension() -> bool {
        return (+id_ >= +RI::R8 and +id_ <= +RI::R15)
            or (+id_ >= +RI::Cr8 and +id_ <= +RI::Cr15)
            or (+id_ >= +RI::Dbg8 and +id_ <= +RI::Dbg15);
    }
    auto kind() -> RK {
        if (+id_ >= +RI::Rax and +id_ <= +RI::R15) { return RK::Gp; }
        if (+id_ >= +RI::Es and +id_ <= +RI::Gs) { return RK::Seg; }
        if (+id_ >= +RI::Cr0 and +id_ <= +RI::Cr15) { return RK::Ctrl; }
        if (+id_ >= +RI::Dbg0 and +id_ <= +RI::Dbg15) { return RK::Dbg; }
        unreachable("Unknown register id (RI) encountered.");
    }
};

struct Mem {
    static constexpr u8 kmod_mem = 0b00;
    static constexpr u8 kmod_mem_disp8 = 0b01;
    static constexpr u8 kmod_mem_disp32 = 0b10;
    static constexpr u8 kmod_reg = 0b11;
    static constexpr u8 ksib_marker = 0b100;
    static constexpr u8 ksib_no_index_reg = 0b100;
    static constexpr u8 ksib_no_base_reg = 0b101;

    enum struct Scale : i8 {
        One = 0,
        Two = 1,
        Four = 2,
        Eight = 8
    };

    BW bit_width_{};
    Opt<Reg> base_reg_{std::nullopt};
    Opt<Reg> index_reg_{std::nullopt};
    Opt<i64> disp_{std::nullopt};
    Opt<Scale> scale_{std::nullopt};

    [[nodiscard]] auto kind() -> MK {
        todo();
    }
};

struct Moffs {
    BW bit_width_{};
    i64 inner_{};

    auto as_i64() -> i64 { return inner_; }
};

struct Imm {
    BW bit_width_{};
    i64 inner_{};

    template <OneOf<i8, i16, i32, i64> Size>
    auto as() -> Size { return static_cast<Size>(inner_); }
};

struct X86Op {
    using Inner = std::variant<
        std::monostate,
        Reg,
        Mem,
        Moffs,
        Imm
    >;
    Inner inner_{};

    X86Op(Inner op) : inner_(op) {}

    template <typename... Ts>
    constexpr bool is() const { return (std::holds_alternative<Ts>(inner_) or ...); }

    template <typename T>
    auto as() -> T& { return std::get<T>(inner_); }

    template <typename T>
    auto as() const -> const T& { return std::get<T>(inner_); }
};

template <typename T>
concept IsX86Op = std::same_as<T, Reg> or std::same_as<T, Imm>
               or std::same_as<T, Mem> or std::same_as<T, Moffs>;

// All |X86Instruction|s and all its derived structs are owned
// by the procedure containing them.
struct X86Instruction {
    X86IK kind_{};

    X86Instruction(X86IK kind) : kind_(kind) {}
    virtual ~X86Instruction() = default;
};

struct Mov : public X86Instruction {
    X86Op dst_;
    X86Op src_;

    Mov(X86Op dst, X86Op src) 
        : X86Instruction(X86IK::Mov), dst_(dst), src_(src) {}
};

struct Expr {
    ExprKind kind_{};

    Expr(ExprKind kind) : kind_(kind) {}
    virtual ~Expr() = default;

    void* operator new(usz sz, Module* mod);
    void* operator new(usz sz) = delete;
};

struct ProcExpr : public Expr {
    StrRef func_name_{};
    Vec<Box<X86Instruction>> instructions_{};

    ProcExpr(StrRef func_name, Vec<Box<X86Instruction>> instructions)
        : Expr(ExprKind::ProcExpr), func_name_(func_name),
        instructions_(std::move(instructions)) {}
};

struct TokStream {
    using Storage = Vec<Tok>;
    using Iterator = Vec<Tok>::iterator;
    Storage storage_;

    auto allocate() -> Tok* { return &storage_.emplace_back(); }
    auto begin() -> Iterator { return storage_.begin(); }
    auto end() -> Iterator { return storage_.end(); }
};

struct Module {
    StringInterner strings_;
    TokStream tokens_;
    Vec<Expr*> ast_;
    Vec<ProcExpr*> procs_;
    Context* ctx_{};

    Module() {}
    ~Module();

    Module(const Module&) = delete;
    Module(Module&&) = delete;
    Module& operator=(const Module&) = delete;
    Module& operator=(Module&&) = delete;
};

struct Context {
    Vec<Box<File>> files_;
    Vec<Box<Module>> modules_;

    Context() {}

    Context(const Context&) = delete;
    Context(Context&&) = delete;
    Context& operator=(const Context&) = delete;
    Context& operator=(Context&&) = delete;

    auto get_file(u16 fid) -> File*; 
    auto load_file(const fs::path& path) -> u16; 
};

struct Lexer {
    const char* curr_{};
    const char* end_{};
    char c_{};
    u16 fid_{};
    Module* mod_{};

    explicit Lexer(File* file, Module* mod)
        : curr_(file->data()),
        end_(file->data() + file->size()),
        fid_(file->fid_), 
        mod_(mod)
    {
        next_c();
        next_tok();
    }

    auto eof() -> bool { return curr_ >= end_; }
    auto tok() -> Tok&;
    auto peek_c() -> char;
    auto file_offset() -> u32;
    void next_c();
    void next_tok();
    void next_tok_helper();
    void lex_comment();
    void lex_hex_digit();
    void lex_digit();
    void lex_ident();
    static void lex_file_into_module(File* file, Module* mod);
};

template <typename... Args>
[[noreturn]] auto report_error(
    Error err,
    fmt::format_string<Args...> fmt,
    Args&&... args
) -> void
{
    using enum fmt::color;
    using enum fmt::emphasis;

    LineColInfo info = err.loc_.line_col_info(err.ctx_);
    File* file = err.ctx_->get_file(err.loc_.fid_);

    auto print_repeated = [](
            char c,
            u32 times,
            Opt<fmt::text_style> style = std::nullopt
            ) 
    { 
        fmt::text_style text_style = style.value_or(static_cast<fmt::emphasis>(0));
        while (times--) { fmt::print(text_style, "{}", c); } 
    };
    auto print_ptr_range = [](
            const char* beg,
            const char* end,
            Opt<char> c = std::nullopt,
            Opt<fmt::text_style> style = std::nullopt)
    {
        fmt::text_style text_style = style.value_or(static_cast<fmt::emphasis>(0));
        for (const char* ptr = beg; ptr != end; ++ptr) {
            fmt::print(text_style, "{}", c.value_or(*ptr));
        }
    };


    // Print the file name, line number and column number of where the error happened.
    fmt::print(bold | underline | fg(medium_slate_blue),
            "\u2192 {}:{}:{}", file->path_.string() , info.line_, info.col_);

    // Print a short summary of what the error is.
    fmt::print(bold | fg(red), " Compile error: ");
    switch (err.kind_) {
    case ErrKind::UnexpectedChar: {
        fmt::print(bold | fg(light_gray), "Unexpected character encountered: '{}'.", err.data_.c_);
        break;
    }
    case ErrKind::UnrecognizedChar: {
        fmt::print(bold | fg(light_gray), "Unrecognized character encountered: '{}'.", err.data_.c_);
        break;
    }
    case ErrKind::UnexpectedTok: {
        fmt::print(bold | fg(light_gray), "Unexpected token encountered: '{}'.", Tok::spelling(err.data_.tok_kind_));
        break;
    }
    case ErrKind::NumberOverflow: {
        fmt::print(bold | fg(light_gray), "Number overflow when converting number: '{}'.", err.data_.overflowed_num_);
        break;
    }
    case ErrKind::IllegalValue: {
        fmt::print(bold | fg(light_gray), "Illegal value encountered.");
        break;
    }
    } // switch
    fmt::print("\n");

    // Print the line number and the vertical dash.
    fmt::print("{} | ", info.line_);

    // Print the line where the error occured and highlight the problematic range.
    const char* error_pos = file->data() + err.loc_.pos_;
    StrRef problematic_range = StrRef{error_pos, err.loc_.len_};

    print_ptr_range(info.line_start_, error_pos); 
    fmt::print(fg(red) | bold, "{}", problematic_range);
    print_ptr_range(error_pos + err.loc_.len_, info.line_end_);
    fmt::print("\n");

    // Print a '^' and a detailed error message below the highlighted char. 
    // The highlighted char is defined as the last char we lexed when the error happened.
    //
    // Skip the line number and the vertical dash.
    print_repeated(' ',/*side_bar_size=*/utils::number_width(info.line_) + std::strlen(" | "));

    print_ptr_range(info.line_start_, error_pos, ' ');
    assert(err.loc_.len_ >= 1, "Unsigned integer overflow detected.");
    fmt::print(fg(red) | bold, "^"); print_repeated('~', err.loc_.len_ - 1, bold | fg(red));
    fmt::print(fg(medium_slate_blue) | bold, " {}", fmt::format(fmt, std::forward<Args>(args)...));
    fmt::print("\n");

    std::exit(1);
}

struct Parser {
    Module* mod_{};
    u16 fid_{};
    TokStream::Iterator curr_tok_it_{};

    explicit Parser(File* file, Module* mod) 
        : mod_(mod), fid_(file->fid_), curr_tok_it_(mod->tokens_.begin()) {}

    auto at(std::same_as<TK> auto... tok_tys) -> bool {
        return ((tok().kind_ == tok_tys) or ...);
    }

    auto consume(std::same_as<TK> auto... tok_tys) -> bool {
        if (not at(tok_tys...)) { return false; }
        next_tok();
        return true;
    }

    void expect(std::same_as<TK> auto... tok_kind) {
        if ((consume(tok_kind) and ...)) { return; }
        expect_error_handler(tok_kind...):
    }
    void expect_either(std::same_as<TK> auto... tok_kind) {
        if ((consume(tok_kind) or ...)) { return; }
        expect_error_handler(tok_kind...):
    }

    [[noreturn]] void expect_error_handler(std::same_as<TK> auto... tok_kind) {
        std::string tokens_spelling{};
        auto helper = [&](TK tok_kind) {
            tokens_spelling += fmt::format("{}, ", Tok::spelling(tok_kind));
        }();
        (helper(tok_kind), ...);

        // Remove the last " ," suffix from the list of token spellings.
        tokens_spelling.pop_back();
        tokens_spelling.pop_back();

        Erorr err {
            .kind_ = ErrKind::UnexpectedTok,
            .ctx_ = mod_->ctx_,
            .loc_ = tok().loc_,
            .data_ = { .tok_kind = tok().kind_ }
        };
        report_error(err, "was expecting the following tokens '{}' but found '{}' instead.",
                fmt::format("[{}]", token_spelling), err.data_.tok_kind_);
    }

    auto parse_proc_expr() -> ProcExpr*;
    auto parse_x86_instruction() -> X86Instruction*;
    auto parse_x86_operand() -> X86Op;

    template <OneOf<Reg, Imm, Mem, Moffs> Type>
    auto try_parse_x86_operand() -> Opt<Type>;

    auto match_next_toks(std::same_as<TK> auto... tok_tys) -> bool {
        u32 lookbehind = 0;
        auto match_tok = [&](TK tok_kind) {
            return peek_tok(lookbehind++).kind_ == tok_kind;
        };
        
        return (match_tok(tok_tys) and ...);
    }

    auto tok() -> const Tok&;
    auto prev(u32 lookbehind = 0) -> const Tok&;
    auto peek_tok(u32 lookahead = 0) -> const Tok&;
    void next_tok();
    static void parse_file_into_module(File* file, Module* mod);
};

}  // namespace fiska

#endif
