#ifndef __X86_ASSEMBLER_X86_FRONT_END_HH__
#define __X86_ASSEMBLER_X86_FRONT_END_HH__

#include "lib/core.hh"
#include "lib/x86_core.hh"

#include <cstring>
#include <string_view>

namespace fiska {

struct Module;

enum struct BW : u16 {
    B8 = 8,
    B16 = 16,
    B32 = 32,
    B64 = 64
};

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

// Error kind.
// This enum will decide what short message will be shown to the user when 
// encountering an error during compilation.
enum struct EK {
    UnrecognizedChar,
    UnexpectedChar,
};

struct Tok {
    TK kind_{};
    StrRef str_{};
    Location loc_{};

    static auto spelling(TK kind) -> StrRef;
};

struct X86Instruction {
};


enum struct ExprKind {
    ProcExpr,
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

    template <typename... Args>
    [[noreturn]] auto error(EK ek, fmt::format_string<Args...> fmt, Args&&... args) -> void {
        using enum fmt::color;
        using enum fmt::emphasis;

        LineColInfo info = tok().loc_.line_col_info(mod_->ctx_);
        File* file = mod_->ctx_->get_file(fid_);
        auto print_ptr_range = [](
                const char* beg,
                const char* end,
                Opt<char> c = std::nullopt,
                Opt<fmt::text_style> style = std::nullopt)
        {
            const fmt::text_style& ts = style.value_or(static_cast<fmt::emphasis>(0));
            for (const char* ptr = beg; ptr != end; ++ptr) {
                fmt::print(ts, "{}", c.value_or(*ptr));
            }
        };
        auto print_spaces = [](u32 num) { while(num--) { fmt::print(" "); } };


        // Print the file name, line number and column number of where the error happened.
        fmt::print(bold | underline | fg(medium_slate_blue),
                "\u2192 {}:{}:{}", file->path_.string() , info.line_, info.col_);

        // Print the error kind and the error message to the user.
        fmt::print(bold | fg(red), " Compile error: ");
        // Print a short message summarizing the error.
        switch (ek) {
        case EK::UnexpectedChar: {
            fmt::print(bold | fg(light_gray), "Unexpected character encountered: '{}'.", c_);
            break;
        }
        case EK::UnrecognizedChar: {
            fmt::print(bold | fg(light_gray), "Unrecognized character: '{}'.", c_);
            break;
        }
        } // switch
        fmt::print("\n");

        // Print the line number and the vertical dash.
        fmt::print("{} | ", info.line_);

        // Print the line where the error occured and highlight the last char we 
        // lexed when the error occured.
        const char* pos_of_highlighted_char = file->data() + tok().loc_.pos_;

        print_ptr_range(info.line_start_, pos_of_highlighted_char); 
        fmt::print(fg(red) | bold, "{}", *pos_of_highlighted_char);
        print_ptr_range(pos_of_highlighted_char + 1, info.line_end_);
        fmt::print("\n");

        // Print a '^' and a detailed error message below the highlighted char. 
        // The highlighted char is defined as the last char we lexed when the error happened.
        //
        // Skip the line number and the vertical dash.
        print_spaces(/*side_bar_size=*/utils::number_width(info.line_) + std::strlen(" | "));

        print_ptr_range(info.line_start_, pos_of_highlighted_char, ' ');
        fmt::print(fg(red) | bold, "^ ");
        fmt::print(fg(medium_slate_blue) | bold, fmt::format(fmt, std::forward<Args>(args)...));
        print_ptr_range(pos_of_highlighted_char + 1, info.line_end_, ' ');
        fmt::print("\n");

        std::exit(1);
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

struct Parser {
    Module* mod_{};
    u16 fid_{};
    TokStream::Iterator curr_tok_it_{};

    explicit Parser(File* file, Module* mod) 
        : mod_(mod), fid_(file->fid_), curr_tok_it_(mod->tokens_.begin()) {}

    template <typename... Args>
    [[noreturn]] void error(EK ek, fmt::format_string<Args...> fmt, Args&&... args) {
        todo("implement error reporting in the parser");
    }

    auto at(std::same_as<TK> auto... tok_tys) -> bool {
        return ((tok().kind_ == tok_tys) or ...);
    }

    auto consume(std::same_as<TK> auto... tok_tys) -> bool {
        if (not at(tok_tys...)) { return false; }
        next_tok();
        return true;
    }

    void expect(TK tok_kind) {
        if (consume(tok_kind)) { return; }
        error(EK::UnexpectedChar
                , "was expecting the token '{}' but found '{}' instead.",
                Tok::spelling(tok_kind), Tok::spelling(tok().kind_));
    }

    auto parse_proc_expr() -> ProcExpr*;
    auto parse_x86_instruction() -> X86Instruction*;
    auto tok() -> const Tok&;
    void next_tok();
    static void parse_file_into_module(File* file, Module* mod);
};

}  // namespace fiska

#endif
