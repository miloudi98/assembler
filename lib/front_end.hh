#ifndef __X86_ASSEMBLER_X86_FRONT_END_HH__
#define __X86_ASSEMBLER_X86_FRONT_END_HH__

#include "lib/core.hh"
#include "lib/x86_core.hh"

#include <cstring>
#include <string_view>

namespace fiska {

enum struct BW : u16 {
    B8 = 8,
    B16 = 16,
    B32 = 32,
    B64 = 64
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

struct Tok {
    TK kind_{};
    StrRef str_{};
    Location loc_{};

    auto spelling() -> StrRef;
};

struct Expr {
};

struct ProcExpr : public Expr {
};

struct TokStream {
    using Storage = Vec<Tok>;
    Storage storage_;

    auto allocate() -> Tok* { return &storage_.emplace_back(); }
};

struct Module {
    StringInterner strings_;
    TokStream tokens_;
    Vec<Expr*> ast_;
    Vec<ProcExpr*> procs_;
    Context* ctx_{};

    Module() {}

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

    static void lex_file_into_module(File* file, Module* mod) {
        Lexer lxr{file, mod};
        while (lxr.tok().kind_ != TK::Eof) {
            lxr.next_tok();
        }
    }

    template <typename... Args>
    [[noreturn]] auto error(fmt::format_string<Args...> fmt, Args&&... args) -> void {
        using enum fmt::color;
        using enum fmt::emphasis;

        LineColInfo info = tok().loc_.line_col_info(mod_->ctx_);
        File* file = mod_->ctx_->get_file(fid_);
        auto print_ptr_range = [](
                const char* beg,
                const char* end,
                Opt<char> c = std::nullopt,
                Opt<text_style> style = std::nullopt)
        {
            const text_style& ts = style.value_or(static_cast<fmt::emphasis>(0));
            for (const char* ptr = beg; ptr != end; ++ptr) {
                fmt::print(ts, "{}", c.value_or(*ptr));
            }
        };

        // Print the file name, line number and column number of where the error happened.
        fmt::print(bold | underline | fg(medium_slate_blue),
                "\u2192 {}:{}:{}", file->path_.string() , info.line_, info.col_);

        // Print the error kind and the error message to the user.
        fmt::print(bold | fg(red), " Compile error: ");
        fmt::print(fmt, std::forward<Args>(args)...);
        fmt::print("\n");

        // Print the line number and the vertical dash.
        u32 side_bar_size = utils::number_width(info.line_) + std::strlen(" | ");
        fmt::print("{} | ", info.line_);

        const char* pos_of_highlighted_char = file->data() + tok().loc_.pos_;
        print_ptr_range(info.line_start_, pos_of_char_to_highlight, ' '); 
        fmt::print(fg(red) | bold, "{}", *pos_of_char_to_highlight);
        print_ptr_range(pos_of_char_to_highlight + 1, info.line_end_, ' ');
        fmt::print("\n");

        // Print a '^' below the highlighted char. The highlighted char is defined
        // as the last char we lexed when the error happened.
        //
        // Skip the line number and the vertical dash.
        for (u32 i = 0; i < side_bar_size; ++i) { fmt::print(" "); }

        print_ptr_range(info.line_start_, pos_of_highlighted_char, ' ');
        fmt::print(fg(red) | bold, "^");
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
};

}  // namespace fiska

#endif
