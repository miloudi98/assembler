#ifndef __X86_ASSEMBLER_LIB_FRONTEND_TOKEN_HH__
#define __X86_ASSEMBLER_LIB_FRONTEND_TOKEN_HH__

#include "lib/common/base.hh"
#include "lib/common/support.hh"

namespace fiska::assembler::frontend {

// Token kind.
enum struct TK : i32 {
    Invalid,

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
    // '/'
    Slash,
    // '*'
    Star,
    // '='
    Eq,

    // Multi-character tokens.
    // Identifier
    Ident,
    // String literal.
    StrLit,
    // Number
    Num,
    // Bit width. (e.g. 'b8', 'b16', 'b32')
    BitWidth,
    // keyword 'let'
    Let,
    // keyword 'fn'
    Fn,
    // keyword 'section'
    Section,
    // x86 Registers.
    Reg,
    // x86 mnemonic. (e.g 'mov', 'addcx')
    Mnemonic,
    // End of file.
    Eof,

    Unknown,
};


struct Tok {
    Span span_{};
    TK kind_ = TK::Invalid;
    StrRef str_{};

    auto is_one_char_token() const -> i1 {
        return (+kind_ >= +TK::LParen) 
            and (+kind_ <= +TK::Eq);
    }
};

struct TokStream {
    NOT_COPYABLE(TokStream);

    using Storage = Vec<Tok>;
    Storage storage_;

    TokStream() = default;
    TokStream(TokStream&& other) : storage_(std::move(other.storage_)) {}

    auto alloc() -> Tok* { return &storage_.emplace_back(); }
    auto begin() -> Storage::iterator { return storage_.begin(); }
    auto back() -> Tok& { return storage_.back(); }
    auto end() -> Storage::iterator { return storage_.end(); }

    auto size() const -> u64 { return storage_.size(); }
    auto back() const -> const Tok& { return storage_.back(); }
    auto empty() const -> i1 { return storage_.empty(); }

    auto operator[](i32 idx) -> const Tok& {
        assert(idx >= 0 and idx < i64(size()), "Index out of bounds.");
        return storage_[u32(idx)];
    }
};

} // namespace fiska::assembler::frontend

// Support formatting tokens.
template <>
struct fmt::formatter<fiska::assembler::frontend::TK> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    template <typename FormatContext>
    auto format(const fiska::assembler::frontend::TK tk, FormatContext& ctx) const -> decltype(ctx.out()) {
        using fiska::assembler::frontend::TK;

        auto str_of_tk = [&] {
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
                case TK::Star: return "*";
                case TK::Eq: return "=";
                case TK::StrLit: return "STR_LIT";
                case TK::Ident: return "IDENTIFIER";
                case TK::Section: return "SECTION";
                case TK::Num: return "NUMBER";
                case TK::BitWidth: return "BIT_WIDTH";
                case TK::Fn: return "FN";
                case TK::Let: return "LET";
                case TK::Reg: return "REG";
                case TK::Mnemonic: return "MNEMONIC";
                case TK::Unknown: return "UNKNOWN";
                case TK::Invalid: return "INVALID";
                case TK::Eof: return "EOF";
            } // switch
            unreachable();
        }();
        return fmt::format_to(ctx.out(), "{}", str_of_tk);
    }
};

#endif // __X86_ASSEMBLER_LIB_FRONTEND_TOKEN_HH__
