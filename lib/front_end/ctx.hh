#ifndef __X86_ASSEMBLER_LIB_FRONT_END_CTX_HH__
#define __X86_ASSEMBLER_LIB_FRONT_END_CTX_HH__

#include "lib/support/core.hh"
#include "lib/x86/common.hh"

namespace fiska::x86 {

// Forward declarations.
namespace fe { struct Ctx; }

struct File {
    u16 fid_{};
    fs::path path_;
    Vec<char> code_;

    File(const File&) = delete;
    File(File&&) = delete;
    File& operator=(const File&) = delete;
    File& operator=(File&&) = delete;

    File(u16 fid, fs::path path, Vec<char> code) 
        : fid_(fid), path_(path), code_(std::move(code)) {}

    auto data() const -> const char* { return code_.data(); }
    auto size() const -> u64 { return code_.size(); }
};

struct Location {
    u32 pos_{};
    u32 len_ = 1;
    u16 fid_{};

    auto merge(const Location& o) -> Location&;
};

struct ErrorSpan {
    static constexpr i8 kCtxSize = 3;

    const char* start_{};
    const char* end_{};
    const char* ctx_start_{};
    const char* ctx_end_{};
    Str msg_;
    u32 line_ = 1;
    u32 col_ = 1;

    template <typename... Args>
    [[nodiscard]] static auto from(
        fe::Ctx* ctx,
        Location loc,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) -> ErrorSpan
    {
        return err_span_builder(ctx, loc, fmt::format(fmt, std::forward<Args>(args)...));
    }

    [[nodiscard]] static auto err_span_builder(fe::Ctx* ctx, Location loc, Str msg) -> ErrorSpan;
    [[noreturn]] static auto emit(ErrorSpan err_span) -> void;
};

struct StringInterner {
    std::unordered_set<StrRef> unique_strings_;
    Vec<char*> storage_;

    auto save(StrRef str) -> StrRef; 
    ~StringInterner(); 
};

namespace fe {

// Token kind.
enum struct TK {
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
    TK kind_{};
    StrRef str_{};
    Location loc_{};
};

struct TokStream {
    using Ref = const TokStream&;
    using Storage = Vec<Tok>;
    using Iterator = Vec<Tok>::iterator;
    Storage storage_;

    auto alloc_tok() -> Tok* { return &storage_.emplace_back(); }
    auto back() -> Tok& { return storage_.back(); }
    auto begin() -> Iterator { return storage_.begin(); }
    auto end() -> Iterator { return storage_.end(); }
};

struct TokStreamView {
    using TKMisMatch = std::pair<i1, std::pair<TK, TK>>;
    const TokStream::Iterator beg_;
    const TokStream::Iterator end_;
    TokStream::Iterator cur_;

    explicit TokStreamView(TokStream::Iterator beg, TokStream::Iterator end)
        : beg_(beg), end_(end), cur_(beg) {}

    auto tok() -> const Tok& { return *cur_; }
    auto advance() -> void { cur_ += (cur_ < end_); }
    auto current() -> TokStream::Iterator { return cur_; }

    auto peek(i32 idx = 0) -> const Tok& {
        auto ptok = cur_ + idx;
        // Bounds checking.
        assert(ptok >= beg_ and ptok < end_);
        return *ptok;
    }

    // Lookahead.
    auto match(std::same_as<TK> auto... tk) -> i1 {
        i32 idx = 0;
        return ((peek(idx++).kind_ == tk) and ...);
    }

    auto at(std::same_as<TK> auto... tk) -> i1 {
        return ((tok().kind_ == tk) or ...);
    }

    auto consume(std::same_as<TK> auto... tk) -> i1 {
        if (not at(tk...)) { return false; }
        advance();
        return true;
    }

    auto ingest(Ctx* ctx, std::same_as<TK> auto... tk) -> void {
        auto [mmatch, diff] = mismatch(tk...);
        if (not mmatch) { return; }

        ErrorSpan::emit(
            ErrorSpan::from(
                ctx,
                tok().loc_,
                "Expected Token: '{}' but found '{}' instead.",
                diff.first,
                diff.second
            )
        );
    }

    auto mismatch(std::same_as<TK> auto... tk) -> TKMisMatch {
        TKMisMatch mm { false, { TK::Invalid, TK::Invalid } };

        auto check = [&](TK tk) -> i1 {
            mm.first = consume(tk);
            if (not mm.first) {
                mm.second = { tk, tok().kind_ };
            }
            return mm.first;
        };

        std::ignore = (check(tk) and ...);
        return mm;
    }
};

struct Expr {
    using List = Vec<Expr*>;
    using ListRef = const List&;

    enum struct Kind {
        Invalid, 

        Proc,
        RegLit,
        MemRefLit,
        ImmLit,
        MoffsLit,
        IntLit,
        Label,

        BinaryOp,
        UnaryOp,

        X86Instr,
    };

    Kind kind_ = Kind::Invalid;
    Location span_{};
    StrRef section_{};

    Expr(Kind kind) : kind_(kind) {}
    virtual ~Expr() = default;

    // Create an expression and bind it to the global context.
    void* operator new(usz sz, Ctx* ctx);
    // Create an expression with a known code section.
    void* operator new(usz sz, Ctx* ctx, StrRef section);
    // Disallow creating expressions with no global context.
    void* operator new(usz sz) = delete;

    auto expand(const Tok& tok) -> Expr* {
        span_.merge(tok.loc_);
        return this;
    }
};

// Register expression.
struct RegLitExpr : Expr {
    BW bw_ = BW::Invalid;
    RI id_ = RI::Invalid;

    RegLitExpr() : 
        Expr(Expr::Kind::RegLit) {}

    RegLitExpr(BW bw, RI id) : 
        Expr(Expr::Kind::RegLit), bw_(bw), id_(id) {}

};

struct MemRefLitExpr : Expr {
    BW bw_ = BW::Invalid;
    RI brid_ = RI::Invalid;
    RI irid_ = RI::Invalid;
    Expr* scale_{};
    Expr* disp_{};

    MemRefLitExpr() : Expr(Expr::Kind::MemRefLit) {}
};

struct ImmLitExpr : Expr {
    BW bw_ = BW::Invalid;
    Expr* value_ = nullptr;

    ImmLitExpr() : Expr(Expr::Kind::ImmLit) {}
    ImmLitExpr(BW b, Expr* v) :
        Expr(Expr::Kind::ImmLit), bw_(b), value_(v) {}
};

struct MoffsLitExpr : Expr {
    BW bw_ = BW::Invalid;
    Expr* addr_ = nullptr;

    MoffsLitExpr() : Expr(Expr::Kind::MoffsLit) {}
    MoffsLitExpr(BW b, Expr* a) :
        Expr(Expr::Kind::MoffsLit), bw_(b), addr_(a) {}
};

struct IntLitExpr : Expr {
    i64 value_{};

    IntLitExpr(i64 v) : 
        Expr(Expr::Kind::IntLit), value_(v) {}
};

struct LabelExpr : Expr {
    StrRef name_;

    LabelExpr(StrRef name) :
        Expr(Expr::Kind::Label), name_(name) {}
};

struct BinaryOpExpr : Expr {
    Expr* lhs_ = nullptr;
    Expr* rhs_ = nullptr;
    TK op_ = TK::Invalid;

    BinaryOpExpr(TK op, Expr* lhs, Expr* rhs) : 
        Expr(Expr::Kind::BinaryOp), lhs_(lhs), rhs_(rhs), op_(op) {}
};

struct UnaryOpExpr : Expr {
    TK op_ = TK::Invalid;
    Expr* inner_ = nullptr;

    UnaryOpExpr() : Expr(Expr::Kind::UnaryOp) {}
    UnaryOpExpr(TK o, Expr* i) :
        Expr(Expr::Kind::UnaryOp), op_(o), inner_(i) {}
};

struct X86InstrExpr : Expr {
    X86Mnemonic mmic_ = X86Mnemonic::Invalid;
    Vec<Expr*> ops_;

    X86InstrExpr() : Expr(Expr::Kind::X86Instr) {}
};

struct ProcExpr : Expr {
    StrRef name_;
    Vec<X86InstrExpr*> body_;

    ProcExpr() : Expr(Expr::Kind::Proc) {}
};

struct VarExpr : Expr {
    StrRef name_;
    Expr* value_;
};

// Global context for the entire front end.
// It is not thread-safe as everything is single-threaded for now.
struct Ctx {
    // List of files loaded.
    Vec<Box<File>> files_{};
    // Tokens of each file loaded.
    Vec<TokStream> tok_streams_{};
    // Ast nodes allocated.
    Vec<Expr*> ast_{};
    // Pool of interned strings.
    StringInterner str_pool_{};

    // Dealloacte all ast nodes.
    ~Ctx();
    // Load file to memory.
    auto load_file(const fs::path& path) -> File*;
    // Return the file with fid |fid|.
    auto get_file(u16 fid) -> File*;
};

} // namespace fe

} // namespace fiska::x86

// Support formatting tokens.
template <>
struct fmt::formatter<fiska::x86::fe::TK> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    template <typename FormatContext>
    auto format(const fiska::x86::fe::TK tk, FormatContext& ctx) const -> decltype(ctx.out()) {
        using fiska::x86::fe::TK;

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

#endif // __X86_ASSEMBLER_LIB_FRONT_END_CTX_HH__
