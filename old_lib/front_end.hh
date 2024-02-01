#ifndef __X86_ASSEMBLER_X86_FRONT_END_HH__
#define __X86_ASSEMBLER_X86_FRONT_END_HH__

#include "lib/core.hh"
#include "lib/x86_core.hh"

#include <cstring>
#include <string_view>
#include <random>

namespace fiska {

struct Module;
struct ProcExpr;

enum struct X86IK {
    Mov,
};

enum struct BW : u16 {
    B8 = 8,
    B16 = 16,
    B24 = 24,
    B32 = 32,
    B64 = 64
};
auto str_of_bw(BW bit_width) -> StrRef;

// Register Ids.
// Never change the declaration order or the values of this enumeration.
// TODO(miloudi): Add a mapping from RI to BW showing the possible sizes for a certain RI.
// It's useful because non general purpose registers have a set size and also because
// gprs like 'rip' can't be 8 bits. We need to make sure not to generate those configurations
// when generating instances.
enum struct RI {
    Rax = 0,      Es = 16, Cr0 = 24,  Dbg0 = 40, 
    Rcx = 1,      Cs = 17, Cr1 = 25,  Dbg1 = 41, 
    Rdx = 2,      Ss = 18, Cr2 = 26,  Dbg2 = 42, 
    Rbx = 3,      Ds = 19, Cr3 = 27,  Dbg3 = 43, 
    Rsp = 4,      Fs = 20, Cr4 = 28,  Dbg4 = 44, 
    Rbp = 5,      Gs = 21, Cr5 = 29,  Dbg5 = 45, 
    Rsi = 6,               Cr6 = 30,  Dbg6 = 46, 
    Rdi = 7,               Cr7 = 31,  Dbg7 = 47, 
    R8 = 8,                Cr8 = 32,  Dbg8 = 48,  
    R9 = 9,                Cr9 = 33,  Dbg9 = 49,
    R10 = 10,              Cr10 = 34, Dbg10 = 50,
    R11 = 11,              Cr11 = 35, Dbg11 = 51,
    R12 = 12,              Cr12 = 36, Dbg12 = 52,
    R13 = 13,              Cr13 = 37, Dbg13 = 53,
    R14 = 14,              Cr14 = 38, Dbg14 = 54,
    R15 = 15,              Cr15 = 39, Dbg15 = 55,
    // Rsp
    Rah = (1 << 10) | 4,
    // Rbp
    Rch = (1 << 10) | 5,
    // Rsi
    Rdh = (1 << 10) | 6,
    // Rdi
    Rbh = (1 << 10) | 7,
    // Rbp 
    Rip = (1 << 11) | 5,
};
auto str_of_ri(RI id) -> StrRef;

enum struct RK {
    Gp, Seg, Ctrl, Dbg
};
auto str_of_rk(RK kind) -> StrRef;

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
    IllegalStmt,
};

enum struct OpEn {
    MR,
    RM,
    FD,
    TD,
    OI,
    MI
};

struct Tok {
    TK kind_{};
    StrRef str_{};
    Location loc_{};

    static auto spelling(TK kind) -> StrRef;
};

// TODO(miloudi): Remove this nonsense.
struct Error {
    ErrKind kind_{};
    Ctx* ctx_{};
    Location loc_{};
    union {
        char c_;
        TK tok_kind_;
        StrRef overflowed_num_;
        StrRef illegal_lxm_;
    } data_{};
};

struct Reg {
    BW bit_width_{};
    RI id_{};

    auto index() const -> u8 { return +id_ & 0x7; }
    auto requires_ext() const -> i1 {
        return (+id_ >= +RI::R8 and +id_ <= +RI::R15)
            or (+id_ >= +RI::Cr8 and +id_ <= +RI::Cr15)
            or (+id_ >= +RI::Dbg8 and +id_ <= +RI::Dbg15);
    }
    auto kind() const -> RK {
        using enum RI;
        if ((::is<Rip, Rah, Rch, Rdh, Rbh>(id_)) or (+id_ >= +Rax and +id_ <= +R15)) { return RK::Gp; }
        if (+id_ >= +Es and +id_ <= +Gs) { return RK::Seg; }
        if (+id_ >= +Cr0 and +id_ <= +Cr15) { return RK::Ctrl; }
        if (+id_ >= +Dbg0 and +id_ <= +Dbg15) { return RK::Dbg; }
        unreachable("Unknown register id (RI) encountered.");
    }

};

struct Mem {
    enum struct Scale : u8 {
        One = 0,
        Two = 1,
        Four = 2,
        Eight = 3
    };

    BW bit_width_{};
    Opt<Reg> base_reg_{std::nullopt};
    Opt<Reg> index_reg_{std::nullopt};
    Opt<Scale> scale_{std::nullopt};
    Opt<i64> disp_{std::nullopt};

    [[nodiscard]] auto disp() const -> Opt<i64> {
        using enum RI;

        switch (kind()) {
        case MK::BaseDisp:
        case MK::BaseIndexDisp: {
            if (::is<Rbp, R13>(base_reg_->id_)) {
                return disp_.value_or(0);
            }
            break;
        }
        case MK::IndexDisp:
        case MK::DispOnly: {
            return disp_.value_or(0);
        }
        } // switch

        if (disp_ and *disp_ == 0) { return std::nullopt; }
        return disp_;
    }

    [[nodiscard]] auto kind() const -> MK;

    [[nodiscard]] auto sib() const -> Opt<u8>;
};

struct Moffs {
    BW bit_width_{};
    i64 inner_{};

    template <OneOf<i64, u64> Size>
    auto as() const -> Size { return static_cast<Size>(inner_); }
};

struct Imm {
    BW bit_width_{};
    i64 inner_{};

    template <OneOf<i8, i16, i32, i64, u64> Size>
    auto as() const -> Size { return static_cast<Size>(inner_); }
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
    constexpr i1 is() const { return (std::holds_alternative<Ts>(inner_) or ...); }

    template <typename T>
    auto as() -> T& { return std::get<T>(inner_); }

    template <typename T>
    auto as() const -> const T& { return std::get<T>(inner_); }

    auto modrm_encoding() const -> u8;
    auto modrm_mod() const -> u8;
    auto bit_width() const -> BW;
};

// Concept identifying all the x86 operand classes below.
template <typename T>
concept IsX86OpClass = requires(const X86Op& op) {
    { T::match(op) } -> std::same_as<i1>;
};

//====================================================================
// X86OpClass Any combinator. 
// This combinator will be satisfied if the X86Op passed
// to it as an argument satisfies any of the X86OpClasses passed as 
// template arguments.
//====================================================================
template <IsX86OpClass... X86OpClass>
struct Any {
    static constexpr auto match(const X86Op& op) -> i1 {
        return (X86OpClass::match(op) or ...);
    }

    static constexpr auto instances() -> Vec<X86Op> {
        // Concatenate the instances of the underlying operands.
        Vec<X86Op> ret;

        auto helper = [&](const Vec<X86Op>& instances) {
            // TODO(miloudi): reserve memory before you extend the vector.
            ret.insert(ret.end(), instances.begin(), instances.end());
        };

        (helper(X86OpClass::instances()), ...);
        return ret;
    }
};

//=====================================================
// Wild card that accepts any class of x86 operands.
//=====================================================
struct any {
    static constexpr auto match(const X86Op& op) -> i1 { return true; }
};

//=====================================================
// Register classes.
//=====================================================
template <auto... args>
struct r;

template <BW bit_width>
struct r<bit_width> {
    static constexpr auto match(const X86Op& op) -> i1 {
        return op.is<Reg>() 
            and op.as<Reg>().bit_width_ == bit_width
            and op.as<Reg>().kind() == RK::Gp;
    }

    static constexpr auto instances() -> Vec<X86Op> {
        using enum RI;
        Vec<X86Op> ret;

        // This needs to change.
        for (u32 ri = +Rax; ri <= +R15; ++ri) {
            ret.push_back({ Reg{bit_width, RI(ri)} });
        }

        if (bit_width == BW::B8) {
            ret.push_back({ Reg{BW::B8, Rah} });
            ret.push_back({ Reg{BW::B8, Rch} });
            ret.push_back({ Reg{BW::B8, Rdh} });
            ret.push_back({ Reg{BW::B8, Rbh} });
        }

        return ret;
    }
};

template <RK kind>
struct r<kind> {
    static constexpr auto match(const X86Op& op) -> i1 {
        return op.is<Reg>()
            and op.as<Reg>().kind() == kind;
    }

    static constexpr auto instances() -> Vec<X86Op> {
        using enum RI;
        using enum BW;

        if (kind == RK::Gp) { return Any<r<B8>, r<B16>, r<B32>, r<B64>>::instances(); }

        Vec<X86Op> ret;
        auto [start_ri, end_ri, bw] = [&] {
            switch (kind) {
            case RK::Seg: return std::make_tuple(+Es, +Gs, B16);
            case RK::Ctrl: return std::make_tuple(+Cr0, +Cr15, B64);
            case RK::Dbg: return std::make_tuple(+Dbg0, +Dbg15, B64);
            // handled earlier.
            case RK::Gp: unreachable();
            } // switch
            unreachable();
        }();

        for (int ri = start_ri; ri <= end_ri; ++ri) {
            ret.push_back({ Reg{bw, RI(ri)} });
        }

        return ret;
    }
};

template <BW bit_width, RI id>
struct r<bit_width, id> {
    static constexpr auto match(const X86Op& op) -> i1 {
        if (not op.is<Reg>()) { return false; }

        const Reg& reg = op.as<Reg>();
        return reg.bit_width_ == bit_width
            and reg.id_ == id;
    }

    static constexpr auto instances() -> Vec<X86Op> {
        return { {Reg{bit_width, id}} };
    }
};

template <RI id>
struct r<id> {
    static constexpr auto match(const X86Op& op) -> i1 {
        return op.is<Reg>() and op.as<Reg>().id_ == id;
    }

    static constexpr auto instances() -> Vec<X86Op> {
        using enum BW; 

        Vec<X86Op> ret;
        for (BW w : {B8, B16, B32, B64}) {
            if (id == RI::Rip and w == B8) { continue; }

            ret.push_back({ Reg{w, id} });
        }
        return ret;
    }
};

//=====================================================
// Memory classes.
//=====================================================
template <auto... args>
struct m;

template <BW bit_width>
struct m<bit_width> {
    static constexpr auto match(const X86Op& op) -> i1 {
        return op.is<Mem>() and op.as<Mem>().bit_width_ == bit_width;
    }

    static constexpr auto instances() -> Vec<X86Op> {
        using enum BW;
        using enum RI;
        using enum Mem::Scale;

        Vec<X86Op> ret;
        std::mt19937 gen(std::random_device{}());

        std::uniform_int_distribution<> disp_8bits(std::numeric_limits<i8>::min(),
                std::numeric_limits<i8>::max());

        std::uniform_int_distribution<> disp_32bits(std::numeric_limits<i32>::min(),
                std::numeric_limits<i32>::max());

        auto get_random_disp = [&]() {
            // 75% of memory references will have a displacement.
            if ((gen() % 100) < 75) {
                return Opt<int>{gen() & 1 ? disp_8bits(gen) : disp_32bits(gen)};
            }
            return static_cast<Opt<int>>(std::nullopt);
        };

        Vec<X86Op> gp_regs = r<B64>::instances();

        // MK::BaseDisp
        for (const X86Op& op : gp_regs) {
            ret.push_back({ Mem{bit_width, op.as<Reg>(), std::nullopt, std::nullopt, get_random_disp()} });
        }
        // MK::BaseIndexDisp
        for (const X86Op& base : gp_regs) {
            if (::is<Rip>(base.as<Reg>().id_)) { continue; }
            for (Mem::Scale s : {One, Two, Four, Eight}) {
                for (const X86Op& index : gp_regs) {
                    if (::is<Rip, Rsp>(index.as<Reg>().id_)) { continue; }
                    ret.push_back({ Mem{bit_width, base.as<Reg>(), index.as<Reg>(), s, get_random_disp()} });
                }
            }
        }
        // MK::IndexDisp
        for (const X86Op& index : gp_regs) {
            for (Mem::Scale s : {One, Two, Four, Eight}) {
                if (::is<Rip, Rsp>(index.as<Reg>().id_)) { continue; }
                ret.push_back({ Mem{bit_width, std::nullopt, index.as<Reg>(), s, get_random_disp()} });
            }
        }
        // MK::DispOnly
        for (u32 i = 0; i < 10; ++i) {
            ret.push_back({ Mem{bit_width, std::nullopt, std::nullopt, std::nullopt, get_random_disp().value_or(0)} });
        }

        return ret;
    }
};

template <RI base_id>
struct m<base_id> {
    static constexpr auto match(const X86Op& op) -> i1 {
        return op.is<Mem>() 
            and op.as<Mem>().base_reg_ 
            and op.as<Mem>().base_reg_->id_ == base_id;
    }
};

template <RI base_id, RI index_id>
struct m<base_id, index_id> {
    static constexpr auto match(const X86Op& op) -> i1 {
        return m<base_id>::match(op) 
            and op.as<Mem>().index_reg_
            and op.as<Mem>().index_reg_->id_ == index_id;
    }
};

// TODO(miloudi): Plot performance regressions on every commit. By performance
// we simply mean the time it took to execute the millions of automatic tests
// we have that tests every possible instruction.
template <IsX86OpClass auto base_reg_class>
struct m<base_reg_class> {
    static constexpr auto match(const X86Op& op) -> i1 {
        if (not op.is<Mem>()) { return false; }

        constexpr bool is_any_base_reg_class = std::same_as<decltype(base_reg_class), any>;

        const Mem& mem = op.as<Mem>();

        return is_any_base_reg_class or (mem.base_reg_ and base_reg_class.match({*mem.base_reg_}));
    }
};

template <IsX86OpClass auto base_reg_class, IsX86OpClass auto index_reg_class>
struct m<base_reg_class, index_reg_class> {
    static constexpr auto match(const X86Op& op) -> i1 {
        if (not op.is<Mem>()) { return false; }

        constexpr bool accepts_any_base_reg = std::same_as<std::remove_cvref_t<decltype(base_reg_class)>, any>;
        constexpr bool accepts_any_index_reg = std::same_as<std::remove_cvref_t<decltype(index_reg_class)>, any>;

        const Mem& mem = op.as<Mem>();

        i1 base_reg_match = accepts_any_base_reg or (mem.base_reg_ and base_reg_class.match({*mem.base_reg_}));
        i1 index_reg_match = accepts_any_index_reg or (mem.index_reg_ and index_reg_class.match({*mem.index_reg_}));

        return base_reg_match and index_reg_match;
    };
};

//=====================================================
// Immediate classes.
//=====================================================
template <auto... args>
struct i;

template <BW bit_width>
struct i<bit_width> {
    static constexpr auto match(const X86Op& op) -> i1 {
        return op.is<Imm>() 
            and op.as<Imm>().bit_width_ == bit_width;
    }

    static constexpr auto instances(i64 n_inst = 10) -> Vec<X86Op> {
        Vec<X86Op> ret;
        // Generate random numbers that fit in |bit_width| bits.
        i64 mn = -(1LL << (+bit_width - 1));
        i64 mx = (1LL << (+bit_width - 1LL)) - 1LL;

        std::mt19937 gen(std::random_device{}());
        std::uniform_int_distribution<i64> imms(mn, mx);
        while (n_inst--) { ret.push_back({ Imm{bit_width, imms(gen)} }); }
        return ret;
    }
};

//=====================================================
// Memory offset classes.
//=====================================================
template <auto... args>
struct mo;

template <BW bit_width>
struct mo<bit_width> {
    static constexpr auto match(const X86Op& op) -> i1 {
        return op.is<Moffs>() 
            and op.as<Moffs>().bit_width_ == bit_width;
    }

    static constexpr auto instances(i64 n_inst = 10) -> Vec<X86Op> {
        Vec<X86Op> ret;

        std::mt19937 gen(std::random_device{}());
        std::uniform_int_distribution<i64> moffsets(std::numeric_limits<i64>::min(), std::numeric_limits<i64>::max());
        while (n_inst--) { ret.push_back({ Moffs{bit_width, moffsets(gen)} }); }
        return ret;
    }
};


union Sib {
    struct {
        u8 base: 3{};
        u8 index: 3{};
        u8 scale: 2{};
    };
    u8 raw;
};

union ModRM {
    struct {
        u8 rm: 3{};
        u8 reg: 3{};
        u8 mod: 2{};
    };
    u8 raw;
};

union Rex {
    struct {
        // Mod_Rm::r/m or Sib::Base extension or opcode extension.
        u8 b: 1{};
        // Sib::Index extension.
        u8 x: 1{};
        // Mod_Rm::reg extension.
        u8 r: 1{};
        // Operand size override.
        u8 w: 1{};
        u8 mod: 4 {0b0100}; 
    };
    u8 raw;

    auto is_required(Span<const X86Op> ops) const -> i1 {
        using enum BW;
        using enum RI;
        using new_low_byte_regs = Any< 
            ::fiska::r<B8, Rsp>, 
            ::fiska::r<B8, Rbp>,
            ::fiska::r<B8, Rsi>,
            ::fiska::r<B8, Rdi>
        >;

        return w or r or x or b or rgs::any_of(ops, new_low_byte_regs::match);
    }
};

template <typename T>
concept IsX86Op = OneOf<T, Reg, Mem, Moffs, Imm>;

// All |X86Instruction|s and all its derived structs are owned
// by the procedure containing them.
struct X86Instruction {
    X86IK kind_{};
    ByteVec encoding_{};

    X86Instruction(X86IK kind) : kind_(kind) {}
    X86Instruction(X86IK kind, ByteVec encoding) : kind_(kind), encoding_(encoding) {}
    virtual ~X86Instruction() = default;

    // Create an instruction and bind it to the enclosing procedure.
    // The enclosing procedure is responsible for freeing the instructions
    // it contains.
    void* operator new(usz sz, ProcExpr* enclosing_proc);
    // Disallow creating expressions with no enclosing procedure.
    void* operator new(usz sz) = delete;

    // Human readable representation of the instruction's encoding.
    auto str_of_encoding() const -> Str;
};

struct Mov : public X86Instruction {
    X86Op dst_;
    X86Op src_;

    Mov(X86Op dst, X86Op src, ByteVec encoding) 
        : X86Instruction(X86IK::Mov, encoding), dst_(dst), src_(src) {}
};

struct Expr {
    ExprKind kind_{};

    Expr(ExprKind kind) : kind_(kind) {}
    virtual ~Expr() = default;

    // Create an expression and bind it to its parent module.
    void* operator new(usz sz, Module* mod, i1 is_top_level_expr);
    // Disallow creating expressions with no owner module.
    void* operator new(usz sz) = delete;
};

struct ProcExpr : public Expr {
    StrRef func_name_{};
    Vec<X86Instruction*> instructions_{};

    ProcExpr() : Expr(ExprKind::ProcExpr) {}
    ProcExpr(StrRef func_name, Vec<X86Instruction*> instructions)
        : Expr(ExprKind::ProcExpr), func_name_(func_name),
        instructions_(std::move(instructions)) {}
    ~ProcExpr();
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
    Str name_{};
    StringInterner strings_;
    TokStream tokens_;
    Vec<Expr*> ast_;
    Vec<Expr*> top_level_exprs_;
    Ctx* ctx_{};

    Module() {}
    ~Module();

    Module(const Module&) = delete;
    Module(Module&&) = delete;
    Module& operator=(const Module&) = delete;
    Module& operator=(Module&&) = delete;
};

struct Ctx {
    Vec<Box<File>> files_;
    Vec<Box<Module>> modules_;

    Ctx() {}

    Ctx(const Ctx&) = delete;
    Ctx(Ctx&&) = delete;
    Ctx& operator=(const Ctx&) = delete;
    Ctx& operator=(Ctx&&) = delete;

    auto get_file(u16 fid) -> File*; 
    auto load_file(const fs::path& path) -> u16; 

    //===============================================
    // Misc constatns needed for assembling.
    //===============================================
    static constexpr u8 k16_bit_prefix = 0x66;
    static constexpr u8 kmod_mem = 0b00;
    static constexpr u8 kmod_mem_disp8 = 0b01;
    static constexpr u8 kmod_mem_disp32 = 0b10;
    static constexpr u8 kmod_reg = 0b11;
    static constexpr u8 ksib_marker = 0b100;
    static constexpr u8 ksib_no_index_reg = 0b100;
    static constexpr u8 ksib_no_base_reg = 0b101;
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

    auto eof() -> i1 { return curr_ >= end_; }
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

// TODO(miloudi): The error reporting is simply bad.
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
        for (const char* ptr = beg; ptr < end; ++ptr) {
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
        fmt::print(bold | fg(light_gray), "Integer overflow encountered when converting: '{}'.", err.data_.overflowed_num_);
        break;
    }
    case ErrKind::IllegalValue: {
        fmt::print(bold | fg(light_gray), "Illegal value encountered: '{}'.", err.data_.illegal_lxm_);
        break;
    }
    case ErrKind::IllegalStmt: {
        fmt::print(bold | fg(light_gray), "Ill-formed statement encountered.");
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
    // TODO(miloudi): Underlining the problematic range is a complete mess when the range
    // contains multiple lines. Split the problematic range by lines. Print each line on its
    // own and underline it in the next one. Before you print each new line make sure to leave
    // space for the line number + vertical bar. 
    fmt::print(fg(red) | bold, "{}", problematic_range);
    print_ptr_range(error_pos + err.loc_.len_, info.line_end_);
    fmt::print("\n");

    // Print a '^' and a detailed error message below the highlighted char. 
    // The highlighted char is defined as the last char we lexed when the error happened.
    //
    // Skip the line number and the vertical dash.
    print_repeated(' ',/*side_bar_size=*/utils::number_width(info.line_) + std::strlen(" | "));

    print_ptr_range(info.line_start_, error_pos, ' ');

    fmt::print(fg(red) | bold, "^");
    // TODO(miloudi): This does not work well with problematic_ranges that span multiple lines.
    // We simply print len(problematic_range) '~' without taking into account the new lines which is
    // really bad.
    print_repeated('~', std::max<u32>(1, err.loc_.len_) - 1, bold | fg(red));

    fmt::print(fg(medium_slate_blue) | bold, " {}", fmt::format(fmt, std::forward<Args>(args)...));
    fmt::print("\n");

    std::exit(1);
}

struct Parser {
    Module* mod_{};
    u16 fid_{};
    TokStream::Iterator curr_tok_it_{};

    explicit Parser(File* file, Module* mod); 

    auto at(std::same_as<TK> auto... tok_tys) -> i1 {
        return ((tok().kind_ == tok_tys) or ...);
    }

    auto consume(std::same_as<TK> auto... tok_tys) -> i1 {
        if (not at(tok_tys...)) { return false; }
        next_tok();
        return true;
    }

    void expect(std::same_as<TK> auto... tok_kind) {
        // TODO: this logic messes up the error reporting. make sure you consume the tokens
        // that match and only pass the rest of the tokens to the error_handler.
        if ((consume(tok_kind) and ...)) { return; }
        expect_error_handler(tok_kind...);
    }
    void expect_either(std::same_as<TK> auto... tok_kind) {
        if ((consume(tok_kind) or ...)) { return; }
        expect_error_handler(tok_kind...);
    }

    [[noreturn]] void expect_error_handler(std::same_as<TK> auto... tok_kind) {
        Str token_spelling_list{};
        auto helper = [&](TK tok_kind) {
            token_spelling_list += fmt::format("'{}', ", Tok::spelling(tok_kind));
        };
        (helper(tok_kind), ...);

        // Remove the last " ," suffix from the list of token spellings.
        token_spelling_list.pop_back();
        token_spelling_list.pop_back();

        Error err {
            .kind_ = ErrKind::UnexpectedTok,
            .ctx_ = mod_->ctx_,
            .loc_ = tok().loc_,
            .data_ = { .tok_kind_ = tok().kind_ }
        };
        report_error(err, "was expecting the following tokens {} but found '{}' instead.",
                fmt::format("[ {} ]", token_spelling_list), Tok::spelling(err.data_.tok_kind_));
    }

    auto parse_proc_expr() -> Expr*;
    auto parse_x86_instruction(ProcExpr* enclosing_proc) -> X86Instruction*;
    auto parse_x86_operand() -> X86Op;

    template <OneOf<Reg, Imm, Mem, Moffs> Type>
    auto try_parse_x86_operand() -> Opt<Type>;

    auto match_next_toks(std::same_as<TK> auto... tok_tys) -> i1 {
        u32 lookahead = 0;
        auto match_tok = [&](TK tok_kind) {
            return peek_tok(lookahead++).kind_ == tok_kind;
        };
        
        return (match_tok(tok_tys) and ...);
    }

    auto tok() -> const Tok&;
    auto prev(u32 lookbehind = 0) -> const Tok&;
    auto peek_tok(u32 lookahead = 0) -> const Tok&;
    void next_tok();
    static void parse_file_into_module(File* file, Module* mod);
};

struct ModulePrinter {
    // Helper constants, types and functions.
    using UTF32Str = std::u32string;
    using enum fmt::color;
    using enum fmt::emphasis;
    static constexpr char32_t bleft_corn = U'\u2514';
    static constexpr char32_t bleft_corn_cont = U'\u251c';
    static constexpr char32_t dash = U'\u2500';
    static constexpr char32_t vline = U'\u2502';

    u32 indent_{};
    UTF32Str prev_line_prefix_{};
    UTF32Str out_{};

    auto line_prefix(i1 is_last) -> UTF32Str; 

    template <typename T>
    auto c(const T& value, const fmt::text_style& ts) const -> Str {
        return fmt::format(ts, "{}", value);
    }

    void print_module_helper(Module* mod);
    void print(Expr* expr, i1 is_last);
    void print(X86Instruction* instruction, i1 is_last);
    void print(const X86Op& op, i1 is_last);
    static void print_module(Module* mod);
};

struct ByteStream {
    ByteVec out_;

    ByteStream() {
        out_.reserve(15);
    }
    // No copies or moves allowed.
    ByteStream(const ByteStream&) = delete;
    ByteStream(ByteStream&&) = delete;
    ByteStream& operator=(const ByteStream&) = delete;
    ByteStream& operator=(ByteStream&&) = delete;
    
    auto append(BW bw, u64 qword) -> ByteStream&;
    auto append(ByteVec byte_vec) -> ByteStream&;

    template <typename T>
    auto append_if(const T& cond, BW width, u64 qword) -> ByteStream& {
        if (not cond) { return *this; }
        return append(width, qword);
    }
};


// Concept identifying x86 operand patterns.
template <typename T>
concept IsInstrPat = requires(T) {
    { T::match(Span<const X86Op>{}) } -> std::same_as<i1>;
};
//====================================================================
// Patterns of x86 operands. 
//====================================================================
enum struct Rex_W : i1 {
    Yes = true,
    No = false
};

enum struct B16OpSz : i1 {
    Yes = true,
    No = false
};

// TODO(miloudi): Use the cartesian product to get all the instances of a certain pattern.
// https://en.cppreference.com/w/cpp/ranges/cartesian_product_view
template <Rex_W rex_w, B16OpSz b16_opsz, IsX86OpClass... X86Ops>
struct Pat {
    static constexpr auto needs_rex_w(Span<const X86Op> ops) -> i1 { return +rex_w; }

    // TODO(miloudi): Remove the |is_b16_opsz| function. The name is bad.
    static constexpr auto is_b16_opsz(Span<const X86Op> ops) -> i1 { return +b16_opsz; }

    static constexpr auto needs_b16_opsz_override(Span<const X86Op> ops) -> i1 { return +b16_opsz; }

    static constexpr auto instances() -> Vec<Vec<X86Op>> {
        Vec<Vec<X86Op>> ret;

        using legacy_high_byte_regs = Any<
            r<BW::B8, RI::Rah>,
            r<BW::B8, RI::Rch>,
            r<BW::B8, RI::Rdh>,
            r<BW::B8, RI::Rbh>
                >;

        using new_low_byte_regs = Any<
            r<BW::B8, RI::Rsp>,
            r<BW::B8, RI::Rbp>,
            r<BW::B8, RI::Rsi>,
            r<BW::B8, RI::Rdi>
                >;

        using gprs_requiring_rex = Any<
            new_low_byte_regs,

            r<RI::R8>, r<RI::R9>, r<RI::R10>,
            r<RI::R11>, r<RI::R12>, r<RI::R13>,
            r<RI::R14>, r<RI::R15>
        >;

        using mem_refs_requiring_rex = Any<
            m<gprs_requiring_rex{}, any{}>,
            m<any{}, gprs_requiring_rex{}>
                >;

        using x86_op_requiring_rex = Any<gprs_requiring_rex, mem_refs_requiring_rex>;

        auto is_illegal = [](const Vec<X86Op>& op_list) {
            return rgs::any_of(op_list, legacy_high_byte_regs::match)
               and rgs::any_of(op_list, x86_op_requiring_rex::match);
        };

        auto to_vec = [](auto&&... args) {
            return {std::forward<decltype(args)>(args)...};
        };

        for (const auto& t : vws::cartesian_product(X86Ops::instances()...)) {
            Vec<X86Op> op_list = std::apply(to_vec, t);
            if (is_illegal(op_list)) { continue; }
            ret.push_back(std::move(op_list));
        }

        return ret;
    };

    static constexpr auto match(Span<const X86Op> ops) -> i1 {
        static_assert(sizeof...(X86Ops) < 255 and "op_idx overflow");
        using enum BW;
        using enum RI;

        static constexpr usz pat_sz = sizeof...(X86Ops);

        if (pat_sz != ops.size()) { return false; }

        u8 op_idx = 0;
        return (X86Ops::match(ops[op_idx++]) and ...);
    }
};

//====================================================================
// Or combinator of patterns.
//====================================================================
template <IsInstrPat... Pattern>
struct Or {
    static constexpr auto needs_rex_w(Span<const X86Op> ops) -> i1 {
        return ((Pattern::match(ops) and Pattern::needs_rex_w(ops)) or ...);
    }

    static constexpr auto needs_b16_opsz_override(Span<const X86Op> ops) -> i1 { 
        return is_b16_opsz(ops);
    }

    static constexpr auto is_b16_opsz(Span<const X86Op> ops) -> i1 {
        return ((Pattern::match(ops) and Pattern::is_b16_opsz(ops)) or ...);
    }

    static constexpr auto match(Span<const X86Op> ops) -> i1 {
        return (Pattern::match(ops) or ...);
    }

    static constexpr auto instances() -> Vec<Vec<X86Op>> {
        Vec<Vec<X86Op>> ret;

        auto helper = [&]<typename T>() {
            Vec<Vec<X86Op>> insts = T::instances();
            ret.insert(ret.end(), insts.begin(), insts.end()); 
        };

        (helper.template operator()<Pattern>(), ...);
        return ret;
    }
};

//====================================================================
// Emitters (Instruction Encoding formats.)
//====================================================================
template <OpEn encoding>
struct Emitter;

template <>
struct Emitter<OpEn::MR> {
    GCC_DIAG_IGNORE_PUSH(-Wconversion)
    static constexpr auto emit(ByteVec opcode, Span<const X86Op> ops, i1 rex_w, i1 b16_opsz) -> ByteVec {
        using enum BW;
                       
        assert(ops.size() == 2);
        assert((ops[0].is<Reg, Mem>() and ops[1].is<Reg>()));

        const X86Op& rm = ops[0];
        const X86Op& r = ops[1];

        ByteStream bs{};

        ModRM modrm {
            .rm = rm.modrm_encoding(),
            .reg = r.modrm_encoding(),
            .mod = u8(rm.modrm_mod() & r.modrm_mod()),
        };

        Rex rex {
            .b = rm.is<Reg>() and rm.as<Reg>().requires_ext(),
            .x = 0,
            .r = r.is<Reg>() and r.as<Reg>().requires_ext(),
            .w = rex_w
        };

        Opt<u8> sib{std::nullopt};
        Opt<i64> disp{std::nullopt};
        BW disp_sz{};

        if (rm.is<Mem>()) {
            const Mem& mem = rm.as<Mem>();

            sib = mem.sib();
            disp = mem.disp();

            disp_sz = utils::fits_in_b8(disp.value_or(0)) ? B8 : B32;

            switch (mem.kind()) {
            case MK::BaseDisp: {
                if (::is<RI::Rip>(mem.base_reg_->id_)) {
                    disp_sz = B32;
                    disp = disp.value_or(0);
                }
                break;
            }
            case MK::BaseIndexDisp:
                break;

            case MK::IndexDisp:
            case MK::DispOnly:
                disp_sz = B32;
                disp = disp.value_or(0);
                break;
            } // switch

            rex.b = mem.base_reg_ and mem.base_reg_->requires_ext();
            rex.x = mem.index_reg_ and mem.index_reg_->requires_ext();
        }

        bs.append_if(b16_opsz, B8, Ctx::k16_bit_prefix)
          .append_if(rex.is_required(ops), B8, rex.raw)
          .append(std::move(opcode))
          .append(B8, modrm.raw)
          .append_if(sib, B8, *sib)
          .append_if(disp, disp_sz, u64(*disp));

        return bs.out_;
    }
    GCC_DIAG_IGNORE_POP();
};

template <>
struct Emitter<OpEn::RM> {
    static constexpr auto emit(ByteVec opcode, Span<const X86Op> ops, i1 rex_w, i1 b16_opsz) -> ByteVec {
        assert(ops.size() == 2);
        assert((ops[0].is<Reg>() and ops[1].is<Reg, Mem>()));

        // Reverse the order of the operands and run the OpEn::MR routine.
        return Emitter<OpEn::MR>::emit(std::move(opcode), Vec<X86Op>{ops[1], ops[0]}, rex_w, b16_opsz);
    }
};

template <>
struct Emitter<OpEn::FD> {
    static constexpr auto emit(ByteVec opcode, Span<const X86Op> ops, i1 rex_w, i1 b16_opsz) -> ByteVec {
        using enum BW;

        assert(ops.size() == 2);
        assert((ops[0].is<Reg>() and ops[1].is<Moffs>()));

        ByteStream bs{};

        Rex rex {
            .w = rex_w
        };

        bs.append_if(b16_opsz, B8, Ctx::k16_bit_prefix)
          .append_if(rex.is_required(ops), B8, rex.raw)
          .append(std::move(opcode))
          .append(B64, ops[1].as<Moffs>().as<u64>());

        return bs.out_;
    }
};

template <>
struct Emitter<OpEn::TD> {
    static constexpr auto emit(ByteVec opcode, Span<const X86Op> ops, i1 rex_w, i1 b16_opsz) -> ByteVec {
        assert(ops.size() == 2);
        assert(ops[0].is<Moffs>() and ops[1].is<Reg>());

        return Emitter<OpEn::FD>::emit(std::move(opcode), Vec<X86Op>{ops[1], ops[0]}, rex_w, b16_opsz);
    }
};

template <>
struct Emitter<OpEn::OI> {
    static constexpr auto emit(ByteVec opcode, Span<const X86Op> ops, i1 rex_w, i1 b16_opsz) -> ByteVec {
        using enum BW; 

        assert(ops.size() == 2);
        assert((ops[0].is<Reg>() and ops[1].is<Imm>()));

        ByteStream bs{};

        Rex rex {
            .b = ops[0].as<Reg>().requires_ext(), 
            .w = rex_w
        };

        opcode.back() |= ops[0].as<Reg>().index();

        bs.append_if(b16_opsz, B8, Ctx::k16_bit_prefix)
          .append_if(rex.is_required(ops), B8, rex.raw)
          .append(std::move(opcode))
          .append(ops[1].bit_width(), ops[1].as<Imm>().as<u64>());

        return bs.out_;
    }
};

template <>
struct Emitter<OpEn::MI> {
    GCC_DIAG_IGNORE_PUSH(-Wconversion)
    static constexpr auto emit(ByteVec opcode, Span<const X86Op> ops, i1 rex_w, i1 b16_opsz) -> ByteVec {
        using enum BW;

        assert(ops.size() == 2);
        assert((ops[0].is<Reg, Mem>() and ops[1].is<Imm>()));

        ByteStream bs{};

        Opt<u8> sib{std::nullopt};
        Opt<i64> disp{std::nullopt};

        ModRM modrm {
            .rm = ops[0].modrm_encoding(),
            .mod = ops[0].modrm_mod()
        };

        Rex rex {
            .b = ops[0].is<Reg>() ? ops[0].as<Reg>().requires_ext() : i1(0),
            .w = rex_w
        };

        if (ops[0].is<Mem>()) {
            sib = ops[0].as<Mem>().sib();
            disp = ops[0].as<Mem>().disp_;
        }

        bs.append_if(b16_opsz, B8, Ctx::k16_bit_prefix)
          .append_if(rex.is_required(ops), B8, rex.raw)
          .append(std::move(opcode))
          .append(B8, modrm.raw)
          .append_if(sib, B8, *sib)
          .append_if(disp, utils::fits_in_b8(*disp) ? B8 : B32, u64(*disp))
          .append(ops[1].bit_width(), ops[1].as<Imm>().as<u64>());

        return bs.out_;
    }
    GCC_DIAG_IGNORE_POP();
};

// TODO(miloudi): Explain what this type does.
struct InstrExpr {
    using MatchFunction = i1(*)(Span<const X86Op>);
    using RexWFunction = i1(*)(Span<const X86Op>);
    using B16OpSzOverrideFunction = i1(*)(Span<const X86Op>);
    using EmitFunction = ByteVec(*)(ByteVec, Span<const X86Op>, i1 rex_w, i1 b16_opsz);

    MatchFunction match_{};
    EmitFunction emit_{};
    ByteVec opcode_{};
    RexWFunction rex_w_{};
    B16OpSzOverrideFunction b16_opsz_{};

    template <typename Matcher, typename Emitter>
    requires requires (ByteVec opcode, Span<const X86Op> ops, i1 rex_w, i1 b16_opsz) {
        { Matcher::match(ops) } -> std::same_as<i1>;
        { Emitter::emit(opcode, ops, rex_w, b16_opsz) } -> std::same_as<ByteVec>;
    }
    /*implicit*/ InstrExpr(ByteVec opcode, Matcher, Emitter) : 
            match_(Matcher::match), emit_(Emitter::emit),
            opcode_(opcode), rex_w_(Matcher::needs_rex_w), b16_opsz_(Matcher::is_b16_opsz)
    {}

    auto match(Span<const X86Op> ops) const -> i1 { return match_(ops); }
    auto emit(Span<const X86Op> ops) const -> ByteVec { return emit_(opcode_, ops, rex_w_(ops), b16_opsz_(ops)); }
};

template <u8... bs>
struct Op {
    static constexpr std::array bytes{bs...};
};

// TODO(miloudi): Remove the |_test| from the class name.
template <typename OpCode, typename Pattern, typename Emitter>
struct InstrExpr_test {
    using opcode = OpCode;
    using pattern = Pattern;
    using emitter = Emitter;
    using X86OpList = const Vec<X86Op>&;

    static constexpr auto match(X86OpList op_list) -> i1 {
        return Pattern::match(op_list);
    }

    // TODO(miloudi): Replace the ByteVec with a Buffer<>;
    static constexpr auto emit(X86OpList op_list) -> ByteVec {
        return Emitter::emit(
            ByteVec{OpCode::bytes.begin(), OpCode::bytes.end()},
            op_list,
            Pattern::needs_rex_w(op_list),
            Pattern::needs_b16_opsz_override(op_list)
        );
    }
};

template <typename... InstrExprs>
struct InstrExprList {
    using X86OpList = const Vec<X86Op>&;

    static constexpr auto is_valid_op_list(X86OpList op_list) -> i1 {
        return (InstrExprs::match(op_list) and ...);
    }
    
    static constexpr auto emit(X86OpList op_list) -> ByteVec {
        ByteVec ret;

        // TODO(miloudi): Remove the _test from the name.
        auto check_for_match = [&]<typename InstrExpr_test>() {
            // Return the first match to stay consistent with gas.
            if (not ret.empty()) { return; }

            if (InstrExpr_test::match(op_list)) {
                ret = InstrExpr_test::emit(op_list);
            }
        };

        (check_for_match.template operator()<InstrExprs>(), ...);

        return ret;
    }

    static constexpr auto instances() -> Vec<Vec<X86Op>> {
        Vec<Vec<X86Op>> ret;

        auto get_pat_instances = [&]<typename InstrExpr>() {
            auto v = InstrExpr::pattern::instances();
            ret.insert(ret.end(), v.begin(), v.end());
        };

        (get_pat_instances.template operator()<InstrExprs>(), ...);

        return ret;
    }
};

struct InstrHandler {

};

struct Assembler {
    // Global instruction table.
    static inline std::unordered_map<X86IK, Vec<InstrExpr>> git_;
    static inline HashMap<X86IK, InstrHandler> git;

    Assembler();
    // No copies or moves allowed.
    Assembler(const Assembler&) = delete;
    Assembler(Assembler&&) = delete;
    Assembler& operator=(const Assembler&) = delete;
    Assembler& operator=(Assembler&&) = delete;

    template <X86IK ik>
    static void register_instruction();

    template <X86IK ik>
    static auto encode(const Vec<X86Op>& ops) -> ByteVec;

    template <X86IK ik>
    static auto is_valid_instr(const Vec<X86Op>& ops) -> i1 {
        return not encode<ik>(ops).empty();
    }
};

}  // namespace fiska

#endif
