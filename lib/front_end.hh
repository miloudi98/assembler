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
    i1 new_8_bit_reg_{false};

    auto index() const -> u8 { return +id_ & 0x7; }
    auto requires_ext() const -> bool {
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
    constexpr bool is() const { return (std::holds_alternative<Ts>(inner_) or ...); }

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
concept IsX86OpClass = requires(X86Op op) {
    { T::match(op) } -> std::same_as<bool>;
};
//=====================================================
// Register classes.
//=====================================================
template <auto... args>
struct r {
    static constexpr auto match(const X86Op&) -> i1 { return false; }
};

template <BW bit_width>
struct r<bit_width> {
    static constexpr auto match(const X86Op& op) -> bool {
        return op.is<Reg>() 
            and op.as<Reg>().bit_width_ == bit_width
            and op.as<Reg>().kind() == RK::Gp;
    }

    static constexpr auto instances() -> Vec<X86Op> {
        using enum RI;

        Vec<X86Op> ret;

        for (u32 ri = +Rax; ri <= +R15; ++ri) {
            ret.push_back({ Reg{bit_width, RI(ri)} });
        }

        // Add |Rip| if the register is not an 8-bit register.
        if (bit_width != BW::B8) {
            ret.push_back({ Reg{bit_width, Rip} });
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

// FIXME(miloudi): Non GP registers usually have a fixed size. You don't really
// need to specify their bitwidth.
template <BW bit_width, RK kind>
struct r<bit_width, kind> {
    static constexpr auto match(const X86Op& op) -> bool {
        return op.is<Reg>()
            and op.as<Reg>().bit_width_ == bit_width
            and op.as<Reg>().kind() == kind;
    }

    static constexpr auto instances() -> Vec<X86Op> {
        using enum RI;

        // Use the r<BW> specialization if you want a GP register class.
        assert(kind != RK::Gp);

        Vec<X86Op> ret;

        auto [start_ri, end_ri] = [&] {
            switch (kind) {
            case RK::Seg: return std::make_pair(+Es, +Gs);
            case RK::Ctrl: return std::make_pair(+Cr0, +Cr15);
            case RK::Dbg: return std::make_pair(+Dbg0, +Dbg15);
            case RK::Gp: unreachable("Use the r<BW> specialization if you want a GP register class.");
            } // switch
            unreachable();
        }();

        for (u32 ri = start_ri; ri <= end_ri; ++ri) {
            ret.push_back({ Reg{bit_width, RI(ri)} });
        }

        return ret;
    }
};

template <BW bit_width, RI id>
struct r<bit_width, id> {
    static constexpr auto match(const X86Op& op) -> bool {
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
    static constexpr auto match(const X86Op& op) -> bool {
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
template <r base_reg_class>
struct m<base_reg_class> {
    static constexpr auto match(const X86Op& op) -> i1 {
        return op.is<Mem>()
            and op.as<Mem>().base_reg_
            and base_reg_class.match({op.as<Mem>().base_reg_.value()});
    }
};

//=====================================================
// Immediate classes.
//=====================================================
template <auto... args>
struct i;

template <BW bit_width>
struct i<bit_width> {
    static constexpr auto match(const X86Op& op) -> bool {
        return op.is<Imm>() 
            and op.as<Imm>().bit_width_ == bit_width;
    }

    static constexpr auto instances(i64 n_inst = 10) -> Vec<X86Op> {
        Vec<X86Op> ret;
        // Generate random numbers that fit in |bit_width| bits.
        i64 mn = -(1 << (+bit_width - 1));
        i64 mx = (1 << (+bit_width - 1)) - 1;

        std::mt19937 gen(std::random_device{}());
        std::uniform_int_distribution<> imms(mn, mx);
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
    static constexpr auto match(const X86Op& op) -> bool {
        return op.is<Moffs>() 
            and op.as<Moffs>().bit_width_ == bit_width;
    }

    static constexpr auto instances(i64 n_inst = 10) -> Vec<X86Op> {
        Vec<X86Op> ret;

        std::mt19937 gen(std::random_device{}());
        std::uniform_int_distribution<> moffsets(0, std::numeric_limits<u64>::max());
        while (n_inst--) { ret.push_back({ Moffs{bit_width, moffsets(gen)} }); }
        return ret;
    }
};

//====================================================================
// X86OpClass Any combinator. 
// This combinator will be satisfied if the X86Op passed
// to it as an argument satisfies any of the X86OpClasses passed as 
// template arguments.
//====================================================================
template <IsX86OpClass... X86OpClass>
struct Any {
    static constexpr auto match(const X86Op& op) -> bool {
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

    // TODO(miloudi): Make this function take in the list of operands and 
    // check whether we have any of the new 8-bit registers. If we do, then we need
    // to output the REX prefix with the mod alone, meaning we need to return true.
    auto is_required(Span<const X86Op> ops) const -> bool {
        using enum BW;
        using enum RI;
        using new_low_byte_regs = Any< 
            ::fiska::r<B8, Rsp>, 
            ::fiska::r<B8, Rbp>,
            ::fiska::r<B8, Rsi>,
            ::fiska::r<B8, Rdi>
        >;

        if (w or r or x or b) { return true; }

        return rgs::any_of(ops, new_low_byte_regs::match);
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
    void* operator new(usz sz, Module* mod, bool is_top_level_expr);
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

    auto at(std::same_as<TK> auto... tok_tys) -> bool {
        return ((tok().kind_ == tok_tys) or ...);
    }

    auto consume(std::same_as<TK> auto... tok_tys) -> bool {
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

    auto match_next_toks(std::same_as<TK> auto... tok_tys) -> bool {
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

    auto line_prefix(bool is_last) -> UTF32Str; 

    template <typename T>
    auto c(const T& value, const fmt::text_style& ts) const -> Str {
        return fmt::format(ts, "{}", value);
    }

    void print_module_helper(Module* mod);
    void print(Expr* expr, bool is_last);
    void print(X86Instruction* instruction, bool is_last);
    void print(const X86Op& op, bool is_last);
    static void print_module(Module* mod);
};

struct ByteStream {
    ByteVec out_{};

    ByteStream() {}
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
    { T::match(Span<const X86Op>{}) } -> std::same_as<bool>;
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

    static constexpr auto is_b16_opsz(Span<const X86Op> ops) -> i1 { return +b16_opsz; }

    static constexpr auto match(Span<const X86Op> ops) -> bool {
        using enum BW;
        using enum RI;

        static constexpr usz pat_sz = sizeof...(X86Ops);
        static_assert(pat_sz < 255 and "op_idx overflow");

        if (pat_sz != ops.size()) { return false; }

        u8 op_idx = 0;
        if (not (X86Ops::match(ops[op_idx++]) and ...)) { return false; }

        using rex_byte_regs = Any<
            r<B8, RI::Rsp>,
            r<B8, RI::Rbp>,
            r<B8, RI::Rsi>,
            r<B8, RI::Rdi>
        >;
        using high_byte_regs = Any<
            r<B8, RI::Rah>,
            r<B8, RI::Rch>,
            r<B8, RI::Rdh>,
            r<B8, RI::Rbh>
        >;
        i1 pat_has_high_byte_regs = rgs::any_of(ops, high_byte_regs::match);

        i1 has_rex = +rex_w;
        for (const X86Op& op : ops) {
            has_rex |= (op.is<Reg>() and op.as<Reg>().requires_ext());
            has_rex |= rex_byte_regs::match(op);

            if (op.is<Mem>()) {
                const Mem& mem = op.as<Mem>();

                has_rex |= mem.base_reg_ and mem.base_reg_->requires_ext();
                has_rex |= mem.index_reg_ and mem.index_reg_->requires_ext();
                // TODO(miloudi): you can create a mem blueprint that matches all memory 
                // references that require an extended reg.
                // using extended_reg = Any<r8, r9, ....>;
                // it's that simple.
                // using extended_mem = m<extended_reg{}, extended_reg{}>;
                // has_rex |= m<r<>{}, r<R8>{}>::match(op);
            }
        }

        return not (has_rex and pat_has_high_byte_regs);
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

    static constexpr auto is_b16_opsz(Span<const X86Op> ops) -> i1 {
        return ((Pattern::match(ops) and Pattern::is_b16_opsz(ops)) or ...);
    }

    static constexpr auto match(Span<const X86Op> ops) -> bool {
        return (Pattern::match(ops) or ...);
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
            .b = ops[0].is<Reg>() ? ops[0].as<Reg>().requires_ext() : bool(0),
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
    using MatchFunction = bool(*)(Span<const X86Op>);
    using RexWFunction = bool(*)(Span<const X86Op>);
    using B16OpSzOverrideFunction = bool(*)(Span<const X86Op>);
    using EmitFunction = ByteVec(*)(ByteVec, Span<const X86Op>, i1 rex_w, i1 b16_opsz);

    MatchFunction match_{};
    EmitFunction emit_{};
    ByteVec opcode_{};
    RexWFunction rex_w_{};
    B16OpSzOverrideFunction b16_opsz_{};

    template <typename Matcher, typename Emitter>
    requires requires (ByteVec opcode, Span<const X86Op> ops, i1 rex_w, i1 b16_opsz) {
        { Matcher::match(ops) } -> std::same_as<bool>;
        { Emitter::emit(opcode, ops, rex_w, b16_opsz) } -> std::same_as<ByteVec>;
    }
    /*implicit*/ InstrExpr(ByteVec opcode, Matcher, Emitter) : 
            match_(Matcher::match), emit_(Emitter::emit),
            opcode_(opcode), rex_w_(Matcher::needs_rex_w), b16_opsz_(Matcher::is_b16_opsz)
    {}

    auto match(Span<const X86Op> ops) const -> bool { return match_(ops); }
    auto emit(Span<const X86Op> ops) const -> ByteVec { return emit_(opcode_, ops, rex_w_(ops), b16_opsz_(ops)); }
};

struct Assembler {
    // Global instruction table.
    static inline std::unordered_map<X86IK, Vec<InstrExpr>> git_;

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
    static auto is_valid_instr(const Vec<X86Op>& ops) -> bool {
        return not encode<ik>(ops).empty();
    }
};

}  // namespace fiska

#endif
