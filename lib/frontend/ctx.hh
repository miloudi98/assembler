#ifndef __X86_ASSEMBLER_LIB_COMMON_CTX_HH__
#define __X86_ASSEMBLER_LIB_COMMON_CTX_HH__

#include "lib/common/base.hh"
#include "lib/common/support.hh"

namespace fiska::assembler::frontend {

struct Expr;

struct Ctx {
    NOT_COPYABLE_NOT_MOVABLE(Ctx);

    // Files loaded from disk.
    Vec<Box<File>> files_;
    // Interned string pool.
    StringInterner string_pool_;
    // All AST nodes allocated.
    Vec<Expr*> exprs_;

    Ctx() = default;
    // Destroy all the AST nodes allocated.
    ~Ctx();
    // Load file to memory.
    auto load_file(const fs::path& path) -> File*;
    // Return the file with fid |fid|.
    auto read_file(u16 fid) -> File*;
};

// Error Handling types and utility functions.
struct UnexpectedToken {
    static constexpr StrRef fmt_string = "Unexpected token: '{}', found: '{}'.";
};

struct InvalidNumber {
    static constexpr StrRef fmt_string = "Invalid number encountered: '{}'.";
};

struct InvalidStartOfExpr {
    static constexpr StrRef fmt_string = "Token '{}' does not start a valid expression.";
};

struct InvalidX86Op {
    static constexpr StrRef fmt_string = "Token '{}' does not start a valid x86 operand.";
};

struct ExpectedComma {
    static constexpr StrRef fmt_string = "Expected ','.";
};

struct InvalidX86Mnemonic {
    static constexpr StrRef fmt_string = "Unrecognized x86 mnemonic: '{}'.";
};

struct IllegalSymbolStart {
    static constexpr StrRef fmt_string = "Token '{}' does not start a valid symbol.";
};

struct IntegerAdditionOverflow {
    static constexpr StrRef fmt_string = "64-bit Integer overflow encountered.";
};

struct MultipleLabelsInsideExpression {
    static constexpr StrRef fmt_string = "Multiple labels are not allowed inside a single expression.";
};

struct IllegalOffsetExpression {
    static constexpr StrRef fmt_string = "Illegal offset expression found: '{}'.";
};

struct IllegalSubstractionOnLabel {
    static constexpr StrRef fmt_string = "A label can not appear on the right hand side of a substraction.";
};

struct IllegalBinaryOperator {
    static constexpr StrRef fmt_string = "Illegal binary operator: '{}'.";
};

struct IllegalUnaryOperator {
    static constexpr StrRef fmt_string = "Illegal unary operator: '{}'.";
};

struct LabelFoundInMemScale {
    static constexpr StrRef fmt_string = "Labels are not allowed in memory index scales.";
};

struct IllegalMemScaleValue {
    static constexpr StrRef fmt_string = "Illegal index scale value found: '{}'.";
};

struct MemDispTooLarge {
    static constexpr StrRef fmt_string = "Memory displacement: '{}' does not fit in 32-bits.";
};

template <typename ErrorKind>
struct Diag {
    template <typename... Args>
    [[noreturn]] Diag(Ctx* ctx, Span span, Args&&... args) {
        unreachable(ErrorKind::fmt_string, FWD(args)...);
    }
};


} // namespace fiska::assembler

#endif // __X86_ASSEMBLER_LIB_COMMON_CTX_HH__
