#ifndef __X86_ASSEMBLER_LIB_CODEGEN_IR_HH__
#define __X86_ASSEMBLER_LIB_CODEGEN_IR_HH__

#include "lib/support/core.hh"
#include "lib/x86/common.hh"

namespace fiska::x86::codegen {


//struct IRX86Instr {
//    using Ref = const IRX86Instr&;
//
//    X86Mnemonic mmic_ = X86Mnemonic::Invalid;
//    IRX86Op::List ops_;
//    mutable u64 sct_offset_{};
//};
//
//struct IRProc {
//    StrRef name_;
//    Vec<IRX86Instr> body_;
//};
//
//struct IRVar {
//    StrRef name_;
//    ByteVec data_;
//};
//
//struct IRSymbol {
//    using Inner = std::variant<
//        std::monostate,
//        IRProc,
//        IRVar
//    >;
//
//    enum struct Kind {
//        Invalid,
//        Proc,
//        Var,
//    };
//
//    Inner inner_{};
//    StrRef section_;
//    StrRef name_;
//    Kind kind_;
//
//    [[nodiscard]] auto as_proc() -> IRProc& { return std::get<IRProc>(inner_); }
//    [[nodiscard]] auto as_var() -> IRVar& { return std::get<IRVar>(inner_); }
//};

//struct IRBuilder {
//    //using IRObject = std::variant<IRProc, IRVar>;
//    using IRObject = IRProc;
//
//    Vec<IRObject> ir_objects_;
//    Vec<IRSymbol> ir_syms_;
//    // TODO: You don't need this at all.
//    utils::StringMap<Str> sym_sct_info_;
//
//    auto lower_x86_instr_expr(fe::X86InstrExpr* x86_instr_expr) -> IRX86Instr;
//    auto lower_expr(fe::Expr* expr) -> IRX86Op;
//    auto link_sym_name_to_section(StrRef sym_name, StrRef sct_name) -> void;
//    auto build(fe::Expr::ListRef ast_);
//};

} // namespace fiska::x86::codegen

#endif // __X86_ASSEMBLER_LIB_CODEGEN_IR_HH__
