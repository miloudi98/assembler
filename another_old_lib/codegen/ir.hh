#ifndef __X86_ASSEMBLER_LIB_CODEGEN_IR_HH__
#define __X86_ASSEMBLER_LIB_CODEGEN_IR_HH__

#include "lib/core.hh"
#include "lib/front_end/parser.hh"
#include "lib/front_end/support.hh"

namespace fiska::x86::ir {

// Rewrite everything.
// define the operator [] using an enum containing the fields of the REX prefix. This is just an idea that occured to me.
// Get the instruction kind to figure out what relocation must be done.
// include the relocation type with the X86Operand.
// Each IR_X86Operand needs to have PatchInfo struct that is going to contain information about the offset of the relocation,
// the relocation type and the name of the symbol with respect to which the relocation is happening.

struct IR_Proc {
    fe::ProcExpr* ast_node_ = nullptr;
    X86Instruction::List body_;
};

struct IRBuilder {
    fe::Expr::List ast_;
    fe::Ctx* ctx_{};

    // Perform constant folding on the AST.
    auto constant_fold() -> void;
    auto constant_fold_helper(fe::Expr* node) -> fe::Expr*;

    auto lower_x86_op_expr(fe::X86OpExpr* x86_op_expr) -> X86Op;
    auto lower_x86_instruction(fe::X86InstructionExpr* x86_instr_expr) -> X86Instruction;
    auto lower_ast() -> Vec<IR_Proc>;
};

} // namespace fiska::x86::ir

#endif // __X86_ASSEMBLER_LIB_CODEGEN_IR_HH__
