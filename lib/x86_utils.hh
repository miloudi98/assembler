#ifndef __X86_ASSEMBLER_LIB_X86_UTILS_HH__
#define __X86_ASSEMBLER_LIB_X86_UTILS_HH__

#include "lib/core.hh"
#include <random>

namespace fiska::x86 {
// TODO(miloudi): Make fmt able to print the enums defined in this file.

// X86 Instruction Kind.
enum struct X86IK {
    Mov
};

// Bit width.
enum struct BW : u16 {
    B0 = 0,
    B8 = 8,
    B16 = 16,
    B24 = 24,
    B32 = 32,
    B64 = 64
};

// Register Id.
enum struct RI : u16 {
    Rax,  Rcx,  Rdx,  
    Rbx,  Rsp,  Rbp,  
    Rsi,  Rdi,  R8,    
    R9,   R10,  R11,  
    R12,  R13,  R14,  
    R15,
    // 8-bit Rsp
    Rah,
    // 8-bit Rbp
    Rch,
    // 8-bit Rsi
    Rdh,
    // 8-bit Rdi
    Rbh,

    Rip,

    Es, Cs, Ss,
    Ds, Fs, Gs,

    Cr0, Cr1, Cr2,
    Cr3, Cr4, Cr5,
    Cr6, Cr7, Cr8,
    Cr9, Cr10, Cr11,
    Cr12, Cr13, Cr14,
    Cr15,

    Dbg0, Dbg1, Dbg2,
    Dbg3, Dbg4, Dbg5,
    Dbg6, Dbg7, Dbg8,
    Dbg9, Dbg10, Dbg11,
    Dbg12, Dbg13, Dbg14,
    Dbg15
};

// Register kind.
enum struct RK {
    Ip,
    Gp,
    Seg,
    Ctrl,
    Dbg
};

// Returns the register kind of a register id.
auto rk_of_ri(RI ri) -> RK;

// Memory reference kind.
enum struct MK {
    BaseDisp,
    BaseIndexDisp,
    IndexDisp,
    DispOnly
};

// Memory reference index scale.
enum struct MemIndexScale : u8 {
    One = 0,
    Two = 1,
    Four = 2,
    Eight = 3
};

// Instruction operand encoding.
enum struct OpEn {
    MR,
    RM,
    FD,
    TD,
    OI,
    MI
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
        // Reserved.
        u8 mod: 4 {0b0100}; 
        // Must emit the prefix.
        i1 required: 1{};
    };
    u16 raw;
};
static_assert(sizeof(Rex) == 2);


struct Moffs {
    BW bw_{};
    i64 addr_{};
};

struct Imm {
    BW bw_{};
    i64 value_{};
};

struct Reg {
    BW bw_{};
    RI id_{};

    [[nodiscard]] auto kind() const -> RK;
    [[nodiscard]] auto index() const -> u8;
    [[nodiscard]] auto requires_ext() const -> i1;
};

struct Mem {
    BW bw_{};
    Opt<Reg> base_reg_{};
    Opt<Reg> index_reg_{};
    Opt<MemIndexScale> scale_{};
    i64 disp_{};
    BW disp_bw_{};
    MK kind_{};
    u8 mod_{};
    Opt<u8> sib_{};

    static constexpr u8 kmod_reg = 0b11;
    static constexpr u8 kmod_mem = 0b00;
    static constexpr u8 kmod_mem_disp8 = 0b01;
    static constexpr u8 kmod_mem_disp32 = 0b10;
    static constexpr u8 ksib_marker = 0b100;
    static constexpr u8 ksib_no_index_reg = 0b100;
    static constexpr u8 ksib_no_base_reg = 0b101;

public:
    // Supress warnings from bit-fields narrowings. We are not aware of 
    // any way to supress them other than removing the -Wconversion flag
    // from the code in question.
    GCC_DIAG_IGNORE_PUSH(-Wconversion)
    // Factory function. All 'Mem's are creating through it.
    // We need this mechanism to make sure all members are correctly initialized.
    template <typename... Args>
    static auto make(Args&&... args) -> Mem {
        using enum BW;
        using enum RI;

        Mem mem{std::forward<Args>(args)...};

        // set the kind.
        if (mem.base_reg_ and mem.index_reg_) {
            mem.kind_ = MK::BaseIndexDisp;
        } else if (not mem.base_reg_ and mem.index_reg_) {
            mem.kind_ = MK::IndexDisp;
        } else if (mem.base_reg_ and not mem.index_reg_) {
            mem.kind_ = MK::BaseDisp;
        } else {
            mem.kind_ = MK::DispOnly;
        }

        // set the displacement bit width.
        if (mem.base_reg_ and mem.base_reg_->id_ == Rip) {
            mem.disp_bw_ = B32;
        }
        
        if (::is<MK::IndexDisp, MK::DispOnly>(mem.kind_)) {
            mem.disp_bw_ = B32;
        }

        if (mem.base_reg_ and ::is<Rbp, R13>(mem.base_reg_->id_)) {
            mem.disp_bw_ = utils::fits_in_b8(mem.disp_) ? B8 : B32;
        }

        if (mem.disp_ and mem.disp_bw_ == B0) {
            mem.disp_bw_ = utils::fits_in_b8(mem.disp_) ? B8 : B32;
        }

        // set the mod.
        if (::is<MK::DispOnly, MK::IndexDisp>(mem.kind_) 
                or mem.disp_bw_ == B0 
                or (mem.base_reg_ and mem.base_reg_->id_ == Rip)) {
            mem.mod_ = kmod_mem;
        } else {
            mem.mod_ = utils::fits_in_b8(mem.disp_) ? kmod_mem_disp8 : kmod_mem_disp32;
        }

        // set the sib.
        // This is the only case where we don't need a sib byte.
        if (not (mem.kind_ == MK::BaseDisp and not ::is<Rsp, R12>(mem.base_reg_->id_))) {
            Sib sib{};
            switch (mem.kind_) {
            case MK::BaseDisp: {
                sib.base = mem.base_reg_->index();
                sib.index = ksib_no_index_reg;
                break;
            }
            case MK::BaseIndexDisp: {
                sib.base = mem.base_reg_->index();
                sib.index = mem.index_reg_->index();
                sib.scale = +mem.scale_.value();
                break;
            }
            case MK::IndexDisp: {
                sib.base = ksib_no_base_reg;
                sib.index = mem.index_reg_->index();
                sib.scale = +mem.scale_.value();
                break;
            }
            case MK::DispOnly: {
                sib.base = ksib_no_base_reg;
                sib.index = ksib_no_index_reg;
                break;
            }
            } // switch

            mem.sib_ = sib.raw;
        }

        return mem;
    }
    GCC_DIAG_IGNORE_POP();
};

struct X86Op {
    using Ref = const X86Op&;
    using List = Vec<X86Op>;
    using ListRef = const Vec<X86Op>&;
    using Inner = std::variant<
        std::monostate,
        Reg,
        Mem,
        Moffs,
        Imm
    >;
    Inner inner_{};


    template <OneOf<Reg, Mem, Moffs, Imm>... Ts>
    [[nodiscard]] auto is() const -> i1 { return (std::holds_alternative<Ts>(inner_) or ...); }

    template <OneOf<Reg, Mem, Moffs, Imm> T>
    [[nodiscard]] auto as() -> T& { return std::get<T>(inner_); }

    template <OneOf<Reg, Mem, Moffs, Imm> T>
    [[nodiscard]] auto as() const -> const T& { return std::get<T>(inner_); }

    [[nodiscard]] auto modrm_encoding() const -> u8;
};

struct X86Instruction {
    using Ref = const X86Instruction&;
    using List = Vec<X86Instruction>;
    using ListRef = const Vec<X86Instruction>&;

    X86IK kind_{};
    X86Op::List op_list{};
};

// Assembler context.
struct AsCtx {
    // Must be kept in sync.
    static constexpr u8 knum_regs = +RI::Dbg15 + 1;
    static inline Vec<Vec<BW>> kbw_of_ri;
    static inline std::mt19937 kgen{std::random_device{}()};
    static inline std::uniform_int_distribution<i8>  k8bit_rand_int{std::numeric_limits<i8>::min(), std::numeric_limits<i8>::max()}; 
    static inline std::uniform_int_distribution<i16> k16bit_rand_int{std::numeric_limits<i16>::min(), std::numeric_limits<i16>::max()}; 
    static inline std::uniform_int_distribution<i32> k32bit_rand_int{std::numeric_limits<i32>::min(), std::numeric_limits<i32>::max()};
    static inline std::uniform_int_distribution<i64> k64bit_rand_int{std::numeric_limits<i64>::min(), std::numeric_limits<i64>::max()};

    AsCtx() = delete;
    AsCtx(const AsCtx&) = delete;
    AsCtx(AsCtx&&) = delete;
    AsCtx& operator=(const AsCtx&) = delete;
    AsCtx& operator=(AsCtx&&) = delete;

    static void init();
};

}  // namespace fiska::x86

#endif // __X86_ASSEMBLER_LIB_X86_UTILS_HH__
