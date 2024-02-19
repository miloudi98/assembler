#ifndef __X86_ASSEMBLER_LIB_COMMON_X86_TYPES_HH__
#define __X86_ASSEMBLER_LIB_COMMON_X86_TYPES_HH__

#include "lib/common/base.hh"

namespace fiska::assembler {

// X86 Instruction mnemonic.
enum struct X86Mnemonic {
    Invalid,

    Mov, Add, Adc, Syscall, And
};

// Bit width.
enum struct BW : i8 {
    Invalid = -1,

    B8 = 8,
    B16 = 16,
    B24 = 24,
    B32 = 32,
    B64 = 64
};

// Register Id.
enum struct RI : u8 {
    Invalid,

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
enum struct RK : i8 {
    Invalid, 

    Ip,
    Gp,
    Seg,
    Ctrl,
    Dbg
};

// Memory reference kind.
enum struct MK {
    Invalid,

    BaseDisp,
    BaseIndexDisp,
    IndexDisp,
    DispOnly
};

// Operand size. Used to determine the instruction prefixes required for a given
// x86 instruction expression.
enum struct OpSz{
    Default,
    B16,
    B64
};

// Instruction operand encoding.
enum struct OpEn {
    MR,
    RM,
    FD,
    TD,
    OI,
    MI,
    I,
    ZO,
};

// Opcode type.
template <u8... byte>
requires (sizeof...(byte) <= 3)
struct OpCode {
    static constexpr auto value() -> u32 {
        constexpr std::array bytes = {byte...};

        switch (sizeof...(byte)) {
        case 1: return bytes[0];
        case 2: return bytes[0] | bytes[1] << 8;
        case 3: return bytes[0] | bytes[1] << 8 | bytes[2] << 16;
        default: unreachable();
        }
        unreachable();
    }
};
//=====================================================================================================================
// Opcode identifier concept.
//=====================================================================================================================
template <typename T> 
struct is_opcode_t : std::false_type {};

template <u8... byte> 
struct is_opcode_t<OpCode<byte...>> : std::true_type {};

template <typename T> concept is_opcode = is_opcode_t<T>::value;


//=====================================================================================================================
// [[intel]]
// /digit — A digit between 0 and 7 indicates that the ModR/M byte of the instruction uses only the r/m (register
// or memory) operand. The reg field contains the digit that provides an extension to the instruction's opcode
//=====================================================================================================================
enum struct SlashDigit : u8 {
    Zero = 0,
    One = 1,
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7
};

struct Rex {
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

    auto raw() const -> u8 {
        return (mod << 4) | (w << 3) | (r << 2) | (x << 1) | b;
    }
    auto has_set_bits() const -> i1 {
        return b or x or r or w;
    }
};

struct ModRM {
    u8 rm: 3{};
    u8 reg: 3{};
    u8 mod: 2{};

    auto raw() const -> u8 {
        return (mod << 6) | (reg << 3) | rm;
    }
};

struct Sib {
    u8 base: 3{};
    u8 index: 3{};
    u8 scale: 2{};

    auto raw() const -> u8 {
        return (scale << 6) | (index << 3) | base;
    }
};

struct X86Info {
    static constexpr u8 kModReg = 0b11;
    static constexpr u8 kMaxInstrLength = 15;
    static constexpr u8 k16bitOpSzOverridePrefix = 0x66;
    static constexpr u8 kSibMarker = 0b100;
    static constexpr u8 kModMem = 0b00;
    static constexpr u8 kModMemDisp8 = 0b01;
    static constexpr u8 kModMemDisp32 = 0b10;
    static constexpr u8 kNoIndexRegInSib = 0b100;
    static constexpr u8 kNoBaseRegInSib = 0b101;
    static const utils::StringMap<X86Mnemonic> kMnemonics;
    static const utils::StringMap<RI> kRegIds;
    static const utils::StringMap<BW> kBitWidths;

    static auto symbol_binding_and_type(u8 b, u8 t) -> u8 { return (b << 4) | (t & 0x0f); }
    static auto symbol_relocation_info(u64 sym_idx, u32 relocation_type) -> u64 { return (sym_idx << 32) | relocation_type; }
    static auto bit_width(StrRef bw) -> BW { return utils::strmap_get(kBitWidths, bw); }
    static auto register_id(StrRef ri) -> RI { return utils::strmap_get(kRegIds, ri); }
    static auto mnemonic(StrRef mmic) -> X86Mnemonic { return utils::strmap_get(kMnemonics, mmic); }
    static auto register_kind(RI ri) -> RK;
    static auto register_req_ext(RI ri) -> i1;
    static auto register_ndx(RI ri) -> u8;

};

}  // namespace fiska::assembler

// Support formatting mnemonics.
template <>
struct fmt::formatter<fiska::assembler::X86Mnemonic> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    template <typename FormatContext>
    auto format(const fiska::assembler::X86Mnemonic mmic, FormatContext& ctx) const -> decltype(ctx.out()) {
        for (const auto& [mmic_str, mmic_enum] : fiska::assembler::X86Info::kMnemonics) {
            if (mmic_enum == mmic) {
                return fmt::format_to(ctx.out(), "{}", mmic_str);
            }
        }
        return fmt::format_to(ctx.out(), "{{Unkown mnemonic}}");
    }
};

// Support formatting bit widths.
template <>
struct fmt::formatter<fiska::assembler::BW> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    template <typename FormatContext>
    auto format(const fiska::assembler::BW bw, FormatContext& ctx) const -> decltype(ctx.out()) {
        using enum fiska::assembler::BW;

        auto str_of_bw = [&] {
            switch (bw) {
            case Invalid: return "b{Invalid}";
            case B8: return "b8";
            case B16: return "b16";
            case B24: return "b24";
            case B32: return "b32";
            case B64: return "b64";
            } // switch
            unreachable();
        }();
        return fmt::format_to(ctx.out(), "{}", str_of_bw);
    }
};

// Support formatting register ids.
template <>
struct fmt::formatter<fiska::assembler::RI> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    template <typename FormatContext>
    auto format(const fiska::assembler::RI ri, FormatContext& ctx) const -> decltype(ctx.out()) {
        for (const auto& [ri_str, ri_enum] : fiska::assembler::X86Info::kRegIds) {
            if (ri_enum == ri) {
                return fmt::format_to(ctx.out(), "{}", ri_str);
            }
        }
        return fmt::format_to(ctx.out(), "{{Unkown rid}}");
    }
};
#endif // __X86_ASSEMBLER_LIB_COMMON_X86_TYPES_HH__
