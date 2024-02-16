#include "lib/x86_utils.hh"

namespace {

auto build_bw_of_ri_map() -> Vec<Vec<fiska::x86::BW>> {
    using fiska::x86::RI;
    using fiska::x86::BW;
    using fiska::x86::AsCtx;

    Vec<Vec<fiska::x86::BW>> bws_of_ri(AsCtx::knum_regs);

    bws_of_ri[+RI::Rax] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::Rcx] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::Rdx] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::Rbx] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::Rsp] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::Rbp] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::Rsi] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::Rdi] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::R8]  = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::R9]  = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::R10] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::R11] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::R12] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::R13] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::R14] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::R15] = {BW::B8, BW::B16, BW::B32, BW::B64};
    bws_of_ri[+RI::Rah] = {BW::B8};
    bws_of_ri[+RI::Rch] = {BW::B8};
    bws_of_ri[+RI::Rdh] = {BW::B8};
    bws_of_ri[+RI::Rbh] = {BW::B8};

    bws_of_ri[+RI::Rip] = {BW::B16, BW::B32, BW::B64};

    bws_of_ri[+RI::Es] = {BW::B16};
    bws_of_ri[+RI::Cs] = {BW::B16};
    bws_of_ri[+RI::Ss] = {BW::B16};
    bws_of_ri[+RI::Ds] = {BW::B16};
    bws_of_ri[+RI::Fs] = {BW::B16};
    bws_of_ri[+RI::Gs] = {BW::B16};

    bws_of_ri[+RI::Cr0] = {BW::B64};
    bws_of_ri[+RI::Cr1] = {BW::B64};
    bws_of_ri[+RI::Cr2] = {BW::B64};
    bws_of_ri[+RI::Cr3] = {BW::B64};
    bws_of_ri[+RI::Cr4] = {BW::B64};
    bws_of_ri[+RI::Cr5] = {BW::B64};
    bws_of_ri[+RI::Cr6] = {BW::B64};
    bws_of_ri[+RI::Cr7] = {BW::B64};
    bws_of_ri[+RI::Cr8] = {BW::B64};
    bws_of_ri[+RI::Cr9] = {BW::B64};
    bws_of_ri[+RI::Cr10] = {BW::B64};
    bws_of_ri[+RI::Cr11] = {BW::B64};
    bws_of_ri[+RI::Cr12] = {BW::B64};
    bws_of_ri[+RI::Cr13] = {BW::B64};
    bws_of_ri[+RI::Cr14] = {BW::B64};
    bws_of_ri[+RI::Cr15] = {BW::B64};

    bws_of_ri[+RI::Dbg0] = {BW::B64};
    bws_of_ri[+RI::Dbg1] = {BW::B64};
    bws_of_ri[+RI::Dbg2] = {BW::B64};
    bws_of_ri[+RI::Dbg3] = {BW::B64};
    bws_of_ri[+RI::Dbg4] = {BW::B64};
    bws_of_ri[+RI::Dbg5] = {BW::B64};
    bws_of_ri[+RI::Dbg6] = {BW::B64};
    bws_of_ri[+RI::Dbg7] = {BW::B64};
    bws_of_ri[+RI::Dbg8] = {BW::B64};
    bws_of_ri[+RI::Dbg9] = {BW::B64};
    bws_of_ri[+RI::Dbg10] = {BW::B64};
    bws_of_ri[+RI::Dbg11] = {BW::B64};
    bws_of_ri[+RI::Dbg12] = {BW::B64};
    bws_of_ri[+RI::Dbg13] = {BW::B64};
    bws_of_ri[+RI::Dbg14] = {BW::B64};
    bws_of_ri[+RI::Dbg15] = {BW::B64};

    return bws_of_ri;
}

} // namespace

auto fiska::x86::rk_of_ri(RI ri) -> RK {
    using enum RI;

    switch (ri) {
    case Invalid: unreachable();

    case Rax:
    case Rcx:
    case Rdx:
    case Rbx:
    case Rsp:
    case Rbp:
    case Rsi:
    case Rdi:
    case R8:
    case R9:
    case R10:
    case R11:
    case R12:
    case R13:
    case R14:
    case R15:
    case Rah:
    case Rch:
    case Rdh:
    case Rbh:
        return RK::Gp;

    case Rip:
        return RK::Ip;

    case Es:
    case Cs:
    case Ss:
    case Ds:
    case Fs:
    case Gs:
        return RK::Seg;

    case Cr0:
    case Cr1:
    case Cr2:
    case Cr3:
    case Cr4:
    case Cr5:
    case Cr6:
    case Cr7:
    case Cr8:
    case Cr9:
    case Cr10:
    case Cr11:
    case Cr12:
    case Cr13:
    case Cr14:
    case Cr15:
        return RK::Ctrl;

    case Dbg0:
    case Dbg1:
    case Dbg2:
    case Dbg3:
    case Dbg4:
    case Dbg5:
    case Dbg6:
    case Dbg7:
    case Dbg8:
    case Dbg9:
    case Dbg10:
    case Dbg11:
    case Dbg12:
    case Dbg13:
    case Dbg14:
    case Dbg15:
        return RK::Dbg;
    } // switch
    unreachable();
}

auto fiska::x86::idx_of_ri(RI ri) -> u8 {
    using enum RI;

    switch (ri) {
    case Invalid: unreachable();

    case Rax: return 0;
    case Rcx: return 1;
    case Rdx: return 2;
    case Rbx: return 3;
    case Rsp: return 4;
    case Rbp: return 5;
    case Rsi: return 6;
    case Rdi: return 7;
    case R8:  return 0;
    case R9:  return 1;
    case R10: return 2;
    case R11: return 3;
    case R12: return 4;
    case R13: return 5;
    case R14: return 6;
    case R15: return 7;
    case Rah: return 4;
    case Rch: return 5;
    case Rdh: return 6;
    case Rbh: return 7;

    case Rip: return 5;

    case Es: return 0;
    case Cs: return 1;
    case Ss: return 2;
    case Ds: return 3;
    case Fs: return 4;
    case Gs: return 5;

    case Cr0:  return 0;
    case Cr1:  return 1;
    case Cr2:  return 2;
    case Cr3:  return 3;
    case Cr4:  return 4;
    case Cr5:  return 5;
    case Cr6:  return 6;
    case Cr7:  return 7;
    case Cr8:  return 0;
    case Cr9:  return 1;
    case Cr10: return 2;
    case Cr11: return 3;
    case Cr12: return 4;
    case Cr13: return 5;
    case Cr14: return 6;
    case Cr15: return 7;

    case Dbg0:  return 0;
    case Dbg1:  return 1;
    case Dbg2:  return 2;
    case Dbg3:  return 3;
    case Dbg4:  return 4;
    case Dbg5:  return 5;
    case Dbg6:  return 6;
    case Dbg7:  return 7;
    case Dbg8:  return 0;
    case Dbg9:  return 1;
    case Dbg10: return 2;
    case Dbg11: return 3;
    case Dbg12: return 4;
    case Dbg13: return 5;
    case Dbg14: return 6;
    case Dbg15: return 7;
    } // switch
    unreachable();
}

void fiska::x86::AsCtx::init() {
    kbw_of_ri = build_bw_of_ri_map();
}
