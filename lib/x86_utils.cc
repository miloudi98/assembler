#include "lib/x86_utils.hh"

namespace {

auto build_bw_of_ri_map() -> Vec<Vec<fiska::x86::BW>> {
    using enum fiska::x86::RI;
    using enum fiska::x86::BW;
    using fiska::x86::AsCtx;

    Vec<Vec<fiska::x86::BW>> bws_of_ri(AsCtx::knum_regs);

    bws_of_ri[+Rax] = {B8, B16, B32, B64};
    bws_of_ri[+Rcx] = {B8, B16, B32, B64};
    bws_of_ri[+Rdx] = {B8, B16, B32, B64};
    bws_of_ri[+Rbx] = {B8, B16, B32, B64};
    bws_of_ri[+Rsp] = {B8, B16, B32, B64};
    bws_of_ri[+Rbp] = {B8, B16, B32, B64};
    bws_of_ri[+Rsi] = {B8, B16, B32, B64};
    bws_of_ri[+Rdi] = {B8, B16, B32, B64};
    bws_of_ri[+R8]  = {B8, B16, B32, B64};
    bws_of_ri[+R9]  = {B8, B16, B32, B64};
    bws_of_ri[+R10] = {B8, B16, B32, B64};
    bws_of_ri[+R11] = {B8, B16, B32, B64};
    bws_of_ri[+R12] = {B8, B16, B32, B64};
    bws_of_ri[+R13] = {B8, B16, B32, B64};
    bws_of_ri[+R14] = {B8, B16, B32, B64};
    bws_of_ri[+R15] = {B8, B16, B32, B64};

    bws_of_ri[+Rip] = {B16, B32, B64};

    bws_of_ri[+Rah] = {B8};
    bws_of_ri[+Rch] = {B8};
    bws_of_ri[+Rdh] = {B8};
    bws_of_ri[+Rbh] = {B8};

    bws_of_ri[+Cr0] = {B64};
    bws_of_ri[+Cr1] = {B64};
    bws_of_ri[+Cr2] = {B64};
    bws_of_ri[+Cr3] = {B64};
    bws_of_ri[+Cr4] = {B64};
    bws_of_ri[+Cr5] = {B64};
    bws_of_ri[+Cr6] = {B64};
    bws_of_ri[+Cr7] = {B64};
    bws_of_ri[+Cr8] = {B64};
    bws_of_ri[+Cr9] = {B64};
    bws_of_ri[+Cr10] = {B64};
    bws_of_ri[+Cr11] = {B64};
    bws_of_ri[+Cr12] = {B64};
    bws_of_ri[+Cr13] = {B64};
    bws_of_ri[+Cr14] = {B64};
    bws_of_ri[+Cr15] = {B64};

    bws_of_ri[+Dbg0] = {B64};
    bws_of_ri[+Dbg1] = {B64};
    bws_of_ri[+Dbg2] = {B64};
    bws_of_ri[+Dbg3] = {B64};
    bws_of_ri[+Dbg4] = {B64};
    bws_of_ri[+Dbg5] = {B64};
    bws_of_ri[+Dbg6] = {B64};
    bws_of_ri[+Dbg7] = {B64};
    bws_of_ri[+Dbg8] = {B64};
    bws_of_ri[+Dbg9] = {B64};
    bws_of_ri[+Dbg10] = {B64};
    bws_of_ri[+Dbg11] = {B64};
    bws_of_ri[+Dbg12] = {B64};
    bws_of_ri[+Dbg13] = {B64};
    bws_of_ri[+Dbg14] = {B64};
    bws_of_ri[+Dbg15] = {B64};

    return bws_of_ri;
}

} // namespace

auto fiska::x86::Reg::kind() const -> RK {
    return rk_of_ri(id_);
}

auto fiska::x86::Reg::index() const -> u8 {
    using enum RI;

    switch (id_) {
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

auto fiska::x86::X86Op::modrm_encoding() const -> u8 {
    assert((is<Reg, Mem>()));

    if (is<Reg>()) {
        return as<Reg>().index();
    }

    if (not as<Mem>().sib_) {
        return as<Mem>().base_reg_->index();
    }

    return Mem::ksib_marker;
}

void fiska::x86::AsCtx::init() {
    kbw_of_ri = build_bw_of_ri_map();
}


