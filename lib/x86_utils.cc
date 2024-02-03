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
    bws_of_ri[+Rah] = {B8};
    bws_of_ri[+Rch] = {B8};
    bws_of_ri[+Rdh] = {B8};
    bws_of_ri[+Rbh] = {B8};

    bws_of_ri[+Rip] = {B16, B32, B64};

    bws_of_ri[+Es] = {B16};
    bws_of_ri[+Cs] = {B16};
    bws_of_ri[+Ss] = {B16};
    bws_of_ri[+Ds] = {B16};
    bws_of_ri[+Fs] = {B16};
    bws_of_ri[+Gs] = {B16};

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

void fiska::x86::AsCtx::init() {
    kbw_of_ri = build_bw_of_ri_map();
}
