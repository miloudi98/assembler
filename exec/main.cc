#include "lib/core.hh"

void h() {
    assert(1 > 2, "This is an assertion, {}, {}.", 1, 2);
}

void g() {
    h();
}

void f() {
    g();
}

auto main(i32 argc, char* argv[]) -> i32 {
    fmt::print("Bismillah\n");
    unreachable();
    f();
    return 0;
}
