#include "lib/front_end/support.hh"

#include "lib/core.hh"

auto fiska::fe::StringInterner::save(StrRef str) -> StrRef {
    auto [slot, is_inserted] = unique_strings_.insert(str);
    if (is_inserted) {
        char* alloc = new char[str.size() + 1];
        alloc[str.size()] = '\0';
        storage_.push_back(alloc);
    }
    return *slot;
}

fiska::fe::StringInterner::~StringInterner() {
    rgs::for_each(storage_, [](char* alloc) { delete[] alloc; });
}
