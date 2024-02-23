#include "lib/common/support.hh"
#include "lib/common/base.hh"

auto fiska::assembler::StringInterner::save(StrRef str) -> StrRef {
    if (not unique_strings_.contains(str)) {
        // Intern the string.
        storage_.push_back(new char[str.size()]);
        std::memcpy(storage_.back(), str.data(), str.size());
        unique_strings_.insert(StrRef{storage_.back(), str.size()});
    }

    return *unique_strings_.find(str);
}

fiska::assembler::StringInterner::~StringInterner() {
    rgs::for_each(storage_, [](char* alloc) { delete[] alloc; });
}

