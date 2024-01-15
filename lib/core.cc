#include "lib/core.hh"
#include "sys/mman.h"
#include "fcntl.h"
#include "sys/stat.h"
#include "execinfo.h"

namespace {

auto print_stack_trace() -> void {
    constexpr u8 max_stack_frames = 64; 
    void *buffer[max_stack_frames];

    int stack_frames = backtrace(buffer, max_stack_frames);
    char **strings = backtrace_symbols(buffer, stack_frames);

    if (strings == NULL) {
        fmt::print("Failed to print the stack trace! Exiting...\n");
        std::exit(1);
    }

    for (i32 idx = 0; idx < stack_frames; ++idx) {
        fmt::print("--> {}\n", strings[idx]);
    }

    free(strings);
}

template <typename T, typename... Ts>
concept isa = (std::same_as<T, Ts> or ...);

template <typename T>
requires isa<T, u8, u16, u32>
auto fits_in_unsigned(i64 num) -> bool {
    return num >= 0 and num <= std::numeric_limits<T>::max();
}

template <typename T>
requires isa<T, i8, i16, i32>
auto fits_in_signed(i64 num) -> bool {
    return num >= std::numeric_limits<T>::min() 
        and num <= std::numeric_limits<T>::max();
}

}  // namespace

auto utils::fits_in_u8(i64 num) -> bool { return fits_in_unsigned<u8>(num); }
auto utils::fits_in_u16(i64 num) -> bool { return fits_in_unsigned<u16>(num); }
auto utils::fits_in_u32(i64 num) -> bool { return fits_in_unsigned<u32>(num); }

auto utils::fits_in_i8(i64 num) -> bool { return fits_in_signed<i8>(num); }
auto utils::fits_in_i16(i64 num) -> bool { return fits_in_signed<i16>(num); }
auto utils::fits_in_i32(i64 num) -> bool { return fits_in_signed<i32>(num); }

[[noreturn]] auto utils::assert_helper(
    utils::AK k,
    std::string_view cond,
    std::string_view file,
    std::string_view func_name,
    u32 line,
    std::string message
) -> void
{
    using enum fmt::color;
    using enum fmt::emphasis;

    // Print the file location and line number.
    fmt::print(bold | underline | fg(medium_slate_blue), "\u2192 {}:{}", file, line);
    fmt::print(" in");
    fmt::print(fg(medium_slate_blue), " {}: ", func_name);

    // Print the assertion kind.
    switch (k) {
    case AK::Assertion:
        fmt::print(bold | fg(red), "Assertion failed:");
        fmt::print(" '{}'", cond);
        if (not message.empty()) {
            fmt::print(" {}", message);
        }
        fmt::print("\n");
        break;
    case AK::Todo:
        fmt::print(bold | fg(red), "TODO\n");
        break;
    case AK::Unreachable:
        fmt::print(bold | fg(red), "Unreachable code reached\n");
        break;
    } // switch

    // Skip a line to show the stack trace.
    fmt::print("\n");

    fmt::print("Stack trace (most recent call first):\n");
    print_stack_trace();

    std::exit(1);
}


auto load_file(const fs::path& path) -> Vec<char> {
    i32 fd = open(path.c_str(), O_RDONLY);
    defer { close(fd); };
    assert(fd >= 0, "Failed to open file '{}'", path.string());

    struct stat file_st{};
    assert(fstat(fd, &file_st) >= 0, "Failed to query stats of file: '{}'", path.string());

    void* ptr = mmap(nullptr, usz(file_st.st_size), PROT_READ, MAP_PRIVATE, fd, 0);
    defer { munmap(ptr, static_cast<usz>(file_st.st_size)); };

    assert(ptr != MAP_FAILED, "Failed to mmap file: '{}'", path.string());

    Vec<char> content(usz(file_st.st_size));
    memcpy(content.data(), ptr, usz(file_st.st_size));

    return content;
}
