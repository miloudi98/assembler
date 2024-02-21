#ifndef __X86_ASSEMBLER_LIB_BASE_HH__
#define __X86_ASSEMBLER_LIB_BASE_HH__

#include <vector>
#include <memory>
#include <list>
#include <array>
#include <expected>
#include <map>
#include <algorithm>
#include <chrono>
#include <unordered_set>
#include <unordered_map>
#include <span>
#include <ranges>
#include <string>
#include <utility>
#include <optional>
#include <span>
#include <source_location>
#include <string_view>
#include <concepts>
#include <filesystem>
#include <unordered_map>
#include <fmt/format.h>
#include <fmt/color.h>

//=======================================================================================
// A lot of the code shown here was inspired by the following project:
// https://github.com/Sirraide/source
//=======================================================================================

//=======================================================================================
// Miscallaneous types, namespace aliases and concepts.
//=======================================================================================
using i1 = bool;
using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;
using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using f32 = float;
using f64 = double;

using usz = size_t;
using uptr = uintptr_t;
using isz = ptrdiff_t;
using iptr = intptr_t;

template <typename T>
using Rc = std::shared_ptr<T>;

template <typename T>
using Box = std::unique_ptr<T>;

template<typename T>
using Vec = std::vector<T>;

template <typename T>
using Opt = std::optional<T>;

template <typename T, typename E>
using Result = std::expected<T, E>;

template <typename... Args>
using HashMap = std::unordered_map<Args...>;

template <typename... Args>
using HashSet = std::unordered_set<Args...>;

template <typename... Args>
using Map = std::map<Args...>;

template <typename First, typename Second>
using Pair = std::pair<First, Second>;

template <typename T, usz Size>
using Arr = std::array<T, Size>;

using ByteVec = Vec<u8>;
using StrRef = std::string_view;
using Str = std::string;

template <typename T, typename... Us>
concept OneOf = (std::same_as<T, Us> or ...);

namespace fs = std::filesystem;
namespace vws = std::views;
namespace rgs = std::ranges;
namespace chr = std::chrono;
using namespace std::string_literals;

//=======================================================================================
// Helper Macros.
//=======================================================================================
#define CAT(x, y) CAT_HELPER(x, y)
#define CAT_HELPER(x, y) x##y

#define FWD(arg) std::forward<decltype(arg)>(arg)

#define FISKA_PRAGMA_HELPER(x) _Pragma (#x)

#define GCC_DIAG_IGNORE_PUSH(warning) \
    _Pragma("GCC diagnostic push") \
    FISKA_PRAGMA_HELPER(GCC diagnostic ignored #warning) \

#define GCC_DIAG_IGNORE_POP() \
    _Pragma("GCC diagnostic pop")

#define defer auto CAT(__defer, __COUNTER__) = ::utils::DeferType1{}->*[&]

#define assert(cond, ...)  (cond ? void(0) :       \
    ::utils::assert_helper(                        \
        ::utils::AK::Assertion,                    \
        #cond,                                     \
        __FILE__,                                  \
        __PRETTY_FUNCTION__,                       \
        __LINE__                                   \
        __VA_OPT__(,fmt::format(__VA_ARGS__))      \
    ))                                             \

#define todo(...)                                  \
    ::utils::assert_helper(                        \
        ::utils::AK::Todo,                         \
        "",                                        \
        __FILE__,                                  \
        __PRETTY_FUNCTION__,                       \
        __LINE__                                   \
        __VA_OPT__(,fmt::format(__VA_ARGS__))      \
    )

#define unreachable(...)                           \
    ::utils::assert_helper(                        \
        ::utils::AK::Unreachable,                  \
        "",                                        \
        __FILE__,                                  \
        __PRETTY_FUNCTION__,                       \
        __LINE__                                   \
        __VA_OPT__(,fmt::format(__VA_ARGS__))      \
    )

#define NOT_COPYABLE(Type)                         \
    Type(const Type&) = delete;                    \
    Type& operator=(const Type&) = delete          \

#define NOT_MOVABLE(Type)                          \
    Type(Type&&) = delete;                         \
    Type& operator=(Type&&) = delete               

#define NOT_COPYABLE_NOT_MOVABLE(Type)             \
    NOT_COPYABLE(Type);                            \
    NOT_MOVABLE(Type)
 
//=======================================================================================
// Helper functions and types.
//=======================================================================================
template <typename E> 
requires std::is_enum_v<E>
constexpr auto operator+(E e) -> std::underlying_type_t<E> {
    return std::to_underlying(e);
}

template <auto... Us>
auto isa(const auto& arg) -> i1 {
    return ((arg == Us) or ...);
}

namespace utils {

// Assertion kinds.
enum struct AK {
    Assertion,
    Todo,
    Unreachable
};

[[noreturn]] auto assert_helper(
    AK k,
    std::string_view cond,
    std::string_view file,
    std::string_view func_name,
    u32 line,
    std::string message = ""
) -> void;

// StringMap utility type.
//
// This is basically an std::unordered_map<std::string, T> but with the 
// additional property of heterogeneous lookups. Lookups can be made using
// any type that is convertible to a string_view. One important thing to note
// here is that the type with which the lookup is made must be comparable to 
// a string. Something like std::span<const char> will not work!
// A vanilla std::unordered_map doesn't allow this kind of behaviour by default.
struct StringHash {
    using is_transparent = void;

    [[nodiscard]] auto operator()(std::string_view data) const { return std::hash<std::string_view>{}(data); }

    [[nodiscard]] auto operator()(const std::string& data) const { return std::hash<std::string>{}(data); }

    template <typename Ty>
    requires requires(const Ty& ty) {
        { ty.size() } -> std::convertible_to<usz>;
        { ty.data() } -> std::convertible_to<const char*>;
    }
    [[nodiscard]] auto operator()(const Ty& ty) const { 
        return std::hash<std::string_view>{}(std::string_view{ty.data(), ty.size()});
    }
};

template <typename T>
using StringMap = std::unordered_map<std::string, T, StringHash, std::equal_to<>>;

template <typename T, typename Str>
requires requires(const Str& str) {
    { str.size() } -> std::convertible_to<usz>;
    { str.data() } -> std::convertible_to<const char*>;
}
auto strmap_get(const StringMap<T>& strmap, const Str& str_key) -> T {
    assert(strmap.contains(str_key), "Key: '{}' not found in the stringmap", str_key);
    return strmap.find(str_key)->second;
}

template <typename T>
auto strmap_get(const StringMap<T>& strmap, const char* str_key) -> T {
    return strmap_get(strmap, StrRef{str_key});
}

template <typename Callable>
struct DeferType2 {
    Callable cb_;

    explicit DeferType2(Callable&& cb) : 
        cb_(std::forward<Callable>(cb)) {}

    ~DeferType2() { cb_(); }
};

struct DeferType1 {
    template <typename Callable>
    auto operator->*(Callable&& cb) -> DeferType2<Callable> {
        return DeferType2<Callable>{std::forward<Callable>(cb)};
    }
};

auto fits_in_u8(i64 num) -> i1;
auto fits_in_u16(i64 num) -> i1;
auto fits_in_u24(i64 num) -> i1;
auto fits_in_u32(i64 num) -> i1;

auto fits_in_i8(i64 num) -> i1;
auto fits_in_i16(i64 num) -> i1;
auto fits_in_i32(i64 num) -> i1;

auto fits_in_b8(i64 num) -> i1;
auto fits_in_b16(i64 num) -> i1;
auto fits_in_b32(i64 num) -> i1;

auto safe_add(i64 lhs, i64 rhs) -> Pair<i64, i1>;

auto number_width(u64 num, u32 base = 10) -> u8;

auto load_file(const fs::path& path) -> Vec<char>;
auto write_file(const void* data, usz size, const fs::path& path) -> i1;

auto random_tmp_path(StrRef extension) -> fs::path;

}  // namespace utils


#endif

