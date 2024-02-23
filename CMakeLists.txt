## ============================================================================
## Initial setup
## ============================================================================
cmake_minimum_required(VERSION 3.20)
project(x86_assembler VERSION 0.0.1 LANGUAGES CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

## Use colors when emitting diagnostics
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
	add_compile_options(-fdiagnostics-color=always)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
	add_compile_options(-fcolor-diagnostics)
endif()

## ============================================================================
## Third party libraries:
## 1. libfmt
## ============================================================================
cmake_policy(SET CMP0135 NEW)

include(FetchContent)
message(STATUS "Downloading and installing libfmt...")
FetchContent_Declare(
	libfmt
	GIT_REPOSITORY "https://github.com/fmtlib/fmt.git"
	GIT_TAG 10.1.0
	SOURCE_DIR "${PROJECT_SOURCE_DIR}/third_party/fmt"
)
FetchContent_MakeAvailable(libfmt)

## ============================================================================
## libraries
## ============================================================================
file(GLOB_RECURSE x86_assembler_sources
    "${PROJECT_SOURCE_DIR}/lib/**.cc"
    "${PROJECT_SOURCE_DIR}/lib/**.hh")

add_library(x86_assembler STATIC ${x86_assembler_sources})

target_include_directories(x86_assembler PUBLIC "${PROJECT_SOURCE_DIR}/")
target_include_directories(x86_assembler PUBLIC "${libfmt_SOURCE_DIR}/include")
target_link_libraries(x86_assembler PUBLIC fmt)

target_compile_options(x86_assembler PUBLIC
	-Wall -Wextra					 # Enable ‘all’ warnings.
	-Wundef							 # Invalid #undef or undefined macro in #if.
	-Wcast-align					 # Casting that changes alignment.
	-Wconversion					 # Implicit conversions.
	-Wsign-conversion				 # Implicit sign conversions.
	-Wformat=2						 # Stricter format checking.

	## Disabled warnings.
	-Wno-unused-function
	-Wno-unused-local-typedefs
	-Wno-unused-parameter

	## NULL Errors.
	-Werror=nonnull					 # Passing NULL to nonnull parameter.

	## Memory Errors.
	-Werror=address					 # Suspicious use of addresses.
	-Werror=init-self				 # Initialization of a variable with itself.
	-Werror=uninitialized

	## Return type.
	-Werror=return-type

	## C/C++.
	-Werror=implicit-fallthrough
	-Werror=missing-include-dirs	 # User-specified include dir does not exist.
	-Werror=pointer-arith			 # Disallow void* and function pointer arithmetic.
	-Werror=string-compare			 # Nonsensical string comparisons.
	-Werror=switch					 # Missing switch cases.
	# -Werror=switch-enum			 # Switch on enum (even if there is a default case).
	-Werror=write-strings			 # Strings in C should be const char*.

	## C++.
	-Werror=missing-field-initializers
	-Werror=non-virtual-dtor
	-Werror=pessimizing-move
)

# Debug and release compile options.
target_compile_options(x86_assembler PUBLIC 
	$<$<CONFIG:DEBUG>: -O0 -g -ggdb3 -rdynamic -funwind-tables -fsanitize=address>
	$<$<CONFIG:RELEASE>:-O3 -march=native>)
target_link_options(x86_assembler PUBLIC 
	$<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3 -rdynamic -funwind-tables -fsanitize=address>
	$<$<CONFIG:RELEASE>:-O3 -march=native>)

## ============================================================================
## Executables
## ============================================================================
add_executable(x86_assembler_driver "${PROJECT_SOURCE_DIR}/exec/main.cc")
set_target_properties(x86_assembler_driver PROPERTIES OUTPUT_NAME "fiskaas")
target_link_libraries(x86_assembler_driver PRIVATE x86_assembler)
