cmake_minimum_required(VERSION 3.15)
project(oonta_runtime VERSION 0.3.0)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

find_program(LLVM_CONFIG NAMES llvm-config)
if(LLVM_CONFIG)
  execute_process(COMMAND "${LLVM_CONFIG}" --libdir
    OUTPUT_VARIABLE _llvm_libdir
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  execute_process(COMMAND "${LLVM_CONFIG}" --includedir
    OUTPUT_VARIABLE _llvm_incdir
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
endif()

find_path(UNWIND_INCLUDE_DIR NAMES libunwind.h HINTS "${_llvm_incdir}")
find_library(UNWIND_LIBRARY NAMES libunwind.a HINTS "${_llvm_libdir}/x86_64-unknown-linux-gnu")

add_library(oonta_runtime STATIC
	src/runtime_extern.cpp
	src/gc_extern.cpp
	src/gc.cpp
  src/heap.cpp
)

target_include_directories(oonta_runtime PUBLIC ${UNWIND_INCLUDE_DIR})
target_link_libraries(oonta_runtime PUBLIC ${UNWIND_LIBRARY})

set(CPACK_GENERATOR "DEB")
set(CPACK_PACKAGE_NAME "liboonta_runtime")
set(CPACK_PACKAGE_VERSION ${CMAKE_PROJECT_VERSION})
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Fuad Ismail <fuad1502@gmail.com>")
set(CPACK_DEBIAN_PACKAGE_DESCRIPTION "Oonta runtime library")
set(CPACK_DEBIAN_PACKAGE_HOMEPAGE "https://github.com/fuad1502/oonta")
set(CPACK_DEBIAN_PACKAGE_SECTION "devel")
set(CPACK_DEBIAN_PACKAGE_PRIORITY "optional")
include(CPack)

install(TARGETS oonta_runtime)
