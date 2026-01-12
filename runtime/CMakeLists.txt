cmake_minimum_required(VERSION 3.15)
project(oonta_runtime)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -march=native")

add_library(oonta_runtime STATIC
	src/runtime_extern.cpp
	src/gc_extern.cpp
	src/gc.cpp
)
install(TARGETS oonta_runtime)
