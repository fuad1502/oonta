cmake_minimum_required(VERSION 3.15)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
project(oonta_runtime)

add_library(oonta_runtime SHARED
	src/runtime_extern.cpp
	src/gc_extern.cpp
	src/gc.cpp
)
install(TARGETS oonta_runtime)
