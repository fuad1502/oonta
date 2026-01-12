cmake_minimum_required(VERSION 3.15)
project(oonta_runtime VERSION 0.2.0)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

add_library(oonta_runtime STATIC
	src/runtime_extern.cpp
	src/gc_extern.cpp
	src/gc.cpp
)

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
