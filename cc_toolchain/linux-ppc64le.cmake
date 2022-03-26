#
# CMake Toolchain file for cross-compiling for ppc64le on Linux (ubuntu 20.04).
#
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR ppc64le)

set(CMAKE_C_COMPILER "/usr/bin/powerpc64le-linux-gnu-gcc")
set(CMAKE_CXX_COMPILER "/usr/bin/powerpc64le-linux-gnu-g++")

set(CMAKE_FIND_ROOT_PATH /usr/powerpc64le-linux-gnu)

set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
