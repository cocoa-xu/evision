#
# CMake Toolchain file for cross-compiling for s390x-linux-gnu.
#
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR s390x)

set(CMAKE_C_COMPILER "/usr/bin/s390x-linux-gnu-gcc")
set(CMAKE_CXX_COMPILER "/usr/bin/s390x-linux-gnu-g++")

set(CMAKE_FIND_ROOT_PATH /usr/s390x-linux-gnu)

set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
