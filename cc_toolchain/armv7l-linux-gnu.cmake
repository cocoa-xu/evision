#
# CMake Toolchain file for cross-compiling for armv7l on Linux (ubuntu 20.04).
#
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR armv7l)

set(CMAKE_C_COMPILER "/usr/bin/arm-linux-gnueabihf-gcc-9")
set(CMAKE_CXX_COMPILER "/usr/bin/arm-linux-gnueabihf-g++-9")

set(CMAKE_FIND_ROOT_PATH /usr/arm-linux-gnueabihf)

set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
