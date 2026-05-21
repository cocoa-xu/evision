#
# CMake Toolchain file for cross-compiling for aarch64-linux-musl.
#
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR aarch64)
set(TARGET_SYS aarch64-linux-musl)
include(${CMAKE_CURRENT_LIST_DIR}/zig.toolchain.cmake)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")

# OpenCV's KleidiCV HAL passes `-march=armv8-a`, which `zig cc` rejects
# (it parses the value as the unknown CPU `armv8`). NEON is baseline on
# aarch64 anyway, so disable the HAL for this cross-compile target.
set(WITH_KLEIDICV OFF CACHE BOOL "" FORCE)

set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
