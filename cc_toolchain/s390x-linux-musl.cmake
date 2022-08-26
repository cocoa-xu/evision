#
# CMake Toolchain file for cross-compiling for s390x-linux-musl.
#
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR s390x)
set(TARGET_SYS s390x-linux-musl)
include(${CMAKE_CURRENT_LIST_DIR}/zig.toolchain.cmake)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")

set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
