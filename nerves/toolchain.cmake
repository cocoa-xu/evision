cmake_minimum_required(VERSION 3.4 FATAL_ERROR)

set(CMAKE_SYSTEM "Linux")
set(CMAKE_SYSTEM_NAME "Linux")

find_program(NERVES_CC "${CMAKE_C_COMPILER}")
string(REPLACE "-gcc" "" TOOLCHAIN_PREFIX "${CMAKE_C_COMPILER}")
get_filename_component(NERVES_CC_PATH "${NERVES_CC}" PATH)
set(CMAKE_FIND_ROOT_PATH "${NERVES_CC_PATH}/../${TOOLCHAIN_PREFIX}/sysroot")

# adjust the default behavior of the find commands:
# search headers and libraries in the target environment
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)

# search programs in the host environment
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
