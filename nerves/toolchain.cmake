if(NOT "$ENV{NERVES_SDK_SYSROOT}" STREQUAL "" AND NOT "$ENV{CROSSCOMPILE}" STREQUAL "")
    if("$ENV{MIX_TARGET}" MATCHES "^(rpi4|rpi3|rpi3a|rpi2|rpi0|rpi|bbb|x86_64|osd32mp1)$")
        cmake_minimum_required(VERSION 2.9 FATAL_ERROR)

        set(CMAKE_SYSTEM "Linux")
        set(CMAKE_SYSTEM_NAME "Linux")

        # set nerves sysroot
        set(CMAKE_FIND_ROOT_PATH "$ENV{NERVES_SDK_SYSROOT}")

        # adjust the default behavior of the find commands:
        # search headers and libraries in the target environment
        set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
        set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)

        # never search programs in the host environment
        set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
    endif()
endif()
