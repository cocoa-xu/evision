#
# CMake Toolchain file for cross-compiling for aarch64-windows-mvsc.
#
set(CMAKE_CROSSCOMPILING TRUE)
set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_GENERATOR_PLATFORM ARM64)
set(CMAKE_SYSTEM_PROCESSOR aarch64)

set(ENV_PATH "$ENV{PATH}")
set(ARM64_CL_PATH)
foreach(P IN LISTS ENV_PATH)
  string(TOLOWER "${P}" LP)
  if("${LP}" MATCHES "(.*)\\\\msvc\\\\(.*)bin\\\\hostx64\\\\arm64$")
    set(ARM64_CL_PATH "${P}")
  endif()
endforeach()

string(REPLACE "\\" "/" FORWARD_SLASH_PATH "${ARM64_CL_PATH}")
find_program(CL cl.exe HINTS "${FORWARD_SLASH_PATH}" PATHS "${FORWARD_SLASH_PATH}" NO_CACHE NO_DEFAULT_PATH NO_SYSTEM_ENVIRONMENT_PATH)

if(NOT CL)
  message(FATAL_ERROR "Cannot find cl.exe")
else()
  message(STATUS "Found cl.exe: ${CL}")
endif()

set(CMAKE_C_COMPILER "${CL}")
set(CMAKE_CXX_COMPILER "${CL}")
