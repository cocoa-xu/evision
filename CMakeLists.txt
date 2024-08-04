cmake_minimum_required(VERSION 3.5 FATAL_ERROR)
project(evision)

if(NOT DEFINED CMAKE_BUILD_TYPE OR "${CMAKE_BUILD_TYPE}" STREQUAL "")
    set(CMAKE_BUILD_TYPE "Release")
endif()

if(DEFINED MIX_APP_PATH AND NOT "${MIX_APP_PATH}" STREQUAL "")
    set(PRIV_DIR "${MIX_APP_PATH}/priv")
else()
    set(PRIV_DIR "${CMAKE_CURRENT_SOURCE_DIR}/priv")
endif()
message(STATUS "Using PRIV_DIR: ${PRIV_DIR}")

if(DEFINED ERTS_INCLUDE_DIR AND NOT "${ERTS_INCLUDE_DIR}" STREQUAL "")
    set(ERTS_INCLUDE_DIR "${ERTS_INCLUDE_DIR}")
else()
    set(ERTS_INCLUDE_DIR_ONE_LINER "erl -noshell -eval \"io:format('~ts/erts-~ts/include/', [code:root_dir(), erlang:system_info(version)]), halt().\"")
    if(WIN32)
        execute_process(COMMAND powershell -command "${ERTS_INCLUDE_DIR_ONE_LINER}" OUTPUT_VARIABLE ERTS_INCLUDE_DIR)
    else()
        execute_process(COMMAND bash -c "${ERTS_INCLUDE_DIR_ONE_LINER}" OUTPUT_VARIABLE ERTS_INCLUDE_DIR)
    endif()
    set(ERTS_INCLUDE_DIR "${ERTS_INCLUDE_DIR}")
endif()
message(STATUS "Using ERTS_INCLUDE_DIR: ${ERTS_INCLUDE_DIR}")

find_package(OpenCV REQUIRED PATHS "${PRIV_DIR}/" NO_DEFAULT_PATH)
include_directories("${OpenCV_INCLUDE_DIRS}")

option(EVISION_ENABLE_CONTRIB "Enable OpenCV contrib modules" ON)
message(STATUS "EVISION_ENABLE_CONTRIB=${EVISION_ENABLE_CONTRIB}")
if(EVISION_ENABLE_CONTRIB)
    set(C_SRC_HEADERS_TXT "headers-contrib.txt")
else()
    set(C_SRC_HEADERS_TXT "headers.txt")
endif()

option(EVISION_ENABLE_CUDA "Enable OpenCV CUDA modules" ON)
message(STATUS "EVISION_ENABLE_CUDA=${EVISION_ENABLE_CUDA}")

option(GLEAM_EVISION "Compile for Gleam" OFF)
if(GLEAM_EVISION)
    add_compile_definitions(GLEAM_EVISION)
    set(EVISION_GENERATE_LANG "gleam,elixir")
endif()

if(POLICY CMP0068)
    cmake_policy(SET CMP0068 NEW)
endif()

if(WIN32)
    get_filename_component(_PRIV_DIR "${PRIV_DIR}" ABSOLUTE)
    string(LENGTH "${_PRIV_DIR}" _PRIV_DIR_LENGTH)
    string(LENGTH "${_PRIV_DIR}" _OpenCV_LIB_PATH_LENGTH)
    math(EXPR _PRIV_DIR_LENGTH "${_PRIV_DIR_LENGTH}+1")
    math(EXPR _OpenCV_REL_LIB_LENGTH "${_OpenCV_LIB_PATH_LENGTH}-${_PRIV_DIR_LENGTH}")
    string(SUBSTRING "${OpenCV_LIB_PATH}" "${_PRIV_DIR_LENGTH}" "${_OpenCV_REL_LIB_LENGTH}" EVISION_REL_LIB)
    execute_process(COMMAND python3.exe "${PY_SRC}\\gen2.py" "--c_src=${C_SRC}" "--elixir_gen=${GENERATED_ELIXIR_SRC_DIR}" "--erlang_gen=${GENERATED_ERLANG_SRC_DIR}" "--gleam_gen=${GENERATED_GLEAM_SRC_DIR}" "--headers=${C_SRC}\\${C_SRC_HEADERS_TXT}" "--lang=${EVISION_GENERATE_LANG}" "--modules=${ENABLED_CV_MODULES}" RESULT_VARIABLE STATUS)
    if(NOT STATUS STREQUAL "0")
        execute_process(COMMAND python.exe "${PY_SRC}\\gen2.py" "--c_src=${C_SRC}" "--elixir_gen=${GENERATED_ELIXIR_SRC_DIR}" "--erlang_gen=${GENERATED_ERLANG_SRC_DIR}" "--gleam_gen=${GENERATED_GLEAM_SRC_DIR}" "--headers=${C_SRC}\\${C_SRC_HEADERS_TXT}" "--lang=${EVISION_GENERATE_LANG}" "--modules=${ENABLED_CV_MODULES}" RESULT_VARIABLE STATUS)
    endif()
else()
    execute_process(COMMAND bash -c "python3 ${PY_SRC}/gen2.py --c_src=\"${C_SRC}\" --elixir_gen=\"${GENERATED_ELIXIR_SRC_DIR}\" --erlang_gen=\"${GENERATED_ERLANG_SRC_DIR}\" --gleam_gen=\"${GENERATED_GLEAM_SRC_DIR}\" --headers=\"${C_SRC}/${C_SRC_HEADERS_TXT}\" --lang=\"${EVISION_GENERATE_LANG}\" --modules=\"${ENABLED_CV_MODULES}\"" RESULT_VARIABLE STATUS)
endif()
if(STATUS STREQUAL "0")
    message(STATUS "Successfully generated binding code for: ${EVISION_GENERATE_LANG}")
else()
    message(FATAL_ERROR "Failed to generate binding code for: ${EVISION_GENERATE_LANG}")
endif()

if(WIN32)
    string(REPLACE "\\" "/" C_SRC "${C_SRC}")
endif()
file(GLOB evision_bindings_main "${C_SRC}/*.hpp" "${C_SRC}/*.h" "${C_SRC}/*.cpp")
file(GLOB evision_custom_headers "${C_SRC}/evision_custom_headers/*.hpp")
file(GLOB evision_modules "${C_SRC}/modules/*.hpp" "${C_SRC}/modules/*.h" "${C_SRC}/modules/evision_cuda_ipc.cc")
file(GLOB evision_backend "${C_SRC}/modules/evision_backend/*.h")
set(EVISION_LIBRARIES "${OpenCV_LIBS}")

if(EVISION_ENABLE_CUDA)
    find_package(CUDA REQUIRED)
    include_directories("${CUDA_INCLUDE_DIRS}")

    if(WIN32)
        if("${CMAKE_BUILD_TYPE}" STREQUAL "Release")
            set(MSVC_LIB_LINK "/MD")
        else()
            set(MSVC_LIB_LINK "/MDd")
        endif()
        execute_process(COMMAND nvcc -shared --std "c++17" "-Xcompiler=${MSVC_LIB_LINK}" -DCUDA_ENABLED -c "${C_SRC}/modules/evision_cuda.cc" -o "${C_SRC}/modules/evision_cuda.o" RESULT_VARIABLE STATUS)
    else()
        execute_process(COMMAND bash -c "nvcc --std c++17 -shared -Xcompiler -fPIC -DCUDA_ENABLED -c \"${C_SRC}/modules/evision_cuda.cc\" -o \"${C_SRC}/modules/evision_cuda.o\"" RESULT_VARIABLE STATUS)
    endif()
    if(STATUS STREQUAL "0")
        message(STATUS "Successfully compiled evision_cuda.cc using nvcc")
    else()
        message(FATAL_ERROR "Failed to compile evision_cuda.cc using nvcc")
    endif()
    list(APPEND evision_bindings_main "${C_SRC}/modules/evision_cuda.o")
    list(APPEND EVISION_LIBRARIES cudart)
    add_compile_definitions(CUDA_ENABLED)
    # if(UNIX)
    #     add_compile_definitions(CUDA_HOST_IPC_ENABLED)
    # endif()
else()
    list(APPEND evision_bindings_main "${C_SRC}/modules/evision_cuda.cc" "${C_SRC}/modules/evision_cuda.h")
endif()

add_library(evision SHARED
    ${evision_bindings_main}
    ${evision_custom_headers}
    ${evision_modules}
    ${evision_backend}
)
install(
    TARGETS evision
    RUNTIME DESTINATION "${PRIV_DIR}"
)
target_link_libraries(evision "${EVISION_LIBRARIES}")
set_property(TARGET evision PROPERTY CXX_STANDARD 17)
set_property(TARGET evision PROPERTY POSITION_INDEPENDENT_CODE ON)
set_target_properties(evision PROPERTIES PREFIX "")
if(NOT WIN32)
    set_target_properties(evision PROPERTIES SUFFIX ".so")
endif()
set_target_properties(evision PROPERTIES
    INSTALL_RPATH_USE_LINK_PATH TRUE
    BUILD_WITH_INSTALL_RPATH TRUE
)

if(UNIX AND NOT APPLE)
    set_target_properties(evision PROPERTIES INSTALL_RPATH "\$ORIGIN/lib:\$ORIGIN/lib64")
elseif(UNIX AND APPLE)
    # Although the compiler complains about not using these,
    # things only work with them set
    set(CMAKE_SHARED_LINKER_FLAGS "-flat_namespace -undefined dynamic_lookup")
    set_target_properties(evision PROPERTIES INSTALL_RPATH "@loader_path/lib")
endif()

include_directories(${ERTS_INCLUDE_DIR})
if(WIN32)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /bigobj /wd4996 /wd4267 /wd4068")
else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-unused-parameter -Wno-missing-field-initializers -Wno-deprecated-declarations -Wno-unused-but-set-variable")
endif()

if(WIN32)
    add_library(windows_fix SHARED
        "${C_SRC}/windows_fix/windows_fix.cpp"
    )
    if(EVISION_ENABLE_CUDA)
        target_compile_definitions(windows_fix PRIVATE EVISION_ENABLE_CUDA)
    endif()
    install(
        TARGETS windows_fix
        RUNTIME DESTINATION "${PRIV_DIR}"
    )
    set_property(TARGET windows_fix PROPERTY CXX_STANDARD 17)
    set_property(TARGET windows_fix PROPERTY POSITION_INDEPENDENT_CODE ON)
    set_target_properties(windows_fix PROPERTIES PREFIX "")
    set_target_properties(windows_fix PROPERTIES
        INSTALL_RPATH_USE_LINK_PATH TRUE
        BUILD_WITH_INSTALL_RPATH TRUE
    )
endif()
