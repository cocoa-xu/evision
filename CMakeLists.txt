cmake_minimum_required(VERSION 3.1 FATAL_ERROR)
project(evision)

find_package(OpenCV REQUIRED PATHS "${PRIV_DIR}/" NO_DEFAULT_PATH)
include_directories("${OpenCV_INCLUDE_DIRS}")

execute_process(COMMAND bash -c "rm -rf ${GENERATED_ELIXIR_SRC_DIR} && mkdir -p ${GENERATED_ELIXIR_SRC_DIR}")
message("enabled modules: ${ENABLED_CV_MODULES}")
execute_process(COMMAND bash -c "python3 ${PY_SRC}/gen2.py ${C_SRC} ${GENERATED_ELIXIR_SRC_DIR} ${C_SRC}/headers.txt ${ENABLED_CV_MODULES}" RESULT_VARIABLE STATUS)
if(STATUS STREQUAL "0")
    message("Successfully generated Erlang/Elixir bindings")
else()
    message(FATAL_ERROR "Failed to generate Erlang/Elixir bindings")
endif()

file(GLOB evision_bindings_main "${C_SRC}/*.hpp" "${C_SRC}/*.h" "${C_SRC}/*.cpp")
file(GLOB evision_custom_headers "${C_SRC}/evision_custom_headers/*.hpp")
file(GLOB evision_modules "${C_SRC}/modules/*.hpp" "${C_SRC}/modules/*.h")

add_library(evision SHARED
    ${evision_bindings_main}
    ${evision_custom_headers}
    ${evision_modules}
)
target_link_libraries(evision "${OpenCV_LIBS}")
set_property(TARGET evision PROPERTY CXX_STANDARD 14)
set_target_properties(evision PROPERTIES PREFIX "")
set_target_properties(evision PROPERTIES SUFFIX ".so")

cmake_policy(SET CMP0068 NEW)
set_target_properties(evision PROPERTIES
    INSTALL_RPATH_USE_LINK_PATH TRUE
    BUILD_WITH_INSTALL_RPATH TRUE
)

if(UNIX AND NOT APPLE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -shared")
    set_target_properties(evision PROPERTIES INSTALL_RPATH "\$ORIGIN/lib")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-but-set-variable")
else()
    # Although the compiler complains about not using these,
    # things only work with them set
    set(CMAKE_SHARED_LINKER_FLAGS "-flat_namespace -undefined suppress -undefined dynamic_lookup")
    set_target_properties(evision PROPERTIES INSTALL_RPATH "@loader_path/lib")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -I${ERTS_INCLUDE_DIR} -O3 -Wall -Wextra -Wno-unused-parameter -Wno-missing-field-initializers -Wno-deprecated-declarations")
