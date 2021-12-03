cmake_minimum_required(VERSION 3.12 FATAL_ERROR)
project(evision)

find_package(OpenCV REQUIRED PATHS "${PRIV_DIR}/" NO_DEFAULT_PATH)
include_directories("${OpenCV_INCLUDE_DIRS}")
file(GLOB evision_sources CONFIGURE_DEPENDS "${C_SRC}/*.cpp" "${C_SRC}/*.hpp" "${C_SRC}/*.h")

execute_process(COMMAND bash -c "python3 ${PY_SRC}/gen2.py ${C_SRC} ${GENERATED_ELIXIR_SRC_DIR} ${C_SRC}/headers.txt")

add_executable(evision.so ${evision_sources})
target_link_libraries(evision.so "${OpenCV_LIBS}")
set_property(TARGET evision.so PROPERTY CXX_STANDARD 14)

set_target_properties(evision.so PROPERTIES
    INSTALL_RPATH_USE_LINK_PATH TRUE
    BUILD_WITH_INSTALL_RPATH TRUE
)

if(UNIX AND NOT APPLE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -shared")
    set_target_properties(evision.so PROPERTIES INSTALL_RPATH "\$ORIGIN/lib")
else()
    # Although the compiler complains about not using these,
    # things only work with them set
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -undefined dynamic_lookup -dynamiclib")
    set(CMAKE_SHARED_LINKER_FLAGS "-bundle -flat_namespace -undefined suppress")
    set_target_properties(evision.so PROPERTIES INSTALL_RPATH "@loader_path/lib")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -I${ERTS_INCLUDE_DIR} -O3 -Wall -Wextra -Wno-unused-parameter -Wno-missing-field-initializers")
