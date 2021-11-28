PRIV_DIR = $(MIX_APP_PATH)/priv
EVISION_SO = $(PRIV_DIR)/evision.so

CMAKE_EVISION_BUILD_DIR = $(MIX_APP_PATH)/cmake_evision
CMAKE_OPENCV_BUILD_DIR = $(MIX_APP_PATH)/cmake_opencv
CMAKE_BUILD_FLAGS ?= "-j1"
C_SRC = $(shell pwd)/c_src
PY_SRC = $(shell pwd)/py_src
LIB_SRC = $(shell pwd)/lib
OPENCV_VER ?= 4.5.4
OPENCV_DIR = $(shell pwd)/3rd_party/opencv
OPENCV_CONTRIB_DIR = $(shell pwd)/3rd_party/opencv_contrib
PYTHON3_LIBRARY = $(shell python3 ${PY_SRC}/find_python3_lib.py)
PYTHON3_INCLUDE_DIR = $(shell python3 -c 'import distutils.sysconfig as s; print(s.get_python_inc())')
PYTHON3_EXECUTABLE = $(shell which python3)
HEADERS_TXT = $(CMAKE_OPENCV_BUILD_DIR)/modules/python_bindings_generator/headers.txt
CONFIGURATION_PRIVATE_HPP = $(C_SRC)/configuration.private.hpp
.DEFAULT_GLOBAL := build

build: $(EVISION_SO)

$(CONFIGURATION_PRIVATE_HPP):
	@ git submodule update --init --recursive
	@ cp "$(OPENCV_DIR)/modules/core/include/opencv2/core/utils/configuration.private.hpp" "$(C_SRC)/configuration.private.hpp"

$(HEADERS_TXT):
	@ mkdir -p $(CMAKE_OPENCV_BUILD_DIR)
	@ git submodule update --init --recursive
	@ cd $(OPENCV_DIR) && git checkout "tags/${OPENCV_VER}"
	@ cd $(OPENCV_CONTRIB_DIR) && git checkout "tags/${OPENCV_VER}"
	@ cd $(CMAKE_OPENCV_BUILD_DIR) && \
	 	cmake -S $(OPENCV_DIR) \
	 	-D CMAKE_BUILD_TYPE=RELEASE \
	 	-D CMAKE_INSTALL_PREFIX=$(PRIV_DIR) \
	 	-D PYTHON3_EXECUTABLE=$(PYTHON3_EXECUTABLE) \
	 	-D BUILD_opencv_python2=OFF \
	 	-D BUILD_opencv_python3=OFF \
	 	-D BUILD_opencv_dnn=OFF \
	 	-D BUILD_opencv_gapi=OFF \
	 	-D INSTALL_PYTHON_EXAMPLES=OFF \
	 	-D INSTALL_C_EXAMPLES=OFF \
	 	-D OPENCV_ENABLE_NONFREE=OFF \
	 	-D OPENCV_GENERATE_PKGCONFIG=ON \
	 	-D OPENCV_PC_FILE_NAME=opencv4.pc \
	 	-D BUILD_EXAMPLES=OFF \
	 	-D BUILD_TESTS=OFF && \
	 	cmake --build . --config Release "$(CMAKE_BUILD_FLAGS)" && \
	 	cmake --install . --config Release

$(EVISION_SO): $(CONFIGURATION_PRIVATE_HPP) $(HEADERS_TXT)
	@ mkdir -p $(PRIV_DIR)
	@ mkdir -p $(CMAKE_EVISION_BUILD_DIR)
	@ cp "$(CMAKE_OPENCV_BUILD_DIR)/modules/python_bindings_generator/headers.txt" "$(C_SRC)/headers.txt"
	@ cd "$(CMAKE_EVISION_BUILD_DIR)" && \
		cmake -DC_SRC="$(C_SRC)" -DLIB_SRC="$(LIB_SRC)" \
		-DPY_SRC="$(PY_SRC)" -DPRIV_DIR="$(PRIV_DIR)" \
		-DERTS_INCLUDE_DIR="$(ERTS_INCLUDE_DIR)" -S "$(shell pwd)" && \
		cmake --build . "$(CMAKE_BUILD_FLAGS)"
	@ mv "$(CMAKE_EVISION_BUILD_DIR)/evision.so" "$(EVISION_SO)"
