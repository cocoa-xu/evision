PRIV_DIR = "$(MIX_APP_PATH)/priv"
EVISION_SO = "$(PRIV_DIR)/evision.so"

CMAKE_EVISION_BUILD_DIR ?= "$(MIX_APP_PATH)/cmake_evision"
CMAKE_OPENCV_BUILD_DIR ?= "$(MIX_APP_PATH)/cmake_opencv"
C_SRC = "$(shell pwd)/c_src"
PY_SRC = "$(shell pwd)/py_src"
OPENCV_VER ?= 4.5.4
OPENCV_DIR = "$(shell pwd)/3rd_party/opencv"
OPENCV_CONTRIB_DIR = "$(shell pwd)/3rd_party/opencv_contrib"

.DEFAULT_GLOBAL := build

build: $(EVISION_SO)

$(EVISION_SO):
	@ mkdir -p "$(PRIV_DIR)" "$(CMAKE_EVISION_BUILD_DIR)" "$(CMAKE_OPENCV_BUILD_DIR)"
	@ git submodule update --init --recursive
	@ pushd "$(OPENCV_DIR)" && git checkout "tags/${OPENCV_VER}" && popd
	@ pushd "$(OPENCV_CONTRIB_DIR)" && git checkout "tags/${OPENCV_VER}" && popd
	@ cd "$(CMAKE_OPENCV_BUILD_DIR)" && \
		cmake -S "$(OPENCV_DIR)" \
		-D CMAKE_BUILD_TYPE=RELEASE \
		-D CMAKE_INSTALL_PREFIX="$(PRIV_DIR)" \
		-D OPENCV_EXTRA_MODULES_PATH="$(OPENCV_CONTRIB_DIR)/modules" \
		-D PYTHON3_LIBRARY="$(python3 $(PY_SRC)/find_python3_lib.py)" \
		-D PYTHON3_INCLUDE_DIR="$(python3 -c 'import distutils.sysconfig as s; print(s.get_python_inc())')" \
		-D PYTHON3_EXECUTABLE="$(which python3)" \
		-D BUILD_opencv_python2=OFF \
		-D BUILD_opencv_python3=ON \
		-D INSTALL_PYTHON_EXAMPLES=OFF \
		-D INSTALL_C_EXAMPLES=OFF \
		-D OPENCV_ENABLE_NONFREE=OFF \
		-D OPENCV_GENERATE_PKGCONFIG=ON \
		-D OPENCV_PC_FILE_NAME=opencv4.pc \
		-D BUILD_EXAMPLES=OFF \
		-D BUILD_TESTS=OFF && \
		cmake --build . --config Release "$(CMAKE_BUILD_FLAGS)" && \
		cmake --install . --config Release
	@ cp "$(CMAKE_OPENCV_BUILD_DIR)/modules/python_bindings_generator/headers.txt" "$(C_SRC)/headers.txt" && \
		cp "$(CMAKE_OPENCV_BUILD_DIR)/modules/python_bindings_generator/pyopencv_custom_headers.h" "$(C_SRC)/evision_custom_headers.h" && \
		cp "$(OPENCV_DIR)/modules/core/include/opencv2/utils/configuration.private.hpp" "$(C_SRC)/configuration.private.hpp"
	@ cd "$(CMAKE_EVISION_BUILD_DIR)" && \
		cmake -DC_SRC="$(C_SRC)" -DPY_SRC="$(PY_SRC)" -DPRIV_DIR="$(PRIV_DIR)" \
      	-DERTS_INCLUDE_DIR="$(ERTS_INCLUDE_DIR)" -S "$(shell pwd)" && \
		cmake --build . "$(CMAKE_BUILD_FLAGS)"
	@ mv "$(CMAKE_EVISION_BUILD_DIR)/evision.so" "$(EVISION_SO)"
