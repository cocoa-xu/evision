PRIV_DIR = $(MIX_APP_PATH)/priv
EVISION_SO = $(PRIV_DIR)/evision.so
C_SRC = $(shell pwd)/c_src
PY_SRC = $(shell pwd)/py_src
LIB_SRC = $(shell pwd)/lib
ifdef CMAKE_TOOLCHAIN_FILE
	CMAKE_CONFIGURE_FLAGS=-D CMAKE_TOOLCHAIN_FILE="$(CMAKE_TOOLCHAIN_FILE)"
endif

# OpenCV
OPENCV_VER ?= 4.5.4
OPENCV_CACHE_DIR = $(shell pwd)/3rd_party/cache
OPENCV_SOURCE_URL = "https://github.com/opencv/opencv/archive/$(OPENCV_VER).zip"
OPENCV_SOURCE_ZIP = $(OPENCV_CACHE_DIR)/opencv-$(OPENCV_VER).zip
OPENCV_ROOT_DIR = $(shell pwd)/3rd_party/opencv
OPENCV_DIR = $(OPENCV_ROOT_DIR)/opencv-$(OPENCV_VER)
OPENCV_CONFIGURATION_PRIVATE_HPP = $(OPENCV_DIR)/modules/core/include/opencv2/core/utils/configuration.private.hpp
PYTHON3_EXECUTABLE = $(shell which python3)
CMAKE_OPENCV_BUILD_DIR = $(MIX_APP_PATH)/cmake_opencv_$(OPENCV_VER)
CMAKE_OPENCV_MODULE_SELECTION ?= "-D BUILD_opencv_python2=OFF \
-D BUILD_opencv_python3=OFF \
-D BUILD_opencv_dnn=OFF \
-D BUILD_opencv_gapi=OFF "
CMAKE_OPENCV_IMG_CODER_SELECTION ?= "-D BUILD_PNG=ON \
-D BUILD_JPEG=ON \
-D BUILD_TIFF=ON \
-D BUILD_WEBP=ON \
-D BUILD_OPENJPEG=ON \
-D BUILD_JASPER=ON \
-D BUILD_OPENEXR=ON "
CMAKE_OPTIONS ?= "$(CMAKE_OPENCV_MODULE_SELECTION) $(CMAKE_OPENCV_IMG_CODER_SELECTION)"

# evision
HEADERS_TXT = $(CMAKE_OPENCV_BUILD_DIR)/modules/python_bindings_generator/headers.txt
CONFIGURATION_PRIVATE_HPP = $(C_SRC)/configuration.private.hpp
GENERATED_ELIXIR_SRC_DIR = $(LIB_SRC)/generated
CMAKE_EVISION_BUILD_DIR = $(MIX_APP_PATH)/cmake_evision
MAKE_BUILD_FLAGS ?= "-j1"

.DEFAULT_GLOBAL := build

build: $(EVISION_SO)
clean_opencv:
	@ rm -rf "$(OPENCV_DIR)"
	@ rm -rf "$(CMAKE_OPENCV_BUILD_DIR)"
	@ rm -f "$(CONFIGURATION_PRIVATE_HPP)"
clean_evision:
	@ rm -rf "$(CMAKE_EVISION_BUILD_DIR)"
	@ rm -f "$(EVISION_SO)"
clean: clean_opencv clean_evision

# in simple words
# 1. download "https://github.com/opencv/opencv/archive/$(OPENCV_VER).zip" to "3rd_party/cache/opencv-$(OPENCV_VER).zip"
# 2. unzip -o "3rd_party/cache/opencv-$(OPENCV_VER).zip" -d "OPENCV_ROOT_DIR"
#    3rd_party
#    ├── cache
#    │   └── opencv_$(OPENCV_VER).zip
#    └── opencv
#        └── opencv-$(OPENCV_VER)

$(OPENCV_CACHE_DIR):
	@ mkdir -p "$(OPENCV_CACHE_DIR)"

$(OPENCV_SOURCE_ZIP): $(OPENCV_CACHE_DIR)
	@ if [ ! -e "$(OPENCV_SOURCE_ZIP)" ]; then \
		if [ -e "$(shell which curl)" ]; then \
			curl -fSL "$(OPENCV_SOURCE_URL)" -o $(OPENCV_SOURCE_ZIP) ; \
		elif [ -e "$(shell which wget)" ]; then \
			wget "$(OPENCV_SOURCE_URL)" -O $(OPENCV_SOURCE_ZIP) ; \
		else \
			echo "cannot find curl or wget, cannot download opencv source code" ; \
			exit 1 ; \
		fi \
	fi

$(OPENCV_CONFIGURATION_PRIVATE_HPP): $(OPENCV_SOURCE_ZIP)
	@ if [ ! -e "$(OPENCV_CONFIGURATION_PRIVATE_HPP)" ]; then \
		rm -rf "$(OPENCV_DIR)" ; \
		unzip -qq -o "$(OPENCV_SOURCE_ZIP)" -d "$(OPENCV_ROOT_DIR)" ; \
	fi

$(CONFIGURATION_PRIVATE_HPP): $(OPENCV_CONFIGURATION_PRIVATE_HPP)
	@ cp "$(OPENCV_CONFIGURATION_PRIVATE_HPP)" "$(CONFIGURATION_PRIVATE_HPP)"

$(HEADERS_TXT): $(CONFIGURATION_PRIVATE_HPP)
	@ mkdir -p $(CMAKE_OPENCV_BUILD_DIR) && \
	cd $(CMAKE_OPENCV_BUILD_DIR) && \
	cmake -S $(OPENCV_DIR) \
	 	-D CMAKE_BUILD_TYPE=RELEASE \
	 	-D CMAKE_INSTALL_PREFIX=$(PRIV_DIR) \
	 	-D PYTHON3_EXECUTABLE=$(PYTHON3_EXECUTABLE) \
	 	-D INSTALL_PYTHON_EXAMPLES=OFF \
		-D INSTALL_C_EXAMPLES=OFF \
		-D BUILD_EXAMPLES=OFF \
		-D BUILD_TESTS=OFF \
		-D OPENCV_ENABLE_NONFREE=OFF \
		-D OPENCV_GENERATE_PKGCONFIG=ON \
		-D OPENCV_PC_FILE_NAME=opencv4.pc \
		-D CMAKE_TOOLCHAIN_FILE="$(TOOLCHAIN_FILE)" \
	 	$(CMAKE_OPTIONS) $(CMAKE_CONFIGURE_FLAGS) && \
	make "$(MAKE_BUILD_FLAGS)"
	@ cd $(CMAKE_OPENCV_BUILD_DIR) && make install

$(EVISION_SO): $(HEADERS_TXT)
	@ mkdir -p $(PRIV_DIR)
	@ mkdir -p $(CMAKE_EVISION_BUILD_DIR)
	@ cp "$(HEADERS_TXT)" "$(C_SRC)/headers.txt"
	@ mkdir -p "$(GENERATED_ELIXIR_SRC_DIR)"
	@ cd "$(CMAKE_EVISION_BUILD_DIR)" && \
		cmake -D C_SRC="$(C_SRC)" \
		-D CMAKE_TOOLCHAIN_FILE="$(TOOLCHAIN_FILE)" \
		-D GENERATED_ELIXIR_SRC_DIR="$(GENERATED_ELIXIR_SRC_DIR)" \
		-D PY_SRC="$(PY_SRC)" \
		-D PRIV_DIR="$(PRIV_DIR)" \
		-D ERTS_INCLUDE_DIR="$(ERTS_INCLUDE_DIR)" \
		-S "$(shell pwd)" $(CMAKE_CONFIGURE_FLAGS) && \
		make "$(MAKE_BUILD_FLAGS)"
	@ cp "$(CMAKE_EVISION_BUILD_DIR)/evision.so" "$(EVISION_SO)"
