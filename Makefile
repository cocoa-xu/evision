ifndef MIX_APP_PATH
	MIX_APP_PATH=$(shell pwd)
endif

PRIV_DIR = $(MIX_APP_PATH)/priv
OPENCV_CONFIG_CMAKE = $(PRIV_DIR)/lib/cmake/opencv4/OpenCVConfig.cmake
EVISION_SO = $(PRIV_DIR)/evision.so
WINDOWS_FIX_SO = $(PRIV_DIR)/windows_fix.so
SRC = $(shell pwd)/src
C_SRC = $(shell pwd)/c_src
PY_SRC = $(shell pwd)/py_src
LIB_SRC = $(shell pwd)/lib
GLEAM_SRC = $(shell pwd)/gleam_src
SCRIPTS = $(shell pwd)/scripts
ifdef CMAKE_TOOLCHAIN_FILE
	CMAKE_CONFIGURE_FLAGS=-D CMAKE_TOOLCHAIN_FILE="$(CMAKE_TOOLCHAIN_FILE)"
endif

CMAKE_BUILD_TYPE ?= Release
# OpenCV
OPENCV_USE_GIT_HEAD ?= false
OPENCV_GIT_REPO ?= "https://github.com/opencv/opencv.git"
OPENCV_VER ?= 4.10.0
ifneq ($(OPENCV_USE_GIT_HEAD), false)
	OPENCV_VER=$(OPENCV_USE_GIT_BRANCH)
endif
OPENCV_CONTRIB_USE_GIT_HEAD ?= false
OPENCV_CONTRIB_GIT_REPO ?= "https://github.com/opencv/opencv_contrib.git"
OPENCV_CONTRIB_VER ?= 4.10.0
ifneq ($(OPENCV_CONTRIB_USE_GIT_HEAD), false)
	OPENCV_CONTRIB_VER=$(OPENCV_CONTRIB_USE_GIT_BRANCH)
endif
OPENCV_CACHE_DIR = $(shell pwd)/3rd_party/cache
OPENCV_ROOT_DIR = $(shell pwd)/3rd_party/opencv
OPENCV_DIR = $(OPENCV_ROOT_DIR)/opencv-$(OPENCV_VER)
OPENCV_CONTRIB_DIR = $(OPENCV_ROOT_DIR)/opencv_contrib-$(OPENCV_CONTRIB_VER)
OPENCV_CONFIGURATION_PRIVATE_HPP = $(OPENCV_DIR)/modules/core/include/opencv2/core/utils/configuration.private.hpp
PYTHON3_EXECUTABLE = $(shell which python3)
CMAKE_OPENCV_BUILD_DIR = $(MIX_APP_PATH)/cmake_opencv_$(OPENCV_VER)
CMAKE_OPENCV_MODULE_SELECTION ?= -D BUILD_opencv_python2=OFF \
-D BUILD_opencv_python3=OFF \
-D BUILD_opencv_gapi=OFF
CMAKE_OPENCV_IMG_CODER_SELECTION ?= -D BUILD_PNG=ON \
-D BUILD_JPEG=ON \
-D BUILD_TIFF=ON \
-D BUILD_WEBP=ON \
-D BUILD_OPENJPEG=ON \
-D BUILD_JASPER=ON \
-D BUILD_OPENEXR=ON
DEFAULT_JOBS ?= $(shell erl -noshell -eval "io:format('~p~n',[erlang:system_info(logical_processors_online)]), halt().")
CMAKE_OPENCV_OPTIONS ?= ""
CMAKE_OPTIONS ?= $(CMAKE_OPENCV_MODULE_SELECTION)
ENABLED_CV_MODULES ?= ""

# ------ options for opencv_contrib start ------
EVISION_ENABLE_CONTRIB ?= true
ifeq ($(EVISION_ENABLE_CONTRIB),true)
	CMAKE_OPTIONS += -DOPENCV_EXTRA_MODULES_PATH="$(OPENCV_CONTRIB_DIR)/modules" -D BUILD_opencv_hdf=OFF -D BUILD_opencv_freetype=OFF -D BUILD_opencv_sfm=OFF
	C_SRC_HEADERS_TXT = "$(C_SRC)/headers-contrib.txt"
	HEADERS_TXT = $(CMAKE_OPENCV_BUILD_DIR)/modules/python_bindings_generator/headers-contrib.txt
else
	C_SRC_HEADERS_TXT = "$(C_SRC)/headers.txt"
	HEADERS_TXT = $(CMAKE_OPENCV_BUILD_DIR)/modules/python_bindings_generator/headers.txt
endif
EVISION_ENABLE_CUDA ?= false
# ------ options for opencv_contrib end ------

# ------ Apple device specific options start ------
APPLE_DEVICE_COMMON_OPTIONS = -DAPPLE_FRAMEWORK=ON -DOPENCV_INCLUDE_INSTALL_PATH=include \
	-DOPENCV_3P_LIB_INSTALL_PATH=lib/3rdparty -DFRAMEWORK_NAME=opencv2 -DCPU_BASELINE=DETECT \
	-DCMAKE_C_FLAGS=-fembed-bitcode -DCMAKE_CXX_FLAGS=-fembed-bitcode \
	-DBUILD_opencv_objc=OFF -DBUILD_ZLIB=OFF -DWITH_ADE=ON -D BUILD_opencv_gapi=ON \
	-DBUILD_opencv_wechat_qrcode=OFF -G Xcode
BUILD_FOR_APPLE_DEVICE = false
IPHONEOS_DEPLOYMENT_TARGET ?= 12.0
XROS_DEPLOYMENT_TARGET ?= 1.0
IPHONEOS_ARCH ?= arm64
VISIONOS_ARCH ?= arm64
ifeq ($(MIX_TARGET), ios)
CMAKE_OPENCV_OPTIONS += $(APPLE_DEVICE_COMMON_OPTIONS) \
	-DIOS_ARCH=$(IPHONEOS_ARCH) \
	-DCMAKE_TOOLCHAIN_FILE=$(OPENCV_DIR)/platforms/ios/cmake/Toolchains/Toolchain-iPhoneOS_Xcode.cmake \
	-DIPHONEOS_DEPLOYMENT_TARGET=$(IPHONEOS_DEPLOYMENT_TARGET) \
	-D BUILD_PNG=ON -D BUILD_JPEG=ON -D BUILD_TIFF=ON -D BUILD_WEBP=ON -D BUILD_OPENJPEG=ON -D BUILD_JASPER=ON -D BUILD_OPENEXR=ON
DEFAULT_JOBS = $(shell sysctl -n hw.ncpu)
CMAKE_OPENCV_IMG_CODER_SELECTION = ""
BUILD_FOR_APPLE_DEVICE = true
else ifeq ($(MIX_TARGET), xros)
CMAKE_OPENCV_OPTIONS += $(APPLE_DEVICE_COMMON_OPTIONS) \
	-DVISIONOS_ARCH=$(VISIONOS_ARCH) \
	-DCMAKE_TOOLCHAIN_FILE=$(OPENCV_DIR)/platforms/ios/cmake/Toolchains/Toolchain-XROS_Xcode.cmake \
	-DXROS_DEPLOYMENT_TARGET=$(XROS_DEPLOYMENT_TARGET) \
	-D WITH_PNG=OFF -D BUILD_PNG=OFF \
	-D WITH_JPEG=OFF -D BUILD_JPEG=OFF -D BUILD_OPENJPEG=OFF \
	-D WITH_TIFF=OFF -D BUILD_TIFF=OFF \
	-D WITH_WEBP=OFF -D BUILD_WEBP=OFF \
	-D WITH_JASPER=OFF -D BUILD_JASPER=OFF \
	-D WITH_OPENEXR=OFF -D BUILD_OPENEXR=OFF
DEFAULT_JOBS = $(shell sysctl -n hw.ncpu)
CMAKE_OPENCV_IMG_CODER_SELECTION = ""
BUILD_FOR_APPLE_DEVICE = true
else
CMAKE_OPTIONS += $(CMAKE_OPENCV_IMG_CODER_SELECTION)
endif
# ------ Apple device specific options end ------

CMAKE_OPTIONS += $(CMAKE_CONFIGURE_FLAGS) $(CMAKE_OPENCV_OPTIONS)
ifdef TARGET_GCC_FLAGS
    CMAKE_OPTIONS += -DCMAKE_CXX_FLAGS="$(TARGET_GCC_FLAGS)" -DCMAKE_C_FLAGS="$(TARGET_GCC_FLAGS)"
endif

# evision
OPENCV_HEADERS_TXT = $(CMAKE_OPENCV_BUILD_DIR)/modules/python_bindings_generator/headers.txt
CONFIGURATION_PRIVATE_HPP = $(C_SRC)/configuration.private.hpp
GENERATED_ELIXIR_SRC_DIR = $(LIB_SRC)/generated
GENERATED_ERLANG_SRC_DIR = $(SRC)/generated
GENERATED_GLEAM_SRC_DIR = $(GLEAM_SRC)
CMAKE_EVISION_BUILD_DIR = $(MIX_APP_PATH)/cmake_evision
CMAKE_EVISION_OPTIONS ?= ""
ifeq ($(MIX_TARGET), ios)
	CMAKE_EVISION_OPTIONS += -DCMAKE_OSX_SYSROOT=iphoneos -DSYSTEM=iOS -DARCH=$(IPHONEOS_ARCH)
else ifeq ($(MIX_TARGET), xros)
	CMAKE_EVISION_OPTIONS += -DCMAKE_OSX_SYSROOT=xros -DSYSTEM=visionOS -DARCH=$(VISIONOS_ARCH)
endif
ifdef TARGET_GCC_FLAGS
    CMAKE_EVISION_OPTIONS += -DCMAKE_CXX_FLAGS="$(TARGET_GCC_FLAGS)" -DCMAKE_C_FLAGS="$(TARGET_GCC_FLAGS)"
endif
ifdef CMAKE_TOOLCHAIN_FILE
    CMAKE_EVISION_OPTIONS += -D CMAKE_TOOLCHAIN_FILE="$(CMAKE_TOOLCHAIN_FILE)"
endif
MAKE_BUILD_FLAGS ?= -j$(DEFAULT_JOBS)
EVISION_GENERATE_LANG ?= elixir
EVISION_PREFER_PRECOMPILED ?= false
EVISION_COMPILE_WITH_REBAR ?= false
ifeq ($(EVISION_COMPILE_WITH_REBAR), true)
	EVISION_GENERATE_LANG = elixir,erlang
endif
EVISION_PRECOMPILED_CACHE_DIR ?= $(shell pwd)/.cache
EVISION_MAKE ?= make
GLEAM_EVISION ?= false

.DEFAULT_GLOBAL := build

build: $(EVISION_SO)
	@echo > /dev/null

download_opencv_contrib:
	@ if [ "$(EVISION_PREFER_PRECOMPILED)" != "true" ]; then \
		if [ "$(EVISION_ENABLE_CONTRIB)" = "true" ]; then \
			if [ ! -e "$(OPENCV_CONTRIB_DIR)/modules" ]; then \
				if [ "$(OPENCV_CONTRIB_USE_GIT_HEAD)" = "false" ]; then \
					echo "using opencv_contrib $(OPENCV_CONTRIB_VER)" ; \
					bash scripts/download_opencv_contrib.sh $(OPENCV_CONTRIB_VER) "$(OPENCV_CACHE_DIR)" "$(OPENCV_ROOT_DIR)" ; \
				else \
					rm -rf "$(OPENCV_CONTRIB_DIR)" ; \
					git clone --branch=$(OPENCV_CONTRIB_USE_GIT_BRANCH) --depth=1 $(OPENCV_CONTRIB_GIT_REPO) "$(OPENCV_CONTRIB_DIR)" ; \
				fi \
			fi \
		fi \
	fi

$(OPENCV_CONFIGURATION_PRIVATE_HPP): download_opencv_contrib
	@ if [ "$(EVISION_PREFER_PRECOMPILED)" != "true" ]; then \
   		if [ ! -e "$(OPENCV_CONFIGURATION_PRIVATE_HPP)" ]; then \
			if [ "$(OPENCV_USE_GIT_HEAD)" = "false" ]; then \
				echo "using opencv $(OPENCV_VER)" ; \
				bash $(SCRIPTS)/download_opencv.sh $(OPENCV_VER) "$(OPENCV_CACHE_DIR)" "$(OPENCV_ROOT_DIR)"; \
			else \
		  		rm -rf "$(OPENCV_DIR)" ; \
				git clone --branch=$(OPENCV_USE_GIT_BRANCH) --depth=1 $(OPENCV_GIT_REPO) "$(OPENCV_DIR)" ; \
			fi \
		fi \
	fi

$(CONFIGURATION_PRIVATE_HPP): $(OPENCV_CONFIGURATION_PRIVATE_HPP)
	@ if [ "$(EVISION_PREFER_PRECOMPILED)" != "true" ]; then \
   		cp "$(OPENCV_CONFIGURATION_PRIVATE_HPP)" "$(CONFIGURATION_PRIVATE_HPP)" ; \
	fi

$(HEADERS_TXT): $(CONFIGURATION_PRIVATE_HPP)
	@ if [ "$(EVISION_PREFER_PRECOMPILED)" != "true" ]; then \
		python3 "$(shell pwd)/patches/apply_patch.py" "$(OPENCV_DIR)" "$(OPENCV_VER)" ; \
		mkdir -p "$(CMAKE_OPENCV_BUILD_DIR)" && \
		cd "$(CMAKE_OPENCV_BUILD_DIR)" && \
		if [ "$(MIX_TARGET)" = "ios" ]; then \
			export IPHONEOS_DEPLOYMENT_TARGET=$(IPHONEOS_DEPLOYMENT_TARGET) ; \
		elif [ "$(MIX_TARGET)" = "xros" ]; then \
			export XROS_DEPLOYMENT_TARGET=$(XROS_DEPLOYMENT_TARGET) ; \
		fi && \
		echo "CMAKE_OPTIONS: $(CMAKE_OPTIONS)" && \
		cmake -D CMAKE_BUILD_TYPE="$(CMAKE_BUILD_TYPE)" \
			-D CMAKE_INSTALL_PREFIX="$(PRIV_DIR)" \
			-D PYTHON3_EXECUTABLE="$(PYTHON3_EXECUTABLE)" \
			-D INSTALL_PYTHON_EXAMPLES=OFF \
			-D INSTALL_C_EXAMPLES=OFF \
			-D BUILD_EXAMPLES=OFF \
			-D BUILD_TESTS=OFF \
			-D BUILD_PERF_TESTS=OFF \
			-D OPENCV_ENABLE_NONFREE=OFF \
			-D OPENCV_GENERATE_PKGCONFIG=ON \
			-D OPENCV_PC_FILE_NAME=opencv4.pc \
			-D BUILD_ZLIB=ON \
			-D BUILD_opencv_gapi=OFF \
			-D BUILD_opencv_apps=OFF \
			-D BUILD_opencv_java=OFF \
			$(CMAKE_OPTIONS) "$(OPENCV_DIR)" && \
		if [ "$(MIX_TARGET)" = "ios" ]; then \
			xcodebuild BITCODE_GENERATION_MODE=bitcode IPHONEOS_DEPLOYMENT_TARGET=$(IPHONEOS_DEPLOYMENT_TARGET) ARCHS=$(IPHONEOS_ARCH) -sdk iphoneos -configuration $(CMAKE_BUILD_TYPE) -parallelizeTargets -jobs $(DEFAULT_JOBS) -target ALL_BUILD build ; \
			cmake -DBUILD_TYPE=$(CMAKE_BUILD_TYPE) -P cmake_install.cmake ; \
		elif [ "$(MIX_TARGET)" = "xros" ]; then \
			xcodebuild BITCODE_GENERATION_MODE=bitcode XROS_DEPLOYMENT_TARGET=$(XROS_DEPLOYMENT_TARGET) ARCHS=$(VISIONOS_ARCH) -sdk visionos -configuration $(CMAKE_BUILD_TYPE) -parallelizeTargets -jobs $(DEFAULT_JOBS) -target ALL_BUILD build ; \
			cmake -DBUILD_TYPE=$(CMAKE_BUILD_TYPE) -P cmake_install.cmake ; \
		else \
			make -j$(DEFAULT_JOBS) ; \
		fi && \
		if [ "$(OPENCV_HEADERS_TXT)" != "$(HEADERS_TXT)" ]; then \
			cp -f "$(OPENCV_HEADERS_TXT)" "$(HEADERS_TXT)" ; \
		fi \
	fi

$(C_SRC_HEADERS_TXT): $(HEADERS_TXT)
	@ if [ "$(EVISION_PREFER_PRECOMPILED)" != "true" ]; then \
		cp -f "$(HEADERS_TXT)" "$(C_SRC_HEADERS_TXT)" ; \
	fi

$(OPENCV_CONFIG_CMAKE):
	@cd "$(CMAKE_OPENCV_BUILD_DIR)" && make install

opencv: $(C_SRC_HEADERS_TXT)
	@echo > /dev/null

$(EVISION_SO): $(C_SRC_HEADERS_TXT) $(OPENCV_CONFIG_CMAKE)
	@ mkdir -p "$(EVISION_PRECOMPILED_CACHE_DIR)"
	@ mkdir -p "$(PRIV_DIR)"
	@ mkdir -p "$(GENERATED_ELIXIR_SRC_DIR)"
	@ mkdir -p "$(GENERATED_ERLANG_SRC_DIR)"
	@ if [ "$(EVISION_PREFER_PRECOMPILED)" = "true" ] && [ "$(EVISION_COMPILE_WITH_REBAR)" = "true" ]; then \
		{ \
			erlc checksum.erl && \
			erlc evision_precompiled.erl && \
			erl -noshell -s evision_precompiled install_precompiled_binary_if_available -s init stop ; } || \
		{ \
		 	DEFAULT_JOBS=$(shell erl -noshell -eval "io:format('~p~n',[erlang:system_info(logical_processors_online)]), halt().") \
			$(EVISION_MAKE) EVISION_PREFER_PRECOMPILED=false EVISION_COMPILE_WITH_REBAR=true ; } ; \
	fi
	@ if [ ! -f "${EVISION_SO}" ]; then \
		mkdir -p "$(CMAKE_EVISION_BUILD_DIR)" && \
		cd "$(CMAKE_EVISION_BUILD_DIR)" && \
			{ cmake --no-warn-unused-cli \
			-D C_SRC="$(C_SRC)" \
			-D GENERATED_ELIXIR_SRC_DIR="$(GENERATED_ELIXIR_SRC_DIR)" \
			-D GENERATED_ERLANG_SRC_DIR="$(GENERATED_ERLANG_SRC_DIR)" \
			-D GENERATED_GLEAM_SRC_DIR="$(GENERATED_GLEAM_SRC_DIR)" \
			-D PY_SRC="$(PY_SRC)" \
			-D MIX_APP_PATH="$(MIX_APP_PATH)" \
			-D PRIV_DIR="$(PRIV_DIR)" \
			-D ERTS_INCLUDE_DIR="$(ERTS_INCLUDE_DIR)" \
			-D ENABLED_CV_MODULES=$(ENABLED_CV_MODULES) \
			-D EVISION_GENERATE_LANG="$(EVISION_GENERATE_LANG)" \
			-D EVISION_ENABLE_CONTRIB="$(EVISION_ENABLE_CONTRIB)" \
			-D EVISION_ENABLE_CUDA="$(EVISION_ENABLE_CUDA)" \
			-D GLEAM_EVISION="$(GLEAM_EVISION)" \
			$(CMAKE_CONFIGURE_FLAGS) $(CMAKE_EVISION_OPTIONS) "$(shell pwd)" && \
			make "-j$(DEFAULT_JOBS)" \
			|| { echo "\033[0;31mincomplete build of OpenCV found in '$(CMAKE_OPENCV_BUILD_DIR)', please delete that directory and retry\033[0m" && exit 1 ; } ; } \
			&& if [ "$(EVISION_PREFER_PRECOMPILED)" != "true" ]; then \
				cp "$(CMAKE_EVISION_BUILD_DIR)/evision.so" "$(EVISION_SO)" ; \
			fi && \
			if [ "$(MIX_TARGET)" = "ios" ]; then \
				rm -rf "$(PRIV_DIR)/lib" "$(PRIV_DIR)/include" ; \
			elif [ "$(MIX_TARGET)" = "xros" ]; then \
				rm -rf "$(PRIV_DIR)/lib" "$(PRIV_DIR)/include" ; \
			fi ; \
		fi

clean_dev:
	@echo "rm -rf src/generated"
	@echo "rm -rf lib/generated"
	@echo "rm -f $(C_SRC)/headers.txt"
	@echo "rm -f $(C_SRC)/headers-contrib.txt"
	@echo "rm -rf _build/dev/lib/evision"
