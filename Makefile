PRIV_DIR = $(MIX_APP_PATH)/priv
EVISION_SO = $(PRIV_DIR)/evision.so

OPENCV_INCLUDE_PATH = $(OPENCV_PREFIX_DIR)/include/opencv4
OPENCV_LIB_PATH = $(OPENCV_PREFIX_DIR)/lib

CMAKE_BUILD_DIR ?= $(MIX_APP_PATH)/cmake
C_SRC = $(shell pwd)/c_src

.DEFAULT_GLOBAL := build

build: $(EVISION_SO)

$(EVISION_SO):
	@ mkdir -p $(PRIV_DIR)
	@ mkdir -p $(CMAKE_BUILD_DIR)
	@ find $(OPENCV_INCLUDE_PATH) -type f > $(C_SRC)/opencv_hdr.txt
	@ python3 py_src/gen2.py $(C_SRC) $(C_SRC)/opencv_hdr.txt
	@ ln -sf $(abspath $(OPENCV_LIB_PATH)) $(PRIV_DIR)/evision
	@ cd $(CMAKE_BUILD_DIR) && \
		cmake -DC_SRC=$(C_SRC) \
      	-DERTS_INCLUDE_DIR=$(ERTS_INCLUDE_DIR) -S $(shell pwd) && \
		cmake --build . $(CMAKE_BUILD_FLAGS)
	@ mv $(CMAKE_BUILD_DIR)/evision.so $(EVISION_SO)
