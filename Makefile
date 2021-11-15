PRIV_DIR = $(MIX_APP_PATH)/priv
EVISION_SO = $(PRIV_DIR)/evision.so

CMAKE_BUILD_DIR ?= $(MIX_APP_PATH)/cmake
C_SRC = $(shell pwd)/c_src
PY_SRC = $(shell pwd)/py_src

.DEFAULT_GLOBAL := build

build: $(EVISION_SO)

$(EVISION_SO):
	@ mkdir -p $(PRIV_DIR)
	@ mkdir -p $(CMAKE_BUILD_DIR)
	@ cd $(CMAKE_BUILD_DIR) && \
		cmake -DC_SRC=$(C_SRC) -DPY_SRC=$(PY_SRC) -DPRIV_DIR=$(PRIV_DIR) \
      	-DERTS_INCLUDE_DIR=$(ERTS_INCLUDE_DIR) -S $(shell pwd) && \
		cmake --build . $(CMAKE_BUILD_FLAGS)
	@ mv $(CMAKE_BUILD_DIR)/evision.so $(EVISION_SO)
