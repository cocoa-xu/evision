#!/bin/sh
# Build a CSP-clean opencv.js from OpenCV source.
#
# "CSP-clean" means the artifact runs under a Content-Security-Policy of
# `script-src 'wasm-unsafe-eval'` with NO `'unsafe-eval'`. Stock
# docs.opencv.org/<ver>/opencv.js needs `'unsafe-eval'` because embind's
# createNamedFunction/craftInvokerFunction build invokers with `new Function`.
# Emscripten `-s DYNAMIC_EXECUTION=0` compiles those string->code paths out, and
# `-s EMBIND_AOT=1` (on by default) regenerates the invokers ahead of time so the
# eval-free path is also fast. Neither changes the exported JS API.
#
# The build is otherwise identical to the stock recipe: single-threaded
# (USE_PTHREADS=0), single-file WASM, and OpenCV's own default
# platforms/js/opencv_js.config.py whitelist. Keeping every other input
# equal is deliberate: evision's `@js` correspondence table is generated
# from that same whitelist, so the exported JS API must not drift.
#
# Requires an activated Emscripten SDK (emcc on PATH).
#
# Usage: scripts/build_opencv_js.sh <opencv_ver> <opencv_src_dir> <out_dir>

set -eu

OPENCV_VER="${1:?usage: build_opencv_js.sh <opencv_ver> <opencv_src_dir> <out_dir>}"
OPENCV_DIR="${2:?opencv source directory (contains platforms/js/build_js.py)}"
OUT_DIR="${3:?output directory}"

if ! command -v emcmake >/dev/null 2>&1; then
  echo "emcmake not found on PATH; activate the Emscripten SDK first (source emsdk_env.sh)" >&2
  exit 1
fi

if ! command -v python3 >/dev/null 2>&1; then
  echo "python3 not found on PATH (build_js.py needs it)" >&2
  exit 1
fi

if [ ! -f "${OPENCV_DIR}/platforms/js/build_js.py" ]; then
  echo "no build_js.py under ${OPENCV_DIR}/platforms/js" >&2
  exit 1
fi

BUILD_DIR="${OUT_DIR}/build_js_${OPENCV_VER}"
mkdir -p "${OUT_DIR}"

echo "emcc: $(emcc --version | head -1)"
echo "building opencv.js ${OPENCV_VER} (CSP-clean: -s DYNAMIC_EXECUTION=0)"

# CSP-clean core: -s DYNAMIC_EXECUTION=0 compiles out embind's eval/new Function
# invokers; embind falls back to an eval-free path (correct, but the JS<->wasm
# invoker layer is ~8-10x slower for hot calls; the OpenCV compute itself is
# unaffected). This is the only flag routed through --build_flags, because
# build_js.py bakes --build_flags into CMAKE_CXX_FLAGS -- i.e. into every compiler
# probe CMake runs at configure time -- and DYNAMIC_EXECUTION=0 is inert there.
CONFIG_FLAGS="-s DYNAMIC_EXECUTION=0"

# -s EMBIND_AOT=1 regenerates embind's invokers at build time so they are eval-free
# AND fast (the DYNAMIC_EXECUTION=0-only path above is correct but slow). It is ON
# by default; set OPENCV_JS_EMBIND_AOT=0 to fall back to the plain build.
#   It must NOT reach the configure step. OpenCV's compiler probes link bare
# `int main(){}` executables with no embind registrations, and EMBIND_AOT's
# link-time codegen (emscripten link.py phase_embind_aot) aborts on those with a
# missing embind_generated_output.js -- so every probe "fails" and CMake stops at
# "Compiler doesn't support baseline optimization flags". So we split the build:
# configure without EMBIND_AOT, then inject it via EMCC_CFLAGS for the opencv.js
# link alone, where real embind bindings are present. (AOT can still fail to link
# on some binding shapes -- emscripten #22862; OPENCV_JS_EMBIND_AOT=0 opts out.)
AOT_CFLAGS=""
if [ "${OPENCV_JS_EMBIND_AOT:-1}" = "1" ]; then
  AOT_CFLAGS="-s EMBIND_AOT=1"
fi
echo "configure flags (--build_flags): ${CONFIG_FLAGS}"
echo "opencv.js link-only flags (EMCC_CFLAGS): ${AOT_CFLAGS:-<none>}"

# --build_wasm + defaults reproduce the stock build; the flags above are the only
# delta. Do NOT pass --threads/--simd/--config: single-thread and the default
# whitelist are exactly what the published docs build uses, so the exported JS
# API stays identical to stock and evision's @js table stays valid.
#
# Phase 1 -- configure only (no EMBIND_AOT; see above).
emcmake python3 "${OPENCV_DIR}/platforms/js/build_js.py" "${BUILD_DIR}" \
  --opencv_dir "${OPENCV_DIR}" \
  --build_wasm \
  --build_flags="${CONFIG_FLAGS}" \
  --config_only

# Phase 2 -- build opencv.js, adding EMBIND_AOT via EMCC_CFLAGS so it lands only on
# the final embind link. EMCC_CFLAGS also reaches the object compiles, where a
# linker-only setting is just a harmless -Wunused-command-line-argument. Only the
# opencv.js target is built here, so no embind-less executable is ever linked.
EMCC_CFLAGS="${AOT_CFLAGS}" emcmake python3 "${OPENCV_DIR}/platforms/js/build_js.py" "${BUILD_DIR}" \
  --opencv_dir "${OPENCV_DIR}" \
  --build_wasm \
  --build_flags="${CONFIG_FLAGS}" \
  --skip_config

OPENCV_JS="${BUILD_DIR}/bin/opencv.js"
if [ ! -f "${OPENCV_JS}" ]; then
  OPENCV_JS="$(find "${BUILD_DIR}" -name opencv.js -type f 2>/dev/null | head -1 || true)"
fi
if [ -z "${OPENCV_JS}" ] || [ ! -f "${OPENCV_JS}" ]; then
  echo "build produced no opencv.js under ${BUILD_DIR}" >&2
  exit 1
fi

cp "${OPENCV_JS}" "${OUT_DIR}/opencv.js"
( cd "${OUT_DIR}" && { if command -v sha256sum >/dev/null 2>&1; then sha256sum opencv.js; else shasum -a 256 opencv.js; fi; } > opencv.sha256 )

echo "built: ${OUT_DIR}/opencv.js ($(wc -c < "${OUT_DIR}/opencv.js") bytes)"
cat "${OUT_DIR}/opencv.sha256"
