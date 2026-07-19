#!/bin/sh
# Build a CSP-clean opencv.js from OpenCV source.
#
# "CSP-clean" means the artifact runs under a Content-Security-Policy of
# `script-src 'wasm-unsafe-eval'` with NO `'unsafe-eval'`. Stock
# docs.opencv.org/<ver>/opencv.js needs `'unsafe-eval'` because embind's
# createNamedFunction/craftInvokerFunction build invokers with `new Function`.
# Emscripten `-s DYNAMIC_EXECUTION=0` compiles those string->code paths out.
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

# -s DYNAMIC_EXECUTION=0 compiles out embind's eval/new Function invokers; embind
# falls back to an eval-free path (correct, but the JS<->wasm invoker layer is
# ~8-10x slower for hot calls; the OpenCV compute itself is unaffected).
#   Set OPENCV_JS_EMBIND_AOT=1 to also pass -s EMBIND_AOT=1, which regenerates
# those invokers at compile time (eval-free AND fast). AOT can fail to link on
# some binding shapes (emscripten #22862); if it does, rerun without it -- the
# plain build is still fully CSP-clean.
EXTRA_FLAGS="-s DYNAMIC_EXECUTION=0"
if [ "${OPENCV_JS_EMBIND_AOT:-0}" = "1" ]; then
  EXTRA_FLAGS="${EXTRA_FLAGS} -s EMBIND_AOT=1"
fi
echo "extra emscripten flags: ${EXTRA_FLAGS}"

# --build_wasm + defaults reproduce the stock build; EXTRA_FLAGS is the only
# delta. Do NOT pass --threads/--simd/--config: single-thread and the default
# whitelist are exactly what the published docs build uses, so the exported JS
# API stays identical to stock and evision's @js table stays valid.
emcmake python3 "${OPENCV_DIR}/platforms/js/build_js.py" "${BUILD_DIR}" \
  --opencv_dir "${OPENCV_DIR}" \
  --build_wasm \
  --build_flags="${EXTRA_FLAGS}"

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
