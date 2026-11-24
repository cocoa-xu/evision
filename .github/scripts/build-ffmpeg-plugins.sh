#!/usr/bin/env bash
# Build OpenCV's FFmpeg videoio plugin once per FFmpeg major into PRIV_DIR/lib.
# Each plugin links against a cocoa-xu/ffmpeg-build skeleton, so at runtime it
# loads only where the system has that FFmpeg major; OpenCV tries the plugins
# and skips any whose libraries are missing.
#
# usage: build-ffmpeg-plugins.sh TRIPLET PRIV_DIR [CMAKE_TOOLCHAIN_FILE]
set -euo pipefail

triplet="$1"
priv="$(cd "$2" && pwd)"
toolchain="${3:-}"

# The first release of each library major: a plugin built against X.0 loads with every X.y.
versions="4.0.6 5.0.3 6.0.1 7.0.3 8.0.3 9.0.2"

opencv_src="$(pwd)/3rd_party/opencv/opencv-${OPENCV_VER}"
work="$(mktemp -d)"
trap 'rm -rf "${work}"' EXIT

cmake_args=(
  -D CMAKE_BUILD_TYPE=Release
  -D CMAKE_CXX_COMPILER_LAUNCHER=ccache
  -D OpenCV_DIR="${priv}/lib/cmake/opencv5"
  -D OPENCV_PLUGIN_DESTINATION="${priv}/lib"
  -D CMAKE_BUILD_WITH_INSTALL_RPATH=ON
  -D CMAKE_INSTALL_RPATH='$ORIGIN'
)
if [ -n "${toolchain}" ]; then
  # Cross toolchains search only their sysroot, which has neither the skeleton nor OpenCV.
  cat > "${work}/toolchain.cmake" <<EOF
include("${toolchain}")
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE BOTH)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY BOTH)
EOF
  cmake_args+=(-D CMAKE_TOOLCHAIN_FILE="${work}/toolchain.cmake")
fi

for version in ${versions}; do
  archive="ffmpeg-skeleton-${triplet}.tar.gz"
  url="https://github.com/cocoa-xu/ffmpeg-build/releases/download/v${version}/${archive}"
  curl -fsSL --retry 3 "${url}" -o "${work}/${archive}"
  echo "$(curl -fsSL --retry 3 "${url}.sha256" | cut -d' ' -f1)  ${work}/${archive}" | sha256sum -c -
  mkdir "${work}/${version}"
  tar -xzf "${work}/${archive}" -C "${work}/${version}"

  export PKG_CONFIG_LIBDIR="${work}/${version}/lib/pkgconfig"
  major="$(pkg-config --modversion libavcodec | cut -d. -f1)"
  cmake -S "${opencv_src}/modules/videoio/misc/plugin_ffmpeg" -B "${work}/build-${version}" \
    "${cmake_args[@]}" -D OPENCV_PLUGIN_NAME="opencv_videoio_ffmpeg_avcodec${major}"
  cmake --build "${work}/build-${version}" -j"$(nproc)"
done

ls -l "${priv}"/lib/libopencv_videoio_ffmpeg_avcodec*.so
