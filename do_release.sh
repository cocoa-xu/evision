#!/bin/bash
#
# evision hex.pm release script
# please run this script in the root of the project on an x86_64 Linux machine
set -xe

export MIX_ENV=docs
export EVISION_PREFER_PRECOMPILED=true
export EVISION_ENABLE_CUDA=true
export EVISION_CUDA_VERSION=12
export EVISION_CUDNN_VERSION=9
export NIF_VERSION=2.16
export EVISION_VERSION="$(grep 'def version, do: "' mix.exs | grep -P -oh '(\d+).(\d+).(\d+)')"

export TARBALL_FILENAME="evision-nif_${NIF_VERSION}-x86_64-linux-gnu-contrib-cuda${EVISION_CUDA_VERSION}-cudnn${EVISION_CUDNN_VERSION}-${EVISION_VERSION}"
export TARBALL_NAME="${TARBALL_FILENAME}.tar.gz"
curl -fSL "https://github.com/cocoa-xu/evision/releases/download/v${EVISION_VERSION}/${TARBALL_NAME}" -o "/tmp/${TARBALL_NAME}"
export TARBALL_SHA256="$(sha256sum "/tmp/${TARBALL_NAME}" | cut -d ' ' -f 1)"
cat <<EOF > checksum.exs
%{
  "${TARBALL_NAME}" => "sha256:${TARBALL_SHA256}"
}
EOF
rm -rf release
mkdir -p release
tar xf "/tmp/${TARBALL_NAME}" -C release
rm -rf lib/generated
mv "release/${TARBALL_FILENAME}/elixir_generated" lib/generated
EVISION_FETCH_PRECOMPILED=true mix evision.fetch --all
MIX_ENV=docs mix hex.publish
