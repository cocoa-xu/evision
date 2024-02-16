#!/bin/bash
#
# evision hex.pm release script
# please run this script in the root of the project on an x86_64 Linux machine
set -xe

export MIX_ENV=docs
export EVISION_PREFER_PRECOMPILED=true
export EVISION_ENABLE_CUDA=true
export EVISION_CUDA_VERSION=121
export NIF_VERSION=2.16
export EVISION_VERSION="$(grep 'def version, do: "' mix.exs | grep -E -oh '(\d+).(\d+).(\d+)')"

export TARBALL_FILENAME="evision-nif_${NIF_VERSION}-x86_64-linux-gnu-contrib-cuda${EVISION_CUDA_VERSION}-${EVISION_VERSION}.tar.gz"
curl -fSL "https://github.com/cocoa-xu/evision/releases/download/v${EVISION_VERSION}/${TARBALL_FILENAME}" -o "/tmp/${TARBALL_FILENAME}"
export TARBALL_SHA256="$(sha256sum "/tmp/${TARBALL_FILENAME}" | cut -d ' ' -f 1)"
cat <<EOF > checksum.exs
%{
  "${TARBALL_FILENAME}" => "sha256:${TARBALL_SHA256}"
}
EOF
EVISION_FETCH_PRECOMPILED=true mix evision.fetch --all
MIX_ENV=docs mix hex.publish
