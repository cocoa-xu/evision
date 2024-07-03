#!/bin/bash

set -xe

CUDA_PIN=$1
CUDA_DEB=$2
CUDA_TOOLKIT=$3
CUDA_ID=$4
CUDNN_DEB=$5
CUDNN_PACKAGE=$6
CUDNN_ID=$7
OTP_VERSION=$8
ELIXIR_VERSION=$9
TRIPLET=${10}
GITHUB_REF=${11}

export LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8 DEBIAN_FRONTEND=noninteractive
apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential gcc g++ make autoconf m4 libncurses5-dev libssl-dev \
    cmake make ninja-build git wget ca-certificates libtinfo5 \
    automake autoconf pkg-config bc unzip zip curl gzip python3 libeigen3-dev \
    locales libtool libtool-bin libopenblas-dev libfreetype-dev \
    libgstreamer1.0-dev libgstreamer-plugins-base1.0-dev libgstreamer-plugins-bad1.0-dev \
    gstreamer1.0-plugins-base gstreamer1.0-plugins-good gstreamer1.0-plugins-bad gstreamer1.0-plugins-ugly \
    gstreamer1.0-libav gstreamer1.0-tools gstreamer1.0-x gstreamer1.0-alsa gstreamer1.0-gl gstreamer1.0-gtk3 \
    gstreamer1.0-qt5 gstreamer1.0-pulseaudio sudo
echo "LC_ALL=en_US.UTF-8" >> /etc/environment
echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf
locale-gen en_US.UTF-8

curl -fSL "${CUDA_PIN}" -o cuda.pin
mv cuda.pin /etc/apt/preferences.d/cuda-repository-pin-600
curl -fSL "${CUDA_DEB}" -o cuda.deb
sudo dpkg -i cuda.deb
sudo cp /var/cuda-repo-*/cuda-*-keyring.gpg /usr/share/keyrings/
sudo apt-get update
sudo apt-get -y install "${CUDA_TOOLKIT}"
sudo rm -rf cuda.deb

curl -fSL "${CUDNN_DEB}" -o cudnn.deb
sudo dpkg -i cudnn.deb
sudo cp /var/cudnn-local-repo-*/cudnn-*-keyring.gpg /usr/share/keyrings/
sudo apt-get update
sudo apt-get -y install "${CUDNN_PACKAGE}"
sudo rm -rf cudnn.deb

export PATH="/usr/local/cuda/bin:${PATH}"

mkdir -p ./cache/otp
curl -fSL "https://github.com/cocoa-xu/otp-build/releases/download/v${OTP_VERSION}/otp-${TRIPLET}.tar.gz" -o "./cache/otp/otp-v${OTP_VERSION}-${TRIPLET}.tar.gz"
export ROOT_DIR="$(pwd)"
cd ./cache/otp
tar -xzf "otp-v${OTP_VERSION}-${TRIPLET}.tar.gz"
cd "${ROOT_DIR}"

ELIXIR_VERSION="1.16.2"
export PATH="$(pwd)/./cache/otp/usr/local/bin:$(pwd)/./cache/elixir/elixir-${ELIXIR_VERSION}/bin:${PATH}"
export ERL_ROOTDIR="$(pwd)/./cache/otp/usr/local/lib/erlang"
mkdir -p ./cache/elixir
curl -fSL "https://github.com/elixir-lang/elixir/archive/refs/tags/v${ELIXIR_VERSION}.tar.gz" -o "./cache/elixir/elixir-${ELIXIR_VERSION}.tar.gz"
cd ./cache/elixir
tar -xzf "elixir-${ELIXIR_VERSION}.tar.gz"
cd "elixir-${ELIXIR_VERSION}"
make -j$(nproc) install

cd "${ROOT_DIR}"
mix local.hex --force
mix local.rebar --force

export MIX_ENV=prod NIF_VERSION="2.16" EVISION_PREFER_PRECOMPILED="false" EVISION_GENERATE_LANG="erlang,elixir" EVISION_ENABLE_CUDA="true" EVISION_ENABLE_CONTRIB="true"
cd /work
mix deps.get
mix compile

export PKG_NAME="evision-nif_${NIF_VERSION}-${TRIPLET}-contrib-cuda${CUDA_ID}-cudnn${CUDNN_ID}-${GITHUB_REF##*/v}"
mkdir -p "${PKG_NAME}"

export PRIV_DIR="$(pwd)/_build/${MIX_ENV}/lib/evision/priv"
mv "${PRIV_DIR}/include" /tmp/include
cp -a "${PRIV_DIR}" "${PKG_NAME}"
cp -a lib/generated "${PKG_NAME}/elixir_generated"
cp -a src/generated "${PKG_NAME}/erlang_generated"
tar -czf "${PKG_NAME}.tar.gz" "${PKG_NAME}"
rm -rf "${PKG_NAME}"
ls -lah "${PKG_NAME}.tar.gz"
mkdir -p artifacts
mv "${PKG_NAME}.tar.gz" artifacts
cd artifacts
sha256sum "${PKG_NAME}.tar.gz" | tee "${PKG_NAME}.tar.gz.sha256"
sudo chmod a+rw "${PKG_NAME}.tar.gz" "${PKG_NAME}.tar.gz.sha256"
