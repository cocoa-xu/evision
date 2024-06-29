#!/bin/sh

set -x

MIX_ENV=$1
OTP_VERSION=$2
HOST_ARCHITECTURE=$3
ELIXIR_VERSION=$4
EVISION_ENABLE_CONTRIB=$5
GLEAM_VERSION=$6

export PATH=/work/cache/otp/usr/local/bin:/work/cache/elixir/elixir-${ELIXIR_VERSION}/bin:${PATH}
export ERL_ROOTDIR=/work/cache/otp/usr/local/lib/erlang
export CMAKE_OPENCV_OPTIONS="-D CMAKE_C_FLAGS=\"-static-libgcc -static-libstdc++\" -D CMAKE_CXX_FLAGS=\"-static-libgcc -static-libstdc++\""
export CMAKE_EVISION_OPTIONS="-D CMAKE_C_FLAGS=\"-static-libgcc -static-libstdc++\" -D CMAKE_CXX_FLAGS=\"-static-libgcc -static-libstdc++\""

yum install -y openssl-devel ncurses-devel perl-IPC-Cmd python3 eigen3 && \
    cd /work && \
    mix local.hex --force && \
    mix local.rebar --force && \
    mix deps.get && \
    mix compile_opencv

# Mix compile
cd /work
export EVISION_ENABLE_CONTRIB="${EVISION_ENABLE_CONTRIB}"
export MIX_ENV="${MIX_ENV}"
export EVISION_PREFER_PRECOMPILED="false"
export GLEAM_VERSION="${GLEAM_VERSION}"
rm -f _build/${MIX_ENV}/lib/evision/priv/evision.so

rm -rf lib/generated && rm -rf src/generated
mkdir -p lib/generated && mkdir -p src/generated

mix compile
