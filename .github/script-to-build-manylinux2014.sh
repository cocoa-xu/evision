#!/bin/sh

set -x

MIX_ENV=$1
OTP_VERSION=$2
ELIXIR_VERSION=$3
EVISION_ENABLE_CONTRIB=$4
CMAKE_TOOLCHAIN_FILE=$5

echo "$(pwd)"
ls -lah
sudo docker run --privileged --network=host --rm -v `pwd`:/work quay.io/pypa/manylinux2014_x86_64:latest \
            sh -c "chmod a+x /work/do-build-maylinux2014.sh && /work/do-build-maylinux2014.sh ${MIX_ENV} ${OTP_VERSION} ${ELIXIR_VERSION} ${EVISION_ENABLE_CONTRIB} ${CMAKE_TOOLCHAIN_FILE}"
sudo chmod -R a+wr `pwd`/_build
