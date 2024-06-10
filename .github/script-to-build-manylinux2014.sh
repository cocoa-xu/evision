#!/bin/sh

set -x

MIX_ENV=$1
OTP_VERSION=$2
HOST_ARCHITECTURE=$3
ELIXIR_VERSION=$4
EVISION_ENABLE_CONTRIB=$5
GLEAM_VERSION=$6

sudo docker run --privileged --network=host --rm -v "$(pwd):/work" quay.io/pypa/manylinux2014_x86_64:latest \
  sh -c "chmod a+x /work/do-build-manylinux2014.sh && /work/do-build-manylinux2014.sh ${MIX_ENV} ${OTP_VERSION} ${HOST_ARCHITECTURE} ${ELIXIR_VERSION} ${EVISION_ENABLE_CONTRIB} ${GLEAM_VERSION}"
