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
OS_IMAGE=${12:-ubuntu:20.04}
IMAGE_NAME="${OS_IMAGE}"

RC=0
sudo docker run --privileged --network=host --platform=linux/arm64 --rm -v $(pwd):/work "${IMAGE_NAME}" \
    sh -c "chmod a+x /work/do-build.sh && /work/do-build.sh '${CUDA_PIN}' '${CUDA_DEB}' '${CUDA_TOOLKIT}' '${CUDA_ID}' '${CUDNN_DEB}' '${CUDNN_PACKAGE}' '${CUDNN_ID}' '${OTP_VERSION}' '${ELIXIR_VERSION}' '${TRIPLET}' '${GITHUB_REF}'" || RC=$?
# chown back even on failure so actions/cache can read the cached tarballs
sudo chown -R $(id -u):$(id -g) . || true
sudo chmod a+rw -R ./artifacts || true
exit $RC
