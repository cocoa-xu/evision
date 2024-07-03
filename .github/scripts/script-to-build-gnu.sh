#!/bin/sh

set -x

CUDA_PIN=$1
CUDA_DEB=$2
CUDA_TOOLKIT=$3
CUDA_ID=$4
CUDNN_DEB=$5
CUDNN_PACKAGE=$6
CUDNN_ID=$7
OTP_VERSION=$8
ELIXIR_VERSION=$9
TRIPLET=$10
GITHUB_REF=$11
IMAGE_NAME="ubuntu:20.04"

sudo docker run --privileged --network=host --rm -v $(pwd):/work "${IMAGE_NAME}" \
    sh -c "chmod a+x /work/do-build.sh && /work/do-build.sh '${CUDA_PIN}' '${CUDA_DEB}' '${CUDA_TOOLKIT}' '${CUDA_ID}' '${CUDNN_DEB}' '${CUDNN_PACKAGE}' '${CUDNN_ID}' '${OTP_VERSION}' '${ELIXIR_VERSION}' '${TRIPLET}' '${GITHUB_REF}'"
sudo chown -R $(id -u):$(id -g) .
sudo chmod a+rw -R ./artifacts
