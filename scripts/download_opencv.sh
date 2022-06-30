#!/bin/sh

OPENCV_VER="$1"
OPENCV_CACHE_DIR="$2"
OPENCV_ROOT_DIR="$3"

OPENCV_ZIP_URL="https://github.com/opencv/opencv/archive/${OPENCV_VER}.zip"
OPENCV_SOURCE_ZIP="${OPENCV_CACHE_DIR}/opencv-${OPENCV_VER}.zip"

function download_opencv() {
    if [ ! -f "${OPENCV_SOURCE_ZIP}" ]; then
        echo "Downloading OpenCV ${OPENCV_VER}..."
        mkdir -p "${OPENCV_CACHE_DIR}"
        if [ -e "$(which wget)" ]; then
            wget "${OPENCV_ZIP_URL}" -O "${OPENCV_SOURCE_ZIP}"
        elif [ -e "$(which curl)" ]; then
            curl -fSsL "${OPENCV_ZIP_URL}" -o "${OPENCV_SOURCE_ZIP}"
        else
            echo "No wget or curl found, please install one of them"
            exit 1
        fi
    fi
}

function unzip_opencv() {
    if [ ! -d "${OPENCV_ROOT_DIR}/opencv-${OPENCV_VER}" ]; then
        echo "Unzipping OpenCV ${OPENCV_VER}..."
        mkdir -p "${OPENCV_ROOT_DIR}"
        if [ -e "$(which unzip)" ]; then
            unzip "${OPENCV_SOURCE_ZIP}" -d "${OPENCV_ROOT_DIR}" ;
        elif [ -e "$(which 7z)" ]; then
            7z x "${OPENCV_SOURCE_ZIP}" -o"${OPENCV_ROOT_DIR}" ;
        else
            echo "No unzip or 7z found, please install one of them"
            exit 1
        fi
    fi
}

download_opencv && unzip_opencv
