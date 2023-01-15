#!/bin/sh

OPENCV_VER="$1"
OPENCV_CACHE_DIR="$2"
OPENCV_ROOT_DIR="$3"

OPENCV_CONTRIB_ZIP_URL="https://github.com/opencv/opencv_contrib/archive/${OPENCV_VER}.zip"
OPENCV_CONTRIB_SOURCE_ZIP="${OPENCV_CACHE_DIR}/opencv_contrib-${OPENCV_VER}.zip"

download_opencv_contrib() {
    if [ ! -f "${OPENCV_CONTRIB_SOURCE_ZIP}" ]; then
        echo "Downloading OpenCV contrib ${OPENCV_VER}..."
        mkdir -p "${OPENCV_CACHE_DIR}"
        if [ -e "$(which wget)" ]; then
            wget --quiet "${OPENCV_CONTRIB_ZIP_URL}" -O "${OPENCV_CONTRIB_SOURCE_ZIP}"
        elif [ -e "$(which curl)" ]; then
            curl -fSsL "${OPENCV_CONTRIB_ZIP_URL}" -o "${OPENCV_CONTRIB_SOURCE_ZIP}"
        else
            echo "No wget or curl found, please install one of them"
            exit 1
        fi
    fi
}

unzip_opencv_contrib() {
    if [ ! -d "${OPENCV_ROOT_DIR}/opencv_contrib-${OPENCV_VER}" ]; then
        echo "Unzipping OpenCV contrib ${OPENCV_VER}..."
        mkdir -p "${OPENCV_ROOT_DIR}"
        if [ -e "$(which unzip)" ]; then
            unzip -qq "${OPENCV_CONTRIB_SOURCE_ZIP}" -d "${OPENCV_ROOT_DIR}" ;
        elif [ -e "$(which 7z)" ]; then
            7z x "${OPENCV_CONTRIB_SOURCE_ZIP}" -o"${OPENCV_ROOT_DIR}" ;
        else
            echo "No unzip or 7z found, please install one of them"
            exit 1
        fi
    fi
}

download_opencv_contrib && unzip_opencv_contrib
