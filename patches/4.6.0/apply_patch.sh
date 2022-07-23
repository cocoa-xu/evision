#!/bin/sh

# use OPENCV_DIR env var from Makefile
cd "${OPENCV_DIR}" || exit 1
patch -p0 -N -s --no-backup-if-mismatch -r '-' < ../../../patches/4.6.0/0001-fix-getLayerShapes.patch > /dev/null

# cleanup
rm -f "${OPENCV_DIR}/-"

