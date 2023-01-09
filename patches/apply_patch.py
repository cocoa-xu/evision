#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
from pathlib import Path
if sys.version_info[0] >= 3:
    from io import StringIO
else:
    from cStringIO import StringIO


def patch_fix_getLayerShapes(opencv_version: str, opencv_src_root: str):
    if opencv_version not in ['4.5.4', '4.5.5', '4.6.0', '4.7.0']:
        print(f"warning: applying `patch_fix_getLayerShapes` to opencv version `{opencv_version}`")

    # modules/dnn/include/opencv2/dnn/dnn.hpp
    dnn_hpp = Path(opencv_src_root) / 'modules' / 'dnn' / 'include' / 'opencv2' / 'dnn' / 'dnn.hpp'
    fixed = StringIO()
    patched_1 = False
    patched_2 = False
    with open(dnn_hpp, 'r') as source:
        for line in source:
            if not patched_1 and line.strip() == 'void getLayerShapes(const MatShape& netInputShape,':
                fixed.write('        CV_WRAP void getLayerShapes(const MatShape& netInputShape,\n')
                patched_1 = True
            elif not patched_2 and line.strip() == 'void getLayerShapes(const std::vector<MatShape>& netInputShapes,':
                fixed.write('        CV_WRAP void getLayerShapes(const std::vector<MatShape>& netInputShapes,\n')
                patched_2 = True
            else:
                fixed.write(line)

    with open(dnn_hpp, 'w') as dst:
        dst.truncate(0)
        dst.write(fixed.getvalue())


def patch_winograd(opencv_version: str, opencv_src_root: str):
    if opencv_version not in ['4.7.0']:
        print(f"warning: skipped applying `patch_winograd` to opencv version `{opencv_version}`")
        return

    # modules/dnn/src/layers/fast_convolution/fast_convolution.cpp
    fast_convolution_cpp = Path(opencv_src_root) / 'modules' / 'dnn' / 'src' / 'layers' / 'fast_convolution' / 'fast_convolution.cpp'
    fixed = StringIO()
    patched_1 = False
    with open(fast_convolution_cpp, 'r') as source:
        for line in source:
            if not patched_1 and line.strip() == 'Mat weightsMat = _weightsMat.getMat();':
                fixed.write("""#if CV_TRY_AVX2
    // Disabel Winograd when CV_TRY_AVX2 is true, but conv->useAVX2 is false.
    if (conv->conv_type == _FX_CONV_TYPE_WINOGRAD3X3 && !conv->useAVX2)
        conv->conv_type = _FX_CONV_TYPE_GENERIC;
#endif

    Mat weightsMat = _weightsMat.getMat(); // patched\n""")
                patched_1 = True
            else:
                fixed.write(line)

    with open(fast_convolution_cpp, 'w') as dst:
        dst.truncate(0)
        dst.write(fixed.getvalue())

patches = [patch_fix_getLayerShapes, patch_winograd]


if __name__ == '__main__':
    cv_version = None
    cv_src_root = None
    if len(sys.argv) != 3:
        sys.exit(1)
    cv_src_root = sys.argv[1]
    cv_version = sys.argv[2]
    print(f"[+] applying patches to OpenCV {cv_version} at {cv_src_root}")
    for p in patches:
        p(cv_version, cv_src_root)
