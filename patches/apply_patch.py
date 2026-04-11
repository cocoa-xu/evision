#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
from pathlib import Path
if sys.version_info[0] >= 3:
    from io import StringIO
else:
    from cStringIO import StringIO


def patch_imread(opencv_version: str, opencv_src_root: str):
    if opencv_version not in ['4.10.0']:
        print(f"warning: applying `patch_imread` to opencv version `{opencv_version}`")
    
    # modules/imgcodecs/src/loadsave.cpp
    loadsave_cpp = Path(opencv_src_root) / 'modules' / 'imgcodecs' / 'src' / 'loadsave.cpp'
    fixed = StringIO()
    patched_1 = False
    with open(loadsave_cpp, 'r') as source:
        for line in source:
            if not patched_1 and line.strip() == 'Mat img = dst.getMat();':
                fixed.write("    Mat& img = dst.getMatRef(); // patched\n")
                patched_1 = True
            else:
                fixed.write(line)
    
    if patched_1:
        with open(loadsave_cpp, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())
                

def patch_fix_getLayerShapes(opencv_version: str, opencv_src_root: str):
    if opencv_version not in ['4.5.4', '4.5.5', '4.6.0', '4.7.0', '4.8.0', '4.9.0', '4.10.0', '4.11.0']:
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

    if patched_1 or patched_2:
        with open(dnn_hpp, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())


def patch_winograd(opencv_version: str, opencv_src_root: str):
    if opencv_version not in ['4.7.0']:
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

    if patched_1:
        with open(fast_convolution_cpp, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())


def patch_rpath_linux(opencv_version: str, opencv_src_root: str):
    # CMakeLists.txt
    cmakelists_txt = Path(opencv_src_root) / 'CMakeLists.txt'
    fixed = StringIO()
    patched_1 = False
    with open(cmakelists_txt, 'r') as source:
        for line in source:
            if not patched_1 and line.strip() == 'string(REPLACE "opencv_" "" OPENCV_MODULES_BUILD_ST          "${OPENCV_MODULES_BUILD_ST}")':
                fixed.write(r"""if(UNIX AND NOT APPLE)
  foreach(the_module ${OPENCV_MODULES_BUILD_ST})
    set_target_properties(${the_module} PROPERTIES
      INSTALL_RPATH_USE_LINK_PATH TRUE
      BUILD_WITH_INSTALL_RPATH TRUE
    )
    set_target_properties(${the_module} PROPERTIES INSTALL_RPATH "\$ORIGIN")
  endforeach()
endif()
string(REPLACE "opencv_" "" OPENCV_MODULES_BUILD_ST          "${OPENCV_MODULES_BUILD_ST}") # patched
""")
                patched_1 = True
            else:
                fixed.write(line)

    if patched_1:
        with open(cmakelists_txt, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())


def patch_python_bindings_generator(opencv_version: str, opencv_src_root: str):
    # modules/python/CMakeLists.txt
    cmakelists_txt = Path(opencv_src_root) / 'modules' / 'python' / 'CMakeLists.txt'
    fixed = StringIO()
    patched_1 = False
    with open(cmakelists_txt, 'r') as source:
        for line in source:
            if not patched_1 and line.strip() == 'if(ANDROID OR APPLE_FRAMEWORK OR WINRT)':
                fixed.write("""if(ANDROID OR APPLE_FRAMEWORK OR WINRT) # patched
  add_subdirectory(bindings)
""")
                patched_1 = True
            else:
                fixed.write(line)

    if patched_1:
        with open(cmakelists_txt, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())

def patch_intrin_rvv(opencv_version: str, opencv_src_root: str):
    if opencv_version in ['4.11.0']:
        return
    if opencv_version not in ['4.9.0']:
        print(f"warning: skipped, applying `patch_intrin_rvv` to opencv version `{opencv_version}`")
    # modules/core/include/opencv2/core/hal/intrin_rvv.hpp
    intrin_rvv_hpp = Path(opencv_src_root) / 'modules' / 'core' / 'include' / 'opencv2' / 'core' / 'hal' / 'intrin_rvv.hpp'
    fixed = StringIO()
    patched_1 = False
    with open(intrin_rvv_hpp, 'r') as source:
        for line in source:
            if not patched_1 and line.strip() == '#define OPENCV_HAL_INTRIN_RVV_HPP':
                fixed.write("""#define OPENCV_HAL_INTRIN_RVV_HPP // patched
typedef float float32_t;
typedef double float64_t;
""")
                patched_1 = True
            else:
                fixed.write(line)

    if patched_1:
        with open(intrin_rvv_hpp, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())

def patch_ocr_tesseract_run_with_components(opencv_version: str, opencv_src_root: str):
    """Add CV_WRAP runWithComponents() to OCRTesseract.

    The existing CV_WRAP run() overloads only return a String.  The void run()
    overloads that also return component_rects/texts/confidences are not
    CV_WRAP-ped, so the Evision binding generator never sees them.

    This patch adds a non-virtual inline runWithComponents() that delegates
    to the virtual run() and is CV_WRAP-ped with CV_OUT params so the
    binding generator exposes the bounding-box data to Elixir.
    """
    # contrib lives as a sibling: 3rd_party/opencv/opencv_contrib-{ver}/
    opencv_root = Path(opencv_src_root).parent
    contrib_root = opencv_root / f'opencv_contrib-{opencv_version}'
    ocr_hpp = (
        contrib_root / 'modules' / 'text' / 'include' / 'opencv2' / 'text' / 'ocr.hpp'
    )
    if not ocr_hpp.exists():
        print(f"warning: {ocr_hpp} not found, skipping patch_ocr_tesseract_run_with_components")
        return

    anchor = '    CV_WRAP virtual void setWhiteList(const String& char_whitelist) = 0;'
    insertion = '''\
    /** @brief Recognize text and return per-component bounding boxes.

    Unlike the run() overloads that return only a text string, this method
    also outputs per-component Rects, text strings, and confidence values.

    @param image Input image CV_8UC1 or CV_8UC3
    @param output_text Output recognised text.
    @param component_rects Output list of Rects for individual text elements.
    @param component_texts Output list of text strings for individual text elements.
    @param component_confidences Output list of confidence values.
    @param component_level OCR_LEVEL_WORD (default) or OCR_LEVEL_TEXTLINE.
    */
    CV_WRAP void runWithComponents(InputArray image,
                                   CV_OUT String& output_text,
                                   CV_OUT std::vector<Rect>& component_rects,
                                   CV_OUT std::vector<String>& component_texts,
                                   CV_OUT std::vector<float>& component_confidences,
                                   int component_level = 0);

'''

    # Idempotency: skip if already patched.
    with open(ocr_hpp, 'r') as f:
        if 'runWithComponents' in f.read():
            print("[+] OCRTesseract already patched with runWithComponents()")
            return

    fixed = StringIO()
    patched = False
    with open(ocr_hpp, 'r') as source:
        for line in source:
            if not patched and line.rstrip() == anchor:
                fixed.write(insertion)
                patched = True
            fixed.write(line)

    if patched:
        with open(ocr_hpp, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())
        print("[+] patched OCRTesseract with runWithComponents()")
    else:
        print(f"warning: anchor line not found in {ocr_hpp}, skipping patch")


def patch_cmake_minimum_version(opencv_version: str, opencv_src_root: str):
    # cmake/OpenCVGenPkgconfig.cmake uses cmake_minimum_required(VERSION 2.8.12.2)
    # which fails with CMake 4.x that removed compat with < 3.5
    pkgconfig_cmake = Path(opencv_src_root) / 'cmake' / 'OpenCVGenPkgconfig.cmake'
    fixed = StringIO()
    patched_1 = False
    with open(pkgconfig_cmake, 'r') as source:
        for line in source:
            if not patched_1 and line.strip() == 'cmake_minimum_required(VERSION 2.8.12.2)':
                fixed.write("cmake_minimum_required(VERSION 3.5) # patched\n")
                patched_1 = True
            else:
                fixed.write(line)

    if patched_1:
        with open(pkgconfig_cmake, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())


patches = [
    patch_fix_getLayerShapes,
    patch_winograd,
    patch_rpath_linux,
    patch_python_bindings_generator,
    patch_intrin_rvv,
    patch_imread,
    patch_cmake_minimum_version,
    patch_ocr_tesseract_run_with_components
]


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
