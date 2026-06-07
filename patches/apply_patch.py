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
    # 4.13.0 still has `// FIXIT: CV_WRAP` next to these declarations, so the
    # patch is still required for the binding generator to pick them up.
    if opencv_version not in ['4.5.4', '4.5.5', '4.6.0', '4.7.0', '4.8.0', '4.9.0', '4.10.0', '4.11.0', '4.13.0']:
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

def patch_carotene_vround(opencv_version: str, opencv_src_root: str):
    """Gate carotene's ARMv8 round-to-nearest intrinsics on __aarch64__.

    OpenCV's carotene HAL (hal/carotene/src/vround_helper.hpp) guards the
    vcvtn* intrinsics with `__ARM_ARCH >= 8`. On 32-bit ARMv8 targets such as
    Nerves rpi3/rpi3a/rpi0_2 (Cortex-A53 built for armv7hf) `__ARM_ARCH` is 8,
    but GCC's AArch32 arm_neon.h does not provide vcvtn* at all, so the HAL
    fails to compile. Restrict the fast path to AArch64; AArch32 keeps the
    portable rounding fallback already present in the file.
    """
    vround_hpp = (
        Path(opencv_src_root) / 'hal' / 'carotene' / 'src' / 'vround_helper.hpp'
    )
    if not vround_hpp.exists():
        print(f"warning: {vround_hpp} not found, skipping patch_carotene_vround")
        return

    original = '#if defined(__ARM_ARCH) && (__ARM_ARCH >= 8)'
    fixed = StringIO()
    patched = 0
    with open(vround_hpp, 'r') as source:
        for line in source:
            if line.strip() == original:
                fixed.write('#if defined(__aarch64__) // patched\n')
                patched += 1
            else:
                fixed.write(line)

    if patched:
        with open(vround_hpp, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())
        print(f"[+] patched carotene vround_helper.hpp ({patched} guards)")


def patch_intrin_neon_v_floor(opencv_version: str, opencv_src_root: str):
    """Gate v_floor's ARMv8 round-to-minus-infinity intrinsic on __aarch64__.

    modules/core/include/opencv2/core/hal/intrin_neon.hpp implements
    v_floor(v_float32x4) with `#if __ARM_ARCH > 7` -> vcvtmq_s32_f32, an
    AArch64-only NEON intrinsic. On 32-bit ARMv8 targets such as Nerves
    rpi3/rpi3a/rpi0_2 (Cortex-A53 built for armv7hf) __ARM_ARCH is 8, but GCC's
    AArch32 arm_neon.h does not provide vcvtmq_s32_f32, so the core module
    fails to compile. Restrict the fast path to AArch64; AArch32 keeps the
    portable floor fallback already present in the #else branch.

    Mirrors patch_carotene_vround for the universal-intrinsics NEON backend.
    """
    intrin_neon_hpp = (
        Path(opencv_src_root) / 'modules' / 'core' / 'include' / 'opencv2'
        / 'core' / 'hal' / 'intrin_neon.hpp'
    )
    if not intrin_neon_hpp.exists():
        print(f"warning: {intrin_neon_hpp} not found, skipping patch_intrin_neon_v_floor")
        return

    original = '#if __ARM_ARCH > 7'
    fixed = StringIO()
    patched = 0
    with open(intrin_neon_hpp, 'r') as source:
        for line in source:
            if line.strip() == original:
                fixed.write('#if defined(__aarch64__) // patched\n')
                patched += 1
            else:
                fixed.write(line)

    if patched:
        with open(intrin_neon_hpp, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())
        print(f"[+] patched intrin_neon.hpp v_floor guard ({patched})")


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


def patch_mlas_skip_msvc(opencv_version: str, opencv_src_root: str):
    """Skip the vendored MLAS subdirectory when building with MSVC.

    MLAS ships GNU-style ``.S`` SGEMM kernels (AT&T syntax, ``-msse2`` /
    ``-mavx`` style flags). On Windows + MSVC, ``check_language(ASM)`` finds
    MASM (``ml64.exe``) and ``enable_language(ASM)`` succeeds, so the OBJECT
    target gets created; CMake (CMP0091 NEW) then auto-applies
    ``MSVC_RUNTIME_LIBRARY=MultiThreadedDLL`` to every language on the target
    and configure dies with::

        CMake Error in 3rdparty/mlas/CMakeLists.txt:
          MSVC_RUNTIME_LIBRARY value 'MultiThreadedDLL' not known for this
          ASM compiler.

    Even if that propagation were silenced, the GNU-style ``.S`` files
    couldn't be assembled with MASM/armasm. Reuse the file's existing
    early-return pattern (``OPENCV_DNN_MLAS_SKIP_REASON`` + ``return()``)
    so the DNN module falls back to its built-in SGEMM.
    """
    mlas_cmake = (
        Path(opencv_src_root) / '3rdparty' / 'mlas' / 'CMakeLists.txt'
    )
    if not mlas_cmake.exists():
        return

    anchor = 'include(CheckLanguage)'
    insertion = """\
if(MSVC AND _MLAS_REQUIRES_ASM)
  set(OPENCV_DNN_MLAS_SKIP_REASON
    "MLAS .S kernels are GNU-style; MASM/armasm cannot assemble them"
    CACHE INTERNAL "" FORCE)
  message(STATUS "MLAS: skipped on MSVC (DNN will use its built-in SGEMM)")
  return()
endif()

"""

    with open(mlas_cmake, 'r') as f:
        original = f.read()
    if 'MLAS: skipped on MSVC' in original:
        return

    fixed = StringIO()
    patched = False
    for line in original.splitlines(keepends=True):
        if not patched and line.strip() == anchor:
            fixed.write(insertion)
            patched = True
        fixed.write(line)

    if patched:
        with open(mlas_cmake, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())
        print("[+] patched 3rdparty/mlas/CMakeLists.txt to skip MLAS on MSVC")
    else:
        print(f"warning: anchor not found in {mlas_cmake}, skipping patch_mlas_skip_msvc")


def patch_mlas_skip_crosscompile(opencv_version: str, opencv_src_root: str):
    """Skip the vendored MLAS subdirectory when cross-compiling.

    MLAS builds its .S kernels through a separate ASM language. Cross toolchains
    (Nerves, iOS, ...) usually set only CMAKE_C/CXX_COMPILER, so cmake's
    enable_language(ASM) auto-detects the *host* assembler (e.g. /usr/bin/cc on an
    x86_64 runner) and feeds it the target's AArch64 .S, which fails with host
    GAS errors (``no such instruction: 'fmla v5.4s...'``, ``too many memory
    references``). This hits every Nerves ARM board: the 32-bit ones report an
    arm64 CMAKE_SYSTEM_PROCESSOR (so MLAS picks lib/aarch64), and rpi5 is genuine
    aarch64 — both assembled by the host x86 `as`. Skip MLAS whenever it needs
    ASM and we are cross-compiling; the DNN module falls back to its built-in
    SGEMM. Mirrors patch_mlas_skip_msvc.
    """
    mlas_cmake = (
        Path(opencv_src_root) / '3rdparty' / 'mlas' / 'CMakeLists.txt'
    )
    if not mlas_cmake.exists():
        return

    anchor = 'include(CheckLanguage)'
    insertion = """\
if(_MLAS_REQUIRES_ASM AND (CMAKE_CROSSCOMPILING OR CMAKE_TOOLCHAIN_FILE))
  set(OPENCV_DNN_MLAS_SKIP_REASON
    "cross toolchain has no matching ASM compiler for MLAS .S kernels"
    CACHE INTERNAL "" FORCE)
  message(STATUS "MLAS: skipped when cross-compiling (DNN will use its built-in SGEMM)")
  return()
endif()

"""

    with open(mlas_cmake, 'r') as f:
        original = f.read()
    if 'MLAS: skipped when cross-compiling' in original:
        return

    fixed = StringIO()
    patched = False
    for line in original.splitlines(keepends=True):
        if not patched and line.strip() == anchor:
            fixed.write(insertion)
            patched = True
        fixed.write(line)

    if patched:
        with open(mlas_cmake, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())
        print("[+] patched 3rdparty/mlas/CMakeLists.txt to skip MLAS when cross-compiling")
    else:
        print(f"warning: anchor not found in {mlas_cmake}, skipping patch_mlas_skip_crosscompile")


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
    patch_rpath_linux,
    patch_python_bindings_generator,
    patch_carotene_vround,
    patch_intrin_neon_v_floor,
    patch_imread,
    patch_cmake_minimum_version,
    patch_mlas_skip_msvc,
    patch_mlas_skip_crosscompile,
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
