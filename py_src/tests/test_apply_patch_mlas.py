import importlib.util
from pathlib import Path

import pytest


REPO_ROOT = Path(__file__).resolve().parents[2]
PATCHER_PATH = REPO_ROOT / "patches" / "apply_patch.py"


def _load_patcher():
    spec = importlib.util.spec_from_file_location("apply_patch", PATCHER_PATH)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _scaffold_mlas_tree(tmp_path):
    upstream = (
        "set(_MLAS_REQUIRES_ASM FALSE)\n"
        "if(MLAS_X86_64 OR MLAS_X86 OR MLAS_ARM64 OR MLAS_LOONGARCH64)\n"
        "  set(_MLAS_REQUIRES_ASM TRUE)\n"
        "endif()\n"
        "\n"
        "include(CheckLanguage)\n"
        "set(MLAS_HAS_ASM FALSE CACHE INTERNAL \"\" FORCE)\n"
        "check_language(ASM)\n"
    )
    mlas_dir = tmp_path / "3rdparty" / "mlas"
    mlas_dir.mkdir(parents=True)
    mlas_cmake = mlas_dir / "CMakeLists.txt"
    mlas_cmake.write_text(upstream)
    return mlas_cmake


def test_patch_inserts_msvc_skip_before_check_language(tmp_path):
    mlas_cmake = _scaffold_mlas_tree(tmp_path)

    _load_patcher().patch_mlas_skip_msvc("5.0.0", str(tmp_path))

    content = mlas_cmake.read_text()
    assert "MLAS: skipped on MSVC" in content
    assert content.index("MLAS: skipped on MSVC") < content.index("include(CheckLanguage)")
    assert "if(MSVC AND _MLAS_REQUIRES_ASM)" in content
    assert "return()" in content


def test_patch_is_idempotent(tmp_path):
    mlas_cmake = _scaffold_mlas_tree(tmp_path)
    patcher = _load_patcher()

    patcher.patch_mlas_skip_msvc("5.0.0", str(tmp_path))
    once = mlas_cmake.read_text()
    patcher.patch_mlas_skip_msvc("5.0.0", str(tmp_path))
    twice = mlas_cmake.read_text()

    assert once == twice
    assert twice.count("MLAS: skipped on MSVC") == 1


def test_patch_no_op_when_file_missing(tmp_path):
    _load_patcher().patch_mlas_skip_msvc("5.0.0", str(tmp_path))

    assert not (tmp_path / "3rdparty" / "mlas" / "CMakeLists.txt").exists()


def test_crosscompile_patch_inserts_skip_before_check_language(tmp_path):
    mlas_cmake = _scaffold_mlas_tree(tmp_path)

    _load_patcher().patch_mlas_skip_crosscompile("5.0.0", str(tmp_path))

    content = mlas_cmake.read_text()
    assert "MLAS: skipped when cross-compiling" in content
    assert content.index("MLAS: skipped when cross-compiling") < content.index(
        "include(CheckLanguage)"
    )
    assert "CMAKE_CROSSCOMPILING OR CMAKE_TOOLCHAIN_FILE" in content
    # s390x / riscv64 use pure-C++ kernels (so _MLAS_REQUIRES_ASM is FALSE), but
    # OpenCV 5.0's vendored subset cannot build them; the skip must cover them too.
    assert "MLAS_S390X" in content
    assert "MLAS_RISCV64" in content


def test_crosscompile_patch_is_idempotent(tmp_path):
    mlas_cmake = _scaffold_mlas_tree(tmp_path)
    patcher = _load_patcher()

    patcher.patch_mlas_skip_crosscompile("5.0.0", str(tmp_path))
    once = mlas_cmake.read_text()
    patcher.patch_mlas_skip_crosscompile("5.0.0", str(tmp_path))
    twice = mlas_cmake.read_text()

    assert once == twice
    assert twice.count("MLAS: skipped when cross-compiling") == 1


def test_crosscompile_patch_no_op_when_file_missing(tmp_path):
    _load_patcher().patch_mlas_skip_crosscompile("5.0.0", str(tmp_path))

    assert not (tmp_path / "3rdparty" / "mlas" / "CMakeLists.txt").exists()
