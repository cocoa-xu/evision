#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Unit tests for ``py_src/opencv_js_whitelist.py``.

Run with::

    python3 -m unittest py_src.tests.test_opencv_js_whitelist

or, from the repo root::

    python3 -m unittest discover -s py_src/tests -t .
"""

from __future__ import annotations

import os
import sys
import textwrap
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "py_src"))

import opencv_js_whitelist as w


SAMPLE_CONFIG = textwrap.dedent(
    """
    core = {
        '': ['absdiff', 'add'],
        'Algorithm': [],
    }
    imgproc = {
        '': ['Canny', 'blur', 'cvtColor'],
        'CLAHE': ['apply', 'collectGarbage'],
    }
    objdetect = {
        '': ['groupRectangles'],
        'CascadeClassifier': ['load', 'CascadeClassifier', 'detectMultiScale'],
        'aruco_Dictionary': ['Dictionary', 'getDistanceToId'],
        'QRCodeDetectorAruco_Params': ['Params'],
        'aruco_PredefinedDictionaryType': [],
    }
    dnn = {
        '': ['readNetFromCaffe'],
        'dnn_Net': ['setInput', 'forward'],
    }
    calib3d = {
        '': ['findHomography', 'fisheye_projectPoints'],
    }

    white_list = makeWhiteList([core, imgproc, objdetect, dnn, calib3d])
    """
).strip() + "\n"


def _write_fixture(tmpdir: Path, body: str = SAMPLE_CONFIG) -> Path:
    config = tmpdir / "opencv_js.config.py"
    config.write_text(body)
    return config


class LoadModulesTest(unittest.TestCase):
    def test_returns_per_module_dicts(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp))
            modules = w.load_modules(config)

        self.assertEqual(
            set(modules.keys()),
            {"core", "imgproc", "objdetect", "dnn", "calib3d"},
        )
        self.assertEqual(modules["core"][""], ["absdiff", "add"])
        self.assertEqual(modules["core"]["Algorithm"], [])
        self.assertEqual(modules["imgproc"]["CLAHE"], ["apply", "collectGarbage"])

    def test_excludes_reserved_names(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp))
            modules = w.load_modules(config)
        self.assertNotIn("white_list", modules)
        self.assertNotIn("makeWhiteList", modules)
        self.assertNotIn("namespace_prefix_override", modules)

    def test_returned_lists_are_independent_copies(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp))
            modules = w.load_modules(config)
            modules["core"][""].append("MUTATED")
            modules2 = w.load_modules(config)
            self.assertNotIn("MUTATED", modules2["core"][""])

    def test_real_opencv_config_loads(self):
        real = w.discover_config_path()
        modules = w.load_modules(real)
        self.assertIn("core", modules)
        self.assertIn("imgproc", modules)
        self.assertIn("dnn", modules)
        self.assertIn("Canny", modules["imgproc"][""])


class FlatSetTest(unittest.TestCase):
    def test_free_functions_have_none_classname(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp))
            s = w.flat_set(config)
        self.assertIn(("imgproc", None, "Canny"), s)
        self.assertIn(("core", None, "absdiff"), s)
        self.assertIn(("dnn", None, "readNetFromCaffe"), s)

    def test_class_keyed_methods(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp))
            s = w.flat_set(config)
        self.assertIn(("objdetect", "CascadeClassifier", "load"), s)
        self.assertIn(("dnn", "dnn_Net", "forward"), s)
        self.assertIn(("imgproc", "CLAHE", "apply"), s)

    def test_empty_class_lists_contribute_nothing(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp))
            s = w.flat_set(config)
        for entry in s:
            self.assertNotEqual(entry[1], "Algorithm")
            self.assertNotEqual(entry[1], "aruco_PredefinedDictionaryType")

    def test_accepts_preloaded_modules(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp))
            modules = w.load_modules(config)
            from_path = w.flat_set(config)
            from_modules = w.flat_set(modules=modules)
        self.assertEqual(from_path, from_modules)


class EntriesTest(unittest.TestCase):
    def test_free_functions_emit_kind_function(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp))
            entries = set(w.entries(config))
        self.assertIn(("imgproc", None, "Canny", "function"), entries)
        self.assertIn(("dnn", None, "readNetFromCaffe", "function"), entries)
        self.assertIn(("calib3d", None, "fisheye_projectPoints", "function"), entries)

    def test_methods_emit_kind_method(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp))
            entries = set(w.entries(config))
        self.assertIn(("objdetect", "CascadeClassifier", "load", "method"), entries)
        self.assertIn(("objdetect", "CascadeClassifier", "detectMultiScale", "method"), entries)
        self.assertIn(("dnn", "dnn_Net", "setInput", "method"), entries)
        self.assertIn(("imgproc", "CLAHE", "apply", "method"), entries)

    def test_constructor_when_fn_matches_bare_class_name(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp))
            entries = set(w.entries(config))
        self.assertIn(("objdetect", "CascadeClassifier", "CascadeClassifier", "constructor"), entries)
        self.assertIn(("objdetect", "aruco_Dictionary", "Dictionary", "constructor"), entries)
        self.assertIn(("objdetect", "QRCodeDetectorAruco_Params", "Params", "constructor"), entries)

    def test_methods_outnumber_constructors(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp))
            entries = list(w.entries(config))
        kinds = {e[3] for e in entries}
        self.assertEqual(kinds, {"function", "method", "constructor"})

    def test_outer_iteration_is_stable_sorted(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp))
            entries = list(w.entries(config))
        module_order = []
        for e in entries:
            if not module_order or module_order[-1] != e[0]:
                module_order.append(e[0])
        self.assertEqual(module_order, sorted(set(module_order)))

    def test_real_opencv_config_yields_expected_starter_palette(self):
        entries = set(w.entries())
        self.assertIn(("imgproc", None, "blur", "function"), entries)
        self.assertIn(("imgproc", None, "Canny", "function"), entries)
        self.assertIn(("imgproc", None, "threshold", "function"), entries)
        self.assertIn(("imgproc", None, "cvtColor", "function"), entries)
        self.assertIn(("imgproc", None, "resize", "function"), entries)
        self.assertIn(("objdetect", "CascadeClassifier", "CascadeClassifier", "constructor"), entries)
        self.assertIn(("objdetect", "CascadeClassifier", "detectMultiScale", "method"), entries)
        self.assertIn(("dnn", None, "readNetFromCaffe", "function"), entries)


class NamespacePrefixOverrideTest(unittest.TestCase):
    def test_defaults_when_config_silent(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp))
            overrides = w.namespace_prefix_override(config)
        self.assertEqual(overrides, {"dnn": "", "aruco": ""})

    def test_config_can_extend_defaults(self):
        body = SAMPLE_CONFIG + "namespace_prefix_override = {'fisheye': 'fish'}\n"
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp), body=body)
            overrides = w.namespace_prefix_override(config)
        self.assertEqual(
            overrides,
            {"dnn": "", "aruco": "", "fisheye": "fish"},
        )

    def test_config_can_shadow_defaults(self):
        body = SAMPLE_CONFIG + "namespace_prefix_override = {'dnn': 'deep'}\n"
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp), body=body)
            overrides = w.namespace_prefix_override(config)
        self.assertEqual(overrides["dnn"], "deep")
        self.assertEqual(overrides["aruco"], "")


class EmptyAndMissingTest(unittest.TestCase):
    def test_empty_whitelist_yields_no_entries(self):
        with TemporaryDirectory() as tmp:
            config = _write_fixture(Path(tmp), body="white_list = makeWhiteList([])\n")
            modules = w.load_modules(config)
            entries = list(w.entries(config))
            flat = w.flat_set(config)
        self.assertEqual(modules, {})
        self.assertEqual(entries, [])
        self.assertEqual(flat, set())

    def test_missing_file_raises(self):
        with TemporaryDirectory() as tmp:
            missing = Path(tmp) / "does_not_exist.py"
            with self.assertRaises(w.ConfigNotFoundError):
                w.load_modules(missing)
            with self.assertRaises(w.ConfigNotFoundError):
                w.flat_set(missing)
            with self.assertRaises(w.ConfigNotFoundError):
                list(w.entries(missing))
            with self.assertRaises(w.ConfigNotFoundError):
                w.namespace_prefix_override(missing)

    def test_config_not_found_error_is_file_not_found_error(self):
        self.assertTrue(issubclass(w.ConfigNotFoundError, FileNotFoundError))


class DiscoverConfigPathTest(unittest.TestCase):
    def test_env_var_drives_lookup(self):
        with TemporaryDirectory() as tmp:
            root = Path(tmp)
            opencv = root / "3rd_party" / "opencv" / "opencv-9.9.9" / "platforms" / "js"
            opencv.mkdir(parents=True)
            (opencv / "opencv_js.config.py").write_text("white_list = makeWhiteList([])\n")

            saved = os.environ.get("OPENCV_VER")
            os.environ["OPENCV_VER"] = "9.9.9"
            try:
                found = w.discover_config_path(repo_root=root)
            finally:
                if saved is None:
                    del os.environ["OPENCV_VER"]
                else:
                    os.environ["OPENCV_VER"] = saved

        self.assertEqual(found.name, "opencv_js.config.py")
        self.assertIn("opencv-9.9.9", found.parts)

    def test_falls_back_to_highest_checkout(self):
        with TemporaryDirectory() as tmp:
            root = Path(tmp)
            for ver in ("4.10.0", "4.13.0"):
                d = root / "3rd_party" / "opencv" / f"opencv-{ver}" / "platforms" / "js"
                d.mkdir(parents=True)
                (d / "opencv_js.config.py").write_text("white_list = makeWhiteList([])\n")
            (root / "3rd_party" / "opencv" / "opencv_contrib-4.13.0").mkdir(parents=True)

            saved = os.environ.pop("OPENCV_VER", None)
            try:
                found = w.discover_config_path(repo_root=root)
            finally:
                if saved is not None:
                    os.environ["OPENCV_VER"] = saved

        self.assertIn("opencv-4.13.0", found.parts)

    def test_missing_checkout_raises(self):
        with TemporaryDirectory() as tmp:
            saved = os.environ.pop("OPENCV_VER", None)
            try:
                with self.assertRaises(w.ConfigNotFoundError):
                    w.discover_config_path(repo_root=Path(tmp))
            finally:
                if saved is not None:
                    os.environ["OPENCV_VER"] = saved

    def test_env_var_pointing_to_missing_version_raises(self):
        with TemporaryDirectory() as tmp:
            saved = os.environ.get("OPENCV_VER")
            os.environ["OPENCV_VER"] = "0.0.0"
            try:
                with self.assertRaises(w.ConfigNotFoundError):
                    w.discover_config_path(repo_root=Path(tmp))
            finally:
                if saved is None:
                    del os.environ["OPENCV_VER"]
                else:
                    os.environ["OPENCV_VER"] = saved


if __name__ == "__main__":
    unittest.main()
