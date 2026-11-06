import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

import hdr_parser  # noqa: E402


def _parse(tmp_path, body):
    header = tmp_path / "sample.hpp"
    header.write_text("namespace cv {\n" + body + "\n}\n", encoding="utf-8")
    return hdr_parser.CppHeaderParser().parse(str(header))


def test_deprecated_external_function_keeps_its_name(tmp_path):
    decls = _parse(
        tmp_path,
        "CV_DEPRECATED_EXTERNAL CV_EXPORTS_W void transposeND("
        "InputArray src, const std::vector<int>& order, OutputArray dst);",
    )
    assert [(d[0], d[1]) for d in decls] == [("cv.transposeND", "void")]


def test_deprecated_function_keeps_its_name(tmp_path):
    decls = _parse(tmp_path, "CV_DEPRECATED CV_EXPORTS_W int legacyCount(InputArray src);")
    assert [(d[0], d[1]) for d in decls] == [("cv.legacyCount", "int")]
