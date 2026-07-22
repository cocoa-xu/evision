import ast

import pytest

import cv_depths
import evision_templates as ET
from py2e import Py2EExpressionGenerator
from erl_enum_expression_generator import ErlEnumExpressionGenerator


def _eval(generator_cls, expr):
    gen = generator_cls()
    gen.visit(ast.parse(expr, mode="eval"))
    return gen.expression


# OpenCV 5.0 core depth codes and structural constants, verified against
# modules/core/include/opencv2/core/hal/interface.h at the 5.0.0 tag. Spelled
# out here so they stay an oracle independent of py_src/cv_depths.py.
DEPTHS = {
    "CV_8U": 0,
    "CV_8S": 1,
    "CV_16U": 2,
    "CV_16S": 3,
    "CV_32S": 4,
    "CV_32F": 5,
    "CV_64F": 6,
    "CV_16F": 7,
    "CV_16BF": 8,
    "CV_Bool": 9,
    "CV_64U": 10,
    "CV_64S": 11,
    "CV_32U": 12,
}

STRUCTURAL = {
    "CV_DEPTH_CURR_MAX": 13,
    "CV_DEPTH_MAX": 32,
}

DEPTH_CONSTANTS = {**DEPTHS, **STRUCTURAL}

CHANNELS = (1, 2, 3, 4)


def test_shared_table_matches_the_opencv_header():
    assert cv_depths.CV_DEPTHS == DEPTHS
    assert cv_depths.CV_CONSTANTS == DEPTH_CONSTANTS
    assert cv_depths.CV_CN_SHIFT == 5


def test_depth_codes_are_contiguous_from_zero():
    # CV_DEPTH_CURR_MAX is derived as len(CV_DEPTHS), which only holds while
    # the codes stay dense and in declaration order.
    assert list(cv_depths.CV_DEPTHS.values()) == list(range(len(DEPTHS)))


@pytest.mark.parametrize("name,value", DEPTH_CONSTANTS.items())
def test_py2e_resolves_depth_constants(name, value):
    assert _eval(Py2EExpressionGenerator, name) == str(value)


@pytest.mark.parametrize("name,value", DEPTH_CONSTANTS.items())
def test_erl_enum_resolves_depth_constants(name, value):
    assert _eval(ErlEnumExpressionGenerator, name) == str(value)


def test_depth_mask_all_expression_is_well_formed():
    # cv::Mat DEPTH_MASK_ALL = (1 << CV_DEPTH_CURR_MAX) - 1 (mat.hpp), the
    # enum that introduced CV_DEPTH_CURR_MAX into the generated surface.
    assert _eval(Py2EExpressionGenerator, "(1 << CV_DEPTH_CURR_MAX) - 1") == "(bsl(1, 13) - 1)"


@pytest.mark.parametrize("name,value", DEPTHS.items())
def test_erlang_template_exposes_depth(name, value):
    depth = name[len("CV_"):]
    src = ET.gen_cv_types_erlang
    assert f"\ncv_{depth}() -> {value}.\n" in src
    assert f"\ncv_{depth}C(Cn) ->\n    cv_maketype(cv_{depth}(), Cn).\n" in src
    for n in CHANNELS:
        assert f"\ncv_{depth}C{n}() ->\n    cv_{depth}C({n}).\n" in src


@pytest.mark.parametrize("name,value", DEPTHS.items())
def test_elixir_template_exposes_depth(name, value):
    depth = name[len("CV_"):]
    src = ET.gen_cv_types_elixir
    assert f"\n  def cv_{depth}, do: {value}\n" in src
    assert f"\n  def cv_{depth}C(cn), do: cv_maketype(cv_{depth}(), cn)\n" in src
    for n in CHANNELS:
        assert f"\n  def cv_{depth}C{n}, do: cv_{depth}C({n})\n" in src


def test_erlang_16u_helper_is_not_misnamed():
    # cv_16SU1/0 was a typo that both took the place of cv_16UC1/0 and left
    # the CV_16UC1 maker missing from the Erlang surface entirely.
    assert "cv_16SU1" not in ET.gen_cv_types_erlang


def test_erlang_template_block_layout():
    assert """
cv_64SC(Cn) ->
    cv_maketype(cv_64S(), Cn).
cv_64SC1() ->
    cv_64SC(1).
cv_64SC2() ->
    cv_64SC(2).
cv_64SC3() ->
    cv_64SC(3).
cv_64SC4() ->
    cv_64SC(4).
""" in ET.gen_cv_types_erlang


def test_elixir_template_block_layout():
    assert """
  def cv_64SC(cn), do: cv_maketype(cv_64S(), cn)
  def cv_64SC1, do: cv_64SC(1)
  def cv_64SC2, do: cv_64SC(2)
  def cv_64SC3, do: cv_64SC(3)
  def cv_64SC4, do: cv_64SC(4)
""" in ET.gen_cv_types_elixir
