import ast

import pytest

from py2e import Py2EExpressionGenerator
from erl_enum_expression_generator import ErlEnumExpressionGenerator


def _eval(generator_cls, expr):
    gen = generator_cls()
    gen.visit(ast.parse(expr, mode="eval"))
    return gen.expression


# OpenCV 5.0 core depth codes and structural constants, verified against
# modules/core/include/opencv2/core/hal/interface.h at the 5.0.0 tag.
DEPTH_CONSTANTS = {
    "CV_8U": "0",
    "CV_8S": "1",
    "CV_16U": "2",
    "CV_16S": "3",
    "CV_32S": "4",
    "CV_32F": "5",
    "CV_64F": "6",
    "CV_16F": "7",
    "CV_16BF": "8",
    "CV_Bool": "9",
    "CV_64U": "10",
    "CV_64S": "11",
    "CV_32U": "12",
    "CV_DEPTH_CURR_MAX": "13",
    "CV_DEPTH_MAX": "32",
}


@pytest.mark.parametrize("name,value", DEPTH_CONSTANTS.items())
def test_py2e_resolves_depth_constants(name, value):
    assert _eval(Py2EExpressionGenerator, name) == value


@pytest.mark.parametrize("name,value", DEPTH_CONSTANTS.items())
def test_erl_enum_resolves_depth_constants(name, value):
    assert _eval(ErlEnumExpressionGenerator, name) == value


def test_depth_mask_all_expression_is_well_formed():
    # cv::Mat DEPTH_MASK_ALL = (1 << CV_DEPTH_CURR_MAX) - 1 (mat.hpp), the
    # enum that introduced CV_DEPTH_CURR_MAX into the generated surface.
    assert _eval(Py2EExpressionGenerator, "(1 << CV_DEPTH_CURR_MAX) - 1") == "(bsl(1, 13) - 1)"
