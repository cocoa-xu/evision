import pytest

from pipeline import _enum_struct_name


@pytest.mark.parametrize(
    "original,expected",
    [
        # 5.0 renamed ccm enums (CCM_TYPE -> CcmType, etc.)
        ("ccm.CcmType", "Evision.CCM.CcmType"),
        ("ccm.ColorCheckerType", "Evision.CCM.ColorCheckerType"),
        # new dnn-engine enums in 5.0
        ("dnn.ActivationType", "Evision.DNN.ActivationType"),
        ("dnn.NaryEltwiseLayer.OPERATION", "Evision.DNN.NaryEltwiseLayer.OPERATION"),
        ("mcc.ColorChart", "Evision.MCC.ColorChart"),
        # classes moved to contrib xfeatures2d keep their Evision module
        ("xfeatures2d.AKAZE", "Evision.XFeatures2D.AKAZE"),
    ],
)
def test_enum_struct_name(original, expected):
    assert _enum_struct_name(original) == expected
