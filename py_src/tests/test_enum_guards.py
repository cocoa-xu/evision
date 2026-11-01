import pytest

import helper
from ir.functions import FuncInfo


@pytest.fixture
def enums():
    saved = dict(helper.parsed_enum_modules)
    helper.parsed_enum_modules.clear()
    helper.parsed_enum_modules.update({
        'ccm_ColorSpace': 'Evision.CCM.ColorSpace',
        'dnn_ProfilingMode': 'Evision.DNN.ProfilingMode',
    })
    yield
    helper.parsed_enum_modules.clear()
    helper.parsed_enum_modules.update(saved)


@pytest.mark.parametrize('kind', ['elixir', 'erlang'])
@pytest.mark.parametrize('classname,argtype,decl', [
    ('ccm_ColorCorrectionModel', 'ColorSpace', ['cv.ccm.ColorCorrectionModel.ColorCorrectionModel']),
    ('dnn_Net', 'ProfilingMode', ['cv.dnn.Net.setProfilingMode']),
])
def test_enum_arguments_are_guarded_as_integers(enums, kind, classname, argtype, decl):
    assert helper.map_argtype_to_guard(kind, 'x', argtype, classname=classname, decl=decl) == 'is_integer(x)'


@pytest.mark.parametrize('kind,expected', [
    ('elixir', 'is_struct(x, Evision.DNN.Net)'),
    ('erlang', '(is_tuple(x) and (element(1, x) == evision_dnn_net))'),
])
def test_struct_arguments_keep_their_struct_guard(enums, kind, expected):
    assert helper.map_argtype_to_guard(kind, 'x', 'dnn_Net', classname='dnn_Net', decl=['cv.dnn.Net.forward']) == expected


@pytest.mark.parametrize('kind', ['elixir', 'erlang'])
def test_function_guards_see_the_declaration(enums, kind):
    func = FuncInfo('dnn_Net', 'setProfilingMode', 'cv::dnn::Net::setProfilingMode', False, 'cv.dnn', False)
    func.add_variant(['cv.dnn.Net.setProfilingMode', 'void', [], [['ProfilingMode', 'profilingMode', '', []]], None, ''])
    variant = func.variants[0]
    assert variant.normalized_guard(kind) == ['is_integer(__a0)']
    assert any(guard.startswith('is_integer(') for guard in variant.function_guard(kind))
