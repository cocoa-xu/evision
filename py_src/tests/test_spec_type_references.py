from types import SimpleNamespace

import pytest

import evision_structures as ES
import helper
from emit.elixir.helpers import map_argtype_in_spec_elixir
from emit.erlang.helpers import map_argtype_in_spec_erlang
from ir.functions import FuncInfo
from pipeline import Pipeline


@pytest.fixture
def enums():
    saved = dict(helper.parsed_enum_modules)
    helper.parsed_enum_modules.clear()
    yield helper.parsed_enum_modules
    helper.parsed_enum_modules.clear()
    helper.parsed_enum_modules.update(saved)


def _enum_decl(name, *consts):
    scope = name.rsplit('.', 1)[0]
    values = [[f'const {scope}.{c}', str(i), [], [], None, ''] for i, c in enumerate(consts)]
    return [f'enum {name}', '', [], values, None, '']


def _pipeline(*namespaces):
    pipeline = Pipeline([], ['elixir'])
    pipeline.parser = SimpleNamespace(namespaces=set(namespaces))
    return pipeline


def test_named_enums_are_registered_by_elixir_module(enums):
    _pipeline('cv', 'cv.ccm').add_enum('cv.ccm.ColorSpace', _enum_decl('cv.ccm.ColorSpace', 'COLOR_SPACE_sRGB'))
    assert enums == {'ccm_ColorSpace': 'Evision.CCM.ColorSpace'}


def test_unnamed_enums_are_not_registered(enums):
    _pipeline('cv').add_enum('cv.Stitcher.<unnamed>', _enum_decl('cv.Stitcher.<unnamed>', 'ORIG_RESOL'))
    assert enums == {}


@pytest.mark.parametrize('classname,argtype,decl,expected', [
    ('ccm_ColorCorrectionModel', 'ColorSpace', ['cv.ccm.ColorCorrectionModel.setColorSpace'], 'Evision.CCM.ColorSpace.enum()'),
    ('', 'MomentsOrder', ['cv.cuda.numMoments'], 'Evision.CUDA.MomentsOrder.enum()'),
    ('QRCodeEncoder_Params', 'QRCodeEncoder_EncodeMode', [], 'Evision.QRCodeEncoder.EncodeMode.enum()'),
    ('cuda_HOG', 'HOGDescriptor_DescriptorStorageFormat', [], 'Evision.HOGDescriptor.DescriptorStorageFormat.enum()'),
])
def test_enums_resolve_from_the_innermost_scope(enums, classname, argtype, decl, expected):
    enums.update({
        'ccm_ColorSpace': 'Evision.CCM.ColorSpace',
        'cuda_MomentsOrder': 'Evision.CUDA.MomentsOrder',
        'QRCodeEncoder_EncodeMode': 'Evision.QRCodeEncoder.EncodeMode',
        'HOGDescriptor_DescriptorStorageFormat': 'Evision.HOGDescriptor.DescriptorStorageFormat',
    })
    assert map_argtype_in_spec_elixir(classname, argtype, True, decl) == expected



@pytest.mark.parametrize('classname,argtype,decl,expected', [
    ('ccm_ColorCorrectionModel', 'ColorSpace', ['cv.ccm.ColorCorrectionModel.setColorSpace'], 'integer()'),
    ('', 'CameraModel', ['cv.fisheye.calibrate'], 'integer()'),
    ('ccm_ColorCorrectionModel', 'Mat', [], '#evision_mat{}'),
])
def test_erlang_specs_take_enums_as_integers(enums, classname, argtype, decl, expected):
    enums.update({'ccm_ColorSpace': 'Evision.CCM.ColorSpace', 'CameraModel': 'Evision.CameraModel'})
    assert map_argtype_in_spec_erlang(classname, argtype, True, decl) == expected


def test_erlang_gpumat_record_has_every_field_to_struct_sets():
    def fields(text, start):
        body = text[text.index(start) + len(start):]
        return {f.split('=')[0].strip() for f in body[:body.index('}')].split(',')}

    hrl = Pipeline([], ['erlang']).evision_erlang_hrl.getvalue()
    assert fields(ES.gpumat_struct_erlang, '#evision_cuda_gpumat{') <= fields(hrl, '-record(evision_cuda_gpumat, {')


@pytest.mark.parametrize('classname,argtype,expected', [
    ('cuda_StereoBM', 'CUDA.StereoBM', 'Evision.CUDA.StereoBM.t()'),
    ('ALIKED_Params', 'ALIKED_Params', 'Evision.ALIKED.Params.t()'),
    ('mcc_CCheckerDetector', 'vector_Ptr_CChecker', 'list(Evision.MCC.CChecker.t())'),
    ('kinfu_KinFu', 'KinFu.KinFu', 'Evision.KinFu.t()'),
    ('', 'TrackerVit', 'Evision.TrackerVit.t()'),
    ('', 'aruco_DetectorParameters', 'Evision.ArUco.DetectorParameters.t()'),
])
def test_structs_name_their_generated_module(enums, classname, argtype, expected):
    assert map_argtype_in_spec_elixir(classname, argtype, True, []) == expected


def test_instance_methods_of_namespaced_classes_take_their_own_struct(enums):
    func = FuncInfo('cuda_StereoConstantSpaceBP', 'setNrPlane', 'cv::cuda::StereoConstantSpaceBP::setNrPlane', False, 'cv.cuda', False)
    func.add_variant(['cv.cuda.StereoConstantSpaceBP.setNrPlane', 'void', [], [['int', 'nr_plane', '', []]], None, ''])
    spec = func.variants[0].generate_spec('elixir', 'setNrPlane', True, False, 'CUDA.StereoConstantSpaceBP')
    assert spec == ('@spec setNrPlane(Evision.CUDA.StereoConstantSpaceBP.t(), integer()) :: '
                    'Evision.CUDA.StereoConstantSpaceBP.t() | {:error, String.t()}')
