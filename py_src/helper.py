#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from io import StringIO
import re
import inspect
from collections import namedtuple
from format_strings import FormatStrings
from evision_structures import evision_structrised_classes
from typing import Optional


ArgTypeInfo = namedtuple('ArgTypeInfo',
                        ['atype', 'format_str', 'default_value',
                         'strict_conversion', 'is_enum'])
# strict_conversion is False by default
ArgTypeInfo.__new__.__defaults__ = (False,)

simple_argtype_mapping = {
    "bool": ArgTypeInfo("bool", FormatStrings.unsigned_char, "", True, False),
    "size_t": ArgTypeInfo("size_t", FormatStrings.unsigned_long_long, "", True, False),
    "int": ArgTypeInfo("int", FormatStrings.int, "", True, False),
    "float": ArgTypeInfo("float", FormatStrings.float, "", True, False),
    "double": ArgTypeInfo("double", FormatStrings.double, "", True, False),
    "c_string": ArgTypeInfo("char*", FormatStrings.string, '(char*)""', False, False),
    "string": ArgTypeInfo("std::string", FormatStrings.object, None, True, False),
    "Stream": ArgTypeInfo("Stream", FormatStrings.object, 'Stream::Null()', True, False),
}

def handle_ptr(tp):
    if tp.startswith('Ptr_'):
        tp = 'Ptr<' + "::".join(tp.split('_')[1:]) + '>'
    return tp


def normalize_class_name(name):
    return re.sub(r"^cv\.", "", name).replace(".", "_")


def get_type_format_string(arg_type_info):
    if arg_type_info.strict_conversion:
        return FormatStrings.object
    else:
        return arg_type_info.format_str


def argsort(seq):
    # http://stackoverflow.com/questions/3382352/equivalent-of-numpy-argsort-in-basic-python/3383106#3383106
    #lambda version by Tony Veijalainen
    return [x for x,y in sorted(enumerate(seq), key = lambda x: x[1])]


from config.c_types import (
    FORBIDDEN_ARG_TYPES,
    IGNORED_ARG_TYPES,
    IO_BOUND_FUNCS,
    NIF_PREFIX,
    PASS_BY_VAL_TYPES,
    RESERVED_KEYWORDS,
    SPECIAL_HANDLING_FUNCS,
    SPECIAL_HANDLING_FUNCS_ONLY_IN_BEAM,
)
from config.enum_types import ENUM_TYPES
from config.struct_types import (
    MODULE_NAME_MAP,
    SPECIAL_STRUCTS,
    STRICT_MATCH,
    STRUCT_TYPES,
)

module_name_map = MODULE_NAME_MAP


def reserved_keywords():
    return RESERVED_KEYWORDS


def forbidden_arg_types():
    return FORBIDDEN_ARG_TYPES


def ignored_arg_types():
    return IGNORED_ARG_TYPES


def io_bound_funcs():
    return IO_BOUND_FUNCS


def pass_by_val_types():
    return PASS_BY_VAL_TYPES


def evision_nif_prefix():
    return NIF_PREFIX


def special_handling_funcs():
    return SPECIAL_HANDLING_FUNCS


def special_handling_funcs_only_in_beam():
    return SPECIAL_HANDLING_FUNCS_ONLY_IN_BEAM

def handle_inline_math_escaping(text, start_pos=0):
    inline_docs_inline_math_re = re.compile(r'(?:.*?)\\\\f[$\[](.*?)\\\\f[$\]]', re.MULTILINE|re.DOTALL)
    processed = text[:start_pos]
    todo = text[start_pos:]
    if len(todo) == 0:
        return text
    inline_math_match = inline_docs_inline_math_re.match(todo)
    if inline_math_match:
        start = inline_math_match.start(1)
        end = inline_math_match.end(1)
        math_text = inline_math_match.group(1)
        if math_text and len(math_text) > 0:
            # avoid inline math `*` translating to `<em>` when passing through markdown parser
            math_text = math_text.replace('*', r'\\*')
            # avoid inline math `_` translating to `<em>` when passing through markdown parser
            math_text = math_text.replace('_', r'\\_')
            # avoid markdown parser trying to match inline math `[` for markdown `[]()`
            math_text = math_text.replace('[', r'\\[')
            # escape left/right curly brackets
            math_text = math_text.replace(r'\\{', r'\\\\{').replace(r'\\}', r'\\\\}')
            # avoid plus(`+`)/minus(`-`) sign translating as list when passing through markdown parser
            math_lines = ""
            for line in math_text.split("\n"):
                strip_line = line.lstrip()
                if strip_line.startswith('- '):
                    math_lines += line.replace('- ', r'\\- ', 1)
                elif strip_line.startswith('+ '):
                    math_lines += line.replace('+ ', r'\\+ ', 1)
                else:
                    if line.strip() == '=':
                        math_lines = math_lines.rstrip()
                        math_lines += '='
                    else:
                        math_lines += line
                math_lines += "\n"
            math_text = math_lines[:-1]
            replaced = processed + todo[:start] + math_text + todo[end:]
            return handle_inline_math_escaping(replaced, len(processed) + start + len(math_text) + 1)
        else:
            return text
    else:
        return text

def make_elixir_module_names(module_name: Optional[str] = None, separated_ns: Optional[list] = None):
    global module_name_map
    if module_name is not None:
        return module_name_map.get(module_name, f"{module_name[0].upper()}{module_name[1:]}")
    if separated_ns is not None:
        return ".".join([module_name_map.get(n, f"{n[0].upper()}{n[1:]}") for n in separated_ns])


def get_module_func_name(module_func_name: str, is_ns: bool, full_qualified_name: str):
    if len(module_func_name) > 0 and not ('a' <= module_func_name[0] <= 'z'):
        if len(module_func_name) >= 2 and ('a' <= module_func_name[1] <= 'z'):
            module_func_name = module_func_name[0].lower() + module_func_name[1:]
        elif len(module_func_name) == 1:
            module_func_name = module_func_name.lower()
        else:
            mapping = {
                'BFMatcher': 'bfMatcher',
                'DMatch': 'dMatcher',
                'HOGDescriptor': 'hogDescriptor',
                'QRCodeDetector': 'qrCodeDetector',
                'PSNR': 'psnr',
                'LUT': 'lut',
                'KAZE_create': 'kaze_create',
                'ORB_create': 'orb_create',
                'SIFT_create': 'sift_create',
                'AKAZE_create': 'akaze_create',
                'EMD': 'emd',
                'PCAProject': 'pcaProject',
                'PCABackProject': 'pcaBackProject',
                'PCACompute': 'pcaCompute',
                'PCACompute2': 'pcaCompute2',
                'SVBackSubst': 'svBackSubst',
                'SVDecomp': 'svdecomp',
                'RQDecomp3x3': 'rqdecomp3x3',
                'ECCEnabled': 'eccEnabled',
                'BOWKMeansTrainer': 'bowKMeansTrainer',
                'BOWImgDescriptorExtractor': 'bowImgDescriptorExtractor',
                'UMat': 'uMat',
                'NMSBoxes': 'nmsBoxes',
                'NMSBoxesBatched': 'nmsBoxesBatched',
                'NMSBoxesRotated': 'nmsBoxesRotated',
            }
            if module_func_name in mapping:
                module_func_name = mapping[module_func_name]
            else:
                if is_ns and full_qualified_name == 'cv':
                    ignore_names = [
                        'BFMatcher_BFMatcher',                                  # OpenCV.BFMatch.bfMather
                        'BFMatcher_create',                                     # OpenCV.BFMatch.create
                        'BOWImgDescriptorExtractor_BOWImgDescriptorExtractor',  # OpenCV.BOWImgDescriptorExtractor.bowImgDescriptorExtractor
                        'BOWKMeansTrainer_BOWKMeansTrainer',                    # OpenCV.BOWKMeansTrainer.bowKMeansTrainer
                        'BRISK_create',                                         # OpenCV.BRISK.create
                        'DMatch_DMatch',                                        # OpenCV.DMatch.dMatcher
                        'DISOpticalFlow_create',                                # OpenCV.DISOpticalFlow.create
                        'GFTTDetector_create',                                  # OpenCV.GFTTDetector.create
                        'HOGDescriptor_HOGDescriptor',                          # OpenCV.HOGDescriptor.hogDescriptor
                        'HOGDescriptor_getDaimlerPeopleDetector',               # OpenCV.HOGDescriptor.getDaimlerPeopleDetector
                        'HOGDescriptor_getDefaultPeopleDetector',               # OpenCV.HOGDescriptor.getDefaultPeopleDetector
                        'MSER_create',                                          # OpenCV.MSER.create
                        'QRCodeDetector_QRCodeDetector',                        # OpenCV.QRCodeDetector.qrCodeDetector
                        'UMat_UMat',                                            # OpenCV.UMat.uMat
                        'UMat_context',                                         # OpenCV.UMat.context
                        'UMat_queue'                                            # OpenCV.UMat.queue
                    ]
                    if module_func_name in ignore_names:
                        return True, module_func_name
                module_func_name = module_func_name.lower()
    return False, module_func_name

def when_guard(kind: str, func_guard_data: list):
    if kind == 'elixir':
        from emit.elixir.helpers import when_guard_elixir
        return when_guard_elixir(func_guard_data)
    elif kind == 'erlang':
        from emit.erlang.helpers import when_guard_erlang
        return when_guard_erlang(func_guard_data)
    else:
        print(f'warning: when_guard: unknown kind `{kind}`')
    return ' '

def map_argtype_to_type(argtype: str, classname: Optional[str] = None):
    if len(argtype) > 0 and argtype.startswith('Ptr<') and argtype.endswith('>'):
        argtype = argtype[4:-1]
    if len(argtype) > 0 and argtype[-1] == '*':
        argtype = argtype[:-1]

    if is_int_type(argtype) or is_enum_type(argtype, classname, decl=None):
        return 'i'
    elif argtype == 'bool':
        return 'b'
    elif argtype == 'double':
        return 'd'
    elif argtype == 'float':
        return 'd'
    elif argtype == 'vector_char' or argtype == 'vector_uchar' or argtype == 'std::vector<char>' or argtype == 'std::vector<uchar>':
        return 'b'
    elif argtype == 'String' or argtype == 'c_string' or argtype == 'char' or argtype == 'string':
        return 'b'
    elif argtype[:7] == 'vector_' or argtype[:12] == 'std::vector<':
        return 'l'
    elif is_list_type(argtype):
        return 'l'
    elif is_tuple_type(argtype):
        return 'T'
    elif argtype in ['Scalar', 'cv::Scalar']:
        return 'T'
    else:
        if is_struct(argtype, classname=classname):
            if argtype == 'UMat':
                return 'Mat'
            return argtype
        if argtype == 'Moments':
            return '(map)'
        return 't'


def map_argname(kind, argname, **kwargs):
    if kind == 'elixir':
        from emit.elixir.helpers import map_argname_elixir
        return map_argname_elixir(argname, **kwargs)
    elif kind == 'erlang':
        from emit.erlang.helpers import map_argname_erlang
        return map_argname_erlang(argname, **kwargs)
    else:
        print(f'warning: map_argname: unknown kind `{kind}`')


def map_argtype_to_guard(kind, argname, argtype, classname: Optional[str] = None):
    if kind == 'elixir':
        from emit.elixir.helpers import map_argtype_to_guard_elixir
        return map_argtype_to_guard_elixir(argname, argtype, classname=classname)
    elif kind == 'erlang':
        from emit.erlang.helpers import map_argtype_to_guard_erlang
        return map_argtype_to_guard_erlang(argname, argtype, classname=classname)
    else:
        print(f'warning: map_argtype_to_guard: unknown kind `{kind}`')

def is_basic_types(argtype: str):
    argtype = argtype.strip()
    if argtype.startswith("vector<"):
        argtype = argtype[len("vector<"):-1]
        return is_basic_types(argtype)
    return argtype in ['bool', 'float', 'double', 'string', 'void*', 'String', 'c_string', 'Scalar', 'cv::Scalar'] or \
        is_int_type(argtype) or is_tuple_type(argtype) or is_enum_type(argtype, classname=None, decl=None)

def is_int_type(argtype):
    int_types = [
        'uchar',
        'uint8_t',
        'uint16_t',
        'uint32_t',
        'uint64_t',
        'int8_t',
        'int16_t',
        'int32_t',
        'int64_t',
        'unsigned',
        'int',
        'size_t',
        'int64',
        # OpenCV 4.13's IStreamReader takes signed `long long` offsets;
        # BEAM `integer()` is arbitrary precision so mapping is safe.
        'long long',
    ]
    return argtype in int_types

def is_enum_type(argtype, classname, decl, get_type: str=None):
    enum_types = ENUM_TYPES
    if get_type is None:
        return argtype in enum_types
    elif get_type == 'elixir':
        return enum_types[argtype]
    else:
        return 'integer()'

def is_list_type(argtype):
    list_types = [
        'ImageFeatures',
        'MatchesInfo',
        'CameraParams',
        'MatShape'
    ]
    if argtype[:7] == 'vector_' or argtype[len('std::vector<'):] == 'std::vector<':
        return True
    return argtype in list_types

def is_tuple_type(argtype):
    tuple_types = [
        'Rect',
        'cv::Rect',
        'Rect2d',
        'Rect2f',
        'Rect2i',
        'RotatedRect',
        'cv::RotatedRect',
        'TermCriteria',
        'cv::TermCriteria',
        'CvTermCriteria',
        'CvSlice',
        'cv::Point',
        'Point',
        'Point2i',
        'Point2f',
        'Point2d',
        'Point3i',
        'Point3f',
        'Point3d',
        'Size',
    ]
    return argtype in tuple_types

def is_ref_or_struct(argtype: str):
    ref_or_struct_types = [
        'Mat',
        'UMat',
        'Net',
        'DMatch',
        'BFMatcher',
        'BOWImgDescriptorExtractor',
        'BOWKMeansTrainer',
        'CascadeClassifier',
        'FileStorage',
        'FlannBasedMatcher',
        'HOGDescriptor',
        'KalmanFilter',
        'PyRotationWarper',
        'QRCodeDetector',
        'Subdiv2D',
        'TickMeter',
        'VideoWriter',
        'BufferPool',
        'DeviceInfo',
        'AffineBasedEstimator',
        'AffineBestOf2NearestMatcher',
        'BestOf2NearestMatcher',
        'BestOf2NearestRangeMatcher',
        'BlocksGainCompensator',
        'BundleAdjusterAffine',
        'BundleAdjusterAffinePartial',
        'BundleAdjusterRay',
        'BundleAdjusterReproj',
        'ChannelsCompensator',
        'DpSeamFinder',
        'FeatherBlender',
        'GainCompensator',
        'GraphCutSeamFinder',
        'HomographyBasedEstimator',
        'MultiBandBlender',
        'NoBundleAdjuster',
        'DictValue',
        'KeypointsModel',
        'SegmentationModel',
        'BlocksChannelsCompensator',

        'QRCodeEncoder_Params',
        'SimpleBlobDetector_Params',
        'TrackerDaSiamRPN_Params',
        'TrackerGOTURN_Params',
        'TrackerMIL_Params',
        'OriginalClassName_Params',
        'HistogramPhaseUnwrapping_Params',

        'ClassificationModel',
        'TextRecognitionModel',
        'DetectionModel',
        'TextDetectionModel_EAST',
        'TextDetectionModel_DB',
        'IntelligentScissorsMB',

        'Model',
        'Event',
        'AsyncArray',
        'GpuMat_Allocator*',
        'GpuMat::Allocator',
        'GpuMat_Allocator',
        'HostMem',
        'FileNode',
        'Stream'
    ]
    if argtype.startswith('Ptr<'):
        return True
    return argtype in ref_or_struct_types

def get_elixir_module_name(cname, double_quote_if_has_dot=False):
    global module_name_map
    if cname.startswith('cv::'):
        module_name_classname = cname[4:].split("::", maxsplit=2)
        if len(module_name_classname) == 2:
            module_name = module_name_classname[0]
            if module_name in module_name_map:
                mapped_module_name = module_name_map[module_name]
                cname = "cv::" + mapped_module_name + cname[4+len(module_name):]
    # elif cname.startswith("cv::") and 'a' <= cname[4] <= 'z':
    #     print("warning cname=", cname)

    wname = cname
    elixir_module_name = make_elixir_module_names(module_name=wname)
    inner_ns = []
    if wname.startswith('cv::'):
        wname = wname[4:]
        inner_ns = wname.split('::')
        elixir_module_name = make_elixir_module_names(separated_ns=inner_ns)

    elixir_module_name = elixir_module_name.replace('_', '').strip()
    if double_quote_if_has_dot and '.' in elixir_module_name:
        elixir_module_name = f'"{elixir_module_name}"'
    return elixir_module_name

def is_struct(argtype: str, also_get: Optional[str] = None, classname: Optional[str] = None, decl: list=None):
    argtype = argtype.replace("std::", "").replace("cv::", "").replace("::", "_")
    special_structs = SPECIAL_STRUCTS
    struct_types = STRUCT_TYPES
    strict_match = STRICT_MATCH
    second_ret = None

    argtype = argtype.strip()
    second_ret = None
    if argtype.startswith('Ptr<'):
        argtype = argtype[len('Ptr<'):-1].strip()
    arg_is_struct = argtype in struct_types or argtype in special_structs

    if argtype in ["pair<int, double>", "Scalar", "kinfu_VolumeType", "VolumeType"]:
        return False

    is_strict_match = False
    if not arg_is_struct and argtype in strict_match and classname is not None:
        arg_is_struct = classname in strict_match[argtype]
        is_strict_match = arg_is_struct

    if not arg_is_struct:
        if is_basic_types(argtype):
            return False
        if classname is not None and decl is not None and len(classname) == 0 and len(decl) > 0:
            if decl[0].startswith('cv.'):
                classname = decl[0][3:].replace('.', '_')
        if classname:
            module_class = classname.split("_", maxsplit=2)
            if len(module_class) == 2:
                module_name, class_name = module_class[0], module_class[1]
                struct_name = None

                lowercase_start = 'a' <= argtype[0] <= 'z'
                if lowercase_start and not argtype.startswith("vector_"):
                    if module_name == 'ml' and (argtype == 'float*' or argtype == 'TermCriteria'):
                        pass
                    elif module_name == 'legacy' and argtype.startswith('legacy_'):
                        argtype = argtype[len('legacy_'):]
                    elif module_name == 'cuda' and argtype == "cuda_GpuMat":
                        arg_is_struct = True
                        argtype = "Evision.CUDA.GpuMat"
                    elif module_name == 'cuda' and class_name == "Filter":
                        arg_is_struct = True
                        argtype = "Evision.CUDA.Filter"
                    elif module_name == 'detail' and argtype in ['vector<KeyPoint>', 'vector<DMatch>']:
                        arg_is_struct = True
                        if argtype == 'vector<KeyPoint>':
                            struct_name = "[Evision.KeyPoint.t()]"
                        elif argtype == 'vector<DMatch>':
                            struct_name = "[Evision.DMatch.t()]"
                    elif argtype == 'vector<Mat>':
                        arg_is_struct = True
                        struct_name = "[Evision.Mat.t()]"
                    elif argtype == 'vector< pair<int, double> >':
                        arg_is_struct = False
                    elif argtype == 'vector<Template>':
                        arg_is_struct = True
                        struct_name = "[Evision.LineMode.Template.t()]"
                    else:
                        print(f"warning: found class in {module_name} starts with lower case: {argtype}, class_name={class_name}")

                elif argtype.startswith("vector"):
                    if also_get == 'struct_name':
                        if argtype.startswith("vector_"):
                            argtype = argtype.strip()[len("vector_"):]
                        elif argtype.startswith("vector<"):
                            argtype = argtype.strip()[len("vector<"):-1].strip()
                        arg_is_struct, struct_name = is_struct(argtype, classname=classname)
                module_name = module_name_map.get(module_name, module_name)
                if 'a' <= module_name[0] <= 'z':
                    print(f"warning: module name starts with lower case: {module_name}")

                arg_is_struct = arg_is_struct if arg_is_struct else not lowercase_start
                if '*' in argtype:
                    arg_is_struct = False
                argtype = argtype.replace("std::", "").replace("::", ".")

                if also_get == 'struct_name':
                    if struct_name is not None:
                        return arg_is_struct, struct_name
                    return arg_is_struct, f"Evision.{module_name}.{argtype}"
                else:
                    return arg_is_struct

    if also_get == 'struct_name':
        if not is_strict_match:
            second_ret = struct_types.get(argtype, special_structs.get(argtype, argtype))
        else:
            second_ret = strict_match.get(argtype).get(classname)

    if second_ret is None:
        return arg_is_struct
    else:
        return arg_is_struct, second_ret

def map_argtype_in_docs(kind: str, argtype: str, classname: str="") -> str:
    if kind == 'elixir':
        from emit.elixir.helpers import map_argtype_in_docs_elixir
        return map_argtype_in_docs_elixir(kind, argtype, classname)
    elif kind == 'erlang':
        from emit.erlang.helpers import map_argtype_in_docs_erlang
        return map_argtype_in_docs_erlang(kind, argtype, classname)
    else:
        return ''

vec_out_types = {}
vec_dt = ['i', 'f', 'd']
vec_n = [2, 3, 4, 5, 6]
for dt in vec_dt:
    for n in vec_n:
        spec_str = StringIO()
        spec_str.write('{')
        t = 'integer()' if dt == 'i' else 'number()'
        spec_str.write(', '.join([t] * n))
        spec_str.write('}')
        vec_out_types[f'Vec{n}{dt}'] = spec_str.getvalue()

manual_type_spec_map_common = {
    'Rect': '{number(), number(), number(), number()}',
    'Rect2i': '{integer(), integer(), integer(), integer()}',
    'Rect2f': '{number(), number(), number(), number()}',
    'Rect2d': '{number(), number(), number(), number()}',
    'Range': '{number(), number()}',
    'Size': '{number(), number()}',
    'Size2i': '{number(), number()}',
    'Size2f': '{number(), number()}',
    'Size2d': '{number(), number()}',
    'cv::Point': '{number(), number()}',
    'Point': '{number(), number()}',
    'Point2i': '{integer(), integer()}',
    'Point2f': '{number(), number()}',
    'Point2d': '{number(), number()}',
    'Point3f': '{number(), number(), number()}',
    'Point3f': '{number(), number(), number()}',
    'RotatedRect': '{{number(), number()}, {number(), number()}, number()}',
    'TermCriteria': '{integer(), integer(), number()}',
    'cv::TermCriteria': '{integer(), integer(), number()}',
    'MatShape': 'list(integer())',
}

manual_type_spec_elixir = {
    **manual_type_spec_map_common,
    'ImageFeatures': 'Evision.Detail.ImageFeatures.t()',
    'UsacParams': 'Evision.UsacParams.t()',
    'MatchesInfo': 'Evision.Detail.MatchesInfo.t()',
    'CirclesGridFinderParameters': 'Evision.CirclesGridFinderParameters.t()',
    'CameraParams': 'Evision.Detail.CameraParams.t()',
    'KeyPoint': 'Evision.KeyPoint.t()',
    'ParamGrid': 'Evision.ML.ParamGrid.t()',
    'Layer': 'Evision.DNN.Layer.t()',
    'Scalar': 'Evision.scalar()',
}

manual_type_spec_erlang = {
    **manual_type_spec_map_common,
    'ImageFeatures': '#evision_detail_imagefeatures{}',
    'UsacParams': '#evision_usacparams{}',
    'MatchesInfo': '#evision_detail_matchesinfo{}',
    'CirclesGridFinderParameters': '#evision_circlesgridfinderparameters{}',
    'CameraParams': '#evision_detail_cameraparams{}',
    'KeyPoint': '#evision_keypoint{}',
    'ParamGrid': '#evision_ml_paramgrid{}',
    'Layer': '#evision_dnn_layer{}',
    'Scalar': 'number() | {number()} | {number(), number()} | {number(), number(), number()} | {number(), number(), number(), number()}',
}

def map_argtype_in_spec(kind: str, classname: str, argtype: str, is_in: bool, decl: list) -> str:
    if kind == 'elixir':
        from emit.elixir.helpers import map_argtype_in_spec_elixir
        return map_argtype_in_spec_elixir(classname, argtype, is_in, decl)
    elif kind == 'erlang':
        from emit.erlang.helpers import map_argtype_in_spec_erlang
        return map_argtype_in_spec_erlang(classname, argtype, is_in, decl)
    else:
        return ''

def __LINE__():
    return inspect.stack()[1].lineno
