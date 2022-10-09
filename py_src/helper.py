#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
import inspect
from collections import namedtuple
from format_strings import FormatStrings


ArgTypeInfo = namedtuple('ArgTypeInfo',
                        ['atype', 'format_str', 'default_value',
                         'strict_conversion', 'is_enum'])
# strict_conversion is False by default
ArgTypeInfo.__new__.__defaults__ = (False,)


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


def reserved_keywords():
    # Set of reserved keywords for Erlang/Elixir. 
    # Keywords that are reserved in C/C++ are excluded because they can not be
    # used as variables identifiers
    return [
        "true", "false", "nil", "as", "def", "end",
        "rescue", "defmodule",  "defmacro", "when", "in", "fn", "with",
        "of"
    ]


def forbidden_arg_types():
    return ["void*"]


def ignored_arg_types():
    return ["RNG*"]


def io_bound_funcs():
    return [
        # read
        'evision_cv_dnn_dnn_Net_readFromModelOptimizer_static',
        'evision_cv_imread',
        'evision_cv_imreadmulti',
        'evision_cv_readOpticalFlow',
        'evision_cv_dnn_Net_readFromModelOptimizer',
        'evision_cv_dnn_readNet',
        'evision_cv_dnn_readNetFromCaffe',
        'evision_cv_dnn_readNetFromDarknet',
        'evision_cv_dnn_readNetFromModelOptimizer',
        'evision_cv_dnn_readNetFromONNX',
        'evision_cv_dnn_readNetFromTensorflow',
        'evision_cv_dnn_readNetFromTorch',
        'evision_cv_dnn_readTensorFromONNX',
        'evision_cv_dnn_readTorchBlob',
        # write
        'evision_cv_FileStorage_writeComment',
        'evision_cv_imwrite',
        'evision_cv_imwritemulti',
        'evision_cv_writeOpticalFlow',
        'evision_cv_dnn_writeTextGraph',
    ]


def pass_by_val_types(): 
    return ["Point*", "Point2f*", "Rect*", "String*", "double*", "float*", "int*"]


def evision_nif_prefix():
    return 'evision_cv_'


def special_handling_funcs():
    return [
        "{}{}".format(evision_nif_prefix(), name) for name in [
            'imshow',
            'waitKey',
            'destroyWindow',
            'destroyAllWindows',
            'imdecode']
        ]


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
        return when_guard_elixir(func_guard_data)
    elif kind == 'erlang':
        return when_guard_erlang(func_guard_data)
    else:
        print(f'warning: when_guard: unknown kind `{kind}`')
    return ' '

def when_guard_elixir(func_guard_data: list):
    when_guard = ' '
    if len(func_guard_data) > 0:
        when_guard = ' when '
        when_guard += ' and '.join(func_guard_data) + '\n  '
    return when_guard

def when_guard_erlang(func_guard_data: list):
    when_guard = ' '
    if len(func_guard_data) > 0:
        when_guard = ' when '
        when_guard += ', '.join(func_guard_data)
    return when_guard

def map_argtype_to_type(argtype):
    if argtype == 'int' or argtype == 'size_t':
        return 'int'
    elif argtype == 'bool':
        return 'bool'
    elif argtype == 'double':
        return 'numerical'
    elif argtype == 'float':
        return 'float'
    elif argtype == 'String' or argtype == 'c_string' or argtype == 'char':
        return 'binary'
    elif argtype == 'Size' or argtype == 'Scalar' or argtype == 'Point2f' or argtype == 'Point':
        return 'list'
    elif argtype[:7] == 'vector_':
        return 'list'
    else:
        return 't'


def map_argname(kind, argname, **kwargs):
    if kind == 'elixir':
        return map_argname_elixir(argname, **kwargs)
    elif kind == 'erlang':
        return map_argname_erlang(argname, **kwargs)
    else:
        print(f'warning: map_argname: unknown kind `{kind}`')


def map_argname_elixir(argname, ignore_upper_starting=False, argtype=None, from_struct=False):
    struct_types = ["Mat", "vector_Mat", "UMat", "vector_UMat", "GpuMat", "VideoCapture"]
    argname_prefix_re = re.compile(r'^[_]*')
    name = ""
    if argname in reserved_keywords():
        if argname == 'fn':
            name = 'func'
        elif argname == 'end':
            name = 'end_arg'
        else:
            name = f'arg_{argname}'
    else:
        name = argname_prefix_re.sub('', argname)
    if not ignore_upper_starting:
        name = f"{name[0:1].lower()}{name[1:]}"
    if from_struct and argtype is not None:
        if argtype in struct_types:
            name = f"Evision.Internal.Structurise.from_struct({name})"
    return name

def map_argname_erlang(argname):
    argname_prefix_re = re.compile(r'^[_]*')
    name = argname_prefix_re.sub('', argname)
    return f"{name[0:1].upper()}{name[1:]}"


def map_argtype_to_guard(kind, argname, argtype):
    if kind == 'elixir':
        return map_argtype_to_guard_elixir(argname, argtype)
    elif kind == 'erlang':
        return map_argtype_to_guard_erlang(argname, argtype)
    else:
        print(f'warning: map_argtype_to_guard: unknown kind `{kind}`')

def is_int_type(argtype):
    int_types = [
        'int',
        'size_t',
        'ORB_ScoreType',
        'dnn_Backend',
        'dnn_Target',
        'AKAZE_DescriptorType',
        'KAZE_DiffusivityType',
        'DescriptorMatcher_MatcherType',
        'AgastFeatureDetector_DetectorType',
        'FastFeatureDetector_DetectorType',
        'InterpolationFlags',
        'AccessFlag',
        'WaveCorrectKind',
        'flann_distance_t',
        'cvflann_flann_algorithm_t',
        'cvflann_flann_distance_t'
    ]
    return argtype in int_types

def is_list_type(argtype):
    list_types = [
        'ImageFeatures',
        'MatchesInfo',
        'CameraParams',
        'VideoCaptureAPIs',
        'MatShape',

        'UsacParams',
        'CirclesGridFinderParameters',
        'KeyPoint'
    ]
    if argtype[:7] == 'vector_':
        return True
    return argtype in list_types

def is_tuple_type(argtype):
    tuple_types = [
        'Rect',
        'RotatedRect',
        'Range',
        'TermCriteria',
        'CvTermCriteria',
        'CvSlice'
    ]
    return argtype in tuple_types

def is_list_or_tuple(argtype):
    list_or_tuple_types = [
        'Point',
        'Point2f',
        'Point2d',
        'Point3f',
        'Point3d',
        'Size',
        'Scalar',
    ]
    return argtype in list_or_tuple_types

def is_ref_or_struct(argtype: str):
    ref_or_struct_types = [
        'Mat',
        'UMat',
        'Net',

        'GpuMat_Allocator*',
    ]
    if argtype.startswith('Ptr<'):
        return True
    return argtype in ref_or_struct_types

def map_argtype_to_guard_elixir(argname, argtype):
    if is_int_type(argtype):
        return f'is_integer({argname})'
    elif argtype == 'bool':
        return f'is_boolean({argname})'
    elif argtype == 'double':
        return f'is_number({argname})'
    elif argtype == 'float':
        return f'is_float({argname})'
    elif argtype == 'String' or argtype == 'c_string' or argtype == 'string':
        return f'is_binary({argname})'
    elif argtype == 'char':
        return f'is_binary({argname})'
    elif is_list_or_tuple(argtype):
        return f'(is_tuple({argname}) or is_list({argname}))'
    elif is_tuple_type(argtype):
        return f'is_tuple({argname})'
    elif is_ref_or_struct(argtype):
        return f'(is_reference({argname}) or is_struct({argname}))'
    elif is_list_type(argtype):
        return f'is_list({argname})'
    else:
        if argtype == 'LayerId':
            return ''
        if argtype == 'GpuMat' or argtype == 'cuda::GpuMat':
            return f'is_list({argname})'
        if argtype == 'IndexParams' or argtype == 'SearchParams' or argtype == 'Moments':
            return f'is_map({argname})'
        else:
            return ''


def map_argtype_to_guard_erlang(argname, argtype):
    if is_int_type(argtype):
        return f'is_integer({argname})'
    elif argtype == 'bool':
        return f'is_boolean({argname})'
    elif argtype == 'double':
        return f'is_number({argname})'
    elif argtype == 'float':
        return f'is_float({argname})'
    elif argtype == 'String' or argtype == 'c_string' or argtype == 'string':
        return f'is_list({argname})'
    elif argtype == 'char':
        return f'is_list({argname})'
    elif is_list_or_tuple(argtype):
        return f'(is_tuple({argname}) or is_list({argname}))'
    elif is_tuple_type(argtype):
        return f'is_tuple({argname})'
    elif is_ref_or_struct(argtype):
        return f'is_reference({argname})'
    elif is_list_type(argtype):
        return f'is_list({argname})'
    else:
        if argtype == 'LayerId':
            return ''
        if argtype == 'GpuMat' or argtype == 'cuda::GpuMat':
            return f'is_list({argname})'
        if argtype == 'IndexParams' or argtype == 'SearchParams' or argtype == 'Moments':
            return f'is_map({argname})'
        else:
            return ''


def map_uppercase_to_erlang_name(name):
    namespace_map = {
        "AKAZE": "akaze",
        "BFMatcher": "bfMatcher",
        "BOWImgDescriptorExtractor": "bowImgDescriptorExtractor",
        "BOWKMeansTrainer": "bowKMeansTrainer",
        "BOWTrainer": "bowTrainer",
        "BRISK": "brisk",
        "CLAHE": "clahe",
        "DISOpticalFlow": "disOpticalFlow",
        "DMatch": "dMatch",
        "MSER": "mser",
        "KAZE": "kaze",
        "HOGDescriptor": "hogDescriptor",
        "GFTTDetector": "gfttDetector",
        "SIFT": "sift",
        "ORB": "orb",
        "QRCodeDetector": "qrCodeDetector",
        "QRCodeEncoder": "qrCodeEncoder",
        "UMat": "uMat",
        "EMD": "emd",
        "LUT": "lut",
        "PCABackProject": "pcaBackProject",
        "PCACompute": "pcaCompute",
        "PCACompute2": "pcaCompute2",
        "PCAProject": "pcaProject",
        "PSNR": "psnr",
        "RQDecomp3x3": "rqDecomp3x3",
        "SVBackSubst": "svBackSubst",
        "SVDecomp": "svdDecomp",
        "GArrayT": "gArrayT",
        "GCompileArg": "gCompileArg",
        "GComputation": "gComputation",
        "GFrame": "gFrame",
        "GInferInputs": "gInferInputs",
        "GInferListInputs": "gInferListInputs",
        "GInferListOutputs": "gInferListOutputs",
        "GInferOutputs": "gInferOutputs",
        "GMat": "gMat",
        "GMatDesc": "gMatDesc",
        "GOpaqueT": "gOpaqueT",
        "GScalar": "gScalar",
        "GStreamingCompiled": "gStreamingCompiled"
    }
    if name[0:3] == 'cv_':
        name = name[3:]
    name_parts = name.split('_')
    if len(name_parts[0]) > 1:
        if name_parts[0][0].isupper() and name_parts[0][1].islower():
            name_parts[0] = name_parts[0][0].lower() + name_parts[0][1:]
    name_parts[0] = namespace_map.get(name_parts[0], name_parts[0])
    name = '_'.join(name_parts)
    if not name[0].islower():
        raise RuntimeError('The function %s is not started with lowercase name' % name)
    return name


def __LINE__(): 
    return inspect.stack()[1].lineno
