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


def push_to_function_group(name, group, value):
    if name:
        if name not in group:
            group[name] = []
        group[name].append(value.getvalue())
