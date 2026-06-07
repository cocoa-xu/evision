#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
from typing import Optional

from helper import *


def when_guard_erlang(func_guard_data: list):
    when_guard = ' '
    if len(func_guard_data) > 0:
        when_guard = ' when '
        when_guard += ', '.join(func_guard_data)
    return when_guard


def map_argname_erlang(argname, argtype=None, from_struct=False):
    argname_prefix_re = re.compile(r'^[_]*')
    name = argname_prefix_re.sub('', argname)
    name = f"{name[0:1].upper()}{name[1:]}"
    if from_struct and argtype is not None:
        name = f"evision_internal_structurise:from_struct({name})"
    return name


def map_argtype_in_docs_erlang(kind: str, argtype: str, classname: str) -> str:
    argtype = argtype.replace("std::", "").strip()
    is_array = argtype.startswith('vector_') or argtype.startswith('vector<')
    if is_array:
        argtype_inner = argtype
        if argtype.startswith('vector<'):
            argtype_inner = argtype[len('vector<'):-1].strip()
        else:
            argtype_inner = argtype[len('vector_'):].strip()
        mapped_type = '[' + map_argtype_in_docs_erlang(kind, argtype_inner, classname) + ']'
        return mapped_type
    if argtype.startswith('Ptr<'):
        if argtype == 'Ptr<char>' or argtype == 'Ptr<uchar>':
            return 'binary()'
        argtype = argtype[len('Ptr<'):-1].strip()
    mapping = {
        'UMat': '#evision_mat{}',
        'Mat': '#evision_mat{}',
        'std::string': 'binary()',
        'cv::String': 'binary()',
        'RotatedRect': '{centre={x, y}, size={s1, s2}, angle}',
        'int': 'integer()',
    }
    mapped_type = mapping.get(argtype, None)
    if mapped_type is None:
        if is_basic_types(argtype):
            mapped_type = argtype
            if mapped_type.startswith("evision_"):
                mapped_type = f"#{mapped_type}" + "{}"
        elif is_struct(argtype, classname=classname):
            _, mapped_type = is_struct(argtype, 'struct_name', classname=classname)
            mapped_type = mapped_type.replace(".", "_").lower()
            if not mapped_type.startswith("evision_"):
                mapped_type = f"#evision_{mapped_type}" + "{}"
        else:
            mapped_type = argtype
            if mapped_type.startswith("evision_"):
                mapped_type = f"#{mapped_type}" + "{}"
    return mapped_type


def map_argtype_in_spec_erlang(classname: str, argtype: str, is_in: bool, decl: list) -> str:
    global vec_out_types
    argtype = argtype.strip()
    if len(argtype) > 0 and argtype[-1] == '*':
        if argtype == 'char*' or argtype == 'uchar*':
            return 'binary()'
        argtype = argtype[:-1]
    if argtype.startswith('Ptr<'):
        if argtype == 'Ptr<char>' or argtype == 'Ptr<uchar>':
            return 'binary()'
        argtype = argtype[len('Ptr<'):-1]
    if argtype.startswith('Ptr_'):
        if argtype == 'Ptr_char' or argtype == 'Ptr_uchar':
            return 'binary()'
        argtype = argtype[len('Ptr_'):]

    argtype = argtype.strip()
    if argtype.startswith("cv::"):
        argtype = argtype[4:]

    if is_int_type(argtype) or is_enum_type(argtype, classname, decl):
        return 'integer()'
    elif argtype == 'bool':
        return 'boolean()'
    elif argtype == 'double':
        return 'number()'
    elif argtype == 'float':
        return 'number()'
    elif argtype in ['String', 'c_string', 'string', 'cv::String', 'std::string']:
        return 'unicode:charlist()'
    elif argtype in ['char', 'uchar']:
        return 'char()'
    elif argtype == 'void':
        return 'ok'
    elif argtype == 'Range':
        return '{integer(), integer()} | all'
    elif is_in and argtype in ['Mat', 'UMat', 'cv::Mat', 'cv::UMat']:
        return '#evision_mat{}'
    elif argtype == 'QRCodeDetectorAruco_Params':
        return '#evision_qrcodedetectoraruco_params{}'
    elif argtype in ['OCRBeamSearchDecoder::ClassifierCallback', 'OCRHMMDecoder::ClassifierCallback']:
        return 'term()'
    elif argtype == 'ERFilter::Callback':
        return 'term()'
    elif argtype == 'Modality':
        return '#evision_linemod_modality{}'
    elif argtype in vec_out_types:
        return vec_out_types[argtype]
    elif 'IndexParams' in argtype:
        return 'map()'
    elif argtype in ["FeatureDetector", "DescriptorExtractor"]:
        return 'reference() | term()'
    elif argtype in ['GpuMat::Allocator', 'GpuMat_Allocator']:
        return 'reference()'
    elif argtype == 'Status' and classname == 'Stitcher':
        return 'integer()'
    elif argtype == 'Device' and classname == 'ocl_Device':
        return '#evision_ocl_device{}'
    elif argtype == 'Index' and classname == 'flann_Index':
         return '#evision_flann_index{}'
    elif argtype == 'TrackerVit':
        return '#evision_trackervit{}'
    elif argtype == 'QRCodeDetectorAruco':
        return '#evision_qrcodedetectoraruco{}'
    elif argtype in ['aruco_DetectorParameters', 'aruco::DetectorParameters']:
        return 'term()'
    elif argtype == 'LayerId':
        return 'term()'
    elif argtype == 'GpuMat' or argtype == 'cuda::GpuMat':
        return '#evision_cuda_gpumat{}'
    elif argtype == 'NativeByteArray':
        # cv::NativeByteArray (OpenCV 4.13+) returns raw bytes; the
        # evision_objdetect.hpp converter emits it as a BEAM binary.
        return 'binary()'
    elif argtype == 'SearchParams' or argtype == 'Moments':
        return 'map()'
    elif argtype in ['Board', 'Dictionary'] and len(decl) > 0 and decl[0].startswith("cv.aruco."):
        return '#evision_aruco_board{}'
    elif argtype in ['Board', 'Dictionary'] and len(decl) > 0 and decl[0].startswith("cv.aruco."):
        return '#evision_aruco_board{}'
    elif argtype in manual_type_spec_erlang:
        return manual_type_spec_erlang[argtype]
    elif argtype in evision_structrised_classes:
        ty = argtype.replace('.', '_').lower()
        return f'#evision_{ty}'+'{}'
    elif argtype in ['Mat', 'cv::Mat', 'UMat', 'cv::UMat']:
        return '#evision_mat{}'
    elif argtype.startswith('vector_'):
        argtype_inner = argtype[len('vector_'):]
        if argtype == 'vector_char' or argtype == 'vector_uchar':
            return 'binary()'
        spec_type = 'list(' + map_argtype_in_spec_erlang(classname, argtype_inner, is_in, decl) + ')'
        return spec_type
    elif argtype.startswith('std::vector<'):
        if argtype == 'std::vector<char>' or argtype == 'std::vector<uchar>':
            return 'binary()'
        argtype_inner = argtype[len('std::vector<'):-1]
        spec_type = 'list(' + map_argtype_in_spec_erlang(classname, argtype_inner, is_in, decl) + ')'
        return spec_type
    elif argtype.startswith('std::pair<'):
        argtype_inner = ", ".join([map_argtype_in_spec_erlang(classname, a.strip(), is_in, decl) for a in argtype[len('std::pair<'):-1].split(",")])
        spec_type = '{' + argtype_inner + '}'
        return spec_type
    elif is_struct(argtype, classname=classname, decl=decl):
        _, struct_name = is_struct(argtype, also_get='struct_name', classname=classname, decl=decl)
        ty = struct_name.replace('.', '_').lower()
        return f'#{ty}' + '{}'
    else:
        print(f'warning: generate_spec: unknown argtype `{argtype}`, input_arg? {is_in}, class={classname}')
        return 'term()'


def map_argtype_to_guard_erlang(argname, argtype, classname: Optional[str] = None):
    if argtype == 'vector_char' or argtype == 'vector_uchar' or argtype == 'std::vector<char>' or argtype == 'std::vector<uchar>':
        return f'is_binary({argname})'

    if is_int_type(argtype) or is_enum_type(argtype, classname, None):
        return f'is_integer({argname})'
    elif argtype == 'bool':
        return f'is_boolean({argname})'
    elif argtype == 'double':
        return f'is_number({argname})'
    elif argtype == 'float':
        return f'is_float({argname})'
    elif argtype == 'String' or argtype == 'c_string' or argtype == 'string':
        return f'(is_list({argname}) or is_binary({argname}))'
    elif argtype == 'char':
        return f'is_list({argname})'
    elif argtype == 'Range':
        return f'(is_tuple({argname}) or {argname} == all)'
    elif is_tuple_type(argtype):
        return f'is_tuple({argname})'
    elif argtype in ['Scalar', 'cv::Scalar']:
        return f'(is_number({argname}) or is_tuple({argname}))'
    elif argtype == 'IndexParams' or argtype == 'SearchParams' or argtype == 'Moments':
        return f'is_map({argname})'
    elif is_struct(argtype, classname=classname):
        _, struct_name = is_struct(argtype, also_get='struct_name', classname=classname)
        struct_name = struct_name.replace(".", "_").lower()
        if not struct_name.startswith("evision_"):
            struct_name = f"evision_{struct_name}"
        return f'(is_tuple({argname}) and (element(1, {argname}) == {struct_name}))'
    elif is_ref_or_struct(argtype):
        return f'is_reference({argname})'
    elif is_list_type(argtype):
        return f'is_list({argname})'
    else:
        return ''


def _fold_leading_acronym(name):
    if not name or not name[0].isupper():
        return name
    run = 0
    while run < len(name) and name[run].isupper():
        run += 1
    if run == len(name):
        return name.lower()
    if run == 1:
        return name[0].lower() + name[1:]
    return name[:run - 1].lower() + name[run - 1:]


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
        "GStreamingCompiled": "gStreamingCompiled",

        "EMDHistogramCostExtractor": "emdHistogramCostExtractor",
        "QRCodeDetectorAruco": "qrCodeDetectorAruco",
        "IStreamReader": "iStreamReader",
    }
    if name[0:3] == 'cv_':
        name = name[3:]
    name_parts = name.split('_')
    # special-case casing wins; otherwise fold the leading uppercase acronym
    # (ALIKED -> aliked, ANNIndex -> annIndex, PCABackProject -> pcaBackProject)
    name_parts[0] = namespace_map.get(name_parts[0], _fold_leading_acronym(name_parts[0]))
    name = '_'.join(name_parts)
    if not name[0].islower():
        raise RuntimeError('The function %s is not started with lowercase name' % name)
    return name
