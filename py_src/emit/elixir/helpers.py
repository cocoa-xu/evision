#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
from typing import Optional

from helper import *


def when_guard_elixir(func_guard_data: list):
    when_guard = ' '
    if len(func_guard_data) > 0:
        when_guard = ' when '
        when_guard += ' and '.join(func_guard_data) + '\n  '
    return when_guard


def map_argname_elixir(argname, ignore_upper_starting=False, argtype=None, from_struct=False):
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
        name = f"Evision.Internal.Structurise.from_struct({name})"
    return name


def map_argtype_in_docs_elixir(kind: str, argtype: str, classname: str) -> str:
    argtype = argtype.replace("std::", "").strip()
    is_array = argtype.startswith('vector_') or argtype.startswith('vector<')
    if is_array:
        argtype_inner = argtype
        if argtype.startswith('vector<'):
            argtype_inner = argtype[len('vector<'):-1].strip()
        else:
            argtype_inner = argtype[len('vector_'):].strip()
        mapped_type = '[' + map_argtype_in_docs_elixir(kind, argtype_inner, classname) + ']'
        return mapped_type
    if argtype.startswith('Ptr<'):
        if argtype == 'Ptr<char>' or argtype == 'Ptr<uchar>':
            return 'binary()'
        argtype = argtype[len('Ptr<'):-1].strip()
    mapping = {
        'UMat': 'Evision.Mat',
        'Mat': 'Evision.Mat',
        'std::string': 'String',
        'cv::String': 'String',
        'RotatedRect': '{centre={x, y}, size={s1, s2}, angle}',
        'Scalar': 'Evision.scalar()',
        'cv::Scalar': 'Evision.scalar()',
        'int': 'integer()',
    }
    mapped_type = mapping.get(argtype, None)
    if mapped_type is None:
        if is_basic_types(argtype):
            mapped_type = argtype
        elif is_struct(argtype, classname=classname):
            _, mapped_type = is_struct(argtype, also_get='struct_name', classname=classname)
        else:
            mapped_type = argtype
    return mapped_type


def map_argtype_in_spec_elixir(classname: str, argtype: str, is_in: bool, decl: list) -> str:
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

    argtype = argtype.strip()
    if argtype.startswith("cv::"):
        argtype = argtype[4:]

    if is_int_type(argtype):
        return 'integer()'
    elif is_enum_type(argtype, classname, decl):
        return is_enum_type(argtype, classname, decl, get_type='elixir')
    elif argtype == 'bool':
        return 'boolean()'
    elif argtype == 'double':
        return 'number()'
    elif argtype == 'float':
        return 'number()'
    elif argtype in ['String', 'c_string', 'string', 'cv::String', 'std::string']:
        return 'binary()'
    elif argtype in ['char', 'uchar']:
        return 'char()'
    elif argtype == 'void':
        return ':ok'
    elif argtype == 'Range':
        return '{integer(), integer()} | :all'
    elif is_in and argtype in ['Mat', 'UMat', 'cv::Mat', 'cv::UMat']:
        return 'Evision.Mat.maybe_mat_in()'
    elif argtype == 'QRCodeDetectorAruco_Params':
        return 'Evision.QRCodeDetectorAruco.Params.t()'
    elif argtype in ['OCRBeamSearchDecoder::ClassifierCallback', 'OCRHMMDecoder::ClassifierCallback']:
        return 'term()'
    elif argtype == 'ERFilter::Callback':
        return 'term()'
    elif 'IndexParams' in argtype:
        return 'map()'
    elif argtype == 'SearchParams' or argtype == 'Moments':
        return 'map()'
    elif argtype in vec_out_types:
        return vec_out_types[argtype]
    elif argtype in evision_structrised_classes:
        return f'Evision.{argtype}.t()'
    elif argtype in ['Mat', 'cv::Mat', 'UMat', 'cv::UMat']:
        if is_in:
            return 'Evision.Mat.maybe_mat_in()'
        else:
            return 'Evision.Mat.t()'
    elif argtype in ["FeatureDetector", "DescriptorExtractor"]:
        return 'reference() | term()'
    elif argtype in ['GpuMat::Allocator', 'GpuMat_Allocator']:
        return 'reference()'
    elif argtype == 'Status' and classname == 'Stitcher':
        return 'integer()'
    elif argtype == 'Device' and classname == 'ocl_Device':
        return 'Evision.OCL.Device.t()'
    elif argtype == 'Index' and classname == 'flann_Index':
         return 'Evision.Flann.Index.t()'
    elif argtype == 'TrackerVit':
        return 'Evision.TrackerVit'
    elif argtype == 'QRCodeDetectorAruco':
        return 'Evision.QRCodeDetectorAruco'
    elif argtype in ['QRCodeDetectorAruco_Params', 'QRCodeDetectorAruco::Params']:
        return 'Evision.QRCodeDetectorAruco.Params'
    elif argtype in ['aruco_DetectorParameters', 'aruco::DetectorParameters']:
        return 'Evision.Aruco.DetectorParameters'
    elif argtype == 'LayerId':
        return 'term()'
    elif argtype in manual_type_spec_elixir:
        return manual_type_spec_elixir[argtype]
    elif len(decl) > 0 and decl[0].startswith("cv.aruco.") and argtype in ['Board', 'Dictionary']:
        if argtype == 'Board':
            return 'Evision.ArUco.Board.t()'
        elif argtype == 'Dictionary':
            return 'Evision.ArUco.Dictionary.t()'
    elif argtype.startswith('vector_'):
        argtype_inner = argtype[len('vector_'):]
        if argtype == 'vector_char' or argtype == 'vector_uchar':
            return 'binary()'
        spec_type = 'list(' + map_argtype_in_spec_elixir(classname, argtype_inner, is_in, decl) + ')'
        return spec_type
    elif argtype.startswith('std::vector<'):
        if argtype == 'std::vector<char>' or argtype == 'std::vector<uchar>':
            return 'binary()'
        argtype_inner = argtype[len('std::vector<'):-1]
        spec_type = 'list(' + map_argtype_in_spec_elixir(classname, argtype_inner, is_in, decl) + ')'
        return spec_type
    elif argtype.startswith('std::pair<'):
        argtype_inner = ", ".join([map_argtype_in_spec_elixir(classname, a.strip(), is_in, decl) for a in argtype[len('std::pair<'):-1].split(",")])
        spec_type = '{' + argtype_inner + '}'
        return spec_type
    elif is_struct(argtype, classname=classname, decl=decl):
        _, struct_name = is_struct(argtype, also_get='struct_name', classname=classname, decl=decl)
        return f'{struct_name}.t()'
    elif 'Volume' == argtype:
        return 'Evision.KinFu.Volume.t()'
    elif argtype == 'NativeByteArray':
        # cv::NativeByteArray (OpenCV 4.13+) returns raw bytes; the
        # evision_objdetect.hpp converter emits it as a BEAM binary.
        return 'binary()'
    else:
        # print(f'warning: generate_spec: unknown argtype `{argtype}`, input_arg? {is_in}, class={classname}')
        return 'term()'


def map_argtype_to_guard_elixir(argname, argtype, classname: Optional[str] = None):
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
        return f'is_binary({argname})'
    elif argtype == 'char':
        return f'(-128 <= {argname} and {argname} <= 127)'
    elif argtype == 'Range':
        return f'(is_tuple({argname}) or {argname} == :all)'
    elif is_tuple_type(argtype):
        return f'is_tuple({argname})'
    elif argtype in ['Scalar', 'cv::Scalar']:
        return f'(is_number({argname}) or is_tuple({argname}))'
    elif argtype == 'IndexParams' or argtype == 'SearchParams' or argtype == 'Moments':
        return f'is_map({argname})'
    elif is_struct(argtype, classname=classname):
        _, struct_name = is_struct(argtype, also_get='struct_name', classname=classname)
        if struct_name == 'Evision.Feature2D':
            return f''
        if struct_name == 'Evision.Mat':
            return f'(is_struct({argname}, Evision.Mat) or is_struct({argname}, Nx.Tensor) or is_number({argname}) or is_tuple({argname}))'
        else:
            return f'is_struct({argname}, {struct_name})'
    elif 'Volume' == argtype:
        return f'is_struct({argname}, Evision.KinFu.Volume)'
    elif is_ref_or_struct(argtype):
        return f'(is_reference({argname}) or is_struct({argname}))'
    elif is_list_type(argtype):
        return f'is_list({argname})'
    else:
        return ''
