#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from __future__ import print_function

import ast
import sys
from pathlib import Path
import argparse

import hdr_parser
import re
from erl_enum_expression_generator import ErlEnumExpressionGenerator
import evision_templates as ET
import evision_structures as ES
import evision_extra_functions as EF
from helper import *
from namespace import Namespace
from func_info import FuncInfo
from class_info import ClassInfo
from module_generator import ModuleGenerator
from fixes import evision_elixir_fixes, evision_erlang_fixes, evision_elixir_module_fixes, evision_erlang_module_fixes, evision_erlang_fixes_gleam_typed, evision_gleam_module_fixes
from pathlib import Path
import os
from os import makedirs
from shutil import rmtree
from glob import glob


if sys.version_info[0] >= 3:
    from io import StringIO
else:
    from cStringIO import StringIO


class BeamWrapperGenerator(object):
    def __init__(self, enabled_modules, langs):
        self.clear()
        self.argname_prefix_re = re.compile(r'^[_]*')
        self.inline_docs_code_type_re = re.compile(r'@code{.(.*)}')
        self.inline_docs_inline_math_re = re.compile(r'(?:.*?)\\\\f[$\[](.*?)\\\\f[$\]]', re.MULTILINE|re.DOTALL)
        self.enabled_modules = enabled_modules
        self.langs = langs

    def clear(self):
        self.classes = {}
        self.namespaces = {}
        self.consts = {}
        self.enums = {}
        self.typed_enums = {}
        self.enum_names = {}
        self.enum_names_io = StringIO()
        self.enum_names_io_erlang = StringIO()
        self.code_include = StringIO()
        self.code_enums = StringIO()
        self.code_types = StringIO()
        self.code_funcs = StringIO()
        self.code_ns_reg = StringIO()

        # lib/generated/evision_nif.ex
        self.evision_nif = StringIO()
        # src/generated/evision_nif.erl
        self.evision_nif_erlang = StringIO()
        # gleam_src/evision_nif.erl
        self.evision_nif_gleam = StringIO()

        # lib/generated/evision.ex
        self.evision_elixir = StringIO()
        # lib/generated/evision_constant.ex
        self.evision_constant_elixir = StringIO()

        # src/generated/evision.erl
        self.evision_erlang = StringIO()
        # src/generated/evision_constant.erl
        self.evision_constant_erlang = StringIO()
        # src/evision.hrl
        self.evision_erlang_hrl = StringIO()
        
        # gleam_src/evision.erl
        self.evision_gleam = StringIO()
        # gleam_src/evision.gleam
        self.evision_gleam_typed = StringIO()
        # gleam_src/evision_constant.erl
        self.evision_constant_gleam = StringIO()
        # gleam_src/evision.hrl
        self.evision_gleam_hrl = StringIO()

        self.evision_ex = ModuleGenerator("Evision")
        self.evision_elixir.write('defmodule Evision do\n')
        self.evision_elixir.write('  import Kernel, except: [apply: 2, apply: 3, min: 2, max: 2]\n\n')
        self.evision_elixir.write('  @doc false\n')
        self.evision_elixir.write('  def to_struct(any), do: Evision.Internal.Structurise.to_struct(any)\n\n')
        self.evision_elixir.write(
'''
  @typedoc """
  Scalars are n-element tuples, where n can be 1, 2, 3, or 4.

  When passing a single number `X` as a scalar, it will be converted to `{X, 0, 0, 0}`.

  When passing a tuple with more than 4 elements, it will raise an error.
  """
  @type scalar :: number() | {number()} | {number(), number()} | {number(), number(), number()} | {number(), number(), number(), number()}
''')

        self.evision_constant_elixir.write('defmodule Evision.Constant do\n')
        self.evision_constant_elixir.write('  import Bitwise\n')
        self.evision_constant_elixir.write(ET.gen_cv_types_elixir)

        self.evision_erlang.write('-module(evision).\n-compile(nowarn_export_all).\n-compile([export_all]).\n-include("evision.hrl").\n\n')
        self.evision_erlang.write('to_struct(Any) ->\n  evision_internal_structurise:to_struct(Any).\n\n')
        self.evision_gleam.write('-module(evision).\n-compile(nowarn_export_all).\n-compile([export_all]).\n-include("evision.hrl").\n\n')
        self.evision_gleam.write('to_struct(Any) ->\n  evision_internal_structurise:to_struct(Any).\n\n')
        self.evision_gleam_typed.write('import evision/mat.{type Mat}\n')
        self.evision_gleam_typed.write('import gleam/dict.{type Dict}\n\n')

        self.evision_constant_erlang.write('-module(evision_constant).\n-compile(nowarn_export_all).\n-compile([export_all]).\n\n')
        self.evision_constant_gleam.write('-module(evision_constant).\n-compile(nowarn_export_all).\n-compile([export_all]).\n\n')

        self.evision_erlang_hrl.write(
            "-record(evision_mat, {channels, dims, type, raw_type, shape, ref}).\n"
            "-record(evision_cuda_gpumat, {channels, type, raw_type, shape, ref, elemSize}).\n"
            "-record(evision_videocapture, {fps, frame_count, frame_width, frame_height, isOpened, ref}).\n"
        )
        self.evision_gleam_hrl.write(
            "-record(evision_mat, {channels, dims, type, raw_type, shape, ref}).\n"
            "-record(evision_cuda_gpumat, {channels, type, raw_type, shape, ref, elemSize}).\n"
            "-record(evision_videocapture, {fps, frame_count, frame_width, frame_height, isOpened, ref}).\n"
        )

        self.evision_modules = {}
        self.code_type_publish = StringIO()
        self.py_signatures = dict()
        self.class_idx = 0
        self.evision_nif_names = dict()
        self.not_struct_types = dict()

    def get_export_scope_name(self, original_scope_name):
        # Outer classes should be registered before their content - inner classes in this case
        class_scope = self.classes.get(normalize_class_name(original_scope_name), None)

        if class_scope:
            return class_scope.full_export_name

        # Otherwise it is a namespace.
        # If something is messed up at this point - it will be revelead during
        # library import
        return original_scope_name

    def add_class(self, stype, name, decl):
        classinfo = ClassInfo(name, decl, self)
        classinfo.decl_idx = self.class_idx
        self.class_idx += 1

        if classinfo.name in self.classes:
            print("Generator error: class %s (cname=%s) already exists" \
                % (classinfo.name, classinfo.cname))
            # sys.exit(-1)
            return
        
        self.classes[classinfo.name] = classinfo

        # Add Class to json file.
        namespace, classes, name = self.split_decl_name(name)
        namespace = '.'.join(namespace)
        name = '_'.join(classes+[name])

        py_name = 'cv.' + classinfo.wname  # use wrapper name
        py_signatures = self.py_signatures.setdefault(classinfo.cname, [])
        py_signatures.append(dict(name=py_name))
        #print('class: ' + classinfo.cname + " => " + py_name)

    def split_decl_name(self, name):
        chunks = name.split('.')
        namespace = chunks[:-1]
        classes = []
        while namespace and '.'.join(namespace) not in self.parser.namespaces:
            classes.insert(0, namespace.pop())
        return namespace, classes, chunks[-1]

    def add_const(self, name, decl, original_name=None):
        (module_name, erl_const_name) = name.split('.')[-2:]
        
        if original_name.startswith('cvflann.'):
            original_name = original_name.replace('cvflann.', 'cv.flann.')
            name = name.replace('cvflann.', 'cv.flann.')
            
            if original_name == 'cv.flann.flann_algorithm_t':
                if not name.startswith('cv.flann.FLANN_INDEX_'):
                    return
            if original_name == 'cv.flann.flann_centers_init_t':
                if not name.startswith('cv.flann.FLANN_CENTERS_'):
                    return
            if original_name == 'cv.flann.flann_distance_t':
                if not name.startswith('cv.flann.FLANN_DIST_'):
                    return

        if original_name is not None:
            if not original_name.startswith('cv.'):
                print(f'warning: enum name {original_name} does not start with `cv.`')
                sys.exit(-1)
            if original_name.endswith('.<unnamed>'):
                original_name = original_name[:-10]
            if original_name == 'cv':
               struct_name = 'Evision'
            else:
                original_name = original_name[3:]
                if is_struct(original_name):
                    _, struct_name = is_struct(original_name, also_get='struct_name')
                else:
                    mname = {
                        'aruco.CornerRefineMethod': 'Evision.ArUco.CornerRefineMethod',
                        'aruco.PatternPositionType': 'Evision.ArUco.PatternPositionType',
                        'aruco.PredefinedDictionaryType': 'Evision.ArUco.PredefinedDictionaryType',
                        'bgsegm.LSBPCameraMotionCompensation': 'Evision.BgSegm.LSBPCameraMotionCompensation',
                        'bioinspired': 'Evision.Bioinspired',
                        'ccm.CCM_TYPE': 'Evision.CCM.CCM_TYPE',
                        'ccm.COLOR_SPACE': 'Evision.CCM.COLOR_SPACE',
                        'ccm.CONST_COLOR': 'Evision.CCM.CONST_COLOR',
                        'ccm.DISTANCE_TYPE': 'Evision.CCM.DISTANCE_TYPE',
                        'ccm.INITIAL_METHOD_TYPE': 'Evision.CCM.INITIAL_METHOD_TYPE',
                        'ccm.LINEAR_TYPE': 'Evision.CCM.LINEAR_TYPE',
                        'cuda.SURF_CUDA.KeypointLayout': 'Evision.CUDA.SURF_CUDA.KeypointLayout',
                        'cuda.DemosaicTypes': 'Evision.CUDA.DemosaicTypes',
                        'cuda.AlphaCompTypes': 'Evision.CUDA.AlphaCompTypes',
                        'cuda.ConnectedComponentsAlgorithmsTypes': 'Evision.CUDA.ConnectedComponentsAlgorithmsTypes',
                        'cuda.MomentsOrder': 'Evision.CUDA.MomentsOrder',
                        'cudacodec.Codec': 'Evision.CUDACodec.Codec',
                        'cudacodec.ColorFormat': 'Evision.CUDACodec.ColorFormat',
                        'cudacodec.EncodeParamsRcMode': 'Evision.CUDACodec.EncodeParamsRcMode',
                        'cudacodec.EncodeMultiPass': 'Evision.CUDACodec.EncodeMultiPass',
                        'cudacodec.EncodeProfile': 'Evision.CUDACodec.EncodeProfile',
                        'cudacodec.EncodePreset': 'Evision.CUDACodec.EncodePreset',
                        'cudacodec.EncodeTuningInfo': 'Evision.CUDACodec.EncodeTuningInfo',
                        'cudacodec.ChromaFormat': 'Evision.CUDACodec.ChromaFormat',
                        'cudacodec.DeinterlaceMode': 'Evision.CUDACodec.DeinterlaceMode',
                        'cudacodec.VideoReaderProps': 'Evision.CUDACodec.VideoReaderProps',
                        'detail.Blender': 'Evision.Detail.Blender',
                        'detail.CvFeatureParams.FeatureType': 'Evision.Detail.CvFeatureParams.FeatureType',
                        'detail.DpSeamFinder.CostFunction': 'Evision.Detail.DpSeamFinder.CostFunction',
                        'detail.ExposureCompensator': 'Evision.Detail.ExposureCompensator',
                        'detail.GraphCutSeamFinderBase.CostType': 'Evision.Detail.GraphCutSeamFinderBase.CostType',
                        'detail.SeamFinder': 'Evision.Detail.SeamFinder',
                        'detail.Timelapser': 'Evision.Detail.Timelapser',
                        'detail.TrackerContribSamplerCSC': 'Evision.Detail.TrackerContribSamplerCSC',
                        'detail.TrackerSamplerCS': 'Evision.Detail.TrackerSamplerCS',
                        'detail.TrackerSamplerCSC.MODE': 'Evision.Detail.TrackerSamplerCSC.MODE',
                        'detail.WaveCorrectKind': 'Evision.Detail.WaveCorrectKind',
                        'fisheye': 'Evision.FishEye',
                        'ft': 'Evision.Ft',
                        'flann': 'Evision.Flann',
                        'flann.flann_algorithm_t': 'Evision.Flann.Algorithm',
                        'flann.flann_centers_init_t': 'Evision.Flann.CentersInit',
                        'flann.flann_log_level_t': 'Evision.Flann.LogLevel',
                        'flann.flann_distance_t': 'Evision.Flann.Distance',
                        'flann.flann_datatype_t': 'Evision.Flann.Datatype',
                        'kinfu.VolumeType': 'Evision.KinFu.VolumeType',
                        'mcc.TYPECHART': 'Evision.MCC.TYPECHART',
                        'ml.LogisticRegression.Methods': 'Evision.ML.LogisticRegression.Methods',
                        'ml.SVMSGD.MarginType': 'Evision.ML.SVMSGD.MarginType',
                        'multicalib.MultiCameraCalibration': 'Evision.MultiCalib.MultiCameraCalibration',
                        'omnidir': 'Evision.Omnidir',
                        'rgbd.DepthCleaner.DEPTH_CLEANER_METHOD': 'Evision.RGBD.DepthCleaner.DEPTH_CLEANER_METHOD',
                        'rgbd.Odometry': 'Evision.RGBD.Odometry',
                        'rgbd.OdometryFrame': 'Evision.RGBD.OdometryFrame',
                        'rgbd.RgbdNormals.RGBD_NORMALS_METHOD': 'Evision.RGBD.RgbdNormals.RGBD_NORMALS_METHOD',
                        'rgbd.RgbdPlane.RGBD_PLANE_METHOD': 'Evision.RGBD.RgbdPlane.RGBD_PLANE_METHOD',
                        'stereo.StereoBinaryBM': 'Evision.Stereo.StereoBinaryBM',
                        'stereo.StereoBinarySGBM': 'Evision.Stereo.StereoBinarySGBM',
                        'stereo.StereoMatcher': 'Evision.Stereo.StereoMatcher',
                        'text': 'Evision.Text',
                        'text.classifier_type': 'Evision.Text.ClassifierType',
                        'text.decoder_mode': 'Evision.Text.DecoderMode',
                        'text.erGrouping_Modes': 'Evision.Text.ERGroupingModes',
                        'text.ocr_engine_mode': 'Evision.Text.OCREngineMode',
                        'text.page_seg_mode': 'Evision.Text.PageSegMode',
                        'xfeatures2d.BEBLID.BeblidSize': 'Evision.XFeatures2D.BEBLID.BeblidSize',
                        'xfeatures2d.DAISY.NormalizationType': 'Evision.XFeatures2D.DAISY.NormalizationType',
                        'xfeatures2d.PCTSignatures.DistanceFunction': 'Evision.XFeatures2D.PCTSignatures.DistanceFunction',
                        'xfeatures2d.PCTSignatures.PointDistribution': 'Evision.XFeatures2D.PCTSignatures.PointDistribution',
                        'xfeatures2d.PCTSignatures.SimilarityFunction': 'Evision.XFeatures2D.PCTSignatures.SimilarityFunction',
                        'xfeatures2d.TEBLID.TeblidSize': 'Evision.XFeatures2D.TEBLID.TeblidSize',
                        'ximgproc.AngleRangeOption': 'Evision.XImgProc.AngleRangeOption',
                        'ximgproc.EdgeAwareFiltersList': 'Evision.XImgProc.EdgeAwareFiltersList',
                        'ximgproc.EdgeDrawing.GradientOperator': 'Evision.XImgProc.EdgeDrawing.GradientOperator',
                        'ximgproc.HoughDeskewOption': 'Evision.XImgProc.HoughDeskewOption',
                        'ximgproc.HoughOp': 'Evision.XImgProc.HoughOp',
                        'ximgproc.LocalBinarizationMethods': 'Evision.XImgProc.LocalBinarizationMethods',
                        'ximgproc.SLICType': 'Evision.XImgProc.SLICType',
                        'ximgproc.ThinningTypes': 'Evision.XImgProc.ThinningTypes',
                        'ximgproc.WMFWeightType': 'Evision.XImgProc.WMFWeightType',
                        'img_hash.BlockMeanHashMode': 'Evision.ImgHash.BlockMeanHashMode',
                        'structured_light': 'Evision.StructuredLight',
                        'stereo': 'Evision.Stereo',
                        'detail.TestOp': 'Evision.Detail.TestOp',
                        'cuda.HostMem.AllocType': 'Evision.CUDA.HostMem.AllocType',
                        'cuda.Event.CreateFlags': 'Evision.CUDA.Event.CreateFlags',
                        'cuda.FeatureSet': 'Evision.CUDA.FeatureSet',
                        'cuda.DeviceInfo.ComputeMode': 'Evision.CUDA.DeviceInfo.ComputeMode',
                        '_InputArray.KindFlag': 'Evision.Mat.KindFlag',
                        '_OutputArray.DepthMask': 'Evision.Mat.DepthMask',
                        'ocl.Device': 'Evision.OCL.Device',
                        'ocl.KernelArg': 'Evision.OCL.KernelArg',
                        'ocl.OclVectorStrategy': 'Evision.OCL.OclVectorStrategy',
                        'ogl.Buffer.Target': 'Evision.OGL.Buffer.Target',
                        'ogl.Buffer.Access': 'Evision.OGL.Buffer.Access',
                        'ogl.Texture2D.Format': 'Evision.OGL.Texture2D.Format',
                        'ogl.RenderModes': 'Evision.OGL.RenderModes',
                        'flann.FlannIndexType': 'Evision.Flann.FlannIndexType',
                        'ml.VariableTypes': 'Evision.ML.VariableTypes',
                        'ml.ErrorTypes': 'Evision.ML.ErrorTypes',
                        'ml.SampleTypes': 'Evision.ML.SampleTypes',
                        'ml.StatModel.Flags': 'Evision.ML.StatModel.Flags',
                        'ml.KNearest.Types': 'Evision.ML.KNearest.Types',
                        'ml.SVM.Types': 'Evision.ML.SVM.Types',
                        'ml.SVM.KernelTypes': 'Evision.ML.SVM.KernelTypes',
                        'ml.SVM.ParamTypes': 'Evision.ML.SVM.ParamTypes',
                        'ml.EM.Types': 'Evision.ML.EM.Types',
                        'ml.EM': 'Evision.ML.EM',
                        'ml.DTrees.Flags': 'Evision.ML.DTrees.Flags',
                        'ml.Boost.Types': 'Evision.ML.Boost.Types',
                        'ml.ANN_MLP.TrainingMethods': 'Evision.ML.ANN_MLP.TrainingMethods',
                        'ml.ANN_MLP.ActivationFunctions': 'Evision.ML.ANN_MLP.ActivationFunctions',
                        'ml.ANN_MLP.TrainFlags': 'Evision.ML.ANN_MLP.TrainFlags',
                        'ml.LogisticRegression.RegKinds': 'Evision.ML.LogisticRegression.RegKinds',
                        'ml.SVMSGD.SvmsgdType': 'Evision.ML.SVMSGD.SvmsgdType',
                        'xphoto.TransformTypes': 'Evision.XPhoto.TransformTypes',
                        'xphoto.Bm3dSteps': 'Evision.XPhoto.Bm3dSteps',
                        'xphoto.InpaintTypes': 'Evision.XPhoto.InpaintTypes',
                        'dnn.Backend': 'Evision.DNN.Backend',
                        'dnn.Target': 'Evision.DNN.Target',
                        'dnn.DataLayout': 'Evision.DNN.DataLayout',
                        'dnn.ImagePaddingMode': 'Evision.DNN.ImagePaddingMode',
                        'dnn.SoftNMSMethod': 'Evision.DNN.SoftNMSMethod',
                    }
                    if not any([x[0].islower() or x[0] == '_' for x in original_name.split('.')]):
                        struct_name = f'Evision.{original_name}'
                    else:
                        the_name = mname.get(original_name, None)
                        if the_name is not None:
                            struct_name = the_name
                        else:
                            print(f'warning: enum name {original_name} is not a struct')
                            struct_name = '!fixme!'
            typed_enum = self.typed_enums.get(struct_name, None)
            if typed_enum is None:
                self.typed_enums[struct_name] = list()
        else:
            print(f'warning: const named `{name}` does not have an original name')
            struct_name = '!fixme!'

        val = decl[1]
        skip_this = False
        if val == "std::numeric_limits<uint8_t>::max()":
            val_erlang = 255
        else:
            val_tree = ast.parse(val, mode='eval')
            val_gen = ErlEnumExpressionGenerator()
            val_gen.visit(val_tree)
            skip_this = val_gen.skip_this
            val = val_gen.expression
            val_erlang = val_gen.expression_erlang

        if not skip_this:
            erl_const_name = map_argname('elixir', erl_const_name, ignore_upper_starting=True)
            if self.enum_names.get(val, None) is not None:
                val = f'cv_{val}()'

            self.typed_enums[struct_name].append((erl_const_name, val, val_erlang))
            
            if self.enum_names.get(erl_const_name, None) is None:
                self.enum_names[erl_const_name] = val
                self.enum_names_io.write(f"  def cv_{erl_const_name}, do: {val}\n")
                self.enum_names_io_erlang.write(f"cv_{erl_const_name}() ->\n    {val_erlang}.\n")
            else:
                if self.enum_names[erl_const_name] != val:
                    erl_const_name = map_argname('elixir', f'{module_name}_{erl_const_name}', ignore_upper_starting=True)
                    if self.enum_names.get(erl_const_name, None) is None:
                        self.enum_names[erl_const_name] = val
                        self.enum_names_io.write(f"  def cv_{erl_const_name}, do: {val}\n")
                        self.enum_names_io_erlang.write(f"cv_{erl_const_name}() ->\n    {val_erlang}.\n")
                    else:
                        raise "duplicated constant name"

        cname = name.replace('.', '::')
        namespace, classes, name = self.split_decl_name(name)
        namespace = '.'.join(namespace)
        name = '_'.join(classes+[name])
        ns = self.namespaces.setdefault(namespace, Namespace())
        if name in ns.consts:
            print("Generator error: constant %s (cname=%s) already exists" \
                % (name, cname))
            return
        ns.consts[name] = cname

        value = decl[1]
        py_name = '.'.join([namespace, name])
        py_signatures = self.py_signatures.setdefault(cname, [])
        py_signatures.append(dict(name=py_name, value=value))
        #print(cname + ' => ' + str(py_name) + ' (value=' + value + ')')

    def add_enum(self, name, decl):
        original_name = name
        wname = normalize_class_name(name)
        if wname.endswith("<unnamed>"):
            wname = None
        else:
            self.enums[wname] = name
        const_decls = decl[3]
        for decl in const_decls:
            enum_name = decl[0].replace("const ", "").strip()
            self.add_const(enum_name, decl, original_name=original_name)

    def add_func(self, decl):
        namespace, classes, barename = self.split_decl_name(decl[0])
        cname = "::".join(namespace+classes+[barename])
        name = barename
        classname = ''
        bareclassname = ''
        if classes:
            classname = normalize_class_name('.'.join(namespace+classes))
            bareclassname = classes[-1]
        namespace_str = '.'.join(namespace)

        isconstructor = name == bareclassname
        is_static = False
        isphantom = False
        mappable = None
        for m in decl[2]:
            if m == "/S":
                is_static = True
            elif m == "/phantom":
                isphantom = True
                cname = cname.replace("::", "_")
            elif m.startswith("="):
                name = m[1:]
            elif m.startswith("/mappable="):
                mappable = m[10:]
                self.classes[classname].mappables.append(mappable)
                return

        if isconstructor:
            name = "_".join(classes[:-1]+[name])

        if is_static:
            # Add it as a method to the class
            func_map = self.classes[classname].methods
            func = func_map.setdefault(name, FuncInfo(classname, name, cname, isconstructor, namespace_str, is_static))
            func.add_variant(decl, isphantom)

            # Add it as global function
            g_name = "_".join(classes+[name])
            w_classes = []
            for i in range(0, len(classes)):
                classes_i = classes[:i+1]
                classname_i = normalize_class_name('.'.join(namespace+classes_i))
                w_classname = self.classes[classname_i].wname
                namespace_prefix = normalize_class_name('.'.join(namespace)) + '_'
                if w_classname.startswith(namespace_prefix):
                    w_classname = w_classname[len(namespace_prefix):]
                w_classes.append(w_classname)
            g_wname = "_".join(w_classes+[name])
            func_map = self.namespaces.setdefault(namespace_str, Namespace()).funcs
            func = func_map.setdefault(g_name, FuncInfo("", g_name, cname, isconstructor, namespace_str, False))
            func.add_variant(decl, isphantom)
            if g_wname != g_name:  # TODO OpenCV 5.0
                wfunc = func_map.setdefault(g_wname, FuncInfo("", g_wname, cname, isconstructor, namespace_str, False))
                wfunc.add_variant(decl, isphantom)
        else:
            if classname and not isconstructor:
                if not isphantom:
                    cname = barename
                func_map = self.classes[classname].methods
            else:
                func_map = self.namespaces.setdefault(namespace_str, Namespace()).funcs

            func = func_map.setdefault(name, FuncInfo(classname, name, cname, isconstructor, namespace_str, is_static))
            func.add_variant(decl, isphantom)

        if classname and isconstructor:
            self.classes[classname].constructor = func

    def handle_custom_file(self, module_text):
        check_defs = None
        with open(module_text, "rt") as f:
            for line in f:
                if line.startswith("// @evision enable_with: "):
                    with_module = line[len("// @evision enable_with: "):].strip()
                    if check_defs is None:
                        check_defs = with_module in self.enabled_modules
                    else:
                        check_defs = check_defs and (with_module in self.enabled_modules)

        if check_defs is True or check_defs is None:
            with open(module_text, "rt") as f:
                for line in f:
                    line = line.strip()
                    if line.startswith("// @evision c: "):
                        parts = line[len("// @evision c: "):].split(',')
                        if len(parts) != 3:
                            raise Exception(f'Invalid comment: {line}')
                        erl_name = parts[0].strip()
                        func_name = parts[1].strip()
                        func_arity = parts[2].strip()

                        if func_name.endswith('_read') or func_name.endswith('_load_static') or \
                                func_name.endswith('_write') or func_name.endswith('_save') or \
                                func_name in io_bound_funcs():
                            self.code_ns_reg.write(f'    F_IO({erl_name}, {func_name}, {func_arity}),\n')
                        else:
                            self.code_ns_reg.write(f'    F_CPU({erl_name}, {func_name}, {func_arity}),\n')
                        if int(func_arity) > 0:
                            self.evision_nif_erlang.write(f'{erl_name}(_opts) ->\n    not_loaded(?LINE).\n')
                            self.evision_nif_gleam.write(f'{erl_name}(_opts) ->\n    not_loaded(?LINE).\n')
                        else:
                            self.evision_nif_erlang.write(f'{erl_name}() ->\n    not_loaded(?LINE).\n')
                            self.evision_nif_gleam.write(f'{erl_name}() ->\n    not_loaded(?LINE).\n')
                    elif line.startswith("// @evision nif: "):
                        line = line[len("// @evision nif: "):].strip()
                        self.evision_nif.write(f'  {line}\n')

    def gen_namespace(self):
        for ns_name in self.namespaces:
            ns = self.namespaces[ns_name]
            wname = normalize_class_name(ns_name)
            for name, func in sorted(ns.funcs.items()):
                if wname == 'cv':
                    self.evision_ex.gen_ns_method(wname, name, func, namespace_list=None)
                else:
                    module_file_generator, _ = self.get_module_writer(wname, wname=wname, name=name, is_ns=True)
                    module_file_generator.gen_ns_method(wname, name, func, namespace_list=None)
                self.code_ns_reg.write(func.get_tab_entry())

        modules_dir = Path(self.output_path) / 'modules'
        for module_text in modules_dir.glob('*.h'):
            self.handle_custom_file(module_text)
        backend_dir = Path(self.output_path) / 'modules' / 'evision_backend'
        for module_text in backend_dir.glob('*.h'):
            self.handle_custom_file(module_text)

    def gen_enum_reg(self, enum_name):
        name_seg = enum_name.split(".")
        if len(name_seg) >= 2 and name_seg[-1] == name_seg[-2]:
            enum_name = ".".join(name_seg[:-1])

        wname = normalize_class_name(enum_name)
        cname = enum_name.replace(".", "::")

        code = ""
        if re.sub(r"^cv\.", "", enum_name) != wname:
            code += "typedef {0} {1};\n".format(cname, wname)
        code += "CV_ERL_FROM_ENUM({0});\nCV_ERL_TO_ENUM({0});\n\n".format(wname)
        self.code_enums.write(code)

    def save(self, path, name, buf):
        outdir_path = path
        outdir = Path(outdir_path)
        if not outdir.exists():
            outdir.mkdir(parents=True, exist_ok=True)
        with open(f"{outdir_path}/{name}", "wt", encoding='utf-8') as f:
            if name.endswith(".h"):
                f.write("#include <erl_nif.h>\n")
                f.write('#include "nif_utils.hpp"\n')
                f.write('using namespace evision::nif;\n')
            if type(buf) == str:
                f.write(buf)
            else:
                f.write(buf.getvalue())

    def save_json(self, path, name, value):
        import json
        with open(path + "/" + name, "wt", encoding='utf-8') as f:
            json.dump(value, f)

    def get_module_writer(self, module_name, wname, name, is_ns):
        elixir_module_name = make_elixir_module_names(module_name=module_name)
        inner_ns = []
        if wname.startswith('cv::'):
            wname = wname[4:]
            inner_ns = wname.split('::')
            elixir_module_name = make_elixir_module_names(separated_ns=inner_ns)

        if module_name == 'ximgproc_segmentation':
            elixir_module_name = "XImgProc"
            inner_ns = ["Segmentation"]
        elif module_name == 'utils_nested':
            elixir_module_name = "Utils.Nested"
        elif module_name == 'ximgproc_segmentation_GraphSegmentation':
            elixir_module_name = "XImgProc.GraphSegmentation"
            inner_ns = ["Segmentation"]
        elif module_name == 'ximgproc_segmentation_SelectiveSearchSegmentationStrategy':
            elixir_module_name = "XImgProc.SelectiveSearchSegmentationStrategy"
            inner_ns = ["Segmentation"]
        elif module_name == 'ximgproc_segmentation_SelectiveSearchSegmentationStrategyMultiple':
            elixir_module_name = "XImgProc.SelectiveSearchSegmentationStrategyMultiple"
            inner_ns = ["Segmentation"]
        elif module_name == 'phase_unwrapping_HistogramPhaseUnwrapping':
            elixir_module_name = "PhaseUnwrapping.HistogramPhaseUnwrapping"
        else:
            elixir_module_name = elixir_module_name.replace('_', '').strip()

        evision_module_filename = elixir_module_name.replace('.', '_')

        if evision_module_filename in self.evision_modules:
            return self.evision_modules[evision_module_filename], inner_ns
        else:
            module_file_generator = ModuleGenerator(elixir_module_name)
            module_file_generator.write_elixir(f'defmodule Evision.{elixir_module_name} do\n')
            if elixir_module_name not in ['Flann', 'Segmentation', 'ML']:
                module_file_generator.write_elixir('  import Kernel, except: [apply: 2, apply: 3]\n\n')

            if not evision_module_filename.startswith("evision_"):
                module_file_generator.write_erlang(f'-module(evision_{evision_module_filename.lower()}).\n-compile(nowarn_export_all).\n-compile([export_all]).\n-include("evision.hrl").\n\n')
                module_file_generator.write_gleam(f'-module(evision_{evision_module_filename.lower()}).\n-compile(nowarn_export_all).\n-compile([export_all]).\n-include("evision.hrl").\n\n')
            else:
                module_file_generator.write_erlang(f'-module({evision_module_filename.lower()}).\n-compile(nowarn_export_all).\n-compile([export_all]).\n-include("evision.hrl").\n\n')
                module_file_generator.write_gleam(f'-module({evision_module_filename.lower()}).\n-compile(nowarn_export_all).\n-compile([export_all]).\n-include("evision.hrl").\n\n')

            if ES.evision_structs.get(elixir_module_name, None) is not None:
                module_file_generator.write_elixir(ES.evision_structs[elixir_module_name]["elixir"])
                module_file_generator.write_elixir("\n")
                module_file_generator.write_erlang(ES.evision_structs[elixir_module_name]["erlang"])
                module_file_generator.write_erlang("\n")
                module_file_generator.write_gleam(ES.evision_structs[elixir_module_name]["gleam"][0])
                module_file_generator.write_gleam("\n")
                module_file_generator.write_gleam_file(ES.evision_structs[elixir_module_name]["gleam"][1])
                module_file_generator.write_gleam_file("\n")
                
            if EF.extra_functions.get(elixir_module_name, None) is not None:
                module_file_generator.write_elixir(EF.extra_functions[elixir_module_name]["elixir"])
                module_file_generator.write_elixir("\n")
                module_file_generator.write_erlang(EF.extra_functions[elixir_module_name]["erlang"])
                module_file_generator.write_erlang("\n")
                module_file_generator.write_gleam(EF.extra_functions[elixir_module_name]["erlang"])
                module_file_generator.write_gleam("\n")

            if elixir_module_name not in evision_structrised_classes:
                mapped_elixir_module_name = get_elixir_module_name(elixir_module_name)
                atom_elixir_module_name = f"Evision.{mapped_elixir_module_name}"
                atom_erlang_module_name = atom_elixir_module_name
                if atom_erlang_module_name == 'Evision.XImgProc.Segmentation.GraphSegmentation':
                    atom_erlang_module_name = 'Evision.XImgProc.GraphSegmentation'
                elif atom_erlang_module_name == 'Evision.XImgProc.Segmentation.SelectiveSearchSegmentationStrategy':
                    atom_erlang_module_name = 'Evision.XImgProc.SelectiveSearchSegmentationStrategy'
                module_file_generator.write_elixir( 
                    ES.generic_struct_template_elixir.substitute(
                        atom_elixir_module_name=atom_elixir_module_name,
                        elixir_module_name=mapped_elixir_module_name
                    )
                )
                atom_erlang_module_name = atom_erlang_module_name.replace("Evision.", "evision_").replace(".", "_").lower()
                if not atom_erlang_module_name.startswith("evision_"):
                    atom_erlang_module_name = f"evision_{atom_erlang_module_name}"
                module_file_generator.write_erlang(
                    ES.generic_struct_template_erlang.substitute(
                        atom_elixir_module_name=f"Elixir.{atom_elixir_module_name}",
                        atom_erlang_module_name=atom_erlang_module_name
                    )
                )
                module_file_generator.write_gleam(
                    ES.generic_struct_template_erlang.substitute(
                        atom_elixir_module_name=f"Elixir.{atom_elixir_module_name}",
                        atom_erlang_module_name=atom_erlang_module_name
                    )
                )
                self.evision_erlang_hrl.write(
                    f"-record({atom_erlang_module_name}, "
                    "{ref}).\n"
                )
                self.evision_gleam_hrl.write(
                    f"-record({atom_erlang_module_name}, "
                    "{ref}).\n"
                )
                gleam_named_type = atom_elixir_module_name.split(".")[-1]
                module_file_generator.write_gleam_file("import gleam/erlang.{type Reference}\n\npub type " + gleam_named_type + " {\n  " + gleam_named_type + "(\n    ref: Reference\n  )\n}\n\n")

            self.evision_modules[evision_module_filename] = module_file_generator
            return self.evision_modules[evision_module_filename], inner_ns

    def gen_enabled_modules(self):
        all_modules = {
            # opencv/opencv_contrib
            'opencv': [
                'calib3d',
                'core',
                'dnn',
                'features2d',
                'flann',
                'highgui',
                'imgcodecs',
                'imgproc',
                'ml',
                'photo',
                'stitching',
                'ts',
                'video',
                'videoio',

                'gapi',
                'world',
                'python2',
                'python3',
                'java',
            ],

            'opencv_contrib': [
                'aruco',
                'barcode',
                'bgsegm',
                'bioinspired',
                'dnn_superres',
                'face',
                'hfs',
                'img_hash',
                'line_descriptor',
                'mcc',
                'plot',
                'quality',
                'rapid',
                'reg',
                'rgbd',
                'saliency',
                'shape',
                'stereo',
                'structured_light',
                'surface_matching',
                'text',
                'tracking',
                'wechat_qrcode',
                'xfeatures2d',
                'ximgproc',
                'xphoto',

                # no bindings yet
                'datasets',
                'dnn_objdetect',
                'dpm',
                'optflow',
                'sfm',
                'videostab',
                'xobjdetect',
            ],

            'cuda': [
                'cudaarithm',
                'cudabgsegm',
                'cudacodec',
                'cudafeatures2d',
                'cudafilters',
                'cudaimgproc',
                'cudalegacy',
                'cudaobjdetect',
                'cudaoptflow',
                'cudastereo',
                'cudawarping',
                'cudev',
            ]
        }

        num_total_modules = sum([len(components) for s, components in all_modules.items()])
        self.code_funcs.write('#include "evision_consts.h"\n')
        self.code_funcs.write("static ERL_NIF_TERM evision_cv_enabled_modules(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]){\n")
        self.code_funcs.write(f"""    const size_t num_total_modules = {num_total_modules};
    size_t index = 0;

    ERL_NIF_TERM keys[num_total_modules];
    ERL_NIF_TERM values[num_total_modules];
""")

        for _, components in all_modules.items():
            for component in components:
                self.code_funcs.write(f"""    keys[index] = evision::nif::atom(env, "{component}");
#ifdef HAVE_OPENCV_{component.upper()}
    values[index] = kAtomTrue;
#else
    values[index] = kAtomFalse;
#endif
    index++;
""")

        self.code_funcs.write("""    ERL_NIF_TERM map;
    if (enif_make_map_from_arrays(env, keys, values, index, &map)) {
        return map;
    } else {
        return evision::nif::error(env, "enif_make_map_from_arrays failed in evision_from_as_map");
    }
""")

        self.code_funcs.write("}\n")
        self.code_ns_reg.write(f'    F(enabled_modules, evision_cv_enabled_modules, 0),\n')
        self.evision_nif.write(f'  def enabled_modules, do: :erlang.nif_error(:undefined)\n')
        self.evision_nif_erlang.write(f'enabled_modules() ->\n    not_loaded(?LINE).\n')
        self.evision_nif_gleam.write(f'enabled_modules() ->\n    not_loaded(?LINE).\n')

    def gen(self, srcfiles, output_path, erl_output_path, erlang_output_path, gleam_output_path):
        self.output_path = output_path
        self.clear()
        self.parser = hdr_parser.CppHeaderParser(generate_umat_decls=True, generate_gpumat_decls=True)

        self.evision_nif.write('defmodule :evision_nif do\n{}\n'.format(ET.gen_evision_nif_load_nif))
        self.evision_nif_erlang.write('-module(evision_nif).\n-compile(nowarn_export_all).\n-compile([export_all]).\n\n{}\n{}\n'.format(ET.gen_evision_nif_load_nif_erlang, ET.gen_cv_types_erlang))
        self.evision_nif_gleam.write('-module(evision_nif).\n-compile(nowarn_export_all).\n-compile([export_all]).\n\n{}\n{}\n'.format(ET.gen_evision_nif_load_nif_erlang, ET.gen_cv_types_erlang))

        self.code_ns_reg.write('static ErlNifFunc nif_functions[] = {\n')
        self.gen_enabled_modules()

        # step 1: scan the headers and build more descriptive maps of classes, consts, functions
        for hdr in srcfiles:
            decls = self.parser.parse(hdr)
            if len(decls) == 0:
                continue

            if 'gapi' in hdr:
                continue

            if hdr.find('misc/python/shadow_') < 0:  # Avoid including the "shadow_" files
                if hdr.find('opencv2/') >= 0:
                    # put relative path
                    self.code_include.write('#include "{0}"\n'.format(hdr[hdr.rindex('opencv2/'):]))
                else:
                    self.code_include.write('#include "{0}"\n'.format(hdr))

            for decl in decls:
                name = decl[0]
                if name.startswith("struct") or name.startswith("class"):
                    # class/struct
                    p = name.find(" ")
                    stype = name[:p]
                    name = name[p+1:].strip()
                    self.add_class(stype, name, decl)
                elif name.startswith("const"):
                    # constant
                    self.add_const(name.replace("const ", "").strip(), decl)
                elif name.startswith("enum"):
                    # enum
                    self.add_enum(name.rsplit(" ", 1)[1], decl)
                else:
                    # function
                    self.add_func(decl)

        # step 1.5 check if all base classes exist
        for name, classinfo in self.classes.items():
            if classinfo.base:
                chunks = classinfo.base.split('_')
                base = '_'.join(chunks)
                while base not in self.classes and len(chunks)>1:
                    del chunks[-2]
                    base = '_'.join(chunks)
                if base not in self.classes:
                    print("Generator error: unable to resolve base %s for %s"
                        % (classinfo.base, classinfo.name))
                    sys.exit(-1)
                base_instance = self.classes[base]
                classinfo.base = base
                classinfo.isalgorithm |= base_instance.isalgorithm  # wrong processing of 'isalgorithm' flag:
                                                                    # doesn't work for trees(graphs) with depth > 2
                self.classes[name] = classinfo

        # tree-based propagation of 'isalgorithm'
        processed = dict()
        def process_isalgorithm(classinfo):
            if classinfo.isalgorithm or classinfo in processed:
                return classinfo.isalgorithm
            res = False
            if classinfo.base:
                res = process_isalgorithm(self.classes[classinfo.base])
                #assert not (res == True or classinfo.isalgorithm is False), "Internal error: " + classinfo.name + " => " + classinfo.base
                classinfo.isalgorithm |= res
                res = classinfo.isalgorithm
            processed[classinfo] = True
            return res
        for name, classinfo in self.classes.items():
            process_isalgorithm(classinfo)

        # step 2: generate code for the classes and their methods
        classlist = list(self.classes.items())
        classlist.sort()
        for name, classinfo in classlist:
            self.code_types.write("//{}\n".format(80*"="))
            self.code_types.write("// {} ({})\n".format(name, 'Map' if classinfo.ismap else 'Generic'))
            self.code_types.write("//{}\n".format(80*"="))
            classinfo.gen_erl_func_list(self)
            self.code_types.write(classinfo.gen_code(self))
            if classinfo.ismap:
                self.code_types.write(ET.gen_template_map_type_cvt.substitute(name=classinfo.name, cname=classinfo.cname))
            else:
                mappable_code = "\n".join([
                                      ET.gen_template_mappable.substitute(cname=classinfo.cname, mappable=mappable)
                                          for mappable in classinfo.mappables])
                code = ET.gen_template_type_decl.substitute(
                    name=classinfo.name,
                    cname=classinfo.cname if classinfo.issimple else "Ptr<{}>".format(classinfo.cname),
                    mappable_code=mappable_code
                )
                self.code_types.write(code)

        # register classes in the same order as they have been declared.
        # this way, base classes will be registered in Python before their derivatives.
        classlist1 = [(classinfo.decl_idx, name, classinfo) for name, classinfo in classlist]
        classlist1.sort()

        for decl_idx, name, classinfo in classlist1:
            if classinfo.ismap:
                continue
            self.code_type_publish.write(classinfo.gen_def(self, self.evision_modules, self.evision_erlang_hrl, self.evision_gleam_hrl))

        # step 3: generate the code for all the global functions
        for ns_name, ns in sorted(self.namespaces.items()):
            if ns_name.split('.')[0] != 'cv':
                continue
            for name, func in sorted(ns.funcs.items()):
                if func.isconstructor:
                    continue
                code = func.gen_code(self)
                self.code_funcs.write(code)
        self.gen_namespace()

        # step 4: generate the code for enum types
        enumlist = list(self.enums.values())
        enumlist.sort()
        for name in enumlist:
            self.gen_enum_reg(name)

        # step 5: generate the code for constants
        constlist = list(self.consts.items())
        constlist.sort()
        for name, constinfo in constlist:
            self.gen_const_reg(constinfo)

        bitwise_ops = ['band', 'bnot', 'bor', 'bsl', 'bsr', 'bxor']

        # end 'evision.ex'
        if 'elixir' in self.langs:
            for fix in evision_elixir_fixes():
                self.evision_elixir.write(fix)
                self.evision_elixir.write("\n")
            evision_enums = self.typed_enums.get('Evision', None)
            if evision_enums is not None:
                enum_written = set()
                for enum_name, enum_values_elixir, _ in evision_enums:
                    if any([x in enum_values_elixir for x in bitwise_ops]):
                        self.evision_elixir.write(f'  import Bitwise\n\n')
                        break
                self.evision_elixir.write(f'  @type enum :: integer()\n')
                for enum_name, enum_values_elixir, _ in evision_enums:
                    enum_written.add(enum_name)
                    self.evision_elixir.write(f'  @doc enum: true\n')
                    self.evision_elixir.write(f'  def cv_{enum_name}, do: {enum_values_elixir}\n')
                self.evision_elixir.write("\n")
            self.evision_elixir.write(ET.enabled_modules_code.substitute(
                enabled_modules=",".join([f'\n      "{m}"' for m in self.enabled_modules]))
            )

        # end 'evision.erl'
        if 'erlang' in self.langs:
            self.evision_ex.end('erlang')
            for fix in evision_erlang_fixes():
                self.evision_erlang.write(fix)
                self.evision_erlang.write("\n")
            evision_enums = self.typed_enums.get('Evision', None)
            if evision_enums is not None:
                enum_written = set()
                for enum_name, _, enum_values_erlang in evision_enums:
                    if enum_name in enum_written:
                        continue
                    enum_written.add(enum_name)
                    self.evision_erlang.write(f'cv_{enum_name}() ->\n  {enum_values_erlang}.\n')
                self.evision_erlang.write("\n")
            self.evision_erlang.write(ET.enabled_modules_code_erlang.substitute(
                enabled_modules=",".join([f'\'{m}\'' for m in self.enabled_modules]))
            )

        # end gleam's 'evision.erl'
        if 'gleam' in self.langs:
            self.evision_ex.end('gleam')
            for fix in evision_erlang_fixes():
                self.evision_gleam.write(fix)
                self.evision_gleam.write("\n")
            for fix in evision_erlang_fixes_gleam_typed():
                self.evision_gleam_typed.write(fix)
                self.evision_gleam_typed.write("\n")
            evision_enums = self.typed_enums.get('Evision', None)
            if evision_enums is not None:
                enum_written = set()
                for enum_name, _, enum_values_erlang in evision_enums:
                    if enum_name in enum_written:
                        continue
                    enum_written.add(enum_name)
                    self.evision_gleam.write(f'cv_{enum_name}() ->\n  {enum_values_erlang}.\n')
                    self.evision_gleam_typed.write(f'@external(erlang, "evision", "cv_{enum_name}")\n')
                    self.evision_gleam_typed.write(f'pub fn {enum_name.lower()}() -> Int\n\n')
                self.evision_gleam.write("\n")
            self.evision_gleam.write(ET.enabled_modules_code_erlang.substitute(
                enabled_modules=",".join([f'\'{m}\'' for m in self.enabled_modules]))
            )
            self.evision_gleam_typed.write('@external(erlang, "evision", "enabled_modules")\npub fn enabled_modules() -> Dict(String, Bool)\n\n')

        self.typed_enums['Evision'] = None

        # That's it. Now save all the files
        self.save(output_path, "evision_generated_include.h", self.code_include)
        self.save(output_path, "evision_generated_funcs.h", self.code_funcs)
        self.save(output_path, "evision_generated_enums.h", self.code_enums)
        self.save(output_path, "evision_generated_types.h", self.code_type_publish)
        self.save(output_path, "evision_generated_types_content.h", self.code_types)

        # write all module files
        for name in self.evision_modules:
            module_file_generator = self.evision_modules[name]
            
            delete_typed_enum = False

            if 'elixir' in self.langs:
                if name in evision_elixir_module_fixes():
                    fixes = evision_elixir_module_fixes()[name]
                    for f,d in fixes:
                        module_file_generator.write_elixir(f)
                        self.evision_nif.write(d)
                typed_enum_key = f'Evision.{module_file_generator.module_name}'
                evision_enums = self.typed_enums.get(typed_enum_key, None)
                if evision_enums is not None:
                    enum_written = set()
                    for enum_name, enum_values_elixir, _ in evision_enums:
                        if any([x in enum_values_elixir for x in bitwise_ops]):
                            module_file_generator.write_elixir(f'  import Bitwise\n\n')
                            break

                    module_file_generator.write_elixir(f'  @type enum :: integer()\n')
                    for enum_name, enum_values_elixir, _ in evision_enums:
                        if enum_name in enum_written:
                            continue
                        enum_written.add(enum_name)
                        module_file_generator.write_elixir(f'  @doc enum: true\n')
                        module_file_generator.write_elixir(f'  def cv_{enum_name}, do: {enum_values_elixir}\n')
                    module_file_generator.write_elixir("\n")
                    delete_typed_enum = True
                module_file_generator.end('elixir')
                self.evision_nif.write(module_file_generator.get_nif_declaration('elixir'))
                self.save(erl_output_path, f"evision_{name.lower()}.ex", module_file_generator.get_generated_code('elixir'))

            if 'erlang' in self.langs:
                if name in evision_erlang_module_fixes():
                    fixes = evision_erlang_module_fixes()[name]
                    for f,d in fixes:
                        module_file_generator.write_erlang(f)
                        self.evision_nif_erlang.write(d)
                typed_enum_key = f'Evision.{module_file_generator.module_name}'
                evision_enums = self.typed_enums.get(typed_enum_key, None)
                if evision_enums is not None:
                    enum_written = set()
                    for enum_name, _, enum_values_erlang in evision_enums:
                        if enum_name in enum_written:
                            continue
                        enum_written.add(enum_name)
                        module_file_generator.write_erlang(f'cv_{enum_name}() ->\n  {enum_values_erlang}.\n')
                    module_file_generator.write_erlang("\n")
                    delete_typed_enum = True
                module_file_generator.end('erlang')
                self.evision_nif_erlang.write(module_file_generator.get_nif_declaration('erlang'))
                self.save(erlang_output_path, f"evision_{name.lower()}.erl", module_file_generator.get_generated_code('erlang'))

            if 'gleam' in self.langs:
                if name in evision_erlang_module_fixes():
                    fixes = evision_erlang_module_fixes()[name]
                    for f,d in fixes:
                        module_file_generator.write_gleam(f)
                        self.evision_nif_gleam.write(d)
                typed_enum_key = f'Evision.{module_file_generator.module_name}'
                erlang_module_name = typed_enum_key.replace('.', '_').lower()
                typed_gleam_names = typed_enum_key.split('.')
                typed_gleam_name = typed_gleam_names[-1].lower()
                typed_gleam = StringIO()
                evision_enums = self.typed_enums.get(typed_enum_key, None)
                if evision_enums is not None:
                    enum_written = set()
                    for enum_name, _, enum_values_erlang in evision_enums:
                        if enum_name in enum_written:
                            continue
                        enum_written.add(enum_name)
                        module_file_generator.write_gleam(f'cv_{enum_name}() ->\n  {enum_values_erlang}.\n')
                        typed_gleam.write(f'@external(erlang, "{erlang_module_name}", "cv_{enum_name}")\n')
                        typed_gleam.write(f'pub fn {enum_name.lower()}() -> Int\n\n')
                    module_file_generator.write_gleam("\n")
                    delete_typed_enum = True
                if name in evision_gleam_module_fixes():
                    fixes = evision_gleam_module_fixes()[name]
                    for f in fixes:
                        module_file_generator.write_gleam_file(f)
                module_file_generator.end('gleam')
                self.evision_nif_gleam.write(module_file_generator.get_nif_declaration('gleam'))
                self.save(gleam_output_path, f"evision_{name.lower()}.erl", module_file_generator.get_generated_code('gleam'))
                typed_enums = typed_gleam.getvalue()
                if typed_gleam_name == 'type':
                    typed_gleam_name = typed_gleam_names[-2].lower() + '_' + typed_gleam_name
                if len(typed_enums) > 0:
                    self.save(gleam_output_path + '/' + "/".join([x.lower() for x in typed_gleam_names[:-1]]), f"{typed_gleam_name}.gleam", typed_gleam.getvalue())
                self.save(gleam_output_path + '/' + "/".join([x.lower() for x in typed_gleam_names[:-1]]), f"{typed_gleam_name}.gleam", module_file_generator.get_generated_code('gleam_file'))

            if delete_typed_enum:
                self.typed_enums[typed_enum_key] = None

            self.code_ns_reg.write(module_file_generator.get_erl_nif_func_entry())

        # write all typed enum files
        for typed_enum_key, evision_enums in self.typed_enums.items():
            if evision_enums is None:
                continue
            elixir_module_name = typed_enum_key[8:]
            if typed_enum_key == 'Evision.Mat':
                continue
            else:
                module_file_generator = ModuleGenerator(elixir_module_name)

            if 'elixir' in self.langs:
                module_file_generator.write_elixir(f'defmodule {typed_enum_key} do\n')
                
                for enum_name, enum_values_elixir, _ in evision_enums:
                    if any([x in enum_values_elixir for x in bitwise_ops]):
                        module_file_generator.write_elixir(f'  import Bitwise\n\n')
                        break

                module_file_generator.write_elixir(f'  @type enum :: integer()\n')
                enum_written = set()
                for enum_name, enum_values_elixir, _ in evision_enums:
                    if enum_name in enum_written:
                        continue
                    enum_written.add(enum_name)
                    module_file_generator.write_elixir(f'  @doc enum: true\n')
                    module_file_generator.write_elixir(f'  def cv_{enum_name}, do: {enum_values_elixir}\n')
                module_file_generator.end('elixir')
                name = elixir_module_name.replace('.', '_')
                self.save(erl_output_path, f"evision_{name.lower()}.ex", module_file_generator.get_generated_code('elixir'))
            
            if 'erlang' in self.langs:
                erlang_module_name = typed_enum_key.replace('.', '_').lower()
                module_file_generator.write_erlang(f'-module({erlang_module_name}).\n-compile(nowarn_export_all).\n-compile([export_all]).\n\n')
                enum_written = set()
                for enum_name, _, enum_values_erlang in evision_enums:
                    if enum_name in enum_written:
                        continue
                    enum_written.add(enum_name)
                    module_file_generator.write_erlang(f'cv_{enum_name}() ->\n  {enum_values_erlang}.\n')
                module_file_generator.end('erlang')
                self.save(erlang_output_path, f"{erlang_module_name}.erl", module_file_generator.get_generated_code('erlang'))

            if 'gleam' in self.langs:
                erlang_module_name = typed_enum_key.replace('.', '_').lower()
                typed_gleam_names = typed_enum_key.split('.')
                typed_gleam_name = typed_gleam_names[-1].lower()
                module_file_generator.write_gleam(f'-module({erlang_module_name}).\n-compile(nowarn_export_all).\n-compile([export_all]).\n\n')
                enum_written = set()
                typed_gleam = StringIO()
                for enum_name, _, enum_values_erlang in evision_enums:
                    if enum_name in enum_written:
                        continue
                    enum_written.add(enum_name)
                    module_file_generator.write_gleam(f'cv_{enum_name}() ->\n  {enum_values_erlang}.\n')
                    typed_gleam.write(f'@external(erlang, "{erlang_module_name}", "cv_{enum_name}")\n')
                    typed_gleam.write(f'pub fn {enum_name.lower()}() -> Int\n\n')
                module_file_generator.end('gleam')
                self.save(gleam_output_path, f"{erlang_module_name}.erl", module_file_generator.get_generated_code('gleam'))
                if typed_gleam_name == 'type':
                    typed_gleam_name = typed_gleam_names[-2].lower() + '_' + typed_gleam_name
                self.save(gleam_output_path + '/' + "/".join([x.lower() for x in typed_gleam_names[:-1]]), f"{typed_gleam_name}.gleam", typed_gleam.getvalue())

        if 'elixir' in self.langs:
            # 'evision_nif.ex'
            self.evision_nif.write(self.evision_ex.get_nif_declaration('elixir'))
            self.evision_nif.write('\nend\n')
            self.save(erl_output_path, "evision_nif.ex", self.evision_nif)

            # 'evision_constant.ex'
            self.evision_constant_elixir.write(self.enum_names_io.getvalue())
            self.evision_constant_elixir.write('\nend\n')
            self.save(erl_output_path, "evision_constant.ex", self.evision_constant_elixir)

            # 'evision.ex'
            self.evision_ex.end('elixir')
            self.evision_elixir.write(self.evision_ex.get_generated_code('elixir'))
            self.save(erl_output_path, "evision.ex", self.evision_elixir)

        if 'erlang' in self.langs:
            # 'evision_nif.erl'
            self.evision_nif_erlang.write(self.evision_ex.get_nif_declaration('erlang'))
            self.save(erlang_output_path, "evision_nif.erl", self.evision_nif_erlang)

            # 'evision.erl'
            self.evision_ex.end('erlang')
            self.evision_erlang.write(self.evision_ex.get_generated_code('erlang'))
            self.save(erlang_output_path, "evision.erl", self.evision_erlang)            

            # 'evision_constant.erl'
            self.evision_constant_erlang.write(self.enum_names_io_erlang.getvalue())
            self.evision_constant_erlang.write('\n')
            self.save(erlang_output_path, "evision_constant.erl", self.evision_constant_erlang)

            # 'evision.hrl'
            self.save(erlang_output_path, "evision.hrl", self.evision_erlang_hrl)

        if 'gleam' in self.langs:
            # 'evision_nif.erl'
            self.evision_nif_gleam.write(self.evision_ex.get_nif_declaration('gleam'))
            self.save(gleam_output_path, "evision_nif.erl", self.evision_nif_gleam)
            
            # 'evision.erl'
            self.evision_ex.end('gleam')
            self.evision_gleam.write(self.evision_ex.get_generated_code('gleam'))
            self.evision_gleam_typed.write(self.evision_ex.get_generated_code('gleam_file'))
            self.save(gleam_output_path, "evision.erl", self.evision_gleam)
            self.save(gleam_output_path + "/evision", "evision.gleam", self.evision_gleam_typed)

            # 'evision.hrl'
            self.save(gleam_output_path, "evision.hrl", self.evision_gleam_hrl)

        self.code_ns_reg.write('\n};\n\n')
        self.save(output_path, "evision_generated_modules_content.h", self.code_ns_reg)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--c_src", type=str, default="./c_src", help="Path to the c_src dir")
    parser.add_argument("--elixir_gen", type=str, default="./lib", help="Path to the output dir of elixir binding files")
    parser.add_argument("--erlang_gen", type=str, default="./src", help="Path to the output dir of erlang binding files")
    parser.add_argument("--gleam_gen", type=str, default="./gleam_src", help="Path to the output dir of gleam binding files")
    parser.add_argument("--headers", help="Path to the headers.txt/header-contrib.txt in c_src")
    parser.add_argument("--lang", type=str, help="Comma-seperated values. erlang,elixir,gleam")
    parser.add_argument("--modules", type=str, default='', help="Comma-seperated values.")
    args = parser.parse_args()

    srcfiles = hdr_parser.opencv_hdr_list
    dstdir = args.c_src
    elixir_dstdir = args.elixir_gen
    erlang_dstdir = args.erlang_gen
    gleam_dstdir = args.gleam_gen

    if len(args.headers) > 4:
        srcfiles = []
        with open(args.headers, 'r') as f:
            for l in f.readlines():
                l = l.strip()
                srcfiles.append(l)
                if l.endswith("modules/flann/include/opencv2/flann.hpp"):
                    flann_defines = l.replace("modules/flann/include/opencv2/flann.hpp", "modules/flann/include/opencv2/flann/defines.h")
                    if os.path.exists(flann_defines):
                        srcfiles.append(flann_defines)
    lang = []
    if len(args.lang) >= 5:
        lang = list(set([l.lower().strip() for l in args.lang.split(",")]))
    if len(lang) == 0:
        raise RuntimeError("env var EVISION_GENERATE_LANG is empty")
    for l in lang:
        if l not in ['elixir', 'erlang', 'gleam']:
            raise RuntimeError(f"unknown value found in EVISION_GENERATE_LANG: `{l}`. Allowed values are `elixir`, `erlang`, `gleam`")

    # default
    enabled_modules = ['calib3d', 'core', 'features2d', 'flann', 'highgui', 'imgcodecs', 'imgproc', 'ml', 'photo',
                       'stitching', 'ts', 'video', 'videoio', 'dnn']
    if len(args.modules) > 0:
        enabled_modules = args.modules.split(",")
    generator = BeamWrapperGenerator(enabled_modules, lang)
    rmtree(elixir_dstdir)
    rmtree(erlang_dstdir)
    makedirs(elixir_dstdir)
    makedirs(erlang_dstdir)
    generator.gen(srcfiles, dstdir, elixir_dstdir, erlang_dstdir, gleam_dstdir)
    # for n in generator.namespaces:
    #     print(f'"{n}": &(&1[:namespace] == :"{n}"),')
