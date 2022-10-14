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


def make_elixir_module_names(module_name: Optional[str] = None, separated_ns: Optional[list] = None):
    mapping = {
        'dnn': 'DNN',
        'ml': 'ML',
        'ocl': 'OCL',
        'ipp': 'IPP',
        'videoio_registry': 'VideoIORegistry',
        'fisheye': 'FishEye',
        'utils_fs': 'UtilsFS',
        'cuda': 'CUDA',
    }
    if module_name is not None:
        return mapping.get(module_name, f"{module_name[0].upper()}{module_name[1:]}")
    if separated_ns is not None:
        return ".".join([mapping.get(n, f"{n[0].upper()}{n[1:]}") for n in separated_ns])


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

def map_argtype_to_type(argtype: str):
    if len(argtype) > 0 and argtype.startswith('Ptr<') and argtype.endswith('>'):
        argtype = argtype[4:-1]
    if len(argtype) > 0 and argtype[-1] == '*':
        argtype = argtype[:-1]

    if is_int_type(argtype):
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
    else:
        if is_struct(argtype):
            if argtype == 'UMat':
                return 'Mat'
            return argtype
        if argtype == 'Moments':
            return '(map)'
        return 't'


def map_argname(kind, argname, **kwargs):
    if kind == 'elixir':
        return map_argname_elixir(argname, **kwargs)
    elif kind == 'erlang':
        return map_argname_erlang(argname, **kwargs)
    else:
        print(f'warning: map_argname: unknown kind `{kind}`')


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
        'int64',
        'ORB_ScoreType',
        'ORB::ScoreType',
        'dnn_Backend',
        'dnn_Target',
        'AKAZE_DescriptorType',
        'AKAZE::DescriptorType',
        'KAZE_DiffusivityType',
        'KAZE::DiffusivityType',
        'DescriptorMatcher_MatcherType',
        'AgastFeatureDetector_DetectorType',
        'AgastFeatureDetector::DetectorType',
        'FastFeatureDetector_DetectorType',
        'FastFeatureDetector::DetectorType',
        'InterpolationFlags',
        'AccessFlag',
        'WaveCorrectKind',
        'flann_distance_t',
        'cvflann_flann_algorithm_t',
        'cvflann::flann_algorithm_t',
        'cvflann_flann_distance_t',
        'cvflann::flann_distance_t',
        'DeviceInfo::ComputeMode',
        'CorrectionLevel',
        'EncodeMode',
        'LocalOptimMethod',
        'NeighborSearchMethod',
        'SamplingMethod',
        'ScoreMethod',
        'HOGDescriptor_HistogramNormType',
    ]
    return argtype in int_types

def is_list_type(argtype):
    list_types = [
        'ImageFeatures',
        'MatchesInfo',
        'CameraParams',
        'VideoCaptureAPIs',
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
        'Point',
        'Point2i',
        'Point2f',
        'Point2d',
        'Point3i',
        'Point3f',
        'Point3d',
        'Size',
        'Scalar',
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

def is_struct(argtype: str, also_get: Optional[str] = None):
    special_structs = {
        'UMat': 'Evision.Mat'
    }
    struct_types = {
        'Mat': 'Evision.Mat',
        'GpuMat': 'Evision.CUDA.GpuMat',
        'cuda::GpuMat': 'Evision.CUDA.GpuMat',
        'AKAZE': 'Evision.AKAZE',
        'AffineFeature': 'Evision.AffineFeature',
        'AgastFeatureDetector': 'Evision.AgastFeatureDetector',
        'Algorithm': 'Evision.Algorithm',
        'AlignExposures': 'Evision.AlignExposures',
        'AlignMTB': 'Evision.AlignMTB',
        'AsyncArray': 'Evision.AsyncArray',
        'BFMatcher': 'Evision.BFMatcher',
        'BOWImgDescriptorExtractor': 'Evision.BOWImgDescriptorExtractor',
        'BOWKMeansTrainer': 'Evision.BOWKMeansTrainer',
        'BOWTrainer': 'Evision.BOWTrainer',
        'BRISK': 'Evision.BRISK',
        'BackgroundSubtractor': 'Evision.BackgroundSubtractor',
        'BackgroundSubtractorKNN': 'Evision.BackgroundSubtractorKNN',
        'BackgroundSubtractorMOG2': 'Evision.BackgroundSubtractorMOG2',
        'CLAHE': 'Evision.CLAHE',
        'CalibrateCRF': 'Evision.CalibrateCRF',
        'CalibrateDebevec': 'Evision.CalibrateDebevec',
        'CalibrateRobertson': 'Evision.CalibrateRobertson',
        'CascadeClassifier': 'Evision.CascadeClassifier',
        'CirclesGridFinderParameters': 'Evision.CirclesGridFinderParameters',
        'DISOpticalFlow': 'Evision.DISOpticalFlow',
        'DMatch': 'Evision.DMatch',
        'DenseOpticalFlow': 'Evision.DenseOpticalFlow',
        'DescriptorMatcher': 'Evision.DescriptorMatcher',
        'FaceDetectorYN': 'Evision.FaceDetectorYN',
        'FaceRecognizerSF': 'Evision.FaceRecognizerSF',
        'FarnebackOpticalFlow': 'Evision.FarnebackOpticalFlow',
        'FastFeatureDetector': 'Evision.FastFeatureDetector',
        'Feature2D': 'Evision.Feature2D',
        'FileNode': 'Evision.FileNode',
        'FileStorage': 'Evision.FileStorage',
        'FlannBasedMatcher': 'Evision.FlannBasedMatcher',
        'GFTTDetector': 'Evision.GFTTDetector',
        'GeneralizedHough': 'Evision.GeneralizedHough',
        'GeneralizedHoughBallard': 'Evision.GeneralizedHoughBallard',
        'GeneralizedHoughGuil': 'Evision.GeneralizedHoughGuil',
        'HOGDescriptor': 'Evision.HOGDescriptor',
        'KAZE': 'Evision.KAZE',
        'KalmanFilter': 'Evision.KalmanFilter',
        'KeyPoint': 'Evision.KeyPoint',
        'LineSegmentDetector': 'Evision.LineSegmentDetector',
        'MSER': 'Evision.MSER',
        'MergeDebevec': 'Evision.MergeDebevec',
        'MergeExposures': 'Evision.MergeExposures',
        'MergeMertens': 'Evision.MergeMertens',
        'MergeRobertson': 'Evision.MergeRobertson',
        'ORB': 'Evision.ORB',
        'PyRotationWarper': 'Evision.PyRotationWarper',
        'QRCodeDetector': 'Evision.QRCodeDetector',
        'QRCodeEncoder': 'Evision.QRCodeEncoder',
        'QRCodeEncoder_Params': 'Evision.QRCodeEncoder.Params',
        'SIFT': 'Evision.SIFT',
        'SimpleBlobDetector': 'Evision.SimpleBlobDetector',
        'SimpleBlobDetector_Params': 'Evision.SimpleBlobDetector.Params',
        'SparseOpticalFlow': 'Evision.SparseOpticalFlow',
        'SparsePyrLKOpticalFlow': 'Evision.SparsePyrLKOpticalFlow',
        'StereoBM': 'Evision.StereoBM',
        'StereoMatcher': 'Evision.StereoMatcher',
        'StereoSGBM': 'Evision.StereoSGBM',
        'Stitcher': 'Evision.Stitcher',
        'Subdiv2D': 'Evision.Subdiv2D',
        'TickMeter': 'Evision.TickMeter',
        'Tonemap': 'Evision.Tonemap',
        'TonemapDrago': 'Evision.TonemapDrago',
        'TonemapMantiuk': 'Evision.TonemapMantiuk',
        'TonemapReinhard': 'Evision.TonemapReinhard',
        'Tracker': 'Evision.Tracker',
        'TrackerDaSiamRPN': 'Evision.TrackerDaSiamRPN',
        'TrackerDaSiamRPN_Params': 'Evision.TrackerDaSiamRPN.Params',
        'TrackerGOTURN': 'Evision.TrackerGOTURN',
        'TrackerGOTURN_Params': 'Evision.TrackerGOTURN.Params',
        'TrackerMIL': 'Evision.TrackerMIL',
        'TrackerMIL_Params': 'Evision.TrackerMIL.Params',
        'UsacParams': 'Evision.UsacParams',
        'VariationalRefinement': 'Evision.VariationalRefinement',
        'VideoCapture': 'Evision.VideoCapture',
        'VideoWriter': 'Evision.VideoWriter',
        "BufferPool": "Evision.CUDA.BufferPool",
        "DeviceInfo": "Evision.CUDA.DeviceInfo",
        "Event": "Evision.CUDA.Event",
        "GpuMat": "Evision.CUDA.GpuMat",
        "HostMem": "Evision.CUDA.HostMem",
        "Stream": "Evision.CUDA.Stream",
        "TargetArchs": "Evision.CUDA.TargetArchs",
        "AffineBasedEstimator": "Evision.Detail.AffineBasedEstimator",
        "AffineBestOf2NearestMatcher": "Evision.Detail.AffineBestOf2NearestMatcher",
        "BestOf2NearestMatcher": "Evision.Detail.BestOf2NearestMatcher",
        "BestOf2NearestRangeMatcher": "Evision.Detail.BestOf2NearestRangeMatcher",
        "Blender": "Evision.Detail.Blender",
        "BlocksChannelsCompensator": "Evision.Detail.BlocksChannelsCompensator",
        "BlocksCompensator": "Evision.Detail.BlocksCompensator",
        "BlocksGainCompensator": "Evision.Detail.BlocksGainCompensator",
        "BundleAdjusterAffine": "Evision.Detail.BundleAdjusterAffine",
        "BundleAdjusterAffinePartial": "Evision.Detail.BundleAdjusterAffinePartial",
        "BundleAdjusterBase": "Evision.Detail.BundleAdjusterBase",
        "BundleAdjusterRay": "Evision.Detail.BundleAdjusterRay",
        "BundleAdjusterReproj": "Evision.Detail.BundleAdjusterReproj",
        "CameraParams": "Evision.Detail.CameraParams",
        "ChannelsCompensator": "Evision.Detail.ChannelsCompensator",
        "DpSeamFinder": "Evision.Detail.DpSeamFinder",
        "Estimator": "Evision.Detail.Estimator",
        "ExposureCompensator": "Evision.Detail.ExposureCompensator",
        "FeatherBlender": "Evision.Detail.FeatherBlender",
        "FeaturesMatcher": "Evision.Detail.FeaturesMatcher",
        "GainCompensator": "Evision.Detail.GainCompensator",
        "GraphCutSeamFinder": "Evision.Detail.GraphCutSeamFinder",
        "HomographyBasedEstimator": "Evision.Detail.HomographyBasedEstimator",
        "ImageFeatures": "Evision.Detail.ImageFeatures",
        "MatchesInfo": "Evision.Detail.MatchesInfo",
        "MultiBandBlender": "Evision.Detail.MultiBandBlender",
        "NoBundleAdjuster": "Evision.Detail.NoBundleAdjuster",
        "NoExposureCompensator": "Evision.Detail.NoExposureCompensator",
        "NoSeamFinder": "Evision.Detail.NoSeamFinder",
        "PairwiseSeamFinder": "Evision.Detail.PairwiseSeamFinder",
        "SeamFinder": "Evision.Detail.SeamFinder",
        "SphericalProjector": "Evision.Detail.SphericalProjector",
        "Timelapser": "Evision.Detail.Timelapser",
        "VoronoiSeamFinder": "Evision.Detail.VoronoiSeamFinder",
        "ClassificationModel": "Evision.DNN.ClassificationModel",
        "DetectionModel": "Evision.DNN.DetectionModel",
        "DictValue": "Evision.DNN.DictValue",
        "KeypointsModel": "Evision.DNN.KeypointsModel",
        "Layer": "Evision.DNN.Layer",
        "Model": "Evision.DNN.Model",
        "Net": "Evision.DNN.Net",
        "SegmentationModel": "Evision.DNN.SegmentationModel",
        "TextDetectionModel": "Evision.DNN.TextDetectionModel",
        "TextDetectionModel_DB": "Evision.DNN.TextDetectionModelDB",
        "TextDetectionModel_EAST": "Evision.DNN.TextDetectionModelEAST",
        "TextRecognitionModel": "Evision.DNN.TextRecognitionModel",
        "Index": "Evision.Flann.Index",
        "ANN_MLP": "Evision.ML.ANNMLP",
        "Boost": "Evision.ML.Boost",
        "DTrees": "Evision.ML.DTrees",
        "EM": "Evision.ML.EM",
        "KNearest": "Evision.ML.KNearest",
        "LogisticRegression": "Evision.ML.LogisticRegression",
        "NormalBayesClassifier": "Evision.ML.NormalBayesClassifier",
        "ParamGrid": "Evision.ML.ParamGrid",
        "RTrees": "Evision.ML.RTrees",
        "SVM": "Evision.ML.SVM",
        "SVMSGD": "Evision.ML.SVMSGD",
        "StatModel": "Evision.ML.StatModel",
        "TrainData": "Evision.ML.TrainData",
        "Device": "Evision.OCL.Device",
        "IntelligentScissorsMB": "Evision.Segmentation.IntelligentScissorsMB",
        "OriginalClassName": "Evision.Utils.Nested.OriginalClassName",
        "OriginalClassName_Params": "Evision.Utils.Nested.OriginalClassName.Params",
    }

    if argtype.startswith('Ptr<'):
        argtype = argtype[len('Ptr<'):-1]
    arg_is_struct = argtype in struct_types or argtype in special_structs

    second_ret = None
    if also_get == 'struct_name':
        second_ret = struct_types.get(argtype, special_structs.get(argtype, argtype))
    if second_ret is None:
        return arg_is_struct
    else:
        return arg_is_struct, second_ret

def map_argtype_in_docs(argtype: str):
    is_array = argtype.startswith('vector_')
    if is_array:
        argtype_inner = argtype[len('vector_'):]
        mapped_type = '[' + map_argtype_in_docs(argtype_inner) + ']'
        return mapped_type
    mapping = {
        'UMat': 'Evision.Mat',
        'Mat': 'Evision.Mat',
        'std::string': 'String',
        'cv::String': 'String',
        'RotatedRect': '{centre={x, y}, size={s1, s2}, angle}'
    }
    mapped_type = mapping.get(argtype, None)
    if mapped_type is None:
        if is_struct(argtype):
            _, mapped_type = is_struct(argtype, 'struct_name')
        else:
            mapped_type = argtype
    return mapped_type

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

manual_type_spec_map = {
    'Rect': '{number(), number(), number(), number()}',
    'Rect2i': '{integer(), integer(), integer(), integer()}',
    'Rect2f': '{number(), number(), number(), number()}',
    'Rect2d': '{number(), number(), number(), number()}',
    'Range': '{number(), number()}',
    'Size': '{number(), number()}',
    'Size2i': '{number(), number()}',
    'Size2f': '{number(), number()}',
    'Size2d': '{number(), number()}',
    'Scalar': '{number()} | {number(), number()} | {number() | number() | number()} | {number(), number(), number(), number()}',
    'Point': '{number(), number()}',
    'Point2i': '{integer(), integer()}',
    'Point2f': '{number(), number()}',
    'Point2d': '{number(), number()}',
    'RotatedRect': '{{number(), number()}, {number(), number()}, number()}',
    'TermCriteria': '{integer(), integer(), number()}',
    'cv::TermCriteria': '{integer(), integer(), number()}',
    'ImageFeatures': 'Evision.Detail.ImageFeatures.t()',
    'UsacParams': 'Evision.UsacParams.t()',
    'MatchesInfo': 'Evision.MatchesInfo.t()',
    'CirclesGridFinderParameters': 'Evision.CirclesGridFinderParameters.t()',
    'CameraParams': 'Evision.Detail.CameraParams.t()',
    'MatShape': 'list(integer())',
    'KeyPoint': 'Evision.KeyPoint.t()',
    'VideoCaptureAPIs': 'list(number())',
    'ParamGrid': 'Evision.ML.ParamGrid.t()',
    'Layer': 'Evision.DNN.Layer.t()'
}

def map_argtype_in_spec(classname: str, argtype: str, is_in: bool):
    global vec_out_types
    if len(argtype) > 0 and argtype[-1] == '*':
        if argtype == 'char*' or argtype == 'uchar*':
            return 'binary()'
        argtype = argtype[:-1]
    if argtype.startswith('Ptr<'):
        if argtype == 'Ptr<char>' or argtype == 'Ptr<uchar>':
            return 'binary()'
        argtype = argtype[len('Ptr<'):-1]

    if is_int_type(argtype):
        return 'integer()'
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
    elif argtype in evision_structrised_classes:
        return f'Evision.{argtype}.t()'
    elif argtype in ['Mat', 'cv::Mat', 'UMat', 'cv::UMat']:
        if is_in:
            return 'Evision.Mat.maybe_mat_in()'
        else:
            return 'Evision.Mat.t()'
    elif argtype.startswith('vector_'):
        argtype_inner = argtype[len('vector_'):]
        if argtype == 'vector_char' or argtype == 'vector_uchar':
            return 'binary()'
        spec_type = 'list(' + map_argtype_in_spec(classname, argtype_inner, is_in) + ')'
        return spec_type
    elif argtype.startswith('std::vector<'):
        if argtype == 'std::vector<char>' or argtype == 'std::vector<uchar>':
            return 'binary()'
        argtype_inner = argtype[len('std::vector<'):-1]
        spec_type = 'list(' + map_argtype_in_spec(classname, argtype_inner, is_in) + ')'
        return spec_type
    elif is_struct(argtype):
        _, struct_name = is_struct(argtype, also_get='struct_name')
        return f'{struct_name}.t()'
    elif argtype in manual_type_spec_map:
        return manual_type_spec_map[argtype]
    elif argtype in ["FeatureDetector", "DescriptorExtractor"]:
        return 'reference() | term()'
    elif argtype in ['GpuMat::Allocator', 'GpuMat_Allocator']:
        return 'reference()'
    elif argtype in vec_out_types:
        return vec_out_types[argtype]
    elif argtype == 'Target':
        return 'integer()'
    elif argtype == 'Status' and classname == 'Stitcher':
        return 'integer()'
    elif argtype == 'Device' and classname == 'ocl_Device':
        return 'Evision.OCL.Device.t()'
    elif argtype == 'Index' and classname == 'flann_Index':
         return 'Evision.Flann.Index.t()'
    else:
        if argtype == 'LayerId':
            return 'term()'
        if argtype == 'GpuMat' or argtype == 'cuda::GpuMat':
            return f'Evision.CUDA.GpuMat.t()'
        if argtype == 'IndexParams' or argtype == 'SearchParams' or argtype == 'Moments':
            return f'map()'
        else:
            print(f'warning: generate_spec: unknown argtype `{argtype}`, input_arg? {is_in}, class={classname}')
            return 'term()'

def map_argtype_to_guard_elixir(argname, argtype):
    if argtype == 'vector_char' or argtype == 'vector_uchar' or argtype == 'std::vector<char>' or argtype == 'std::vector<uchar>':
        return f'is_binary({argname})'

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
        return f'(-128 <= {argname} and {argname} <= 127)'
    elif argtype == 'Range':
        return f'(is_tuple({argname}) or {argname} == :all)'
    elif is_tuple_type(argtype):
        return f'is_tuple({argname})'
    elif is_struct(argtype):
        _, struct_name = is_struct(argtype, also_get='struct_name')
        if struct_name == 'Evision.Mat':
            return f'(is_struct({argname}, Evision.Mat) or is_struct({argname}, Nx.Tensor))'
        else:
            return f'is_struct({argname}, {struct_name})'
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
    if argtype == 'vector_char' or argtype == 'vector_uchar' or argtype == 'std::vector<char>' or argtype == 'std::vector<uchar>':
        return f'is_binary({argname})'

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
    elif argtype == 'Range':
        return f'(is_tuple({argname}) or {argname} == all)'
    elif is_tuple_type(argtype):
        return f'is_tuple({argname})'
    elif is_struct(argtype):
        _, struct_name = is_struct(argtype, also_get='struct_name')
        return f'is_struct({argname}, {struct_name})'
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
