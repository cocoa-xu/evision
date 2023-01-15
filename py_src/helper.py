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
    "bool": ArgTypeInfo("bool", FormatStrings.unsigned_char, "0", True, False),
    "size_t": ArgTypeInfo("size_t", FormatStrings.unsigned_long_long, "0", True, False),
    "int": ArgTypeInfo("int", FormatStrings.int, "0", True, False),
    "float": ArgTypeInfo("float", FormatStrings.float, "0.f", True, False),
    "double": ArgTypeInfo("double", FormatStrings.double, "0", True, False),
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
            'imdecode',
            'videoCapture_waitAny']
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

module_name_map = {
    "aruco": "ArUco",
    "barcode": "Barcode",
    "bgsegm": "BgSegm",
    "bioinspired": "Bioinspired",
    "ccm": "CCM",
    "cuda": "CUDA",
    "cudacodec": "CUDACodec",
    "colored_kinfu": "ColoredKinFu",
    "detail": "Detail",
    "dnn": "DNN",
    "dnn_superres": "DNNSuperRes",
    "DnnSuperResImpl": "DNNSuperResImpl",
    "dynafu": "DynaFu",
    "face": "Face",
    "flann": "Flann",
    "fisheye": "FishEye",
    "hfs": "HFS",
    "img_hash": "ImgHash",
    "kinfu": "KinFu",
    "large_kinfu": "LargeKinfu",
    "legacy": "Legacy",
    "linemod": "LineMod",
    "line_descriptor": "LineDescriptor",
    "mcc": "MCC",
    "ml": "ML",
    "optflow": "OptFlow",
    "ocl": "OCL",
    "plot": "Plot",
    "phase_unwrapping": "PhaseUnwrapping",
    "ppf_match_3d": "PPFMatch3D",
    "quality": "Quality",
    "rapid": "Rapid",
    "reg": "Reg",
    "rgbd": "RGBD",
    "saliency": "Saliency",
    "segmentation": "Segmentation",
    "stereo": "Stereo",
    "structured_light": "StructuredLight",
    "text": "Text",
    "utils": "Utils",
    "utils_fs": "UtilsFS",
    "videoio_registry": "VideoIORegistry",
    "xfeatures2d": "XFeatures2D",
    "ximgproc": "XImgProc",
    "xphoto": "XPhoto",
    "wechat_qrcode": "WeChatQRCode",
}

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

def map_argtype_to_type(argtype: str, classname: Optional[str] = None):
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
        if is_struct(argtype, classname=classname):
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

def map_argname_erlang(argname, argtype=None, from_struct=False):
    argname_prefix_re = re.compile(r'^[_]*')
    name = argname_prefix_re.sub('', argname)
    name = f"{name[0:1].upper()}{name[1:]}"
    if from_struct and argtype is not None:
        name = f"evision_internal_structurise:from_struct({name})"
    return name


def map_argtype_to_guard(kind, argname, argtype, classname: Optional[str] = None):
    if kind == 'elixir':
        return map_argtype_to_guard_elixir(argname, argtype, classname=classname)
    elif kind == 'erlang':
        return map_argtype_to_guard_erlang(argname, argtype, classname=classname)
    else:
        print(f'warning: map_argtype_to_guard: unknown kind `{kind}`')

def is_basic_types(argtype: str):
    argtype = argtype.strip()
    if argtype.startswith("vector<"):
        argtype = argtype[len("vector<"):-1]
        return is_basic_types(argtype)
    return argtype in ['bool', 'float', 'double', 'string', 'void*', 'String', 'c_string'] or \
        is_int_type(argtype) or is_tuple_type(argtype)

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
        'VideoCaptureAPIs',
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
        "VolumeType",
        "Volume",
        "kinfu_VolumeType",
        "text_decoder_mode",
        "cuda_ConnectedComponentsAlgorithmsTypes"
    ]
    return argtype in int_types

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
        'Scalar',
        'cv::Scalar',
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
    special_structs = {
        # todo: UMat should be in its own module
        'UMat': 'Evision.Mat'
    }
    struct_types = {
        'Mat': 'Evision.Mat',
        'GpuMat': 'Evision.CUDA.GpuMat',
        'cuda_GpuMat': 'Evision.CUDA.GpuMat',
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
        'BaseCascadeClassifier': 'Evision.BaseCascadeClassifier',
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

        # opencv_contrib
        "CharucoBoard": "Evision.ArUco.CharucoBoard",
        "AffineTransformer": "Evision.AffineTransformer",
        "EMDHistogramCostExtractor": "Evision.EMDHistogramCostExtractor",
        "HausdorffDistanceExtractor": "Evision.HausdorffDistanceExtractor",
        "HistogramCostExtractor": "Evision.HistogramCostExtractor",
        "NormHistogramCostExtractor": "Evision.NormHistogramCostExtractor",
        "ShapeContextDistanceExtractor": "Evision.ShapeContextDistanceExtractor",
        "ShapeDistanceExtractor": "Evision.ShapeDistanceExtractor",
        "ShapeTransformer": "Evision.ShapeTransformer",
        "ThinPlateSplineShapeTransformer": "Evision.ThinPlateSplineShapeTransformer",
        "TrackerCSRT": "Evision.TrackerCSRT",
        "TrackerCSRT_Params": "Evision.TrackerCSRT.Params",
        "TrackerKCF": "Evision.TrackerKCF",
        "TrackerKCF_Params": "Evision.TrackerKCF.Params",
        "HistogramPhaseUnwrapping": "Evision.HistogramPhaseUnwrapping",
        "HistogramPhaseUnwrapping_Params": "Evision.HistogramPhaseUnwrapping.Params",
        "Facemark": "Evision.Face.Facemark",

        "ImgHashBase": "Evision.ImgHash.ImgHashBase",
        "ColorMomentHash": "Evision.ImgHash.ColorMomentHash",
        "MarrHildrethHash": "Evision.ImgHash.MarrHildrethHash",
        "PHash": "Evision.ImgHash.PHash",
        "RadialVarianceHash": "Evision.ImgHash.RadialVarianceHash",

        "GrayCodePattern": "Evision.StructuredLight.GrayCodePattern",
        "SinusoidalPattern": "Evision.StructuredLight.SinusoidalPattern",
        "SinusoidalPattern_Params": "Evision.StructuredLight.SinusoidalPattern.Params",
        "StructuredLightPattern": "Evision.StructuredLight.StructuredLightPattern",

        "DnnSuperResImpl": "Evision.DNNSuperRes.DNNSuperResImpl",

        "legacy_Tracker": "Evision.Legacy.MultiTracker",

        "ERFilter": "Evision.Text.ERFilter",
        "ERFilter_Callback": "Evision.Text.ERFilter.Callback",
        "OCRBeamSearchDecoder_ClassifierCallback": "Evision.Text.OCRBeamSearchDecoder.ClassifierCallback",
        "OCRHMMDecoder_ClassifierCallback": "Evision.Text.OCRHMMDecoder.ClassifierCallback",

        "AdaptiveManifoldFilter": "Evision.XImgProc.AdaptiveManifoldFilter",
        "ContourFitting": "Evision.XImgProc.ContourFitting",
        "DisparityWLSFilter": "Evision.XImgProc.DisparityWLSFilter",
        "DTFilter": "Evision.XImgProc.DTFilter",
        "EdgeAwareInterpolator": "Evision.XImgProc.EdgeAwareInterpolator",
        "EdgeBoxes": "Evision.XImgProc.EdgeBoxes",
        "EdgeDrawing": "Evision.XImgProc.EdgeDrawing",
        "EdgeDrawing_Params": "Evision.XImgProc.EdgeDrawing.Params",
        "FastBilateralSolverFilter": "Evision.XImgProc.FastBilateralSolverFilter",
        "FastGlobalSmootherFilter": "Evision.XImgProc.FastGlobalSmootherFilter",
        "FastLineDetector": "Evision.XImgProc.FastLineDetector",
        "GraphSegmentation": "Evision.XImgProc.GraphSegmentation",
        "GuidedFilter": "Evision.XImgProc.GuidedFilter",
        "RFFeatureGetter": "Evision.XImgProc.RFFeatureGetter",
        "RICInterpolator": "Evision.XImgProc.RICInterpolator",
        "ScanSegment": "Evision.XImgProc.ScanSegment",
        "SelectiveSearchSegmentation": "Evision.XImgProc.SelectiveSearchSegmentation",
        "SelectiveSearchSegmentationStrategyColor": "Evision.XImgProc.SelectiveSearchSegmentationStrategyColor",
        "SelectiveSearchSegmentationStrategyFill": "Evision.XImgProc.SelectiveSearchSegmentationStrategyFill",
        "SelectiveSearchSegmentationStrategy": "Evision.XImgProc.SelectiveSearchSegmentationStrategy",
        "SelectiveSearchSegmentationStrategyMultiple": "Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple",
        "SelectiveSearchSegmentationStrategySize": "Evision.XImgProc.SelectiveSearchSegmentationStrategySize",
        "SelectiveSearchSegmentationStrategyTexture": "Evision.XImgProc.SelectiveSearchSegmentationStrategyTexture",
        "StructuredEdgeDetection": "Evision.XImgProc.StructuredEdgeDetection",
        "SuperpixelLSC": "Evision.XImgProc.SuperpixelLSC",
        "SuperpixelSEEDS": "Evision.XImgProc.SuperpixelSEEDS",
        "SuperpixelSLIC": "Evision.XImgProc.SuperpixelSLIC",

        "GrayworldWB": "Evision.XPhoto.GrayworldWB",
        "LearningBasedWB": "Evision.XPhoto.LearningBasedWB",
        "SimpleWB": "Evision.XPhoto.SimpleWB",
        "TonemapDurand": "Evision.XPhoto.TonemapDurand",

        "BackgroundSubtractorCNT": "Evision.BgSegm.BackgroundSubtractorCNT",
        "BackgroundSubtractorGMG": "Evision.BgSegm.BackgroundSubtractorGMG",
        "BackgroundSubtractorGSOC": "Evision.BgSegm.BackgroundSubtractorGSOC",
        "BackgroundSubtractorLSBP": "Evision.BgSegm.BackgroundSubtractorLSBP",
        "BackgroundSubtractorMOG": "Evision.BgSegm.BackgroundSubtractorMOG",
        "SyntheticSequenceGenerator": "Evision.BgSegm.SyntheticSequenceGenerator",

        "ColoredKinFu": "Evision.ColoredKinFu",
        "LargeKinfu": "Evision.LargeKinfu",
        "linemod_Detector": "Evision.LineMod.Detector",
        "Template": "Evision.LineMod.Template",

        "Matx33f": "Evision.Mat",
        "Matx33d": "Evision.Mat",
        "Matx44f": "Evision.Mat",
        "Matx44d": "Evision.Mat",
        "TrackerNano": "Evision.TrackerNano",
        "TrackerNano_Params": "Evision.TrackerNano.Params",

        "ArucoDetector": "Evision.ArUco.ArucoDetector",

        "mcc_CChecker": "Evision.MCC.CCheckerDetector",
        "dnn_Net": "Evision.DNN.Net",

        "BinaryDescriptor": "Evision.LineDescriptor.BinaryDescriptor",
        "KeyLine": "Evision.LineDescriptor.BinaryDescriptor.KeyLine",
        "BinaryDescriptorMatcher": "Evision.LineDescriptor.BinaryDescriptorMatcher",
        "LSDParam": "Evision.LineDescriptor.LSDParam",
        "LSDDetectorWithParams": "Evision.LineDescriptor.LSDDetector",
        "LSDDetector": "Evision.LineDescriptor.LSDDetector",

        "Pose3DPtr": "Evision.PPFMatch3D.Pose3D",
        "Pose3D": "Evision.PPFMatch3D.Pose3D",
        "PPF3DDetector": "Evision.PPFMatch3D.PPF3DDetector",

        "WeChatQRCode": "Evision.WeChatQRCode.WeChatQRCode",

        # CUDA
        "cuda_BackgroundSubtractorMOG": "Evision.CUDA.BackgroundSubtractorMOG",
        "cuda_BackgroundSubtractorMOG2": "Evision.CUDA.BackgroundSubtractorMOG2",
        "cuda_CascadeClassifier": "Evision.CUDA.CascadeClassifier",
        "cuda_CLAHE": "Evision.CUDA.CLAHE",
        "cudacodec_VideoWriter": "Evision.CUDACodec.VideoWriter",
        "cuda_DescriptorMatcher": "Evision.CUDA.DescriptorMatcher",
        "cuda_DisparityBilateralFilter": "Evision.CUDA.DisparityBilateralFilter",
        "cuda_FastFeatureDetector": "Evision.CUDA.FastFeatureDetector",
        "cuda_ORB": "Evision.CUDA.ORB",
        "cuda_StereoBeliefPropagation": "Evision.CUDA.StereoBeliefPropagation",
        "cuda_StereoBM": "Evision.CUDA.StereoBM",
        "cuda_StereoConstantSpaceBP": "Evision.CUDA.StereoConstantSpaceBP",
        "cuda_StereoSGM": "Evision.CUDA.StereoSGM",
    }

    # argtype => classname => module name
    strict_match = {
        "Board": {"aruco_Board": "Evision.ArUco.Board"},
        "DetectorParameters": {"aruco_DetectorParameters": "Evision.ArUco.DetectorParameters"},
        "AverageHash": {"img_hash_AverageHash": "Evision.ImgHash.AverageHash"},
        "BlockMeanHash": {"img_hash_BlockMeanHash": "Evision.ImgHash.BlockMeanHash"},
        "PhaseUnwrapping": {"phase_unwrapping_PhaseUnwrapping": "Evision.PhaseUnwrapping.PhaseUnwrapping"},
        "MACE": {"face_MACE": "Evision.Face.MACE"},
        "ml_SVM": {"quality_QualityBRISQUE": "Evision.ML.SVM"},
        "legacy_TrackerBoosting": {"legacy_TrackerBoosting": "Evision.Legacy.TrackerBoosting"},
        "legacy_TrackerCSRT": {"legacy_TrackerCSRT": "Evision.Legacy.TrackerCSRT"},
        "legacy_TrackerKCF": {"legacy_TrackerKCF": "Evision.Legacy.TrackerKCF"},
        "legacy_TrackerMIL": {"legacy_TrackerMIL": "Evision.Legacy.TrackerMIL"},
        "legacy_TrackerMOSSE": {"legacy_TrackerMOSSE": "Evision.Legacy.TrackerMOSSE"},
        "legacy_TrackerMedianFlow": {"legacy_TrackerMedianFlow": "Evision.Legacy.TrackerMedianFlow"},
        "legacy_TrackerTLD": {"legacy_TrackerTLD": "Evision.Legacy.TrackerTLD"},
        "CUDA": {"cuda_SURF_CUDA": "Evision.CUDA.SURFCUDA"},
        "SURF_CUDA": {"cuda_SURF_CUDA": "Evision.CUDA.SURFCUDA"},
        "Params": {
            "large_kinfu_LargeKinfu": "Evision.LargeKinfu.Params",
            "large_kinfu_Params": "Evision.LargeKinfu.Params",
            "colored_kinfu_Params": "Evision.ColoredKinFu.Params",
            "colored_kinfu_ColoredKinFu": "Evision.ColoredKinFu.Params",
        },
        "large_kinfu_Params": {"large_kinfu_Params": "Evision.LargeKinfu.Params"},
        "colored_kinfu_Params": {
            "colored_kinfu_Params": "Evision.ColoredKinFu.Params",
        },
        "kinfu_Params": {
            "kinfu_Params": "Evision.KinFu.Params",
            "dynafu_DynaFu": "Evision.DynaFu.Params"
        },
        "ICP": {
            "ppf_match_3d_ICP": "Evision.PPFMatch3D.ICP"
        }
    }
    second_ret = None

    argtype = argtype.strip()
    second_ret = None
    if argtype.startswith('Ptr<'):
        argtype = argtype[len('Ptr<'):-1].strip()
    arg_is_struct = argtype in struct_types or argtype in special_structs

    if argtype in ["pair<int, double>", "Scalar", "kinfu_VolumeType", "VolumeType", "Volume"]:
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
        return map_argtype_in_docs_elixir(kind, argtype, classname)
    elif kind == 'erlang':
        return map_argtype_in_docs_erlang(kind, argtype, classname)
    else:
        return ''

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
        'RotatedRect': '{centre={x, y}, size={s1, s2}, angle}'
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
        'RotatedRect': '{centre={x, y}, size={s1, s2}, angle}'
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
    'ImageFeatures': 'Evision.Detail.ImageFeatures.t()',
    'UsacParams': 'Evision.UsacParams.t()',
    'MatchesInfo': 'Evision.MatchesInfo.t()',
    'CirclesGridFinderParameters': 'Evision.CirclesGridFinderParameters.t()',
    'CameraParams': 'Evision.Detail.CameraParams.t()',
    'MatShape': 'list(integer())',
    'KeyPoint': 'Evision.KeyPoint.t()',
    'VideoCaptureAPIs': 'number()',
    'ParamGrid': 'Evision.ML.ParamGrid.t()',
    'Layer': 'Evision.DNN.Layer.t()'
}

def map_argtype_in_spec(kind: str, classname: str, argtype: str, is_in: bool, decl: list) -> str:
    if kind == 'elixir':
        return map_argtype_in_spec_elixir(classname, argtype, is_in, decl)
    elif kind == 'erlang':
        return map_argtype_in_spec_erlang(classname, argtype, is_in, decl)
    else:
        return ''

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

    argtype = argtype.strip()
    if argtype.startswith("cv::"):
        argtype = argtype[4:]

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
        return 'ok'
    elif argtype == 'Range':
        return '{integer(), integer()} | all'
    elif is_in and argtype in ['Mat', 'UMat', 'cv::Mat', 'cv::UMat']:
        return '#evision_mat{}'
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
        return '#evision_ocl_device{}'
    elif argtype == 'Index' and classname == 'flann_Index':
         return '#evision_flann_index{}'
    else:
        if argtype == 'LayerId':
            return 'term()'
        if argtype == 'GpuMat' or argtype == 'cuda::GpuMat':
            return '#evision_cuda_gpumat{}'
        if argtype == 'IndexParams' or argtype == 'SearchParams' or argtype == 'Moments':
            return f'map()'
        if argtype in ['Board', 'Dictionary'] and len(decl) > 0 and decl[0].startswith("cv.aruco."):
            return '#evision_aruco_board{}'
        if argtype in ['Board', 'Dictionary'] and len(decl) > 0 and decl[0].startswith("cv.aruco."):
            return '#evision_aruco_board{}'
        else:
            print(f'warning: generate_spec: unknown argtype `{argtype}`, input_arg? {is_in}, class={classname}')
            raise RuntimeError("erlang spec")
            return 'term()'

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
        if argtype == 'IndexParams' or argtype == 'SearchParams' or argtype == 'Moments':
            return f'map()'
        if len(decl) > 0 and decl[0].startswith("cv.aruco.") and argtype in ['Board', 'Dictionary']:
            if argtype == 'Board':
                return f'Evision.ArUco.Board.t()'
            elif argtype == 'Dictionary':
                return f'Evision.ArUco.Dictionary.t()'
        else:
            print(f'warning: generate_spec: unknown argtype `{argtype}`, input_arg? {is_in}, class={classname}')
            return 'term()'

def map_argtype_to_guard_elixir(argname, argtype, classname: Optional[str] = None):
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
    elif is_struct(argtype, classname=classname):
        _, struct_name = is_struct(argtype, also_get='struct_name', classname=classname)
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
        if argtype == 'IndexParams' or argtype == 'SearchParams' or argtype == 'Moments':
            return f'is_map({argname})'
        else:
            return ''


def map_argtype_to_guard_erlang(argname, argtype, classname: Optional[str] = None):
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
    elif is_struct(argtype, classname=classname):
        _, struct_name = is_struct(argtype, also_get='struct_name', classname=classname)
        struct_name = struct_name.replace(".", "_").lower()
        if not struct_name.startswith("evision_"):
            struct_name = f"evision_{struct_name}"
        return f'(is_tuple({argname}) and tuple_size({argname}) > 0 and (element(1, {argname}) == {struct_name}))'
    elif is_ref_or_struct(argtype):
        return f'is_reference({argname})'
    elif is_list_type(argtype):
        return f'is_list({argname})'
    else:
        if argtype == 'LayerId':
            return ''
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
        "GStreamingCompiled": "gStreamingCompiled",

        "EMDHistogramCostExtractor": "emdHistogramCostExtractor"
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
