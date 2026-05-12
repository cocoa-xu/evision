"""Hand-curated tables that classify OpenCV C/C++ types and functions.

These tables are referenced by the binding generator while walking OpenCV's
headers. They were previously buried inside `helper.py`; pulling them here
makes them discoverable when adding support for a new OpenCV release.
"""

NIF_PREFIX = 'evision_cv_'

# Reserved by Erlang/Elixir. C/C++ keywords are excluded — they cannot be
# variable identifiers in the headers we parse so they never reach the
# emitter as argument names.
RESERVED_KEYWORDS = [
    "true", "false", "nil", "as", "def", "end",
    "rescue", "defmodule", "defmacro", "when", "in", "fn", "with",
    "of",
]

FORBIDDEN_ARG_TYPES = ["void*"]

IGNORED_ARG_TYPES = ["RNG*"]

PASS_BY_VAL_TYPES = [
    "Point*", "Point2f*", "Rect*", "String*",
    "double*", "float*", "int*",
]

# Functions that should be dispatched on the BEAM dirty IO scheduler
# (`F_IO` macro) instead of the default dirty CPU scheduler (`F_CPU`).
# The suffix heuristic in func_info.py and gen2.py picks up `_read*`,
# `_load_static`, `_write*`, `_save*` automatically; this list covers
# the names that don't match the suffix pattern.
IO_BOUND_FUNCS = [
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

# Functions whose generated wrapper needs hand-written treatment. Listed by
# their full evision_cv_<name> NIF symbol — the prefix is added below.
_SPECIAL_HANDLING_NAMES = [
    'imshow',
    'waitKey',
    'destroyWindow',
    'destroyAllWindows',
    'imdecode',
    'videoCapture_waitAny_static',
    'videoCapture_waitAny',
]

SPECIAL_HANDLING_FUNCS = [f'{NIF_PREFIX}{n}' for n in _SPECIAL_HANDLING_NAMES]

# Same idea but the special handling only applies on the BEAM emitter side
# (the C/C++ wrapper is still auto-generated).
_SPECIAL_HANDLING_BEAM_ONLY_NAMES = [
    'dnn_NMSBoxes',
    'dnn_NMSBoxesBatched',
    'dnn_softNMSBoxes',
]

SPECIAL_HANDLING_FUNCS_ONLY_IN_BEAM = [
    f'{NIF_PREFIX}{n}' for n in _SPECIAL_HANDLING_BEAM_ONLY_NAMES
]
