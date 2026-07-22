#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Canonical OpenCV depth table.

Mirrors ``modules/core/include/opencv2/core/hal/interface.h``. The expression
generators and the BEAM constant templates all read their values from here, so
adopting a new OpenCV depth is a single edit.
"""

CV_CN_SHIFT = 5

# Declaration order fixes the order of the generated helpers; the codes being
# contiguous from zero is what makes CV_DEPTH_CURR_MAX below correct.
CV_DEPTHS = {
    'CV_8U': 0,
    'CV_8S': 1,
    'CV_16U': 2,
    'CV_16S': 3,
    'CV_32S': 4,
    'CV_32F': 5,
    'CV_64F': 6,
    'CV_16F': 7,
    'CV_16BF': 8,
    'CV_Bool': 9,
    'CV_64U': 10,
    'CV_64S': 11,
    'CV_32U': 12,
}

CV_DEPTH_MAX = 1 << CV_CN_SHIFT
CV_DEPTH_CURR_MAX = len(CV_DEPTHS)

CV_CONSTANTS = {
    **CV_DEPTHS,
    'CV_DEPTH_CURR_MAX': CV_DEPTH_CURR_MAX,
    'CV_DEPTH_MAX': CV_DEPTH_MAX,
}
