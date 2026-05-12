#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Thin CLI wrapper around `pipeline.Pipeline`.

Invoked from CMakeLists.txt:

    python3 py_src/gen2.py --c_src=... --elixir_gen=... --erlang_gen=...
                           --headers=... --lang=... --modules=...

This file intentionally does no work itself; the binding-generator
implementation lives in `pipeline.py`.
"""

from __future__ import print_function

import argparse
import json
import os
from os import makedirs
from shutil import rmtree

import hdr_parser
from pipeline import Pipeline


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--c_src", type=str, default="./c_src", help="Path to the c_src dir")
    parser.add_argument("--elixir_gen", type=str, default="./lib", help="Path to the output dir of elixir binding files")
    parser.add_argument("--erlang_gen", type=str, default="./src", help="Path to the output dir of erlang binding files")
    parser.add_argument("--headers", help="Path to the headers.txt/header-contrib.txt in c_src, or a 4.13+ gen_python_config.json")
    parser.add_argument("--lang", type=str, help="Comma-seperated values. erlang,elixir")
    parser.add_argument("--modules", type=str, default='', help="Comma-seperated values.")
    args = parser.parse_args()

    srcfiles = hdr_parser.opencv_hdr_list
    dstdir = args.c_src
    elixir_dstdir = args.elixir_gen
    erlang_dstdir = args.erlang_gen
    preprocessor_definitions = None

    if len(args.headers) > 4:
        if args.headers.endswith('.json'):
            # OpenCV 4.13+: CMake emits gen_python_config.json with both the
            # header list and the preprocessor_definitions the new hdr_parser
            # needs to evaluate `#if HAVE_OPENCV_*`, `#if CV_VERSION_MAJOR < 5`
            # etc.
            with open(args.headers, 'r') as f:
                cfg = json.load(f)
            srcfiles = cfg['headers']
            preprocessor_definitions = cfg.get('preprocessor_definitions', {})
        else:
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
        if l not in ['elixir', 'erlang']:
            raise RuntimeError(f"unknown value found in EVISION_GENERATE_LANG: `{l}`. Allowed values are `elixir`, `erlang`")

    # default
    enabled_modules = ['calib3d', 'core', 'features2d', 'flann', 'highgui', 'imgcodecs', 'imgproc', 'ml', 'photo',
                       'stitching', 'ts', 'video', 'videoio', 'dnn']
    if len(args.modules) > 0:
        enabled_modules = args.modules.split(",")
    generator = Pipeline(enabled_modules, lang)
    rmtree(elixir_dstdir)
    rmtree(erlang_dstdir)
    makedirs(elixir_dstdir)
    makedirs(erlang_dstdir)
    generator.gen(srcfiles, dstdir, elixir_dstdir, erlang_dstdir,
                  preprocessor_definitions=preprocessor_definitions)
