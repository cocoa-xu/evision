#!/usr/bin/env python3
import distutils.sysconfig as sysconfig
import subprocess
import sys
import os
(M, m) = sys.version_info[:2]
py_lib = "{}/libpython{}.{}.dylib".format(sysconfig.get_config_var("LIBDIR"), M, m)
if not os.path.isfile(py_lib):
    py_lib = py_lib.replace("dylib", "so")
print(py_lib)
