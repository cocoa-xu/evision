#!/usr/bin/env python3
import distutils.sysconfig as sysconfig
import subprocess
import sys
import os

def print_pylib(M, m, p=None, extension='dylib'):
    py_lib = "{}/libpython{}.{}.{}.{}".format(sysconfig.get_config_var("LIBDIR"), M, m, p, extension)
    if p is None:
        py_lib = "{}/libpython{}.{}.{}".format(sysconfig.get_config_var("LIBDIR"), M, m, extension)
    if os.path.isfile(py_lib):
        print(py_lib, end='')
        sys.exit(0)

(M, m, p) = sys.version_info[:3]
print_pylib(M, m, p, 'dylib')
print_pylib(M, m, None, 'dylib')
print_pylib(M, m, p, 'so')
print_pylib(M, m, None, 'so')
