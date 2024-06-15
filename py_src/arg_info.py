#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from helper import *


class ArgInfo(object):
    def __init__(self, atype, name, default_value, modifiers=(),
                 enclosing_arg=None):
        # type: (ArgInfo, str, str, str, tuple[str, ...], ArgInfo | None) -> None
        self.tp = handle_ptr(atype)
        self.name = name
        self.defval = default_value
        self._modifiers = tuple(modifiers)
        self.isarray = False
        self.is_smart_ptr = self.tp.startswith('Ptr<')  # FIXIT: handle through modifiers - need to modify parser
        self.arraylen = 0
        self.arraycvt = None
        for m in self._modifiers:
            if m.startswith("/A"):
                self.isarray = True
                self.arraylen = m[2:].strip()
            elif m.startswith("/CA"):
                self.isarray = True
                self.arraycvt = m[2:].strip()
        self.py_inputarg = False
        self.py_outputarg = False
        self.enclosing_arg = enclosing_arg

    def __str__(self):
        return 'ArgInfo("{}", tp="{}", default="{}", in={}, out={})'.format(
            self.name, self.tp, self.defval, self.inputarg,
            self.outputarg
        )

    def __repr__(self):
        return str(self)

    @property
    def export_name(self):
        return self.name

    @property
    def inputarg(self):
        return '/O' not in self._modifiers

    @property
    def arithm_op_src_arg(self):
        return '/AOS' in self._modifiers

    @property
    def outputarg(self):
        return '/O' in self._modifiers or '/IO' in self._modifiers

    @property
    def pathlike(self):
        return '/PATH' in self._modifiers
    
    @property
    def has_default(self):
        return '/PATH' in self._modifiers

    @property
    def returnarg(self):
        return self.outputarg

    @property
    def isrvalueref(self):
        return '/RRef' in self._modifiers

    @property
    def full_name(self):
        if self.enclosing_arg is None:
            return self.name
        return self.enclosing_arg.name + '.' + self.name

    def isbig(self):
        return self.tp in ["Mat", "vector_Mat",
                           "cuda::GpuMat", "cuda_GpuMat", "GpuMat",
                           "vector_GpuMat", "vector_cuda_GpuMat",
                           "UMat", "vector_UMat"] # or self.tp.startswith("vector")

    def crepr(self, overwrite_defval=None):
        # has_default = 0
        # if (defval is not None and len(defval) > 0) or self.defval == self.tp + "()":
        #     has_default = 1
        has_default = self.defval == self.tp + "()"
        if overwrite_defval is not None and len(overwrite_defval) > 0:
            has_default = 1
        arg  = 0x01 if self.outputarg else 0x0
        arg += 0x02 if self.arithm_op_src_arg else 0x0
        arg += 0x04 if self.pathlike else 0x0
        arg += 0x08 if has_default else 0x0
        return "ArgInfo(\"%s\", %d)" % (self.name, arg)
