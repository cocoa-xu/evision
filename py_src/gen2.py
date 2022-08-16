#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from __future__ import print_function

import ast
import sys
from pathlib import Path

import hdr_parser
import re
from erl_enum_expression_generator import ErlEnumExpressionGenerator
import evision_templates as ET
from helper import *
from namespace import Namespace
from func_info import FuncInfo
from class_info import ClassInfo
from fixes import evision_elixir_fixes, evision_erlang_fixes


if sys.version_info[0] >= 3:
    from io import StringIO
else:
    from cStringIO import StringIO


class BeamWrapperGenerator(object):
    def __init__(self, enabled_modules):
        self.clear()
        self.argname_prefix_re = re.compile(r'^[_]*')
        self.inline_docs_code_type_re = re.compile(r'@code{.(.*)}')
        self.inline_docs_inline_math_re = re.compile(r'(?:.*?)\\\\f[$\[](.*?)\\\\f[$\]]', re.MULTILINE|re.DOTALL)
        self.enabled_modules = enabled_modules

    def clear(self):
        self.classes = {}
        self.namespaces = {}
        self.consts = {}
        self.enums = {}
        self.enum_names = {}
        self.enum_names_io = StringIO()
        self.enum_names_io_erlang = StringIO()
        self.code_include = StringIO()
        self.code_enums = StringIO()
        self.code_types = StringIO()
        self.code_funcs = StringIO()
        self.code_ns_reg = StringIO()
        self.evision_nif = StringIO()
        self.evision_nif_erlang = StringIO()
        self.evision_elixir = StringIO()
        self.evision_erlang = StringIO()
        self.opencv_func = StringIO()
        self.opencv_func.deferror = {}
        self.opencv_func.doc_written = {}
        self.opencv_func_erlang = StringIO()
        self.opencv_func_erlang.deferror = {}
        self.opencv_func_erlang.doc_written = {}
        self.opencv_modules = {}
        self.code_type_publish = StringIO()
        self.py_signatures = dict()
        self.class_idx = 0
        self.evision_nif_names = dict()

    def add_class(self, stype, name, decl):
        classinfo = ClassInfo(name, decl)

        if classinfo.name in self.classes:
            print("Generator error: class %s (cname=%s) already exists" \
                % (classinfo.name, classinfo.cname))
            # sys.exit(-1)
            return

        classinfo.decl_idx = self.class_idx
        self.class_idx += 1

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


    def add_const(self, name, decl):
        (module_name, erl_const_name) = name.split('.')[-2:]
        val = decl[1]
        val_tree = ast.parse(val, mode='eval')
        val_gen = ErlEnumExpressionGenerator()
        val_gen.visit(val_tree)
        if not val_gen.skip_this:
            val = val_gen.expression
            val_erlang = val_gen.expression_erlang
            erl_const_name = self.map_elixir_argname(erl_const_name, ignore_upper_starting=True)
            if self.enum_names.get(val, None) is not None:
                val = f'cv_{val}()'
            if self.enum_names.get(erl_const_name, None) is None:
                self.enum_names[erl_const_name] = val
                self.enum_names_io.write(f"  @doc type: :constants\n  def cv_{erl_const_name}, do: {val}\n")
                self.enum_names_io_erlang.write(f"cv_{erl_const_name}() ->\n    {val_erlang}.\n")
            else:
                if self.enum_names[erl_const_name] != val:
                    erl_const_name = self.map_elixir_argname(f'{module_name}_{erl_const_name}', ignore_upper_starting=True)
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
        wname = normalize_class_name(name)
        if wname.endswith("<unnamed>"):
            wname = None
        else:
            self.enums[wname] = name
        const_decls = decl[3]

        for decl in const_decls:
            name = decl[0]
            self.add_const(name.replace("const ", "").strip(), decl)

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

    def map_argtype_to_type(self, argtype):
        if argtype == 'int' or argtype == 'size_t':
            return 'int'
        elif argtype == 'bool':
            return 'bool'
        elif argtype == 'double':
            return 'numerical'
        elif argtype == 'float':
            return 'float'
        elif argtype == 'String' or argtype == 'c_string' or argtype == 'char':
            return 'binary'
        elif argtype == 'Size' or argtype == 'Scalar' or argtype == 'Point2f' or argtype == 'Point':
            return 'list'
        elif argtype[:7] == 'vector_':
            return 'list'
        else:
            return 't'

    def map_argtype_to_guard(self, argname, argtype):
        if argtype == 'int' or argtype == 'size_t':
            return f'is_integer({argname})'
        elif argtype == 'bool':
            return f'is_boolean({argname})'
        elif argtype == 'double':
            return f'is_number({argname})'
        elif argtype == 'float':
            return f'is_float({argname})'
        elif argtype == 'String' or argtype == 'c_string':
            return f'is_binary({argname})'
        elif argtype == 'char':
            return f'is_binary({argname})'
        elif argtype == 'Size' or argtype == 'Scalar':
            return f'is_list({argname})'
        elif argtype == 'Point2f' or argtype == 'Point':
            return f'is_list({argname})'
        elif argtype[:7] == 'vector_':
            return f'is_list({argname})'
        else:
            if argtype == 'LayerId':
                return ''
            elif argtype == 'TermCriteria':
                return f'is_tuple({argname})'
            else:
                return f'is_reference({argname})'

    def map_argtype_to_guard_erlang(self, argname, argtype):
        if argtype == 'int' or argtype == 'size_t':
            return f'is_integer({argname})'
        elif argtype == 'bool':
            return f'is_boolean({argname})'
        elif argtype == 'double':
            return f'is_number({argname})'
        elif argtype == 'float':
            return f'is_float({argname})'
        elif argtype == 'String' or argtype == 'c_string':
            return f'is_list({argname})'
        elif argtype == 'char':
            return f'is_list({argname})'
        elif argtype == 'Size' or argtype == 'Scalar':
            return f'is_list({argname})'
        elif argtype == 'Point2f' or argtype == 'Point':
            return f'is_list({argname})'
        elif argtype[:7] == 'vector_':
            return f'is_list({argname})'
        else:
            if argtype == 'LayerId':
                return ''
            elif argtype == 'TermCriteria':
                return f'is_tuple({argname})'
            else:
                return f'is_reference({argname})'

    def map_elixir_argname(self, argname, ignore_upper_starting=False):
        name = ""
        if argname in reserved_keywords():
            if argname == 'fn':
                name = 'func'
            elif argname == 'end':
                name = 'end_arg'
            else:
                name = f'arg_{argname}'
        else:
            name = self.argname_prefix_re.sub('', argname)
        if ignore_upper_starting:
            return name
        return f"{name[0:1].lower()}{name[1:]}"
    
    def map_erlang_argname(self, argname):
        name = self.argname_prefix_re.sub('', argname)
        return f"{name[0:1].upper()}{name[1:]}"

    def handle_inline_math_escaping(self, text, start_pos=0):
        processed = text[:start_pos]
        todo = text[start_pos:]
        if len(todo) == 0:
            return text
        inline_math_match = self.inline_docs_inline_math_re.match(todo)
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
                        math_lines += line
                    math_lines += "\n"
                math_text = math_lines[:-1]
                replaced = processed + todo[:start] + math_text + todo[end:]
                return self.handle_inline_math_escaping(replaced, len(processed) + start + len(math_text) + 1)
            else:
                return text
        else:
            return text

    def gen_erl_declaration(self, wname, name, func, writer=None, writer_erlang=None, is_ns=False, is_constructor=False, is_prop=False, prop_class=None, separated_ns=None):
        # functions in namespaces goes to 'evision_nif.ex' and 'evision_{module}.ex'
        # 'evision_nif.ex' contains the declarations of all NIFs
        # 'evision_{module}.ex' contains readable/friendly function names

        if is_prop:
            # wname => class
            # name  => prop name
            # func  => ClassProp
            erl_name = prop_class.wname + '_get_' + name
            if erl_name[0].isupper():
                erl_name = map_uppercase_to_erlang_name(erl_name)

            func_name = "evision_" + prop_class.wname + '_get_' + name
            writer.write(f"  def get_{name}(self) do\n"
                         f"    :evision_nif.{erl_name}(self)\n"
                         "  end\n")

            writer_erlang.write(f"get_{name}(Self) -> \n"
                         f"    evision_nif:{erl_name}(Self).\n\n")

            name_arity_elixir = f'get_{name}!/1'
            name_arity_erlang = f'get_{name}!/1'
            name_arity_erlang_opt = None
            if not writer.deferror.get(name_arity_elixir, False):
                writer.write(f"  deferror get_{name}(self)\n")
                writer.deferror[name_arity_elixir] = True

            self.evision_nif.write(f'  def {erl_name}(_self), do: :erlang.nif_error("{wname}::{name} getter not loaded")\n')
            self.evision_nif_erlang.write(f'{erl_name}(_self) ->\n    not_loaded(?LINE).\n')
            self.code_ns_reg.write(f'    F({erl_name}, {func_name}, 1),\n')
            if not func.readonly:
                erl_name = prop_class.wname + '_set_' + name
                if erl_name[0].isupper():
                    erl_name = map_uppercase_to_erlang_name(erl_name)
                func_name = "evision_" + prop_class.wname + '_set_' + name
                writer.write(f"  def set_{name}(self, opts) do\n    :evision_nif.{erl_name}(self, opts)\n  end\n")
                writer_erlang.write(f"set_{name}(Self, Options) when is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2 ->\n    evision_nif:{erl_name}(Self, Options).\n\n")
                name_arity_elixir = f'set_{name}!/2'
                name_arity_erlang = None
                name_arity_erlang_opt = f'set_{name}!/2'
                if not writer.deferror.get(name_arity_elixir, False):
                    writer.write(f"  deferror set_{name}(self, opts)\n")
                    writer.deferror[name_arity_elixir] = True

                self.evision_nif.write(f'  def {erl_name}(_self, _opts), do: :erlang.nif_error("{wname}::{name} setter not loaded")\n')
                self.evision_nif_erlang.write(f'{erl_name}(_self, _opts) ->\n    not_loaded(?LINE).\n')
                self.code_ns_reg.write(f'    F({erl_name}, {func_name}, 2),\n')
            return

        erl_name, func_name = func.get_wrapper_name(True)
        if func_name in special_handling_funcs():
            return
        if len(func_name) > 0 and not ('a' <= func_name[0] <= 'z'):
            func_name = func_name.lower()
            erl_name = erl_name.lower()

        if self.evision_nif_names.get(erl_name) != True:
            self.evision_nif_names[erl_name] = True
            nif_args = '_opts \\\\ []'
            nif_args_erlang = '_opts'
            if not is_ns and func.classname and not func.is_static and not is_constructor:
                nif_args = f'_self, {nif_args}'
                nif_args_erlang = f'_self, {nif_args_erlang}'
            self.evision_nif.write(f'  def {erl_name}({nif_args}), do: :erlang.nif_error("{wname}::{name} not loaded")\n')
            self.evision_nif_erlang.write(f'{erl_name}({nif_args_erlang}) ->\n    not_loaded(?LINE).\n\n')

        if writer is None:
            return

        erl_signatures = []
        func_guards = []
        func_guards_erlang = []
        for i in range(len(func.variants)):
            pos_end = -func.variants[i].py_noptargs
            if pos_end == 0:
                pos_end = len(func.variants[i].py_arglist)
            func_guards.append(list(filter(lambda x: x != '', [self.map_argtype_to_guard(self.map_elixir_argname(argname), argtype) for argname, _, argtype in func.variants[i].py_arglist[:pos_end]])))
            func_guards_erlang.append(list(filter(lambda x: x != '', [self.map_argtype_to_guard_erlang(self.map_erlang_argname(argname), argtype) for argname, _, argtype in func.variants[i].py_arglist[:pos_end]])))
            erl_signatures.append(''.join(filter(lambda x: x != '', [self.map_argtype_to_type(argtype) for _, _, argtype in func.variants[i].py_arglist[:pos_end]])))

        func_guards_len_desc = reversed(argsort([len(g) for g in func_guards]))
        unique_signatures = {}

        guarded_elixir_function_groups = {}
        guarded_erlang_function_groups = {}
        for i in func_guards_len_desc:
            name_arity_elixir = None
            elixir_function = StringIO()

            name_arity_erlang = None
            name_arity_erlang_opt = None
            erlang_function = StringIO()
            erlang_function_opt = StringIO()

            i = int(i)
            sign = erl_signatures[i]
            func_guard = func_guards[i]
            func_guard_erlang = func_guards_erlang[i]
            current_func = func.variants[i]
            arglist = current_func.py_arglist
            noptargs = current_func.py_noptargs
            min_args = len(arglist) - noptargs
            has_opts = noptargs > 0
            pos_end = len(arglist) if not has_opts else -noptargs
            opt_args = ''
            opt_args_erlang = ''
            opt_doc = ''
            prototype = f'    Python prototype (for reference): {current_func.py_prototype}'
            if has_opts:
                opt_args = 'opts' if min_args == 0 else ', opts'
                opt_args_erlang = 'Options' if min_args == 0 else ', Options'
                opt_doc = '\n'.join(['    @optional {}: {}'.format(arg_name, argtype) for (arg_name, _, argtype) in arglist[-noptargs:]])
                opt_doc += '\n'
            
            func_args = '{}'.format(", ".join(['{}'.format(self.map_elixir_argname(arg_name)) for (arg_name, _, argtype) in arglist[:pos_end]]))
            func_args_erlang = '{}'.format(", ".join(['{}'.format(self.map_erlang_argname(arg_name)) for (arg_name, _, argtype) in arglist[:pos_end]]))
            func_args_with_opts = ''
            func_args_with_opts_erlang = ''
            if has_opts:
                func_args_with_opts = '{}{}'.format(", ".join(['{}'.format(self.map_elixir_argname(arg_name)) for (arg_name, _, argtype) in arglist[:pos_end]]), opt_args)
                func_args_with_opts_erlang = '{}{}'.format(", ".join(['{}'.format(self.map_erlang_argname(arg_name)) for (arg_name, _, argtype) in arglist[:pos_end]]), opt_args_erlang)
            module_func_name = func_name
            if is_ns:
                if module_func_name != f'{evision_nif_prefix()}{name}':
                    module_func_name = module_func_name[len(evision_nif_prefix()):]
                else:
                    module_func_name = name
                if wname != 'cv':
                    namespace_func = wname + '_' + name
                    if namespace_func != module_func_name:
                        return
                elif name and len(name) > 0 and not ('a' <= name[0] <= 'z'):
                    if module_func_name.startswith(name + '_') or module_func_name.endswith('_create'):
                        return
                    elif module_func_name.startswith('CascadeClassifier') \
                            or module_func_name.startswith('HOGDescriptor') \
                            or module_func_name.startswith('KeyPoint') \
                            or module_func_name.startswith('UMat_') \
                            or module_func_name.startswith('VideoWriter_'):
                        return
                    else:
                        # print(wname, '<|>', name, '<|>', module_func_name, '<|>', separated_ns, is_ns)
                        _ = 0
                else:
                    # print(wname, '<>', name, '<>', module_func_name, '<>', separated_ns, is_ns)
                    _ = 0
            else:
                module_func_name = name
                # if this function is an instance method of a C++ class
                if not is_ns and func.classname and not func.is_static and not is_constructor:
                    if len(func_args) > 0:
                        func_args = f'self, {func_args}'
                        func_args_erlang = f'Self, {func_args_erlang}'
                        if len(func_args_with_opts) > 0:
                            func_args_with_opts = f'self, {func_args_with_opts}'
                            func_args_with_opts_erlang = f'Self, {func_args_with_opts_erlang}'
                    else:
                        func_args = 'self'
                        func_args_with_opts = ''

                        func_args_erlang = 'Self'
                        func_args_with_opts_erlang = ''
            if is_ns and wname != 'cv':
                (writer, writer_erlang), _ = self.get_module_writer(wname, wname=wname, name=name, is_ns=is_ns)
                if module_func_name.startswith(wname+'_'):
                    module_func_name = module_func_name[len(wname)+1:]
                if '_' in module_func_name:
                    return
                else:
                    if len(module_func_name) > 0 and not ('a' <= module_func_name <= 'z'):
                        if len(module_func_name) >= 2 and not ('a' <= module_func_name[1] <= 'z'):
                            func_name_mapping = {
                                'NMSBoxes': 'nmsBoxes',
                                'NMSBoxesRotated': 'nmsBoxesRotated'
                            }
                            if module_func_name in func_name_mapping:
                                module_func_name = func_name_mapping.get(module_func_name)
                            else:
                                print(f'NOTICE:function name in namespace[{wname}]:{module_func_name}')
                                module_func_name = module_func_name.lower()
                        else:
                            module_func_name = module_func_name.lower()
            else:
                if separated_ns is not None and len(separated_ns) > 1:
                    prefix = "_".join(separated_ns[:-1]) + '_'
                    if module_func_name.startswith(prefix):
                        module_func_name = module_func_name[len(prefix):]

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
                    }
                    if module_func_name in mapping:
                        module_func_name = mapping[module_func_name]
                    else:
                        if is_ns and wname == 'cv':
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
                                return
                        module_func_name = module_func_name.lower()

            if func_name.startswith(evision_nif_prefix() + "dnn") and module_func_name == "forward":
                sign = evision_nif_prefix() + "_dnn_forward"
            if func_name.startswith(evision_nif_prefix() + "dnn_dnn_Net") and (module_func_name == "getLayerShapes" or module_func_name == "getLayersShapes"):
                sign = evision_nif_prefix() + "dnn_dnn_Net_" + module_func_name
            if unique_signatures.get(sign, None) is True:
                elixir_function.write('\n'.join(["  # {}".format(line.strip()) for line in opt_doc.split("\n")]))
                elixir_function.write(f'  # def {module_func_name}({func_args}) do\n  #   :evision_nif.{erl_name}({func_args})\n  # end\n')
                if len(func_args_with_opts) > 0:
                    elixir_function.write(f'  # def {module_func_name}({func_args_with_opts}) do\n  #   :evision_nif.{erl_name}({func_args_with_opts})\n  # end\n')
            else:
                unique_signatures[sign] = True

                doc_string = "\n".join('    {}'.format(line.strip()) for line in current_func.docstring.split("\n")).strip()
                if len(doc_string) > 0:
                    doc_string = f'\n    {doc_string}\n'
                else:
                    doc_string = '\n'
                inline_doc = f'\n  @doc """{doc_string}{opt_doc}{prototype}\n'
                if writer.doc_written.get(module_func_name, None) is None:
                    writer.doc_written[module_func_name] = True
                    inline_doc1 = ""
                    last_in_list = False
                    last_is_code = False
                    for line in inline_doc.split("\n"):
                        line = line.replace("\\", r"\\")
                        line = line.replace(r"\\\\", r"\\\\\\\\")
                        strip_line = line.strip()
                        if strip_line.startswith("*"):
                            strip_line = strip_line[1:].strip()
                            line = line.replace("*", "", 1)
                        if strip_line.startswith('"""'):
                            strip_line = strip_line.replace('"""', r'\"\"\"', 1)
                            line = line.replace('"""', r'\"\"\"', 1)
                        if strip_line == "=":
                            inline_doc = inline_doc[:-1] + "="
                            continue

                        if strip_line.startswith("@"):
                            if strip_line != "@doc \"\"\"":
                                if strip_line.startswith("@brief"):
                                    inline_doc1 += "  {}\n".format(strip_line[len("@brief"):].strip())
                                    last_in_list = False
                                elif strip_line.startswith("@overload"):
                                    inline_doc1 += "  Has overloading in C++\n\n"
                                    last_in_list = False
                                elif strip_line.startswith("@note"):
                                    inline_doc1 += "  **Note**: {}\n".format(strip_line[len("@note"):].strip())
                                    last_in_list = False
                                elif strip_line.startswith("@param") or strip_line.startswith("@optional"):
                                    # expecting:
                                    # @param <ARG_NAME>[ <DESCRIPTION GOES HERE>]
                                    # @optional <ARG_NAME>[ <DESCRIPTION GOES HERE>]
                                    arg_desc = strip_line.split(' ', 3)
                                    normalized_arg_name = self.map_elixir_argname(arg_desc[1])
                                    normalized_arg_name = normalized_arg_name.replace(":", "")
                                    if len(arg_desc) == 3:
                                        inline_doc1 += "  - **{}** : {}\n".format(normalized_arg_name, arg_desc[2])
                                    else:
                                        inline_doc1 += "  - **{}**.\n".format(normalized_arg_name)
                                    last_in_list = True
                                elif strip_line.startswith("@code"):
                                    last_in_list = False
                                    last_is_code = True
                                    code_type_match = self.inline_docs_code_type_re.match(strip_line)
                                    inline_doc1 += "  ```"
                                    if code_type_match:
                                        inline_doc1 += code_type_match.group(1)
                                    inline_doc1 += "\n"
                                elif strip_line.startswith("@endcode"):
                                    last_in_list = False
                                    last_is_code = False
                                    inline_doc1 += "  ```\n"
                                else:
                                    inline_doc1 += "  {}\n".format(strip_line)
                                    last_in_list = False
                            else:
                                inline_doc1 += "  @doc \"\"\"\n"
                        elif strip_line.startswith("Python prototype (for reference): "):
                            inline_doc1 += "\n  Python prototype (for reference): \n  ```\n  {}\n  ```\n".format(strip_line[len("Python prototype (for reference): "):])
                            last_in_list = False
                        elif len(strip_line) != 0:
                            if last_is_code:
                                inline_doc1 += "  {}\n".format(strip_line)
                            else:
                                inline_doc1 += "{}{}\n".format("  " if last_in_list else "", line)
                        else:
                            last_in_list = False
                    inline_doc = inline_doc1 + '  """\n'
                    inline_doc = self.handle_inline_math_escaping(inline_doc)
                else:
                    inline_doc = ''.join(['  # {}\n'.format(line.strip()) for line in inline_doc.split("\n")])

                when_guard = ' '
                when_guard_erlang = ' '
                if len(func_guard) > 0:
                    when_guard = ' when '
                    when_guard += ' and '.join(func_guard) + '\n  '
                    when_guard_erlang = ' when '
                    when_guard_erlang += ', '.join(func_guard_erlang)

                opt_args = '' if not has_opts else ' ++ opts'
                opt_args_erlang = '' if not has_opts else ' ++ Options'
                module_func_args = func_args
                module_func_args_erlang = func_args_erlang
                module_func_args_with_opts = func_args_with_opts
                module_func_args_with_opts_erlang = func_args_with_opts_erlang
                positional = 'positional = [{}\n    ]'.format(",".join(['\n      {}: {}'.format(self.map_elixir_argname(arg_name), self.map_elixir_argname(arg_name)) for (arg_name, _, argtype) in arglist[:pos_end]]))
                positional_erlang = 'Positional = [{}\n  ]'.format(",".join(['\n    {}, {}'.format('{' + self.map_elixir_argname(arg_name), self.map_erlang_argname(arg_name) + '}') for (arg_name, _, argtype) in arglist[:pos_end]]))
                func_args = 'positional'
                func_args_erlang = 'Positional'
                if len(func_args_with_opts) > 0:
                    func_args_with_opts = f'positional{opt_args}'
                    func_args_with_opts_erlang = f'Positional{opt_args_erlang}'
                if not is_ns and func.classname and not func.is_static and not is_constructor:
                    func_args = f'self, {func_args}'
                    func_args_erlang = f'Self, {func_args_erlang}'
                    if len(func_args_with_opts) > 0:
                        func_args_with_opts = f'self, {func_args_with_opts}'
                        func_args_with_opts_erlang = f'Self, {func_args_with_opts_erlang}'

                function_group = ""
                if len(func.namespace) > 0:
                    if func.namespace == "cv":
                        function_group = f"  @doc namespace: :cv\n"
                    else:
                        function_group = f"  @doc namespace: :\"{func.namespace}\"\n"

                # this could be better perhaps, but it is hurting my brain...
                if func_name.startswith(evision_nif_prefix() + "dnn") and module_func_name == "forward":
                    inline_doc += function_group
                    name_arity_elixir_identifier = f'{module_func_name}!/2'
                    name_arity_elixir = f'{module_func_name}!/1'
                    elixir_function.write(f'{inline_doc}  def {module_func_name}(self, opts \\\\ []) when is_list(opts) do\n    :evision_nif.{erl_name}(self, opts)\n  end\n')
                    
                    name_arity_erlang = f'{module_func_name}!/1'
                    name_arity_erlang_opt = f'{module_func_name}!/2'
                    erlang_function.write(f'{module_func_name}(Self) ->\n  evision_nif:{erl_name}(Self, []).\n')
                    erlang_function_opt.write(f'{module_func_name}(Self, Options) when is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2 ->\n  evision_nif:{erl_name}(Self, Options).\n')
                    if not writer.deferror.get(name_arity_elixir_identifier, False):
                        elixir_function.write(f"  deferror {module_func_name}(self, opts)\n")
                        writer.deferror[name_arity_elixir_identifier] = True

                    push_to_function_group(name_arity_elixir, guarded_elixir_function_groups, elixir_function)
                    push_to_function_group(name_arity_erlang, guarded_erlang_function_groups, erlang_function)
                    push_to_function_group(name_arity_erlang_opt, guarded_erlang_function_groups, erlang_function_opt)
                    
                    continue

                if func_name.startswith(evision_nif_prefix() + "dnn_dnn_Net") and (module_func_name == "getLayerShapes" or module_func_name == "getLayersShapes"):
                    inline_doc += function_group
                    name_arity_elixir_identifier = f'{module_func_name}!/2'
                    name_arity_elixir = f'{module_func_name}!/1'
                    elixir_function.write(f'{inline_doc}  def {module_func_name}(self, opts \\\\ []) when is_list(opts) do\n    :evision_nif.{erl_name}(self, opts)\n  end\n')
                    
                    name_arity_erlang = f'{module_func_name}!/1'
                    name_arity_erlang_opt = f'{module_func_name}!/2'
                    erlang_function.write(f'{module_func_name}(Self) ->\n  evision_nif:{erl_name}(Self, []).\n')
                    erlang_function_opt.write(f'{module_func_name}(Self, Options) when is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2 ->\n  evision_nif:{erl_name}(Self, Options).\n')
                    
                    if not writer.deferror.get(name_arity_elixir_identifier, False):
                        elixir_function.write(f"  deferror {module_func_name}(self, opts)\n")
                        # writer.write(f"  deferror {module_func_name}(self, opts)\n")
                        writer.deferror[name_arity_elixir_identifier] = True
                    
                    push_to_function_group(name_arity_elixir, guarded_elixir_function_groups, elixir_function)
                    push_to_function_group(name_arity_erlang, guarded_erlang_function_groups, erlang_function)
                    push_to_function_group(name_arity_erlang_opt, guarded_erlang_function_groups, erlang_function_opt)
                    
                    continue

                if len(func_args_with_opts) > 0:
                    inline_doc += function_group
                    when_guard_with_opts = when_guard
                    when_guard_with_opts_erlang = when_guard_erlang
                    if len(when_guard_with_opts.strip()) > 0:
                        when_guard_with_opts = f' {when_guard_with_opts.strip()} and is_list(opts)\n  '
                        when_guard_with_opts_erlang = f' {when_guard_with_opts_erlang.strip()}, is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2'
                    else:
                        when_guard_with_opts = ' when is_list(opts)\n  '
                        when_guard_with_opts_erlang = ' when is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2'

                    elixir_function.write(f'{inline_doc}  def {module_func_name}({module_func_args_with_opts}){when_guard_with_opts}do\n'
                                 f'    {positional}\n'
                                 f'    :evision_nif.{erl_name}({func_args_with_opts})\n'
                                 '  end\n')
                    
                    erlang_function_opt.write(f'{module_func_name}({module_func_args_with_opts_erlang}){when_guard_with_opts_erlang} ->\n'
                                        f'  {positional_erlang},\n'
                                        f'  evision_nif:{erl_name}({func_args_with_opts_erlang}).\n\n'
                    )
                    module_func_args_without_opts_defaults = module_func_args_with_opts.replace('\\\\ []', '').strip()
                    if len(module_func_args_without_opts_defaults) > 0:
                        num_args = len(module_func_args_without_opts_defaults.split(","))
                        name_arity_elixir_identifier = f'{module_func_name}!/{num_args}'
                        if '\\\\ []' in module_func_args_with_opts:
                            name_arity_elixir = f'{module_func_name}!/{num_args}'
                        else:
                            name_arity_elixir = f'{module_func_name}!/{num_args}'
                    else:
                        name_arity_elixir_identifier = f'{module_func_name}!/0'
                        name_arity_elixir = f'{module_func_name}!/0'
                    if not writer.deferror.get(name_arity_elixir_identifier, False):
                        elixir_function.write(f"  deferror {module_func_name}({module_func_args_without_opts_defaults})\n")
                        writer.deferror[name_arity_elixir_identifier] = True
                elixir_function.write(f'{function_group}  def {module_func_name}({module_func_args}){when_guard}do\n'
                             f'    {positional}\n'
                             f'    :evision_nif.{erl_name}({func_args})\n'
                             '  end\n')

                erlang_function.write(f'{module_func_name}({module_func_args_erlang}){when_guard_erlang} ->\n'
                            f'  {positional_erlang},\n'
                            f'  evision_nif:{erl_name}({func_args_erlang}).\n\n'
                )

                if len(module_func_args) > 0:
                    name_arity_elixir = f'{module_func_name}!/{len(module_func_args.split(","))}'
                    name_arity_erlang = f'{module_func_name}!/{len(module_func_args.split(","))}'
                    if len(erlang_function_opt.getvalue()) > 0:
                        name_arity_erlang_opt = f'{module_func_name}!/{len(module_func_args.split(","))+1}'
                else:
                    name_arity_elixir = f'{module_func_name}!/0'
                    name_arity_erlang = f'{module_func_name}!/0'
                    if len(erlang_function_opt.getvalue()) > 0:
                        name_arity_erlang_opt = f'{module_func_name}!/1{module_func_args}'
                if not writer.deferror.get(name_arity_elixir, False):
                    elixir_function.write(f'  deferror {module_func_name}({module_func_args})\n')
                    writer.deferror[name_arity_elixir] = True

            push_to_function_group(name_arity_elixir, guarded_elixir_function_groups, elixir_function)
            push_to_function_group(name_arity_erlang, guarded_erlang_function_groups, erlang_function)
            push_to_function_group(name_arity_erlang_opt, guarded_erlang_function_groups, erlang_function_opt)

        for name_arity in sorted(guarded_elixir_function_groups.keys()):
            for guarded_function in guarded_elixir_function_groups[name_arity]:
                writer.write(guarded_function)

        for name_arity in sorted(guarded_erlang_function_groups.keys()):
            guarded_functions = guarded_erlang_function_groups[name_arity]
            if len(guarded_functions) == 1:
                writer_erlang.write(guarded_functions[0])
            else:
                for guarded_function in guarded_functions[:-1]:
                    writer_erlang.write(guarded_function[:-3] + ';\n')
                writer_erlang.write(guarded_functions[-1])

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
                        else:
                            self.evision_nif_erlang.write(f'{erl_name}() ->\n    not_loaded(?LINE).\n')
                    elif line.startswith("// @evision nif: "):
                        line = line[len("// @evision nif: "):].strip()
                        self.evision_nif.write(f'  {line}\n')

    def gen_namespace(self):
        for ns_name in self.namespaces:
            ns = self.namespaces[ns_name]
            wname = normalize_class_name(ns_name)
            for name, func in sorted(ns.funcs.items()):
                self.gen_erl_declaration(wname, name, func, self.opencv_func, self.opencv_func_erlang, is_ns=True)
                self.code_ns_reg.write(func.get_tab_entry())

        modules_dir = Path(self.output_path) / 'modules'
        for module_text in modules_dir.glob('*.h'):
            self.handle_custom_file(module_text)
        backend_dir = Path(self.output_path) / 'modules' / 'evision_backend'
        for module_text in backend_dir.glob('*.h'):
            self.handle_custom_file(module_text)

        self.code_ns_reg.write('\n};\n\n')

    def gen_enum_reg(self, enum_name):
        name_seg = enum_name.split(".")
        is_enum_class = False
        if len(name_seg) >= 2 and name_seg[-1] == name_seg[-2]:
            enum_name = ".".join(name_seg[:-1])
            is_enum_class = True

        wname = normalize_class_name(enum_name)
        cname = enum_name.replace(".", "::")

        code = ""
        if re.sub(r"^cv\.", "", enum_name) != wname:
            code += "typedef {0} {1};\n".format(cname, wname)
        code += "CV_ERL_FROM_ENUM({0});\nCV_ERL_TO_ENUM({0});\n\n".format(wname)
        self.code_enums.write(code)

    def save(self, path, name, buf):
        with open(path + "/" + name, "wt", encoding='utf-8') as f:
            if name.endswith(".h"):
                f.write("#include <erl_nif.h>\n")
                f.write('#include "nif_utils.hpp"\n')
                f.write('using namespace evision::nif;\n')
            f.write(buf.getvalue())

    def save_json(self, path, name, value):
        import json
        with open(path + "/" + name, "wt", encoding='utf-8') as f:
            json.dump(value, f)

    def make_elixir_module_names(self, module_name=None, separated_ns=None):
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

    def get_module_writer(self, module_name, wname, name, is_ns):
        elixir_module_name = self.make_elixir_module_names(module_name=module_name)
        inner_ns = []
        if wname.startswith('cv::'):
            wname = wname[4:]
            inner_ns = wname.split('::')
            elixir_module_name = self.make_elixir_module_names(separated_ns=inner_ns)
        elixir_module_name = elixir_module_name.replace('_', '')
        opencv_module_file_name = elixir_module_name.replace('.', '_')

        if opencv_module_file_name in self.opencv_modules:
            return self.opencv_modules[opencv_module_file_name], inner_ns
        else:
            module_file_writer = StringIO()
            module_file_writer.doc_written = {}
            module_file_writer.write(f'defmodule Evision.{elixir_module_name} do\n')
            module_file_writer.write('  import Kernel, except: [apply: 2, apply: 3]\n')
            module_file_writer.write('  import Evision.Errorize\n')
            module_file_writer.deferror = {}

            module_file_writer_erlang = StringIO()
            module_file_writer_erlang.doc_written = {}
            if not opencv_module_file_name.startswith("evision_"):
                module_file_writer_erlang.write(f'-module(evision_{opencv_module_file_name.lower()}).\n-compile(nowarn_export_all).\n-compile([export_all]).\n\n')
            else:
                module_file_writer_erlang.write(f'-module({opencv_module_file_name.lower()}).\n--compile(nowarn_export_all).\ncompile([export_all]).\n\n')
            module_file_writer_erlang.deferror = {}

            self.opencv_modules[opencv_module_file_name] = (module_file_writer, module_file_writer_erlang)
            return self.opencv_modules[opencv_module_file_name], inner_ns

    def gen(self, srcfiles, output_path, erl_output_path, erlang_output_path):
        self.output_path = output_path
        self.clear()
        self.parser = hdr_parser.CppHeaderParser(generate_umat_decls=True, generate_gpumat_decls=True)

        self.evision_nif.write('defmodule :evision_nif do\n{}\n'.format(ET.gen_evision_nif_load_nif))
        self.evision_elixir.write('defmodule Evision do\n')
        self.evision_elixir.write('  use Bitwise\n')
        self.evision_elixir.write('  import Kernel, except: [apply: 2, apply: 3, min: 2, max: 2]\n')
        self.evision_elixir.write('  import Evision.Errorize\n')
        self.evision_elixir.deferror = {}

        self.evision_nif_erlang.write('-module(evision_nif).\n-compile(nowarn_export_all).\n-compile([export_all]).\n\n{}\n'.format(ET.gen_evision_nif_load_nif_erlang))
        self.evision_erlang.write('-module(evision).\n-compile(nowarn_export_all).\n-compile([export_all]).\n\n')

        self.code_ns_reg.write('static ErlNifFunc nif_functions[] = {\n')

        # step 1: scan the headers and build more descriptive maps of classes, consts, functions
        for hdr in srcfiles:
            decls = self.parser.parse(hdr)
            if len(decls) == 0:
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
            self.code_type_publish.write(classinfo.gen_def(self))


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

        # end 'evision.ex'
        self.evision_elixir.write(self.enum_names_io.getvalue())
        self.evision_elixir.write(self.opencv_func.getvalue())
        for fix in evision_elixir_fixes():
            self.evision_elixir.write(fix)
            self.evision_elixir.write("\n")
        self.evision_elixir.write(ET.enabled_modules_code.substitute(
            enabled_modules=",".join([f'\n      "{m}"' for m in self.enabled_modules]))
        )
        self.evision_elixir.write('\nend\n')

        # end 'evision.erl'
        self.evision_erlang.write(self.enum_names_io_erlang.getvalue())
        self.evision_erlang.write(self.opencv_func_erlang.getvalue())
        for fix in evision_erlang_fixes():
            self.evision_erlang.write(fix)
            self.evision_erlang.write("\n")
        self.evision_erlang.write(ET.enabled_modules_code_erlang.substitute(
            enabled_modules=",".join([f'\'{m}\'' for m in self.enabled_modules]))
        )

        # end 'evision_nif.ex'
        self.evision_nif.write('\nend\n')

        # That's it. Now save all the files
        self.save(output_path, "evision_generated_include.h", self.code_include)
        self.save(output_path, "evision_generated_funcs.h", self.code_funcs)
        self.save(output_path, "evision_generated_enums.h", self.code_enums)
        self.save(output_path, "evision_generated_types.h", self.code_type_publish)
        self.save(output_path, "evision_generated_types_content.h", self.code_types)
        self.save(output_path, "evision_generated_modules_content.h", self.code_ns_reg)

        self.save(erl_output_path, "evision_nif.ex", self.evision_nif)
        self.save(erl_output_path, "evision.ex", self.evision_elixir)

        self.save(erlang_output_path, "evision_nif.erl", self.evision_nif_erlang)
        self.save(erlang_output_path, "evision.erl", self.evision_erlang)

        for name in self.opencv_modules:
            (elixir_writer, erlang_writer) = self.opencv_modules[name]
            elixir_writer.write('\nend\n')
            self.save(erl_output_path, f"evision_{name.lower()}.ex", elixir_writer)
            self.save(erlang_output_path, f"evision_{name.lower()}.erl", erlang_writer)



if __name__ == "__main__":
    srcfiles = hdr_parser.opencv_hdr_list
    dstdir = "./c_src"
    erl_dstdir = "./lib"
    erlang_dstdir = "./src"
    if len(sys.argv) > 1:
        dstdir = sys.argv[1]
    if len(sys.argv) > 2:
        erl_dstdir = sys.argv[2]
    if len(sys.argv) > 3:
        erlang_dstdir = sys.argv[3]
    if len(sys.argv) > 4:
        with open(sys.argv[4], 'r') as f:
            srcfiles = [l.strip() for l in f.readlines()]
    # default
    enabled_modules = ['calib3d', 'core', 'features2d', 'flann', 'highgui', 'imgcodecs', 'imgproc', 'ml', 'photo',
                       'stitching', 'ts', 'video', 'videoio', 'dnn']
    if len(sys.argv) > 5:
        enabled_modules = sys.argv[5].split(",")
    generator = BeamWrapperGenerator(enabled_modules)
    generator.gen(srcfiles, dstdir, erl_dstdir, erlang_dstdir)
    # for n in generator.namespaces:
    #     print(f'"{n}": &(&1[:namespace] == :"{n}"),')
