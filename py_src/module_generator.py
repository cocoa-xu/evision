#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
from string import Template
from func_info import FuncInfo
from class_info import ClassInfo
from class_prop import ClassProp
from helper import *
import evision_templates as ET

if sys.version_info[0] >= 3:
    from io import StringIO
else:
    from cStringIO import StringIO

unique_signatures = {}
nif_declared = {}

class ModuleGenerator(object):
    def __init__(self, module_name: str):
        self.module_name = module_name.strip()

        # generated entries will go into the `evision_generated_modules_content.h` file
        self.erl_nif_func_entry = StringIO()

        # group functions by function_name, then function_arity
        # key: kind
        # value: dict
        #   key: function_name
        #   value: dict
        #     key: function_arity
        #     value: list
        #       entry: (number_guards, function_code)
        self.function = {
            "elixir": {},
            "erlang": {}
        }

        # group function inline docs by function_name, then function_arity
        # key: kind
        # value: dict
        #   key: function_name
        #   value: dict
        #     key: function_arity
        #     value: list
        #       entry: (inline_docs, function_head)
        self.inline_docs = {
            "elixir": {},
            "erlang": {}
        }

        self.deferror = {}

        # key: kind
        # value: StringIO()
        #
        # key == 'elixir'
        #   generated content will go into the `evision_{self.module_name}.ex` file
        # key == 'erlang'
        #   generated content will go into the `evision_{self.module_name}.erl` file
        self.module = {}

        # generated entries will go into the corresponding nif file
        # key: kind
        # value: declarations
        #
        # key == 'elixir', file => `evision_nif.ex`
        # key == 'erlang', file => `evision_nif.erl`
        self.nif_declaration = {}

    def get_erl_nif_func_entry(self):
        return self.erl_nif_func_entry.getvalue()

    def get_nif_declaration(self, kind: str):
        if self.nif_declaration.get(kind, None) is None:
            return ''
        return self.nif_declaration[kind].getvalue()

    def get_generated_code(self, kind: str):
        if self.module.get(kind, None) is None:
            return ''
        return self.module[kind].getvalue()

    def write_elixir(self, content: str):
        if self.module.get('elixir', None) is None:
            self.module['elixir'] = StringIO()
        self.module['elixir'].write(content)

    def write_erlang(self, content: str):
        if self.module.get('erlang', None) is None:
            self.module['erlang'] = StringIO()
        self.module['erlang'].write(content)

    def end(self, kind: str):
        if kind == 'elixir' and kind in self.function:
            self.end_elixir()
        elif kind == 'erlang' and kind in self.function:
            self.end_erlang()

    def end_elixir(self):
        for function_name, function in self.function['elixir'].items():
            for arity, functions in function.items():
                docs_mfa = self.inline_docs['elixir'].get(function_name, {}).get(arity, [])
                if len(docs_mfa) > 0:
                    if len(docs_mfa) == 1:
                        self.write_elixir('\n  @doc """\n')
                        self.write_elixir(docs_mfa[0][0])
                        self.write_elixir('\n  """\n')
                    else:
                        self.write_elixir('\n  @doc """\n')
                        for i in range(len(docs_mfa)):
                            self.write_elixir(f'  #### Variant {i+1}:\n')
                            self.write_elixir(docs_mfa[i][0])
                            self.write_elixir('\n')
                        self.write_elixir('\n  """\n')
                    # function group
                    self.write_elixir(docs_mfa[0][1])
                functions = sorted(functions, key=lambda x: -x[0])
                for _, function_code in functions:
                    self.write_elixir(function_code)
        self.write_elixir("end\n")

    def end_erlang(self):
        for function_name, function in self.function['erlang'].items():
            for arity, functions in function.items():
                if len(functions) == 1:
                    self.write_erlang(functions[0][1])
                else:
                    functions = sorted(functions, key=lambda x: -x[0])
                    for _, function_code in functions[:-1]:
                        self.write_erlang(function_code[:-3] + ';\n')
                    self.write_erlang(functions[-1][1])

    def gen_constructor(self, full_qualified_name: str, constructor: str, func: FuncInfo, namespace_list: list):
        self._process_function(full_qualified_name, name=constructor, func=func, is_ns=False, is_constructor=True, namespace_list=namespace_list)

    def gen_method(self, full_qualified_name: str, name: str, func: FuncInfo, namespace_list: list):
        self._process_function(full_qualified_name, name=name, func=func, is_ns=False, is_constructor=False, namespace_list=namespace_list)

    def gen_ns_method(self, full_qualified_name: str, name: str, func: FuncInfo, namespace_list: list):
        self._process_function(full_qualified_name, name=name, func=func, is_ns=True, is_constructor=False, namespace_list=namespace_list)

    def gen_property(self, full_qualified_name: str, class_name: str, property_name: str, property: ClassProp):
        # ======== step 1. generate property getter ========
        func_arity = 1
        generating_type = 'get'
        property_templates = {
            "elixir": {
                "getter_template": ET.elixir_property_getter,
                "nif_args": '_self',
                "nif_template": Template('  def ${nif_name}(${nif_args}), do: :erlang.nif_error("${nif_error_msg}")\n'),
                "nif_error_msg": f"{class_name}::{property_name} {generating_type}ter not loaded"
            },
            "erlang": {
                "getter_template": ET.erlang_property_getter,
                "nif_args": '_self',
                "nif_template": Template('${nif_name}(${nif_args}) ->\n    not_loaded(?LINE).\n'),
            }
        }
        self._gen_property_impl(class_name, property_name, func_arity, generating_type, property_templates)

        # ======== step 2. generate property setter if the property if not readonly ========
        if not property.readonly:
            func_arity = 2
            generating_type = 'set'
            property_templates = {
                "elixir": {
                    "setter_template": ET.elixir_property_setter,
                    "nif_args": '_self, _prop',
                    "nif_template": Template('  def ${nif_name}(${nif_args}), do: :erlang.nif_error("${nif_error_msg}")\n'),
                    "nif_error_msg": f"{class_name}::{property_name} {generating_type}ter not loaded"
                },
                "erlang": {
                    "setter_template": ET.erlang_property_setter,
                    "nif_args": '_self, _prop',
                    "nif_template": Template('${nif_name}(${nif_args}) ->\n    not_loaded(?LINE).\n'),
                }
            }
            self._gen_property_impl(class_name, property_name, func_arity, generating_type, property_templates)

    def _gen_property_impl(self, class_name: str, property_name: str, func_arity: int, generating_type: str, property_templates: dict):
        # function name in the `evision_{self.module_name}.ex` file
        func_name = f"{generating_type}_{property_name}"
        # nif function name in the `evision_nif.ex` file
        nif_name = f"{class_name}_{func_name}"
        # function name in C
        c_func_name = f"evision_{nif_name}"
        # make nif name
        if nif_name[0].isupper():
            nif_name = map_uppercase_to_erlang_name(nif_name)

        # add one entry to the `static ErlNifFunc nif_functions[]` array in C
        self.register_erl_nif_func(nif_name, c_func_name, func_arity)

        # generate code
        property_code = {}
        for kind, template in property_templates.items():
            if kind == 'erlang':
                property_name = map_argname('elixir', property_name)
            property_code[kind] = template[f"{generating_type}ter_template"].substitute(property_name=property_name, nif_name=nif_name)

        if 'elixir' in property_templates and not self.has_deferror(func_name, func_arity):
            if generating_type == 'set':
                property_code['elixir'] += f"  deferror({func_name}(self, prop))\n\n"
            else:
                property_code['elixir'] += f"  deferror({func_name}(self))\n\n"
            self.register_deferror(func_name, func_arity)

        for kind, template in property_templates.items():
            self.register_nif(kind, nif_name, template)
            self.add_function(kind, func_name, func_arity, 0, property_code[kind])

    def _process_function(self, full_qualified_name: str, name: str, func: FuncInfo, is_ns: bool, is_constructor: bool, namespace_list: list):
        # ======== step 1. get wrapper name and ensure function name starts with a lowercase letter ========
        nif_name, func_name = func.get_wrapper_name(True)
        if func_name in special_handling_funcs():
            return
        if len(func_name) > 0 and not ('a' <= func_name[0] <= 'z'):
            func_name = func_name.lower()
            nif_name = nif_name.lower()

        # ======== step 2. initialise templates ========
        function_templates = {
            "elixir": {
                "nif_args": '_opts \\\\ []',
                "nif_template": Template('  def ${nif_name}(${nif_args}), do: :erlang.nif_error("${nif_error_msg}")\n'),
                "nif_error_msg": f"{full_qualified_name}::{name} not loaded"
            },
            "erlang": {
                "nif_args": '_opts',
                "nif_template": Template('${nif_name}(${nif_args}) ->\n    not_loaded(?LINE).\n'),
            }
        }

        if not is_ns and func.classname and not func.is_static and not is_constructor:
            function_templates["elixir"]["nif_args"] = f'_self, {function_templates["elixir"]["nif_args"]}'
            function_templates["erlang"]["nif_args"] = f'_self, {function_templates["erlang"]["nif_args"]}'

        # ======== step 3. register this NIF function ========
        for kind, template in function_templates.items():
            self.register_nif(kind, nif_name, template)
        
        # ======== step 4. generate binding code ========
        # function signatures
        erl_signatures = []

        # ======== step 4.1 get guards for all variants of the current function ========
        # function guards
        # key: kind
        # value: list
        #   entry: list
        #     entry: guard
        func_guards = {}
        for i in range(len(func.variants)):
            func_variant = func.variants[i]
            for kind in function_templates:
                if func_guards.get(kind, None) is None:
                    func_guards[kind] = []
                func_guards[kind].append(func_variant.function_guard(kind))
            erl_signatures.append(func.cname + nif_name + func_variant.function_signature())

        # ======== step 4.2 prepare for generating binding code for each kind ========
        for kind in function_templates:
            # get a sorted list based on the number of guards in descending order
            # i.e., function variant that has the most number of constraints will be the first element in the list
            func_guards_len_desc = list(reversed(argsort([len(g) for g in func_guards[kind]])))

            # start from the variant with the most number of constaints/guards
            for i in func_guards_len_desc:
                # generated binding code will be written to this variable
                function_code = StringIO()

                # gather some basic infomation of current function variant
                func_variant = func.variants[i]
                function_sign = erl_signatures[i]
                func_guard = func_guards[kind][i]
                is_instance_method = is_ns == False and func.classname and not func.is_static and not is_constructor
                func_args, func_args_with_opts = func_variant.func_args(kind, instance_method=is_instance_method)

                # `func_arity` only counts the number of positional args
                # for function with opts args, it should be `func_arity + 1`

                # for instance method, it should be `func_arity + 1`
                func_arity = func_variant.py_noptargs
                if is_instance_method:
                    func_arity += 1

                # generate `namespace` tag for `@doc`
                function_group = ""
                if len(func.namespace) > 0:
                    if func.namespace == "cv":
                        function_group = f"  @doc namespace: :cv\n"
                    else:
                        function_group = f"  @doc namespace: :\"{func.namespace}\"\n"

                # module_func_name will be used to generate function declaration
                #   def {module_func_name}...
                module_func_name = func_name

                if namespace_list is not None and len(namespace_list) > 1:
                    prefix = "_".join(namespace_list[:-1]) + '_'
                    if module_func_name.startswith(prefix):
                        module_func_name = module_func_name[len(prefix):]

                rejected, module_func_name = get_module_func_name(module_func_name, is_ns, full_qualified_name)
                if rejected:
                    return

                if is_ns:
                    # we intentionally ignore some functions
                    rejected, module_func_name = self._reject_ns_func(full_qualified_name, func.name, module_func_name)
                    if rejected:
                        return

                    if module_func_name.startswith(full_qualified_name+'_'):
                        module_func_name = module_func_name[len(full_qualified_name)+1:]
                    if '_' in module_func_name:
                        return
                else:
                    module_func_name = name

                rejected, module_func_name = get_module_func_name(module_func_name, is_ns, full_qualified_name)
                if rejected:
                    return

                if func_name.startswith(evision_nif_prefix() + "dnn") and module_func_name == "forward":
                    function_sign = evision_nif_prefix() + "dnn_forward"
                if func_name.startswith(evision_nif_prefix() + "dnn_dnn_Net") and (module_func_name == "getLayerShapes" or module_func_name == "getLayersShapes"):
                    function_sign = evision_nif_prefix() + "dnn_dnn_Net_" + module_func_name

                global unique_signatures
                usign = ''
                if func.classname and len(func.classname) > 0:
                    usign = kind + func.classname + function_sign
                else:
                    usign = kind + function_sign
                if unique_signatures.get(usign, None) is True:
                    pass
                else:
                    unique_signatures[usign] = True
                    inline_doc = func_variant.inline_docs(kind)
                    func_when_guard = when_guard(kind, func_guard)

                    # module_func_args and module_func_args_with_opts are used in function declaration
                    module_func_args = func_args
                    module_func_args_with_opts = func_args_with_opts

                    positional_args, positional_var = func_variant.positional_args(kind)
                    opts_args = func_variant.opts_args(kind, in_func_body=True)

                    # merge positional args and opts args
                    func_args = positional_var
                    if len(func_args_with_opts) > 0:
                        func_args_with_opts = f'{positional_var}{opts_args}'

                    if is_instance_method:
                        self_arg = {'elixir': 'Evision.Internal.Structurise.from_struct(self)', 'erlang': 'Self'}
                        func_args = f'{self_arg[kind]}, {func_args}'
                        if len(func_args_with_opts) > 0:
                            func_args_with_opts = f'{self_arg[kind]}, {func_args_with_opts}'

                    if func_name.startswith(evision_nif_prefix() + "dnn") and module_func_name == "forward":
                        func_arity = 2
                        if kind == 'elixir':
                            function_code.write(f'  def {module_func_name}(self, opts \\\\ [])\n'
                                f'  def {module_func_name}(self, opts) when is_list(opts) and (opts == [] or is_tuple(hd(opts))) do\n'
                                '    self = Evision.Internal.Structurise.from_struct(self)\n'
                                '    opts = Evision.Internal.Structurise.from_struct(opts)\n'
                                f'    :evision_nif.{nif_name}(self, opts)\n'
                                '    |> Evision.Internal.Structurise.to_struct()\n'
                                '  end\n')
                            if not self.has_deferror(func_name, func_arity):
                                function_code.write(f"  deferror {module_func_name}(self, opts)\n\n")
                                self.register_deferror(func_name, func_arity)
                            self.add_function(kind, module_func_name, func_arity, guards_count=3, generated_code=function_code.getvalue())
                            self.add_function_docs(kind, module_func_name, 2, inline_doc, function_group)
                        elif kind == 'erlang':
                            function_code.write(f'{module_func_name}(Self) ->\n'
                                f'  evision_nif:{nif_name}(Self, []).\n')
                            self.add_function(kind, module_func_name, 1, guards_count=0, generated_code=function_code.getvalue())
                            function_code = StringIO()
                            function_code.write(f'{module_func_name}(Self, Options) when is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2 ->\n'
                                f'  evision_nif:{nif_name}(Self, Options).\n')
                            self.add_function(kind, module_func_name, 2, guards_count=3, generated_code=function_code.getvalue())
                        continue

                    if func_name.startswith(evision_nif_prefix() + "dnn_dnn_Net") and (module_func_name == "getLayerShapes" or module_func_name == "getLayersShapes"):
                        func_arity = 2
                        if kind == 'elixir':
                            function_code.write(f'  def {module_func_name}(self, opts \\\\ []) when is_list(opts) and (opts == [] or is_tuple(hd(opts))) do\n'
                                '    self = Evision.Internal.Structurise.from_struct(self)\n'
                                '    opts = Evision.Internal.Structurise.from_struct(opts)\n'
                                f'    :evision_nif.{nif_name}(self, opts)\n'
                                '    |> Evision.Internal.Structurise.to_struct()\n'
                                '  end\n')
                            if not self.has_deferror(func_name, func_arity):
                                function_code.write(f"  deferror {module_func_name}(self, opts)\n\n")
                                self.register_deferror(func_name, func_arity)
                            self.add_function(kind, module_func_name, func_arity, guards_count=3, generated_code=function_code.getvalue())
                            self.add_function_docs(kind, module_func_name, 2, inline_doc, function_group)
                        elif kind == 'erlang':
                            function_code.write(f'{module_func_name}(Self) ->\n'
                                f'  evision_nif:{nif_name}(Self, []).\n')
                            self.add_function(kind, module_func_name, function_arity=1, guards_count=0, generated_code=function_code.getvalue())
                            function_code = StringIO()
                            function_code.write(f'{module_func_name}(Self, Options) when is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2 ->\n'
                                f'  evision_nif:{nif_name}(Self, Options).\n')
                            self.add_function(kind, module_func_name, function_arity=2, guards_count=3, generated_code=function_code.getvalue())
                        continue

                    # if func_args_with_opts is not None
                    # then two functions will be generated
                    # - one without opts
                    # - one with opts
                    if func_args_with_opts:
                        # generate binding code with opts
                        when_guard_with_opts = func_when_guard
                        if len(when_guard_with_opts.strip()) > 0:
                            # when there are some positional args
                            if kind == 'elixir':
                                when_guard_with_opts = f' {when_guard_with_opts.strip()} and is_list(opts) and (opts == [] or is_tuple(hd(opts)))\n  '
                            elif kind == 'erlang':
                                when_guard_with_opts = f' {when_guard_with_opts.strip()}, is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2'
                            else:
                                raise RuntimeError(f"unknown kind `{kind}`")
                        else:
                            # when there is no positional args
                            if kind == 'elixir':
                                when_guard_with_opts = ' when is_list(opts) and (opts == [] or is_tuple(hd(opts)))\n  '
                            elif kind == 'erlang':
                                when_guard_with_opts = ' when is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2'
                            else:
                                raise RuntimeError(f"unknown kind `{kind}`")

                        # generate binding code with opts args
                        func_arity = len(module_func_args_with_opts.split(','))
                        if kind == 'elixir':
                            # function_code.write(f'  def {module_func_name}({module_func_args_with_opts})\n')
                            function_code.write(f'  def {module_func_name}({module_func_args_with_opts}){when_guard_with_opts}do\n'
                                f'    {positional_args}\n'
                                f'    :evision_nif.{nif_name}({func_args_with_opts})\n'
                                '     |> Evision.Internal.Structurise.to_struct()\n'
                                '  end\n')

                            # remove the default value, i.e., `..., opts \\ []` => `..., opts`
                            module_func_args_with_opts = module_func_args_with_opts.replace('\\\\ []', '').strip()
                            # generate the bang(!) version for elixir
                            if not self.has_deferror(func_name, func_arity):
                                function_code.write(f"  deferror({module_func_name}({module_func_args_with_opts}))\n\n")
                                self.register_deferror(func_name, func_arity)
                            self.add_function_docs(kind, module_func_name, func_arity, inline_doc, function_group)
                        elif kind == 'erlang':
                            function_code.write(f'{module_func_name}({module_func_args_with_opts}){when_guard_with_opts} ->\n'
                                f'  {positional_args},\n'
                                f'  evision_nif:{nif_name}({func_args_with_opts}).\n\n'
                            )
                        else:
                            raise RuntimeError(f"unknown kind `{kind}`")

                        # add this function to the collection
                        self.add_function(kind, module_func_name, func_arity, guards_count=3 + len(func_guard), generated_code=function_code.getvalue())
                        # "empty" buffer in function_code
                        # so that we can use it later
                        function_code = StringIO()
                    # === end of `if func_args_with_opt` ===

                    # generate binding code with opts
                    func_arity = len(module_func_args.split(','))
                    if func_arity == 1 and module_func_args == '':
                        func_arity = 0

                    if kind == 'elixir':
                        function_code.write(f'  def {module_func_name}({module_func_args}){func_when_guard}do\n'
                            f'    {positional_args}\n'
                            f'    :evision_nif.{nif_name}({func_args})\n'
                            '    |> Evision.Internal.Structurise.to_struct()\n'
                            '  end\n')
                        # generate the bang(!) version for elixir
                        if not self.has_deferror(func_name, func_arity):
                            # remove the default value, i.e., `..., opts \\ []` => `..., opts`
                            function_code.write(f"  deferror({module_func_name}({module_func_args}))\n\n")
                            self.register_deferror(func_name, func_arity)
                        self.add_function_docs(kind, module_func_name, func_arity, inline_doc, function_group)
                    elif kind == 'erlang':
                        function_code.write(f'{module_func_name}({module_func_args}){func_when_guard} ->\n'
                            f'  {positional_args},\n'
                            f'  evision_nif:{nif_name}({func_args}).\n\n'
                        )
                    else:
                        raise RuntimeError(f"unknown kind `{kind}`")
                    # add this function to the collection
                    self.add_function(kind, module_func_name, func_arity, guards_count=len(func_guard), generated_code=function_code.getvalue())

    def _reject_ns_func(self, full_qualified_name: str, class_name: str, func_name: str):
        if func_name != f'{evision_nif_prefix()}{class_name}':
            func_name = func_name[len(evision_nif_prefix()):]
        else:
            func_name = class_name

        if full_qualified_name != 'cv':
            namespace_func = full_qualified_name + '_' + class_name
            if namespace_func != func_name:
                return True, func_name
        else:
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
            if func_name in ignore_names:
                return True, func_name

        if full_qualified_name == 'cv' and class_name and len(class_name) > 0 and not ('a' <= class_name[0] <= 'z'):
            if func_name.startswith(class_name + '_') or func_name.endswith('_create'):
                return True, func_name
            elif func_name.startswith('CascadeClassifier') \
                    or func_name.startswith('HOGDescriptor') \
                    or func_name.startswith('KeyPoint') \
                    or func_name.startswith('UMat_') \
                    or func_name.startswith('VideoWriter_'):
                return True, func_name

        return False, func_name

    def has_deferror(self, func_name: str, func_arity: int):
        return self.deferror.get(f"{func_name}!/{func_arity}", False)

    def register_deferror(self, func_name: str, func_arity: int):
        self.deferror[f"{func_name}!/{func_arity}"] = True

    def register_erl_nif_func(self, nif_name: str, c_func_name: str, func_arity: int, bound: str = None):
        function_bound_macro = 'F'
        if bound == 'cpu':
            function_bound_macro = 'F_CPU'
        elif bound == 'io':
            function_bound_macro = 'F_IO'
        elif bound is not None:
            print(f"warning: unknown nif function bound type `{bound}` for C function {c_func_name}.")

        # generated entries will go into the `evision_generated_modules_content.h` file
        self.erl_nif_func_entry.write(f'    {function_bound_macro}({nif_name}, {c_func_name}, {func_arity}),\n')

    def register_nif(self, kind: str, nif_name: str, template: dict):
        global nif_declared
        if self.nif_declaration.get(kind, None) is None:
            self.nif_declaration[kind] = StringIO()

        if nif_declared.get(kind, None) is None:
            nif_declared[kind] = {}
        
        if nif_declared[kind].get(nif_name, None) is None:
            nif_declaration = template["nif_template"].substitute(nif_name=nif_name, **template)
            self.nif_declaration[kind].write(nif_declaration)
            nif_declared[kind][nif_name] = True

    def add_function(self, kind: str, function_name: str, function_arity: int, guards_count: int, generated_code: str):
        if self.function.get(kind, None) is None:
            self.function[kind] = {}
        function_dict = self.function[kind]
        if function_name not in function_dict:
            function_dict[function_name] = {}
        if function_arity not in function_dict[function_name]:
            function_dict[function_name][function_arity] = []
        function_dict[function_name][function_arity].append((guards_count, generated_code))

    def add_function_docs(self, kind: str, function_name: str, function_arity: int, inline_docs: str, function_group: str):
        if self.inline_docs.get(kind, None) is None:
            self.inline_docs[kind] = {}
        inline_docs_dict = self.inline_docs[kind]
        if function_name not in inline_docs_dict:
            inline_docs_dict[function_name] = {}
        if function_arity not in inline_docs_dict[function_name]:
            inline_docs_dict[function_name][function_arity] = []
        inline_docs_dict[function_name][function_arity].append((inline_docs, function_group))

    def print_functions(self, kind: str):
        for function_name, function in self.function[kind].items():
            for function_arity, _function_code in function.items():
                print(f"M={self.module_name}, F={function_name}, A={function_arity}")
