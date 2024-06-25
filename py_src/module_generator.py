#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
from string import Template
from func_info import FuncInfo
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
            "erlang": {},
            "gleam": {}
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
            "erlang": {},
            "gleam": {}
        }

        # key: kind
        # value: StringIO()
        #
        # key == 'elixir'
        #   generated content will go into the `evision_{self.module_name}.ex` file
        # key == 'erlang'
        #   generated content will go into the `evision_{self.module_name}.erl` file
        # key == 'gleam'
        #   generated content will go into the `{self.module_name}.erl` file
        self.module = {}

        # generated entries will go into the corresponding nif file
        # key: kind
        # value: declarations
        #
        # key == 'elixir', file => `evision_nif.ex`
        # key == 'erlang', file => `evision_nif.erl`
        # key == 'gleam', file => `evision_nif.erl`
        self.nif_declaration = {}
        
        self.erlang_ended = False
        self.gleam_ended = False

    def get_erl_nif_func_entry(self):
        return self.erl_nif_func_entry.getvalue()

    def get_nif_declaration(self, kind: str):
        if self.nif_declaration.get(kind, None) is None:
            return ''
        return self.nif_declaration[kind].getvalue()

    def get_generated_code(self, kind: str):
        if kind == 'gleam':
            return self.module[kind][0].getvalue()
        elif kind == 'gleam_file':
            return self.module['gleam'][1].getvalue()
        if self.module.get(kind, None) is None:
            return ''
        else:
            return self.module[kind].getvalue()

    def write_elixir(self, content: str):
        if self.module.get('elixir', None) is None:
            self.module['elixir'] = StringIO()
        self.module['elixir'].write(content)

    def write_erlang(self, content: str):
        if self.module.get('erlang', None) is None:
            self.module['erlang'] = StringIO()
        self.module['erlang'].write(content)
    
    def write_gleam(self, content: str):
        if self.module.get('gleam', None) is None:
            self.module['gleam'] = []
            self.module['gleam'].append(StringIO())
            self.module['gleam'].append(StringIO())
        self.module['gleam'][0].write(content)
        
    def write_gleam_file(self, content: str):
        if self.module.get('gleam', None) is None:
            self.module['gleam'] = []
            self.module['gleam'].append(StringIO())
            self.module['gleam'].append(StringIO())
        if content != None:
            self.module['gleam'][1].write(content)

    def end(self, kind: str):
        if kind == 'elixir' and kind in self.function:
            self.end_elixir()
        elif kind == 'erlang' and kind in self.function:
            self.end_erlang()
        elif kind == 'gleam' and kind in self.function:
            self.end_gleam()

    def end_elixir(self):
        for function_name, function in self.function['elixir'].items():
            for arity, functions in function.items():
                docs_mfa = self.inline_docs['elixir'].get(function_name, {}).get(arity, [])
                if len(docs_mfa) > 0:
                    if len(docs_mfa) == 1:
                        self.write_elixir('\n  @doc """\n')
                        self.write_elixir(docs_mfa[0])
                        self.write_elixir('\n  """\n')
                    else:
                        self.write_elixir('\n  @doc """\n')
                        for i in range(len(docs_mfa)):
                            self.write_elixir(f'  #### Variant {i+1}:\n')
                            self.write_elixir(docs_mfa[i])
                            self.write_elixir('\n')
                        self.write_elixir('\n  """\n')
                functions = sorted(functions, key=lambda x: -x[0])
                for _, function_code in functions:
                    self.write_elixir(function_code)
        self.write_elixir("end\n")

    def end_erlang(self):
        if self.erlang_ended:
            return
        for function_name, function in self.function['erlang'].items():
            for arity, functions in function.items():
                docs_spec_mfa = self.inline_docs['erlang'].get(function_name, {}).get(arity, [])
                if len(docs_spec_mfa) > 0:
                    if len(docs_spec_mfa) == 1:
                        _docs, spec = docs_spec_mfa[0]
                        self.write_erlang(spec)
                        self.write_erlang("\n")
                    else:
                        _docs, spec = docs_spec_mfa[0]
                        prefix = spec[:-1].index("(")
                        self.write_erlang(spec[:-1])
                        self.write_erlang("\n")
                        space = ' ' * (prefix - 1)
                        for i in range(1, len(docs_spec_mfa) - 1):
                            _docs, spec = docs_spec_mfa[i]
                            self.write_erlang(space)
                            self.write_erlang(';')
                            self.write_erlang(spec[prefix:-1])
                            self.write_erlang("\n")
                        _docs, spec = docs_spec_mfa[-1]
                        self.write_erlang(space)
                        self.write_erlang(';')
                        self.write_erlang(spec[prefix:])
                        self.write_erlang("\n")

                if len(functions) == 1:
                    self.write_erlang(functions[0][1])
                else:
                    functions = sorted(functions, key=lambda x: -x[0])
                    for _, function_code in functions[:-1]:
                        self.write_erlang(function_code[:-3] + ';\n')
                    self.write_erlang(functions[-1][1])
        self.erlang_ended = True

    def end_gleam(self):
        if self.gleam_ended:
            return
        for function_name, function in self.function['gleam'].items():
            for arity, functions in function.items():
                has_gleam = False
                docs_spec_mfa = self.inline_docs['gleam'].get(function_name, {}).get(arity, [])
                if len(docs_spec_mfa) > 0:
                    if len(docs_spec_mfa) == 1:
                        _docs, spec = docs_spec_mfa[0]
                        self.write_gleam(spec)
                        self.write_gleam("\n")
                    else:
                        _docs, spec = docs_spec_mfa[0]
                        prefix = spec[:-1].index("(")
                        self.write_gleam(spec[:-1])
                        self.write_gleam("\n")
                        space = ' ' * (prefix - 1)
                        for i in range(1, len(docs_spec_mfa) - 1):
                            _docs, spec = docs_spec_mfa[i]
                            self.write_gleam(space)
                            self.write_gleam(';')
                            self.write_gleam(spec[prefix:-1])
                            self.write_gleam("\n")
                        _docs, spec = docs_spec_mfa[-1]
                        self.write_gleam(space)
                        self.write_gleam(';')
                        self.write_gleam(spec[prefix:])
                        self.write_gleam("\n")

                if len(functions) == 1:
                    self.write_gleam(functions[0][1])
                    if not has_gleam:
                        self.write_gleam_file(functions[0][2])
                        # has_gleam = True
                else:
                    functions = sorted(functions, key=lambda x: -x[0])
                    for _, function_code, typed_function in functions[:-1]:
                        self.write_gleam(function_code[:-3] + ';\n')
                        # self.write_gleam_file(typed_function)
                    self.write_gleam(functions[-1][1])
                    if not has_gleam:
                        self.write_gleam_file(functions[-1][2])
                        # has_gleam = True
        self.gleam_ended = True

    def gen_constructor(self, full_qualified_name: str, constructor: str, func: FuncInfo, namespace_list: list):
        self._process_function(full_qualified_name, name=constructor, func=func, is_ns=False, is_constructor=True, namespace_list=namespace_list)

    def gen_method(self, full_qualified_name: str, name: str, func: FuncInfo, namespace_list: list):
        self._process_function(full_qualified_name, name=name, func=func, is_ns=False, is_constructor=False, namespace_list=namespace_list)

    def gen_ns_method(self, full_qualified_name: str, name: str, func: FuncInfo, namespace_list: list):
        self._process_function(full_qualified_name, name=name, func=func, is_ns=True, is_constructor=False, namespace_list=namespace_list)

    def gen_property(self, full_qualified_name: str, class_name: str, property_name: str, property: ClassProp):
        # ======== step 1. generate property getter ========
        func_arity = 1
        ns = full_qualified_name.split('::')
        cname = ns[-1]
        if cname == 'Params':
            cname = '_'.join(ns[-2:])
        self_spec_in = {
            "elixir": map_argtype_in_spec('elixir', class_name, cname, is_in=True, decl=[]),
            "erlang": map_argtype_in_spec('erlang', class_name, cname, is_in=True, decl=[]),
            "gleam": map_argtype_in_spec('gleam', class_name, cname, is_in=True, decl=[])
        }
        prop_spec_out = {
            "elixir": map_argtype_in_spec('elixir', class_name, property.tp, is_in=False, decl=[]),
            "erlang": map_argtype_in_spec('erlang', class_name, property.tp, is_in=False, decl=[]),
            "gleam": map_argtype_in_spec('gleam', class_name, property.tp, is_in=False, decl=[]),
        }
        generating_type = 'get'
        property_templates = {
            "elixir": {
                "getter_template": ET.elixir_property_getter,
                "nif_args": '_self',
                "nif_template": Template('  def ${nif_name}(${nif_args}), do: :erlang.nif_error(:undefined)\n'),
                "self_spec": self_spec_in["elixir"],
                'prop_spec': prop_spec_out["elixir"]
            },
            "erlang": {
                "getter_template": ET.erlang_property_getter,
                "nif_args": '_self',
                "nif_template": Template('${nif_name}(${nif_args}) ->\n    not_loaded(?LINE).\n'),
                "self_spec": self_spec_in["erlang"],
                'prop_spec': prop_spec_out["erlang"]
            },
            "gleam": {
                "getter_template": ET.erlang_property_getter,
                "nif_args": '_self',
                "nif_template": Template('${nif_name}(${nif_args}) ->\n    not_loaded(?LINE).\n'),
                "self_spec": self_spec_in["gleam"],
                'prop_spec': prop_spec_out["gleam"],
                'typed_function': Template('@external(erlang, "${erlang_module}", "${nif_name}")\npub fn ${typed_function_name}(self: self) -> prop\n\n')
            }
        }
        self._gen_property_impl(class_name, property_name, func_arity, generating_type, property_templates)

        # ======== step 2. generate property setter if the property if not readonly ========
        if not property.readonly:
            func_arity = 2
            self_spec_out = {
                "elixir": map_argtype_in_spec('elixir', class_name, cname, is_in=False, decl=[]),
                "erlang": map_argtype_in_spec('erlang', class_name, cname, is_in=False, decl=[]),
                "gleam": map_argtype_in_spec('gleam', class_name, cname, is_in=False, decl=[]),
            }
            prop_spec_in = {
                "elixir": map_argtype_in_spec('elixir', class_name, property.tp, is_in=True, decl=[]),
                "erlang": map_argtype_in_spec('erlang', class_name, property.tp, is_in=True, decl=[]),
                "gleam": map_argtype_in_spec('gleam', class_name, property.tp, is_in=True, decl=[])
            }
            generating_type = 'set'
            property_templates = {
                "elixir": {
                    "setter_template": ET.elixir_property_setter,
                    "nif_args": '_self, _prop',
                    "nif_template": Template('  def ${nif_name}(${nif_args}), do: :erlang.nif_error(:undefined)\n'),
                    "self_spec_in": self_spec_in["elixir"],
                    "self_spec_out": self_spec_out["elixir"],
                    'prop_spec': prop_spec_in["elixir"]
                },
                "erlang": {
                    "setter_template": ET.erlang_property_setter,
                    "nif_args": '_self, _prop',
                    "nif_template": Template('${nif_name}(${nif_args}) ->\n    not_loaded(?LINE).\n'),
                    "self_spec_in": self_spec_in["erlang"],
                    "self_spec_out": self_spec_out["erlang"],
                    'prop_spec': prop_spec_in["erlang"]
                },
                "gleam": {
                    "setter_template": ET.erlang_property_setter,
                    "nif_args": '_self, _prop',
                    "nif_template": Template('${nif_name}(${nif_args}) ->\n    not_loaded(?LINE).\n'),
                    "self_spec_in": self_spec_in["gleam"],
                    "self_spec_out": self_spec_out["gleam"],
                    'prop_spec': prop_spec_in["gleam"],
                    'typed_function': Template('@external(erlang, "${erlang_module}", "${nif_name}")\npub fn ${typed_function_name}(self: self, prop: prop) -> any\n\n')
                },
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
            if kind == 'erlang' or kind == 'gleam':
                property_name = map_argname('elixir', property_name)
            property_code[kind] = template[f"{generating_type}ter_template"].substitute(property_name=property_name, nif_name=nif_name, **template)

        for kind, template in property_templates.items():
            self.register_nif(kind, nif_name, template)
            if kind == 'gleam':
                typed_function_tmpl = template['typed_function']
                typed_function_module_name = self.module_name.replace('.', '_').lower()
                typed_function_name = self.to_gleam_func_name(nif_name)
                typed_function = typed_function_tmpl.substitute(erlang_module=typed_function_module_name, nif_name=nif_name, typed_function_name=typed_function_name)
                self.add_function(kind, func_name, func_arity, 0, property_code[kind], typed_function=typed_function)
            else:
                self.add_function(kind, func_name, func_arity, 0, property_code[kind])

    def _process_function(self, full_qualified_name: str, name: str, func: FuncInfo, is_ns: bool, is_constructor: bool, namespace_list: list):
        # ======== step 1. get wrapper name and ensure function name starts with a lowercase letter ========
        nif_name, func_name = func.get_wrapper_name(True)
        if func_name in special_handling_funcs() or func_name in special_handling_funcs_only_in_beam():
            return
        if 'waitany' in func_name.lower():
            print(func_name, "1")
        if len(func_name) > 0 and not ('a' <= func_name[0] <= 'z'):
            func_name = func_name.lower()
            nif_name = nif_name.lower()

        # ======== step 2. initialise templates ========
        function_templates = {
            "elixir": {
                "nif_args": '_opts \\\\ []',
                "nif_template": Template('  def ${nif_name}(${nif_args}), do: :erlang.nif_error(:undefined)\n'),
            },
            "erlang": {
                "nif_args": '_opts',
                "nif_template": Template('${nif_name}(${nif_args}) ->\n    not_loaded(?LINE).\n'),
            },
            "gleam": {
                "nif_args": '_opts',
                "nif_template": Template('${nif_name}(${nif_args}) ->\n    not_loaded(?LINE).\n'),
            }
        }

        if not is_ns and func.classname and not func.is_static and not is_constructor:
            function_templates["elixir"]["nif_args"] = f'_self, {function_templates["elixir"]["nif_args"]}'
            function_templates["erlang"]["nif_args"] = f'_self, {function_templates["erlang"]["nif_args"]}'
            function_templates["gleam"]["nif_args"] = f'_self, {function_templates["gleam"]["nif_args"]}'

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
            if 'detectTextRectangles' in func.name:
                # prefer returning both detections and confidences
                # len(py_outlist) == 2: [('detections', 1), ('confidences', 2)] 
                # len(py_outlist) == 1: [('detections', 1)]
                if len(func_variant.py_outlist) == 1:
                    continue

            for kind in function_templates:
                if func_guards.get(kind, None) is None:
                    func_guards[kind] = []
                func_guards[kind].append(func_variant.function_guard(kind))
            sign_added = False
            if "readNet" in func_name:
                if len(func_variant.py_arglist) > 0:
                    argname, _, argtype = func_variant.py_arglist[0]
                    if 'buffer' in argname and argtype == 'vector_uchar':
                        erl_signatures.append(func.cname + 'buffer' + nif_name + func_variant.function_signature())
                        sign_added = True
            if not sign_added:
                erl_signatures.append(func.cname + nif_name + func_variant.function_signature())

        # ======== step 4.2 prepare for generating binding code for each kind ========
        for kind in function_templates:
            # get a sorted list based on the number of guards in descending order
            # i.e., function variant that has the most number of constraints will be the first element in the list
            func_guards_len_desc = list(reversed(argsort([len(g) for g in func_guards[kind]])))
            more_than_one_variant = len(func_guards_len_desc) > 1
            named_args_generated = False
            # start from the variant with the most number of constraints/guards
            for i in func_guards_len_desc:
                # generated binding code will be written to this variable
                function_code = StringIO()

                # gather some basic information of current function variant
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

                if '_' in module_func_name and len(namespace_list) > 1:
                    prefix = '_'.join([map_argname('elixir', ns) for ns in namespace_list[:-1]])
                    prefix += '_'
                    if module_func_name.startswith(prefix):
                        module_func_name = module_func_name[len(prefix):]

                if len(module_func_name) > 0 and not ('a' <= module_func_name[0] <= 'z'):
                    module_func_name = module_func_name[:1].lower() + module_func_name[1:]
                    if module_func_name == 'textDetectionModel_DB':
                        module_func_name = 'textDetectionModelDB'
                    elif module_func_name == 'textDetectionModel_EAST':
                        module_func_name = 'textDetectionModelEAST'

                if func_name.startswith(evision_nif_prefix() + "dnn") and module_func_name == "forward":
                    function_sign = evision_nif_prefix() + "dnn_forward"
                if func_name.startswith(evision_nif_prefix() + "dnn_dnn_Net") and (module_func_name == "getLayerShapes" or module_func_name == "getLayersShapes"):
                    function_sign = evision_nif_prefix() + "dnn_dnn_Net_" + module_func_name

                if "readNet" in func_name:
                    if len(func_variant.py_arglist) > 0:
                        argname, _, argtype = func_variant.py_arglist[0]
                        if 'buffer' in argname and argtype == 'vector_uchar':
                            module_func_name += 'Buffer'

                if "qrcodeencoder_params" == module_func_name:
                    module_func_name = 'params'

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
                    inline_doc = func_variant.inline_docs(kind, is_instance_method, self.module_name)
                    func_when_guard = when_guard(kind, func_guard)

                    # module_func_args and module_func_args_with_opts are used in function declaration
                    module_func_args = func_args
                    module_func_args_with_opts = func_args_with_opts

                    if kind == 'gleam':
                        positional_args, positional_var, positional_arg_types = func_variant.positional_args(kind)
                    else:
                        positional_args, positional_var = func_variant.positional_args(kind)
                    opts_args = func_variant.opts_args(kind, in_func_body=True)
                    function_spec = func_variant.generate_spec(kind, module_func_name, is_instance_method, include_opts=False, module_name=self.module_name)
                    function_spec_opts = func_variant.generate_spec(kind, module_func_name, is_instance_method, include_opts=True, module_name=self.module_name)

                    # merge positional args and opts args
                    func_args = positional_var
                    if len(func_args_with_opts) > 0:
                        func_args_with_opts = f'{positional_var}{opts_args}'

                    if is_instance_method:
                        self_arg = {
                            'elixir': 'Evision.Internal.Structurise.from_struct(self)', 
                            'erlang': 'evision_internal_structurise:from_struct(Self)',
                            'gleam': 'evision_internal_structurise:from_struct(Self)'
                        }
                        func_args = f'{self_arg[kind]}, {func_args}'
                        if len(func_args_with_opts) > 0:
                            func_args_with_opts = f'{self_arg[kind]}, {func_args_with_opts}'

                    if func_name.startswith(evision_nif_prefix() + "dnn") and module_func_name == "forward":
                        func_arity = 2
                        if kind == 'elixir':
                            function_code.write(f'  @spec forward(Evision.Net.t(), [{{atom(), term()}},...] | nil) :: list(Evision.Mat.t()) | Evision.Mat.t() | {{:error, String.t()}}\n'
                                f'  def {module_func_name}(self, opts \\\\ nil)\n'
                                f'  def {module_func_name}(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts))) do\n'
                                '    self = Evision.Internal.Structurise.from_struct(self)\n'
                                '    opts = Evision.Internal.Structurise.from_struct(opts || [])\n'
                                f'    :evision_nif.{nif_name}(self, opts)\n'
                                '    |> to_struct()\n'
                                '  end\n')
                            self.add_function(kind, module_func_name, func_arity,
                                guards_count=3, 
                                generated_code=function_code.getvalue()
                            )
                            self.add_function_docs(kind, module_func_name, 
                                function_arity=2,
                                inline_docs=inline_doc
                            )
                        elif kind == 'erlang':
                            function_code.write(f'{module_func_name}(Self) ->\n'
                                f'  evision_nif:{nif_name}(evision_internal_structurise:from_struct(Self), []).\n')
                            self.add_function(kind, module_func_name, 1, guards_count=0, generated_code=function_code.getvalue())
                            function_code = StringIO()
                            function_code.write(f'{module_func_name}(Self, Options) when is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2 ->\n'
                                f'  Ret = evision_nif:{nif_name}(evision_internal_structurise:from_struct(Self), evision_internal_structurise:from_struct(Options)),\n'
                                "  to_struct(Ret).\n\n")
                            self.add_function(kind, module_func_name, 2, guards_count=3, generated_code=function_code.getvalue())
                        elif kind == 'gleam':
                            function_code.write(f'{module_func_name}(Self) ->\n'
                                f'  evision_nif:{nif_name}(evision_internal_structurise:from_struct(Self), []).\n')
                            typed_function = '@external(erlang, "evision_net", "forward")\npub fn forward1(self: any) -> any \n\n'
                            self.add_function(kind, module_func_name, 1, guards_count=0, generated_code=function_code.getvalue(), typed_function=typed_function)
                            function_code = StringIO()
                            function_code.write(f'{module_func_name}(Self, Options) when is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2 ->\n'
                                f'  Ret = evision_nif:{nif_name}(evision_internal_structurise:from_struct(Self), evision_internal_structurise:from_struct(Options)),\n'
                                "  to_struct(Ret).\n\n")
                            typed_function = '@external(erlang, "evision_net", "forward")\npub fn forward2(self: any, opts: any) -> any \n\n'
                            self.add_function(kind, module_func_name, 2, guards_count=3, generated_code=function_code.getvalue(), typed_function=typed_function)
                        continue

                    if func_name.startswith(evision_nif_prefix() + "dnn_dnn_Net") and (module_func_name == "getLayerShapes" or module_func_name == "getLayersShapes"):
                        func_arity = 2
                        if kind == 'elixir':
                            specs = {
                                'getLayerShapes': {
                                    'elixir': '@spec getLayerShapes(Evision.Net.t(), [{{atom(), term()}},...] | nil) :: {list(list(integer())), list(list(integer()))} | {:error, String.t()}',
                                    'erlang': '-spec getLayerShapes(#evision_mat{}, [{{atom(), term()}},...] | nil) -> {list(list(integer())), list(list(integer()))} | {error, binary()}.',
                                    'gleam': '-spec getLayerShapes(#evision_mat{}, [{{atom(), term()}},...] | nil) -> {list(list(integer())), list(list(integer()))} | {error, binary()}.'
                                },
                                'getLayersShapes': {
                                    'elixir': '@spec getLayerShapes(Evision.Net.t(), [{{atom(), term()}},...] | nil) :: {list(integer()), list(list(list(integer()))), list(list(list(integer())))} | {:error, String.t()}',
                                    'erlang': '-spec getLayerShapes(#evision_mat{}, [{{atom(), term()}},...] | nil) ->  {list(integer()), list(list(list(integer()))), list(list(list(integer())))} | {error, binary()}.',
                                    'gleam': '-spec getLayerShapes(#evision_mat{}, [{{atom(), term()}},...] | nil) ->  {list(integer()), list(list(list(integer()))), list(list(list(integer())))} | {error, binary()}.'
                                }
                            }
                            spec = specs[module_func_name][kind]
                            function_code.write(f'  {spec}\n'
                                f'  def {module_func_name}(self, opts \\\\ nil) when opts == nil or (is_list(opts) and is_tuple(hd(opts))) do\n'
                                '    self = Evision.Internal.Structurise.from_struct(self)\n'
                                '    opts = Evision.Internal.Structurise.from_struct(opts || [])\n'
                                f'   :evision_nif.{nif_name}(self, opts)\n'
                                '    |> to_struct()\n'
                                '  end\n')
                            self.add_function(kind, module_func_name, func_arity,
                                guards_count=3,
                                generated_code=function_code.getvalue()
                            )
                            self.add_function_docs(kind, module_func_name, 
                                function_arity=2,
                                inline_docs=inline_doc,
                                typespec=spec
                            )
                        elif kind == 'erlang':
                            function_code.write(f'{module_func_name}(Self) ->\n'
                                f'  Ret = evision_nif:{nif_name}(evision_internal_structurise:from_struct(Self), []),\n'
                                "  to_struct(Ret).\n\n"
                            )
                            self.add_function(kind, module_func_name, 
                                function_arity=1, 
                                guards_count=0,
                                generated_code=function_code.getvalue()
                            )
                            function_code = StringIO()
                            function_code.write(f'{module_func_name}(Self, Options) when is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2 ->\n'
                                f'  Ret = evision_nif:{nif_name}(evision_internal_structurise:from_struct(Self), evision_internal_structurise:from_struct(Options)),\n'
                                "  to_struct(Ret).\n\n"
                            )
                            self.add_function(kind, module_func_name, 
                                function_arity=2,
                                guards_count=3,
                                generated_code=function_code.getvalue()
                            )
                        elif kind == 'gleam':
                            function_code.write(f'{module_func_name}(Self) ->\n'
                                f'  Ret = evision_nif:{nif_name}(evision_internal_structurise:from_struct(Self), []),\n'
                                "  to_struct(Ret).\n\n"
                            )
                            typed_function_name = module_func_name.lower()
                            typed_function = f'@external(erlang, "evision_net", "getLayerShapes")\npub fn {typed_function_name}1(self: any) -> any \n\n'
                            self.add_function(kind, module_func_name, 
                                function_arity=1,
                                guards_count=3,
                                generated_code=function_code.getvalue(),
                                typed_function=typed_function
                            )
                            function_code = StringIO()
                            function_code.write(f'{module_func_name}(Self, Options) when is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2 ->\n'
                                f'  Ret = evision_nif:{nif_name}(evision_internal_structurise:from_struct(Self), evision_internal_structurise:from_struct(Options)),\n'
                                "  to_struct(Ret).\n\n"
                            )
                            typed_function = f'@external(erlang, "evision_net", "getLayersShapes")\npub fn {typed_function_name}2(self: any, opts: any) -> any \n\n'
                            self.add_function(kind, module_func_name, 
                                function_arity=2,
                                guards_count=3,
                                generated_code=function_code.getvalue(),
                                typed_function=typed_function
                            )
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
                                when_guard_with_opts = f' {when_guard_with_opts.strip()} and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))\n  '
                            elif kind == 'erlang':
                                when_guard_with_opts = f' {when_guard_with_opts.strip()}, is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2'
                            elif kind == 'gleam':
                                when_guard_with_opts = f' {when_guard_with_opts.strip()}, is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2'
                            else:
                                raise RuntimeError(f"unknown kind `{kind}`")
                        else:
                            # when there is no positional args
                            if kind == 'elixir':
                                when_guard_with_opts = ' when opts == nil or (is_list(opts) and is_tuple(hd(opts)))\n  '
                            elif kind == 'erlang':
                                when_guard_with_opts = ' when is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2'
                            elif kind == 'gleam':
                                when_guard_with_opts = ' when is_list(Options), is_tuple(hd(Options)), tuple_size(hd(Options)) == 2'
                            else:
                                raise RuntimeError(f"unknown kind `{kind}`")

                        # generate binding code with opts args
                        func_arity = len(module_func_args_with_opts.split(','))
                        if kind == 'elixir':
                            keyword_arg_names_elixir = func_variant.keyword_arg_names(kind)
                            if len(keyword_arg_names_elixir) > 0:
                                allowed_opts = ', '.join([f':{arg}' for arg in keyword_arg_names_elixir])
                                keyword_validate = f'    Keyword.validate!(opts || [], [{allowed_opts}])\n'
                            else:
                                keyword_validate = ''
                            function_code.write(f'  {function_spec_opts}\n'
                                f'  def {module_func_name}({module_func_args_with_opts}){when_guard_with_opts}do\n{keyword_validate}'
                                f'    {positional_args}\n'
                                f'    :evision_nif.{nif_name}({func_args_with_opts})\n'
                                '     |> to_struct()\n'
                                '  end\n')

                            # remove the default value, i.e., `..., opts \\ []` => `..., opts`
                            module_func_args_with_opts = module_func_args_with_opts.replace('\\\\ []', '').strip()
                            self.add_function_docs(kind, module_func_name, func_arity,
                                inline_docs=inline_doc,
                            )
                        elif kind == 'erlang':
                            function_code.write(
                                f'{module_func_name}({module_func_args_with_opts}){when_guard_with_opts}->\n'
                                f'  {positional_args},\n'
                                f'  Ret = evision_nif:{nif_name}({func_args_with_opts}),\n'
                                "  to_struct(Ret).\n\n"
                            )
                            self.add_function_docs(kind, module_func_name, func_arity,
                                inline_docs=inline_doc,
                                typespec=function_spec_opts
                            )
                        elif kind == 'gleam':
                            function_code.write(
                                f'{module_func_name}({module_func_args_with_opts}){when_guard_with_opts}->\n'
                                f'  {positional_args},\n'
                                f'  Ret = evision_nif:{nif_name}({func_args_with_opts}),\n'
                                "  to_struct(Ret).\n\n"
                            )
                            
                            self.add_function_docs(kind, module_func_name, func_arity,
                                inline_docs=inline_doc,
                                typespec=function_spec_opts,
                            )
                        else:
                            raise RuntimeError(f"unknown kind `{kind}`")

                        # add this function to the collection
                        if self.module_name == 'Evision':
                            typed_function = f'@external(erlang, "evision", "{module_func_name}")\n'
                        else:
                            typed_function_module_name = self.module_name.replace('.', '_').lower()
                            typed_function = f'@external(erlang, "evision_{typed_function_module_name}", "{module_func_name}")\n'
                        if func_arity == 1:
                            if is_instance_method:
                                typed_function += f'pub fn {self.to_gleam_func_name(module_func_name)}1(self: self) -> any\n\n'
                            else:
                                typed_function += f'pub fn {self.to_gleam_func_name(module_func_name)}1(opts: opts) -> any\n\n'
                        else:
                            if is_instance_method:
                                typed_function += f'pub fn {self.to_gleam_func_name(module_func_name)}{func_arity}(self: self, {", ".join([f"{self.to_gleam_arg_name(func_variant.py_arglist[i][0])}: any{i}" for i in range(func_arity - 1)])}) -> any\n\n'
                            else:
                                typed_function += f'pub fn {self.to_gleam_func_name(module_func_name)}{func_arity}({", ".join([f"{self.to_gleam_arg_name(func_variant.py_arglist[i][0])}: any{i}" for i in range(func_arity - 1)])}, opts: opts) -> any\n\n'
                        self.add_function(kind, module_func_name, func_arity,
                            guards_count=3 + len(func_guard),
                            generated_code=function_code.getvalue(),
                            typed_function=typed_function
                        )
                        # "empty" buffer in function_code
                        # so that we can use it later
                        function_code = StringIO()
                    # === end of `if func_args_with_opt` ===

                    # generate binding code with opts
                    func_arity = len(module_func_args.split(','))
                    if func_arity == 1 and module_func_args == '':
                        func_arity = 0

                    if kind == 'elixir':
                        function_code.write(f'  {function_spec}\n'
                            f'  def {module_func_name}({module_func_args}){func_when_guard}do\n'
                            f'    {positional_args}\n'
                            f'    :evision_nif.{nif_name}({func_args})\n'
                            '    |> to_struct()\n'
                            '  end\n')
                        self.add_function_docs(kind, module_func_name, func_arity, inline_doc)
                    elif kind == 'erlang':
                        function_code.write(
                            f'{module_func_name}({module_func_args}){func_when_guard}->\n'
                            f'  {positional_args},\n'
                            f'  Ret = evision_nif:{nif_name}({func_args}),\n'
                            "  to_struct(Ret).\n\n"
                        )
                        self.add_function_docs(kind, module_func_name, func_arity,
                            inline_docs=inline_doc,
                            typespec=function_spec
                        )
                    elif kind == 'gleam':
                        function_code.write(
                            f'{module_func_name}({module_func_args}){func_when_guard}->\n'
                            f'  {positional_args},\n'
                            f'  Ret = evision_nif:{nif_name}({func_args}),\n'
                            "  to_struct(Ret).\n\n"
                        )
                        self.add_function_docs(kind, module_func_name, func_arity,
                            inline_docs=inline_doc,
                            typespec=function_spec
                        )
                    else:
                        raise RuntimeError(f"unknown kind `{kind}`")
                    # add this function to the collection
                    if self.module_name == 'Evision':
                        typed_function = f'@external(erlang, "evision", "{module_func_name}")\n'
                    else:
                        typed_function_module_name = self.module_name.replace('.', '_').lower()
                        typed_function = f'@external(erlang, "evision_{typed_function_module_name}", "{module_func_name}")\n'
                    
                    func_name_with_arity = ''
                    if more_than_one_variant or func_args_with_opts:
                        func_name_with_arity = f'{func_arity}'
                    if is_instance_method:
                        if func_arity == 1:
                            typed_function += f'pub fn {self.to_gleam_func_name(module_func_name)}{func_name_with_arity}(self: self) -> any\n\n'
                        else:
                            typed_function += f'pub fn {self.to_gleam_func_name(module_func_name)}{func_name_with_arity}(self: self, {", ".join([f"{self.to_gleam_arg_name(func_variant.py_arglist[i][0])}: any{i}" for i in range(func_arity - 1)])}) -> any\n\n'
                    else:
                        typed_function += f'pub fn {self.to_gleam_func_name(module_func_name)}{func_name_with_arity}({", ".join([f"{self.to_gleam_arg_name(func_variant.py_arglist[i][0])}: any{i}" for i in range(func_arity)])}) -> any\n\n'
                    
                    if not named_args_generated:
                        # lastly, generate one that uses named args
                        named_args_generated = True
                        named_args_code = StringIO()
                        if kind == 'elixir':
                            all_named_args = func.get_all_named_args()
                            keyword_validate = ''
                            if len(all_named_args) > 0:
                                keyword_validate = ",".join([f':{arg}' for arg in all_named_args])
                                keyword_validate = f'\n    named_args = Keyword.validate!(named_args, [{keyword_validate}])'
                            named_args_code.write(f'  @spec {module_func_name}(Keyword.t()) :: any() | {{:error, String.t()}}\n'
                                f'  def {module_func_name}([{{arg, _}} | _] = named_args) when is_atom(arg) do{keyword_validate}\n'
                                 '    Enum.map(named_args, fn {named_arg, value} -> {named_arg, Evision.Internal.Structurise.from_struct(value)} end)\n'
                                f'    |> :evision_nif.{nif_name}()\n'
                                 '    |> to_struct()\n'
                                 '  end\n')
                            self.add_function(kind, module_func_name, 1,
                                guards_count=99,
                                generated_code=named_args_code.getvalue()
                            )
                        elif kind in ['erlang', 'gleam']:
                            named_args_code.write(
                                f'{module_func_name}([{{ArgName, _}} | _] = NamedArgs) when is_atom(ArgName) ->\n'
                                 '  NamedArgs_ = lists:foldl(fun({Arg, Value}, Acc) -> [{Arg, evision_internal_structurise:from_struct(Value)} | Acc] end, [], NamedArgs),\n'
                                f'  Ret = evision_nif:{nif_name}(NamedArgs_),\n'
                                "  to_struct(Ret).\n\n"
                            )
                            self.add_function(kind, module_func_name, 1,
                                guards_count=99,
                                generated_code=named_args_code.getvalue()
                            )
                    self.add_function(kind, module_func_name, func_arity, guards_count=len(func_guard), generated_code=function_code.getvalue(), typed_function=typed_function)

    def to_gleam_func_name(self, module_func_name: str):
        gleam_func_name = module_func_name.lower()
        if gleam_func_name == 'type':
            gleam_func_name = 'type_'
        return gleam_func_name
    
    def to_gleam_arg_name(self, func_arg_name: str):
        gleam_arg_name = func_arg_name.lower()
        if gleam_arg_name.startswith('_'):
            gleam_arg_name = gleam_arg_name[1:]
        if gleam_arg_name in ['type', 'test']:
            gleam_arg_name = f'{gleam_arg_name}_'
        return gleam_arg_name

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

    def add_function(self, kind: str, function_name: str, function_arity: int, guards_count: int, generated_code: str, typed_function: str = None):
        if self.function.get(kind, None) is None:
            self.function[kind] = {}
        function_dict = self.function[kind]
        if function_name not in function_dict:
            function_dict[function_name] = {}
        if function_arity not in function_dict[function_name]:
            function_dict[function_name][function_arity] = []
        if kind == 'gleam':
            function_dict[function_name][function_arity].append((guards_count, generated_code, typed_function))
        else:
            function_dict[function_name][function_arity].append((guards_count, generated_code))

    def add_function_docs(self, kind: str, function_name: str, function_arity: int, inline_docs: str, typespec: str = None):
        if self.inline_docs.get(kind, None) is None:
            self.inline_docs[kind] = {}
        inline_docs_dict = self.inline_docs[kind]
        if function_name not in inline_docs_dict:
            inline_docs_dict[function_name] = {}
        if function_arity not in inline_docs_dict[function_name]:
            inline_docs_dict[function_name][function_arity] = []
        
        if kind == 'elixir':
            inline_docs_dict[function_name][function_arity].append(inline_docs)
        elif kind == 'erlang':
            inline_docs_dict[function_name][function_arity].append((inline_docs, typespec))
        elif kind == 'gleam':
            inline_docs_dict[function_name][function_arity].append((inline_docs, typespec))

    def print_functions(self, kind: str):
        for function_name, function in self.function[kind].items():
            for function_arity, _function_code in function.items():
                print(f"M={self.module_name}, F={function_name}, A={function_arity}")
