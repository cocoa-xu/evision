#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
from string import Template
from func_variant import FuncVariant
from format_strings import FormatStrings
import evision_templates as ET
from helper import *


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


class FuncInfo(object):
    def __init__(self, classname, name, cname, isconstructor, namespace, is_static):
        self.classname = classname
        self.name = name
        self.cname = cname
        self.isconstructor = isconstructor
        self.namespace = namespace
        self.is_static = is_static
        self.variants = []
        self.argname_prefix_re = re.compile(r'^[_]*')

    def __copy__(self):
        copy_info = FuncInfo(self.classname, self.name, self.cname, self.isconstructor, self.namespace, self.is_static)
        copy_info.variants = self.variants
        return copy_info

    def __deepcopy__(self):
        import copy
        copy_info = FuncInfo(self.classname, self.name, self.cname, self.isconstructor, self.namespace, self.is_static)
        copy_info.variants = copy.deepcopy(self.variants)
        return copy_info

    def add_variant(self, decl, isphantom=False):
        self.variants.append(FuncVariant(self.classname, self.name, decl, self.isconstructor, isphantom))

    def get_wrapper_name(self, add_cv):
        name = self.name
        if self.classname:
            classname = self.classname + "_"
            if "[" in name:
                name = "getelem"
        else:
            classname = ""

        if self.is_static:
            name += "_static"

        erl_name = self.namespace.replace('.', '_') + '_' + classname + name
        erl_name = map_uppercase_to_erlang_name(erl_name)
        if add_cv:
            func_name = f"evision_cv_{erl_name}"
        return erl_name, func_name

    def get_wrapper_prototype(self, add_cv):
        erl_name, full_fname = self.get_wrapper_name(add_cv)
        return "static ERL_NIF_TERM %s(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])" % (full_fname,), erl_name, full_fname

    def get_tab_entry(self):
        prototype_list = []
        docstring_list = []

        have_empty_constructor = False
        for v in self.variants:
            s = v.py_prototype
            if (not v.py_arglist) and self.isconstructor:
                have_empty_constructor = True
            if s not in prototype_list:
                prototype_list.append(s)
                docstring_list.append(v.docstring)

        # if there are just 2 constructors: default one and some other,
        # we simplify the notation.
        # Instead of ClassName(args ...) -> object or ClassName() -> object
        # we write ClassName([args ...]) -> object
        if have_empty_constructor and len(self.variants) == 2:
            idx = self.variants[1].py_arglist != []
            s = self.variants[idx].py_prototype
            p1 = s.find("(")
            p2 = s.rfind(")")
            prototype_list = [s[:p1+1] + "[" + s[p1+1:p2] + "]" + s[p2:]]

        # The final docstring will be: Each prototype, followed by
        # their relevant doxygen comment
        full_docstring = ""
        for prototype, body in zip(prototype_list, docstring_list):
            full_docstring += Template("$prototype\n$docstring\n\n\n\n").substitute(
                prototype=prototype,
                docstring='\n'.join(
                    ['.   ' + line
                     for line in body.split('\n')]
                )
            )

        func_arity = 1
        if self.classname:
            if not self.is_static and not self.isconstructor:
                func_arity = 2
        erl_name, fname = self.get_wrapper_name(True)
        if fname in special_handling_funcs():
            return ""
        if fname.endswith('_read') or fname.endswith('_load_static') or \
                fname.endswith('_write') or fname.endswith('_save') or \
                fname in io_bound_funcs():
            nif_function_decl = f'    F_IO({erl_name}, {fname}, {func_arity}),\n'
        else:
            nif_function_decl = f'    F_CPU({erl_name}, {fname}, {func_arity}),\n'
        return nif_function_decl

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

    def gen_code(self, codegen):
        all_classes = codegen.classes
        proto, _erl_name, fname = self.get_wrapper_prototype(True)
        opt_arg_index = 0
        if self.classname and not self.is_static and not self.isconstructor:
            opt_arg_index = 1
        # special handling for these highgui functions
        if fname in special_handling_funcs():
            return ""
        code = "%s\n{\n" % (proto,)
        code += "    using namespace %s;\n    ERL_NIF_TERM error_term = 0;\n    std::map<std::string, ERL_NIF_TERM> erl_terms;\n" % self.namespace.replace('.', '::')
        code += "    int nif_opts_index = %d; // <- autogenerated value \n    if (nif_opts_index < argc) {\n" % (opt_arg_index, )
        code += "        evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);\n    }\n"

        selfinfo = None
        ismethod = self.classname != "" and not self.isconstructor
        # full name is needed for error diagnostic in PyArg_ParseTupleAndKeywords
        fullname = self.name

        if self.classname:
            selfinfo = all_classes[self.classname]
            if not self.is_static:
                if not self.isconstructor:
                    check_self_templ = ET.gen_template_check_self
                    if "dnn" in self.namespace and selfinfo.cname in ["cv::dnn::Net"]:
                        check_self_templ = ET.gen_template_safe_check_self
                    code += check_self_templ.substitute(
                        name=selfinfo.name,
                        cname=selfinfo.cname if selfinfo.issimple else "Ptr<{}>".format(selfinfo.cname),
                        pname=(selfinfo.cname + '*') if selfinfo.issimple else "Ptr<{}>".format(selfinfo.cname),
                        cvt='' if selfinfo.issimple else '*'
                    )
            fullname = selfinfo.wname + "." + fullname

        all_code_variants = []

        for v in self.variants:
            code_decl = ""
            code_ret = ""
            code_cvt_list = []

            code_args = "("
            all_cargs = []

            if v.isphantom and ismethod and not self.is_static:
                code_args += "_self_"

            # declare all the C function arguments,
            # add necessary conversions from Erlang objects to code_cvt_list,
            # form the function/method call,
            # for the list of type mappings
            for a_index, a in enumerate(v.args):
                if a.tp in ignored_arg_types():
                    defval = a.defval
                    if not defval and a.tp.endswith("*"):
                        defval = "0"
                    assert defval
                    if not code_args.endswith("("):
                        code_args += ", "
                    code_args += defval
                    all_cargs.append([[None, ""], ""])
                    continue
                tp1 = tp = a.tp
                amp = ""
                defval0 = ""
                if tp in pass_by_val_types():
                    tp = tp1 = tp[:-1]
                    amp = "&"
                    if tp.endswith("*"):
                        defval0 = "0"
                        tp1 = tp.replace("*", "_ptr")
                tp_candidates = [a.tp, normalize_class_name(self.namespace + "." + a.tp), normalize_class_name(self.classname + "." + a.tp)]
                if any(tp in codegen.enums.keys() for tp in tp_candidates):
                    defval0 = "static_cast<%s>(%d)" % (a.tp, 0)

                arg_type_info = simple_argtype_mapping.get(tp, ArgTypeInfo(tp, FormatStrings.object, defval0, True, False))
                if any(tp in codegen.enums.keys() for tp in tp_candidates):
                    defval = None
                    if len(a.defval) == 0:
                        defval = f"static_cast<std::underlying_type_t<{arg_type_info.atype}>>({arg_type_info.default_value})"
                    else:
                        defval = f"static_cast<std::underlying_type_t<{arg_type_info.atype}>>({a.defval})"
                    arg_type_info = ArgTypeInfo(f"std::underlying_type_t<{arg_type_info.atype}>", arg_type_info.format_str, defval, True, True)
                    a.defval = defval

                parse_name = a.name
                if a.py_inputarg:
                    parse_name = "erl_term_" + a.name
                    erl_term = "evision_get_kw(env, erl_terms, \"%s\")" % (self.map_elixir_argname(a.name),)
                    self_offset = 0
                    if self.classname and not self.is_static and not self.isconstructor:
                        self_offset = 1
                    if a_index + self_offset < opt_arg_index:
                        erl_term = "argv[%d]" % (a_index + opt_arg_index,)
                    if a.tp == 'char':
                        code_cvt_list.append("convert_to_char(env, %s, &%s, %s)" % (erl_term, a.name, a.crepr()))
                    elif a.tp == 'c_string':
                        code_cvt_list.append("convert_to_char(env, %s, &%s, %s)" % (erl_term, a.name, a.crepr()))
                    else:
                        code_cvt_list.append("evision_to_safe(env, %s, %s, %s)" % (erl_term, a.name, a.crepr()))

                all_cargs.append([arg_type_info, parse_name])

                defval = a.defval
                if not defval:
                    defval = arg_type_info.default_value
                else:
                    if "UMat" in tp:
                        if "Mat" in defval and "UMat" not in defval:
                            defval = defval.replace("Mat", "UMat")
                    if "cuda::GpuMat" in tp:
                        if "Mat" in defval and "GpuMat" not in defval:
                            defval = defval.replace("Mat", "cuda::GpuMat")
                # "tp arg = tp();" is equivalent to "tp arg;" in the case of complex types
                if defval == tp + "()" and arg_type_info.format_str == FormatStrings.object:
                    defval = ""
                if a.outputarg and not a.inputarg:
                    defval = ""
                if defval:
                    if arg_type_info.atype == "QRCodeEncoder_Params":
                        # arg_type_info.atype = ""
                        code_decl += "    QRCodeEncoder::Params %s=%s;\n" % (a.name, defval)
                    else:
                        code_decl += "    %s %s=%s;\n" % (arg_type_info.atype, a.name, defval)
                else:
                    if a.name == "nodeName":
                        code_decl += "    %s %s = String();\n" % (arg_type_info.atype, a.name)
                    else:
                        code_decl += "    %s %s;\n" % (arg_type_info.atype, a.name)

                if not code_args.endswith("("):
                    code_args += ", "

                if a.isrvalueref:
                    a.name = 'std::move(' + a.name + ')'

                if arg_type_info.is_enum:
                    code_args += f"static_cast<{tp}>({a.name})"
                else:
                    code_args += amp + a.name

            code_args += ")"

            if self.isconstructor:
                if selfinfo.issimple:
                    templ_prelude = ET.gen_template_simple_call_constructor_prelude
                    templ = ET.gen_template_simple_call_constructor
                    if "cv::dnn::" in selfinfo.cname:
                        templ_prelude = ET.gen_template_simple_call_dnn_constructor_prelude
                        templ = ET.gen_template_simple_call_dnn_constructor
                else:
                    templ_prelude = ET.gen_template_call_constructor_prelude
                    templ = ET.gen_template_call_constructor

                code_prelude = templ_prelude.substitute(name=selfinfo.name, cname=selfinfo.cname)
                code_fcall = templ.substitute(name=selfinfo.name, cname=selfinfo.cname, py_args=code_args)
                if v.isphantom:
                    code_fcall = code_fcall.replace("new " + selfinfo.cname, self.cname.replace("::", "_"))
            else:
                code_prelude = ""
                code_fcall = ""
                if v.rettype:
                    code_decl += "    " + v.rettype + " retval;\n"
                    code_fcall += "retval = "
                if not v.isphantom and ismethod and not self.is_static:
                    if self.classname and ("Matcher" in v.classname or "Algorithm" in v.classname) and '/PV' not in v.decl[2]:
                        cls = v.classname
                        if self.namespace.startswith('cv.'):
                            inner = self.namespace[3:]
                            prefix = f'{inner}_'
                            if v.classname.startswith(prefix):
                                cls = f'{inner}::' + v.classname[len(prefix):]
                        code_fcall += "_self_->" + cls + "::" + self.cname
                    else:
                        code_fcall += "_self_->" + self.cname
                else:
                    code_fcall += self.cname
                code_fcall += code_args

            # add info about return value, if any, to all_cargs. if there non-void return value,
            # it is encoded in v.py_outlist as ("retval", -1) pair.
            # As [-1] in Python accesses the last element of a list, we automatically handle the return value by
            # adding the necessary info to the end of all_cargs list.
            if v.rettype:
                tp = v.rettype
                tp1 = tp.replace("*", "_ptr")
                default_info = ArgTypeInfo(tp, FormatStrings.object, "0", False, False)
                arg_type_info = simple_argtype_mapping.get(tp, default_info)
                all_cargs.append(arg_type_info)

            if v.args and v.py_arglist:
                # form the argument parse code that:
                #   - declares the list of keyword parameters
                #   - calls evision::nif::parse_arg
                #   - converts complex arguments from ERL_NIF_TERM's to native OpenCV types
                code_parse = ET.gen_template_parse_args.substitute(
                    kw_list=", ".join(['"' + aname + '"' for aname, argno, argtype in v.py_arglist]),
                    code_cvt="{}".format(" && \n        ".join(code_cvt_list)))
            else:
                code_parse = "if((argc - nif_opts_index == 1) && erl_terms.size() == 0)"

            if len(v.py_outlist) == 0:
                code_ret = "return evision::nif::atom(env, \"nil\")"
                if not v.isphantom and ismethod and not self.is_static:
                    code_ret = "return evision::nif::ok(env, self)"
            elif len(v.py_outlist) == 1:
                if self.isconstructor:
                    if self.classname in ["VideoCapture"]:
                        code_ret = ET.code_ret_constructor_structurise % (self.classname,)
                    else:
                        code_ret = ET.code_ret_constructor
                else:
                    aname, argno = v.py_outlist[0]
                    if v.rettype == 'bool':
                        code_ret = "if (%s) {\n                return evision::nif::atom(env, \"ok\");\n            } else {\n                return evision::nif::atom(env, \"error\");\n            }" % (aname,)
                    elif v.rettype == 'Mat':
                        code_ret = f"ERL_NIF_TERM mat_ret = evision_from(env, {aname});\n" \
                                   "            if (enif_is_ref(env, mat_ret) || enif_is_map(env, mat_ret)) return evision::nif::ok(env, mat_ret);\n" \
                                   "            else return mat_ret;"
                    else:
                        code_ret = "return evision::nif::ok(env, evision_from(env, %s))" % (aname,)
            else:
                # there is more than 1 return parameter; form the tuple out of them
                n_tuple = len(v.py_outlist)
                evision_from_calls = ["evision_from(env, " + aname + ")" for aname, argno in v.py_outlist]
                if v.rettype == 'bool':
                    if n_tuple >= 10:
                        code_ret = ET.code_ret_ge_10_tuple_except_bool % (",\n        ".join(evision_from_calls[1:]), n_tuple-1)
                    elif (n_tuple-1) == 1:
                        if "imencode" in fname:
                            code_ret = ET.code_ret_as_binary % (", ".join(evision_from_calls[1:]).replace("evision_from", "evision_from_as_binary").replace(")", ", success)"),)
                        else:
                            code_ret = ET.code_ret_1_tuple_except_bool % (", ".join(evision_from_calls[1:]),)
                    else:
                        code_ret = ET.code_ret_2_to_10_tuple_except_bool % (n_tuple-1, ", ".join(evision_from_calls[1:]))
                else:
                    if n_tuple >= 10:
                        code_ret = ET.code_ret_ge_10_tuple % (",\n                ".join(evision_from_calls), n_tuple)
                    else:
                        code_ret = ET.code_ret_lt_10_tuple % (n_tuple, ", ".join(evision_from_calls))

            all_code_variants.append(ET.gen_template_func_body.substitute(code_decl=code_decl, code_parse=code_parse,
                                                                       code_prelude=code_prelude, code_fcall=code_fcall,
                                                                       code_ret=code_ret))

        if len(all_code_variants) == 1:
            # if the function/method has only 1 signature, then just put it
            code += all_code_variants[0]
        else:
            # try to execute each signature, add an interlude between function
            # calls to collect error from all conversions
            code += '    \n'.join(ET.gen_template_overloaded_function_call.substitute(variant=v)
                                  for v in all_code_variants)

        def_ret = "if (error_term != 0) return error_term;\n    else return evision::nif::error(env, \"overload resolution failed\");"
        if self.isconstructor:
            def_ret = "return evision::nif::error(env, \"overload resolution failed\");"
        code += "\n    %s\n}\n\n" % def_ret

        cname = self.cname
        classinfo = None
        #dump = False
        #if dump: pprint(vars(self))
        #if dump: pprint(vars(self.variants[0]))
        if self.classname:
            classinfo = all_classes[self.classname]
            #if dump: pprint(vars(classinfo))
            if self.isconstructor:
                py_name = 'cv.' + classinfo.wname
            elif self.is_static:
                py_name = '.'.join([self.namespace, classinfo.sname + '_' + self.variants[0].wname])
            else:
                cname = classinfo.cname + '::' + cname
                py_name = 'cv.' + classinfo.wname + '.' + self.variants[0].wname
        else:
            py_name = '.'.join([self.namespace, self.variants[0].wname])
        #if dump: print(cname + " => " + py_name)
        py_signatures = codegen.py_signatures.setdefault(cname, [])
        for v in self.variants:
            s = dict(name=py_name, arg=v.py_arg_str, ret=v.py_return_str)
            for old in py_signatures:
                if s == old:
                    break
            else:
                py_signatures.append(s)

        return code
