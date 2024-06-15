#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from io import StringIO
from typing import Tuple
from helper import handle_ptr, forbidden_arg_types, ignored_arg_types, map_argtype_to_guard, map_argname, map_argtype_to_type, handle_inline_math_escaping, map_argtype_in_docs, map_argtype_in_spec, is_struct
from arg_info import ArgInfo
import re
inline_docs_code_type_re = re.compile(r'@code{.(.*)}')

class FuncVariant(object):
    def __init__(self, classname: str, name: str, decl: list, isconstructor: bool, isphantom: bool = False):
        self.name = self.wname = name
        self.isconstructor = isconstructor
        self.isphantom = isphantom
        
        # ---- added in evision start ----
        self.keyword_args = []
        self.classname = classname
        self.from_base = False
        self.base_classname = None
        self.decl = decl
        # ---- added in evision end ----
        
        self.docstring = decl[5]

        self.rettype = decl[4] or handle_ptr(decl[1])
        if self.rettype == "void":
            self.rettype = ""
        self.args = []
        self.array_counters = {}
        for arg_decl in decl[3]:
            assert len(arg_decl) == 4, \
                'ArgInfo contract is violated. Arg declaration should contain:' \
                '"arg_type", "name", "default_value", "modifiers". '\
                'Got tuple: {}'.format(arg_decl)

            ainfo = ArgInfo(atype=arg_decl[0], name=arg_decl[1],
                            default_value=arg_decl[2], modifiers=arg_decl[3])
            if ainfo.isarray and not ainfo.arraycvt:
                c = ainfo.arraylen
                c_arrlist = self.array_counters.get(c, [])
                if c_arrlist:
                    c_arrlist.append(ainfo.name)
                else:
                    self.array_counters[c] = [ainfo.name]
            self.args.append(ainfo)
        self.init_pyproto()

    def init_pyproto(self):
        # string representation of argument list, with '[', ']' symbols denoting optional arguments, e.g.
        # "src1, src2[, dst[, mask]]" for cv.add
        argstr = ""

        # list of all input arguments of the Python function, with the argument numbers:
        #    [("src1", 0), ("src2", 1), ("dst", 2), ("mask", 3)]
        # we keep an argument number to find the respective argument quickly, because
        # some of the arguments of C function may not present in the Python function (such as array counters)
        # or even go in a different order ("heavy" output parameters of the C function
        # become the first optional input parameters of the Python function, and thus they are placed right after
        # non-optional input parameters)
        arglist = []

        # the list of "heavy" output parameters. Heavy parameters are the parameters
        # that can be expensive to allocate each time, such as vectors and matrices (see isbig).
        outarr_list = []

        # the list of output parameters. Also includes input/output parameters.
        outlist = []

        firstoptarg = 1000000
        argno = -1
        for a in self.args:
            argno += 1
            if a.name in self.array_counters:
                continue
            assert not a.tp in forbidden_arg_types(), 'Forbidden type "{}" for argument "{}" in "{}" ("{}")'.format(a.tp, a.name, self.name, self.classname)
            if a.tp in ignored_arg_types():
                continue
            if a.returnarg:
                outlist.append((a.name, argno))
            if (not a.inputarg) and a.isbig():
                outarr_list.append((a.name, argno, a.tp))
                continue
            if not a.inputarg:
                continue
            if not a.defval:
                arglist.append((a.name, argno, a.tp))
            else:
                firstoptarg = min(firstoptarg, len(arglist))
                # if there are some array output parameters before the first default parameter, they
                # are added as optional parameters before the first optional parameter
                if outarr_list:
                    arglist += outarr_list
                    outarr_list = []
                arglist.append((a.name, argno, a.tp))

        if outarr_list:
            firstoptarg = min(firstoptarg, len(arglist))
            arglist += outarr_list
        firstoptarg = min(firstoptarg, len(arglist))

        noptargs = len(arglist) - firstoptarg
        argnamelist = [aname for aname, argno, argtype in arglist]
        argstr = ", ".join(argnamelist[:firstoptarg])
        argstr = "[, ".join([argstr] + argnamelist[firstoptarg:])
        argstr += "]" * noptargs
        if self.rettype:
            outlist = [("retval", -1)] + outlist
        elif self.isconstructor:
            assert outlist == []
            outlist = [("self", -1)]
        if self.isconstructor:
            classname = self.classname
            if classname.startswith("Cv"):
                classname=classname[2:]
            outstr = "<%s object>" % (classname,)
        elif outlist:
            outstr = ", ".join([o[0] for o in outlist])
        else:
            outstr = "None"

        self.py_arg_str = argstr
        self.py_return_str = outstr
        self.py_prototype = "%s(%s) -> %s" % (self.wname, argstr, outstr)
        self.py_noptargs = noptargs
        self.py_arglist = arglist
        for aname, argno, argtype in arglist:
            self.args[argno].py_inputarg = True
        for aname, argno in outlist:
            if argno >= 0:
                self.args[argno].py_outputarg = True
        self.py_outlist = outlist

    @property
    def pos_end(self):
        return len(self.py_arglist) if not self.has_opts else -self.py_noptargs

    @property
    def has_opts(self):
        return self.py_noptargs > 0

    @property
    def min_args(self):
        return len(self.py_arglist) - self.py_noptargs

    def function_guard(self, kind: str):
        return list(filter(lambda x: x != '', [map_argtype_to_guard(kind, map_argname(kind, argname), argtype, classname=self.classname) for argname, _, argtype in self.py_arglist[:self.pos_end]]))

    def function_signature(self):
        return ''.join(filter(lambda x: x != '', [map_argtype_to_type(argtype, classname=self.classname) for _, _, argtype in self.py_arglist[:self.pos_end]]))
    
    def inline_docs(self, kind: str, is_instance_method: bool, module_name: str) -> str:
        if kind == 'elixir':
            return self.inline_docs_elixir(is_instance_method, module_name)
        elif kind == 'erlang':
            return self.inline_docs_erlang(is_instance_method, module_name)
        elif kind == 'gleam':
            return self.inline_docs_erlang(is_instance_method, module_name)
        else:
            return ''

    def __escape_inline_docs__(self, line: str):
        line = line.replace("\\", r"\\")
        line = line.replace(r"\\\\", r"\\\\\\\\")
        strip_line = line.strip()
        if strip_line.startswith("*"):
            strip_line = strip_line[1:].strip()
            line = line.replace("*", "", 1)
        if strip_line.startswith('"""'):
            strip_line = strip_line.replace('"""', r'\"\"\"', 1)
            line = line.replace('"""', r'\"\"\"', 1)
        return strip_line, line

    def __maybe_multiline__(self, lines, line_index: int, total_lines: int, append_to: StringIO, current_indent: int, add_newline_before_appending: bool, multiline_end_mark=['@']):
        # deal with multi-line inline docs items
        peak_line_index = line_index + 1
        is_multiline = False
        added_newline = False
        last_line = ''
        while peak_line_index < total_lines:
            peak_line = lines[peak_line_index].strip()
            peak_line_stripped, peak_line_escaped = self.__escape_inline_docs__(peak_line)
            if len(peak_line_stripped) == 0:
                if not last_line.endswith('\\\\'):
                    break
                else:
                    peak_line_index += 1
                    continue

            if sum(peak_line_stripped.startswith(mark) for mark in multiline_end_mark) > 0:
                break

            last_line = peak_line_escaped
            is_multiline = True
            if add_newline_before_appending and not added_newline:
                added_newline = True
                append_to.write('\n')

            append_to.write(' ' * current_indent)

            if peak_line_stripped[0] in ['-', '*']:
                append_to.write(peak_line_stripped[0])
                append_to.write(' ')
                append_to.write(peak_line_stripped[1:].strip())
                append_to.write('\n')
                _, peak_line_index = self.__maybe_multiline__(lines, peak_line_index, total_lines, append_to, 
                    current_indent=current_indent + 2,
                    add_newline_before_appending=False,
                    multiline_end_mark=['-', '@', '*/', '\\\\f']
                )
            else:
                append_to.write(peak_line_stripped)
                append_to.write('\n')
            peak_line_index += 1

        if is_multiline:
            line_index = peak_line_index - 1
        return is_multiline, line_index

    def inline_docs_erlang(self, is_instance_method: bool, module_name: str) -> str:
        return ''

    def inline_docs_elixir(self, is_instance_method: bool, module_name: str) -> str:
        global inline_docs_code_type_re
        parameter_info = {}
        doc_string = "\n".join('  {}'.format(line.strip()) for line in self.docstring.split("\n")).strip()
        if len(doc_string) > 0:
            doc_string = f'\n  {doc_string}\n'
        else:
            doc_string = '\n'

        function_brief = ''
        opt_doc = ''
        prototype = f'  Python prototype (for reference): {self.py_prototype}'
        if self.has_opts:
            opt_doc = '\n'.join(['  @optional {}: {}'.format(arg_name, argtype) for (arg_name, _, argtype) in self.py_arglist[-self.py_noptargs:]])
            opt_doc += '\n'

        inline_doc = f'\n  @doc """<evision_param_info>\n{doc_string}{opt_doc}\n{prototype}\n'
        inline_doc1 = ""
        last_in_list = False
        last_is_code = False
        lines = inline_doc.split("\n")
        line_index = 0
        total_lines = len(lines)
        current_indent = 2
        while line_index < total_lines:
            line = lines[line_index]
            strip_line, line = self.__escape_inline_docs__(line)
            if strip_line == "=":
                inline_doc = inline_doc[:-1] + "="
                line_index += 1
                continue

            if strip_line.startswith("@"):
                if strip_line != "@doc \"\"\"":
                    if strip_line.startswith("@brief"):
                        brief_str = StringIO()
                        brief_str.write("{}\n".format(strip_line[len("@brief"):].strip()))
                        is_multiline, line_index = self.__maybe_multiline__(lines, line_index, total_lines, brief_str, 
                            current_indent=current_indent,
                            add_newline_before_appending=False
                        )
                        if is_multiline:
                            brief_str.write('\n')
                        function_brief = brief_str.getvalue()
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
                        arg_desc = strip_line.split(' ', 2)
                        description = StringIO()
                        if len(arg_desc) == 3:
                            description.write(arg_desc[2].strip())

                        # deal with multi-line @param/@optional
                        is_multiline, line_index = self.__maybe_multiline__(lines, line_index, total_lines, description, 
                            current_indent=current_indent + 2,
                            add_newline_before_appending=True
                        )
                        if is_multiline is False:
                            description.write('\n')

                        normalized_arg_name = map_argname('elixir', arg_desc[1])
                        normalized_arg_name = normalized_arg_name.replace(":", "")
                        is_optional = strip_line.startswith("@optional")

                        description = description.getvalue()
                        if parameter_info.get(normalized_arg_name) is None:
                            parameter_info[normalized_arg_name] = {
                                "is_optional": is_optional,
                                "desc": description
                            }
                        else:
                            if len(parameter_info[normalized_arg_name]["desc"]) < len(description):
                                parameter_info[normalized_arg_name]["desc"] = description
                        last_in_list = True
                    elif strip_line.startswith("@code"):
                        last_in_list = False
                        last_is_code = True
                        code_type_match = inline_docs_code_type_re.match(strip_line)
                        inline_doc1 += "  ```"
                        if code_type_match:
                            inline_doc1 += code_type_match.group(1)
                        inline_doc1 += "\n"
                    elif strip_line.startswith("@endcode"):
                        last_in_list = False
                        last_is_code = False
                        inline_doc1 += "  ```\n"
                    elif strip_line.startswith("@see set"):
                        parts = strip_line.split(' ')
                        if len(parts) == 2:
                            inline_doc1 += "  @see `{}/2`\n".format(parts[1])
                        else:
                            inline_doc1 += "  {}\n".format(strip_line)
                        last_in_list = False
                    elif strip_line.startswith("@copybrief "):
                        # upstream: hdr_parser.py
                        parts = strip_line.split(' ')
                        if len(parts) == 2 and parts[1].startswith("get"):
                            inline_doc1 += "  @see `{}/1`\n".format(parts[1])
                        elif len(parts) == 4 and parts[2] == "@see" and parts[3].startswith("get"):
                            inline_doc1 += "  @see `{}/1`\n".format(parts[1])
                        else:
                            inline_doc1 += "  {}\n".format(strip_line)
                        last_in_list = False
                    else:
                        inline_doc1 += "  {}\n".format(strip_line)
                        last_in_list = False
                else:
                    inline_doc1 += "  @doc \"\"\"\n"
            elif strip_line.startswith("Python prototype (for reference): "):
                inline_doc1 += "\n  Python prototype (for reference only):\n  ```python3\n  {}\n  ```\n".format(strip_line[len("Python prototype (for reference): "):])
                last_in_list = False
            elif strip_line.startswith("-"):
                list_content = StringIO()
                list_content.write(' ' * current_indent)
                list_content.write('- ')
                list_content.write(strip_line[1:].strip())
                _, line_index = self.__maybe_multiline__(lines, line_index, total_lines, list_content,
                    current_indent=current_indent + 2,
                    add_newline_before_appending=True,
                    multiline_end_mark=['-', '@']
                )
                list_content.write('\n')
                inline_doc1 += list_content.getvalue()
                last_in_list = True
            elif len(strip_line) != 0:
                if last_in_list:
                    inline_doc1 += '\n'
                    last_in_list = False
                if strip_line == '\\note':
                    strip_line = '**Note**'
                elif strip_line == '\\\\overload':
                    line_index += 1
                    continue

                if last_is_code:
                    inline_doc1 += "  {}\n".format(strip_line)
                else:
                    inline_doc1 += "{}{}\n".format("  " if last_in_list else "", line)
            else:
                if last_in_list:
                    inline_doc1 += '\n'
                    last_in_list = False
            line_index += 1

        if len(function_brief) == 0:
            function_brief = self.name
        inline_doc = inline_doc1.replace('@doc """', function_brief)
        inline_doc = handle_inline_math_escaping(inline_doc)
        parameter_info_doc = StringIO()

        out_args = [o[0] for o in self.py_outlist]

        min_args = self.min_args
        if is_instance_method:
            min_args += 1

        if min_args > 0:
            positional_args = []
            if is_instance_method:
                if module_name.startswith("Evision"):
                    positional_args.append(('self', -1, f"{module_name}.t()"))
                else:
                    positional_args.append(('self', -1, f"Evision.{module_name}.t()"))
            for (arg_name, argno, argtype) in self.py_arglist[:self.pos_end]:
                if arg_name not in out_args:
                    positional_args.append((arg_name, argno, argtype))
            if len(positional_args) > 0:
                parameter_info_doc.write("\n  ##### Positional Arguments\n")
                for (arg_name, _, argtype) in positional_args:
                    argtype1 = map_argtype_in_docs('elixir', argtype) #, classname=self.classname)
                    if argtype1.startswith("Evision."):
                        if not argtype1.endswith(".t()"):
                            if (argtype1[9] <= 'Z' and argtype1[9] >= 'A'):
                                argtype1 = f"{argtype1}.t()"
                    normalized_arg_name = map_argname('elixir', arg_name)
                    normalized_arg_name = normalized_arg_name.replace(":", "")
                    if parameter_info.get(normalized_arg_name, None) is None:
                        parameter_info_doc.write(f"  - **{normalized_arg_name}**: `{argtype1}`\n")
                    else:
                        info = parameter_info[normalized_arg_name]
                        parameter_info_doc.write(f"  - **{normalized_arg_name}**: `{argtype1}`.\n\n    {info['desc']}\n")
                parameter_info_doc.write("\n")

        if self.has_opts:
            optional_args = []
            for (arg_name, argno, argtype) in self.py_arglist[self.pos_end:]:
                if arg_name not in out_args:
                    optional_args.append((arg_name, argno, argtype))
            if len(optional_args) > 0:
                parameter_info_doc.write("  ##### Keyword Arguments\n")
                for (arg_name, _, argtype) in optional_args:
                    argtype1 = map_argtype_in_docs('elixir', argtype)# , classname=self.classname)
                    if argtype1.startswith("Evision."):
                        if not argtype1.endswith(".t()"):
                            if (argtype1[9] <= 'Z' and argtype1[9] >= 'A'):
                                argtype1 = f"{argtype1}.t()"
                    normalized_arg_name = map_argname('elixir', arg_name)
                    normalized_arg_name = normalized_arg_name.replace(":", "")
                    if parameter_info.get(normalized_arg_name, None) is None:
                        parameter_info_doc.write(f"  - **{normalized_arg_name}**: `{argtype1}`\n")
                    else:
                        info = parameter_info[normalized_arg_name]
                        if argtype == info['desc'].strip():
                            parameter_info_doc.write(f"  - **{normalized_arg_name}**: `{argtype1}`.\n")
                        else:
                            parameter_info_doc.write(f"  - **{normalized_arg_name}**: `{argtype1}`.\n\n    {info['desc']}\n")
                parameter_info_doc.write("\n")

        if len(out_args) > 0:
            return_values = []
            for arg in self.args:
                if arg.name in out_args:
                    return_values.append((arg.name, arg.tp))
            out_args_name = [o[0] for o in self.py_outlist]
            if len(out_args_name) > 0 and (out_args_name[0] in ['retval', 'self']) and self.py_outlist[0][1] == -1:
                if out_args_name[0] == 'retval':
                    rettype_docs = map_argtype_in_docs('elixir', self.rettype)
                    if rettype_docs.startswith("Evision."):
                        if not rettype_docs.endswith(".t()"):
                            rettype_docs = f"{rettype_docs}.t()"
                    return_values.insert(0, ('retval', rettype_docs))
                elif out_args_name[0] == 'self':
                    selftype_docs = map_argtype_in_docs('elixir', self.name)
                    if selftype_docs.startswith("Evision."):
                        if not selftype_docs.endswith(".t()"):
                            selftype_docs = f"{selftype_docs}.t()"
                    return_values.insert(0, ('self', selftype_docs))
            elif self.isconstructor:
                selftype_docs = map_argtype_in_docs('elixir', self.classname)
                if selftype_docs.startswith("Evision."):
                    if not selftype_docs.endswith(".t()"):
                        selftype_docs = f"{selftype_docs}.t()"
                return_values.insert(0, ('self', map_argtype_in_docs('elixir', selftype_docs)))

            if len(return_values) > 0:
                parameter_info_doc.write("  ##### Return\n")
                for (arg_name, argtype) in return_values:
                    argtype1 = map_argtype_in_docs('elixir', argtype)# , classname=self.classname)
                    if argtype1.startswith("Evision."):
                        if not argtype1.endswith(".t()"):
                            argtype1 = f"{argtype1}.t()"
                    normalized_arg_name = map_argname('elixir', arg_name)
                    normalized_arg_name = normalized_arg_name.replace(":", "")
                    if parameter_info.get(normalized_arg_name, None) is None:
                        parameter_info_doc.write(f"  - **{normalized_arg_name}**: `{argtype1}`\n")
                    else:
                        info = parameter_info[normalized_arg_name]
                        if argtype == info['desc'].strip():
                            parameter_info_doc.write(f"  - **{normalized_arg_name}**: `{argtype1}`.\n")
                        else:
                            parameter_info_doc.write(f"  - **{normalized_arg_name}**: `{argtype1}`.\n\n    {info['desc']}\n")

        if '\n<evision_param_info>' in inline_doc:
            inline_doc = inline_doc.replace('<evision_param_info>', parameter_info_doc.getvalue())
        else:
            inline_doc = inline_doc.replace('<evision_param_info>', '\n' + parameter_info_doc.getvalue())

        inline_docs = StringIO()
        last_empty = False
        for line in inline_doc.split("\n"):
            line_1 = line.strip()
            empty = len(line_1) == 0
            if last_empty and empty:
                pass
            else:
                inline_docs.write(line)
                inline_docs.write("\n")
            last_empty = empty

        inline_doc = inline_docs.getvalue()[:-1]
        return inline_doc.rstrip()
    
    def keyword_arg_names(self, kind: str):
        keyword_args = set()
        if self.has_opts:
            out_args = [o[0] for o in self.py_outlist]
            optional_args = []
            for (arg_name, argno, argtype) in self.py_arglist[self.pos_end:]:
                if arg_name not in out_args:
                    optional_args.append((arg_name, argno, argtype))
            if len(optional_args) > 0:
                for (arg_name, _, _) in optional_args:
                    normalized_arg_name = map_argname(kind, arg_name)
                    normalized_arg_name = normalized_arg_name.replace(":", "")
                    keyword_args.add(normalized_arg_name)
        self.keyword_args = sorted(keyword_args)
        return self.keyword_args
    
    def out_args(self):
        out_args = list(self.py_outlist)
        out_args_name = [o[0] for o in out_args]
        
        if len(out_args_name) > 0 and (out_args_name[0] in ['retval', 'self']) and self.py_outlist[0][1] == -1:
            if out_args_name[0] == 'retval':
                out_args = [self.rettype]
            elif out_args_name[0] == 'self':
                out_args = [self.name]
            out_args_name = out_args_name[1:]
        elif self.isconstructor:
            out_args = [self.classname]
        else:
            if self.name == "setInputParams":
                out_args = [self.classname[4:]]
            else:
                out_args = []

        for arg in self.args:
            if arg.name in out_args_name:
                out_args.append(arg.tp)
        return out_args, out_args_name
    
    def in_args(self, out_args):
        in_args = None
        in_args_name = None
        if self.min_args > 0:
            in_args = []
            in_args_name = []
            for (arg_name, _, argtype) in self.py_arglist[:self.pos_end]:
                if arg_name not in out_args:
                    in_args.append(argtype)
                    in_args_name.append(arg_name)
        return in_args, in_args_name

    def generate_spec(self, kind: str, module_func_name: str, is_instance_method: bool, include_opts: bool, module_name: str) -> str:
        if kind == 'elixir':
            return self.generate_spec_elixir(module_func_name, is_instance_method, include_opts, module_name)
        elif kind == 'erlang':
            return self.generate_spec_erlang(module_func_name, is_instance_method, include_opts, module_name)
        elif kind == 'gleam':
            return self.generate_spec_erlang(module_func_name, is_instance_method, include_opts, module_name)
        else:
            return ''

    def generate_spec_elixir(self, module_func_name: str, is_instance_method: bool, include_opts: bool, module_name: str) -> str:
        spec = StringIO()
        ismethod = self.classname != "" and not self.isconstructor

        out_args, out_args_name = self.out_args()
        in_args, in_args_name = self.in_args(out_args)
        in_args_spec = []
        if in_args is not None:
            for argtype in in_args:
                in_args_spec.append(map_argtype_in_spec('elixir', self.classname, argtype, is_in=True, decl=self.decl))

        spec.write(f'@spec {module_func_name}(')
        if self.has_opts and include_opts:
            keyword_arg_names = self.keyword_arg_names('elixir')
            named_in_args_spec = []
            for arg_name in keyword_arg_names:
                named_in_args_spec.append(f'{{:{arg_name}, term()}}')
            if len(named_in_args_spec) > 0:
                named_in_args_spec = f"[{' | '.join(named_in_args_spec)}] | nil"
            else:
                named_in_args_spec = "[{atom(), term()},...] | nil"
            in_args_spec.append(named_in_args_spec)
        if is_instance_method:
            self.spec_self = ''
            tmp_name = module_name
            # tmp_name = self.classname

            if len(tmp_name) > 0:
                is_param = False
                if tmp_name.endswith('_Param'):
                    tmp_name = tmp_name[:len('_Param')]
                    is_param = True
                if tmp_name.startswith('ppf_match_3d'):
                    self.spec_self = tmp_name[len('ppf_match_3d_'):]
                else:
                    parts = tmp_name.split('_', maxsplit=2)
                    self.spec_self = parts[-1]
                if is_param:
                    self.spec_self += '_Param'
                if tmp_name == 'ml_ANN_MLP':
                    self.spec_self = 'ANN_MLP'
                elif tmp_name == 'dnn_TextDetectionModel_DB':
                    self.spec_self = 'TextDetectionModel_DB'
                elif tmp_name == 'dnn_TextDetectionModel_EAST':
                    self.spec_self = 'TextDetectionModel_EAST'
                if len(out_args) == 0 and len(out_args_name) == 0:
                    out_args = [self.spec_self]

            if is_struct(self.spec_self, classname=self.classname):
                _, struct_name = is_struct(self.spec_self, also_get='struct_name', classname=self.classname)
                self.spec_self = f'{struct_name}.t()'
            else:
                print(f'warning: {self.spec_self} should be a struct. classname={self.classname}. possible fix: change function is_struct accordingly in helper.py.')
                self.spec_self = f'Evision.{self.spec_self}.t()'
            in_args_spec.insert(0, self.spec_self)

        spec.write(", ".join(in_args_spec))
        spec.write(") :: ")

        out_spec = ''
        out_args_spec = []
        if out_args is not None and len(out_args) > 0:
            for argtype in out_args:
                out_args_spec.append(map_argtype_in_spec('elixir', self.classname, argtype, is_in=False, decl=self.decl))
            out_spec = ", ".join(out_args_spec)
        else:
            out_args_spec = [':ok']

        ok_error = True
        if len(out_args_spec) == 1:
            if out_args_spec[0] == 'boolean()':
                out_spec = 'boolean() | {:error, String.t()}'
                ok_error = False
            elif out_args_spec[0] == ':ok':
                out_spec = ':ok | {:error, String.t()}'
                ok_error = False
        else:
            if out_args_spec[0] == 'boolean()':
                out_spec = ", ".join(out_args_spec[1:])
                if len(out_args_spec) == 2:
                    out_spec = f'{out_spec} | false | {{:error, String.t()}}'
                else:
                    out_spec = f'{{{out_spec}}} | false | {{:error, String.t()}}'
                ok_error = False

        if ok_error:
            if len(out_args_spec) == 1:
                out_spec = f'{out_spec} | {{:error, String.t()}}'
            else:
                out_spec = f'{{{out_spec}}} | {{:error, String.t()}}'
        spec.write(out_spec)

        spec = spec.getvalue()
        return spec

    def generate_spec_erlang(self, module_func_name: str, is_instance_method: bool, include_opts: bool, module_name: str) -> str:
        spec = StringIO()

        out_args, out_args_name = self.out_args()
        in_args, in_args_name = self.in_args(out_args)

        spec.write(f'-spec {module_func_name}(')
        in_args_spec = []
        if in_args is not None:
            for argtype in in_args:
                in_args_spec.append(map_argtype_in_spec('erlang', self.classname, argtype, is_in=True, decl=self.decl))
        if self.has_opts and include_opts:
            in_args_spec.append('[{atom(), term()},...] | nil')
        if is_instance_method:
            self.spec_self = ''
            tmp_name = module_name
            if len(tmp_name) > 0:
                is_param = False
                if tmp_name.endswith('_Param'):
                    tmp_name = tmp_name[:len('_Param')]
                    is_param = True
                if tmp_name.startswith('ppf_match_3d'):
                    self.spec_self = tmp_name[len('ppf_match_3d_'):]
                else:
                    parts = tmp_name.split('_', maxsplit=2)
                    self.spec_self = parts[-1]
                if is_param:
                    self.spec_self += '_Param'
                if tmp_name == 'ml_ANN_MLP':
                    self.spec_self = 'ANN_MLP'
                elif tmp_name == 'dnn_TextDetectionModel_DB':
                    self.spec_self = 'TextDetectionModel_DB'
                elif tmp_name == 'dnn_TextDetectionModel_EAST':
                    self.spec_self = 'TextDetectionModel_EAST'
                if len(out_args_name) == 0:
                    out_args = [self.spec_self]

            if is_struct(self.spec_self, classname=self.classname):
                _, struct_name = is_struct(self.spec_self, also_get='struct_name', classname=self.classname, decl=self.decl)
                # duplicated = struct_name.split('.')
                # if len(duplicated) >= 3 and duplicated[1] == duplicated[2]:
                #     print("here", self.spec_self, "classname", self.classname)
                ty = struct_name.replace('.', '_').lower()
                self.spec_self = f'#{ty}'+'{}'
            else:
                ty = self.spec_self.replace('.', '_').lower()
                self.spec_self = f'#evision_{ty}' + '{}'
            in_args_spec.insert(0, self.spec_self)

        spec.write(", ".join(in_args_spec))
        spec.write(") -> ")

        out_spec = ''
        out_args_spec = []
        if out_args is not None and len(out_args) > 0:
            for argtype in out_args:
                out_args_spec.append(map_argtype_in_spec('erlang', self.classname, argtype, is_in=False, decl=self.decl))
            out_spec = ", ".join(out_args_spec)
        else:
            out_args_spec = ['ok']

        ok_error = True
        if len(out_args_spec) == 1:
            if out_args_spec[0] == 'boolean()':
                out_spec = 'boolean() | {error, binary()}'
                ok_error = False
            elif out_args_spec[0] == 'ok':
                out_spec = 'ok | {error, binary()}'
                ok_error = False
        else:
            if out_args_spec[0] == 'boolean()':
                out_spec = ", ".join(out_args_spec[1:])
                if len(out_args_spec) == 2:
                    out_spec = f'{out_spec} | false | {{error, binary()}}'
                else:
                    out_spec = f'{{{out_spec}}} | false | {{error, binary()}}'
                ok_error = False

        if ok_error:
            if len(out_args_spec) == 1:
                out_spec = f'{out_spec} | {{error, binary()}}'
            else:
                out_spec = f'{{{out_spec}}} | {{error, binary()}}'
        spec.write(out_spec)
        spec.write('.')

        spec = spec.getvalue()
        return spec

    def opts_args(self, kind: str, in_func_body: bool = False):
        if self.has_opts:
            if kind == 'elixir':
                return self.opts_args_elixir(in_func_body=in_func_body)
            elif kind == 'erlang':
                return self.opts_args_erlang(in_func_body=in_func_body)
            elif kind == 'gleam':
                return self.opts_args_erlang(in_func_body=in_func_body)
            else:
                print(f'warning: opt_args: unknown kind `{kind}`')
                return ''
        else:
            return ''

    def opts_args_elixir(self, in_func_body: bool = False):
        opts_args = ''
        if self.has_opts:
            if in_func_body:
                opts_args = ' ++ Evision.Internal.Structurise.from_struct(opts || [])'
            else:
                opts_args = 'opts' if self.min_args == 0 else ', opts'
        return opts_args

    def opts_args_erlang(self, in_func_body: bool = False):
        opts_args = ''
        if self.has_opts:
            if in_func_body:
                opts_args = ' ++ evision_internal_structurise:from_struct(Options)'
            else:
                opts_args = 'Options' if self.min_args == 0 else ', Options'
        return opts_args
 
    def positional_args(self, kind: str):
        if kind == 'elixir':
            return self.positional_args_elixir()
        elif kind == 'erlang':
            return self.positional_args_erlang()
        elif kind == 'gleam':
            return self.positional_args_gleam()
        else:
            print(f'warning: positional_args: unknown kind `{kind}`')

    def positional_args_elixir(self):
        positional_var = 'positional'
        positional = '{} = [{}\n    ]'.format(positional_var, ",".join(['\n      {}: {}'.format(map_argname('elixir', arg_name), map_argname('elixir', arg_name, argtype=argtype, from_struct=True)) for (arg_name, _, argtype) in self.py_arglist[:self.pos_end]]))
        return positional, positional_var
    
    def positional_args_erlang(self):
        # [{elixir_arg_atom, evision_internal_structurise:from_struct(ErlangVar)}, ...]
        positional_var = 'Positional'
        positional = '{} = [{}\n  ]'.format(positional_var, ",".join(['\n    {}, evision_internal_structurise:from_struct({}'.format('{' + map_argname('elixir', arg_name), map_argname('erlang', arg_name) + ')}') for (arg_name, _, argtype) in self.py_arglist[:self.pos_end]]))
        return positional, positional_var
    
    def positional_args_gleam(self):
        positional_var = 'Positional'
        positional = '{} = [{}\n  ]'.format(positional_var, ",".join(['\n    {}, evision_internal_structurise:from_struct({}'.format('{' + map_argname('elixir', arg_name), map_argname('erlang', arg_name) + ')}') for (arg_name, _, argtype) in self.py_arglist[:self.pos_end]]))
        pos_typed = [(argname, argtype) for (argname, _, argtype) in self.py_arglist[:self.pos_end]]
        return positional, positional_var, pos_typed

    def func_args(self, kind: str, instance_method: bool = False, in_func_body: bool = False):
        positional_args = self.py_arglist[:self.pos_end]
        func_args = '{}'.format(", ".join(['{}'.format(map_argname(kind, arg_name)) for (arg_name, _, _argtype) in positional_args]))
        func_args_with_opts = ''
        if self.has_opts:
            if len(positional_args) > 0:
                func_args_with_opts = '{}{}'.format(", ".join(['{}'.format(map_argname(kind, arg_name)) for (arg_name, _, _argtype) in positional_args]), self.opts_args(kind))
            else:
                func_args_with_opts = self.opts_args(kind)

        if instance_method:
            self_arg = ''
            if kind == 'elixir':
                self_arg = 'self'
                if in_func_body:
                    self_arg = 'Evision.Internal.Structurise.from_struct(self)'
            elif kind == 'erlang':
                self_arg = 'Self'
                if in_func_body:
                    self_arg = 'evision_internal_structurise:from_struct(Self)'
            elif kind == 'gleam':
                self_arg = 'Self'
                if in_func_body:
                    self_arg = 'evision_internal_structurise:from_struct(Self)'
            else:
                print(f'warning: func_args: unknown kind `{kind}`')

            if len(func_args) > 0:
                func_args = f'{self_arg}, {func_args}'
            else:
                func_args = self_arg
        
            if len(func_args_with_opts) > 0:
                func_args_with_opts = f'{self_arg}, {func_args_with_opts}'

        return func_args, func_args_with_opts
