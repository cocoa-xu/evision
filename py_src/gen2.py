#!/usr/bin/env python3

from __future__ import print_function

import ast
import sys
from collections import namedtuple
from string import Template

import hdr_parser
import re

if sys.version_info[0] >= 3:
    from io import StringIO
else:
    from cStringIO import StringIO

evision_nif_prefix = 'evision_cv_'
special_handling_funcs = ["{}{}".format(evision_nif_prefix, name) for name in [
    'imshow',
    'waitKey',
    'destroyWindow',
    'destroyAllWindows',
    'imdecode']
]
opencv_ex_fixes = [
  """
  @doc namespace: :cv
  def imdecode(buf, flags) when is_integer(flags)
  do
    positional = [
      buf: buf,
      flags: flags
    ]
    :erl_cv_nif.evision_cv_imdecode(positional)
  end
  deferror imdecode(buf, flags)
"""
]

forbidden_arg_types = ["void*"]

ignored_arg_types = ["RNG*"]

pass_by_val_types = ["Point*", "Point2f*", "Rect*", "String*", "double*", "float*", "int*"]

gen_erl_cv_nif_load_nif = """
  @moduledoc false
  @on_load :load_nif
  def load_nif do
    require Logger
    nif_file = '#{:code.priv_dir(:evision)}/evision'
    :ok = 
      case :os.type() do
        {:win32, _} -> DLLLoaderHelper.addDLLDirectory("#{:code.priv_dir(:evision)}")
        _ -> :ok
      end

    case :erlang.load_nif(nif_file, 0) do
      :ok -> :ok
      {:error, {:reload, _}} -> :ok
      {:error, reason} -> Logger.warn("Failed to load nif: #{inspect(reason)}")
    end
  end
"""

enabled_modules_code = Template("""
  @doc \"\"\"
  return a list of enabled modules in this build
  \"\"\"
  def __enabled_modules__ do
    [${enabled_modules}]
  end
""")

gen_template_check_self = Template("""
    ERL_NIF_TERM self = argv[0];
    ${cname} * self1 = 0;
    if (!evision_${name}_getp(env, self, self1)) {
        return enif_make_badarg(env);
    }
    ${pname} _self_ = ${cvt}(self1);
""")

gen_template_safe_check_self = Template("""
    ERL_NIF_TERM self = argv[0];
    ${cname} self1;
    const ArgInfo selfArg("self", false);
    if (!evision_to_safe(env, self, self1, selfArg)) {
        return enif_make_badarg(env);
    }
    ${pname} _self_ = &self1;
""")

gen_template_simple_call_dnn_constructor_prelude = Template("""evision_res<$cname> * self = nullptr;
        alloc_resource(&self);
        if(self) """)

gen_template_simple_call_dnn_constructor = Template("""new (&(self->val)) ${cname}${py_args}""")

gen_template_call_constructor_prelude = Template("""evision_res<Ptr<$cname>> * self = nullptr;
        if (alloc_resource(&self)) {
            new (&(self->val)) Ptr<$cname>(); // init Ptr with placement new
        }
        if(self) """)

gen_template_call_constructor = Template("""self->val.reset(new ${cname}${py_args})""")

gen_template_simple_call_constructor_prelude = Template("""evision_res<$cname> * self = new evision_res<$cname>();\n    if(self) """)

gen_template_simple_call_constructor = Template("""new (&(self->val)) ${cname}${py_args}""")

gen_template_parse_args = Template("""// const char* keywords[] = { $kw_list, NULL }; // <- no more in use, left for debugging purpose
    if( $code_cvt )""")

gen_template_func_body = Template("""$code_decl
    $code_parse
    {
        int error_flag = false;
        ${code_prelude}ERRWRAP2($code_fcall, env, error_flag, error_term);
        if (!error_flag) {
            $code_ret;
        }
    }
""")

gen_template_mappable = Template("""
    {
        ${mappable} _src;
        if (evision_to_safe(env, src, _src, info))
        {
            return cv_mappable_to(_src, dst);
        }
    }
""")

gen_template_type_decl = Template("""
// Converter (${name})

template<>
struct Evision_Converter< ${cname} >
{
    static ERL_NIF_TERM from(ErlNifEnv *env, const ${cname}& r)
    {
        return evision_${name}_Instance(env, r);
    }
    static bool to(ErlNifEnv *env, ERL_NIF_TERM src, ${cname}& dst, const ArgInfo& info)
    {
        if(!src || evision::nif::check_nil(env, src))
            return true;
        ${cname} * dst_ = nullptr;
        if (evision_${name}_getp(env, src, dst_))
        {
            dst = *dst_;
            return true;
        }
        ${mappable_code}
        failmsg(env, "Expected ${cname} for argument '%s'", info.name);
        return false;
    }
};

""")

gen_template_map_type_cvt = Template("""
template<> bool evision_to(ErlNifEnv *env, ERL_NIF_TERM src, ${cname}& dst, const ArgInfo& info);

""")

gen_template_set_prop_from_map = Template("""
    if( enif_get_map_value(env, src, evision::nif::atom(env, "$propname"), &tmp) )
    {
        ok = evision_to_safe(env, tmp, dst.$propname, ArgInfo("$propname", false));
        if(!ok) return false;
    }""")

gen_template_type_impl = Template("""
// GetSet (${name})

${getset_code}

// Methods (${name})

${methods_code}
""")


gen_template_get_prop_ptr = Template("""
static ERL_NIF_TERM evision_${name}_get_${member}(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM self = argv[0];
    ${storage_name}* self1_ptr = 0;
    if (!evision_${name}_getp(env, self, self1_ptr) && !self1_ptr) {
        return enif_make_badarg(env);
    }
    
    ${storage_name} &self2 = *self1_ptr;
    $cname* _self_ = dynamic_cast<$cname*>(self2.get());
    return evision_from(env, _self_->${member});
}
""")

gen_template_get_prop = Template("""
static ERL_NIF_TERM evision_${name}_get_${member}(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM self = argv[0];
    ${storage_name}* self1 = 0;
    if (!evision_${name}_getp(env, self, self1) && !self1) {
        return enif_make_badarg(env);
    }
    
    return evision_from(env, self1${access}${member});
}
""")

gen_template_get_prop_algo = Template("""
static ERL_NIF_TERM evision_${name}_get_${member}(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM self = argv[0];
    ${storage_name}* self1 = 0;
    if (!evision_${name}_getp(env, self, self1)) {
        return enif_make_badarg(env);
    }
    
    $cname* _self_algo_ = dynamic_cast<$cname*>(self1->get());
    if (!_self_algo_)
        return failmsgp(env, "Incorrect type of object (must be '${name}' or its derivative)");
    return evision_from(env, _self_algo_${access}${member});
}
""")

gen_template_set_prop = Template("""
static ERL_NIF_TERM evision_${name}_set_${member}(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM self = argv[0];
    ${storage_name}* self1 = 0;
    if (!evision_${name}_getp(env, self, self1)) {
        return enif_make_badarg(env);
    }
    return evision::nif::atom(env, "not implemented setter");

    // if (!value)
    // {
    //     // todo: error("Cannot delete the ${member} attribute");
    //     return -1;
    // }
    // return evision_to_safe(env, value, p->val${access}${member}, ArgInfo("value", false)) ? 0 : -1;
}
""")

gen_template_set_prop_algo = Template("""
static ERL_NIF_TERM evision_${name}_set_${member}(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM self = argv[0];
    ${storage_name}* self1 = 0;
    if (!evision_${name}_getp(env, self, self1)) {
        return enif_make_badarg(env);
    }
    $cname* _self_algo_ = dynamic_cast<$cname*>(self1->get());
    if (!_self_algo_)
    {
        failmsgp(env, "Incorrect type of object (must be '${name}' or its derivative)");
        return -1;
    }
    if (evision_to_safe(env, argv[1], _self_algo_${access}${member}, ArgInfo("value", false))) {
        return evision::nif::ok(env, self);
    }
    return evision::nif::atom(env, "error");
}
""")


gen_template_prop_init = Template("""
    {(char*)"${member}", (getter)evision_${name}_get_${member}, NULL, (char*)"${member}", NULL},""")

gen_template_rw_prop_init = Template("""
    {(char*)"${member}", (getter)evision_${name}_get_${member}, (setter)evision_${name}_set_${member}, (char*)"${member}", NULL},""")

gen_template_overloaded_function_call = Template("""
    {
${variant}
    }
""")

class FormatStrings:
    string = 's'
    unsigned_char = 'b'
    short_int = 'h'
    int = 'i'
    unsigned_int = 'I'
    long = 'l'
    unsigned_long = 'k'
    long_long = 'L'
    unsigned_long_long = 'K'
    size_t = 'n'
    float = 'f'
    double = 'd'
    object = 'O'

ArgTypeInfo = namedtuple('ArgTypeInfo',
                        ['atype', 'format_str', 'default_value',
                         'strict_conversion', 'is_enum'])
# strict_conversion is False by default
ArgTypeInfo.__new__.__defaults__ = (False,)

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

# Set of reserved keywords for Python. Can be acquired via the following call
# $ python -c "help('keywords')"
# Keywords that are reserved in C/C++ are excluded because they can not be
# used as variables identifiers
python_reserved_keywords = {
    "True", "None", "False", "as", "assert", "def", "del", "elif", "except", "exec",
    "finally", "from", "global",  "import", "in", "is", "lambda", "nonlocal",
    "pass", "print", "raise", "with", "yield"
}


def normalize_class_name(name):
    return re.sub(r"^cv\.", "", name).replace(".", "_")


def get_type_format_string(arg_type_info):
    if arg_type_info.strict_conversion:
        return FormatStrings.object
    else:
        return arg_type_info.format_str


def argsort(seq):
    # http://stackoverflow.com/questions/3382352/equivalent-of-numpy-argsort-in-basic-python/3383106#3383106
    #lambda version by Tony Veijalainen
    return [x for x,y in sorted(enumerate(seq), key = lambda x: x[1])]


class ClassProp(object):
    def __init__(self, decl):
        self.tp = decl[0].replace("*", "_ptr")
        self.name = decl[1]
        self.readonly = True
        if "/RW" in decl[3]:
            self.readonly = False

class ClassInfo(object):
    def __init__(self, name, decl=None):
        self.cname = name.replace(".", "::")
        self.name = self.wname = normalize_class_name(name)
        self.sname = name[name.rfind('.') + 1:]
        self.ismap = False
        self.issimple = False
        self.isalgorithm = False
        self.methods = {}
        self.props = []
        self.mappables = []
        self.consts = {}
        self.base = None
        self.constructor = None
        customname = False

        if decl:
            bases = decl[1].split()[1:]
            if len(bases) > 1:
                print("Note: Class %s has more than 1 base class (not supported by Python C extensions)" % (self.name,))
                print("      Bases: ", " ".join(bases))
                print("      Only the first base class will be used")
                #return sys.exit(-1)
            elif len(bases) == 1:
                self.base = bases[0].strip(",")
                if self.base.startswith("cv::"):
                    self.base = self.base[4:]
                if self.base == "Algorithm":
                    self.isalgorithm = True
                self.base = self.base.replace("::", "_")

            for m in decl[2]:
                if m.startswith("="):
                    wname = m[1:]
                    npos = name.rfind('.')
                    if npos >= 0:
                        self.wname = normalize_class_name(name[:npos] + '.' + wname)
                    else:
                        self.wname = wname
                    customname = True
                elif m == "/Map":
                    self.ismap = True
                elif m == "/Simple":
                    self.issimple = True
            self.props = [ClassProp(p) for p in decl[3]]

        if not customname and self.wname.startswith("Cv"):
            self.wname = self.wname[2:]

    def gen_map_code(self, codegen):
        all_classes = codegen.classes
        code = "static bool evision_to(ErlNifEnv *env, ERL_NIF_TERM src, %s& dst, const ArgInfo& info)\n{\n    ERL_NIF_TERM tmp;\n    bool ok;\n" % (self.cname)
        code += "".join([gen_template_set_prop_from_map.substitute(propname=p.name,proptype=p.tp) for p in self.props])
        if self.base:
            code += "\n    return evision_to_safe(env, src, (%s&)dst, info);\n}\n" % all_classes[self.base].cname
        else:
            code += "\n    return true;\n}\n"
        return code

    def gen_erl_func_list(self, codegen):
        if self.ismap:
            return self.gen_map_code(codegen)

        sorted_props = [(p.name, p) for p in self.props]
        sorted_props.sort()

        methods = self.methods.copy()

        if self.base and (self.cname.startswith("cv::ml") or "Calibrate" in self.cname or "Feature2D" in self.base or "Matcher" in self.base):
            if self.base in codegen.classes:
                base_class = codegen.classes[self.base]
                for base_method_name in base_class.methods:
                    if base_method_name not in methods:
                        base_method = base_class.methods[base_method_name].__deepcopy__()
                        base_method.classname = self.name
                        methods[base_method_name] = base_method
                    else:
                        # print(self.cname, "overrides base method:", base_method_name)
                        _ = 0
        sorted_methods = list(methods.items())
        sorted_methods.sort()

        # generate functions for constructor
        if self.constructor is not None:
            module_file_writer, separated_ns = codegen.get_module_writer(
                self.name, wname=self.cname, name=self.name, is_ns=False)
            codegen.gen_erl_declaration(
                self.cname, self.name, self.constructor, module_file_writer,
                is_constructor=True, separated_ns=separated_ns)

        # generate functions for methods
        for mname, m in sorted_methods:
            codegen.code_ns_reg.write(m.get_tab_entry())
            module_file_writer, separated_ns = codegen.get_module_writer(
                self.name, wname=self.cname, name=mname, is_ns=False)
            codegen.gen_erl_declaration(
                self.cname, mname, m, module_file_writer,
                is_constructor=False, separated_ns=separated_ns)

        # generate functions for properties
        for pname, m in sorted_props:
            module_file_writer, separated_ns = codegen.get_module_writer(
                self.name, wname=self.cname, name=pname, is_ns=False)
            codegen.gen_erl_declaration(
                self.cname, pname, m, module_file_writer,
                is_constructor=False, is_prop=True, prop_class=self, separated_ns=separated_ns)

    def gen_code(self, codegen):
        all_classes = codegen.classes
        if self.ismap:
            return self.gen_map_code(codegen)
        getset_code = StringIO()
        getset_inits = StringIO()

        sorted_props = [(p.name, p) for p in self.props]
        sorted_props.sort()

        access_op = "->"
        if self.issimple:
            access_op = "."

        for pname, p in sorted_props:
            if self.isalgorithm:
                getset_code.write(gen_template_get_prop_algo.substitute(
                    name=self.name, cname=self.cname, member=pname, membertype=p.tp, access=access_op,
                    storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))
            else:
                if self.issimple:
                    getset_code.write(gen_template_get_prop.substitute(
                        name=self.name, member=pname, membertype=p.tp,
                        access='->' if self.issimple else '.', cname=self.cname,
                        storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))
                else:
                    getset_code.write(gen_template_get_prop_ptr.substitute(
                        name=self.name, member=pname, membertype=p.tp,
                        access='->' if self.issimple else '.', cname=self.cname,
                        storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))
            if p.readonly:
                getset_inits.write(gen_template_prop_init.substitute(
                    name=self.name, member=pname,
                    storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))
            else:
                if self.isalgorithm:
                    getset_code.write(gen_template_set_prop_algo.substitute(
                        name=self.name, cname=self.cname, member=pname,  membertype=p.tp, access=access_op,
                        storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))
                else:
                    getset_code.write(gen_template_set_prop.substitute(
                        name=self.name, member=pname, membertype=p.tp, access=access_op, cname=self.cname,
                        storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))
                getset_inits.write(gen_template_rw_prop_init.substitute(
                    name=self.name, member=pname,
                    storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))

        methods_code = StringIO()
        methods_inits = StringIO()

        methods = self.methods.copy()
        if self.base and (self.cname.startswith("cv::ml") or "Calibrate" in self.cname or "Feature2D" in self.base or "Matcher" in self.base or "Algorithm" in self.base):
            if self.base in codegen.classes:
                base_class = codegen.classes[self.base]
                for base_method_name in base_class.methods:
                    if base_method_name not in methods:
                        base_method = base_class.methods[base_method_name].__deepcopy__()
                        for v in base_method.variants:
                            v.base_classname = base_method.classname
                            v.from_base = True
                        base_method.classname = self.name
                        methods[base_method_name] = base_method
                    else:
                        # print(self.cname, "overrides base method:", base_method_name)
                        _ = 0
        sorted_methods = list(methods.items())
        sorted_methods.sort()

        if self.constructor is not None:
            methods_code.write(self.constructor.gen_code(codegen))

        for mname, m in sorted_methods:
            methods_code.write(m.gen_code(codegen))
            methods_inits.write(m.get_tab_entry())

        code = gen_template_type_impl.substitute(name=self.name, wname=self.wname, cname=self.cname,
            getset_code=getset_code.getvalue(), getset_inits=getset_inits.getvalue(),
            methods_code=methods_code.getvalue(), methods_inits=methods_inits.getvalue())

        return code

    def gen_def(self, codegen):
        all_classes = codegen.classes
        baseptr = "NoBase"
        if self.base and self.base in all_classes:
            baseptr = all_classes[self.base].name

        constructor_name = "0"
        if self.constructor is not None:
            constructor_name = self.constructor.get_wrapper_name()

        return "CV_ERL_TYPE({}, {}, {}, {}, {}, {});\n".format(
            self.wname,
            self.name,
            self.cname if self.issimple else "Ptr<{}>".format(self.cname),
            self.sname if self.issimple else "Ptr",
            baseptr,
            constructor_name
        )


def handle_ptr(tp):
    if tp.startswith('Ptr_'):
        tp = 'Ptr<' + "::".join(tp.split('_')[1:]) + '>'
    return tp


class ArgInfo(object):
    def __init__(self, arg_tuple):
        self.tp = handle_ptr(arg_tuple[0])
        self.name = arg_tuple[1]
        if self.name in python_reserved_keywords:
            self.name += "_"
        self.defval = arg_tuple[2]
        self.isarray = False
        self.arraylen = 0
        self.arraycvt = None
        self.inputarg = True
        self.outputarg = False
        self.returnarg = False
        self.isrvalueref = False
        for m in arg_tuple[3]:
            if m == "/O":
                self.inputarg = False
                self.outputarg = True
                self.returnarg = True
            elif m == "/IO":
                self.inputarg = True
                self.outputarg = True
                self.returnarg = True
            elif m.startswith("/A"):
                self.isarray = True
                self.arraylen = m[2:].strip()
            elif m.startswith("/CA"):
                self.isarray = True
                self.arraycvt = m[2:].strip()
            elif m == "/RRef":
                self.isrvalueref = True
        self.py_inputarg = False
        self.py_outputarg = False

    def isbig(self):
        return self.tp in ["Mat", "vector_Mat", "cuda::GpuMat", "GpuMat", "vector_GpuMat", "UMat", "vector_UMat"] # or self.tp.startswith("vector")

    def crepr(self):
        return "ArgInfo(\"%s\", %d)" % (self.name, self.outputarg)


class FuncVariant(object):
    def __init__(self, classname, name, decl, isconstructor, isphantom=False):
        self.classname = classname
        self.from_base = False
        self.base_classname = None
        self.name = self.wname = name
        self.isconstructor = isconstructor
        self.isphantom = isphantom

        self.decl = decl
        self.docstring = decl[5]

        self.rettype = decl[4] or handle_ptr(decl[1])
        if self.rettype == "void":
            self.rettype = ""
        self.args = []
        self.array_counters = {}
        for a in decl[3]:
            ainfo = ArgInfo(a)
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
            assert not a.tp in forbidden_arg_types, 'Forbidden type "{}" for argument "{}" in "{}" ("{}")'.format(a.tp, a.name, self.name, self.classname)
            if a.tp in ignored_arg_types:
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

    def get_wrapper_name(self):
        name = self.name
        if self.classname:
            classname = self.classname + "_"
            if "[" in name:
                name = "getelem"
        else:
            classname = ""

        if self.is_static:
            name += "_static"

        return "evision_" + self.namespace.replace('.', '_') + '_' + classname + name

    def get_wrapper_prototype(self):
        full_fname = self.get_wrapper_name()
        return "static ERL_NIF_TERM %s(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])" % (full_fname,), full_fname

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
        fname = self.get_wrapper_name()
        if fname in special_handling_funcs:
            return ""
        nif_function_decl = f'    F({fname}, {func_arity}),\n'
        return nif_function_decl

    def map_erl_argname(self, argname, ignore_upper_starting=False):
        reserved_keywords = ['end', 'fn']
        name = ""
        if argname in reserved_keywords:
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

    def gen_code(self, codegen):
        all_classes = codegen.classes
        proto, fname = self.get_wrapper_prototype()
        opt_arg_index = 0
        if self.classname and not self.is_static and not self.isconstructor:
            opt_arg_index = 1
        # special handling for these highgui functions
        if fname in special_handling_funcs:
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
                    check_self_templ = gen_template_check_self
                    if "dnn" in self.namespace and selfinfo.cname in ["cv::dnn::Net"]:
                        check_self_templ = gen_template_safe_check_self
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
            # add necessary conversions from Python objects to code_cvt_list,
            # form the function/method call,
            # for the list of type mappings
            for a_index, a in enumerate(v.args):
                if a.tp in ignored_arg_types:
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
                if tp in pass_by_val_types:
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
                    erl_term = "evision_get_kw(env, erl_terms, \"%s\")" % (self.map_erl_argname(a.name),)
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
                    templ_prelude = gen_template_simple_call_constructor_prelude
                    templ = gen_template_simple_call_constructor
                    if "cv::dnn::" in selfinfo.cname:
                        templ_prelude = gen_template_simple_call_dnn_constructor_prelude
                        templ = gen_template_simple_call_dnn_constructor
                else:
                    templ_prelude = gen_template_call_constructor_prelude
                    templ = gen_template_call_constructor

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
                code_parse = gen_template_parse_args.substitute(
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
                    code_ret = "ERL_NIF_TERM ret = enif_make_resource(env, self);\n        enif_release_resource(self);\n        return evision::nif::ok(env, ret);"
                else:
                    aname, argno = v.py_outlist[0]
                    if v.rettype == 'bool':
                        code_ret = "if (%s) {\n                return evision::nif::atom(env, \"ok\");\n            } else {\n                return evision::nif::atom(env, \"error\");\n            }" % (aname,)
                    elif v.rettype == 'Mat':
                        code_ret = f"ERL_NIF_TERM mat_ret = evision_from(env, {aname});\n" \
                                   "            if (enif_is_ref(env, mat_ret)) return evision::nif::ok(env, mat_ret);\n" \
                                   "            else return mat_ret;"
                    else:
                        code_ret = "return evision::nif::ok(env, evision_from(env, %s))" % (aname,)
            else:
                # there is more than 1 return parameter; form the tuple out of them
                n_tuple = len(v.py_outlist)
                evision_from_calls = ["evision_from(env, " + aname + ")" for aname, argno in v.py_outlist]
                if v.rettype == 'bool':
                    if n_tuple >= 10:
                        code_ret = "ERL_NIF_TERM arr[] = {%s};\n    if (retval) {\n                return evision::nif::ok(env, enif_make_tuple_from_array(env, arr, %d));\n            } else {\n                return evision::nif::atom(env, \"error\");\n            }" % \
                            (",\n        ".join(evision_from_calls[1:]), n_tuple-1)
                    elif (n_tuple-1) == 1:
                        code_ret = "if (retval) {\n                return evision::nif::ok(env, %s);\n            } else {\n                return evision::nif::atom(env, \"error\");\n            }" % \
                            (", ".join(evision_from_calls[1:]),)
                    else:
                        code_ret = "if (retval) {\n                return evision::nif::ok(env, enif_make_tuple%d(env, %s));\n            } else {\n                return evision::nif::atom(env, \"error\");\n            }" % \
                            (n_tuple-1, ", ".join(evision_from_calls[1:]))
                else:
                    if n_tuple >= 10:
                        code_ret = "ERL_NIF_TERM arr[] = {%s};\n    return evision::nif::ok(env, enif_make_tuple_from_array(env, arr, %d))" % \
                            (",\n        ".join(evision_from_calls), n_tuple)
                    else:
                        code_ret = "return evision::nif::ok(env, enif_make_tuple%d(env, %s))" % \
                            (n_tuple, ", ".join(evision_from_calls))

            all_code_variants.append(gen_template_func_body.substitute(code_decl=code_decl, code_parse=code_parse,
                                                                       code_prelude=code_prelude, code_fcall=code_fcall,
                                                                       code_ret=code_ret))

        if len(all_code_variants) == 1:
            # if the function/method has only 1 signature, then just put it
            code += all_code_variants[0]
        else:
            # try to execute each signature, add an interlude between function
            # calls to collect error from all conversions
            code += '    \n'.join(gen_template_overloaded_function_call.substitute(variant=v)
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


class Namespace(object):
    def __init__(self):
        self.funcs = {}
        self.consts = {}


class ErlEnumExpressionGenerator(ast.NodeVisitor):
    def __init__(self):
        self.expression = ''
        self.skip_this = False

    def generic_visit(self, node):
        if type(node) is ast.Expression:
            self.visit(node.body)
        elif type(node) is ast.Constant:
            self.expression = f'{node.value}'
        elif type(node) is ast.UnaryOp:
            op = ErlEnumExpressionGenerator()
            op.visit(node.op)
            operand = ErlEnumExpressionGenerator()
            operand.visit(node.operand)
            self.expression = op.expression.format(operand.expression)
        elif type(node) is ast.USub:
            self.expression = '-{}'
        elif type(node) is ast.BinOp:
            op = ErlEnumExpressionGenerator()
            op.visit(node.op)
            lhs = ErlEnumExpressionGenerator()
            lhs.visit(node.left)
            rhs = ErlEnumExpressionGenerator()
            rhs.visit(node.right)
            self.expression = op.expression.format(lhs.expression, rhs.expression)
        elif type(node) is ast.LShift:
            self.expression = 'bsl({}, {})'
        elif type(node) is ast.RShift:
            self.expression = 'bsr({}, {})'
        elif type(node) is ast.Name:
            if node.id[:3] == 'CV_':
                if node.id == 'CV_8U':
                    self.expression = '0'
                elif node.id == 'CV_8S':
                    self.expression = '1'
                elif node.id == 'CV_16U':
                    self.expression = '2'
                elif node.id == 'CV_16S':
                    self.expression = '3'
                elif node.id == 'CV_32S':
                    self.expression = '4'
                elif node.id == 'CV_32F':
                    self.expression = '5'
                elif node.id == 'CV_64F':
                    self.expression = '6'
                elif node.id == 'CV_16F':
                    self.expression = '7'
                elif node.id == 'CV_MAT_CONT_FLAG':
                    self.skip_this = True
                elif node.id == 'CV_SUBMAT_FLAG':
                    self.skip_this = True
                else:
                    print(type(node), node.id, "not handled yet")
                    import sys
                    sys.exit(1)
            else:
                self.expression = f'cv_{node.id}()'
        elif type(node) is ast.Mult:
            self.expression = '({} * {})'
        elif type(node) is ast.Add:
            self.expression = '({} + {})'
        elif type(node) is ast.Sub:
            self.expression = '({} - {})'
        elif type(node) is ast.BitAnd:
            self.expression = 'band({}, {})'
        elif type(node) is ast.Invert:
            self.expression = 'bnot({})'
        elif type(node) is ast.BitOr:
            self.expression = 'bor({}, {})'
        else:
            import sys
            if sys.version_info.minor < 8:
                if type(node) is ast.Num:
                    self.expression = f'{node.n}'
                else:
                    print(type(node), "not implemented yet")
                    sys.exit(1)
            else:
                print(type(node), "not implemented yet")
                sys.exit(1)


class PythonWrapperGenerator(object):
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
        self.code_include = StringIO()
        self.code_enums = StringIO()
        self.code_types = StringIO()
        self.code_funcs = StringIO()
        self.code_ns_reg = StringIO()
        self.erl_cv_nif = StringIO()
        self.opencv_ex = StringIO()
        self.opencv_func = StringIO()
        self.opencv_func.deferror = {}
        self.opencv_func.doc_written = {}
        self.opencv_modules = {}
        self.code_type_publish = StringIO()
        self.py_signatures = dict()
        self.class_idx = 0
        self.erl_cv_nif_names = dict()

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
            erl_const_name = self.map_erl_argname(erl_const_name, ignore_upper_starting=True)
            if self.enum_names.get(val, None) is not None:
                val = f'cv_{val}()'
            if self.enum_names.get(erl_const_name, None) is None:
                self.enum_names[erl_const_name] = val
                self.enum_names_io.write(f"  @doc type: :constants\n  def cv_{erl_const_name}, do: {val}\n")
            else:
                if self.enum_names[erl_const_name] != val:
                    erl_const_name = self.map_erl_argname(f'{module_name}_{erl_const_name}', ignore_upper_starting=True)
                    if self.enum_names.get(erl_const_name, None) is None:
                        self.enum_names[erl_const_name] = val
                        self.enum_names_io.write(f"  def cv_{erl_const_name}, do: {val}\n")
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
        argname = self.map_erl_argname(argname)
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

    def map_erl_argname(self, argname, ignore_upper_starting=False):
        reserved_keywords = ['end', 'fn']
        name = ""
        if argname in reserved_keywords:
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

    def gen_erl_declaration(self, wname, name, func, writer=None, is_ns=False, is_constructor=False, is_prop=False, prop_class=None, separated_ns=None):
        # functions in namespaces goes to 'erl_cv_nif.ex' and 'opencv_{module}.ex'
        # 'erl_cv_nif.ex' contains the declarations of all NIFs
        # 'opencv_{module}.ex' contains readable/friendly function names

        if is_prop:
            # wname => class
            # name  => prop name
            # func  => ClassProp
            func_name = "evision_" + prop_class.wname + '_get_' + name
            writer.write(f"  def get_{name}(self) do\n"
                         f"    :erl_cv_nif.{func_name}(self)\n"
                         "  end\n")
            name_arity = f'get_{name}!/1'
            if not writer.deferror.get(name_arity, False):
                writer.write(f"  deferror get_{name}(self)\n")
                writer.deferror[name_arity] = True

            self.erl_cv_nif.write(f'  def {func_name}(_self), do: :erlang.nif_error("{wname}::{name} getter not loaded")\n')
            self.code_ns_reg.write(f'    F({func_name}, 1),\n')
            if not func.readonly:
                func_name = "evision_" + prop_class.wname + '_set_' + name
                writer.write(f"  def set_{name}(self, opts) do\n    :erl_cv_nif.{func_name}(self, opts)\n  end\n")
                name_arity = f'set_{name}!/1'
                if not writer.deferror.get(name_arity, False):
                    writer.write(f"  deferror set_{name}(self, opts)\n")
                    writer.deferror[name_arity] = True

                self.erl_cv_nif.write(f'  def {func_name}(_self, _opts), do: :erlang.nif_error("{wname}::{name} setter not loaded")\n')
                self.code_ns_reg.write(f'    F({func_name}, 2),\n')
            return

        func_name = func.get_wrapper_name()
        if func_name in special_handling_funcs:
            return
        if len(func_name) > 0 and not ('a' <= func_name[0] <= 'z'):
            func_name = func_name.lower()

        if self.erl_cv_nif_names.get(func_name) != True:
            self.erl_cv_nif_names[func_name] = True
            nif_args = '_opts \\\\ []'
            if not is_ns and func.classname and not func.is_static and not is_constructor:
                nif_args = f'_self, {nif_args}'
            self.erl_cv_nif.write(f'  def {func_name}({nif_args}), do: :erlang.nif_error("{wname}::{name} not loaded")\n')

        if writer is None:
            return

        erl_signatures = []
        func_guards = []
        for i in range(len(func.variants)):
            pos_end = -func.variants[i].py_noptargs
            if pos_end == 0:
                pos_end = len(func.variants[i].py_arglist)
            func_guards.append(list(filter(lambda x: x != '', [self.map_argtype_to_guard(argname, argtype) for argname, _, argtype in func.variants[i].py_arglist[:pos_end]])))
            erl_signatures.append(''.join(filter(lambda x: x != '', [self.map_argtype_to_type(argtype) for _, _, argtype in func.variants[i].py_arglist[:pos_end]])))

        func_guards_len_desc = reversed(argsort([len(g) for g in func_guards]))
        unique_signatures = {}
        for i in func_guards_len_desc:
            i = int(i)
            sign = erl_signatures[i]
            func_guard = func_guards[i]
            current_func = func.variants[i]
            arglist = current_func.py_arglist
            noptargs = current_func.py_noptargs
            min_args = len(arglist) - noptargs
            has_opts = noptargs > 0
            pos_end = len(arglist) if not has_opts else -noptargs
            opt_args = ''
            opt_doc = ''
            prototype = f'    Python prototype (for reference): {current_func.py_prototype}'
            if has_opts:
                opt_args = 'opts' if min_args == 0 else ', opts'
                opt_doc = '\n'.join(['    @optional {}: {}'.format(arg_name, argtype) for (arg_name, _, argtype) in arglist[-noptargs:]])
                opt_doc += '\n'
            func_args = '{}'.format(", ".join(['{}'.format(self.map_erl_argname(arg_name)) for (arg_name, _, argtype) in arglist[:pos_end]]))
            func_args_with_opts = ''
            if has_opts:
                func_args_with_opts = '{}{}'.format(", ".join(['{}'.format(self.map_erl_argname(arg_name)) for (arg_name, _, argtype) in arglist[:pos_end]]), opt_args)
            module_func_name = func_name
            if is_ns:
                if module_func_name != f'{evision_nif_prefix}{name}':
                    module_func_name = module_func_name[len(evision_nif_prefix):]
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
                        if len(func_args_with_opts) > 0:
                            func_args_with_opts = f'self, {func_args_with_opts}'
                    else:
                        func_args = 'self'
                        func_args_with_opts = ''
            if is_ns and wname != 'cv':
                writer, _ = self.get_module_writer(wname, wname=wname, name=name, is_ns=is_ns)
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

            if len(module_func_name) > 0 and not ('a' <= module_func_name <= 'z'):
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

            if module_func_name == "readNet":
                read_net_var = arglist[0][0]
                module_func_name = f"readNet{read_net_var[0].upper()}{read_net_var[1:]}"
            if func_name.startswith(evision_nif_prefix + "dnn") and module_func_name == "forward":
                sign = evision_nif_prefix + "forward"
            if func_name.startswith(evision_nif_prefix + "dnn_dnn_net") and (module_func_name == "getLayerShapes" or module_func_name == "getLayersShapes"):
                sign = evision_nif_prefix + "dnn_dnn_net_" + module_func_name
            if unique_signatures.get(sign, None) is True:
                writer.write('\n'.join(["  # {}".format(line.strip()) for line in opt_doc.split("\n")]))
                writer.write(f'  # def {module_func_name}({func_args}) do\n  #   :erl_cv_nif.{func_name}({func_args})\n  # end\n')
                if len(func_args_with_opts) > 0:
                    writer.write(f'  # def {module_func_name}({func_args_with_opts}) do\n  #   :erl_cv_nif.{func_name}({func_args_with_opts})\n  # end\n')
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
                                    normalized_arg_name = self.map_erl_argname(arg_desc[1])
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
                if len(func_guard) > 0:
                    when_guard = ' when '
                    when_guard += ' and '.join(func_guard) + '\n  '

                opt_args = '' if not has_opts else ' ++ opts'
                module_func_args = func_args
                module_func_args_with_opts = func_args_with_opts
                positional = 'positional = [{}\n    ]'.format(",".join(['\n      {}: {}'.format(self.map_erl_argname(arg_name), self.map_erl_argname(arg_name)) for (arg_name, _, argtype) in arglist[:pos_end]]))
                func_args = f'positional'
                if len(func_args_with_opts) > 0:
                    func_args_with_opts = f'positional{opt_args}'
                if not is_ns and func.classname and not func.is_static and not is_constructor:
                    func_args = f'self, {func_args}'
                    if len(func_args_with_opts) > 0:
                        func_args_with_opts = f'self, {func_args_with_opts}'

                function_group = ""
                if len(func.namespace) > 0:
                    if func.namespace == "cv":
                        function_group = f"  @doc namespace: :cv\n"
                    else:
                        function_group = f"  @doc namespace: :\"{func.namespace}\"\n"

                # this could be better perhaps, but it is hurting my brain...
                if func_name.startswith(evision_nif_prefix + "dnn") and module_func_name == "forward":
                    inline_doc += function_group
                    writer.write(f'{inline_doc}  def {module_func_name}(self, opts \\\\ []) when is_list(opts) do\n    :erl_cv_nif.{func_name}(self, opts)\n  end\n')
                    name_arity = f'{module_func_name}!/2'
                    if not writer.deferror.get(name_arity, False):
                        writer.write(f"  deferror {module_func_name}(self, opts)\n")
                        writer.deferror[name_arity] = True
                    continue
                if func_name.startswith(evision_nif_prefix + "dnn_dnn_net") and (module_func_name == "getLayerShapes" or module_func_name == "getLayersShapes"):
                    inline_doc += function_group
                    writer.write(f'{inline_doc}  def {module_func_name}(self, opts \\\\ []) when is_list(opts) do\n    :erl_cv_nif.{func_name}(self, opts)\n  end\n')
                    name_arity = f'{module_func_name}!/2'
                    if not writer.deferror.get(name_arity, False):
                        writer.write(f"  deferror {module_func_name}(self, opts)\n")
                        writer.deferror[name_arity] = True
                    continue
                if len(func_args_with_opts) > 0:
                    inline_doc += function_group
                    when_guard_with_opts = when_guard
                    if len(when_guard_with_opts.strip()) > 0:
                        when_guard_with_opts = f' {when_guard_with_opts.strip()} and is_list(opts)\n  '
                    else:
                        when_guard_with_opts = ' when is_list(opts)\n  '
                    writer.write(f'{inline_doc}  def {module_func_name}({module_func_args_with_opts}){when_guard_with_opts}do\n'
                                 f'    {positional}\n'
                                 f'    :erl_cv_nif.{func_name}({func_args_with_opts})\n'
                                 '  end\n')
                    module_func_args_without_opts_defaults = module_func_args_with_opts.replace('\\\\ []', '').strip()
                    if len(module_func_args_without_opts_defaults) > 0:
                        name_arity = f'{module_func_name}!/{len(module_func_args_without_opts_defaults.split(","))}'
                    else:
                        name_arity = f'{module_func_name}!/0'
                    if not writer.deferror.get(name_arity, False):
                        writer.write(f"  deferror {module_func_name}({module_func_args_without_opts_defaults})\n")
                        writer.deferror[name_arity] = True
                writer.write(f'{function_group}  def {module_func_name}({module_func_args}){when_guard}do\n'
                             f'    {positional}\n'
                             f'    :erl_cv_nif.{func_name}({func_args})\n'
                             '  end\n')
                if len(module_func_args) > 0:
                    name_arity = f'{module_func_name}!/{len(module_func_args.split(","))}'
                else:
                    name_arity = f'{module_func_name}!/0'
                if not writer.deferror.get(name_arity, False):
                    writer.write(f'  deferror {module_func_name}({module_func_args})\n')
                    writer.deferror[name_arity] = True

    def gen_namespace(self):
        for ns_name in self.namespaces:
            ns = self.namespaces[ns_name]
            wname = normalize_class_name(ns_name)
            for name, func in sorted(ns.funcs.items()):
                self.gen_erl_declaration(wname, name, func, self.opencv_func, is_ns=True)
                self.code_ns_reg.write(func.get_tab_entry())
        from pathlib import Path
        modules_dir = Path(self.output_path) / 'modules'
        for module_text in modules_dir.glob('*.h'):
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
                            if len(parts) == 2:
                                self.code_ns_reg.write(f'    F({parts[0].strip()}, {parts[1].strip()}),\n')
                        elif line.startswith("// @evision nif: "):
                            line = line[len("// @evision nif: "):].strip()
                            self.erl_cv_nif.write(f'  {line}\n')

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
            if not name.endswith(".ex"):
                f.write("#include <erl_nif.h>\n")
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
            self.opencv_modules[opencv_module_file_name] = module_file_writer
            return module_file_writer, inner_ns

    def gen(self, srcfiles, output_path, erl_output_path):
        self.output_path = output_path
        self.clear()
        self.parser = hdr_parser.CppHeaderParser(generate_umat_decls=True, generate_gpumat_decls=True)
        self.erl_cv_nif.write('defmodule :erl_cv_nif do\n{}\n'.format(gen_erl_cv_nif_load_nif))
        self.opencv_ex.write('defmodule Evision do\n')
        self.opencv_ex.write('  use Bitwise\n')
        self.opencv_ex.write('  import Kernel, except: [apply: 2, apply: 3, min: 2, max: 2]\n')
        self.opencv_ex.write('  import Evision.Errorize\n')
        self.opencv_ex.deferror = {}
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
                self.code_types.write(gen_template_map_type_cvt.substitute(name=classinfo.name, cname=classinfo.cname))
            else:
                mappable_code = "\n".join([
                                      gen_template_mappable.substitute(cname=classinfo.cname, mappable=mappable)
                                          for mappable in classinfo.mappables])
                code = gen_template_type_decl.substitute(
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

        # end 'opencv.ex'
        self.opencv_ex.write(self.enum_names_io.getvalue())
        self.opencv_ex.write(self.opencv_func.getvalue())
        for fix in opencv_ex_fixes:
            self.opencv_ex.write(fix)
            self.opencv_ex.write("\n")
        self.opencv_ex.write(enabled_modules_code.substitute(
            enabled_modules=",".join([f'\n      "{m}"' for m in self.enabled_modules]))
        )
        self.opencv_ex.write('\nend\n')
        # end 'erl_cv_nif.ex'
        self.erl_cv_nif.write('\nend\n')

        # That's it. Now save all the files
        self.save(output_path, "evision_generated_include.h", self.code_include)
        self.save(output_path, "evision_generated_funcs.h", self.code_funcs)
        self.save(output_path, "evision_generated_enums.h", self.code_enums)
        self.save(output_path, "evision_generated_types.h", self.code_type_publish)
        self.save(output_path, "evision_generated_types_content.h", self.code_types)
        self.save(output_path, "evision_generated_modules_content.h", self.code_ns_reg)
        self.save(erl_output_path, "erl_cv_nif.ex", self.erl_cv_nif)
        self.save(erl_output_path, "opencv.ex", self.opencv_ex)
        for name in self.opencv_modules:
            writer = self.opencv_modules[name]
            writer.write('\nend\n')
            self.save(erl_output_path, f"opencv_{name.lower()}.ex", writer)


if __name__ == "__main__":
    srcfiles = hdr_parser.opencv_hdr_list
    dstdir = "./c_src"
    erl_dstdir = "./lib"
    if len(sys.argv) > 1:
        dstdir = sys.argv[1]
    if len(sys.argv) > 2:
        erl_dstdir = sys.argv[2]
    if len(sys.argv) > 3:
        with open(sys.argv[3], 'r') as f:
            srcfiles = [l.strip() for l in f.readlines()]
    # default
    enabled_modules = ['calib3d', 'core', 'features2d', 'flann', 'highgui', 'imgcodecs', 'imgproc', 'ml', 'photo',
                       'stitching', 'ts', 'video', 'videoio', 'dnn']
    if len(sys.argv) > 4:
        enabled_modules = sys.argv[4].split(",")
    generator = PythonWrapperGenerator(enabled_modules)
    generator.gen(srcfiles, dstdir, erl_dstdir)
    # for n in generator.namespaces:
    #     print(f'"{n}": &(&1[:namespace] == :"{n}"),')
