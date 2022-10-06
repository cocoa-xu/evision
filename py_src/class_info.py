#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from helper import normalize_class_name
from class_prop import ClassProp
import evision_templates as ET
import sys

if sys.version_info[0] >= 3:
    from io import StringIO
else:
    from cStringIO import StringIO


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
        code += "".join([ET.gen_template_set_prop_from_map.substitute(propname=p.name,proptype=p.tp) for p in self.props])
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

        base_class = self.base
        current_class = self
        while base_class is not None:
            if base_class and (current_class.cname.startswith("cv::ml") or "Calibrate" in current_class.cname or (current_class.base is not None and "Feature2D" in current_class.base) or (current_class.base is not None and "Matcher" in current_class.base)):
                if base_class in codegen.classes:
                    base_class = codegen.classes[current_class.base]
                    for base_method_name in base_class.methods:
                        if base_method_name not in methods:
                            base_method = base_class.methods[base_method_name].__deepcopy__()
                            base_method.classname = self.name
                            methods[base_method_name] = base_method
                        else:
                            # print(self.cname, "overrides base method:", base_method_name)
                            _ = 0
                    base_class, current_class = current_class.base, base_class
                else:
                    break
            else:
                break

        sorted_methods = list(methods.items())
        sorted_methods.sort()

        # generate functions for constructor
        if self.constructor is not None:
            module_file_generator, separated_ns = codegen.get_module_writer(
                self.name, wname=self.cname, name=self.name, is_ns=False)
            module_file_generator.gen_constructor(self.cname, self.name, self.constructor, separated_ns)

        # generate functions for methods
        for mname, m in sorted_methods:
            codegen.code_ns_reg.write(m.get_tab_entry())
            module_file_generator, separated_ns = codegen.get_module_writer(
                self.name, wname=self.cname, name=mname, is_ns=False)
            module_file_generator.gen_method(self.cname, mname, m, separated_ns)

        # generate functions for properties
        for pname, m in sorted_props:
            module_file_generator, separated_ns = codegen.get_module_writer(
                self.name, wname=self.cname, name=pname, is_ns=False)
            module_file_generator.gen_property(self.cname, self.name, pname, m)

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
                getset_code.write(ET.gen_template_get_prop_algo.substitute(
                    name=self.name, cname=self.cname, member=pname, membertype=p.tp, access=access_op,
                    storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))
            else:
                if self.issimple:
                    getset_code.write(ET.gen_template_get_prop.substitute(
                        name=self.name, member=pname, membertype=p.tp,
                        access='->' if self.issimple else '.', cname=self.cname,
                        storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))
                else:
                    getset_code.write(ET.gen_template_get_prop_ptr.substitute(
                        name=self.name, member=pname, membertype=p.tp,
                        access='->' if self.issimple else '.', cname=self.cname,
                        storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))
            if p.readonly:
                getset_inits.write(ET.gen_template_prop_init.substitute(
                    name=self.name, member=pname,
                    storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))
            else:
                if self.isalgorithm:
                    getset_code.write(ET.gen_template_set_prop_algo.substitute(
                        name=self.name, cname=self.cname, member=pname,  membertype=p.tp, access=access_op,
                        storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))
                else:
                    getset_code.write(ET.gen_template_set_prop.substitute(
                        name=self.name, member=pname, membertype=p.tp, access=access_op, cname=self.cname,
                        storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))
                getset_inits.write(ET.gen_template_rw_prop_init.substitute(
                    name=self.name, member=pname,
                    storage_name=self.cname if self.issimple else "Ptr<{}>".format(self.cname)))

        methods_code = StringIO()
        methods_inits = StringIO()

        methods = self.methods.copy()
        base_class = self.base
        current_class = self
        while base_class is not None:
            if base_class and (current_class.cname.startswith("cv::ml") or "Calibrate" in current_class.cname or (current_class.base is not None and "Feature2D" in current_class.base) or (current_class.base is not None and "Matcher" in current_class.base) or (current_class.base is not None and "Algorithm" in current_class.base)):
                if base_class in codegen.classes:
                    base_class = codegen.classes[current_class.base]
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
                    base_class, current_class = current_class.base, base_class
                else:
                    break
            else:
                break

        sorted_methods = list(methods.items())
        sorted_methods.sort()

        if self.constructor is not None:
            methods_code.write(self.constructor.gen_code(codegen))

        for mname, m in sorted_methods:
            methods_code.write(m.gen_code(codegen))
            methods_inits.write(m.get_tab_entry())

        code = ET.gen_template_type_impl.substitute(name=self.name, wname=self.wname, cname=self.cname,
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
            constructor_name = self.constructor.get_wrapper_name(True)

        return "CV_ERL_TYPE({}, {}, {}, {}, {}, {});\n".format(
            self.wname,
            self.name,
            self.cname if self.issimple else "Ptr<{}>".format(self.cname),
            self.sname if self.issimple else "Ptr",
            baseptr,
            constructor_name
        )
