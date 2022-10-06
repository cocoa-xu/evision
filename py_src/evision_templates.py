#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from string import Template


# CV_* type macros
gen_cv_types_erlang = """
cv_8U() -> 0.
cv_8S() -> 1.
cv_16U() -> 2.
cv_16S() -> 3.
cv_32S() -> 4.
cv_32F() -> 5.
cv_64F() -> 6.
cv_16F() -> 7.

cv_cn_shift() -> 3.
cv_depth_max() ->
    1 bsl cv_cn_shift().
cv_mat_depth_mask() ->
    cv_depth_max() - 1.
cv_maketype(Depth, Cn) ->
    (Depth band cv_mat_depth_mask()) + ((Cn-1) bsl cv_cn_shift()).

cv_8UC(Cn) ->
    cv_maketype(cv_8U(), Cn).
cv_8UC1() ->
    cv_8UC(1).
cv_8UC2() ->
    cv_8UC(2).
cv_8UC3() ->
    cv_8UC(3).
cv_8UC4() ->
    cv_8UC(4).

cv_8SC(Cn) ->
    cv_maketype(cv_8S(), Cn).
cv_8SC1() ->
    cv_8SC(1).
cv_8SC2() ->
    cv_8SC(2).
cv_8SC3() ->
    cv_8SC(3).
cv_8SC4() ->
    cv_8SC(4).

cv_16UC(Cn) ->
    cv_maketype(cv_16U(), Cn).
cv_16SU1() ->
    cv_16UC(1).
cv_16UC2() ->
    cv_16UC(2).
cv_16UC3() ->
    cv_16UC(3).
cv_16UC4() ->
    cv_16UC(4).

cv_16SC(Cn) ->
    cv_maketype(cv_16S(), Cn).
cv_16SC1() ->
    cv_16SC(1).
cv_16SC2() ->
    cv_16SC(2).
cv_16SC3() ->
    cv_16SC(3).
cv_16SC4() ->
    cv_16SC(4).

cv_32SC(Cn) ->
    cv_maketype(cv_32S(), Cn).
cv_32SC1() ->
    cv_32SC(1).
cv_32SC2() ->
    cv_32SC(2).
cv_32SC3() ->
    cv_32SC(3).
cv_32SC4() ->
    cv_32SC(4).

cv_32FC(Cn) ->
    cv_maketype(cv_32F(), Cn).
cv_32FC1() ->
    cv_32FC(1).
cv_32FC2() ->
    cv_32FC(2).
cv_32FC3() ->
    cv_32FC(3).
cv_32FC4() ->
    cv_32FC(4).

cv_64FC(Cn) ->
    cv_maketype(cv_64F(), Cn).
cv_64FC1() ->
    cv_64FC(1).
cv_64FC2() ->
    cv_64FC(2).
cv_64FC3() ->
    cv_64FC(3).
cv_64FC4() ->
    cv_64FC(4).

cv_16FC(Cn) ->
    cv_maketype(cv_16F(), Cn).
cv_16FC1() ->
    cv_16FC(1).
cv_16FC2() ->
    cv_16FC(2).
cv_16FC3() ->
    cv_16FC(3).
cv_16FC4() ->
    cv_16FC(4).
"""

gen_cv_types_elixir = """
  def cv_8U, do: 0
  def cv_8S, do: 1
  def cv_16U, do: 2
  def cv_16S, do: 3
  def cv_32S, do: 4
  def cv_32F, do: 5
  def cv_64F, do: 6
  def cv_16F, do: 7

  def cv_cn_shift, do: 3
  def cv_depth_max, do: 1 <<< cv_cn_shift()
  def cv_mat_depth_mask, do: cv_depth_max() - 1
  def cv_maketype(depth, cn), do: (depth &&& cv_mat_depth_mask()) + ((cn - 1) <<< cv_cn_shift())

  def cv_8UC(cn), do: cv_maketype(cv_8U(), cn)
  def cv_8UC1, do: cv_8UC(1)
  def cv_8UC2, do: cv_8UC(2)
  def cv_8UC3, do: cv_8UC(3)
  def cv_8UC4, do: cv_8UC(4)

  def cv_8SC(cn), do: cv_maketype(cv_8S(), cn)
  def cv_8SC1, do: cv_8SC(1)
  def cv_8SC2, do: cv_8SC(2)
  def cv_8SC3, do: cv_8SC(3)
  def cv_8SC4, do: cv_8SC(4)

  def cv_16UC(cn), do: cv_maketype(cv_16U(), cn)
  def cv_16UC1, do: cv_16UC(1)
  def cv_16UC2, do: cv_16UC(2)
  def cv_16UC3, do: cv_16UC(3)
  def cv_16UC4, do: cv_16UC(4)

  def cv_16SC(cn), do: cv_maketype(cv_16S(), cn)
  def cv_16SC1, do: cv_16SC(1)
  def cv_16SC2, do: cv_16SC(2)
  def cv_16SC3, do: cv_16SC(3)
  def cv_16SC4, do: cv_16SC(4)

  def cv_32SC(cn), do: cv_maketype(cv_32S(), cn)
  def cv_32SC1, do: cv_32SC(1)
  def cv_32SC2, do: cv_32SC(2)
  def cv_32SC3, do: cv_32SC(3)
  def cv_32SC4, do: cv_32SC(4)

  def cv_32FC(cn), do: cv_maketype(cv_32F(), cn)
  def cv_32FC1, do: cv_32FC(1)
  def cv_32FC2, do: cv_32FC(2)
  def cv_32FC3, do: cv_32FC(3)
  def cv_32FC4, do: cv_32FC(4)

  def cv_64FC(cn), do: cv_maketype(cv_64F(), cn)
  def cv_64FC1, do: cv_64FC(1)
  def cv_64FC2, do: cv_64FC(2)
  def cv_64FC3, do: cv_64FC(3)
  def cv_64FC4, do: cv_64FC(4)

  def cv_16FC(cn), do: cv_maketype(cv_16F(), cn)
  def cv_16FC1, do: cv_16FC(1)
  def cv_16FC2, do: cv_16FC(2)
  def cv_16FC3, do: cv_16FC(3)
  def cv_16FC4, do: cv_16FC(4)
"""

# template for Elixir NIF
gen_evision_nif_load_nif = """
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

# template for Erlang NIF
gen_evision_nif_load_nif_erlang = """
-on_load(init/0).

-define(APPNAME, evision).
-define(LIBNAME, evision).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    case os:type() of
        {win32, _} ->
            dll_loader_helper:add_dll_directory(filename:dirname(SoName));
        _ -> true
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
"""

# code ret as binary
code_ret_as_binary = """if (retval) {
                bool success = false;
                ERL_NIF_TERM binary_erl_term = %s;
                if (success) {
                    return evision::nif::ok(env, binary_erl_term);
                } else {
                    return evision::nif::error(env, \"out of memory\");
                }
            } else {
                return evision::nif::atom(env, \"error\");
            }"""

code_ret_1_tuple_except_bool = """if (retval) {
                return evision::nif::ok(env, %s);
            } else {
                return evision::nif::atom(env, \"error\");
            }"""

code_ret_2_to_10_tuple_except_bool = """if (retval) {
                return evision::nif::ok(env, enif_make_tuple%d(env, %s));
            } else {
                return evision::nif::atom(env, \"error\");
            }"""

code_ret_ge_10_tuple_except_bool = """ERL_NIF_TERM arr[] = {%s};
            if (retval234) {
                return evision::nif::ok(env, enif_make_tuple_from_array(env, arr, %d));
            } else {
                return evision::nif::atom(env, \"error\");
            }"""

code_ret_lt_10_tuple = "return evision::nif::ok(env, enif_make_tuple%d(env, %s))"

code_ret_ge_10_tuple = """ERL_NIF_TERM arr[] = {%s};
            return evision::nif::ok(env, enif_make_tuple_from_array(env, arr, %d))"""

code_ret_constructor = """ERL_NIF_TERM ret = enif_make_resource(env, self);
        enif_release_resource(self);
        return evision::nif::ok(env, ret);"""

code_ret_constructor_structurise = """ERL_NIF_TERM ret = enif_make_resource(env, self);
        enif_release_resource(self);
        return evision::nif::ok(env, evision_from_as_map<Ptr<%s>>(env, self->val, ret));"""

elixir_property_getter = Template("""  def get_${property_name}(self) do
    :evision_nif.${nif_name}(Evision.Internal.Structurise.from_struct(self))
    |> Evision.Internal.Structurise.to_struct()
  end
""")

erlang_property_getter = Template("""get_${property_name}(Self) ->
    evision_nif:${nif_name}(Self).

""")

elixir_property_setter = Template("""  def set_${property_name}(self, prop) do
    :evision_nif.${nif_name}(Evision.Internal.Structurise.from_struct(self), [${property_name}: Evision.Internal.Structurise.from_struct(prop)])
    |> Evision.Internal.Structurise.to_struct()
  end
""")

erlang_property_setter = Template("""set_${property_name}(Self, Prop) ->
    evision_nif:${nif_name}(Self, [{${property_name}, Prop}]).

""")

# template for `Evision.__enabled_modules__/0`
enabled_modules_code = Template("""
  @doc \"\"\"
  return a list of enabled modules in this build
  \"\"\"
  def __enabled_modules__ do
    [${enabled_modules}]
  end
""")

# template for `evision:enabled_modules/0`
enabled_modules_code_erlang = Template("""
enabled_modules() ->
    [${enabled_modules}].
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
