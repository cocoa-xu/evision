#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from string import Template

from cv_depths import CV_CN_SHIFT, CV_DEPTHS


# CV_* type macros
_BEAM_DEPTHS = [(name[len('CV_'):], value) for name, value in CV_DEPTHS.items()]
_CHANNELS = (1, 2, 3, 4)


def _gen_cv_types_erlang():
    depths = '\n'.join(f'cv_{depth}() -> {value}.' for depth, value in _BEAM_DEPTHS)
    makers = '\n\n'.join(
        f'cv_{depth}C(Cn) ->\n    cv_maketype(cv_{depth}(), Cn).\n'
        + '\n'.join(f'cv_{depth}C{n}() ->\n    cv_{depth}C({n}).' for n in _CHANNELS)
        for depth, _ in _BEAM_DEPTHS
    )
    return f"""
{depths}

cv_cn_shift() -> {CV_CN_SHIFT}.
cv_depth_max() ->
    1 bsl cv_cn_shift().
cv_mat_depth_mask() ->
    cv_depth_max() - 1.
cv_maketype(Depth, Cn) ->
    (Depth band cv_mat_depth_mask()) + ((Cn-1) bsl cv_cn_shift()).

{makers}
"""


def _gen_cv_types_elixir():
    depths = '\n'.join(f'  def cv_{depth}, do: {value}' for depth, value in _BEAM_DEPTHS)
    makers = '\n\n'.join(
        f'  def cv_{depth}C(cn), do: cv_maketype(cv_{depth}(), cn)\n'
        + '\n'.join(f'  def cv_{depth}C{n}, do: cv_{depth}C({n})' for n in _CHANNELS)
        for depth, _ in _BEAM_DEPTHS
    )
    return f"""
{depths}

  def cv_cn_shift, do: {CV_CN_SHIFT}
  def cv_depth_max, do: 1 <<< cv_cn_shift()
  def cv_mat_depth_mask, do: cv_depth_max() - 1
  def cv_maketype(depth, cn), do: (depth &&& cv_mat_depth_mask()) + ((cn - 1) <<< cv_cn_shift())

{makers}
"""


gen_cv_types_erlang = _gen_cv_types_erlang()
gen_cv_types_elixir = _gen_cv_types_elixir()

# template for Elixir NIF
gen_evision_nif_load_nif = """
  @moduledoc false
  @on_load :load_nif
  def load_nif do
    require Logger
    nif_file = ~c"#{:code.priv_dir(:evision)}/evision"
    case :os.type() do
      {:win32, _} ->
        case :evision_windows_fix.run_once() do
          :ok -> :ok
          {:error, reason} -> Logger.warning("Failed to run windows fix: #{inspect(reason)}")
        end
      _ ->
        :ok
    end

    case :erlang.load_nif(nif_file, 0) do
      :ok -> :ok
      {:error, {:reload, _}} -> :ok
      {:error, reason} ->
        Logger.warning("Failed to load nif: #{inspect(reason)}")
        case :os.type() do
          {:win32, _} ->
            case :erlang.load_nif("#{nif_file}.dll", 0) do
              :ok -> :ok
              {:error, {:reload, _}} -> :ok
              {:error, reason} -> Logger.warning("Failed to load nif: #{inspect(reason)}")
            end
          _ ->
            {:error, reason}
        end
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
        {win32, _} -> evision_windows_fix:run_once();
        _ -> ok
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
                    return binary_erl_term;
                } else {
                    return evision::nif::error(env, \"out of memory\");
                }
            } else {
                return evision::nif::atom(env, \"false\");
            }"""

code_ret_1_tuple_except_bool = """if (retval) {
                // code_ret_1_tuple_except_bool
                return %s;
            } else {
                return evision::nif::atom(env, \"false\");
            }"""

code_ret_2_to_10_tuple_except_bool = """if (retval) {
                // code_ret_2_to_10_tuple_except_bool
                return enif_make_tuple%d(env, %s);
            } else {
                return evision::nif::atom(env, \"false\");
            }"""

code_ret_ge_10_tuple_except_bool = """ERL_NIF_TERM arr[] = {%s};
            // code_ret_ge_10_tuple_except_bool
            if (retval) {
                return enif_make_tuple_from_array(env, arr, %d);
            } else {
                return evision::nif::atom(env, \"false\");
            }"""

code_ret_lt_10_tuple = "return enif_make_tuple%d(env, %s)"

code_ret_ge_10_tuple = """ERL_NIF_TERM arr[] = {%s};
            return enif_make_tuple_from_array(env, arr, %d)"""

code_ret_constructor = """ERL_NIF_TERM ret = enif_make_resource(env, self);
            enif_release_resource(self);
            bool success;
            return evision_from_as_map<%s>(env, self->val, ret, "Elixir.Evision.%s", success);"""

code_ret_dnn_setter = Template("""bool success;
            return evision_from_as_map<${storage_name} *>(env, _self_, self, "Elixir.Evision.${elixir_module_name}", success)""")

elixir_property_getter = Template("""  @spec get_${property_name}(${self_spec}) :: ${prop_spec}
  def get_${property_name}(self) do
    :evision_nif.${nif_name}(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
""")

erlang_property_getter = Template("""-spec get_${property_name}(${self_spec}) -> ${prop_spec}.
get_${property_name}(Self) ->
    SelfRef = evision_internal_structurise:from_struct(Self),
    Ret = evision_nif:${nif_name}(SelfRef),
    to_struct(Ret).
""")

elixir_property_setter = Template("""  @spec set_${property_name}(${self_spec_in}, ${prop_spec}) :: ${self_spec_out}
  def set_${property_name}(self, prop) do
    :evision_nif.${nif_name}(
        Evision.Internal.Structurise.from_struct(self),
        [${property_name}: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
""")

erlang_property_setter = Template("""-spec set_${property_name}(${self_spec_in}, ${prop_spec}) -> ${self_spec_out}.
set_${property_name}(Self, Prop) ->
    SelfRef = evision_internal_structurise:from_struct(Self),
    PropRef = evision_internal_structurise:from_struct(Prop),
    Ret = evision_nif:${nif_name}(SelfRef, [{${property_name}, PropRef}]),
    to_struct(Ret).
""")

# template for `Evision.enabled_modules/0`
enabled_modules_code = Template("""
  @doc \"\"\"
  return a list of enabled modules in this build
  \"\"\"
  def enabled_modules do
    :evision_nif.enabled_modules()
  end
""")

# template for `evision:enabled_modules/0`
enabled_modules_code_erlang = Template("""
enabled_modules() ->
    evision_nif:enabled_modules().
""")

gen_template_check_self = Template("""
    ERL_NIF_TERM self = argv[0];
    ${cname} * self1 = 0;
    if (!evision_${name}_getp(env, self, self1)) {
        return failmsgp(env, "cannot get `${cname}` from `self`: mismatched type or invalid resource?");
    }
    ${pname} _self_ = ${cvt}(self1);
""")

gen_template_safe_check_self = Template("""
    ERL_NIF_TERM self = argv[0];
    ${cname} self1;
    const ArgInfo selfArg("self", 0);
    if (!evision_to_safe(env, self, self1, selfArg)) {
        return failmsgp(env, "cannot get `${cname}` from `self`: mismatched type or invalid resource?");
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

gen_template_simple_call_constructor_prelude = Template("""evision_res<$cname> * self = nullptr;
        if (alloc_resource(&self)) """)

gen_template_simple_call_constructor = Template("""new (&(self->val)) ${cname}${py_args}""")

gen_template_parse_args = Template("if( $code_cvt )")

gen_template_func_body = Template("""$code_decl
    $code_parse
    {
        error_flag = false;
        $code_from_ptr
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
        if (evision::nif::check_nil(env, src)) {
            if (info.outputarg) return true;
            return info.has_default;
        }

        ${cname} * dst_ = nullptr;
        if (evision_${name}_getp(env, src, dst_))
        {
            dst = *dst_;
            return true;
        }
        ${mappable_code}
        if (info.has_default) return true;

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
        ok = evision_to_safe(env, tmp, dst.$propname, ArgInfo("$propname", 0));
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
    ${storage_name}* self_ptr = 0;
    if (!evision_${name}_getp(env, self, self_ptr) && !self_ptr) {
        return failmsgp(env, "cannot get `${storage_name}` from `self`: mismatched type or invalid resource?");
    }

    ${storage_name} &self2 = *self_ptr;
    $cname* _self_ = dynamic_cast<$cname*>(self2.get());
    return evision_from(env, _self_->${member});
}
""")

gen_template_get_prop = Template("""
static ERL_NIF_TERM evision_${name}_get_${member}(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM self = argv[0];
    ${storage_name}* self_ptr = 0;
    if (!evision_${name}_getp(env, self, self_ptr) && !self_ptr) {
        return failmsgp(env, "cannot get `${storage_name}` from `self`: mismatched type or invalid resource?");
    }

    return evision_from(env, self_ptr${access}${member});
}
""")

gen_template_get_prop_algo = Template("""
static ERL_NIF_TERM evision_${name}_get_${member}(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM self = argv[0];
    ${storage_name}* self_ptr = 0;
    if (!evision_${name}_getp(env, self, self_ptr) && !self_ptr) {
        return failmsgp(env, "cannot get `${storage_name}` from `self`: mismatched type or invalid resource?");
    }

    $cname* _self_algo_ = dynamic_cast<$cname*>(self_ptr->get());
    if (!_self_algo_) {
        return failmsgp(env, "Incorrect type of object (must be '${name}' or its derivative)");
    }

    return evision_from(env, _self_algo_${access}${member});
}
""")

gen_template_set_prop = Template("""
static ERL_NIF_TERM evision_${name}_set_${member}(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM self = argv[0];
    ${storage_name}* self_ptr = 0;

    if (!evision_${name}_getp(env, self, self_ptr) && !self_ptr) {
        return failmsgp(env, "cannot get `${storage_name}` from `self`: mismatched type or invalid resource?");
    }

    if (evision_to_safe(env, argv[1], self_ptr->${member}, ArgInfo("${member}", 0))) {
        bool success;
        return evision_from_as_map<${storage_name}>(env, *self_ptr, self, "Elixir.Evision.${elixir_module_name}", success);
    }

    return failmsgp(env, "cannot assign new value, mismatched type?");
}
""")

gen_template_set_prop_cv_ptr = Template("""
static ERL_NIF_TERM evision_${name}_set_${member}(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM self = argv[0];
    ${storage_name}* self_ptr = 0;

    if (!evision_${name}_getp(env, self, self_ptr) && !self_ptr) {
        return failmsgp(env, "cannot get `${storage_name}` from `self`: mismatched type or invalid resource?");
    }

    ${storage_name} &_self_ = *self_ptr;

    std::map<std::string, ERL_NIF_TERM> erl_terms;
    evision::nif::parse_arg(env, 1, argv, erl_terms);

    if (evision_to_safe(env, evision_get_kw(env, erl_terms, "${member}"), _self_${access}${member}, ArgInfo("${member}", 0))) {
        bool success;
        return evision_from_as_map<${storage_name}>(env, _self_, self, "Elixir.Evision.${elixir_module_name}", success);
    }

    return failmsgp(env, "cannot assign new value, mismatched type?");
}
""")

gen_template_set_prop_algo = Template("""
static ERL_NIF_TERM evision_${name}_set_${member}(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM self = argv[0];
    ${storage_name}* self_ptr = 0;
    if (!evision_${name}_getp(env, self, self_ptr) && !self_ptr) {
        return failmsgp(env, "cannot get `${storage_name}` from `self`: mismatched type or invalid resource?");
    }

    $cname* _self_algo_ = dynamic_cast<$cname*>(self_ptr->get());
    if (!_self_algo_) {
        return failmsgp(env, "Incorrect type of object (must be '${name}' or its derivative)");
    }

    if (evision_to_safe(env, argv[1], _self_algo_${access}${member}, ArgInfo("${member}", 0))) {
        bool success;
        return evision_from_as_map<${storage_name}>(env, *self_ptr, self, "Elixir.Evision.${elixir_module_name}", success);
    }

    return failmsgp(env, "cannot assign new value, mismatched type?");
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
