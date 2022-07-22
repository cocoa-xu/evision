#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from string import Template


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
