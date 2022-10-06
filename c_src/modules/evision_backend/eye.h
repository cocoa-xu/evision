#ifndef EVISION_BACKEND_EYE_H
#define EVISION_BACKEND_EYE_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_eye,evision_cv_mat_eye,1
// @evision nif: def mat_eye(_opts \\ []), do: :erlang.nif_error("Mat::eye not loaded")
static ERL_NIF_TERM evision_cv_mat_eye(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        std::string t;
        int l = 0;
        uint64_t n = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "n"), n, ArgInfo("n", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type = 0;
            if (!get_binary_type(t, l, 1, type)) return evision::nif::error(env, "not implemented for the given type");

            Mat ret;
            int error_flag = false;
            ERRWRAP2(ret = Mat::eye(n, n, type), env, error_flag, error_term);
            if (!error_flag) {
                return evision::nif::ok(env, evision_from(env, ret));
            } else {
                return error_term;
            }
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_EYE_H
