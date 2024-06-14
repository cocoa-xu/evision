#ifndef EVISION_BACKEND_MATRIX_MULTIPLY_H
#define EVISION_BACKEND_MATRIX_MULTIPLY_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_matrix_multiply, evision_cv_mat_matrix_multiply, 1
// @evision nif: def mat_matrix_multiply(_opts \\ []), do: :erlang.nif_error(:undefinedined)
static ERL_NIF_TERM evision_cv_mat_matrix_multiply(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);
    int error_flag = false;

    {
        Mat lhs;
        Mat rhs;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "lhs"), lhs, ArgInfo("lhs", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "rhs"), rhs, ArgInfo("rhs", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            Mat ret;
            if (get_binary_type(t, l, 0, type)) {
                ERRWRAP2(ret = lhs * rhs, env, error_flag, error_term);
                if (!error_flag) {
                    ERRWRAP2(ret.convertTo(ret, type), env, error_flag, error_term);
                    if (!error_flag) {
                        return evision_from(env, ret);
                    }
                }
            } else {
                ERRWRAP2(ret = Mat(lhs * rhs), env, error_flag, error_term);
                if (!error_flag) {
                    return evision_from(env, ret);
                }
            }
        }
    }

    if (error_flag) return error_term;
    else return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_MATRIX_MULTIPLY_H
