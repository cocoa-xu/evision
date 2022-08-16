#ifndef EVISION_BACKEND_MATRIX_MULTIPLY_H
#define EVISION_BACKEND_MATRIX_MULTIPLY_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_matrix_multiply, evision_cv_mat_matrix_multiply, 1
// @evision nif: def mat_matrix_multiply(_opts \\ []), do: :erlang.nif_error("Mat::matrix_multiply not loaded")
static ERL_NIF_TERM evision_cv_mat_matrix_multiply(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

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
            if (get_binary_type(t, l, 0, type)) {
                Mat ret = lhs * rhs;
                ret.convertTo(ret, type);
                return evision::nif::ok(env, evision_from(env, ret));
            } else {
                return evision::nif::ok(env, evision_from(env, Mat(lhs * rhs)));
            }
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_MATRIX_MULTIPLY_H
