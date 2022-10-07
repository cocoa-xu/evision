#ifndef EVISION_BACKEND_DIVIDE_H
#define EVISION_BACKEND_DIVIDE_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_divide, evision_cv_mat_divide, 1
// @evision nif: def mat_divide(_opts \\ []), do: :erlang.nif_error("Mat::divide not loaded")
static ERL_NIF_TERM evision_cv_mat_divide(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);
    int error_flag = false;

    {
        Mat l;
        Mat r;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", 0))) {
            Mat ret;

            ERRWRAP2(cv::divide(l, r, ret, 1, -1), env, error_flag, error_term);
            if (!error_flag) {
                return evision::nif::ok(env, evision_from(env, ret));
            } else {
                return error_term;
            }
        }
    }

    if (error_flag) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_divide_typed, evision_cv_mat_divide_typed, 1
// @evision nif: def mat_divide_typed(_opts \\ []), do: :erlang.nif_error("Mat::divide not loaded")
static ERL_NIF_TERM evision_cv_mat_divide_typed(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");
            Mat ret;
            cv::divide(lhs, rhs, ret, 1, type);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_DIVIDE_H
