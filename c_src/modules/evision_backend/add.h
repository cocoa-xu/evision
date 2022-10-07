#ifndef EVISION_BACKEND_ADD_H
#define EVISION_BACKEND_ADD_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

// @evision c: mat_add, evision_cv_mat_add, 1
// @evision nif: def mat_add(_opts \\ []), do: :erlang.nif_error("Mat::add not loaded")
static ERL_NIF_TERM evision_cv_mat_add(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat l;
        Mat r;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", 0))) {
            Mat ret;
            int error_flag = false;
            ERRWRAP2(cv::add(l, r, ret, cv::noArray(), -1), env, error_flag, error_term);
            if (!error_flag) {
                return evision::nif::ok(env, evision_from(env, ret));
            } else {
                return error_term;
            }
        }
    }

    if (error_term != 0) return error_term;
    else return error(env, "overload resolution failed");
}

// @evision c: mat_add_typed, evision_cv_mat_add_typed, 1
// @evision nif: def mat_add_typed(_opts \\ []), do: :erlang.nif_error("Mat::add not loaded")
static ERL_NIF_TERM evision_cv_mat_add_typed(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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
            int error_flag = false;
            ERRWRAP2(cv::add(lhs, rhs, ret, cv::noArray(), type), env, error_flag, error_term);
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

#endif // EVISION_BACKEND_ADD_H
