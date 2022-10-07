#ifndef EVISION_BACKEND_BITWISE_OR_H
#define EVISION_BACKEND_BITWISE_OR_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_bitwise_or, evision_cv_mat_bitwise_or, 1
// @evision nif: def mat_bitwise_or(_opts \\ []), do: :erlang.nif_error("Mat::bitwise_or not loaded")
static ERL_NIF_TERM evision_cv_mat_bitwise_or(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
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
            ERRWRAP2(cv::bitwise_or(l, r, ret), env, error_flag, error_term);
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

#endif // EVISION_BACKEND_BITWISE_OR_H
