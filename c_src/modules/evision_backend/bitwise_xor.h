#ifndef EVISION_BACKEND_BITWISE_XOR_H
#define EVISION_BACKEND_BITWISE_XOR_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_bitwise_xor, evision_cv_mat_bitwise_xor, 1
// @evision nif: def mat_bitwise_xor(_opts \\ []), do: :erlang.nif_error("Mat::bitwise_xor not loaded")
static ERL_NIF_TERM evision_cv_mat_bitwise_xor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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
            cv::bitwise_xor(l, r, ret);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_BITWISE_XOR_H
