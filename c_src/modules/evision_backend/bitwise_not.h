#ifndef EVISION_BACKEND_BITWISE_NOT_H
#define EVISION_BACKEND_BITWISE_NOT_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_bitwise_not, evision_cv_mat_bitwise_not, 1
// @evision nif: def mat_bitwise_not(_opts \\ []), do: :erlang.nif_error("Mat::bitwise_not not loaded")
static ERL_NIF_TERM evision_cv_mat_bitwise_not(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            Mat out = Mat(~img);
            return evision::nif::ok(env, evision_from(env, out));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_BITWISE_NOT_H
