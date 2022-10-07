#ifndef EVISION_BACKEND_RESHAPE_H
#define EVISION_BACKEND_RESHAPE_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_reshape, evision_cv_mat_reshape, 1
// @evision nif: def mat_reshape(_opts \\ []), do: :erlang.nif_error("Mat::reshape not loaded")
static ERL_NIF_TERM evision_cv_mat_reshape(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat mat;
        std::vector<int> shape;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "mat"), mat, ArgInfo("mat", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "shape"), shape, ArgInfo("shape", 0))) {
            Mat ret;

            int error_flag = false;
            ERRWRAP2(ret = mat.reshape(0, shape), env, error_flag, error_term);
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

#endif // EVISION_BACKEND_RESHAPE_H
