#ifndef EVISION_BACKEND_ABS_H
#define EVISION_BACKEND_ABS_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_abs, evision_cv_mat_abs, 1
// @evision nif: def mat_abs(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_abs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;
        Mat ret;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            int error_flag = false;
            ERRWRAP2(ret = Mat(cv::abs(img)), env, error_flag, error_term);
            if (!error_flag) {
                return evision_from(env, ret);
            }
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_ABS_H
