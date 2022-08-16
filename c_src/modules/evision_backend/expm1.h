#ifndef EVISION_BACKEND_EXPM1_H
#define EVISION_BACKEND_EXPM1_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_expm1, evision_cv_mat_expm1, 1
// @evision nif: def mat_expm1(_opts \\ []), do: :erlang.nif_error("Mat::expm1 not loaded")
static ERL_NIF_TERM evision_cv_mat_expm1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            Mat out;
            cv::exp(img, out);
            return evision::nif::ok(env, evision_from(env, Mat(out - 1)));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_EXPM1_H
