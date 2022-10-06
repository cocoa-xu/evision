#ifndef EVISION_BACKEND_CLIP_H
#define EVISION_BACKEND_CLIP_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_clip, evision_cv_mat_clip, 1
// @evision nif: def mat_clip(_opts \\ []), do: :erlang.nif_error("Mat::clip not loaded")
static ERL_NIF_TERM evision_cv_mat_clip(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);
    int error_flag = false;

    {
        Mat img;
        double lower;
        double upper;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "lower"), lower, ArgInfo("lower", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "upper"), upper, ArgInfo("upper", 0))) {
            ERRWRAP2(img.setTo(lower, img < lower), env, error_flag, error_term);
            if (!error_flag) {
                ERRWRAP2(img.setTo(lower, img > lower), env, error_flag, error_term);
                if (!error_flag) {
                    return evision::nif::ok(env, evision_from(env, img));
                }
            }
        }
    }

    if (error_flag) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_CLIP_H
