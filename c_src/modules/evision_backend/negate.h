#ifndef EVISION_BACKEND_NEGATE_H
#define EVISION_BACKEND_NEGATE_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_negate, evision_cv_mat_negate, 1
// @evision nif: def mat_negate(_opts \\ []), do: :erlang.nif_error("Mat::negate not loaded")
static ERL_NIF_TERM evision_cv_mat_negate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            int type = img.type();
            uint8_t depth = type & CV_MAT_DEPTH_MASK;

            if (depth == CV_8U) {
                auto ptr = img.ptr<uint8_t>();
                size_t count = img.total();
                for (size_t i = 0; i < count; ++i) {
                    ptr[i] = 0 - ptr[i];
                }
                return evision::nif::ok(env, evision_from(env, img));
            } else if (depth == CV_16U) {
                auto ptr = img.ptr<uint16_t>();
                size_t count = img.total();
                for (size_t i = 0; i < count; ++i) {
                    ptr[i] = 0 - ptr[i];
                }
                return evision::nif::ok(env, evision_from(env, img));
            } else {
                Mat out = Mat(-img);
                return evision::nif::ok(env, evision_from(env, out));
            }
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_NEGATE_H
