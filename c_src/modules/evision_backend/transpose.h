#ifndef EVISION_BACKEND_TRANSPOSE_H
#define EVISION_BACKEND_TRANSPOSE_H

#include <erl_nif.h>
#include <limits>
#include "../../ArgInfo.hpp"

// @evision c: mat_transpose,evision_cv_mat_transpose,1
// @evision nif: def mat_transpose(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_transpose(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    Mat img;
    std::vector<int> axes;
    std::vector<int> as_shape;
    bool as_shaped = true;

    if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0)) &&
        evision_to_safe(env, evision_get_kw(env, erl_terms, "axes"), axes, ArgInfo("axes", 0)) &&
        evision_to_safe(env, evision_get_kw(env, erl_terms, "as_shape"), as_shape, ArgInfo("as_shape", 0)) &&
        evision_to_safe(env, evision_get_kw(env, erl_terms, "as_shaped"), as_shaped, ArgInfo("as_shaped", 0))) {
        int ndims = (int)as_shape.size();
        if (ndims <= 0 || axes.size() != as_shape.size()) {
            return evision::nif::error(env, "invalid transpose shape or axes");
        }
        for (int dim : as_shape) {
            if (dim <= 0) {
                return evision::nif::error(env, "invalid transpose shape");
            }
        }

        int error_flag = false;
        cv::Mat ret;
        if (as_shaped) {
            size_t expected_elems = 1;
            for (int dim : as_shape) {
                if ((size_t)dim > std::numeric_limits<size_t>::max() / expected_elems) {
                    return evision::nif::error(env, "as_shape overflow");
                }
                expected_elems *= (size_t)dim;
            }
            size_t expected_bytes = expected_elems * (size_t)CV_ELEM_SIZE1(img.depth());
            size_t actual_bytes = (size_t)img.total() * (size_t)img.elemSize();
            if (expected_bytes != actual_bytes) {
                return evision::nif::error(env, "as_shape byte size does not match source matrix");
            }
            if (!img.isContinuous()) {
                img = img.clone();
            }
            Mat shaped(ndims, as_shape.data(), img.depth(), img.data);
            ERRWRAP2(cv::transposeND(shaped, axes, ret), env, error_flag, error_term);
        } else {
            ERRWRAP2(cv::transposeND(img, axes, ret), env, error_flag, error_term);
        }

        if (!error_flag) {
            return evision_from(env, ret);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_TRANSPOSE_H
