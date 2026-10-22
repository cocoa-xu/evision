#ifndef EVISION_BACKEND_RESHAPE_H
#define EVISION_BACKEND_RESHAPE_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../../evision_native_safety.hpp"

// @evision c: mat_reshape, evision_cv_mat_reshape, 1
// @evision nif: def mat_reshape(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_reshape(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat mat;
        std::vector<int> shape;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "mat"), mat, ArgInfo("mat", ArgInfo::INPUT_ONLY)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "shape"), shape, ArgInfo("shape", 0))) {
            size_t expected_bytes, actual_bytes;
            if (mat.empty() || !evision_shape_byte_size(shape, mat.elemSize(), expected_bytes) ||
                !evision_checked_mul_size(mat.total(), mat.elemSize(), actual_bytes) ||
                expected_bytes != actual_bytes)
                return evision::nif::error(env, "reshape byte size does not match source matrix");
            Mat ret;

            int error_flag = false;
            ERRWRAP2(ret = mat.reshape(0, shape), env, error_flag, error_term);
            if (!error_flag) {
                return evision_from(env, ret);
            }
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_RESHAPE_H
