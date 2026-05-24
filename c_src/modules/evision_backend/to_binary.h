#ifndef EVISION_BACKEND_TO_BINARY_H
#define EVISION_BACKEND_TO_BINARY_H

#include <erl_nif.h>
#include <algorithm>
#include <limits>
#include "../../ArgInfo.hpp"

// @evision c: mat_to_binary,evision_cv_mat_to_binary,1
// @evision nif: def mat_to_binary(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_to_binary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        evision_res<cv::Mat *> * res;
        if( enif_get_resource(env, evision_get_kw(env, erl_terms, "img"), evision_res<cv::Mat *>::type, (void **)&res) ) {
            if (res->val == nullptr || res->val->data == nullptr) {
                return evision::nif::error(env, "empty matrix");
            }

            ErlNifUInt64 limit = 0;
            ERL_NIF_TERM limit_term = evision_get_kw(env, erl_terms, "limit");
            if (!evision::nif::check_nil(env, limit_term) &&
                !enif_get_uint64(env, limit_term, &limit)) {
                return enif_make_badarg(env);
            }

            const size_t full_size = res->val->total() * res->val->elemSize();
            size_t bin_size = full_size;
            if (limit > 0) {
                const size_t elem_size = res->val->elemSize1();
                if (limit <= std::numeric_limits<size_t>::max() / elem_size) {
                    bin_size = std::min((size_t)limit * elem_size, full_size);
                }
            }

            if (res->val->isContinuous()) {
                return enif_make_resource_binary(env, res, res->val->data, bin_size);
            }

            Mat continuous = res->val->clone();
            ERL_NIF_TERM out_bin_term;
            unsigned char *out = enif_make_new_binary(env, bin_size, &out_bin_term);
            if (out == nullptr) {
                return evision::nif::error(env, "out of memory");
            }
            memcpy(out, continuous.data, bin_size);
            return out_bin_term;
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_TO_BINARY_H
