#ifndef EVISION_BACKEND_TO_BINARY_H
#define EVISION_BACKEND_TO_BINARY_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_to_binary,evision_cv_mat_to_binary,1
// @evision nif: def mat_to_binary(_opts \\ []), do: :erlang.nif_error("Mat::to_binary not loaded")
static ERL_NIF_TERM evision_cv_mat_to_binary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        // const char *keywords[] = {"img", NULL};
        // zero-copy to_binary
        evision_res<cv::Mat *> * res;
        if( enif_get_resource(env, evision_get_kw(env, erl_terms, "img"), evision_res<cv::Mat *>::type, (void **)&res) ) {
            size_t bin_size = res->val->total() * res->val->elemSize();
            ERL_NIF_TERM out_bin_term = enif_make_resource_binary(env, res, res->val->data, bin_size);
            return evision::nif::ok(env, out_bin_term);
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_TO_BINARY_H
