#ifndef EVISION_BACKEND_SELECT_H
#define EVISION_BACKEND_SELECT_H

#include <cstring>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

// Elementwise select (Nx.select): result = mask ? on_true : on_false. All three
// have the same shape (broadcast by the caller); on_true/on_false are already the
// output type and mask is u8. Type-agnostic byte copy: start from on_true, then
// overwrite the elements where the mask is zero with on_false.
// @evision c: mat_select, evision_cv_mat_select, 1
// @evision nif: def mat_select(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_select(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat mask, on_true, on_false;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "mask"), mask, ArgInfo("mask", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "on_true"), on_true, ArgInfo("on_true", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "on_false"), on_false, ArgInfo("on_false", 0))) {
            Mat mc = mask.isContinuous() ? mask : mask.clone();
            Mat fc = on_false.isContinuous() ? on_false : on_false.clone();
            Mat dst = on_true.clone();

            const unsigned char *mp = mc.data;
            const unsigned char *fp = fc.data;
            unsigned char *dp = dst.data;
            size_t elem_size = dst.elemSize();
            size_t n = dst.total();
            for (size_t i = 0; i < n; i++) {
                if (mp[i] == 0)
                    std::memcpy(dp + i * elem_size, fp + i * elem_size, elem_size);
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_SELECT_H
