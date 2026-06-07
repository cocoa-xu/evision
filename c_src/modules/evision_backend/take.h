#ifndef EVISION_BACKEND_TAKE_H
#define EVISION_BACKEND_TAKE_H

#include <cstring>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

// Gather along one axis (Nx.take). Type-agnostic byte copy:
// out[o, i, inner-block] = src[o, indices[i], inner-block], where o in [0,outer),
// i in [0,num_idx). indices are int64; bounds are checked to match Nx.
// @evision c: mat_take, evision_cv_mat_take, 1
// @evision nif: def mat_take(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_take(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat src, indices;
        int outer = 0, axis_dim = 0, inner = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "indices"), indices, ArgInfo("indices", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "outer"), outer, ArgInfo("outer", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "axis_dim"), axis_dim, ArgInfo("axis_dim", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "inner"), inner, ArgInfo("inner", 0))) {
            Mat src_c = src.isContinuous() ? src : src.clone();
            Mat idx_c = indices.isContinuous() ? indices : indices.clone();
            const uchar *sp = src_c.data;
            const int64_t *ip = (const int64_t *)idx_c.data;
            int64_t num_idx = (int64_t)idx_c.total();
            size_t inner_bytes = (size_t)inner * src_c.elemSize();

            Mat dst(1, (int)((int64_t)outer * num_idx * inner), src.type());
            uchar *dp = dst.data;

            for (int64_t o = 0; o < outer; o++) {
                for (int64_t i = 0; i < num_idx; i++) {
                    int64_t idx = ip[i];
                    if (idx < 0 || idx >= axis_dim)
                        return evision::nif::error(env, "take: index out of bounds");
                    std::memcpy(dp + (size_t)(o * num_idx + i) * inner_bytes,
                                sp + (size_t)(o * axis_dim + idx) * inner_bytes,
                                inner_bytes);
                }
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_TAKE_H
