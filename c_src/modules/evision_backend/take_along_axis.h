#ifndef EVISION_BACKEND_TAKE_ALONG_AXIS_H
#define EVISION_BACKEND_TAKE_ALONG_AXIS_H

#include <atomic>
#include <cstring>
#include <vector>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"
#include "parallel.h"

// Per-element gather along one axis (Nx.take_along_axis). out has the indices' shape;
// out[j_0..j_{r-1}] = src[j with the `axis` coord replaced by indices[j]]. The non-axis
// part of the input offset skips the axis stride, and each element adds
// indices[p] * axis_stride. int64 indices, bounds-checked.
// @evision c: mat_take_along_axis, evision_cv_mat_take_along_axis, 1
// @evision nif: def mat_take_along_axis(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_take_along_axis(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat src, indices;
        std::vector<int> in_dims, out_dims;
        int axis = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "indices"), indices, ArgInfo("indices", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "in_dims"), in_dims, ArgInfo("in_dims", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "out_dims"), out_dims, ArgInfo("out_dims", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "axis"), axis, ArgInfo("axis", 0))) {
            int r = (int)in_dims.size();
            if ((int)out_dims.size() != r || axis < 0 || axis >= r)
                return evision::nif::error(env, "take_along_axis: bad rank or axis");

            std::vector<int64_t> in_stride((size_t)(r > 0 ? r : 1));
            int64_t acc = 1;
            for (int k = r - 1; k >= 0; k--) { in_stride[(size_t)k] = acc; acc *= in_dims[(size_t)k]; }

            int64_t out_total = 1;
            for (int k = 0; k < r; k++) out_total *= out_dims[(size_t)k];
            int64_t axis_dim = in_dims[(size_t)axis];
            int64_t axis_stride = in_stride[(size_t)axis];

            Mat src_c = src.isContinuous() ? src : src.clone();
            Mat idx_c = indices.isContinuous() ? indices : indices.clone();
            const int64_t *ip = (const int64_t *)idx_c.data;
            size_t elem = src_c.elemSize();
            Mat dst(1, (int)(out_total > 0 ? out_total : 1), src.type());
            const uchar *sp = src_c.data;
            uchar *dp = dst.data;

            if (out_total > 0) {
                std::atomic<bool> bad_index(false);
                evision_parallel_for(out_total, out_total, EVISION_PARALLEL_SIMPLE_MIN_WORK, [&](int64_t begin, int64_t end) {
                    for (int64_t p = begin; p < end; p++) {
                        int64_t q = p;
                        int64_t base = 0;
                        for (int k = r - 1; k >= 0; k--) {
                            int64_t coord = q % out_dims[(size_t)k];
                            q /= out_dims[(size_t)k];
                            if (k != axis) base += coord * in_stride[(size_t)k];
                        }

                        int64_t iv = ip[p];
                        if (iv < 0 || iv >= axis_dim) {
                            bad_index.store(true, std::memory_order_relaxed);
                        } else {
                            std::memcpy(dp + (size_t)p * elem, sp + (size_t)(base + iv * axis_stride) * elem, elem);
                        }
                    }
                });
                if (bad_index.load(std::memory_order_relaxed))
                    return evision::nif::error(env, "take_along_axis: index out of bounds");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_TAKE_ALONG_AXIS_H
