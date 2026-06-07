#ifndef EVISION_BACKEND_STRIDED_COPY_H
#define EVISION_BACKEND_STRIDED_COPY_H

#include <cstring>
#include <vector>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

// General strided copy (Nx.slice / reverse). For each output element at multi-index
// (i_0..i_{r-1}), reads src at input index (starts[k] + i_k * strides[k]) per axis;
// strides may be negative (reverse). Type-agnostic byte copy via an odometer that
// tracks the input offset incrementally. Per-axis bounds are validated up front.
// @evision c: mat_strided_copy, evision_cv_mat_strided_copy, 1
// @evision nif: def mat_strided_copy(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_strided_copy(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat src;
        std::vector<int> in_dims, out_dims, starts, strides;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "in_dims"), in_dims, ArgInfo("in_dims", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "out_dims"), out_dims, ArgInfo("out_dims", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "starts"), starts, ArgInfo("starts", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "strides"), strides, ArgInfo("strides", 0))) {
            int r = (int)in_dims.size();
            if ((int)out_dims.size() != r || (int)starts.size() != r || (int)strides.size() != r)
                return evision::nif::error(env, "strided_copy: rank mismatch");

            std::vector<int64_t> in_stride((size_t)(r > 0 ? r : 1));
            int64_t acc = 1;
            for (int k = r - 1; k >= 0; k--) { in_stride[(size_t)k] = acc; acc *= in_dims[(size_t)k]; }

            int64_t out_total = 1;
            for (int k = 0; k < r; k++) {
                out_total *= out_dims[(size_t)k];
                if (out_dims[(size_t)k] <= 0) continue;
                int64_t lo = starts[(size_t)k];
                int64_t hi = (int64_t)starts[(size_t)k] + (int64_t)(out_dims[(size_t)k] - 1) * strides[(size_t)k];
                if (lo < 0 || lo >= in_dims[(size_t)k] || hi < 0 || hi >= in_dims[(size_t)k])
                    return evision::nif::error(env, "strided_copy: index out of bounds");
            }

            Mat src_c = src.isContinuous() ? src : src.clone();
            size_t elem = src_c.elemSize();
            Mat dst(1, (int)(out_total > 0 ? out_total : 1), src.type());
            const uchar *sp = src_c.data;
            uchar *dp = dst.data;

            if (out_total > 0) {
                std::vector<int64_t> idx((size_t)(r > 0 ? r : 1), 0);
                int64_t in_off = 0;
                for (int k = 0; k < r; k++) in_off += (int64_t)starts[(size_t)k] * in_stride[(size_t)k];
                for (int64_t o = 0; o < out_total; o++) {
                    std::memcpy(dp + (size_t)o * elem, sp + (size_t)in_off * elem, elem);
                    for (int k = r - 1; k >= 0; k--) {
                        idx[(size_t)k]++;
                        in_off += (int64_t)strides[(size_t)k] * in_stride[(size_t)k];
                        if (idx[(size_t)k] < out_dims[(size_t)k]) break;
                        idx[(size_t)k] = 0;
                        in_off -= (int64_t)out_dims[(size_t)k] * strides[(size_t)k] * in_stride[(size_t)k];
                    }
                }
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_STRIDED_COPY_H
