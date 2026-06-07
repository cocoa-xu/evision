#ifndef EVISION_BACKEND_PUT_SLICE_H
#define EVISION_BACKEND_PUT_SLICE_H

#include <cstring>
#include <vector>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

// Scatter a dense block into a copy of `base` (Nx.put_slice; also the building block
// for concatenate). dst = clone(base); for each element of `slice` at multi-index
// (j_0..j_{r-1}), write it to dst at (starts[k] + j_k). Type-agnostic byte copy via
// an odometer tracking the destination offset; the block is bounds-checked up front.
// @evision c: mat_put_slice, evision_cv_mat_put_slice, 1
// @evision nif: def mat_put_slice(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_put_slice(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat base, slice;
        std::vector<int> base_dims, slice_dims, starts;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "base"), base, ArgInfo("base", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "slice"), slice, ArgInfo("slice", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "base_dims"), base_dims, ArgInfo("base_dims", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "slice_dims"), slice_dims, ArgInfo("slice_dims", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "starts"), starts, ArgInfo("starts", 0))) {
            int r = (int)base_dims.size();
            if ((int)slice_dims.size() != r || (int)starts.size() != r)
                return evision::nif::error(env, "put_slice: rank mismatch");

            std::vector<int64_t> base_stride((size_t)(r > 0 ? r : 1));
            int64_t acc = 1;
            for (int k = r - 1; k >= 0; k--) { base_stride[(size_t)k] = acc; acc *= base_dims[(size_t)k]; }

            int64_t slice_total = 1;
            for (int k = 0; k < r; k++) {
                slice_total *= slice_dims[(size_t)k];
                if (starts[(size_t)k] < 0 || starts[(size_t)k] + slice_dims[(size_t)k] > base_dims[(size_t)k])
                    return evision::nif::error(env, "put_slice: slice out of bounds");
            }

            Mat dst = base.clone();
            Mat sl = slice.isContinuous() ? slice : slice.clone();
            size_t elem = dst.elemSize();
            const uchar *sp = sl.data;
            uchar *dp = dst.data;

            if (slice_total > 0) {
                std::vector<int64_t> idx((size_t)(r > 0 ? r : 1), 0);
                int64_t dst_off = 0;
                for (int k = 0; k < r; k++) dst_off += (int64_t)starts[(size_t)k] * base_stride[(size_t)k];
                for (int64_t s = 0; s < slice_total; s++) {
                    std::memcpy(dp + (size_t)dst_off * elem, sp + (size_t)s * elem, elem);
                    for (int k = r - 1; k >= 0; k--) {
                        idx[(size_t)k]++;
                        dst_off += base_stride[(size_t)k];
                        if (idx[(size_t)k] < slice_dims[(size_t)k]) break;
                        idx[(size_t)k] = 0;
                        dst_off -= (int64_t)slice_dims[(size_t)k] * base_stride[(size_t)k];
                    }
                }
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_PUT_SLICE_H
