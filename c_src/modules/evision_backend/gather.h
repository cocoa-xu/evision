#ifndef EVISION_BACKEND_GATHER_H
#define EVISION_BACKEND_GATHER_H

#include <atomic>
#include <cstring>
#include <vector>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"
#include "parallel.h"
#include "validation.h"

// Gather over the leading axes (Nx.gather). The tensor is pre-transposed so the
// indexed axes lead; each consecutive `depth`-tuple in `indices` is a coordinate
// over `dims` (the leading dim sizes) selecting one contiguous `inner`-element
// block. Type-agnostic byte copy; indices are int64 and every coordinate is
// bounds-checked against its axis to match Nx.
// @evision c: mat_gather, evision_cv_mat_gather, 1
// @evision nif: def mat_gather(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_gather(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat src, indices, dims;
        int inner = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", ArgInfo::INPUT_ONLY)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "indices"), indices, ArgInfo("indices", ArgInfo::INPUT_ONLY)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "dims"), dims, ArgInfo("dims", ArgInfo::INPUT_ONLY)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "inner"), inner, ArgInfo("inner", 0))) {
            if (!evision_backend_indices(indices))
                return evision::nif::error(env, "gather: indices must be s64");
            Mat dim_c = dims;
            std::vector<int64_t> mult;
            if (!evision_backend_coordinates(src, dim_c, inner, mult))
                return evision::nif::error(env, "gather: invalid source shape or dimension type");
            const int64_t *dimp = (const int64_t *)dim_c.data;

            int64_t depth = (int64_t)dim_c.total();
            int64_t total_idx = (int64_t)indices.total();
            if (depth <= 0 || total_idx % depth != 0)
                return evision::nif::error(env, "gather: indices size not a multiple of coordinate depth");
            int64_t num_gathers = total_idx / depth;

            size_t output_count;
            if (!evision_backend_count({(int)num_gathers, inner}, output_count))
                return evision::nif::error(env, "gather: output size overflow");

            Mat src_c = src.isContinuous() ? src : src.clone();
            Mat idx_c = indices.isContinuous() ? indices : indices.clone();
            const uchar *sp = src_c.data;
            const int64_t *ip = (const int64_t *)idx_c.data;
            size_t elem_size = src_c.elemSize();
            size_t inner_bytes = (size_t)inner * elem_size;
            Mat dst(1, (int)output_count, src.type());
            uchar *dp = dst.data;

            std::atomic<bool> bad_index(false);
            evision_parallel_for(num_gathers, (int64_t)output_count, [&](int64_t begin, int64_t end) {
                for (int64_t g = begin; g < end; g++) {
                    int64_t offset = 0;
                    for (int64_t k = 0; k < depth; k++) {
                        int64_t c = ip[g * depth + k];
                        if (c < 0 || c >= dimp[(size_t)k]) {
                            bad_index.store(true, std::memory_order_relaxed);
                            offset = -1;
                            break;
                        }
                        offset += c * mult[(size_t)k];
                    }
                    if (offset >= 0) {
                        std::memcpy(dp + (size_t)g * inner_bytes,
                                    sp + (size_t)offset * elem_size,
                                    inner_bytes);
                    }
                }
            });
            if (bad_index.load(std::memory_order_relaxed))
                return evision::nif::error(env, "gather: index out of bounds");
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_GATHER_H
