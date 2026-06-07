#ifndef EVISION_BACKEND_INDEXED_H
#define EVISION_BACKEND_INDEXED_H

#include <cstring>
#include <vector>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

// out[coord] += src_block, element-wise, in the element's own type. Integer adds
// use an unsigned accumulator so wraparound is well-defined (two's complement),
// matching Nx's modular semantics; floats add directly.
template <typename T, typename Acc>
static inline void scatter_add_block(unsigned char *dst, const unsigned char *src, int64_t n) {
    T *d = (T *)dst;
    const T *s = (const T *)src;
    for (int64_t i = 0; i < n; i++) d[i] = (T)((Acc)d[i] + (Acc)s[i]);
}

// Scatter into a copy of `src` (Nx.indexed_put / indexed_add). `src` is pre-cast
// to the output type and pre-transposed so the indexed axes lead; each consecutive
// `depth`-tuple in `indices` is a coordinate over `dims` (the leading dim sizes)
// addressing one contiguous `inner`-element block. op 0 overwrites the block
// (type-agnostic byte copy, last write wins for duplicate coords); op 1 accumulates
// it (typed). Coordinates are bounds-checked per axis to match Nx.
// @evision c: mat_indexed, evision_cv_mat_indexed, 1
// @evision nif: def mat_indexed(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_indexed(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat src, indices, updates, dims;
        int inner = 0, op = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "indices"), indices, ArgInfo("indices", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "updates"), updates, ArgInfo("updates", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "dims"), dims, ArgInfo("dims", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "inner"), inner, ArgInfo("inner", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "op"), op, ArgInfo("op", 0))) {
            // clone so the immutable source Mat is never mutated; clone() is continuous.
            Mat result = src.clone();
            Mat upd_c = updates.isContinuous() ? updates : updates.clone();
            Mat idx_c = indices.isContinuous() ? indices : indices.clone();
            Mat dim_c = dims.isContinuous() ? dims : dims.clone();

            const int64_t *ip = (const int64_t *)idx_c.data;
            const int64_t *dimp = (const int64_t *)dim_c.data;
            int64_t depth = (int64_t)dim_c.total();
            int64_t total_idx = (int64_t)idx_c.total();
            if (depth <= 0 || total_idx % depth != 0)
                return evision::nif::error(env, "indexed: indices size not a multiple of coordinate depth");
            int64_t num_updates = total_idx / depth;

            std::vector<int64_t> mult((size_t)depth);
            int64_t acc = inner;
            for (int64_t k = depth - 1; k >= 0; k--) {
                mult[(size_t)k] = acc;
                acc *= dimp[(size_t)k];
            }

            uchar *rp = result.data;
            const uchar *up = upd_c.data;
            size_t elem_size = result.elemSize();
            size_t inner_bytes = (size_t)inner * elem_size;
            int depth_id = result.type() & CV_MAT_DEPTH_MASK;

            for (int64_t g = 0; g < num_updates; g++) {
                int64_t offset = 0;
                for (int64_t k = 0; k < depth; k++) {
                    int64_t c = ip[g * depth + k];
                    if (c < 0 || c >= dimp[(size_t)k])
                        return evision::nif::error(env, "indexed: index out of bounds");
                    offset += c * mult[(size_t)k];
                }
                uchar *dstblk = rp + (size_t)offset * elem_size;
                const uchar *srcblk = up + (size_t)g * inner_bytes;

                if (op == 0) {
                    std::memcpy(dstblk, srcblk, inner_bytes);
                    continue;
                }

                switch (depth_id) {
                    case CV_8U:  scatter_add_block<uint8_t, uint32_t>(dstblk, srcblk, inner); break;
                    case CV_8S:  scatter_add_block<int8_t, uint32_t>(dstblk, srcblk, inner); break;
                    case CV_16U: scatter_add_block<uint16_t, uint32_t>(dstblk, srcblk, inner); break;
                    case CV_16S: scatter_add_block<int16_t, uint32_t>(dstblk, srcblk, inner); break;
                    case CV_32S: scatter_add_block<int32_t, uint32_t>(dstblk, srcblk, inner); break;
                    case CV_32U: scatter_add_block<uint32_t, uint32_t>(dstblk, srcblk, inner); break;
                    case CV_64S: scatter_add_block<int64_t, uint64_t>(dstblk, srcblk, inner); break;
                    case CV_64U: scatter_add_block<uint64_t, uint64_t>(dstblk, srcblk, inner); break;
                    case CV_32F: scatter_add_block<float, float>(dstblk, srcblk, inner); break;
                    case CV_64F: scatter_add_block<double, double>(dstblk, srcblk, inner); break;
                    default: return evision::nif::error(env, "indexed_add: unsupported dtype");
                }
            }
            return evision_from(env, result);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_INDEXED_H
