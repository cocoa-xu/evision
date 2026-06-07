#ifndef EVISION_BACKEND_BROADCAST_H
#define EVISION_BACKEND_BROADCAST_H

#include <cstring>
#include <vector>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// Replicate img (interpreted as src_shape, already left-aligned with 1s to
// to_shape's rank) into dst_data shaped as to_shape, following NumPy/Nx
// broadcasting: a source dim of size 1 is stretched to the target size. Source
// dims map to output coordinates with stride 0 where stretched; the contiguous
// trailing run is copied as a block.
static void broadcast(
    const cv::Mat &img,
    void *dst_data,
    const std::vector<int>& src_shape,
    const std::vector<int>& to_shape,
    size_t elem_size
)
{
    const size_t ndims = to_shape.size();

    const uint8_t *src = (const uint8_t *)img.data;
    cv::Mat tmp;
    if (!img.isContinuous()) {
        tmp = img.clone();
        src = (const uint8_t *)tmp.data;
    }

    uint8_t *dst = (uint8_t *)dst_data;

    if (ndims == 0) {
        memcpy(dst, src, elem_size);
        return;
    }

    // Row-major source strides (in elements); stretched dims get stride 0.
    std::vector<size_t> src_strides(ndims, 0);
    size_t stride = 1;
    for (int64_t d = (int64_t)ndims - 1; d >= 0; --d) {
        src_strides[d] = (src_shape[d] == 1) ? 0 : stride;
        stride *= (size_t)src_shape[d];
    }

    const size_t last = (size_t)to_shape[ndims - 1];
    const bool last_is_broadcast = src_shape[ndims - 1] == 1;
    size_t outer = 1;
    for (size_t d = 0; d + 1 < ndims; ++d) outer *= (size_t)to_shape[d];

    std::vector<int> idx(ndims - 1, 0);
    for (size_t o = 0; o < outer; ++o) {
        size_t base = 0;
        for (size_t d = 0; d + 1 < ndims; ++d) base += (size_t)idx[d] * src_strides[d];

        uint8_t *drow = dst + o * last * elem_size;
        if (!last_is_broadcast) {
            memcpy(drow, src + base * elem_size, last * elem_size);
        } else {
            const uint8_t *s = src + base * elem_size;
            for (size_t i = 0; i < last; ++i) memcpy(drow + i * elem_size, s, elem_size);
        }

        for (int64_t d = (int64_t)ndims - 2; d >= 0; --d) {
            if (++idx[d] < to_shape[d]) break;
            idx[d] = 0;
        }
    }
}

// @evision c: mat_broadcast_to, evision_cv_mat_broadcast_to, 1
// @evision nif: def mat_broadcast_to(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_broadcast_to(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;
        std::vector<int> to_shape;
        std::vector<int> force_src_shape;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "to_shape"), to_shape, ArgInfo("to_shape", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "force_src_shape"), force_src_shape, ArgInfo("force_src_shape", 0))) {
            int64_t ndims = to_shape.size();
            std::vector<int> src_shape(ndims);

            int diff_dims = (int)ndims;
            if (force_src_shape.size() > 0) {
                diff_dims -= force_src_shape.size();
            } else {
                diff_dims -= img.size.dims;
                for (int i = 0; i < img.size.dims; ++i) {
                    force_src_shape.push_back(img.size.p[i]);
                }
            }

            if (diff_dims < 0) {
                return evision::nif::error(env, "cannot broadcast to specified shape");
            }

            // align shapes with 1s
            for (int64_t i = 0; i < ndims; ++i) {
                if (i < diff_dims) {
                    src_shape[i] = 1;
                } else {
                    src_shape[i] = force_src_shape[i - diff_dims];
                    // each dim has to be equal unless the src dim is 1 (which we are going to broadcast)
                    if (src_shape[i] != to_shape[i] && src_shape[i] != 1) {
                        return evision::nif::error(env, "cannot broadcast to specified shape.");
                    }
                }
            }

            // calculate number of elements in the new shape
            const size_t elem_size = img.elemSize();
            size_t count_new_elem = 1;
            for (int64_t i = 0; i < ndims; i++) {
                count_new_elem *= to_shape[i];
            }

            // allocate memory
            void * dst_data = (void *)enif_alloc(elem_size * count_new_elem);
            if (dst_data == nullptr) {
                return evision::nif::error(env, "cannot broadcast to specified shape, out of memory");
            }

            // broadcast
            broadcast(img, dst_data, src_shape, to_shape, elem_size);

            int type = img.type() & CV_MAT_DEPTH_MASK;
            Mat result = Mat((int)ndims, to_shape.data(), type, dst_data);
            result = result.clone();
            enif_free((void *)dst_data);
            return evision_from(env, result);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_BROADCAST_H
