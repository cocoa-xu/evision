#ifndef EVISION_BACKEND_SELECT_H
#define EVISION_BACKEND_SELECT_H

#include <cstring>
#include <cstdint>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"
#include "parallel.h"
#include "validation.h"

template <size_t ElementSize>
static void evision_select(const cv::Mat& mask, const cv::Mat& on_true,
                           const cv::Mat& on_false, cv::Mat& dst) {
    const unsigned char *mp = mask.data;
    const unsigned char *tp = on_true.data;
    const unsigned char *fp = on_false.data;
    unsigned char *dp = dst.data;
    const size_t elem_size = ElementSize == 0 ? dst.elemSize() : ElementSize;
    const size_t n = dst.total();
    evision_parallel_for((int64_t)n, (int64_t)n, EVISION_PARALLEL_SIMPLE_MIN_WORK, [&](int64_t begin, int64_t end) {
        int64_t i = begin;
        for (; i + 8 <= end; i += 8) {
            uint64_t mask_bits;
            std::memcpy(&mask_bits, mp + i, sizeof(mask_bits));
            const size_t offset = (size_t)i * elem_size;
            if (mask_bits == 0) {
                std::memcpy(dp + offset, fp + offset, 8 * elem_size);
            } else if (((mask_bits - UINT64_C(0x0101010101010101)) &
                        ~mask_bits & UINT64_C(0x8080808080808080)) == 0) {
                // No zero byte: copy this entire run from on_true.
                std::memcpy(dp + offset, tp + offset, 8 * elem_size);
            } else {
                for (size_t k = 0; k < 8; ++k) {
                    const size_t element_offset = offset + k * elem_size;
                    const unsigned char *source = mp[(size_t)i + k] == 0 ? fp : tp;
                    std::memcpy(dp + element_offset, source + element_offset, elem_size);
                }
            }
        }
        for (; i < end; i++) {
            const size_t offset = (size_t)i * elem_size;
            const unsigned char *source = mp[(size_t)i] == 0 ? fp : tp;
            std::memcpy(dp + offset, source + offset, elem_size);
        }
    });
}

// Elementwise select (Nx.select): result = mask ? on_true : on_false. All three
// have the same shape (broadcast by the caller); on_true/on_false are already the
// output type and mask is u8. Copy each selected element into a fresh result.
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

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "mask"), mask, ArgInfo("mask", ArgInfo::BORROWED_INPUT)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "on_true"), on_true, ArgInfo("on_true", ArgInfo::BORROWED_INPUT)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "on_false"), on_false, ArgInfo("on_false", ArgInfo::BORROWED_INPUT))) {
            if (!evision_backend_tensor(mask) || mask.type() != CV_8U ||
                !evision_backend_tensor(on_true) || !evision_backend_tensor(on_false) ||
                on_true.type() != on_false.type() || mask.total() != on_true.total() ||
                on_false.total() != on_true.total())
                return evision::nif::error(env, "select: invalid shape or type");
            Mat mc = mask.isContinuous() ? mask : mask.clone();
            Mat tc = on_true.isContinuous() ? on_true : on_true.clone();
            Mat fc = on_false.isContinuous() ? on_false : on_false.clone();
            Mat dst(on_true.dims, on_true.size.p, on_true.type());

            // Fixed copy sizes inline into scalar loads/stores instead of
            // calling memcpy once per element. Preserve arbitrary-size support.
            switch (dst.elemSize()) {
                case 1: evision_select<1>(mc, tc, fc, dst); break;
                case 2: evision_select<2>(mc, tc, fc, dst); break;
                case 4: evision_select<4>(mc, tc, fc, dst); break;
                case 8: evision_select<8>(mc, tc, fc, dst); break;
                default: evision_select<0>(mc, tc, fc, dst); break;
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_SELECT_H
