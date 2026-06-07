#ifndef EVISION_BACKEND_SHIFT_H
#define EVISION_BACKEND_SHIFT_H

#include <cstdint>
#include <type_traits>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"
#include "parallel.h"

// Elementwise integer shift (Nx.left_shift / right_shift). `l` and `r` share the
// output type and shape. op 0 = left (<<), 1 = right (>>). Left shifts compute in
// the unsigned counterpart (modular, no signed-overflow UB); right shifts use the
// native >> (arithmetic for signed, logical for unsigned). A shift >= bit width
// yields 0, except a right shift of a negative signed value yields -1 -- matching
// Nx's arbitrary-precision shift truncated to the type.
template <typename T>
static void evision_shift(const cv::Mat &l, const cv::Mat &r, cv::Mat &dst, int op) {
    typedef typename std::make_unsigned<T>::type U;
    const T *lp = (const T *)l.data;
    const T *rp = (const T *)r.data;
    T *dp = (T *)dst.data;
    size_t n = l.total();
    uint64_t width = (uint64_t)(sizeof(T) * 8);
    evision_parallel_for((int64_t)n, (int64_t)n, EVISION_PARALLEL_SIMPLE_MIN_WORK, [&](int64_t begin, int64_t end) {
        for (int64_t i = begin; i < end; i++) {
            T a = lp[(size_t)i];
            T b = rp[(size_t)i];
            if (op == 0) {
                dp[(size_t)i] = ((uint64_t)b >= width) ? (T)0 : (T)((U)a << b);
            } else if ((uint64_t)b >= width) {
                dp[(size_t)i] = (std::is_signed<T>::value && a < 0) ? (T)(-1) : (T)0;
            } else {
                dp[(size_t)i] = (T)(a >> b);
            }
        }
    });
}

// @evision c: mat_shift, evision_cv_mat_shift, 1
// @evision nif: def mat_shift(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_shift(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat l, r;
        int op = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "op"), op, ArgInfo("op", 0))) {
            Mat lc = l.isContinuous() ? l : l.clone();
            Mat rc = r.isContinuous() ? r : r.clone();
            Mat dst = lc.clone();
            switch (dst.depth()) {
                case CV_8U:  evision_shift<uint8_t>(lc, rc, dst, op); break;
                case CV_8S:  evision_shift<int8_t>(lc, rc, dst, op); break;
                case CV_16U: evision_shift<uint16_t>(lc, rc, dst, op); break;
                case CV_16S: evision_shift<int16_t>(lc, rc, dst, op); break;
                case CV_32S: evision_shift<int32_t>(lc, rc, dst, op); break;
                case CV_32U: evision_shift<uint32_t>(lc, rc, dst, op); break;
                case CV_64S: evision_shift<int64_t>(lc, rc, dst, op); break;
                case CV_64U: evision_shift<uint64_t>(lc, rc, dst, op); break;
                default: return evision::nif::error(env, "mat_shift: integer types only");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_SHIFT_H
