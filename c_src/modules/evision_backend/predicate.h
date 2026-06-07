#ifndef EVISION_BACKEND_PREDICATE_H
#define EVISION_BACKEND_PREDICATE_H

#include <cmath>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

// Elementwise predicate -> u8 {0,1} (Nx.is_nan / is_infinity / logical_not).
// op 0 = is_nan, 1 = is_infinity, 2 = is_zero (logical_not). The nan/inf checks go
// through double, so integer inputs are always finite (0); f16/bf16 are widened to
// f32 by the caller, so only real C types reach here. Output is a flat [1, n] u8
// mat the caller reshapes to the input shape.
template <typename T>
static void evision_predicate(const cv::Mat &src, cv::Mat &dst, int op) {
    const T *sp = (const T *)src.data;
    unsigned char *dp = dst.data;
    size_t n = src.total();
    for (size_t i = 0; i < n; i++) {
        T x = sp[i];
        unsigned char r = 0;
        switch (op) {
            case 0: r = std::isnan((double)x) ? 1 : 0; break;
            case 1: r = std::isinf((double)x) ? 1 : 0; break;
            case 2: r = (x == (T)0) ? 1 : 0; break;
            default: break;
        }
        dp[i] = r;
    }
}

// @evision c: mat_predicate, evision_cv_mat_predicate, 1
// @evision nif: def mat_predicate(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_predicate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat src;
        int op = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "op"), op, ArgInfo("op", 0))) {
            Mat src_c = src.isContinuous() ? src : src.clone();
            Mat dst(1, (int)src_c.total(), CV_8U);
            switch (src_c.depth()) {
                case CV_8U:  evision_predicate<uint8_t>(src_c, dst, op); break;
                case CV_8S:  evision_predicate<int8_t>(src_c, dst, op); break;
                case CV_16U: evision_predicate<uint16_t>(src_c, dst, op); break;
                case CV_16S: evision_predicate<int16_t>(src_c, dst, op); break;
                case CV_32S: evision_predicate<int32_t>(src_c, dst, op); break;
                case CV_32U: evision_predicate<uint32_t>(src_c, dst, op); break;
                case CV_64S: evision_predicate<int64_t>(src_c, dst, op); break;
                case CV_64U: evision_predicate<uint64_t>(src_c, dst, op); break;
                case CV_32F: evision_predicate<float>(src_c, dst, op); break;
                case CV_64F: evision_predicate<double>(src_c, dst, op); break;
                default: return evision::nif::error(env, "mat_predicate: unsupported depth");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_PREDICATE_H
