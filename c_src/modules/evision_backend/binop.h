#ifndef EVISION_BACKEND_BINOP_H
#define EVISION_BACKEND_BINOP_H

#include <cmath>
#include <cstdint>
#include <type_traits>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"
#include "parallel.h"

// Elementwise binary ops without an OpenCV primitive (Nx.atan2 / pow / quotient /
// remainder). op 0=atan2, 1=pow, 2=quotient, 3=remainder. l and r share the output
// type and shape (broadcast + cast by the caller; f16/bf16 widened to f32).

template <typename T>
static void evision_binop_float(const cv::Mat &l, const cv::Mat &r, cv::Mat &dst, int op) {
    const T *lp = (const T *)l.data;
    const T *rp = (const T *)r.data;
    T *dp = (T *)dst.data;
    size_t n = l.total();
    evision_parallel_for((int64_t)n, (int64_t)n, EVISION_PARALLEL_SIMPLE_MIN_WORK, [&](int64_t begin, int64_t end) {
        for (int64_t i = begin; i < end; i++) {
            T a = lp[(size_t)i], b = rp[(size_t)i];
            switch (op) {
                case 0: dp[(size_t)i] = std::atan2(a, b); break;
                case 1: dp[(size_t)i] = std::pow(a, b); break;
                case 3: dp[(size_t)i] = std::fmod(a, b); break;
                default: dp[(size_t)i] = (T)0; break;
            }
        }
    });
}

// Integer pow is exponentiation-by-squaring in the unsigned counterpart, so it
// wraps modulo 2^width to match Integer.pow truncated to the type; quotient/remainder
// are C / and % (toward zero, like Elixir div/rem), guarded against divide-by-zero.
template <typename T, typename Acc>
static void evision_binop_int(const cv::Mat &l, const cv::Mat &r, cv::Mat &dst, int op) {
    const T *lp = (const T *)l.data;
    const T *rp = (const T *)r.data;
    T *dp = (T *)dst.data;
    size_t n = l.total();
    evision_parallel_for((int64_t)n, (int64_t)n, EVISION_PARALLEL_SIMPLE_MIN_WORK, [&](int64_t begin, int64_t end) {
        for (int64_t i = begin; i < end; i++) {
            T a = lp[(size_t)i], b = rp[(size_t)i];
            switch (op) {
                case 1: {
                    Acc base = (Acc)a, result = 1;
                    T e = b;
                    while (e > 0) {
                        if (e & 1) result = (Acc)(result * base);
                        base = (Acc)(base * base);
                        e >>= 1;
                    }
                    dp[(size_t)i] = (T)result;
                    break;
                }
                case 2: dp[(size_t)i] = (b == 0) ? (T)0 : (T)(a / b); break;
                case 3: dp[(size_t)i] = (b == 0) ? (T)0 : (T)(a % b); break;
                default: dp[(size_t)i] = (T)0; break;
            }
        }
    });
}

// @evision c: mat_binop, evision_cv_mat_binop, 1
// @evision nif: def mat_binop(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_binop(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat l, r;
        int op = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", ArgInfo::INPUT_ONLY)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", ArgInfo::INPUT_ONLY)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "op"), op, ArgInfo("op", 0))) {
            Mat lc = l.isContinuous() ? l : l.clone();
            Mat rc = r.isContinuous() ? r : r.clone();
            Mat dst = lc.clone();
            switch (dst.depth()) {
                case CV_8U:  evision_binop_int<uint8_t, uint8_t>(lc, rc, dst, op); break;
                case CV_8S:  evision_binop_int<int8_t, uint8_t>(lc, rc, dst, op); break;
                case CV_16U: evision_binop_int<uint16_t, uint16_t>(lc, rc, dst, op); break;
                case CV_16S: evision_binop_int<int16_t, uint16_t>(lc, rc, dst, op); break;
                case CV_32S: evision_binop_int<int32_t, uint32_t>(lc, rc, dst, op); break;
                case CV_32U: evision_binop_int<uint32_t, uint32_t>(lc, rc, dst, op); break;
                case CV_64S: evision_binop_int<int64_t, uint64_t>(lc, rc, dst, op); break;
                case CV_64U: evision_binop_int<uint64_t, uint64_t>(lc, rc, dst, op); break;
                case CV_32F: evision_binop_float<float>(lc, rc, dst, op); break;
                case CV_64F: evision_binop_float<double>(lc, rc, dst, op); break;
                default: return evision::nif::error(env, "mat_binop: unsupported depth");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_BINOP_H
