#ifndef EVISION_BACKEND_UNARY_MATH_H
#define EVISION_BACKEND_UNARY_MATH_H

#include <cmath>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"
#include "parallel.h"

// Element-wise unary float math (Nx trig / hyperbolic / erf / etc.) that OpenCV
// has no primitive for. The caller passes a single-channel f32/f64 mat already
// cast to the output type; ops that produce floats from any input. Op codes match
// Evision.Backend's @uop_* constants.
template <typename T>
static void evision_unary_math(cv::Mat &m, int op) {
    T *p = (T *)m.data;
    size_t n = m.total();
    evision_parallel_for((int64_t)n, (int64_t)n, EVISION_PARALLEL_SIMPLE_MIN_WORK, [&](int64_t begin, int64_t end) {
        for (int64_t i = begin; i < end; i++) {
            T x = p[(size_t)i];
            switch (op) {
                case 0:  p[(size_t)i] = std::sin(x); break;
                case 1:  p[(size_t)i] = std::cos(x); break;
                case 2:  p[(size_t)i] = std::tan(x); break;
                case 3:  p[(size_t)i] = std::asin(x); break;
                case 4:  p[(size_t)i] = std::acos(x); break;
                case 5:  p[(size_t)i] = std::atan(x); break;
                case 6:  p[(size_t)i] = std::sinh(x); break;
                case 7:  p[(size_t)i] = std::cosh(x); break;
                case 8:  p[(size_t)i] = std::tanh(x); break;
                case 9:  p[(size_t)i] = std::asinh(x); break;
                case 10: p[(size_t)i] = std::acosh(x); break;
                case 11: p[(size_t)i] = std::atanh(x); break;
                case 12: p[(size_t)i] = std::erf(x); break;
                case 13: p[(size_t)i] = std::erfc(x); break;
                case 14: p[(size_t)i] = std::cbrt(x); break;
                case 15: p[(size_t)i] = std::log1p(x); break;
                case 16: p[(size_t)i] = (T)1 / std::sqrt(x); break;
                case 17: p[(size_t)i] = (T)1 / ((T)1 + std::exp(-x)); break;
                default: break;
            }
        }
    });
}

// @evision c: mat_unary_math, evision_cv_mat_unary_math, 1
// @evision nif: def mat_unary_math(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_unary_math(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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
            Mat dst = src.clone();
            switch (dst.depth()) {
                case CV_32F: evision_unary_math<float>(dst, op); break;
                case CV_64F: evision_unary_math<double>(dst, op); break;
                default: return evision::nif::error(env, "mat_unary_math: expected f32 or f64");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_UNARY_MATH_H
