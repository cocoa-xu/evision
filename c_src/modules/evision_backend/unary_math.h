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
template <typename T, int Op>
static void evision_unary_math_impl(const cv::Mat& src, cv::Mat& dst) {
    const cv::Mat* arrays[] = {&src, &dst};
    uchar* pointers[2];
    cv::NAryMatIterator it(arrays, pointers, 2);
    for (size_t plane = 0; plane < it.nplanes; ++plane, ++it) {
        const T* sp = (const T*)pointers[0];
        T* dp = (T*)pointers[1];
        const size_t n = it.size;
        evision_parallel_for((int64_t)n, (int64_t)n, EVISION_PARALLEL_SIMPLE_MIN_WORK, [&](int64_t begin, int64_t end) {
            for (int64_t i = begin; i < end; ++i) {
                const T x = sp[(size_t)i];
                if constexpr (Op == 0) dp[(size_t)i] = std::sin(x);
                else if constexpr (Op == 1) dp[(size_t)i] = std::cos(x);
                else if constexpr (Op == 2) dp[(size_t)i] = std::tan(x);
                else if constexpr (Op == 3) dp[(size_t)i] = std::asin(x);
                else if constexpr (Op == 4) dp[(size_t)i] = std::acos(x);
                else if constexpr (Op == 5) dp[(size_t)i] = std::atan(x);
                else if constexpr (Op == 6) dp[(size_t)i] = std::sinh(x);
                else if constexpr (Op == 7) dp[(size_t)i] = std::cosh(x);
                else if constexpr (Op == 8) dp[(size_t)i] = std::tanh(x);
                else if constexpr (Op == 9) dp[(size_t)i] = std::asinh(x);
                else if constexpr (Op == 10) dp[(size_t)i] = std::acosh(x);
                else if constexpr (Op == 11) dp[(size_t)i] = std::atanh(x);
                else if constexpr (Op == 12) dp[(size_t)i] = std::erf(x);
                else if constexpr (Op == 13) dp[(size_t)i] = std::erfc(x);
                else if constexpr (Op == 14) dp[(size_t)i] = std::cbrt(x);
                else if constexpr (Op == 15) dp[(size_t)i] = std::log1p(x);
                else if constexpr (Op == 16) dp[(size_t)i] = (T)1 / std::sqrt(x);
                else if constexpr (Op == 17) dp[(size_t)i] = (T)1 / ((T)1 + std::exp(-x));
            }
        });
    }
}

template <typename T>
static void evision_unary_math(const cv::Mat& src, cv::Mat& dst, int op) {
    switch (op) {
        case 0: evision_unary_math_impl<T, 0>(src, dst); break;
        case 1: evision_unary_math_impl<T, 1>(src, dst); break;
        case 2: evision_unary_math_impl<T, 2>(src, dst); break;
        case 3: evision_unary_math_impl<T, 3>(src, dst); break;
        case 4: evision_unary_math_impl<T, 4>(src, dst); break;
        case 5: evision_unary_math_impl<T, 5>(src, dst); break;
        case 6: evision_unary_math_impl<T, 6>(src, dst); break;
        case 7: evision_unary_math_impl<T, 7>(src, dst); break;
        case 8: evision_unary_math_impl<T, 8>(src, dst); break;
        case 9: evision_unary_math_impl<T, 9>(src, dst); break;
        case 10: evision_unary_math_impl<T, 10>(src, dst); break;
        case 11: evision_unary_math_impl<T, 11>(src, dst); break;
        case 12: evision_unary_math_impl<T, 12>(src, dst); break;
        case 13: evision_unary_math_impl<T, 13>(src, dst); break;
        case 14: evision_unary_math_impl<T, 14>(src, dst); break;
        case 15: evision_unary_math_impl<T, 15>(src, dst); break;
        case 16: evision_unary_math_impl<T, 16>(src, dst); break;
        case 17: evision_unary_math_impl<T, 17>(src, dst); break;
    }
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

        if (!evision_to_safe(env, evision_get_kw(env, erl_terms, "op"), op, ArgInfo("op", 0)))
            return enif_make_badarg(env);
        if (op < 0 || op > 17) return error(env, "mat_unary_math: invalid operation");

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", ArgInfo::BORROWED_INPUT))) {
            const int depth = src.depth();
            if (src.empty() || src.channels() != 1 || (depth != CV_32F && depth != CV_64F))
                return error(env, "mat_unary_math: expected a single-channel f32 or f64 matrix");
            // Read the source once and write directly into private output storage.
            Mat dst(src.dims, src.size.p, src.type());
            if (depth == CV_32F) evision_unary_math<float>(src, dst, op);
            else evision_unary_math<double>(src, dst, op);
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_UNARY_MATH_H
