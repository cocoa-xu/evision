#ifndef EVISION_BACKEND_SORT_H
#define EVISION_BACKEND_SORT_H

#include <algorithm>
#include <cmath>
#include <numeric>
#include <type_traits>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"
#include "parallel.h"

// Row-wise sort/argsort for depths that need custom handling. Floats use Nx's
// NaN order: ascending puts NaNs last, descending puts them first.

template <typename T>
static inline bool evision_sort_before(T a, T b, bool descending) {
    if constexpr (std::is_floating_point<T>::value) {
        bool a_nan = std::isnan(a);
        bool b_nan = std::isnan(b);
        if (a_nan || b_nan) {
            if (a_nan && b_nan) return false;
            return descending ? a_nan : b_nan;
        }
    }
    return descending ? (a > b) : (a < b);
}

template <typename T>
static void evision_sort_rows(const cv::Mat &src, cv::Mat &dst, bool descending) {
    evision_parallel_for(src.rows, (int64_t)src.rows * src.cols, EVISION_PARALLEL_SIMPLE_MIN_WORK, [&](int64_t begin, int64_t end) {
        for (int64_t row = begin; row < end; row++) {
            const T *sp = src.ptr<T>((int)row);
            T *dp = dst.ptr<T>((int)row);
            std::copy(sp, sp + src.cols, dp);
            std::sort(dp, dp + src.cols, [descending](T a, T b) {
                return evision_sort_before(a, b, descending);
            });
        }
    });
}

template <typename T>
static void evision_argsort_rows(const cv::Mat &src, cv::Mat &dst, bool descending) {
    evision_parallel_for(src.rows, (int64_t)src.rows * src.cols, EVISION_PARALLEL_SIMPLE_MIN_WORK, [&](int64_t begin, int64_t end) {
        for (int64_t row = begin; row < end; row++) {
            const T *sp = src.ptr<T>((int)row);
            int32_t *dp = dst.ptr<int32_t>((int)row);
            std::iota(dp, dp + src.cols, 0);
            std::stable_sort(dp, dp + src.cols, [sp, descending](int32_t a, int32_t b) {
                return evision_sort_before(sp[a], sp[b], descending);
            });
        }
    });
}

// @evision c: mat_sort_rows, evision_cv_mat_sort_rows, 1
// @evision nif: def mat_sort_rows(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_sort_rows(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat src;
        int descending = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "descending"), descending, ArgInfo("descending", 0))) {
            Mat dst(src.rows, src.cols, src.type());
            switch (src.depth()) {
                case CV_32U: evision_sort_rows<uint32_t>(src, dst, descending != 0); break;
                case CV_64S: evision_sort_rows<int64_t>(src, dst, descending != 0); break;
                case CV_64U: evision_sort_rows<uint64_t>(src, dst, descending != 0); break;
                case CV_32F: evision_sort_rows<float>(src, dst, descending != 0); break;
                case CV_64F: evision_sort_rows<double>(src, dst, descending != 0); break;
                default: return evision::nif::error(env, "mat_sort_rows: unsupported depth");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

// @evision c: mat_argsort_rows, evision_cv_mat_argsort_rows, 1
// @evision nif: def mat_argsort_rows(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_argsort_rows(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat src;
        int descending = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "descending"), descending, ArgInfo("descending", 0))) {
            Mat dst(src.rows, src.cols, CV_32S);
            switch (src.depth()) {
                case CV_32U: evision_argsort_rows<uint32_t>(src, dst, descending != 0); break;
                case CV_64S: evision_argsort_rows<int64_t>(src, dst, descending != 0); break;
                case CV_64U: evision_argsort_rows<uint64_t>(src, dst, descending != 0); break;
                case CV_32F: evision_argsort_rows<float>(src, dst, descending != 0); break;
                case CV_64F: evision_argsort_rows<double>(src, dst, descending != 0); break;
                default: return evision::nif::error(env, "mat_argsort_rows: unsupported depth");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_SORT_H
