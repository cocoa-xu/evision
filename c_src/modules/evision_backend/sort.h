#ifndef EVISION_BACKEND_SORT_H
#define EVISION_BACKEND_SORT_H

#include <algorithm>
#include <numeric>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

// Row-wise sort/argsort for the wide integer depths (CV_32U/CV_64S/CV_64U) that
// cv::sort/cv::sortIdx do not support. Input is a 2D single-channel Mat; each row
// is sorted independently. argsort uses a stable sort so ties keep ascending
// index order, matching Nx (and Nx.BinaryBackend) for both directions.

template <typename T>
static void evision_sort_rows(const cv::Mat &src, cv::Mat &dst, bool descending) {
    for (int r = 0; r < src.rows; r++) {
        const T *sp = src.ptr<T>(r);
        T *dp = dst.ptr<T>(r);
        std::copy(sp, sp + src.cols, dp);
        if (descending) std::sort(dp, dp + src.cols, std::greater<T>());
        else std::sort(dp, dp + src.cols, std::less<T>());
    }
}

template <typename T>
static void evision_argsort_rows(const cv::Mat &src, cv::Mat &dst, bool descending) {
    for (int r = 0; r < src.rows; r++) {
        const T *sp = src.ptr<T>(r);
        int32_t *dp = dst.ptr<int32_t>(r);
        std::iota(dp, dp + src.cols, 0);
        if (descending)
            std::stable_sort(dp, dp + src.cols, [sp](int32_t a, int32_t b) { return sp[a] > sp[b]; });
        else
            std::stable_sort(dp, dp + src.cols, [sp](int32_t a, int32_t b) { return sp[a] < sp[b]; });
    }
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
                default: return evision::nif::error(env, "mat_argsort_rows: unsupported depth");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_SORT_H
