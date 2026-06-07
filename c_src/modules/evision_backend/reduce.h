#ifndef EVISION_BACKEND_REDUCE_H
#define EVISION_BACKEND_REDUCE_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

// Row-wise sum (op 0) / product (op 1), accumulating left-to-right. Input is a 2D
// single-channel Mat already cast to the accumulator type (CV_64F / CV_64S /
// CV_64U) by the caller, so integer promotion (s64/u64, modular) and f64 float
// accumulation match Nx; output is [rows, 1] of the same type.

template <typename T>
static void evision_reduce_rows(const cv::Mat &src, cv::Mat &dst, int op) {
    for (int r = 0; r < src.rows; r++) {
        const T *sp = src.ptr<T>(r);
        T acc = (op == 1) ? (T)1 : (T)0;
        for (int c = 0; c < src.cols; c++) acc = (op == 1) ? (T)(acc * sp[c]) : (T)(acc + sp[c]);
        dst.ptr<T>(r)[0] = acc;
    }
}

// @evision c: mat_reduce, evision_cv_mat_reduce, 1
// @evision nif: def mat_reduce(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_reduce(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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
            Mat dst(src.rows, 1, src.type());
            switch (src.depth()) {
                case CV_64F: evision_reduce_rows<double>(src, dst, op); break;
                case CV_64S: evision_reduce_rows<int64_t>(src, dst, op); break;
                case CV_64U: evision_reduce_rows<uint64_t>(src, dst, op); break;
                default: return evision::nif::error(env, "mat_reduce: unsupported depth");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_REDUCE_H
