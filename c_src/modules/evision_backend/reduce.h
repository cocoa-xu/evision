#ifndef EVISION_BACKEND_REDUCE_H
#define EVISION_BACKEND_REDUCE_H

#include <cmath>
#include <cstdint>
#include <type_traits>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"
#include "parallel.h"

// Reduction of a 2D single-channel mat: sum (op 0) / product (op 1) accumulate
// left-to-right, max (op 2) / min (op 3) seed from the first element. axis 1
// reduces each row (trailing axis) into a [rows, 1] result; axis 0 reduces each
// column (leading axis) into a [1, cols] result -- the caller picks axis 0 when
// the reduced axes are leading so no transpose is needed.
//
// The input keeps its native dtype: sum/product promote each element to the wide
// accumulator (f64 for floats, s64/u64 for ints, matching Nx integer promotion
// and f64 float accumulation) in-register, so there is no separate cast pass and
// only the native bytes are streamed. max/min do not promote (Nx keeps the type).

template <typename T>
static inline T evision_reduce_max(T acc, T v) {
    if constexpr (std::is_floating_point<T>::value) {
        if (std::isnan(acc) || std::isnan(v)) return std::isnan(acc) ? acc : v;
    }
    return (v > acc) ? v : acc;
}

template <typename T>
static inline T evision_reduce_min(T acc, T v) {
    if constexpr (std::is_floating_point<T>::value) {
        if (std::isnan(acc) || std::isnan(v)) return std::isnan(acc) ? acc : v;
    }
    return (v < acc) ? v : acc;
}

// Wide accumulator for sum/product: floats accumulate in f64, signed ints in
// s64, unsigned ints in u64 (unsigned wraparound is well-defined and matches
// Nx's modular integer semantics).
template <typename T> struct evision_reduce_acc { using type = T; };
template <> struct evision_reduce_acc<float>    { using type = double; };
template <> struct evision_reduce_acc<int8_t>   { using type = int64_t; };
template <> struct evision_reduce_acc<int16_t>  { using type = int64_t; };
template <> struct evision_reduce_acc<int32_t>  { using type = int64_t; };
template <> struct evision_reduce_acc<uint8_t>  { using type = uint64_t; };
template <> struct evision_reduce_acc<uint16_t> { using type = uint64_t; };
template <> struct evision_reduce_acc<uint32_t> { using type = uint64_t; };

template <typename Tin, typename Tacc>
static void evision_reduce_rows(const cv::Mat &src, cv::Mat &dst, int op) {
    int cols = src.cols;
    evision_parallel_for(src.rows, (int64_t)src.rows * cols, EVISION_PARALLEL_SIMPLE_MIN_WORK, [&](int64_t begin, int64_t end) {
        for (int64_t row = begin; row < end; row++) {
            const Tin *sp = src.ptr<Tin>((int)row);
            Tacc acc = (op == 1) ? (Tacc)1 : (op == 2 || op == 3) ? (Tacc)sp[0] : (Tacc)0;
            int start = (op == 2 || op == 3) ? 1 : 0;
            switch (op) {
                case 0: for (int c = start; c < cols; c++) acc = (Tacc)(acc + (Tacc)sp[c]); break;
                case 1: for (int c = start; c < cols; c++) acc = (Tacc)(acc * (Tacc)sp[c]); break;
                case 2: for (int c = start; c < cols; c++) acc = evision_reduce_max(acc, (Tacc)sp[c]); break;
                case 3: for (int c = start; c < cols; c++) acc = evision_reduce_min(acc, (Tacc)sp[c]); break;
            }
            dst.ptr<Tacc>((int)row)[0] = acc;
        }
    });
}

template <typename Tin, typename Tacc>
static void evision_reduce_cols(const cv::Mat &src, cv::Mat &dst, int op) {
    int rows = src.rows, cols = src.cols;
    const Tin *sp = (const Tin *)src.data;
    Tacc *dp = (Tacc *)dst.data;
    evision_parallel_for(cols, (int64_t)rows * cols, EVISION_PARALLEL_SIMPLE_MIN_WORK, [&](int64_t cb, int64_t ce) {
        switch (op) {
            case 1: for (int64_t c = cb; c < ce; c++) dp[c] = (Tacc)1; break;
            case 2: case 3: for (int64_t c = cb; c < ce; c++) dp[c] = (Tacc)sp[c]; break;
            default: for (int64_t c = cb; c < ce; c++) dp[c] = (Tacc)0; break;
        }
        int rstart = (op == 2 || op == 3) ? 1 : 0;
        for (int r = rstart; r < rows; r++) {
            const Tin *row = sp + (int64_t)r * cols;
            switch (op) {
                case 0: for (int64_t c = cb; c < ce; c++) dp[c] = (Tacc)(dp[c] + (Tacc)row[c]); break;
                case 1: for (int64_t c = cb; c < ce; c++) dp[c] = (Tacc)(dp[c] * (Tacc)row[c]); break;
                case 2: for (int64_t c = cb; c < ce; c++) dp[c] = evision_reduce_max(dp[c], (Tacc)row[c]); break;
                case 3: for (int64_t c = cb; c < ce; c++) dp[c] = evision_reduce_min(dp[c], (Tacc)row[c]); break;
            }
        }
    });
}

template <typename Tin>
static void evision_reduce_typed(const cv::Mat &src, cv::Mat &dst, int op, bool leading) {
    if (op == 2 || op == 3) {
        if (leading) evision_reduce_cols<Tin, Tin>(src, dst, op);
        else evision_reduce_rows<Tin, Tin>(src, dst, op);
    } else {
        using Tacc = typename evision_reduce_acc<Tin>::type;
        if (leading) evision_reduce_cols<Tin, Tacc>(src, dst, op);
        else evision_reduce_rows<Tin, Tacc>(src, dst, op);
    }
}

// Output depth: sum/product promote to the wide type; max/min keep the input type.
static inline int evision_reduce_out_depth(int in_depth, int op) {
    if (op == 2 || op == 3) return in_depth;
    switch (in_depth) {
        case CV_32F: case CV_64F: return CV_64F;
        case CV_8U: case CV_16U: case CV_32U: case CV_64U: return CV_64U;
        default: return CV_64S;
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
        int op = 0, axis = 1;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", ArgInfo::INPUT_ONLY)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "op"), op, ArgInfo("op", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "axis"), axis, ArgInfo("axis", 0))) {
            Mat src_c = src.isContinuous() ? src : src.clone();
            bool leading = (axis == 0);
            int out_depth = evision_reduce_out_depth(src_c.depth(), op);
            Mat dst = leading ? Mat(1, src_c.cols, out_depth) : Mat(src_c.rows, 1, out_depth);

            switch (src_c.depth()) {
                case CV_8U:  evision_reduce_typed<uint8_t>(src_c, dst, op, leading); break;
                case CV_8S:  evision_reduce_typed<int8_t>(src_c, dst, op, leading); break;
                case CV_16U: evision_reduce_typed<uint16_t>(src_c, dst, op, leading); break;
                case CV_16S: evision_reduce_typed<int16_t>(src_c, dst, op, leading); break;
                case CV_32S: evision_reduce_typed<int32_t>(src_c, dst, op, leading); break;
                case CV_32U: evision_reduce_typed<uint32_t>(src_c, dst, op, leading); break;
                case CV_64S: evision_reduce_typed<int64_t>(src_c, dst, op, leading); break;
                case CV_64U: evision_reduce_typed<uint64_t>(src_c, dst, op, leading); break;
                case CV_32F: evision_reduce_typed<float>(src_c, dst, op, leading); break;
                case CV_64F: evision_reduce_typed<double>(src_c, dst, op, leading); break;
                default: return evision::nif::error(env, "mat_reduce: unsupported depth");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_REDUCE_H
