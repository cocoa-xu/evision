#ifndef EVISION_BACKEND_WINDOW_H
#define EVISION_BACKEND_WINDOW_H

#include <algorithm>
#include <cmath>
#include <limits>
#include <type_traits>
#include <vector>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"
#include "parallel.h"

// Sliding-window ops (Nx.window_*). Windows preserve the input dtype (no accumulator
// upcast, unlike reduce), so everything is templated over the element type T; f16/bf16 are
// widened to f32 by the caller. Padding is not materialized: out-of-range source coords are
// skipped, which equals folding the per-op identity (0 sum / 1 product / -inf max / +inf min)
// that Nx pads with. op: 0 sum, 1 product, 2 max, 3 min.

template <typename T>
static inline T window_max_value(T acc, T v) {
    if constexpr (std::is_floating_point<T>::value) {
        if (std::isnan(acc) || std::isnan(v)) return std::isnan(acc) ? acc : v;
    }
    return (v > acc) ? v : acc;
}

template <typename T>
static inline T window_min_value(T acc, T v) {
    if constexpr (std::is_floating_point<T>::value) {
        if (std::isnan(acc) || std::isnan(v)) return std::isnan(acc) ? acc : v;
    }
    return (v < acc) ? v : acc;
}

template <typename T>
static T window_identity(int op) {
    switch (op) {
        case 1: return (T)1;
        case 2: return std::numeric_limits<T>::has_infinity
                           ? -std::numeric_limits<T>::infinity()
                           : std::numeric_limits<T>::lowest();
        case 3: return std::numeric_limits<T>::has_infinity
                           ? std::numeric_limits<T>::infinity()
                           : std::numeric_limits<T>::max();
        default: return (T)0;
    }
}

template <typename T>
static void window_reduce_typed(const T *sp, T *dp, int r,
                                const std::vector<int> &in_dims, const std::vector<int> &out_dims,
                                const std::vector<int> &win_dims, const std::vector<int> &strides,
                                const std::vector<int> &pad_lo, const std::vector<int> &dil, int op) {
    std::vector<int64_t> in_stride((size_t)r);
    int64_t acc = 1;
    for (int k = r - 1; k >= 0; k--) { in_stride[(size_t)k] = acc; acc *= in_dims[(size_t)k]; }

    int64_t out_total = 1, win_total = 1;
    for (int k = 0; k < r; k++) { out_total *= out_dims[(size_t)k]; win_total *= win_dims[(size_t)k]; }

    T init = window_identity<T>(op);
    int64_t work = out_total * (win_total > 0 ? win_total : 1);

    if (!evision_should_parallelize(out_total, work, EVISION_PARALLEL_NESTED_MIN_WORK)) {
        std::vector<int> op_idx((size_t)r, 0), w_idx((size_t)r, 0);
        for (int64_t p = 0; p < out_total; p++) {
            T av = init;
            std::fill(w_idx.begin(), w_idx.end(), 0);

            for (int64_t wq = 0; wq < win_total; wq++) {
                bool inrange = true;
                int64_t soff = 0;
                for (int k = 0; k < r; k++) {
                    int64_t s = (int64_t)op_idx[(size_t)k] * strides[(size_t)k] +
                                (int64_t)w_idx[(size_t)k] * dil[(size_t)k] - pad_lo[(size_t)k];
                    if (s < 0 || s >= in_dims[(size_t)k]) { inrange = false; break; }
                    soff += s * in_stride[(size_t)k];
                }
                if (inrange) {
                    T x = sp[soff];
                    switch (op) {
                        case 0: av = (T)(av + x); break;
                        case 1: av = (T)(av * x); break;
                        case 2: av = window_max_value(av, x); break;
                        case 3: av = window_min_value(av, x); break;
                    }
                }
                for (int k = r - 1; k >= 0; k--) { if (++w_idx[(size_t)k] < win_dims[(size_t)k]) break; w_idx[(size_t)k] = 0; }
            }

            dp[p] = av;
            for (int k = r - 1; k >= 0; k--) { if (++op_idx[(size_t)k] < out_dims[(size_t)k]) break; op_idx[(size_t)k] = 0; }
        }
        return;
    }

    evision_parallel_for(out_total, work, EVISION_PARALLEL_NESTED_MIN_WORK, [&](int64_t begin, int64_t end) {
        std::vector<int> op_idx((size_t)r, 0), w_idx((size_t)r, 0);
        int64_t q = begin;
        for (int k = r - 1; k >= 0; k--) {
            op_idx[(size_t)k] = (int)(q % out_dims[(size_t)k]);
            q /= out_dims[(size_t)k];
        }

        for (int64_t p = begin; p < end; p++) {
            T av = init;
            std::fill(w_idx.begin(), w_idx.end(), 0);

            for (int64_t wq = 0; wq < win_total; wq++) {
                bool inrange = true;
                int64_t soff = 0;
                for (int k = 0; k < r; k++) {
                    int64_t s = (int64_t)op_idx[(size_t)k] * strides[(size_t)k] +
                                (int64_t)w_idx[(size_t)k] * dil[(size_t)k] - pad_lo[(size_t)k];
                    if (s < 0 || s >= in_dims[(size_t)k]) { inrange = false; break; }
                    soff += s * in_stride[(size_t)k];
                }
                if (inrange) {
                    T x = sp[soff];
                    switch (op) {
                        case 0: av = (T)(av + x); break;
                        case 1: av = (T)(av * x); break;
                        case 2: av = window_max_value(av, x); break;
                        case 3: av = window_min_value(av, x); break;
                    }
                }
                for (int k = r - 1; k >= 0; k--) { if (++w_idx[(size_t)k] < win_dims[(size_t)k]) break; w_idx[(size_t)k] = 0; }
            }

            dp[p] = av;
            for (int k = r - 1; k >= 0; k--) { if (++op_idx[(size_t)k] < out_dims[(size_t)k]) break; op_idx[(size_t)k] = 0; }
        }
    });
}

#define EVISION_WINDOW_DISPATCH(FN, SRC, DST, ...)                              \
    switch ((SRC).depth()) {                                                    \
        case CV_8U:  FN<uint8_t>((const uint8_t *)(SRC).data, (uint8_t *)(DST).data, __VA_ARGS__); break;   \
        case CV_8S:  FN<int8_t>((const int8_t *)(SRC).data, (int8_t *)(DST).data, __VA_ARGS__); break;      \
        case CV_16U: FN<uint16_t>((const uint16_t *)(SRC).data, (uint16_t *)(DST).data, __VA_ARGS__); break;\
        case CV_16S: FN<int16_t>((const int16_t *)(SRC).data, (int16_t *)(DST).data, __VA_ARGS__); break;   \
        case CV_32S: FN<int32_t>((const int32_t *)(SRC).data, (int32_t *)(DST).data, __VA_ARGS__); break;   \
        case CV_32U: FN<uint32_t>((const uint32_t *)(SRC).data, (uint32_t *)(DST).data, __VA_ARGS__); break;\
        case CV_64S: FN<int64_t>((const int64_t *)(SRC).data, (int64_t *)(DST).data, __VA_ARGS__); break;   \
        case CV_64U: FN<uint64_t>((const uint64_t *)(SRC).data, (uint64_t *)(DST).data, __VA_ARGS__); break;\
        case CV_32F: FN<float>((const float *)(SRC).data, (float *)(DST).data, __VA_ARGS__); break;         \
        case CV_64F: FN<double>((const double *)(SRC).data, (double *)(DST).data, __VA_ARGS__); break;      \
        default: return evision::nif::error(env, "window: unsupported depth");                              \
    }

// @evision c: mat_window_reduce, evision_cv_mat_window_reduce, 1
// @evision nif: def mat_window_reduce(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_window_reduce(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat src;
        std::vector<int> in_dims, out_dims, win_dims, strides, pad_lo, dil;
        int op = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "in_dims"), in_dims, ArgInfo("in_dims", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "out_dims"), out_dims, ArgInfo("out_dims", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "win_dims"), win_dims, ArgInfo("win_dims", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "strides"), strides, ArgInfo("strides", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "pad_lo"), pad_lo, ArgInfo("pad_lo", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "dilations"), dil, ArgInfo("dilations", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "op"), op, ArgInfo("op", 0))) {
            int r = (int)in_dims.size();
            Mat src_c = src.isContinuous() ? src : src.clone();
            int64_t out_total = 1;
            for (int k = 0; k < r; k++) out_total *= out_dims[(size_t)k];
            Mat dst(1, (int)(out_total > 0 ? out_total : 1), src.type());

            EVISION_WINDOW_DISPATCH(window_reduce_typed, src_c, dst,
                                    r, in_dims, out_dims, win_dims, strides, pad_lo, dil, op)
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

// Select-and-scatter (Nx.window_scatter_max/min). For each window, pick the last position
// achieving the extreme (max via >=, min via <=) and add that window's source value there;
// duplicate targets accumulate. Output is the input shape, filled with init. op: 0 max, 1 min.
// `sp` (input, for selection) and `srcvals` (per-window values) are both in the output type T;
// dilation is always 1 for scatter.
template <typename T>
static void window_scatter_typed(const T *sp, const T *srcvals, T *dp, int r,
                                 const std::vector<int> &in_dims, const std::vector<int> &win_dims,
                                 const std::vector<int> &strides, const std::vector<int> &pad_lo,
                                 const std::vector<int> &grid, T init, int op) {
    std::vector<int64_t> in_stride((size_t)r);
    int64_t acc = 1;
    for (int k = r - 1; k >= 0; k--) { in_stride[(size_t)k] = acc; acc *= in_dims[(size_t)k]; }

    int64_t in_total = 1, grid_total = 1, win_total = 1;
    for (int k = 0; k < r; k++) {
        in_total *= in_dims[(size_t)k];
        grid_total *= grid[(size_t)k];
        win_total *= win_dims[(size_t)k];
    }
    evision_parallel_for(in_total, in_total, EVISION_PARALLEL_SIMPLE_MIN_WORK, [&](int64_t begin, int64_t end) {
        for (int64_t i = begin; i < end; i++) dp[i] = init;
    });

    std::vector<int> g_idx((size_t)r, 0), w_idx((size_t)r, 0);
    for (int64_t p = 0; p < grid_total; p++) {
        bool seeded = false;
        T best = init;
        int64_t best_off = -1;
        std::fill(w_idx.begin(), w_idx.end(), 0);

        for (int64_t wq = 0; wq < win_total; wq++) {
            bool inrange = true;
            int64_t soff = 0;
            for (int k = 0; k < r; k++) {
                int64_t s = (int64_t)g_idx[(size_t)k] * strides[(size_t)k] + w_idx[(size_t)k] - pad_lo[(size_t)k];
                if (s < 0 || s >= in_dims[(size_t)k]) { inrange = false; break; }
                soff += s * in_stride[(size_t)k];
            }
            T val = inrange ? sp[soff] : init;
            if (!seeded) {
                best = val;
                best_off = inrange ? soff : -1;
                seeded = true;
            } else {
                bool sel = (op == 0) ? (val >= best) : (val <= best);
                if (sel) { best = val; best_off = inrange ? soff : -1; }
            }
            for (int k = r - 1; k >= 0; k--) { if (++w_idx[(size_t)k] < win_dims[(size_t)k]) break; w_idx[(size_t)k] = 0; }
        }

        if (best_off >= 0) dp[best_off] = (T)(dp[best_off] + srcvals[p]);
        for (int k = r - 1; k >= 0; k--) { if (++g_idx[(size_t)k] < grid[(size_t)k]) break; g_idx[(size_t)k] = 0; }
    }
}

// @evision c: mat_window_scatter, evision_cv_mat_window_scatter, 1
// @evision nif: def mat_window_scatter(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_window_scatter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat src, source;
        std::vector<int> in_dims, win_dims, strides, pad_lo, grid;
        double init = 0.0;
        int op = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "source"), source, ArgInfo("source", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "init"), init, ArgInfo("init", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "in_dims"), in_dims, ArgInfo("in_dims", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "win_dims"), win_dims, ArgInfo("win_dims", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "strides"), strides, ArgInfo("strides", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "pad_lo"), pad_lo, ArgInfo("pad_lo", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "grid"), grid, ArgInfo("grid", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "op"), op, ArgInfo("op", 0))) {
            int r = (int)in_dims.size();
            Mat src_c = src.isContinuous() ? src : src.clone();
            Mat source_c = source.isContinuous() ? source : source.clone();
            int64_t in_total = 1;
            for (int k = 0; k < r; k++) in_total *= in_dims[(size_t)k];
            Mat dst(1, (int)(in_total > 0 ? in_total : 1), src.type());

            switch (src.depth()) {
                case CV_8U:  window_scatter_typed<uint8_t>((const uint8_t *)src_c.data, (const uint8_t *)source_c.data, (uint8_t *)dst.data, r, in_dims, win_dims, strides, pad_lo, grid, (uint8_t)init, op); break;
                case CV_8S:  window_scatter_typed<int8_t>((const int8_t *)src_c.data, (const int8_t *)source_c.data, (int8_t *)dst.data, r, in_dims, win_dims, strides, pad_lo, grid, (int8_t)init, op); break;
                case CV_16U: window_scatter_typed<uint16_t>((const uint16_t *)src_c.data, (const uint16_t *)source_c.data, (uint16_t *)dst.data, r, in_dims, win_dims, strides, pad_lo, grid, (uint16_t)init, op); break;
                case CV_16S: window_scatter_typed<int16_t>((const int16_t *)src_c.data, (const int16_t *)source_c.data, (int16_t *)dst.data, r, in_dims, win_dims, strides, pad_lo, grid, (int16_t)init, op); break;
                case CV_32S: window_scatter_typed<int32_t>((const int32_t *)src_c.data, (const int32_t *)source_c.data, (int32_t *)dst.data, r, in_dims, win_dims, strides, pad_lo, grid, (int32_t)init, op); break;
                case CV_32U: window_scatter_typed<uint32_t>((const uint32_t *)src_c.data, (const uint32_t *)source_c.data, (uint32_t *)dst.data, r, in_dims, win_dims, strides, pad_lo, grid, (uint32_t)init, op); break;
                case CV_64S: window_scatter_typed<int64_t>((const int64_t *)src_c.data, (const int64_t *)source_c.data, (int64_t *)dst.data, r, in_dims, win_dims, strides, pad_lo, grid, (int64_t)init, op); break;
                case CV_64U: window_scatter_typed<uint64_t>((const uint64_t *)src_c.data, (const uint64_t *)source_c.data, (uint64_t *)dst.data, r, in_dims, win_dims, strides, pad_lo, grid, (uint64_t)init, op); break;
                case CV_32F: window_scatter_typed<float>((const float *)src_c.data, (const float *)source_c.data, (float *)dst.data, r, in_dims, win_dims, strides, pad_lo, grid, (float)init, op); break;
                case CV_64F: window_scatter_typed<double>((const double *)src_c.data, (const double *)source_c.data, (double *)dst.data, r, in_dims, win_dims, strides, pad_lo, grid, init, op); break;
                default: return evision::nif::error(env, "window_scatter: unsupported depth");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_WINDOW_H
