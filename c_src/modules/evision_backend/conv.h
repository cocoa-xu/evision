#ifndef EVISION_BACKEND_CONV_H
#define EVISION_BACKEND_CONV_H

#include <vector>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"
#include "parallel.h"

// N-d cross-correlation (Nx.conv) on already-permuted canonical layouts: input [N, Cin,
// *spatial], kernel [O, Cin/G, *spatial], output [N/Bg, O, *spatial]. The caller does the
// input/kernel/output permutations (via transpose) and the float cast; this computes the
// canonical result. Dilations/padding are handled by index math (no materialized zeros), so
// only original kernel positions that land on a real input element contribute -- summed in
// channel-major, spatial-minor order to match Nx exactly. G = feature groups, Bg = batch groups.

template <typename T>
static void conv_typed(const T *I, const T *K, T *Out, int d,
                       const std::vector<int> &in_shape, const std::vector<int> &k_shape,
                       const std::vector<int> &out_shape, const std::vector<int> &stride,
                       const std::vector<int> &pad_lo, const std::vector<int> &in_dil,
                       const std::vector<int> &k_dil, int G, int Bg) {
    int rank = d + 2;
    int Cin_pg = k_shape[1];
    int O = k_shape[0];
    int Nout = out_shape[0];
    int O_per_G = O / G;
    int O_per_Bg = O / Bg;

    auto strides_of = [&](const std::vector<int> &shp) {
        std::vector<int64_t> st((size_t)rank);
        int64_t a = 1;
        for (int i = rank - 1; i >= 0; i--) { st[(size_t)i] = a; a *= shp[(size_t)i]; }
        return st;
    };
    std::vector<int64_t> in_st = strides_of(in_shape);
    std::vector<int64_t> k_st = strides_of(k_shape);

    std::vector<int64_t> dilated_in((size_t)d);
    for (int s = 0; s < d; s++) dilated_in[(size_t)s] = (int64_t)(in_shape[(size_t)(2 + s)] - 1) * in_dil[(size_t)s] + 1;

    int64_t out_total = 1;
    for (int i = 0; i < rank; i++) out_total *= out_shape[(size_t)i];
    int64_t k_spatial = 1;
    for (int s = 0; s < d; s++) k_spatial *= k_shape[(size_t)(2 + s)];

    int64_t work = out_total * Cin_pg * (k_spatial > 0 ? k_spatial : 1);
    evision_parallel_for(out_total, work, EVISION_PARALLEL_CONV_MIN_WORK, [&](int64_t begin, int64_t end) {
        std::vector<int> oidx((size_t)rank, 0), kp((size_t)d, 0);
        int64_t q = begin;
        for (int s = rank - 1; s >= 0; s--) {
            oidx[(size_t)s] = (int)(q % out_shape[(size_t)s]);
            q /= out_shape[(size_t)s];
        }

        for (int64_t flat = begin; flat < end; flat++) {
            int bprime = oidx[0];
            int o = oidx[1];
            int fg = o / O_per_G;
            int jchunk = o / O_per_Bg;
            int n = bprime + jchunk * Nout;
            int cbase = fg * Cin_pg;

            T acc = (T)0;
            for (int ic = 0; ic < Cin_pg; ic++) {
                int64_t in_base = (int64_t)n * in_st[0] + (int64_t)(cbase + ic) * in_st[1];
                int64_t k_base = (int64_t)o * k_st[0] + (int64_t)ic * k_st[1];
                std::fill(kp.begin(), kp.end(), 0);

                for (int64_t kq = 0; kq < k_spatial; kq++) {
                    bool valid = true;
                    int64_t in_off = in_base;
                    for (int s = 0; s < d; s++) {
                        int64_t cd = (int64_t)oidx[(size_t)(2 + s)] * stride[(size_t)s] +
                                     (int64_t)kp[(size_t)s] * k_dil[(size_t)s] - pad_lo[(size_t)s];
                        if (cd < 0 || cd >= dilated_in[(size_t)s]) { valid = false; break; }
                        if (in_dil[(size_t)s] != 1 && (cd % in_dil[(size_t)s]) != 0) { valid = false; break; }
                        int64_t orig = (in_dil[(size_t)s] == 1) ? cd : cd / in_dil[(size_t)s];
                        in_off += orig * in_st[(size_t)(2 + s)];
                    }
                    if (valid) {
                        int64_t k_off = k_base;
                        for (int s = 0; s < d; s++) k_off += (int64_t)kp[(size_t)s] * k_st[(size_t)(2 + s)];
                        acc += I[in_off] * K[k_off];
                    }
                    for (int s = d - 1; s >= 0; s--) { if (++kp[(size_t)s] < k_shape[(size_t)(2 + s)]) break; kp[(size_t)s] = 0; }
                }
            }

            Out[flat] = acc;
            for (int s = rank - 1; s >= 0; s--) { if (++oidx[(size_t)s] < out_shape[(size_t)s]) break; oidx[(size_t)s] = 0; }
        }
    });
}

// @evision c: mat_conv, evision_cv_mat_conv, 1
// @evision nif: def mat_conv(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_conv(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat input, kernel;
        std::vector<int> in_shape, k_shape, out_shape, stride, pad_lo, in_dil, k_dil;
        int feature_groups = 1, batch_groups = 1;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "input"), input, ArgInfo("input", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "kernel"), kernel, ArgInfo("kernel", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "in_shape"), in_shape, ArgInfo("in_shape", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "k_shape"), k_shape, ArgInfo("k_shape", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "out_shape"), out_shape, ArgInfo("out_shape", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "strides"), stride, ArgInfo("strides", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "pad_lo"), pad_lo, ArgInfo("pad_lo", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "input_dilation"), in_dil, ArgInfo("input_dilation", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "kernel_dilation"), k_dil, ArgInfo("kernel_dilation", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "feature_groups"), feature_groups, ArgInfo("feature_groups", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "batch_groups"), batch_groups, ArgInfo("batch_groups", 0))) {
            int d = (int)in_shape.size() - 2;
            Mat in_c = input.isContinuous() ? input : input.clone();
            Mat k_c = kernel.isContinuous() ? kernel : kernel.clone();

            int64_t out_total = 1;
            for (size_t i = 0; i < out_shape.size(); i++) out_total *= out_shape[i];
            Mat dst(1, (int)(out_total > 0 ? out_total : 1), input.type());

            switch (input.depth()) {
                case CV_32F:
                    conv_typed<float>((const float *)in_c.data, (const float *)k_c.data, (float *)dst.data,
                                      d, in_shape, k_shape, out_shape, stride, pad_lo, in_dil, k_dil,
                                      feature_groups, batch_groups);
                    break;
                case CV_64F:
                    conv_typed<double>((const double *)in_c.data, (const double *)k_c.data, (double *)dst.data,
                                       d, in_shape, k_shape, out_shape, stride, pad_lo, in_dil, k_dil,
                                       feature_groups, batch_groups);
                    break;
                default:
                    return evision::nif::error(env, "conv: unsupported depth (expected f32/f64)");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_CONV_H
