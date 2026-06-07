#ifndef EVISION_BACKEND_CUMULATIVE_H
#define EVISION_BACKEND_CUMULATIVE_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

// Row-wise prefix scan (Nx.cumulative_*): op 0=sum, 1=product, 2=min, 3=max,
// accumulating left-to-right (or right-to-left when reverse). Output has the same
// shape and type as the 2D single-channel input, which the caller has already cast
// to the output type; integer sum/product use an unsigned accumulator so wraparound
// is well-defined (two's complement), matching Nx's modular semantics.
template <typename T, typename Acc>
static void evision_scan_rows(const cv::Mat &src, cv::Mat &dst, int op, bool reverse) {
    int cols = src.cols;
    for (int r = 0; r < src.rows; r++) {
        const T *sp = src.ptr<T>(r);
        T *dp = dst.ptr<T>(r);
        if (cols == 0) continue;
        int start = reverse ? cols - 1 : 0;
        int step = reverse ? -1 : 1;
        T running = sp[start];
        dp[start] = running;
        for (int k = 1; k < cols; k++) {
            int c = start + step * k;
            T v = sp[c];
            switch (op) {
                case 0: running = (T)((Acc)running + (Acc)v); break;
                case 1: running = (T)((Acc)running * (Acc)v); break;
                case 2: running = (v < running) ? v : running; break;
                case 3: running = (v > running) ? v : running; break;
            }
            dp[c] = running;
        }
    }
}

// @evision c: mat_cumulative, evision_cv_mat_cumulative, 1
// @evision nif: def mat_cumulative(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_cumulative(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat src;
        int op = 0, reverse = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "op"), op, ArgInfo("op", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "reverse"), reverse, ArgInfo("reverse", 0))) {
            Mat dst(src.rows, src.cols, src.type());
            bool rev = reverse != 0;
            switch (src.depth()) {
                case CV_8U:  evision_scan_rows<uint8_t, uint32_t>(src, dst, op, rev); break;
                case CV_8S:  evision_scan_rows<int8_t, uint32_t>(src, dst, op, rev); break;
                case CV_16U: evision_scan_rows<uint16_t, uint32_t>(src, dst, op, rev); break;
                case CV_16S: evision_scan_rows<int16_t, uint32_t>(src, dst, op, rev); break;
                case CV_32S: evision_scan_rows<int32_t, uint32_t>(src, dst, op, rev); break;
                case CV_32U: evision_scan_rows<uint32_t, uint32_t>(src, dst, op, rev); break;
                case CV_64S: evision_scan_rows<int64_t, uint64_t>(src, dst, op, rev); break;
                case CV_64U: evision_scan_rows<uint64_t, uint64_t>(src, dst, op, rev); break;
                case CV_32F: evision_scan_rows<float, float>(src, dst, op, rev); break;
                case CV_64F: evision_scan_rows<double, double>(src, dst, op, rev); break;
                default: return evision::nif::error(env, "mat_cumulative: unsupported depth");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_CUMULATIVE_H
