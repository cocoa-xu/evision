#ifndef EVISION_BACKEND_BIT_UNARY_H
#define EVISION_BACKEND_BIT_UNARY_H

#include <cstdint>
#include <type_traits>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

static inline int evision_popcount_u64(uint64_t x) {
    int c = 0;
    while (x) { c += (int)(x & 1); x >>= 1; }
    return c;
}

static inline int evision_clz_width(uint64_t x, int width) {
    int c = 0;
    for (int b = width - 1; b >= 0; b--) {
        if (x & ((uint64_t)1 << b)) break;
        c++;
    }
    return c;
}

// Per-element bit op preserving the integer type (Nx.count_leading_zeros /
// population_count). op 0 = clz (within the type's bit width), 1 = popcount. The
// element's raw bits are read as unsigned, matching Nx's unsigned read; the count
// is written back in the same type.
template <typename T>
static void evision_bit_unary(cv::Mat &m, int op) {
    typedef typename std::make_unsigned<T>::type U;
    T *p = (T *)m.data;
    size_t n = m.total();
    int width = (int)(sizeof(T) * 8);
    for (size_t i = 0; i < n; i++) {
        uint64_t x = (uint64_t)(U)p[i];
        int r = (op == 0) ? evision_clz_width(x, width) : evision_popcount_u64(x);
        p[i] = (T)r;
    }
}

// @evision c: mat_bit_unary, evision_cv_mat_bit_unary, 1
// @evision nif: def mat_bit_unary(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_bit_unary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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
                case CV_8U:  evision_bit_unary<uint8_t>(dst, op); break;
                case CV_8S:  evision_bit_unary<int8_t>(dst, op); break;
                case CV_16U: evision_bit_unary<uint16_t>(dst, op); break;
                case CV_16S: evision_bit_unary<int16_t>(dst, op); break;
                case CV_32S: evision_bit_unary<int32_t>(dst, op); break;
                case CV_32U: evision_bit_unary<uint32_t>(dst, op); break;
                case CV_64S: evision_bit_unary<int64_t>(dst, op); break;
                case CV_64U: evision_bit_unary<uint64_t>(dst, op); break;
                default: return evision::nif::error(env, "mat_bit_unary: integer types only");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_BIT_UNARY_H
