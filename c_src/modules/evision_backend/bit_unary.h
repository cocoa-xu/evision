#ifndef EVISION_BACKEND_BIT_UNARY_H
#define EVISION_BACKEND_BIT_UNARY_H

#include <cstdint>
#include <type_traits>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

static inline int evision_popcount_u64(uint64_t x) {
#if defined(__GNUC__) || defined(__clang__)
    return __builtin_popcountll(x);
#else
    x -= (x >> 1) & UINT64_C(0x5555555555555555);
    x = (x & UINT64_C(0x3333333333333333)) + ((x >> 2) & UINT64_C(0x3333333333333333));
    x = (x + (x >> 4)) & UINT64_C(0x0f0f0f0f0f0f0f0f);
    return (int)((x * UINT64_C(0x0101010101010101)) >> 56);
#endif
}

static inline int evision_clz_width(uint64_t x, int width) {
    // Compiler clz builtins are undefined for zero.
    if (x == 0) return width;
#if defined(__GNUC__) || defined(__clang__)
    return __builtin_clzll(x) - (64 - width);
#else
    int count = 0;
    if ((x >> 32) == 0) { count += 32; x <<= 32; }
    if ((x >> 48) == 0) { count += 16; x <<= 16; }
    if ((x >> 56) == 0) { count += 8; x <<= 8; }
    if ((x >> 60) == 0) { count += 4; x <<= 4; }
    if ((x >> 62) == 0) { count += 2; x <<= 2; }
    if ((x >> 63) == 0) ++count;
    return count - (64 - width);
#endif
}

// Per-element bit op preserving the integer type (Nx.count_leading_zeros /
// population_count). op 0 = clz (within the type's bit width), 1 = popcount. The
// element's raw bits are read as unsigned, matching Nx's unsigned read; the count
// is written back in the same type.
template <typename T, bool LeadingZeros>
static void evision_bit_unary_impl(const cv::Mat& src, cv::Mat& dst) {
    using U = typename std::make_unsigned<T>::type;
    const cv::Mat* arrays[] = {&src, &dst};
    uchar* pointers[2];
    cv::NAryMatIterator it(arrays, pointers, 2);
    for (size_t plane = 0; plane < it.nplanes; ++plane, ++it) {
        const T* sp = (const T*)pointers[0];
        T* dp = (T*)pointers[1];
        const size_t n = it.size;
        for (size_t i = 0; i < n; ++i) {
            const uint64_t x = (uint64_t)(U)sp[i];
            if constexpr (LeadingZeros) dp[i] = (T)evision_clz_width(x, (int)(sizeof(T) * 8));
            else dp[i] = (T)evision_popcount_u64(x);
        }
    }
}

template <typename T>
static void evision_bit_unary(const cv::Mat& src, cv::Mat& dst, int op) {
    if (op == 0) evision_bit_unary_impl<T, true>(src, dst);
    else evision_bit_unary_impl<T, false>(src, dst);
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

        if (!evision_to_safe(env, evision_get_kw(env, erl_terms, "op"), op, ArgInfo("op", 0)))
            return enif_make_badarg(env);
        if (op < 0 || op > 1) return error(env, "mat_bit_unary: invalid operation");

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "src"), src, ArgInfo("src", ArgInfo::BORROWED_INPUT))) {
            const int depth = src.depth();
            if (src.empty() || src.channels() != 1 ||
                (depth != CV_8U && depth != CV_8S && depth != CV_16U && depth != CV_16S &&
                 depth != CV_32S && depth != CV_32U && depth != CV_64S && depth != CV_64U))
                return error(env, "mat_bit_unary: expected a single-channel integer matrix");
            // Every output element is computed directly from the immutable source.
            Mat dst(src.dims, src.size.p, src.type());
            switch (depth) {
                case CV_8U:  evision_bit_unary<uint8_t>(src, dst, op); break;
                case CV_8S:  evision_bit_unary<int8_t>(src, dst, op); break;
                case CV_16U: evision_bit_unary<uint16_t>(src, dst, op); break;
                case CV_16S: evision_bit_unary<int16_t>(src, dst, op); break;
                case CV_32S: evision_bit_unary<int32_t>(src, dst, op); break;
                case CV_32U: evision_bit_unary<uint32_t>(src, dst, op); break;
                case CV_64S: evision_bit_unary<int64_t>(src, dst, op); break;
                case CV_64U: evision_bit_unary<uint64_t>(src, dst, op); break;
                default: return evision::nif::error(env, "mat_bit_unary: integer types only");
            }
            return evision_from(env, dst);
        }
    }

    return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_BIT_UNARY_H
