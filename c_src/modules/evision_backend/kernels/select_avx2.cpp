#include "select_loop.hpp"
#include <immintrin.h>

namespace evision { namespace cpu { namespace avx2 {

template <size_t Width>
struct Blend {
    static void run(const uint8_t* mask, const uint8_t* a, const uint8_t* b,
                    uint8_t* out, size_t) {
        // Loading both sources is intentional for mixed blocks; the shared
        // loop handles uniform blocks with a single-source copy. Width is a
        // compile-time constant, so widening choices do not dispatch per lane.
        const __m128i zero = _mm_setzero_si128();
        const __m128i selected = _mm_cmpeq_epi8(_mm_loadl_epi64(reinterpret_cast<const __m128i*>(mask)), zero);
        if constexpr (Width == 1) {
            const __m128i av = _mm_loadl_epi64(reinterpret_cast<const __m128i*>(a));
            const __m128i bv = _mm_loadl_epi64(reinterpret_cast<const __m128i*>(b));
            _mm_storel_epi64(reinterpret_cast<__m128i*>(out), _mm_blendv_epi8(av, bv, selected));
        } else if constexpr (Width == 2) {
            const __m128i av = _mm_loadu_si128(reinterpret_cast<const __m128i*>(a));
            const __m128i bv = _mm_loadu_si128(reinterpret_cast<const __m128i*>(b));
            _mm_storeu_si128(reinterpret_cast<__m128i*>(out), _mm_blendv_epi8(av, bv, _mm_cvtepi8_epi16(selected)));
        } else {
            for (size_t half = 0; half < Width / 4; ++half) {
                const __m256i expanded = Width == 4 ? _mm256_cvtepi8_epi32(selected)
                    : _mm256_cvtepi8_epi64(half == 0 ? selected : _mm_srli_si128(selected, 4));
                const size_t offset = half * 32;
                const __m256i av = _mm256_loadu_si256(reinterpret_cast<const __m256i*>(a + offset));
                const __m256i bv = _mm256_loadu_si256(reinterpret_cast<const __m256i*>(b + offset));
                _mm256_storeu_si256(reinterpret_cast<__m256i*>(out + offset), _mm256_blendv_epi8(av, bv, expanded));
            }
        }
    }
};

} // namespace avx2

void select_avx2(const uint8_t* mask, const uint8_t* a, const uint8_t* b,
                 uint8_t* out, size_t count, size_t width) {
    switch (width) {
        case 1: select_loop<1, avx2::Blend<1>>(mask, a, b, out, count, width); break;
        case 2: select_loop<2, avx2::Blend<2>>(mask, a, b, out, count, width); break;
        case 4: select_loop<4, avx2::Blend<4>>(mask, a, b, out, count, width); break;
        case 8: select_loop<8, avx2::Blend<8>>(mask, a, b, out, count, width); break;
        default: select_baseline(mask, a, b, out, count, width); break;
    }
}

}} // namespace evision::cpu
