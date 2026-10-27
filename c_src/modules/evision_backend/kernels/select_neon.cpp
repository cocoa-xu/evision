#include "select_loop.hpp"
#include <arm_neon.h>

namespace evision { namespace cpu { namespace neon {

template <size_t Width>
static inline void blend(uint8x16_t mask, const uint8_t* a, const uint8_t* b, uint8_t* out) {
    if constexpr (Width == 2) {
        vst1q_u8(out, vbslq_u8(mask, vld1q_u8(a), vld1q_u8(b)));
    } else {
        const uint8x16x2_t expanded = vzipq_u8(mask, mask);
        blend<Width / 2>(expanded.val[0], a, b, out);
        blend<Width / 2>(expanded.val[1], a + 4 * Width, b + 4 * Width, out + 4 * Width);
    }
}

template <size_t Width>
struct Blend {
    static void run(const uint8_t* mask, const uint8_t* a, const uint8_t* b,
                    uint8_t* out, size_t) {
        const uint8x8_t selected = vmvn_u8(vceq_u8(vld1_u8(mask), vdup_n_u8(0)));
        if constexpr (Width == 1) {
            vst1_u8(out, vbsl_u8(selected, vld1_u8(a), vld1_u8(b)));
        } else {
            const uint8x8x2_t expanded = vzip_u8(selected, selected);
            blend<Width>(vcombine_u8(expanded.val[0], expanded.val[1]), a, b, out);
        }
    }
};

} // namespace neon

void select_neon(const uint8_t* mask, const uint8_t* a, const uint8_t* b,
                 uint8_t* out, size_t count, size_t width) {
    switch (width) {
        case 1: select_loop<1, neon::Blend<1>>(mask, a, b, out, count, width); break;
        case 2: select_loop<2, neon::Blend<2>>(mask, a, b, out, count, width); break;
        case 4: select_loop<4, neon::Blend<4>>(mask, a, b, out, count, width); break;
        case 8: select_loop<8, neon::Blend<8>>(mask, a, b, out, count, width); break;
        default: select_baseline(mask, a, b, out, count, width); break;
    }
}

}} // namespace evision::cpu
