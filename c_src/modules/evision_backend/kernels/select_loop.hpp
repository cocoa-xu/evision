#ifndef EVISION_SELECT_LOOP_HPP
#define EVISION_SELECT_LOOP_HPP

#include "dispatch.hpp"
#include <cstring>

namespace evision { namespace cpu {

// Mixed SIMD blocks load both inputs; uniform blocks copy only one.
template <size_t Width, class Blend>
void select_loop(const uint8_t* mask, const uint8_t* on_true, const uint8_t* on_false,
                 uint8_t* output, size_t count, size_t element_size) {
    const size_t width = Width == 0 ? element_size : Width;
    size_t i = 0;
    for (; count - i >= 8; i += 8) {
        uint64_t bits;
        std::memcpy(&bits, mask + i, sizeof(bits));
        const size_t offset = i * width;
        // M6/Clang 21 measurements in PR #346 favored this guard for uniform
        // masks, while unconditional blending won on some mixed masks.
        if (bits == 0) {
            std::memcpy(output + offset, on_false + offset, 8 * width);
        } else if (((bits - UINT64_C(0x0101010101010101)) &
                    ~bits & UINT64_C(0x8080808080808080)) == 0) {
            std::memcpy(output + offset, on_true + offset, 8 * width);
        } else {
            Blend::run(mask + i, on_true + offset, on_false + offset, output + offset, width);
        }
    }
    for (; i < count; ++i) {
        const size_t offset = i * width;
        std::memcpy(output + offset, (mask[i] == 0 ? on_false : on_true) + offset, width);
    }
}

template <size_t Width>
struct BaselineBlend {
    static void run(const uint8_t* mask, const uint8_t* a, const uint8_t* b,
                    uint8_t* out, size_t element_size) {
        const size_t width = Width == 0 ? element_size : Width;
        for (size_t i = 0; i < 8; ++i)
            std::memcpy(out + i * width, (mask[i] == 0 ? b : a) + i * width, width);
    }
};

}} // namespace evision::cpu
#endif
