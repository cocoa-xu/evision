#include "select_loop.hpp"

namespace evision { namespace cpu {

void select_baseline(const uint8_t* mask, const uint8_t* a, const uint8_t* b,
                     uint8_t* out, size_t count, size_t width) {
    switch (width) {
        case 1: select_loop<1, BaselineBlend<1>>(mask, a, b, out, count, width); break;
        case 2: select_loop<2, BaselineBlend<2>>(mask, a, b, out, count, width); break;
        case 4: select_loop<4, BaselineBlend<4>>(mask, a, b, out, count, width); break;
        case 8: select_loop<8, BaselineBlend<8>>(mask, a, b, out, count, width); break;
        default: select_loop<0, BaselineBlend<0>>(mask, a, b, out, count, width); break;
    }
}

}} // namespace evision::cpu
