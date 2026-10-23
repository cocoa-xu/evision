#ifndef EVISION_NUMERIC_CAST_HPP
#define EVISION_NUMERIC_CAST_HPP

#include <cstdint>
#include <cstring>

template <typename From, typename To>
static inline auto _do_cast_type(const void *ptr) -> To {
    // Erlang sub-binaries need not satisfy the alignment of the element type.
    From value;
    std::memcpy(&value, ptr, sizeof(value));
    return static_cast<To>(value);
}

template <typename To>
auto _do_cast_bf16(uint16_t value) -> To {
    // Mat and Nx binaries use native byte order, already decoded by the caller.
    const uint32_t bits = (uint32_t)value << 16;
    float result;
    std::memcpy(&result, &bits, sizeof(result));
    return static_cast<To>(result);
}

template <typename To>
auto _do_cast_f16(uint16_t value) -> To {
    // Subnormal values are mantissa * 2^-24. Preserve the sign of zero too.
    const uint32_t sign = (uint32_t)(value & 0x8000) << 16;
    const uint32_t exponent = (value >> 10) & 0x1f;
    const uint32_t mantissa = value & 0x3ff;
    if (exponent == 0) {
        const To magnitude = static_cast<To>(mantissa) * static_cast<To>(0x1p-24);
        return sign ? -magnitude : magnitude;
    }
    const uint32_t expanded_exponent = exponent == 0x1f ? 0xff : exponent + 112;
    const uint32_t bits = sign | (expanded_exponent << 23) | (mantissa << 13);
    float result;
    std::memcpy(&result, &bits, sizeof(result));
    return static_cast<To>(result);
}

#endif
