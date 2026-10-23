#ifndef EVISION_BACKEND_ARITHMETIC_H
#define EVISION_BACKEND_ARITHMETIC_H

#include <cstdint>
#include <type_traits>

// Nx integer arithmetic wraps at the output width. Widen before arithmetic to
// avoid both signed overflow and the promotion of uint16_t operands to int.
template <typename T>
static inline T evision_wrapping_add(T lhs, T rhs) {
    if constexpr (std::is_integral<T>::value) {
        return (T)((uint64_t)lhs + (uint64_t)rhs);
    } else {
        return lhs + rhs;
    }
}

template <typename T>
static inline T evision_wrapping_mul(T lhs, T rhs) {
    if constexpr (std::is_integral<T>::value) {
        return (T)((uint64_t)lhs * (uint64_t)rhs);
    } else {
        return lhs * rhs;
    }
}

#endif // EVISION_BACKEND_ARITHMETIC_H
