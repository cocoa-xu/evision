#ifndef EVISION_NATIVE_SAFETY_HPP
#define EVISION_NATIVE_SAFETY_HPP

#include <cstddef>
#include <limits>
#include <vector>

// Validate sizes before multiplying, narrowing to OpenCV's int dimensions, or
// constructing a view over caller-owned memory. An empty shape denotes a scalar.
static inline bool evision_checked_mul_size(size_t lhs, size_t rhs, size_t& out) {
    if (rhs != 0 && lhs > std::numeric_limits<size_t>::max() / rhs) return false;
    out = lhs * rhs;
    return true;
}

static inline bool evision_shape_byte_size(const std::vector<int>& shape,
                                          size_t elem_size, size_t& byte_size) {
    if (shape.size() > 32 || elem_size == 0) return false;
    size_t count = 1;
    for (int dim : shape) {
        if (dim <= 0 || !evision_checked_mul_size(count, (size_t)dim, count)) return false;
    }
    return evision_checked_mul_size(count, elem_size, byte_size) &&
           byte_size <= (size_t)std::numeric_limits<ptrdiff_t>::max();
}

#endif // EVISION_NATIVE_SAFETY_HPP
