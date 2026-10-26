#ifndef EVISION_BROADCAST_HPP
#define EVISION_BROADCAST_HPP

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <vector>

namespace evision { namespace backend {

inline void repeat_bytes(uint8_t *dst, const uint8_t *src, size_t elem_size, size_t count) {
    if (count == 0) return;
    std::memcpy(dst, src, elem_size);
    size_t copied = 1;
    while (copied < count) {
        const size_t chunk = std::min(copied, count - copied);
        std::memcpy(dst + copied * elem_size, dst, chunk * elem_size);
        copied += chunk;
    }
}

// The caller validates positive, compatible, equally ranked shapes and byte
// extents, and provides contiguous, non-overlapping source and destination data.
inline void broadcast_copy(const uint8_t *src, uint8_t *dst,
                           const std::vector<int>& src_shape,
                           const std::vector<int>& to_shape,
                           size_t elem_size, size_t target_count) {
    if (std::all_of(src_shape.begin(), src_shape.end(), [](int d) { return d == 1; })) {
        repeat_bytes(dst, src, elem_size, target_count);
        return;
    }

    const size_t ndims = to_shape.size();
    std::vector<size_t> src_strides(ndims, 0);
    size_t stride = 1;
    for (size_t d = ndims; d-- > 0;) {
        src_strides[d] = src_shape[d] == 1 ? 0 : stride;
        stride *= static_cast<size_t>(src_shape[d]);
    }

    size_t block_axis = ndims - 1;
    size_t block_count = static_cast<size_t>(to_shape[block_axis]);
    const bool repeat_last = src_shape[block_axis] != to_shape[block_axis];
    // Matching trailing dimensions are contiguous in both buffers. Stop at a
    // stretched dimension because its output repeats source data instead.
    if (!repeat_last) {
        while (block_axis > 0 && src_shape[block_axis - 1] == to_shape[block_axis - 1]) {
            block_count *= static_cast<size_t>(to_shape[--block_axis]);
        }
    }

    std::vector<int> idx(block_axis, 0);
    for (size_t o = 0; o < target_count / block_count; ++o) {
        size_t base = 0;
        for (size_t d = 0; d < block_axis; ++d) base += static_cast<size_t>(idx[d]) * src_strides[d];
        uint8_t *row = dst + o * block_count * elem_size;
        if (repeat_last) repeat_bytes(row, src + base * elem_size, elem_size, block_count);
        else std::memcpy(row, src + base * elem_size, block_count * elem_size);

        for (size_t d = block_axis; d-- > 0;) {
            if (++idx[d] < to_shape[d]) break;
            idx[d] = 0;
        }
    }
}

}} // namespace evision::backend
#endif
