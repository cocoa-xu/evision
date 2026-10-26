#include "evision_broadcast.hpp"

#include <cstdio>
#include <cstdlib>
#include <memory>

static size_t elements(const std::vector<int>& shape) {
    size_t result = 1;
    for (int dim : shape) result *= static_cast<size_t>(dim);
    return result;
}

static void check(const std::vector<int>& source_shape, const std::vector<int>& target_shape,
                  size_t width, size_t offset) {
    const size_t count = elements(target_shape);
    // Source ends exactly at the allocation boundary for ASan over-read checks.
    const size_t source_size = offset + elements(source_shape) * width;
    std::unique_ptr<uint8_t[]> source(new uint8_t[source_size]);
    for (size_t i = 0; i < source_size; ++i) source[i] = static_cast<uint8_t>(i * 73 + i / 17);
    const std::vector<uint8_t> original(source.get(), source.get() + source_size);
    std::vector<uint8_t> output(offset + count * width + 16, 0xa5);
    evision::backend::broadcast_copy(source.get() + offset, output.data() + offset,
                                    source_shape, target_shape, width, count);

    // Independent coordinate oracle: reconstruct each output element's source
    // index instead of sharing the kernel's block or stride calculations.
    for (size_t index = 0; index < count; ++index) {
        size_t remaining = index;
        std::vector<size_t> coordinates(target_shape.size());
        for (size_t axis = target_shape.size(); axis-- > 0;) {
            coordinates[axis] = remaining % static_cast<size_t>(target_shape[axis]);
            remaining /= static_cast<size_t>(target_shape[axis]);
        }
        size_t input = 0;
        for (size_t axis = 0; axis < source_shape.size(); ++axis) {
            input = input * static_cast<size_t>(source_shape[axis]) +
                    (source_shape[axis] == 1 ? 0 : coordinates[axis]);
        }
        if (std::memcmp(output.data() + offset + index * width,
                        source.get() + offset + input * width, width) != 0) {
            std::fprintf(stderr, "Broadcast mismatch at element %zu, rank %zu, width %zu, offset %zu\n",
                         index, target_shape.size(), width, offset);
            std::exit(EXIT_FAILURE);
        }
    }
    for (size_t i = 0; i < output.size(); ++i) {
        if ((i < offset || i >= offset + count * width) && output[i] != 0xa5) {
            std::fprintf(stderr, "Broadcast overwrote an output guard byte\n");
            std::exit(EXIT_FAILURE);
        }
    }
    if (!std::equal(original.begin(), original.end(), source.get())) {
        std::fprintf(stderr, "Broadcast modified source bytes\n");
        std::exit(EXIT_FAILURE);
    }
}

int main() {
    size_t cases = 0;
    size_t combinations = 1;
    for (size_t rank = 1; rank <= 5; ++rank) {
        combinations *= 5;
        for (size_t code = 0; code < combinations; ++code) {
            std::vector<int> source(rank), target(rank);
            size_t remaining = code;
            for (size_t axis = 0; axis < rank; ++axis) {
                const size_t digit = remaining % 5;
                remaining /= 5;
                target[axis] = digit == 0 ? 1 : (digit <= 2 ? 2 : 3);
                source[axis] = digit == 2 || digit == 4 ? target[axis] : 1;
            }
            for (size_t width : {1, 2, 4, 8, 16, 24}) {
                for (size_t offset : {0, 1, 7}) {
                    check(source, target, width, offset);
                    ++cases;
                }
            }
        }
    }
    for (size_t width : {1, 2, 4, 8, 16, 24}) {
        for (size_t offset : {0, 1, 7}) {
            check({1, 32, 64}, {8, 32, 64}, width, offset);
            check({3, 1, 8, 16}, {3, 7, 8, 16}, width, offset);
            check({3, 1}, {3, 129}, width, offset);
            check({2, 1, 3, 1, 4}, {2, 5, 3, 7, 4}, width, offset);
            cases += 4;
        }
    }
    std::printf("%zu byte-exact broadcast cases passed\n", cases);
    return EXIT_SUCCESS;
}
