#include "dispatch.hpp"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <vector>
#if defined(_MSC_VER) && defined(_M_X64)
#include <intrin.h>
#elif defined(__linux__) && defined(__aarch64__)
#include <sys/auxv.h>
#include <asm/hwcap.h>
#endif

// Production detection uses OpenCV. These standalone tests independently guard
// execution of optional instructions on the test host, without linking OpenCV.
static unsigned host_features() {
    unsigned features = 0;
#if defined(__APPLE__) && defined(__aarch64__)
    features |= evision::cpu::Neon;
#elif defined(__linux__) && defined(__aarch64__)
    if (getauxval(AT_HWCAP) & HWCAP_ASIMD) features |= evision::cpu::Neon;
#elif defined(_MSC_VER) && defined(_M_X64)
    int registers[4];
    __cpuid(registers, 0);
    const int maximum = registers[0];
    if (maximum >= 7) {
        __cpuidex(registers, 1, 0);
        const int avx_osxsave = (1 << 27) | (1 << 28);
        if ((registers[2] & avx_osxsave) == avx_osxsave && (_xgetbv(0) & 6) == 6) {
            __cpuidex(registers, 7, 0);
            if (registers[1] & (1 << 5)) features |= evision::cpu::Avx2;
        }
    }
#elif defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__))
    if (__builtin_cpu_supports("avx2")) features |= evision::cpu::Avx2;
#endif
    return features;
}

static void require(bool condition, const char* message) {
    if (!condition) {
        std::fprintf(stderr, "%s\n", message);
        std::exit(EXIT_FAILURE);
    }
}

int main(int argc, char** argv) {
    using namespace evision::cpu;
    const unsigned available = host_features();
    const auto automatic = select_kernel(available, Mode::Auto);
    if (argc == 2) require(std::strcmp(argv[1], automatic.name) == 0, "Expected ISA implementation did not execute");
    require(argc <= 2, "Expected at most one ISA argument");
    require(select_kernel(~0U, Mode::Baseline).select == select_baseline, "Baseline override was ignored");
    require(select_kernel(0, Mode::Auto).select == select_baseline, "Missing features did not fall back");
    require(select_kernel(~compiled_features(), Mode::Auto).select == select_baseline,
            "Selected an implementation absent from this build");
    std::printf("Compiled features: %u; available features: %u; selected: %s\n",
                compiled_features(), available, automatic.name);

    size_t cases = 0;
    for (size_t width : {size_t{1}, size_t{2}, size_t{4}, size_t{8}, size_t{3}, size_t{16}}) {
        for (size_t count = 0; count <= 65; ++count) {
            for (size_t offset : {size_t{0}, size_t{1}, size_t{3}}) {
                for (unsigned pattern = 0; pattern < 256; ++pattern) {
                    const size_t bytes = count * width;
                    const size_t a_offset = (offset + 1) % 4, b_offset = (offset + 2) % 4;
                    std::vector<uint8_t> mask(count + offset + 1), a(bytes + a_offset + 1), b(bytes + b_offset + 1);
                    const uint8_t nonzero[] = {1, 2, 127, 128, 255};
                    for (size_t i = 0; i < count; ++i)
                        mask[offset + i] = (pattern & (1U << (i % 8))) ? nonzero[i % 5] : 0;
                    for (size_t i = 0; i < bytes; ++i) {
                        a[a_offset + i] = static_cast<uint8_t>(i * 37 + pattern);
                        b[b_offset + i] = static_cast<uint8_t>(i * 19 + 173);
                    }
                    const auto original_mask = mask, original_a = a, original_b = b;
                    std::vector<uint8_t> expected(bytes + offset + 16, 0xa5);
                    for (size_t i = 0; i < count; ++i) {
                        for (size_t byte = 0; byte < width; ++byte)
                            expected[offset + i * width + byte] = mask[offset + i] == 0
                                ? b[b_offset + i * width + byte] : a[a_offset + i * width + byte];
                    }
                    for (Select kernel : {select_baseline, automatic.select}) {
                        std::vector<uint8_t> output(bytes + offset + 16, 0xa5);
                        kernel(mask.data() + offset, a.data() + a_offset, b.data() + b_offset,
                               output.data() + offset, count, width);
                        require(output == expected, "Select result or output guard bytes differ");
                        require(mask == original_mask && a == original_a && b == original_b, "Input bytes changed");
                    }
                    ++cases;
                }
            }
        }
    }
    std::printf("%zu select cases passed: all 8-element mask patterns, widths, tails, and unaligned buffers\n", cases);
    return EXIT_SUCCESS;
}
