#include "dispatch.hpp"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <vector>
#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#else
#include <sys/mman.h>
#include <unistd.h>
#endif
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

// Place the logical extent against either inaccessible page. Unlike padding
// bytes, guard pages detect reads as well as writes past a buffer boundary.
class GuardedBuffer {
public:
    GuardedBuffer(size_t bytes, bool at_end) {
#ifdef _WIN32
        SYSTEM_INFO info;
        GetSystemInfo(&info);
        const size_t page = info.dwPageSize;
#else
        const long page_size = sysconf(_SC_PAGESIZE);
        require(page_size > 0, "Cannot determine page size");
        const size_t page = static_cast<size_t>(page_size);
#endif
        accessible_size_ = ((bytes == 0 ? 1 : bytes) + page - 1) / page * page;
        mapping_size_ = accessible_size_ + 2 * page;
#ifdef _WIN32
        mapping_ = static_cast<uint8_t*>(VirtualAlloc(nullptr, mapping_size_, MEM_RESERVE | MEM_COMMIT, PAGE_NOACCESS));
        require(mapping_ != nullptr, "Cannot allocate guard pages");
#else
        void* allocation = mmap(nullptr, mapping_size_, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        require(allocation != MAP_FAILED, "Cannot allocate guard pages");
        mapping_ = static_cast<uint8_t*>(allocation);
#endif
        accessible_ = mapping_ + page;
        protect(false);
        data = at_end ? accessible_ + accessible_size_ - bytes : accessible_;
    }

    ~GuardedBuffer() {
#ifdef _WIN32
        VirtualFree(mapping_, 0, MEM_RELEASE);
#else
        munmap(mapping_, mapping_size_);
#endif
    }

    GuardedBuffer(const GuardedBuffer&) = delete;
    GuardedBuffer& operator=(const GuardedBuffer&) = delete;

    void protect(bool read_only) {
#ifdef _WIN32
        DWORD previous;
        require(VirtualProtect(accessible_, accessible_size_, read_only ? PAGE_READONLY : PAGE_READWRITE, &previous) != 0,
                "Cannot protect buffer pages");
#else
        require(mprotect(accessible_, accessible_size_, read_only ? PROT_READ : PROT_READ | PROT_WRITE) == 0,
                "Cannot protect buffer pages");
#endif
    }

    uint8_t* data = nullptr;

private:
    uint8_t* mapping_ = nullptr;
    uint8_t* accessible_ = nullptr;
    size_t mapping_size_ = 0;
    size_t accessible_size_ = 0;
};

static size_t check_guard_pages(evision::cpu::Select automatic) {
    using namespace evision::cpu;
    size_t cases = 0;
    std::vector<size_t> counts;
    for (size_t count = 0; count <= 65; ++count) counts.push_back(count);
    for (size_t count : {127, 128, 129, 257, 65539}) counts.push_back(count);
    for (size_t width : {size_t{1}, size_t{2}, size_t{4}, size_t{8}, size_t{3}, size_t{16}}) {
        for (Select kernel : {select_baseline, automatic}) kernel(nullptr, nullptr, nullptr, nullptr, 0, width);
        for (size_t count : counts) for (bool at_end : {false, true}) for (unsigned pattern = 0; pattern < 3; ++pattern) {
            const size_t bytes = count * width;
            GuardedBuffer mask(count, at_end), a(bytes, at_end), b(bytes, at_end), output(bytes, at_end);
            for (size_t i = 0; i < count; ++i)
                mask.data[i] = pattern == 0 || (pattern == 2 && i % 3 == 0) ? 0 : (i % 2 == 0 ? 128 : 255);
            for (size_t i = 0; i < bytes; ++i) {
                a.data[i] = static_cast<uint8_t>(i * 37 + 11);
                b.data[i] = static_cast<uint8_t>(i * 19 + 173);
            }
            mask.protect(true);
            a.protect(true);
            b.protect(true);
            for (Select kernel : {select_baseline, automatic}) {
                if (bytes != 0) std::memset(output.data, 0xa5, bytes);
                kernel(mask.data, a.data, b.data, output.data, count, width);
                for (size_t i = 0; i < count; ++i) for (size_t byte = 0; byte < width; ++byte) {
                    const size_t offset = i * width + byte;
                    require(output.data[offset] == (mask.data[i] == 0 ? b.data[offset] : a.data[offset]),
                            "Guarded select result differs");
                }
            }
            ++cases;
        }
    }
    return cases;
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
    std::printf("%zu guarded select cases passed: inaccessible boundaries, read-only inputs, and empty buffers\n",
                check_guard_pages(automatic.select));
    return EXIT_SUCCESS;
}
