#ifndef EVISION_KERNEL_DISPATCH_HPP
#define EVISION_KERNEL_DISPATCH_HPP

#include <cstddef>
#include <cstdint>

namespace evision { namespace cpu {

enum Feature : unsigned { Neon = 1, Avx2 = 2 };
enum class Mode { Auto, Baseline };
using Select = void (*)(const uint8_t*, const uint8_t*, const uint8_t*, uint8_t*, size_t, size_t);
struct Selection { const char* name; Select select; };

unsigned compiled_features();
Selection select_kernel(unsigned available, Mode mode);
void select_baseline(const uint8_t*, const uint8_t*, const uint8_t*, uint8_t*, size_t, size_t);
void select_neon(const uint8_t*, const uint8_t*, const uint8_t*, uint8_t*, size_t, size_t);
void select_avx2(const uint8_t*, const uint8_t*, const uint8_t*, uint8_t*, size_t, size_t);

}} // namespace evision::cpu
#endif
