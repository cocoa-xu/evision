#include "evision_numeric_cast.hpp"

#include <array>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <limits>

// Derive expected values arithmetically, independently of float bit expansion.
static double expected_value(uint16_t bits, int fraction_bits, int exponent_bits, int bias) {
    const int fraction_scale = 1 << fraction_bits;
    const int max_exponent = (1 << exponent_bits) - 1;
    const int fraction = bits % fraction_scale;
    const int exponent = (bits / fraction_scale) % (max_exponent + 1);
    double magnitude;
    if (exponent == max_exponent) {
        magnitude = fraction == 0 ? std::numeric_limits<double>::infinity()
                                  : std::numeric_limits<double>::quiet_NaN();
    } else if (exponent == 0) {
        magnitude = std::ldexp(static_cast<double>(fraction), 1 - bias - fraction_bits);
    } else {
        magnitude = std::ldexp(1.0 + static_cast<double>(fraction) / fraction_scale, exponent - bias);
    }
    return std::copysign(magnitude, bits >= 0x8000 ? -1.0 : 1.0);
}

static void check_value(double actual, double expected, const char *type, uint16_t bits, size_t offset) {
    const bool equal = std::isnan(expected) ? std::isnan(actual)
        : actual == expected && std::signbit(actual) == std::signbit(expected);
    if (!equal) {
        std::fprintf(stderr, "%s 0x%04x at byte offset %zu: got %a, expected %a\n",
                     type, static_cast<unsigned int>(bits), offset, actual, expected);
        std::exit(EXIT_FAILURE);
    }
}

int main(int argc, char **argv) {
    const uint16_t marker = 0x0102;
    std::array<unsigned char, 2> marker_bytes{};
    std::memcpy(marker_bytes.data(), &marker, sizeof(marker));
    const bool little = marker_bytes[0] == 0x02 && marker_bytes[1] == 0x01;
    const bool big = marker_bytes[0] == 0x01 && marker_bytes[1] == 0x02;
    if (argc != 2 || (!little && !big) || std::strcmp(argv[1], little ? "little" : "big") != 0) {
        std::fprintf(stderr, "Runtime byte order does not match the requested little/big target\n");
        return EXIT_FAILURE;
    }
    std::printf("Runtime byte order: %s\n", little ? "little" : "big");

    for (uint32_t value = 0; value <= 0xffff; ++value) {
        const auto bits = static_cast<uint16_t>(value);
        const unsigned char high = static_cast<unsigned char>(value >> 8);
        const unsigned char low = static_cast<unsigned char>(value & 0xff);
        const double f16 = expected_value(bits, 10, 5, 15);
        const double bf16 = expected_value(bits, 7, 8, 127);
        for (size_t offset : {size_t{0}, size_t{1}}) {
            // Explicit byte sequences exercise native loads at aligned and
            // unaligned addresses, including on the emulated big-endian CPU.
            alignas(uint16_t) std::array<unsigned char, 4> bytes{{0xa5, 0xa5, 0xa5, 0xa5}};
            bytes[offset] = little ? low : high;
            bytes[offset + 1] = little ? high : low;
            const auto original = bytes;
            const auto loaded = _do_cast_type<uint16_t, uint16_t>(bytes.data() + offset);
            if (loaded != bits) {
                std::fprintf(stderr, "Native load changed 0x%04x at byte offset %zu\n",
                             static_cast<unsigned int>(bits), offset);
                return EXIT_FAILURE;
            }
            check_value(_do_cast_f16<double>(loaded), f16, "f16 to f64", bits, offset);
            check_value(_do_cast_bf16<double>(loaded), bf16, "bf16 to f64", bits, offset);
            check_value(_do_cast_f16<float>(loaded), f16, "f16 to f32", bits, offset);
            check_value(_do_cast_bf16<float>(loaded), bf16, "bf16 to f32", bits, offset);
            if (bytes != original) {
                std::fprintf(stderr, "Numeric conversion modified the input bytes\n");
                return EXIT_FAILURE;
            }
        }
    }
    std::puts("f16 and bf16: all 65536 encodings passed for f32/f64, aligned/unaligned loads");
    return EXIT_SUCCESS;
}
