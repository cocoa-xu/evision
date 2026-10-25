#include "dispatch.hpp"

namespace evision { namespace cpu {

unsigned compiled_features() {
    unsigned features = 0;
#ifdef EVISION_HAVE_NEON
    features |= Neon;
#endif
#ifdef EVISION_HAVE_AVX2
    features |= Avx2;
#endif
    return features;
}

Selection select_kernel(unsigned available, Mode mode) {
    available &= compiled_features();
    if (mode == Mode::Auto) {
#ifdef EVISION_HAVE_AVX2
        if (available & Avx2) return {"avx2", select_avx2};
#endif
#ifdef EVISION_HAVE_NEON
        if (available & Neon) return {"neon", select_neon};
#endif
    }
    return {"baseline", select_baseline};
}

}} // namespace evision::cpu
