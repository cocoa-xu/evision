#ifndef EVISION_BACKEND_CPU_DISPATCH_H
#define EVISION_BACKEND_CPU_DISPATCH_H

#include <atomic>
#include <cstdlib>
#include <cstring>
#include <stdexcept>
#include "kernels/dispatch.hpp"

static evision::cpu::Mode evision_initial_cpu_mode() {
    const char* setting = std::getenv("EVISION_CPU_DISPATCH");
    if (!setting || std::strcmp(setting, "auto") == 0) return evision::cpu::Mode::Auto;
    if (std::strcmp(setting, "baseline") == 0) return evision::cpu::Mode::Baseline;
    throw std::invalid_argument("EVISION_CPU_DISPATCH must be auto or baseline");
}

static std::atomic<evision::cpu::Mode>& evision_cpu_mode() {
    static std::atomic<evision::cpu::Mode> mode{evision_initial_cpu_mode()};
    return mode;
}

static unsigned evision_cpu_features() {
    // OpenCV checks OS support as well as CPU capabilities. Query once per NIF
    // call so cv::setUseOptimized and OPENCV_CPU_DISABLE remain effective.
    unsigned features = 0;
    if (cv::checkHardwareSupport(CV_CPU_NEON)) features |= evision::cpu::Neon;
    if (cv::checkHardwareSupport(CV_CPU_AVX2)) features |= evision::cpu::Avx2;
    return features;
}

static evision::cpu::Selection evision_select_dispatch() {
    return evision::cpu::select_kernel(evision_cpu_features(), evision_cpu_mode().load(std::memory_order_relaxed));
}

static ERL_NIF_TERM evision_cpu_info(ErlNifEnv* env) {
    auto put = [&](ERL_NIF_TERM map, const char* key, ERL_NIF_TERM value) {
        ERL_NIF_TERM result;
        enif_make_map_put(env, map, enif_make_atom(env, key), value, &result);
        return result;
    };
    auto boolean = [&](bool value) { return enif_make_atom(env, value ? "true" : "false"); };
    const auto mode = evision_cpu_mode().load(std::memory_order_relaxed);
    const auto selected = evision::cpu::select_kernel(evision_cpu_features(), mode);
    const unsigned compiled = evision::cpu::compiled_features();
    ERL_NIF_TERM capabilities = enif_make_new_map(env);
    const struct { const char* name; int feature; } features[] = {
        {"neon", CV_CPU_NEON}, {"neon_fp16", CV_CPU_NEON_FP16},
        {"neon_dotprod", CV_CPU_NEON_DOTPROD}, {"neon_bf16", CV_CPU_NEON_BF16},
        {"avx2", CV_CPU_AVX2}, {"fma3", CV_CPU_FMA3}, {"avx512_skx", CV_CPU_AVX512_SKX}
    };
    for (const auto& feature : features)
        capabilities = put(capabilities, feature.name, boolean(cv::checkHardwareSupport(feature.feature)));
    ERL_NIF_TERM implementations = enif_make_new_map(env);
    implementations = put(implementations, "baseline", boolean(true));
    implementations = put(implementations, "neon", boolean((compiled & evision::cpu::Neon) != 0));
    implementations = put(implementations, "avx2", boolean((compiled & evision::cpu::Avx2) != 0));
    ERL_NIF_TERM result = enif_make_new_map(env);
    result = put(result, "mode", enif_make_atom(env, mode == evision::cpu::Mode::Auto ? "auto" : "baseline"));
    result = put(result, "select", enif_make_atom(env, selected.name));
    result = put(result, "available_features", capabilities);
    return put(result, "compiled_implementations", implementations);
}

// @evision c: mat_cpu_dispatch_info,evision_cv_mat_cpu_dispatch_info,0
// @evision nif: def mat_cpu_dispatch_info(), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_cpu_dispatch_info(ErlNifEnv* env, int, const ERL_NIF_TERM[]) {
    return evision_cpu_info(env);
}

// @evision c: mat_set_cpu_dispatch,evision_cv_mat_set_cpu_dispatch,1
// @evision nif: def mat_set_cpu_dispatch(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_set_cpu_dispatch(ErlNifEnv* env, int, const ERL_NIF_TERM argv[]) {
    std::map<std::string, ERL_NIF_TERM> terms;
    evision::nif::parse_arg(env, 0, argv, terms);
    std::string mode;
    if (!evision_to_safe(env, evision_get_kw(env, terms, "mode"), mode, ArgInfo("mode", 0)))
        return enif_make_badarg(env);
    if (mode != "auto" && mode != "baseline")
        return evision::nif::error(env, "CPU dispatch mode must be auto or baseline");
    evision_cpu_mode().store(mode == "auto" ? evision::cpu::Mode::Auto : evision::cpu::Mode::Baseline,
                            std::memory_order_relaxed);
    return evision_cpu_info(env);
}

#endif
