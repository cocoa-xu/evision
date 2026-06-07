#ifndef EVISION_BACKEND_PARALLEL_H
#define EVISION_BACKEND_PARALLEL_H

#include <algorithm>
#include <cstdint>
#include <limits>
#include <opencv2/core/utility.hpp>

// Minimum total scalar work before a loop is handed to cv::parallel_for_. The
// fixed cost of waking the thread pool is on the order of ~10us, so the floor is
// set where the serial run clears that by a wide margin while still keeping
// small tensors single-threaded. The tiers scale with the per-element cost:
// MIN/SIMPLE cover flat elementwise and row-wise passes, NESTED/CONV cover the
// kernels whose inner loop also does per-output index math.
static constexpr int64_t EVISION_PARALLEL_MIN_WORK = 32768;
static constexpr int64_t EVISION_PARALLEL_SIMPLE_MIN_WORK = 65536;
static constexpr int64_t EVISION_PARALLEL_NESTED_MIN_WORK = 262144;
static constexpr int64_t EVISION_PARALLEL_CONV_MIN_WORK = 262144;

// Target scalar work per stripe. cv::parallel_for_ defaults nstripes to the full
// range length (modules/core/src/parallel.cpp), which on the GCD backend
// dispatches one block per index -- tens of ns of dispatch overhead per element,
// which dwarfs a cheap body. We instead size the stripe count to the thread pool
// so each worker processes a few large contiguous chunks.
static constexpr int64_t EVISION_PARALLEL_STRIPE_WORK = 16384;

static inline bool evision_should_parallelize(int64_t total, int64_t work, int64_t min_work) {
    return total > 0 && work >= min_work && total <= (int64_t)std::numeric_limits<int>::max();
}

// Stripe count: at least one per thread (so the pool is saturated), at most four
// per thread (headroom for load balancing), scaled down when there is too little
// work to keep a stripe busy, and never more than the range itself.
static inline double evision_nstripes(int64_t total, int64_t work) {
    int64_t threads = (int64_t)std::max(1, cv::getNumThreads());
    int64_t target = work / EVISION_PARALLEL_STRIPE_WORK;
    target = std::min(std::max(target, threads), threads * 4);
    target = std::min(target, total);
    return (double)std::max<int64_t>(target, 1);
}

template <typename Body>
static inline void evision_parallel_for(int64_t total, int64_t work, int64_t min_work, const Body &body) {
    if (total <= 0) return;
    if (!evision_should_parallelize(total, work, min_work)) {
        body(0, total);
        return;
    }

    cv::parallel_for_(cv::Range(0, (int)total), [&](const cv::Range &range) {
        body((int64_t)range.start, (int64_t)range.end);
    }, evision_nstripes(total, work));
}

template <typename Body>
static inline void evision_parallel_for(int64_t total, int64_t work, const Body &body) {
    evision_parallel_for(total, work, EVISION_PARALLEL_MIN_WORK, body);
}

#endif // EVISION_BACKEND_PARALLEL_H
