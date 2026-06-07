#ifndef EVISION_BACKEND_PARALLEL_H
#define EVISION_BACKEND_PARALLEL_H

#include <cstdint>
#include <limits>
#include <opencv2/core/utility.hpp>

static constexpr int64_t EVISION_PARALLEL_MIN_WORK = 32768;
static constexpr int64_t EVISION_PARALLEL_SIMPLE_MIN_WORK = 1048576;
static constexpr int64_t EVISION_PARALLEL_NESTED_MIN_WORK = 4194304;
static constexpr int64_t EVISION_PARALLEL_CONV_MIN_WORK = 16777216;

static inline bool evision_should_parallelize(int64_t total, int64_t work, int64_t min_work) {
    return total > 0 && work >= min_work && total <= (int64_t)std::numeric_limits<int>::max();
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
    });
}

template <typename Body>
static inline void evision_parallel_for(int64_t total, int64_t work, const Body &body) {
    evision_parallel_for(total, work, EVISION_PARALLEL_MIN_WORK, body);
}

#endif // EVISION_BACKEND_PARALLEL_H
