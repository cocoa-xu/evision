#ifndef EVISION_BACKEND_VALIDATION_H
#define EVISION_BACKEND_VALIDATION_H

#include "../../evision_native_safety.hpp"

// These kernels use single-channel tensors and allocate flat Mats with int
// columns. Validate against the actual Mat, never only caller-supplied metadata.
static inline bool evision_backend_tensor(const cv::Mat& mat, size_t& count) {
    if (mat.empty() || mat.channels() != 1) return false;
    count = mat.total();
    size_t bytes;
    return count <= (size_t)std::numeric_limits<int>::max() &&
           evision_checked_mul_size(count, mat.elemSize(), bytes) &&
           bytes <= (size_t)std::numeric_limits<ptrdiff_t>::max();
}

static inline bool evision_backend_tensor(const cv::Mat& mat) {
    size_t count;
    return evision_backend_tensor(mat, count);
}

static inline bool evision_backend_count(const std::vector<int>& dims, size_t& count) {
    return evision_shape_byte_size(dims, 1, count) &&
           count <= (size_t)std::numeric_limits<int>::max();
}

static inline bool evision_backend_shape(const cv::Mat& mat, const std::vector<int>& dims) {
    size_t count, actual;
    return evision_backend_tensor(mat, actual) && evision_backend_count(dims, count) &&
           count == actual;
}

static inline bool evision_backend_pair(const cv::Mat& lhs, const cv::Mat& rhs) {
    size_t lhs_count, rhs_count;
    return evision_backend_tensor(lhs, lhs_count) && evision_backend_tensor(rhs, rhs_count) &&
           lhs.type() == rhs.type() &&
           (lhs_count == rhs_count || lhs_count == 1 || rhs_count == 1);
}

static inline bool evision_backend_indices(const cv::Mat& mat) {
    return evision_backend_tensor(mat) && mat.type() == CV_64S;
}

static inline bool evision_backend_coordinates(const cv::Mat& src, cv::Mat& dims,
                                               int inner, std::vector<int64_t>& mult) {
    size_t source_count, rank;
    if (inner <= 0 || !evision_backend_tensor(src, source_count) ||
        dims.type() != CV_64S || !evision_backend_tensor(dims, rank) || rank > 32) return false;
    if (!dims.isContinuous()) dims = dims.clone();
    const int64_t* sizes = dims.ptr<int64_t>();
    size_t count = (size_t)inner;
    mult.resize(rank);
    for (size_t k = rank; k-- > 0;) {
        mult[k] = (int64_t)count;
        if (sizes[k] <= 0 || sizes[k] > std::numeric_limits<int>::max() ||
            !evision_checked_mul_size(count, (size_t)sizes[k], count) ||
            count > (size_t)std::numeric_limits<int>::max()) return false;
    }
    return count == source_count;
}

static inline bool evision_backend_matrix(const cv::Mat& mat) {
    return evision_backend_tensor(mat) && mat.dims == 2;
}

static inline bool evision_positive_axes(const std::vector<int>& axes, size_t rank) {
    if (axes.size() != rank) return false;
    for (int value : axes) if (value <= 0) return false;
    return true;
}

#endif // EVISION_BACKEND_VALIDATION_H
