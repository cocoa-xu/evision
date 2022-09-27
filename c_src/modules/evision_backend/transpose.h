#ifndef EVISION_BACKEND_TRANSPOSE_H
#define EVISION_BACKEND_TRANSPOSE_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

std::vector<size_t> transpose_axes(uint64_t ndims, size_t * axes,
                    int64_t& min_axis, int64_t& max_axis) {
    std::vector<size_t> axes_to_transpose;
    min_axis = -1;
    for (uint64_t i = 0; i < ndims; i++) {
        if (axes[i] == i) {
            min_axis = i;
        } else {
            min_axis += 1;
            break;
        }
    }

    max_axis = ndims;
    for (int64_t i = ndims - 1; i >= 0; i--) {
        if ((int64_t)axes[i] == i) {
            max_axis = i;
        } else {
            max_axis -= 1;
            break;
        }
    }

    for (int64_t i = min_axis; i <= max_axis; i++) {
        axes_to_transpose.push_back(axes[i]);
    }
    return axes_to_transpose;
}

class transpose_shape_info {
public:
    transpose_shape_info() = default;
    // number of entries for this axis
    uint64_t entries;
    // bytes per entry
    uint64_t weight;
    // total size for all entries
    uint64_t total_size;
};

std::vector<transpose_shape_info> weighted_shape(uint64_t ndims, int * shape, size_t elem_size) {
    std::vector<transpose_shape_info> weighed(ndims);
    size_t weight = elem_size;
    int64_t last_dim = (int64_t)ndims - 1;
    for (int64_t i = last_dim; i >= 0; i--) {
        auto &info = weighed[i];
        info.entries = shape[i];
        if (i == last_dim) {
            info.weight = weight;
        } else {
            info.weight = weighed[i + 1].entries * weight;
        }
        info.total_size = info.weight * info.entries;
        weight = info.weight;
    }
    return weighed;
}

void weighted_traverse(const std::vector<transpose_shape_info>& traverse_list, size_t start_at, void * chunk, size_t read_size, void * out, size_t chunk_offset, size_t& out_offset);
void weighted_traverse(uint64_t dim, uint64_t dim_size, const std::vector<transpose_shape_info>& traverse_list, size_t start_at, void * data, size_t read_size, void * out, size_t data_offset, size_t &out_offset);

void weighted_traverse(uint64_t dim, uint64_t dim_size, const std::vector<transpose_shape_info>& traverse_list, size_t start_at, void * data, size_t read_size, void * out, size_t data_offset, size_t &out_offset) {
    weighted_traverse(traverse_list, start_at, data, read_size, out, data_offset, out_offset);
    if (dim == 1) {
        return;
    } else {
        weighted_traverse(dim - 1, dim_size, traverse_list, start_at, (void *)((uint64_t)(uint64_t *)data + dim_size), read_size, out, data_offset, out_offset);
    }
}

void weighted_traverse(const std::vector<transpose_shape_info>& traverse_list, size_t start_at, void * chunk, size_t read_size, void * out, size_t chunk_offset, size_t& out_offset) {
    if (start_at < traverse_list.size()) {
        auto& info = traverse_list[start_at];
        weighted_traverse(info.entries, info.weight, traverse_list, start_at + 1, chunk, read_size, out, chunk_offset, out_offset);
    } else {
        memcpy((void *)((uint64_t)(uint64_t *)out + out_offset), (void *)((uint64_t)(uint64_t *)chunk + chunk_offset), read_size);
        out_offset += read_size;
    }
}

/// Transpose a matrix
/// @param original matrix data starting address, data must be continous
/// @param out transposed matrix, buffer must be preallocated with sufficient space to hold the result matrix
/// @param ndims number of dimensions
/// @param shape the shape of the original matrix, a list of positive integers
/// @param new_axes the new arrangement of the axes, a list of non-negative integers, 0 <= i < ndims.
/// @param elem_size element size in bytes.
void transpose(void * original, void * out, uint64_t ndims, int * shape, size_t * new_axes, size_t elem_size) {
    if (original == nullptr || out == nullptr) return;
    if (ndims == 0 || shape == nullptr || new_axes == nullptr) return;

    int64_t min_axis, max_axis;
    std::vector<size_t> axes = transpose_axes(ndims, new_axes, min_axis, max_axis);
    auto weighted_shape_info = weighted_shape(ndims, shape, elem_size);

    size_t chunk_size = weighted_shape_info[min_axis].total_size;
    size_t read_size = 0;
    if (max_axis + 1 < (int64_t)ndims) {
        read_size = weighted_shape_info[max_axis + 1].total_size;
    } else {
        read_size = elem_size;
    }

    auto traverse_list = std::vector<transpose_shape_info>(axes.size());
    for (size_t i = 0; i < axes.size(); i++) {
        traverse_list[i] = weighted_shape_info[axes[i]];
    }

    uint64_t original_bytes = elem_size;
    for (uint64_t i = 0; i < ndims; i++) {
        original_bytes *= shape[i];
    }

    size_t num_chunks = original_bytes / chunk_size;
    size_t out_offset = 0;
    for (size_t chunk_index = 0; chunk_index < num_chunks; chunk_index++) {
        void * chunk = (uint64_t *)((uint64_t)original + chunk_size * chunk_index);
        weighted_traverse(traverse_list, 0, chunk, read_size, out, 0, out_offset);
    }
}

// @evision c: mat_transpose,evision_cv_mat_transpose,1
// @evision nif: def mat_transpose(_opts \\ []), do: :erlang.nif_error("Mat::transpose not loaded")
static ERL_NIF_TERM evision_cv_mat_transpose(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    Mat img;
    std::vector<size_t> axes;
    std::vector<int> as_shape;

    if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0)) &&
        evision_to_safe(env, evision_get_kw(env, erl_terms, "axes"), axes, ArgInfo("axes", 0)) &&
        evision_to_safe(env, evision_get_kw(env, erl_terms, "as_shape"), as_shape, ArgInfo("as_shape", 0))) {        
        int ndims = (int)as_shape.size();
        std::vector<int> new_shape(ndims);
        for (size_t i = 0; i < axes.size(); i++) {
            new_shape[i] = as_shape[axes[i]];
        }

        int type = img.type() & CV_MAT_DEPTH_MASK;
        cv::Mat ret = cv::Mat::zeros(ndims, new_shape.data(), type);
        // the data of img must be countinous
        if (!img.isContinuous()) {
            // if not, call clone to force opencv make a continuous matrix for us
            img = img.clone();
        }
        transpose(img.data, ret.data, ndims, as_shape.data(), axes.data(), img.elemSize());
        return evision::nif::ok(env, evision_from(env, ret));
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_TRANSPOSE_H
