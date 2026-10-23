#ifndef EVISION_BACKEND_SELECT_BROADCAST_H
#define EVISION_BACKEND_SELECT_BROADCAST_H

#include "select.h"

static bool evision_select_shape(const cv::Mat& src, const std::vector<int>& shape,
                                const std::vector<int>& out) {
    if (shape.size() > out.size() || !evision_backend_shape(src, shape)) return false;
    for (size_t i = shape.size(); i-- > 0;) {
        const size_t axis = out.size() - shape.size() + i;
        if (shape[i] != 1 && shape[i] != out[axis]) return false;
    }
    return true;
}

template <size_t Width>
static void evision_select_broadcast_rows(const cv::Mat inputs[3], cv::Mat& dst,
                                         const std::vector<int>& dims,
                                         const std::vector<size_t> strides[3]) {
    const size_t width = Width == 0 ? dst.elemSize() : Width;
    const size_t cols = (size_t)dims.back();
    const size_t rows = dst.total() / cols;
    const size_t ms = strides[0].back(), ts = strides[1].back(), fs = strides[2].back();
    evision_parallel_for((int64_t)rows, (int64_t)dst.total(), EVISION_PARALLEL_SIMPLE_MIN_WORK,
                        [&](int64_t begin, int64_t end) {
        for (int64_t row = begin; row < end; ++row) {
            size_t index = (size_t)row, offsets[3] = {0, 0, 0};
            for (size_t dim = dims.size() - 1; dim-- > 0;) {
                const size_t coordinate = index % (size_t)dims[dim];
                index /= (size_t)dims[dim];
                for (size_t input = 0; input < 3; ++input) offsets[input] += coordinate * strides[input][dim];
            }
            const uchar *mask = inputs[0].data + offsets[0];
            const uchar *on_true = inputs[1].data + offsets[1];
            const uchar *on_false = inputs[2].data + offsets[2];
            uchar *target = dst.data + (size_t)row * cols * width;
            for (size_t col = 0; col < cols; ++col) {
                const uchar *selected = mask[col * ms] == 0 ? on_false + col * fs : on_true + col * ts;
                std::memcpy(target + col * width, selected, width);
            }
        }
    });
}

// @evision c: mat_select_broadcast, evision_cv_mat_select_broadcast, 1
// @evision nif: def mat_select_broadcast(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_select_broadcast(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> terms;
    parse_arg(env, 0, argv, terms);
    std::vector<int> dims, shapes[3];
    size_t count;
    if (!evision_to_safe(env, evision_get_kw(env, terms, "out_dims"), dims, ArgInfo("out_dims", 0)))
        return enif_make_badarg(env);
    if (dims.empty() || !evision_backend_count(dims, count)) return error(env, "select: invalid output shape");
    const char *shape_names[3] = {"mask_dims", "true_dims", "false_dims"};
    const char *input_names[3] = {"mask", "on_true", "on_false"};
    Mat inputs[3];
    std::vector<size_t> strides[3];
    for (size_t i = 0; i < 3; ++i) {
        if (!evision_to_safe(env, evision_get_kw(env, terms, shape_names[i]), shapes[i], ArgInfo(shape_names[i], 0)) ||
            !evision_to_safe(env, evision_get_kw(env, terms, input_names[i]), inputs[i], ArgInfo(input_names[i], ArgInfo::BORROWED_INPUT)))
            return enif_make_badarg(env);
        if (!evision_select_shape(inputs[i], shapes[i], dims))
            return error(env, "select: invalid input or broadcast shape");
    }
    size_t bytes;
    if (inputs[0].type() != CV_8U || inputs[1].type() != inputs[2].type() ||
        !evision_shape_byte_size(dims, inputs[1].elemSize(), bytes))
        return error(env, "select: invalid input type or output size");
    for (Mat& input : inputs) if (!input.isContinuous()) input = input.clone();
    Mat dst((int)dims.size(), dims.data(), inputs[1].type());
    const bool dense = inputs[0].total() == count && inputs[1].total() == count && inputs[2].total() == count;
    if (!dense) {
        for (size_t input = 0; input < 3; ++input) {
            strides[input].assign(dims.size(), 0);
            size_t step = inputs[input].elemSize();
            for (size_t i = shapes[input].size(); i-- > 0;) {
                const size_t axis = dims.size() - shapes[input].size() + i;
                strides[input][axis] = shapes[input][i] == 1 ? 0 : step;
                step *= (size_t)shapes[input][i];
            }
        }
    }
    // Retain the block-copy path when broadcasting is unnecessary.
#define EVISION_SELECT_WIDTH(Width) \
    if (dense) evision_select<Width>(inputs[0], inputs[1], inputs[2], dst); \
    else evision_select_broadcast_rows<Width>(inputs, dst, dims, strides)
    switch (dst.elemSize()) {
        case 1: EVISION_SELECT_WIDTH(1); break;
        case 2: EVISION_SELECT_WIDTH(2); break;
        case 4: EVISION_SELECT_WIDTH(4); break;
        case 8: EVISION_SELECT_WIDTH(8); break;
        default: EVISION_SELECT_WIDTH(0); break;
    }
#undef EVISION_SELECT_WIDTH
    return evision_from(env, dst);
}

#endif // EVISION_BACKEND_SELECT_BROADCAST_H
