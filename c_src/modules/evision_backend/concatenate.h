#ifndef EVISION_BACKEND_CONCATENATE_H
#define EVISION_BACKEND_CONCATENATE_H

#include <cstring>
#include "../../ArgInfo.hpp"
#include "validation.h"

// @evision c: mat_concatenate, evision_cv_mat_concatenate, 1
// @evision nif: def mat_concatenate(_opts \\ []), do: :erlang.nif_error(:undefined)
static ERL_NIF_TERM evision_cv_mat_concatenate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    using namespace evision::nif;
    std::map<std::string, ERL_NIF_TERM> terms;
    parse_arg(env, 0, argv, terms);
    std::vector<int> out_dims;
    int axis = 0;
    size_t output_count;
    if (!evision_to_safe(env, evision_get_kw(env, terms, "out_dims"), out_dims, ArgInfo("out_dims", 0)) ||
        !evision_to_safe(env, evision_get_kw(env, terms, "axis"), axis, ArgInfo("axis", 0)))
        return enif_make_badarg(env);
    if (!evision_backend_count(out_dims, output_count) || axis < 0 || axis >= (int)out_dims.size())
        return error(env, "concatenate: invalid output shape or axis");

    std::vector<Mat> inputs;
    std::vector<std::vector<int>> shapes;
    if (!evision_to_safe(env, evision_get_kw(env, terms, "inputs"), inputs, ArgInfo("inputs", ArgInfo::BORROWED_INPUT)))
        return enif_make_badarg(env);
    ERL_NIF_TERM shape_list = evision_get_kw(env, terms, "shapes"), head, tail;
    unsigned shape_count;
    if (!enif_get_list_length(env, shape_list, &shape_count)) return enif_make_badarg(env);
    shapes.resize(shape_count);
    for (auto& shape : shapes) {
        if (!enif_get_list_cell(env, shape_list, &head, &tail) ||
            !evision_to_safe(env, head, shape, ArgInfo("shape", 0))) return enif_make_badarg(env);
        shape_list = tail;
    }
    if (inputs.empty() || inputs.size() != shapes.size())
        return error(env, "concatenate: expected matching nonempty input and shape lists");

    int axis_sum = 0;
    for (size_t i = 0; i < inputs.size(); ++i) {
        if (shapes[i].size() != out_dims.size() || !evision_backend_shape(inputs[i], shapes[i]) ||
            inputs[i].type() != inputs[0].type())
            return error(env, "concatenate: input shape or type mismatch");
        for (size_t dim = 0; dim < out_dims.size(); ++dim)
            if ((int)dim != axis && shapes[i][dim] != out_dims[dim])
                return error(env, "concatenate: non-axis dimensions must match");
        if (shapes[i][axis] > out_dims[axis] - axis_sum)
            return error(env, "concatenate: axis size mismatch");
        axis_sum += shapes[i][axis];
    }
    size_t output_bytes;
    if (axis_sum != out_dims[axis] || !evision_shape_byte_size(out_dims, inputs[0].elemSize(), output_bytes))
        return error(env, "concatenate: output size mismatch");

    size_t outer = 1, inner_bytes = inputs[0].elemSize();
    for (int dim = 0; dim < axis; ++dim) outer *= (size_t)out_dims[dim];
    for (size_t dim = (size_t)axis + 1; dim < out_dims.size(); ++dim) inner_bytes *= (size_t)out_dims[dim];
    const size_t row_bytes = output_bytes / outer;
    Mat dst((int)out_dims.size(), out_dims.data(), inputs[0].type());
    size_t offset = 0;
    for (size_t i = 0; i < inputs.size(); ++i) {
        const Mat src = inputs[i].isContinuous() ? inputs[i] : inputs[i].clone();
        const size_t block_bytes = (size_t)shapes[i][axis] * inner_bytes;
        for (size_t row = 0; row < outer; ++row)
            std::memcpy(dst.data + row * row_bytes + offset, src.data + row * block_bytes, block_bytes);
        offset += block_bytes;
    }
    return evision_from(env, dst);
}

#endif // EVISION_BACKEND_CONCATENATE_H
