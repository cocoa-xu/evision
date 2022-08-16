#ifndef EVISION_BACKEND_BROADCAST_H
#define EVISION_BACKEND_BROADCAST_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

static void broadcast(
    Mat &img,
    void *& dst_data,
    void *& tmp_data,
    std::vector<int>& src_shape,
    std::vector<int>& to_shape,
    size_t elem_size
)
{
    size_t ndims = src_shape.size();
    size_t subgroup_bytes = src_shape[ndims - 1] * elem_size;
    size_t num_subgroups = 1;
    for (size_t i = 0; i < ndims - 1; ++i) {
        num_subgroups *= src_shape[i];
    }

    if (img.isContinuous()) {
        memcpy(dst_data, img.data, num_subgroups * subgroup_bytes);
    } else {
        Mat tmp = img.clone();
        memcpy(dst_data, tmp.data, num_subgroups * subgroup_bytes);
    }

    for (int64_t dim = ndims - 1; dim >= 0; --dim) {
        size_t elem_per_subgroup = src_shape[dim];
        size_t elem_per_group = to_shape[dim];
        size_t num_groups = num_subgroups / elem_per_subgroup;
        if (num_subgroups == 1) {
            num_groups = 1;
        }
        size_t group_bytes = subgroup_bytes * num_subgroups / elem_per_group;

        if (elem_per_subgroup == 1) {
            size_t copy_times = elem_per_group;
            group_bytes = subgroup_bytes * num_subgroups * copy_times / num_groups;

            for (size_t subgroup = 0; subgroup < num_subgroups; ++subgroup) {
                void * subgroup_start = (void *)((uint64_t)(uint64_t *)dst_data + subgroup_bytes * subgroup);
                for (size_t i = 0; i < copy_times; ++i) {
                    void * tmp_data_start = (void *)((uint64_t)(uint64_t *)tmp_data + subgroup_bytes * i + group_bytes * subgroup);
                    memcpy(tmp_data_start, subgroup_start, subgroup_bytes);
                }
            }
            std::swap(tmp_data, dst_data);
            subgroup_bytes = group_bytes;
        } else if (elem_per_subgroup == elem_per_group) {
            subgroup_bytes = subgroup_bytes * num_subgroups / num_groups;
        }

        num_subgroups = num_groups;
    }
}

// @evision c: mat_broadcast_to, evision_cv_mat_broadcast_to, 1
// @evision nif: def mat_broadcast_to(_opts \\ []), do: :erlang.nif_error("Mat::broadcast_to not loaded")
static ERL_NIF_TERM evision_cv_mat_broadcast_to(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;
        std::vector<int> to_shape;
        std::vector<int> force_src_shape;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "to_shape"), to_shape, ArgInfo("to_shape", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "force_src_shape"), force_src_shape, ArgInfo("force_src_shape", 0))) {
            int64_t ndims = to_shape.size();
            std::vector<int> src_shape(ndims);

            int diff_dims = ndims;
            if (force_src_shape.size() > 0) {
                diff_dims -= force_src_shape.size();
            } else {
                diff_dims -= img.size.dims();
                for (int i = 0; i < img.size.dims(); ++i) {
                    force_src_shape.push_back(img.size.p[i]);
                }
            }

            if (diff_dims < 0) {
                return evision::nif::error(env, "cannot broadcast to specified shape");
            }

            // align shapes with 1s
            for (int64_t i = 0; i < ndims; ++i) {
                if (i < diff_dims) {
                    src_shape[i] = 1;
                } else {
                    src_shape[i] = force_src_shape[i - diff_dims];
                    // each dim has to be equal unless the src dim is 1 (which we are going to broadcast)
                    if (src_shape[i] != to_shape[i] && src_shape[i] != 1) {
                        return evision::nif::error(env, "cannot broadcast to specified shape.");
                    }
                }
            }

            // calculate number of elements in the new shape
            const size_t elem_size = img.elemSize();
            size_t count_new_elem = 1;
            for (int64_t i = 0; i < ndims; i++) {
                count_new_elem *= to_shape[i];
            }

            // allocate memory
            void * dst_data = (void *)enif_alloc(elem_size * count_new_elem);
            if (dst_data == nullptr) {
                return evision::nif::error(env, "cannot broadcast to specified shape, out of memory");
            }
            void * tmp_data = (void *)enif_alloc(elem_size * count_new_elem);
            if (tmp_data == nullptr) {
                return evision::nif::error(env, "cannot broadcast to specified shape, out of memory");
            }

            // broadcast
            broadcast(img, dst_data, tmp_data, src_shape, to_shape, elem_size);

            int type = img.type() & CV_MAT_DEPTH_MASK;
            Mat result = Mat(ndims, to_shape.data(), type, dst_data);
            result = result.clone();
            enif_free((void *)dst_data);
            enif_free((void *)tmp_data);
            return evision::nif::ok(env, evision_from(env, result));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_BROADCAST_H
