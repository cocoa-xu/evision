#ifndef EVISION_BACKEND_BROADCAST_H
#define EVISION_BACKEND_BROADCAST_H

#include <algorithm>
#include <cstring>
#include <vector>
#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../../evision_native_safety.hpp"
#include "../../evision_broadcast.hpp"

// @evision c: mat_broadcast_to, evision_cv_mat_broadcast_to, 1
// @evision nif: def mat_broadcast_to(_opts \\ []), do: :erlang.nif_error(:undefined)
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

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", ArgInfo::BORROWED_INPUT)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "to_shape"), to_shape, ArgInfo("to_shape", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "force_src_shape"), force_src_shape, ArgInfo("force_src_shape", 0))) {
            size_t target_bytes, source_bytes;
            if (img.empty() || to_shape.empty() ||
                !evision_shape_byte_size(to_shape, img.elemSize(), target_bytes) ||
                (!force_src_shape.empty() &&
                 (!evision_shape_byte_size(force_src_shape, img.elemSize(), source_bytes) ||
                  source_bytes / img.elemSize() != img.total()))) {
                return evision::nif::error(env, "broadcast: invalid shape or source size mismatch");
            }
            int64_t ndims = to_shape.size();
            std::vector<int> src_shape(ndims);

            int diff_dims = (int)ndims;
            if (force_src_shape.size() > 0) {
                diff_dims -= force_src_shape.size();
            } else {
                diff_dims -= img.size.dims;
                for (int i = 0; i < img.size.dims; ++i) {
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

            const size_t elem_size = img.elemSize();
            int type = img.type();
            Mat result((int)ndims, to_shape.data(), type);
            if (result.data == nullptr) {
                return evision::nif::error(env, "cannot broadcast to specified shape, out of memory");
            }

            // Preserve the logical row order before passing raw contiguous bytes.
            if (!img.isContinuous()) img = img.clone();
            evision::backend::broadcast_copy(img.data, result.data, src_shape, to_shape,
                                            elem_size, target_bytes / elem_size);
            return evision_from(env, result);
        }
    }

    if (error_term != 0) return error_term;
    else return enif_make_badarg(env);
}

#endif // EVISION_BACKEND_BROADCAST_H
