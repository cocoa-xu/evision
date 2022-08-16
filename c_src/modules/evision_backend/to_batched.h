#ifndef EVISION_BACKEND_TO_BATCHED_LIST_H
#define EVISION_BACKEND_TO_BATCHED_LIST_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"

// @evision c: mat_to_batched,evision_cv_mat_to_batched,1
// @evision nif: def mat_to_batched(_opts \\ []), do: :erlang.nif_error("Mat::to_batched not loaded")
static ERL_NIF_TERM evision_cv_mat_to_batched(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    Mat img;
    uint64_t batch_size;
    std::vector<int> as_shape;
    std::string leftover;

    if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("l", 0)) &&
        evision_to_safe(env, evision_get_kw(env, erl_terms, "batch_size"), batch_size, ArgInfo("batch_size", 0)) &&
        evision_to_safe(env, evision_get_kw(env, erl_terms, "as_shape"), as_shape, ArgInfo("as_shape", 0)) &&
        evision_to_safe(env, evision_get_kw(env, erl_terms, "leftover"), leftover, ArgInfo("leftover", 0))) {
        if (leftover != "repeat" && leftover != "discard") {
            return evision::nif::error(env, "to_batched failed: invalid option value for leftover. Valid values are :repeat and :discard");
        }

        size_t mat_num_elem = img.total();
        size_t as_shape_num_elem = 1;
        for (size_t i = 0; i < as_shape.size(); i++) {
            as_shape_num_elem *= as_shape[i];
        }
        if (mat_num_elem != as_shape_num_elem) {
            return evision::nif::error(env, "to_batched failed: cannot treated matrix as the request shape");
        }
        
        uint64_t remainder = as_shape[0] % batch_size;
        uint64_t num_full_batches = as_shape[0] / batch_size;
        uint64_t slice_size = as_shape_num_elem / as_shape[0] * img.elemSize();
        uint64_t batch_bytes = slice_size * batch_size;

        unsigned num_batches = (unsigned)num_full_batches;
        if (remainder != 0) {
            if (leftover == "repeat") {
                num_batches += 1;
            }
        }

        ERL_NIF_TERM * batches = (ERL_NIF_TERM * )enif_alloc(sizeof(ERL_NIF_TERM) * num_batches);
        char * data = (char *)img.data;
        // skip the first (batches)
        int ndims = (int)as_shape.size();
        as_shape[0] = (int)batch_size;
        int * sizes = (int *)as_shape.data();

        // deal with full batches
        char * offset_data = data;
        for (size_t i = 0; i < num_full_batches; i++) {
            Mat mat = Mat(ndims, sizes, img.type(), offset_data);
            batches[i] = evision_from(env, mat.clone());
            offset_data += batch_bytes;
        }
        // deal with leftover
        if (num_batches != num_full_batches) {
            char * last_batch_data = (char *)enif_alloc(batch_bytes);

            // copy leftover first
            uint64_t leftover_bytes = slice_size * remainder;
            memcpy(last_batch_data, offset_data, leftover_bytes);

            // repeat from the beginning
            uint64_t repeat_bytes = batch_bytes - leftover_bytes;

            memcpy(last_batch_data + leftover_bytes, data, repeat_bytes);
            Mat mat = Mat(ndims, sizes, img.type(), last_batch_data);
            batches[num_batches - 1] = evision_from(env, mat.clone());
            enif_free(last_batch_data);
        }

        ERL_NIF_TERM ret = enif_make_list_from_array(env, batches, num_batches);
        enif_free(batches);

        return evision::nif::ok(env, ret);
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_TO_BATCHED_LIST_H
