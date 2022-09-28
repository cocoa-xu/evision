#ifndef EVISION_BACKEND_FROM_BINARY_H
#define EVISION_BACKEND_FROM_BINARY_H

#include <erl_nif.h>
#include "../../ArgInfo.hpp"
#include "../evision_mat_utils.hpp"

// @evision c: mat_from_binary,evision_cv_mat_from_binary,1
// @evision nif: def mat_from_binary(_opts \\ []), do: :erlang.nif_error("Mat::from_binary not loaded")
static ERL_NIF_TERM evision_cv_mat_from_binary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        std::string t;
        int l = 0;
        int img_cols = 0;
        int img_rows = 0;
        int img_channels = 0;

        Mat retval;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "cols"), img_cols, ArgInfo("cols", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "rows"), img_rows, ArgInfo("rows", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "channels"), img_channels, ArgInfo("channels", 0)))
        {
            ERL_NIF_TERM erl_binary = evision_get_kw(env, erl_terms, "binary");

            ErlNifBinary data;
            if (enif_inspect_binary(env, erl_binary, &data)) {
                // validate binary data
                int type = 0;
                if (!get_binary_type(t, l, img_channels, type)) return evision::nif::error(env, "not implemented for the given type");
                size_t declared_size = img_rows * img_cols * img_channels * (l/8);
                if (declared_size != data.size) return evision::nif::error(env, "size mismatch");

                evision_res<cv::Mat *> * res;
                if (alloc_resource(&res)) {
                    if (_evision_binary_ref(erl_binary, res)) {
                        enif_release_resource(res);
                        return enif_make_badarg(env);
                    }
                    // https://docs.opencv.org/4.5.5/d3/d63/classcv_1_1Mat.html#a51615ebf17a64c968df0bf49b4de6a3a
                    // ...
                    // @param data 	Pointer to the user data.
                    // 	Matrix constructors that take data and step parameters do not allocate matrix data.
                    // 	Instead, they just initialize the matrix header that points to the specified data,
                    // 	which means that no data is copied. This operation is very efficient and can be used
                    // 	to process external data using OpenCV functions. The external data is not automatically
                    // 	deallocated, so you should take care of it.
                    res->val = new Mat(img_rows, img_cols, type, (void *)res->in_buf);

                    // transfer ownership to ERTS
                    ERL_NIF_TERM term = enif_make_resource(env, res);
                    enif_release_resource(res);

                    return evision::nif::ok(env, _evision_make_mat_resource_into_map(env, *res->val, term));
                } else {
                    return evision::nif::error(env, "no memory");
                }

                // return evision::nif::ok(env, evision_from(env, retval));
            } else {
                // invalid binary data
                return enif_make_badarg(env);
            }
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: mat_from_binary_by_shape,evision_cv_mat_from_binary_by_shape,1
// @evision nif: def mat_from_binary_by_shape(_opts \\ []), do: :erlang.nif_error("Mat::from_binary_by_shape not loaded")
static ERL_NIF_TERM evision_cv_mat_from_binary_by_shape(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        std::string t;
        int l = 0;
        std::vector<int> shape;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "shape"), shape, ArgInfo("shape", 0))) {
            ERL_NIF_TERM erl_binary = evision_get_kw(env, erl_terms, "binary");
            ErlNifBinary data;
            if (enif_inspect_binary(env, erl_binary, &data)) {
                // validate binary data
                int type = 0;
                int ndims = (int)shape.size();
                if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");

                evision_res<cv::Mat *> * res;
                if (alloc_resource(&res)) {
                    if (_evision_binary_ref(erl_binary, res)) {
                        enif_release_resource(res);
                        return enif_make_badarg(env);
                    }

                    // Mat(int ndims, const int* sizes, int type, void* data, const size_t* steps=0);
                    int * sizes = (int *)enif_alloc(sizeof(int) * ndims);
                    for (int i = 0; i < ndims; i++) {
                        sizes[i] = shape[i];
                    }

                    // https://docs.opencv.org/4.5.5/d3/d63/classcv_1_1Mat.html#a5fafc033e089143062fd31015b5d0f40
                    // ...
                    // @param data 	Pointer to the user data.
                    // 	Matrix constructors that take data and step parameters do not allocate matrix data.
                    // 	Instead, they just initialize the matrix header that points to the specified data,
                    // 	which means that no data is copied. This operation is very efficient and can be used
                    // 	to process external data using OpenCV functions. The external data is not automatically
                    // 	deallocated, so you should take care of it.
                    res->val = new Mat(ndims, sizes, type, (void *)res->in_buf);

                    enif_free((void *)sizes);

                    // transfer ownership to ERTS
                    ERL_NIF_TERM term = enif_make_resource(env, res);
                    enif_release_resource(res);

                    return evision::nif::ok(env, _evision_make_mat_resource_into_map(env, *res->val, term));
                } else {
                    return evision::nif::error(env, "no memory");
                }

                // return evision::nif::ok(env, evision_from(env, retval));
            } else {
                // invalid binary data
                return enif_make_badarg(env);
            }
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

#endif // EVISION_BACKEND_FROM_BINARY_H
