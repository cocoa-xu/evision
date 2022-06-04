#ifndef EVISION_OPENCV_MAT_H
#define EVISION_OPENCV_MAT_H

#include <erl_nif.h>
#include "../nif_utils.hpp"

int get_binary_type(const std::string& t, int l, int n, int& type);

// @evision c: evision_cv_mat_empty, 0
// @evision nif: def evision_cv_mat_empty(), do: :erlang.nif_error("Mat::empty not loaded")
static ERL_NIF_TERM evision_cv_mat_empty(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    evision_res<cv::Mat *> * res;
    if (alloc_resource(&res)) {
        res->val = new cv::Mat();
    } else {
        return evision::nif::error(env, "no memory");
    }

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);

    return evision::nif::ok(env, ret);
}

// @evision c: evision_cv_mat_type, 1
// @evision nif: def evision_cv_mat_type(_opts \\ []), do: :erlang.nif_error("Mat::type not loaded")
static ERL_NIF_TERM evision_cv_mat_type(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            int type = img.type();
            uint8_t depth = type & CV_MAT_DEPTH_MASK;

            switch ( depth ) {
                case CV_8U:  return evision::nif::ok(env, enif_make_tuple2(env, evision::nif::atom(env, "u"), enif_make_uint64(env, 8)));
                case CV_8S:  return evision::nif::ok(env, enif_make_tuple2(env, evision::nif::atom(env, "s"), enif_make_uint64(env, 8)));
                case CV_16U: return evision::nif::ok(env, enif_make_tuple2(env, evision::nif::atom(env, "u"), enif_make_uint64(env, 16)));
                case CV_16S: return evision::nif::ok(env, enif_make_tuple2(env, evision::nif::atom(env, "s"), enif_make_uint64(env, 16)));
                case CV_32S: return evision::nif::ok(env, enif_make_tuple2(env, evision::nif::atom(env, "s"), enif_make_uint64(env, 32)));
                case CV_32F: return evision::nif::ok(env, enif_make_tuple2(env, evision::nif::atom(env, "f"), enif_make_uint64(env, 32)));
                case CV_64F: return evision::nif::ok(env, enif_make_tuple2(env, evision::nif::atom(env, "f"), enif_make_uint64(env, 64)));
                default:     return evision::nif::ok(env, enif_make_tuple2(env, evision::nif::atom(env, "user"), enif_make_uint64(env, depth)));
            }
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_as_type, 1
// @evision nif: def evision_cv_mat_as_type(_opts \\ []), do: :erlang.nif_error("Mat::as_type not loaded")
static ERL_NIF_TERM evision_cv_mat_as_type(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;
        std::string t;
        int l = 0;
        int type;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            if (get_binary_type(t, l, 0, type)) {
                Mat ret;
                img.convertTo(ret, type);
                return evision::nif::ok(env, evision_from(env, ret));
            } else {
                return evision::nif::error(env, "unsupported target type");
            }
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_shape, 1
// @evision nif: def evision_cv_mat_shape(_opts \\ []), do: :erlang.nif_error("Mat::shape not loaded")
static ERL_NIF_TERM evision_cv_mat_shape(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        // const char *keywords[] = {"img", NULL};
        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            cv::MatSize size = img.size;
            int channels = img.channels();
            int dims = size.dims() + (channels == 1 ? 0 : 1);
            ERL_NIF_TERM* shape = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * dims);

            for (int i = 0; i < size.dims(); i++) {
                shape[i] = enif_make_int(env, size[i]);
            }
            if (channels > 1) {
                shape[dims - 1] = enif_make_int(env, channels);
            }
            ERL_NIF_TERM ret = enif_make_tuple_from_array(env, shape, dims);
            return evision::nif::ok(env, ret);
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_clone, 1
// @evision nif: def evision_cv_mat_clone(_opts \\ []), do: :erlang.nif_error("Mat::clone not loaded")
static ERL_NIF_TERM evision_cv_mat_clone(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        // const char *keywords[] = {"img", NULL};
        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            // no need to do clone here as evision_from will copy the data
            return evision::nif::ok(env, evision_from(env, img));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_to_binary, 1
// @evision nif: def evision_cv_mat_to_binary(_opts \\ []), do: :erlang.nif_error("Mat::to_binary not loaded")
static ERL_NIF_TERM evision_cv_mat_to_binary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        // const char *keywords[] = {"img", NULL};
        // zero-copy to_binary
        evision_res<cv::Mat *> * res;
        if( enif_get_resource(env, evision_get_kw(env, erl_terms, "img"), evision_res<cv::Mat *>::type, (void **)&res) ) {
            size_t bin_size = res->val->total() * res->val->elemSize();
            ERL_NIF_TERM out_bin_term = enif_make_resource_binary(env, res, res->val->data, bin_size);
            return evision::nif::ok(env, out_bin_term);
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

int get_binary_type(const std::string& t, int l, int n, int& type) {
    if (t == "u") {
        if (l == 8) {
            if (n != 0) type = CV_8UC(n);
            else type = CV_8U;
            return true;
        }
        if (l == 16) {
            if (n != 0) type = CV_16UC(n);
            else type = CV_16U;
            return true;
        }
    } else if (t == "s") {
        if (l == 8) {
            if (n != 0) type = CV_8SC(n);
            else type = CV_8S;
            return true;
        }
        if (l == 16) {
            if (n != 0) type = CV_16SC(n);
            else type = CV_16S;
            return true;
        }
        if (l == 32) {
            if (n != 0) type = CV_32SC(n);
            else type = CV_32S;
            return true;
        }
    } else if (t == "f") {
        if (l == 32) {
            if (n != 0) type = CV_32FC(n);
            else type = CV_32F;
            return true;
        }
        if (l == 64) {
            if (n != 0) type = CV_64FC(n);
            else type = CV_64F;
            return true;
        }
    }
    return false;
}

static void _evision_binary_unref(void *buf, ErlNifEnv *env) {
    // enif_fprintf(stderr, "freed nif_env\n");
    // freeing env frees unref all terms it contains
    enif_free_env(env);
    return;
}

static ERL_NIF_TERM _evision_binary_ref(ERL_NIF_TERM bin_term, evision_res<cv::Mat *> * zero_copy_mat) {
    // adapted from https://github.com/akash-akya/zero_copy/blob/master/c_src/zero_copy.c
    ErlNifBinary bin;
    ERL_NIF_TERM term;
    ErlNifEnv *new_env;

    zero_copy_mat->in_buf = nullptr;
    zero_copy_mat->in_ref = nullptr;

    // keep reference to binary by creating new nif-env and copying
    // binary-term reference to it
    new_env = enif_alloc_env();
    term = enif_make_copy(new_env, bin_term);

    if (!enif_inspect_binary(new_env, term, &bin)) {
        enif_free_env(new_env);
        return 1;
    }

    // Note that we are *NOT* copying the binary data
    zero_copy_mat->in_buf = bin.data;

    // input buffer specific opaque data which will be passed as second
    // argument to finalizer during unref
    zero_copy_mat->in_ref = (void *)new_env;
    // function to be called to unref the input data
    zero_copy_mat->in_unref = (void (*)(void *, void *))_evision_binary_unref;

    return 0;
}

// @evision c: evision_cv_mat_from_binary, 1
// @evision nif: def evision_cv_mat_from_binary(_opts \\ []), do: :erlang.nif_error("Mat::from_binary not loaded")
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
                    return evision::nif::ok(env, term);
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

// @evision c: evision_cv_mat_from_binary_by_shape, 1
// @evision nif: def evision_cv_mat_from_binary_by_shape(_opts \\ []), do: :erlang.nif_error("Mat::from_binary_by_shape not loaded")
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
                if (!get_binary_type(t, l, 1, type)) return evision::nif::error(env, "not implemented for the given type");

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
                    return evision::nif::ok(env, term);
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

#endif // EVISION_OPENCV_MAT_H
