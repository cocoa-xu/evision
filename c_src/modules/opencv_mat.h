#ifndef EVISION_OPENCV_MAT_H
#define EVISION_OPENCV_MAT_H

#include <erl_nif.h>
#include <math.h>
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

// @evision c: evision_cv_mat_eye, 1
// @evision nif: def evision_cv_mat_eye(_opts \\ []), do: :erlang.nif_error("Mat::eye not loaded")
static ERL_NIF_TERM evision_cv_mat_eye(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        std::string t;
        int l = 0;
        uint64_t n = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "n"), n, ArgInfo("n", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type = 0;
            if (!get_binary_type(t, l, 1, type)) return evision::nif::error(env, "not implemented for the given type");
            Mat out = Mat::eye(n, n, type);
            return evision::nif::ok(env, evision_from(env, out));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_reshape, 1
// @evision nif: def evision_cv_mat_reshape(_opts \\ []), do: :erlang.nif_error("Mat::reshape not loaded")
static ERL_NIF_TERM evision_cv_mat_reshape(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat mat;
        std::vector<int> shape;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "mat"), mat, ArgInfo("mat", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "shape"), shape, ArgInfo("shape", 0))) {
            Mat out = mat.reshape(0, shape);
            return evision::nif::ok(env, evision_from(env, out));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_zeros, 1
// @evision nif: def evision_cv_mat_zeros(_opts \\ []), do: :erlang.nif_error("Mat::zeros not loaded")
static ERL_NIF_TERM evision_cv_mat_zeros(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        std::vector<int> shape;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "shape"), shape, ArgInfo("shape", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");
            Mat out = Mat(Mat::zeros(shape.size(), shape.data(), type));
            return evision::nif::ok(env, evision_from(env, out));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_ones, 1
// @evision nif: def evision_cv_mat_ones(_opts \\ []), do: :erlang.nif_error("Mat::ones not loaded")
static ERL_NIF_TERM evision_cv_mat_ones(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        std::vector<int> shape;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "shape"), shape, ArgInfo("shape", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");
            Mat out = Mat(Mat::ones(shape.size(), shape.data(), type));
            return evision::nif::ok(env, evision_from(env, out));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_arange, 1
// @evision nif: def evision_cv_mat_arange(_opts \\ []), do: :erlang.nif_error("Mat::arange not loaded")
static ERL_NIF_TERM evision_cv_mat_arange(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        int64_t from = 0, to = 0, step = 0;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "from"), from, ArgInfo("from", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "to"), to, ArgInfo("to", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "step"), step, ArgInfo("step", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");

            int64_t count = (to - from) / step;
            int dims[1] = {0};
            dims[0] = (int)count;
            if (count <= 0) return evision::nif::error(env, "invalid values for start/end/step");

            std::vector<double> values(count);
            int64_t v = from;
            for (int64_t i = 0; i < count; i++) {
                values[i] = v;
                v += step;
            }

            Mat out = Mat(1, dims, CV_64F, values.data());
            Mat ret;
            out.convertTo(ret, type);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_full, 1
// @evision nif: def evision_cv_mat_full(_opts \\ []), do: :erlang.nif_error("Mat::full not loaded")
static ERL_NIF_TERM evision_cv_mat_full(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        double number;
        std::vector<int> shape;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "number"), number, ArgInfo("from", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "shape"), shape, ArgInfo("to", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");

            Mat out = Mat(Mat::ones(shape.size(), shape.data(), CV_64F) * number);
            Mat ret;
            out.convertTo(ret, type);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_release, 1
// @evision nif: def evision_cv_mat_release(_opts \\ []), do: :erlang.nif_error("Mat::release not loaded")
static ERL_NIF_TERM evision_cv_mat_release(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            img.release();
            return evision::nif::ok(env);
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_at, 1
// @evision nif: def evision_cv_mat_at(_opts \\ []), do: :erlang.nif_error("Mat::at not loaded")
static ERL_NIF_TERM evision_cv_mat_at(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;
        size_t pos;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "pos"), pos, ArgInfo("img", 0))) {

            int type = img.type();
            uint8_t depth = type & CV_MAT_DEPTH_MASK;

            int i32;
            double f64;
            type = -1;

            switch ( depth ) {
                case CV_8U: {
                    i32 = ((uint8_t *)img.data)[pos]; type = 0;
                    break;
                }
                case CV_16U: {
                    i32 = ((uint16_t *)img.data)[pos]; type = 0;
                    break;
                }
                case CV_8S: {
                    i32 = ((int8_t *)img.data)[pos]; type = 0;
                    break;
                }
                case CV_16S: {
                    i32 = ((int16_t *)img.data)[pos]; type = 0;
                    break;
                }
                case CV_32S: {
                    i32 = ((int32_t *)img.data)[pos]; type = 0;
                    break;
                }

                case CV_32F: {
                    f64 = ((float *)img.data)[pos]; type = 1;
                    break;
                }
                case CV_64F: {
                    f64 = ((double *)img.data)[pos]; type = 1;
                    break;
                }
            }

            ERL_NIF_TERM ret;
            if (type == 0) {
                ret = evision::nif::ok(env, enif_make_int(env, i32));
            } else if (type == 1) {
                ret = evision::nif::ok(env, enif_make_double(env, f64));
            } else {
                ret = evision::nif::error(env, "unknown data type");
            }

            return ret;
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_add, 1
// @evision nif: def evision_cv_mat_add(_opts \\ []), do: :erlang.nif_error("Mat::add not loaded")
static ERL_NIF_TERM evision_cv_mat_add(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat l;
        Mat r;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", 0))) {
            Mat ret;
            cv::add(l, r, ret, cv::noArray(), -1);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_add_typed, 1
// @evision nif: def evision_cv_mat_add_typed(_opts \\ []), do: :erlang.nif_error("Mat::add not loaded")
static ERL_NIF_TERM evision_cv_mat_add_typed(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat lhs;
        Mat rhs;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "lhs"), lhs, ArgInfo("lhs", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "rhs"), rhs, ArgInfo("rhs", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");
            Mat ret;
            cv::add(lhs, rhs, ret, cv::noArray(), type);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_subtract, 1
// @evision nif: def evision_cv_mat_subtract(_opts \\ []), do: :erlang.nif_error("Mat::subtract not loaded")
static ERL_NIF_TERM evision_cv_mat_subtract(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat l;
        Mat r;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", 0))) {
            Mat ret;
            cv::subtract(l, r, ret, cv::noArray(), -1);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_subtract_typed, 1
// @evision nif: def evision_cv_mat_subtract_typed(_opts \\ []), do: :erlang.nif_error("Mat::subtract not loaded")
static ERL_NIF_TERM evision_cv_mat_subtract_typed(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat lhs;
        Mat rhs;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "lhs"), lhs, ArgInfo("lhs", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "rhs"), rhs, ArgInfo("rhs", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");
            Mat ret;
            cv::subtract(lhs, rhs, ret, cv::noArray(), type);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_multiply, 1
// @evision nif: def evision_cv_mat_multiply(_opts \\ []), do: :erlang.nif_error("Mat::multiply not loaded")
static ERL_NIF_TERM evision_cv_mat_multiply(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat l;
        Mat r;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", 0))) {
            Mat ret;
            cv::multiply(l, r, ret, 1, -1);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_multiply_typed, 1
// @evision nif: def evision_cv_mat_multiply_typed(_opts \\ []), do: :erlang.nif_error("Mat::multiply not loaded")
static ERL_NIF_TERM evision_cv_mat_multiply_typed(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat lhs;
        Mat rhs;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "lhs"), lhs, ArgInfo("lhs", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "rhs"), rhs, ArgInfo("rhs", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");
            Mat ret;
            cv::multiply(lhs, rhs, ret, 1, type);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_matrix_multiply, 1
// @evision nif: def evision_cv_mat_matrix_multiply(_opts \\ []), do: :erlang.nif_error("Mat::matrix_multiply not loaded")
static ERL_NIF_TERM evision_cv_mat_matrix_multiply(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat l;
        Mat r;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", 0))) {
            return evision::nif::ok(env, evision_from(env, Mat(l * r)));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_divide, 1
// @evision nif: def evision_cv_mat_divide(_opts \\ []), do: :erlang.nif_error("Mat::divide not loaded")
static ERL_NIF_TERM evision_cv_mat_divide(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat l;
        Mat r;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", 0))) {
            Mat ret;
            cv::divide(l, r, ret, 1, -1);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_divide_typed, 1
// @evision nif: def evision_cv_mat_divide_typed(_opts \\ []), do: :erlang.nif_error("Mat::divide not loaded")
static ERL_NIF_TERM evision_cv_mat_divide_typed(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat lhs;
        Mat rhs;
        std::string t;
        int l = 0;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "lhs"), lhs, ArgInfo("lhs", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "rhs"), rhs, ArgInfo("rhs", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "t"), t, ArgInfo("t", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0))) {
            int type;
            if (!get_binary_type(t, l, 0, type)) return evision::nif::error(env, "not implemented for the given type");
            Mat ret;
            cv::divide(lhs, rhs, ret, 1, type);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_bitwise_and, 1
// @evision nif: def evision_cv_mat_bitwise_and(_opts \\ []), do: :erlang.nif_error("Mat::bitwise_and not loaded")
static ERL_NIF_TERM evision_cv_mat_bitwise_and(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat l;
        Mat r;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", 0))) {
            Mat ret;
            cv::bitwise_and(l, r, ret);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_bitwise_or, 1
// @evision nif: def evision_cv_mat_bitwise_or(_opts \\ []), do: :erlang.nif_error("Mat::bitwise_or not loaded")
static ERL_NIF_TERM evision_cv_mat_bitwise_or(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat l;
        Mat r;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", 0))) {
            Mat ret;
            cv::bitwise_or(l, r, ret);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_bitwise_xor, 1
// @evision nif: def evision_cv_mat_bitwise_xor(_opts \\ []), do: :erlang.nif_error("Mat::bitwise_xor not loaded")
static ERL_NIF_TERM evision_cv_mat_bitwise_xor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat l;
        Mat r;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", 0))) {
            Mat ret;
            cv::bitwise_xor(l, r, ret);
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_cmp, 1
// @evision nif: def evision_cv_mat_cmp(_opts \\ []), do: :erlang.nif_error("Mat::greater not loaded")
static ERL_NIF_TERM evision_cv_mat_cmp(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat l;
        Mat r;
        std::string type;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "l"), l, ArgInfo("l", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "r"), r, ArgInfo("r", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "type"), type, ArgInfo("type", 0))) {

            int cmpop = -1;
            if (type == "eq") cmpop = cv::CMP_EQ;
            else if (type == "gt") cmpop = cv::CMP_GT;
            else if (type == "ge") cmpop = cv::CMP_GE;
            else if (type == "lt") cmpop = cv::CMP_LT;
            else if (type == "le") cmpop = cv::CMP_LE;
            else if (type == "ne") cmpop = cv::CMP_NE;
            else {
                return evision::nif::error(env, "not implemented for the requested compare type, only 'eq', 'gt', 'ge', 'lt', 'le' and 'ne' are supported.");
            }

            Mat ret;
            cv::compare(l, r, ret, cmpop);
            ret = ret / 255;
            return evision::nif::ok(env, evision_from(env, ret));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_abs, 1
// @evision nif: def evision_cv_mat_abs(_opts \\ []), do: :erlang.nif_error("Mat::abs not loaded")
static ERL_NIF_TERM evision_cv_mat_abs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            return evision::nif::ok(env, evision_from(env, Mat(cv::abs(img))));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_expm1, 1
// @evision nif: def evision_cv_mat_expm1(_opts \\ []), do: :erlang.nif_error("Mat::expm1 not loaded")
static ERL_NIF_TERM evision_cv_mat_expm1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            Mat out;
            cv::exp(img, out);
            return evision::nif::ok(env, evision_from(env, Mat(out - 1)));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_clip, 1
// @evision nif: def evision_cv_mat_clip(_opts \\ []), do: :erlang.nif_error("Mat::clip not loaded")
static ERL_NIF_TERM evision_cv_mat_clip(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;
        double lower;
        double upper;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "lower"), lower, ArgInfo("lower", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "upper"), upper, ArgInfo("upper", 0))) {
            img.setTo(lower, img < lower);
            img.setTo(upper, img > upper);
            return evision::nif::ok(env, evision_from(env, img));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_set_to, 1
// @evision nif: def evision_cv_mat_set_to(_opts \\ []), do: :erlang.nif_error("Mat::setTo not loaded")
static ERL_NIF_TERM evision_cv_mat_set_to(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;
        double value;
        Mat mask;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "value"), value, ArgInfo("value", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "mask"), mask, ArgInfo("mask", 0))) {
            img.setTo(value, mask);
            return evision::nif::ok(env, evision_from(env, img));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_dot, 1
// @evision nif: def evision_cv_mat_dot(_opts \\ []), do: :erlang.nif_error("Mat::dot not loaded")
static ERL_NIF_TERM evision_cv_mat_dot(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat a, b;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "a"), a, ArgInfo("a", 0)) &&
            evision_to_safe(env, evision_get_kw(env, erl_terms, "b"), b, ArgInfo("b", 0))) {
            Mat out = a.cross(b);
            return evision::nif::ok(env, evision_from(env, out));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_bitwise_not, 1
// @evision nif: def evision_cv_mat_bitwise_not(_opts \\ []), do: :erlang.nif_error("Mat::bitwise_not not loaded")
static ERL_NIF_TERM evision_cv_mat_bitwise_not(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            Mat out = Mat(~img);
            return evision::nif::ok(env, evision_from(env, out));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_ceil, 1
// @evision nif: def evision_cv_mat_ceil(_opts \\ []), do: :erlang.nif_error("Mat::ceil not loaded")
static ERL_NIF_TERM evision_cv_mat_ceil(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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
            if (depth == CV_32F) {
                auto ptr = img.ptr<float>();
                size_t count = img.total();
                for (size_t i = 0; i < count; ++i) {
                    ptr[i] = ceilf(ptr[i]);
                }
                return evision::nif::ok(env, evision_from(env, img));
            } else if (depth == CV_64F) {
                auto ptr = img.ptr<double>();
                size_t count = img.total();
                for (size_t i = 0; i < count; ++i) {
                    ptr[i] = ceil(ptr[i]);
                }
                return evision::nif::ok(env, evision_from(env, img));
            } else {
                return evision::nif::ok(env, evision_from(env, img));
            }
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_floor, 1
// @evision nif: def evision_cv_mat_floor(_opts \\ []), do: :erlang.nif_error("Mat::floor not loaded")
static ERL_NIF_TERM evision_cv_mat_floor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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
            if (depth == CV_32F) {
                auto ptr = img.ptr<float>();
                size_t count = img.total();
                for (size_t i = 0; i < count; ++i) {
                    ptr[i] = floorf(ptr[i]);
                }
                return evision::nif::ok(env, evision_from(env, img));
            } else if (depth == CV_64F) {
                auto ptr = img.ptr<double>();
                size_t count = img.total();
                for (size_t i = 0; i < count; ++i) {
                    ptr[i] = floor(ptr[i]);
                }
                return evision::nif::ok(env, evision_from(env, img));
            } else {
                return evision::nif::ok(env, evision_from(env, img));
            }
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_negate, 1
// @evision nif: def evision_cv_mat_negate(_opts \\ []), do: :erlang.nif_error("Mat::negate not loaded")
static ERL_NIF_TERM evision_cv_mat_negate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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

            if (depth == CV_8U) {
                auto ptr = img.ptr<uint8_t>();
                size_t count = img.total();
                for (size_t i = 0; i < count; ++i) {
                    ptr[i] = 0 - ptr[i];
                }
                return evision::nif::ok(env, evision_from(env, img));
            } else if (depth == CV_16U) {
                auto ptr = img.ptr<uint16_t>();
                size_t count = img.total();
                for (size_t i = 0; i < count; ++i) {
                    ptr[i] = 0 - ptr[i];
                }
                return evision::nif::ok(env, evision_from(env, img));
            } else {
                Mat out = Mat(-img);
                return evision::nif::ok(env, evision_from(env, out));
            }
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_round, 1
// @evision nif: def evision_cv_mat_round(_opts \\ []), do: :erlang.nif_error("Mat::round not loaded")
static ERL_NIF_TERM evision_cv_mat_round(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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
            if (depth == CV_32F) {
                auto ptr = img.ptr<float>();
                size_t count = img.total();
                for (size_t i = 0; i < count; ++i) {
                    ptr[i] = roundf(ptr[i]);
                }
                return evision::nif::ok(env, evision_from(env, img));
            } else if (depth == CV_64F) {
                auto ptr = img.ptr<double>();
                size_t count = img.total();
                for (size_t i = 0; i < count; ++i) {
                    ptr[i] = round(ptr[i]);
                }
                return evision::nif::ok(env, evision_from(env, img));
            } else {
                return evision::nif::ok(env, evision_from(env, img));
            }
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

// @evision c: evision_cv_mat_sign, 1
// @evision nif: def evision_cv_mat_sign(_opts \\ []), do: :erlang.nif_error("Mat::sign not loaded")
static ERL_NIF_TERM evision_cv_mat_sign(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;
    std::map<std::string, ERL_NIF_TERM> erl_terms;
    int nif_opts_index = 0;
    evision::nif::parse_arg(env, nif_opts_index, argv, erl_terms);

    {
        Mat img;

        if (evision_to_safe(env, evision_get_kw(env, erl_terms, "img"), img, ArgInfo("img", 0))) {
            img.setTo(1, img > 0);
            img.setTo(0, img == 0);
            img.setTo(-1, img < 0);
            return evision::nif::ok(env, evision_from(env, img));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::error(env, "overload resolution failed");
}

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

    for (ssize_t dim = ndims - 1; dim >= 0; --dim) {
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

// @evision c: evision_cv_mat_broadcast_to, 1
// @evision nif: def evision_cv_mat_broadcast_to(_opts \\ []), do: :erlang.nif_error("Mat::broadcast_to not loaded")
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
            ssize_t ndims = to_shape.size();
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
            for (ssize_t i = 0; i < ndims; ++i) {
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
            for (ssize_t i = 0; i < ndims; i++) {
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

#endif // EVISION_OPENCV_MAT_H
