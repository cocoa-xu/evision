#ifndef EVISION_OPENCV_MAT_H
#define EVISION_OPENCV_MAT_H

#include <erl_nif.h>
#include "../nif_utils.hpp"

// @evision c: evision_cv_mat_type, 1
// @evision nif: def evision_cv_mat_type(_opts \\ []), do: :erlang.nif_error("Mat::type not loaded")
static ERL_NIF_TERM evision_cv_mat_type(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;

    {
        ERL_NIF_TERM erl_term_img = evision::nif::atom(env, "nil");
        Mat img;

        const char *keywords[] = {"img", NULL};
        if (0 < argc &&
            evision::nif::parse_arg(env, 0, argv, (char **) keywords, "O:type", &erl_term_img) &&
            evision_to_safe(env, erl_term_img, img, ArgInfo("img", 0))) {
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
    else return evision::nif::atom(env, "nil");
}

// @evision c: evision_cv_mat_shape, 1
// @evision nif: def evision_cv_mat_shape(_opts \\ []), do: :erlang.nif_error("Mat::shape not loaded")
static ERL_NIF_TERM evision_cv_mat_shape(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;

    {
        ERL_NIF_TERM erl_term_img = evision::nif::atom(env, "nil");
        Mat img;

        const char *keywords[] = {"img", NULL};
        if (0 < argc &&
            evision::nif::parse_arg(env, 0, argv, (char **) keywords, "O:shape", &erl_term_img) &&
            evision_to_safe(env, erl_term_img, img, ArgInfo("img", 0))) {
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
    else return evision::nif::atom(env, "nil");
}

// @evision c: evision_cv_mat_clone, 1
// @evision nif: def evision_cv_mat_clone(_opts \\ []), do: :erlang.nif_error("Mat::clone not loaded")
static ERL_NIF_TERM evision_cv_mat_clone(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;

    {
        ERL_NIF_TERM erl_term_img = evision::nif::atom(env, "nil");
        Mat img;
        Mat img_clone;

        const char *keywords[] = {"img", NULL};
        if (0 < argc &&
            evision::nif::parse_arg(env, 0, argv, (char **) keywords, "O:clone", &erl_term_img) &&
            evision_to_safe(env, erl_term_img, img, ArgInfo("img", 0))) {
            img_clone = img.clone();
            return evision::nif::ok(env, evision_from(env, img_clone));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::atom(env, "nil");
}

// @evision c: evision_cv_mat_to_binary, 1
// @evision nif: def evision_cv_mat_to_binary(_opts \\ []), do: :erlang.nif_error("Mat::to_binary not loaded")
static ERL_NIF_TERM evision_cv_mat_to_binary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    using namespace cv;
    ERL_NIF_TERM error_term = 0;

    {
        ERL_NIF_TERM erl_term_img = evision::nif::atom(env, "nil");
        Mat img;

        const char *keywords[] = {"img", NULL};
        if (0 < argc &&
            evision::nif::parse_arg(env, 0, argv, (char **) keywords, "O:to_binary", &erl_term_img) &&
            evision_to_safe(env, erl_term_img, img, ArgInfo("img", 0))) {
            ErlNifBinary bin_data;
            size_t bin_size = img.total() * img.elemSize();
            if (!enif_alloc_binary(bin_size, &bin_data))
                return evision::nif::error(env, "alloc_failed");

            memcpy(bin_data.data, img.data, bin_size);
            return evision::nif::ok(env, enif_make_binary(env, &bin_data));
        }
    }

    if (error_term != 0) return error_term;
    else return evision::nif::atom(env, "nil");
}

#endif // EVISION_OPENCV_MAT_H
