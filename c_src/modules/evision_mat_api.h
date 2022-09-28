#ifndef EVISION_MAT_API_H
#define EVISION_MAT_API_H

#include <erl_nif.h>

static ERL_NIF_TERM _evision_get_mat_type(ErlNifEnv *env, const cv::Mat& img) {
    int type = img.type();
    uint8_t depth = type & CV_MAT_DEPTH_MASK;

    switch ( depth ) {
        case CV_8U:  return enif_make_tuple2(env, evision::nif::atom(env, "u"), enif_make_uint64(env, 8));
        case CV_8S:  return enif_make_tuple2(env, evision::nif::atom(env, "s"), enif_make_uint64(env, 8));
        case CV_16U: return enif_make_tuple2(env, evision::nif::atom(env, "u"), enif_make_uint64(env, 16));
        case CV_16S: return enif_make_tuple2(env, evision::nif::atom(env, "s"), enif_make_uint64(env, 16));
        case CV_32S: return enif_make_tuple2(env, evision::nif::atom(env, "s"), enif_make_uint64(env, 32));
        case CV_32F: return enif_make_tuple2(env, evision::nif::atom(env, "f"), enif_make_uint64(env, 32));
        case CV_64F: return enif_make_tuple2(env, evision::nif::atom(env, "f"), enif_make_uint64(env, 64));
        case CV_16F: return enif_make_tuple2(env, evision::nif::atom(env, "f"), enif_make_uint64(env, 16));
        default:     return enif_make_tuple2(env, evision::nif::atom(env, "user"), enif_make_uint64(env, depth));
    }
}

static ERL_NIF_TERM _evision_get_mat_shape(ErlNifEnv *env, const cv::Mat& img) {
    cv::MatSize size = img.size;
    int channels = img.channels();
    int dims = size.dims();
    int include_channels = 0;
    if (!(img.type() == CV_8S || img.type() == CV_8U || img.type() == CV_16F \
        || img.type() == CV_16S || img.type() == CV_16U || img.type() == CV_32S \
        || img.type() == CV_32F || img.type() == CV_64F)) {
        dims += 1;
        include_channels = 1;
    }
    ERL_NIF_TERM* shape = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * dims);

    for (int i = 0; i < size.dims(); i++) {
        shape[i] = enif_make_int(env, size[i]);
    }
    if (include_channels) {
        shape[dims - 1] = enif_make_int(env, channels);
    }
    ERL_NIF_TERM ret = enif_make_tuple_from_array(env, shape, dims);
    return ret;
}

#endif  // EVISION_MAT_API_H
