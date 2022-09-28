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

static ERL_NIF_TERM _evision_make_mat_resource_into_map(ErlNifEnv *env, const cv::Mat& m, ERL_NIF_TERM res_term) {
    const size_t num_items = 7;
    size_t item_index = 0;

    ERL_NIF_TERM keys[num_items];
    ERL_NIF_TERM values[num_items];

    keys[item_index] = enif_make_atom(env, "channels");
    values[item_index] = enif_make_int(env, m.channels());
    item_index++;

    keys[item_index] = enif_make_atom(env, "dims");
    values[item_index] = enif_make_int(env, m.dims);
    item_index++;

    keys[item_index] = enif_make_atom(env, "type");
    values[item_index] = _evision_get_mat_type(env, m);
    item_index++;

    keys[item_index] = enif_make_atom(env, "raw_type");
    values[item_index] = enif_make_int(env, m.type());
    item_index++;

    keys[item_index] = enif_make_atom(env, "shape");
    values[item_index] = _evision_get_mat_shape(env, m);
    item_index++;

    keys[item_index] = enif_make_atom(env, "ref");
    values[item_index] = res_term;
    item_index++;

    keys[item_index] = enif_make_atom(env, "class");
    values[item_index] = enif_make_atom(env, "Mat");
    item_index++;

    ERL_NIF_TERM map;
    if (enif_make_map_from_arrays(env, keys, values, item_index, &map)) {
        return map;
    } else {
        return enif_make_atom(env, "error when making map from arrays");
    }
}

#endif  // EVISION_MAT_API_H
