#ifdef HAVE_OPENCV_VIDEO

#ifndef EVISION_VIDEO_API_H
#define EVISION_VIDEO_API_H

template <>
ERL_NIF_TERM evision_from_as_map(ErlNifEnv *env, const cv::Ptr<cv::VideoCapture>& cap, ERL_NIF_TERM res_term, const char * class_name, bool& success) {
    const size_t num_items = 7;
    size_t item_index = 0;

    ERL_NIF_TERM keys[num_items];
    ERL_NIF_TERM values[num_items];

    keys[item_index] = evision::nif::atom(env, "frame_width");
    values[item_index] = enif_make_double(env, cap->get(cv::CAP_PROP_FRAME_WIDTH));
    item_index++;

    keys[item_index] = evision::nif::atom(env, "frame_height");
    values[item_index] = enif_make_double(env, cap->get(cv::CAP_PROP_FRAME_HEIGHT));
    item_index++;

    keys[item_index] = evision::nif::atom(env, "fps");
    values[item_index] = enif_make_double(env, cap->get(cv::CAP_PROP_FPS));
    item_index++;

    keys[item_index] = evision::nif::atom(env, "isOpened");
    values[item_index] = evision::nif::atom(env, cap->isOpened() ? "true" : "false");
    item_index++;

    keys[item_index] = evision::nif::atom(env, "frame_count");
    values[item_index] = enif_make_double(env, cap->get(cv::CAP_PROP_FRAME_COUNT));
    item_index++;

    keys[item_index] = evision::nif::atom(env, "ref");
    values[item_index] = res_term;
    item_index++;

    keys[item_index] = evision::nif::atom(env, "class");
    values[item_index] = evision::nif::atom(env, class_name);
    item_index++;

    ERL_NIF_TERM map;
    if (enif_make_map_from_arrays(env, keys, values, item_index, &map)) {
        success = true;
        return map;
    } else {
        success = false;
        return evision::nif::error(env, "enif_make_map_from_arrays failed in evision_from_as_map");
    }
}

#endif //  EVISION_VIDEO_API_H

#endif //  HAVE_OPENCV_VIDEO
