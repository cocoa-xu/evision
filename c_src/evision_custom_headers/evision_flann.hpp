#ifdef HAVE_OPENCV_FLANN
// Note: evision_from/evision_to for cvflann::flann_algorithm_t and
// cvflann::flann_distance_t used to live here because gen2.py was not
// parsing modules/flann/include/opencv2/flann/defines.h, so the enums
// (and their CV_ERL_FROM_ENUM / CV_ERL_TO_ENUM converters) never made
// it into evision_generated_enums.h. That gap was fixed; the generated
// enums.h now declares those converters, so re-defining them here was
// a redefinition error. The IndexParams/SearchParams marshallers below
// remain hand-written — they map an Elixir map to cv::flann::IndexParams
// via setBool/setInt/setDouble/setString, which the binding generator
// cannot produce on its own.

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM o, cv::flann::IndexParams& p, const ArgInfo& info)
{
    CV_UNUSED(info);
    bool ok = true;
    ERL_NIF_TERM key;
    ERL_NIF_TERM value;

    if(enif_is_map(env, o)) {
        ErlNifMapIterator iter;
        enif_map_iterator_create(env, o, &iter, ERL_NIF_MAP_ITERATOR_FIRST);
        while(enif_map_iterator_get_pair(env, &iter, &key, &value))
        {
            // get key
            std::string k;
            if (!evision::nif::get(env, key, k))
            {
                ok = false;
                break;
            }
            // get value
            bool val;
            int i32;
            double f64;
            if( evision::nif::get(env, value, &val) )
            {
                p.setBool(k, val);
            }
            else if( enif_get_int(env, value, &i32) )
            {
                if( strcmp(k.c_str(), "algorithm") == 0 )
                    p.setAlgorithm(i32);
                else
                    p.setInt(k, i32);
            }
            else if( enif_get_double(env, value, &f64) )
            {
                p.setDouble(k, f64);
            }
            else
            {
                std::string val_str;
                if (!evision::nif::get(env, value, val_str))
                {
                    ok = false;
                    break;
                }
                p.setString(k, val_str);
            }
            enif_map_iterator_next(env, &iter);
        }
        enif_map_iterator_destroy(env, &iter);
    }

    return ok;
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, cv::flann::SearchParams & value, const ArgInfo& info)
{
    return evision_to<cv::flann::IndexParams>(env, obj, value, info);
}
#endif
