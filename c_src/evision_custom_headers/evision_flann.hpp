#ifdef HAVE_OPENCV_FLANN
typedef cvflann::flann_distance_t cvflann_flann_distance_t;
typedef cvflann::flann_algorithm_t cvflann_flann_algorithm_t;

// template<>
// ERL_NIF_TERM evision_from(ErlNifEnv *env, const cvflann_flann_algorithm_t& value)
// {
//     return enif_make_int(env, int(value));
// }

// template<>
// ERL_NIF_TERM evision_from(ErlNifEnv *env, const cvflann_flann_distance_t& value)
// {
//     return enif_make_int(env, int(value));
// }

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

// template<>
// bool evision_to(ErlNifEnv *env, ERL_NIF_TERM o, cvflann::flann_distance_t& dist, const ArgInfo& info)
// {
//     int d = (int)dist;
//     bool ok = evision_to(env, o, d, info);
//     dist = (cvflann::flann_distance_t)d;
//     return ok;
// }
#endif
