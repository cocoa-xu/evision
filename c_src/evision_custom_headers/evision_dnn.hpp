#ifdef HAVE_OPENCV_DNN
typedef dnn::DictValue LayerId;
typedef std::vector<dnn::MatShape> vector_MatShape;
typedef std::vector<std::vector<dnn::MatShape> > vector_vector_MatShape;

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, dnn::Net &net, const ArgInfo& info)
{
   if (evision::nif::check_nil(env, obj)) {
       return false;
   }

   evision_res<cv::dnn::Net> * in_res;
   if( enif_get_resource(env, obj, evision_res<cv::dnn::Net>::type, (void **)&in_res) ) {
       net = in_res->val;
       return true;
   }
   return false;
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const dnn::DetectionModel& model)
{
    evision_res<dnn::DetectionModel> * res;
    if (alloc_resource(&res)) {
        res->val = model;
    } else {
        return evision::nif::error(env, "no memory");
    }

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const dnn::Model& model)
{
    evision_res<dnn::Model> * res;
    if (alloc_resource(&res)) {
        res->val = model;
    } else {
        return evision::nif::error(env, "no memory");
    }

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

//template<>
//ERL_NIF_TERM evision_from(ErlNifEnv *env, const dnn::Net& model)
//{
//    printf("evision_from did read\r\n");
//    fflush(stdout);
//    evision_res<dnn::Model> * res;
//    if (alloc_resource(&res)) {
//        res->val = model;
//        printf("net: %p\r\n", &res->val);
//    } else {
//        return evision::nif::error(env, "no memory");
//    }
//
//    ERL_NIF_TERM ret = enif_make_resource(env, res);
//    enif_release_resource(res);
//    return ret;
//}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const dnn::TextDetectionModel_DB& model)
{
    evision_res<dnn::TextDetectionModel_DB> * res;
    if (alloc_resource(&res)) {
        res->val = model;
    } else {
        return evision::nif::error(env, "no memory");
    }

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const dnn::TextDetectionModel_EAST& model)
{
    evision_res<dnn::TextDetectionModel_EAST> * res;
    if (alloc_resource(&res)) {
        res->val = model;
    } else {
        return evision::nif::error(env, "no memory");
    }

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const dnn::TextRecognitionModel& model)
{
    evision_res<dnn::TextRecognitionModel> * res;
    if (alloc_resource(&res)) {
        res->val = model;
    } else {
        return evision::nif::error(env, "no memory");
    }

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, dnn::DictValue &dv, const ArgInfo& info)
{
    if (evision::nif::check_nil(env, obj)) {
        return true; //Current state will be used
    }

    ErlNifSInt64 int64_val;
    int int_val;
    double f64_val;
    if (enif_get_int64(env, obj, &int64_val))
    {
        dv = dnn::DictValue((int64)int64_val);
        return true;
    }
    else if (enif_get_int(env, obj, &int_val))
    {
        dv = dnn::DictValue((int64)int_val);
        return true;
    }
    else if (enif_get_double(env, obj, &f64_val))
    {
        dv = dnn::DictValue(f64_val);
        return true;
    }
    else
    {
        std::string str;
        if (evision::nif::get(env, obj, str))
        {
            dv = dnn::DictValue(str);
            return true;
        }
    }
    return false;
}

template<typename T>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const dnn::DictValue &dv)
{
    if (dv.size() > 1)
    {
        std::vector<T> vec(dv.size());
        for (int i = 0; i < dv.size(); ++i)
            vec[i] = dv.get<T>(i);
        return evision_from_generic_vec(env, vec);
    }
    else
        return evision_from(env, dv.get<T>());
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const dnn::DictValue &dv)
{
    if (dv.isInt()) return evision_from<int>(env, dv);
    if (dv.isReal()) return evision_from<float>(env, dv);
    if (dv.isString()) return evision_from<String>(env, dv);
    CV_Error(Error::StsNotImplemented, "Unknown value type");
    return evision::nif::atom(env, "nil");
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const dnn::LayerParams& lp)
{
    std::vector<ERL_NIF_TERM> _keys, _values;


    size_t i = 0;
    for (std::map<String, dnn::DictValue>::const_iterator it = lp.begin(); it != lp.end(); ++it)
    {
        _keys.push_back(evision::nif::make(env, it->first.c_str()));
        _values.push_back(evision_from(env, it->second));
        i++;
    }
    ERL_NIF_TERM dict;
    size_t s = _keys.size();
    ERL_NIF_TERM * keys = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * s);
    ERL_NIF_TERM * values = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * s);
    for (size_t i = 0; i < s; i++) {
        keys[i] = _keys[i];
        values[i] = _values[i];
    }

    if (enif_make_map_from_arrays(env, keys, values, i, &dict)) {
        enif_free((void *)keys);
        enif_free((void *)values);
        return dict;
    } else {
        enif_free((void *)keys);
        enif_free((void *)values);
        return evision::nif::atom(env, "nil");
    }
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const std::vector<dnn::Target> &t)
{
    return evision_from(env, std::vector<int>(t.begin(), t.end()));
}

#endif  // HAVE_OPENCV_DNN
