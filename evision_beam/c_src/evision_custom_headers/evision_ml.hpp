#ifdef HAVE_OPENCV_ML

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, CvTermCriteria& dst, const ArgInfo& info)
{
    CV_UNUSED(info);
    if (evision::nif::check_nil(env, obj)) {
        return true;
    }

    const ERL_NIF_TERM *terms;
    int length;
    if (!enif_get_tuple(env, obj, &length, &terms)) {
        failmsg(env, "Can't parse '%s' as TermCriteria."
                "Input argument is not a tuple",
                info.name);
        return false;
    }
    const std::size_t sequenceSize = length;
    if (sequenceSize != 3)
    {
        failmsg(env, "Can't parse '%s' as CvTermCriteria. Expected sequence length 3, got %lu",
                info.name, sequenceSize);
        return false;
    }

    {
        const String typeItemName = format("'%s' type", info.name);
        const ArgInfo typeItemInfo(typeItemName.c_str(), false);
        if (!evision_to(env, terms[0], dst.type, typeItemInfo))
        {
            return false;
        }
    }
    {
        const String maxIterItemName = format("'%s' max_iter", info.name);
        const ArgInfo maxIterItemInfo(maxIterItemName.c_str(), false);
        if (!evision_to(env, terms[1], dst.max_iter, maxIterItemInfo))
        {
            return false;
        }
    }
    {
        const String epsilonItemName = format("'%s' epsilon", info.name);
        const ArgInfo epsilonItemInfo(epsilonItemName.c_str(), false);
        if (!evision_to(env, terms[2], dst.epsilon, epsilonItemInfo))
        {
            return false;
        }
    }

    return true;
}

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM obj, CvSlice& r, const ArgInfo& info)
{
    CV_UNUSED(info);
    if (evision::nif::check_nil(env, obj)) {
        return true;
    }

    const ERL_NIF_TERM *terms;
    int length;
    if (!enif_get_tuple(env, obj, &length, &terms)) {
        failmsg(env, "Can't parse '%s' as CvSlice."
                "Input argument is not a tuple",
                info.name);
        return false;
    }
    const std::size_t sequenceSize = length;
    if(sequenceSize == 0)
    {
        r = CV_WHOLE_SEQ;
        return true;
    }

    if (sequenceSize != 2)
    {
        failmsg(env, "Can't parse '%s' as CvSlice. Expected sequence length 2, got %lu",
                info.name, sequenceSize);
        return false;
    }
    {
        const String startIndexItemName = format("'%s' start index", info.name);
        const ArgInfo startIndexItemInfo(startIndexItemName.c_str(), false);
        if (!evision_to(env, terms[0], r.start_index, startIndexItemInfo))
        {
            return false;
        }
    }
    {
        const String endIndexItemName = format("'%s' end index", info.name);
        const ArgInfo endIndexItemInfo(endIndexItemName.c_str(), false);
        if (!evision_to(env, terms[1], r.end_index, endIndexItemInfo))
        {
            return false;
        }
    }
    return true;
}

#endif  // HAVE_OPENCV_ML
