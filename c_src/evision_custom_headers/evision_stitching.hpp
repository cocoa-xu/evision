#ifdef HAVE_OPENCV_STITCHING

typedef Stitcher::Status Status;
typedef Stitcher::Mode Mode;

typedef std::vector<detail::ImageFeatures> vector_ImageFeatures;
typedef std::vector<detail::MatchesInfo> vector_MatchesInfo;
typedef std::vector<detail::CameraParams> vector_CameraParams;

template<> struct evisionVecConverter<detail::ImageFeatures>
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<detail::ImageFeatures>& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<detail::ImageFeatures>& value)
    {
        return evision_from_generic_vec(env, value);
    }
};

template<> struct evisionVecConverter<detail::MatchesInfo>
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<detail::MatchesInfo>& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<detail::MatchesInfo>& value)
    {
        return evision_from_generic_vec(env, value);
    }
};

template<> struct evisionVecConverter<detail::CameraParams>
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<detail::CameraParams>& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<detail::CameraParams>& value)
    {
        return evision_from_generic_vec(env, value);
    }
};

#endif
