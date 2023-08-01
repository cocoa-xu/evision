#ifdef HAVE_OPENCV_STEREO
typedef std::vector<stereo::MatchQuasiDense> vector_MatchQuasiDense;

template<> struct evisionVecConverter<stereo::MatchQuasiDense>
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<stereo::MatchQuasiDense>& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<stereo::MatchQuasiDense>& value)
    {
        return evision_from_generic_vec(env, value);
    }
};

#endif
