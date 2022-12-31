#ifdef HAVE_OPENCV_SURFACE_MATCHING

template<> struct evisionVecConverter<ppf_match_3d::Pose3DPtr >
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<ppf_match_3d::Pose3DPtr >& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<ppf_match_3d::Pose3DPtr >& value)
    {
        return evision_from_generic_vec(env, value);
    }
};

typedef std::vector<ppf_match_3d::Pose3DPtr> vector_Pose3DPtr;

#endif  // HAVE_OPENCV_SURFACE_MATCHING
