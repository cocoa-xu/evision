#ifdef HAVE_OPENCV_RGBD
#include "opencv2/core/saturate.hpp"

template<> struct evisionVecConverter<linemod::Match>
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<linemod::Match>& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<linemod::Match>& value)
    {
        return evision_from_generic_vec(env, value);
    }
};

template<> struct evisionVecConverter<linemod::Template>
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<linemod::Template>& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<linemod::Template>& value)
    {
        return evision_from_generic_vec(env, value);
    }
};

template<> struct evisionVecConverter<linemod::Feature>
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<linemod::Feature>& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<linemod::Feature>& value)
    {
        return evision_from_generic_vec(env, value);
    }
};

template<> struct evisionVecConverter<Ptr<linemod::Modality> >
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<Ptr<linemod::Modality> >& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<Ptr<linemod::Modality> >& value)
    {
        return evision_from_generic_vec(env, value);
    }
};

typedef std::vector<linemod::Match> vector_Match;
typedef std::vector<linemod::Template> vector_Template;
typedef std::vector<linemod::Feature> vector_Feature;
typedef std::vector<Ptr<linemod::Modality> > vector_Ptr_Modality;
#endif
