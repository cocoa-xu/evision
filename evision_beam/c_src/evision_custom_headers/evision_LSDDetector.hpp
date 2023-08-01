#ifdef HAVE_OPENCV_LINE_DESCRIPTOR
#include "opencv2/line_descriptor.hpp"

template<> struct evisionVecConverter<line_descriptor::KeyLine>
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<line_descriptor::KeyLine>& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<line_descriptor::KeyLine>& value)
    {
        return evision_from_generic_vec(env, value);
    }
};

typedef std::vector<line_descriptor::KeyLine> vector_KeyLine;
typedef std::vector<std::vector<line_descriptor::KeyLine> > vector_vector_KeyLine;

#endif  // HAVE_OPENCV_LINE_DESCRIPTOR
