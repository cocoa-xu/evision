#ifdef HAVE_OPENCV_DNN
#ifdef HAVE_OPENCV_MCC

#include "opencv2/mcc.hpp"

template <>
struct evisionVecConverter<Ptr<cv::mcc::CChecker>>
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<Ptr<cv::mcc::CChecker>> &value,
                   const ArgInfo &info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<Ptr<cv::mcc::CChecker>> &value)
    {
        return evision_from_generic_vec(env, value);
    }
};
typedef std::vector<cv::Ptr<cv::mcc::CChecker>> vector_Ptr_CChecker;
typedef dnn::Net dnn_Net;

#endif  // HAVE_OPENCV_MCC
#endif  // HAVE_OPENCV_DNN
