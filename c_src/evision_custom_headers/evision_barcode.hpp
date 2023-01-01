#ifdef HAVE_OPENCV_BARCODE

#include "opencv2/phase_unwrapping/histogramphaseunwrapping.hpp"

typedef std::vector<cv::barcode::BarcodeType> vector_BarcodeType;

template<> struct evisionVecConverter<cv::barcode::BarcodeType>
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<cv::barcode::BarcodeType>& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }
    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<cv::barcode::BarcodeType>& value)
    {

        return evision_from_generic_vec(env, value);
    }
};

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM o, std::vector<cv::barcode::BarcodeType>& types, const ArgInfo& info)
{
  return evisionVecConverter<cv::barcode::BarcodeType>::to(env, o, types, info);
}
#endif  // HAVE_OPENCV_BARCODE
