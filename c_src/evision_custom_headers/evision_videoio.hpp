#ifdef HAVE_OPENCV_VIDEOIO
typedef std::vector<VideoCaptureAPIs> vector_VideoCaptureAPIs;
typedef std::vector<VideoCapture> vector_VideoCapture;

template<> struct evisionVecConverter<cv::VideoCaptureAPIs>
{
    static bool to(ErlNifEnv *env, ERL_NIF_TERM obj, std::vector<cv::VideoCaptureAPIs>& value, const ArgInfo& info)
    {
        return evision_to_generic_vec(env, obj, value, info);
    }

    static ERL_NIF_TERM from(ErlNifEnv *env, const std::vector<cv::VideoCaptureAPIs>& value)
    {
        return evision_from_generic_vec(env, value);
    }
};

template<>
bool evision_to(ErlNifEnv *env, ERL_NIF_TERM o, std::vector<cv::VideoCaptureAPIs>& apis, const ArgInfo& info)
{
  return evisionVecConverter<cv::VideoCaptureAPIs>::to(env, o, apis, info);
}

#endif // HAVE_OPENCV_VIDEOIO
