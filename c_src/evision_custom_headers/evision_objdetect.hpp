#ifdef HAVE_OPENCV_OBJDETECT

#include "opencv2/objdetect.hpp"

// `vector_Dictionary` is the alias the generated wrapper uses for
// `std::vector<cv::aruco::Dictionary>`. cv::aruco::Dictionary lives in the
// main `objdetect` module (since 4.11), and ArucoDetector — which consumes
// the vector overloads — is also in main `objdetect`. Upstream OpenCV
// declares this typedef in cv2.cpp under `#ifdef HAVE_OPENCV_OBJDETECT`;
// match that guard here so the typedef is available even when
// `EVISION_ENABLE_CONTRIB=false` (HAVE_OPENCV_ARUCO undefined). Previously
// it lived in evision_aruco.hpp under HAVE_OPENCV_ARUCO and broke
// non-contrib builds.
typedef std::vector<aruco::Dictionary> vector_Dictionary;

// NativeByteArray models OpenCV's NativeByteArray (defined in upstream
// pyopencv_objdetect.hpp) — a thin wrapper around std::string used by
// QRCodeDetector::detectAndDecodeBytes (new in OpenCV 4.13.0) to return
// raw bytes without going through a UTF-8-validated string.
class NativeByteArray
{
public:
    inline NativeByteArray& operator=(const std::string& from) {
        val = from;
        return *this;
    }
    std::string val;
};

class vector_NativeByteArray : public std::vector<std::string> {};

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const NativeByteArray& from)
{
    ErlNifBinary bin;
    if (!enif_alloc_binary(from.val.size(), &bin)) {
        return evision::nif::error(env, "failed to allocate binary for NativeByteArray");
    }
    if (!from.val.empty()) {
        memcpy(bin.data, from.val.data(), from.val.size());
    }
    return enif_make_binary(env, &bin);
}

template<>
ERL_NIF_TERM evision_from(ErlNifEnv *env, const vector_NativeByteArray& results)
{
    const size_t n = results.size();
    std::vector<ERL_NIF_TERM> items;
    items.reserve(n);
    for (size_t i = 0; i < n; ++i) {
        ErlNifBinary bin;
        if (!enif_alloc_binary(results[i].size(), &bin)) {
            return evision::nif::error(env, "failed to allocate binary for vector_NativeByteArray");
        }
        if (!results[i].empty()) {
            memcpy(bin.data, results[i].data(), results[i].size());
        }
        items.push_back(enif_make_binary(env, &bin));
    }
    return enif_make_list_from_array(env, items.data(), static_cast<unsigned>(n));
}

#endif
