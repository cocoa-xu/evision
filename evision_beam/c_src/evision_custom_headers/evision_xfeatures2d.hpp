#ifdef HAVE_OPENCV_XFEATURES2D

#include "opencv2/xfeatures2d.hpp"
using cv::xfeatures2d::DAISY;

typedef DAISY::NormalizationType DAISY_NormalizationType;

// Compatibility
// SIFT is moved to the main repository

namespace cv {
namespace xfeatures2d {

/** Use cv.SIFT_create() instead */
CV_WRAP static inline
Ptr<cv::SIFT> SIFT_create(int nfeatures = 0, int nOctaveLayers = 3,
        double contrastThreshold = 0.04, double edgeThreshold = 10,
        double sigma = 1.6)
{
    return SIFT::create(nfeatures, nOctaveLayers, contrastThreshold, edgeThreshold, sigma);
}

}}  // namespace

#endif
