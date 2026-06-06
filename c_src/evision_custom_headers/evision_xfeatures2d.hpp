#ifdef HAVE_OPENCV_XFEATURES2D

#include "opencv2/xfeatures2d.hpp"
using cv::xfeatures2d::DAISY;
using cv::xfeatures2d::AKAZE;
using cv::xfeatures2d::KAZE;
using cv::xfeatures2d::AgastFeatureDetector;

typedef DAISY::NormalizationType DAISY_NormalizationType;
typedef AKAZE::DescriptorType AKAZE_DescriptorType;
typedef KAZE::DiffusivityType KAZE_DiffusivityType;
typedef AgastFeatureDetector::DetectorType AgastFeatureDetector_DetectorType;

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
