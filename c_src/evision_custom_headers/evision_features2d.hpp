#ifdef HAVE_OPENCV_FEATURES
typedef SimpleBlobDetector::Params SimpleBlobDetector_Params;
typedef FastFeatureDetector::DetectorType FastFeatureDetector_DetectorType;
typedef DescriptorMatcher::MatcherType DescriptorMatcher_MatcherType;
typedef ORB::ScoreType ORB_ScoreType;
typedef ALIKED::Params ALIKED_Params;
#endif
// AKAZE, KAZE and AgastFeatureDetector moved to contrib cv::xfeatures2d in 5.0;
// their wrappers also need the generated default-value expressions to resolve
// the new namespace, so they are handled with the xfeatures2d codegen work.