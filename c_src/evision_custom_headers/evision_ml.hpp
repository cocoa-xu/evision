// OpenCV 5.0 removed the legacy C ml API (CvTermCriteria, CvSlice,
// CV_WHOLE_SEQ). The modern cv::ml API uses cv::TermCriteria, whose converter
// is generated, so this custom header no longer needs to provide any.
