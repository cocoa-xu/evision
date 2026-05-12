#ifdef HAVE_OPENCV_ARUCO

#include "opencv2/aruco.hpp"

// OpenCV 4.13.0 introduces ArucoDetector overloads that take/return
// `std::vector<aruco::Dictionary>`. The upstream Python binding generator
// references this via the symbol `vector_Dictionary` (defined in
// `cv2.cpp:49` as `typedef std::vector<aruco::Dictionary> vector_Dictionary;`).
// Mirror that typedef here so evision's generated code can name it.
typedef std::vector<aruco::Dictionary> vector_Dictionary;

#endif
