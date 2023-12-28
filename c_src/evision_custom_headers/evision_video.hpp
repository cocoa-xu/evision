#ifdef HAVE_OPENCV_VIDEO
typedef TrackerMIL::Params TrackerMIL_Params;
typedef TrackerGOTURN::Params TrackerGOTURN_Params;
typedef TrackerDaSiamRPN::Params TrackerDaSiamRPN_Params;

#if (CV_VERSION_MAJOR >= 4 && CV_VERSION_MINOR >= 7)
typedef TrackerNano::Params TrackerNano_Params;
#endif

#if (CV_VERSION_MAJOR >= 4 && CV_VERSION_MINOR >= 9)
typedef TrackerVit::Params TrackerVit_Params;
#endif

#endif
