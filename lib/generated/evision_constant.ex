defmodule Evision.Constant do
  import Bitwise

  def cv_8U, do: 0
  def cv_8S, do: 1
  def cv_16U, do: 2
  def cv_16S, do: 3
  def cv_32S, do: 4
  def cv_32F, do: 5
  def cv_64F, do: 6
  def cv_16F, do: 7

  def cv_cn_shift, do: 3
  def cv_depth_max, do: 1 <<< cv_cn_shift()
  def cv_mat_depth_mask, do: cv_depth_max() - 1
  def cv_maketype(depth, cn), do: (depth &&& cv_mat_depth_mask()) + ((cn - 1) <<< cv_cn_shift())

  def cv_8UC(cn), do: cv_maketype(cv_8U(), cn)
  def cv_8UC1, do: cv_8UC(1)
  def cv_8UC2, do: cv_8UC(2)
  def cv_8UC3, do: cv_8UC(3)
  def cv_8UC4, do: cv_8UC(4)

  def cv_8SC(cn), do: cv_maketype(cv_8S(), cn)
  def cv_8SC1, do: cv_8SC(1)
  def cv_8SC2, do: cv_8SC(2)
  def cv_8SC3, do: cv_8SC(3)
  def cv_8SC4, do: cv_8SC(4)

  def cv_16UC(cn), do: cv_maketype(cv_16U(), cn)
  def cv_16UC1, do: cv_16UC(1)
  def cv_16UC2, do: cv_16UC(2)
  def cv_16UC3, do: cv_16UC(3)
  def cv_16UC4, do: cv_16UC(4)

  def cv_16SC(cn), do: cv_maketype(cv_16S(), cn)
  def cv_16SC1, do: cv_16SC(1)
  def cv_16SC2, do: cv_16SC(2)
  def cv_16SC3, do: cv_16SC(3)
  def cv_16SC4, do: cv_16SC(4)

  def cv_32SC(cn), do: cv_maketype(cv_32S(), cn)
  def cv_32SC1, do: cv_32SC(1)
  def cv_32SC2, do: cv_32SC(2)
  def cv_32SC3, do: cv_32SC(3)
  def cv_32SC4, do: cv_32SC(4)

  def cv_32FC(cn), do: cv_maketype(cv_32F(), cn)
  def cv_32FC1, do: cv_32FC(1)
  def cv_32FC2, do: cv_32FC(2)
  def cv_32FC3, do: cv_32FC(3)
  def cv_32FC4, do: cv_32FC(4)

  def cv_64FC(cn), do: cv_maketype(cv_64F(), cn)
  def cv_64FC1, do: cv_64FC(1)
  def cv_64FC2, do: cv_64FC(2)
  def cv_64FC3, do: cv_64FC(3)
  def cv_64FC4, do: cv_64FC(4)

  def cv_16FC(cn), do: cv_maketype(cv_16F(), cn)
  def cv_16FC1, do: cv_16FC(1)
  def cv_16FC2, do: cv_16FC(2)
  def cv_16FC3, do: cv_16FC(3)
  def cv_16FC4, do: cv_16FC(4)
  def cv_SORT_EVERY_ROW, do: 0
  def cv_SORT_EVERY_COLUMN, do: 1
  def cv_SORT_ASCENDING, do: 0
  def cv_SORT_DESCENDING, do: 16
  def cv_COVAR_SCRAMBLED, do: 0
  def cv_COVAR_NORMAL, do: 1
  def cv_COVAR_USE_AVG, do: 2
  def cv_COVAR_SCALE, do: 4
  def cv_COVAR_ROWS, do: 8
  def cv_COVAR_COLS, do: 16
  def cv_KMEANS_RANDOM_CENTERS, do: 0
  def cv_KMEANS_PP_CENTERS, do: 2
  def cv_KMEANS_USE_INITIAL_LABELS, do: 1
  def cv_REDUCE_SUM, do: 0
  def cv_REDUCE_AVG, do: 1
  def cv_REDUCE_MAX, do: 2
  def cv_REDUCE_MIN, do: 3
  def cv_REDUCE_SUM2, do: 4
  def cv_ROTATE_90_CLOCKWISE, do: 0
  def cv_ROTATE_180, do: 1
  def cv_ROTATE_90_COUNTERCLOCKWISE, do: 2
  def cv_DATA_AS_ROW, do: 0
  def cv_DATA_AS_COL, do: 1
  def cv_USE_AVG, do: 2
  def cv_MODIFY_A, do: 1
  def cv_NO_UV, do: 2
  def cv_FULL_UV, do: 4
  def cv_UNIFORM, do: 0
  def cv_NORMAL, do: 1
  def cv_FMT_DEFAULT, do: 0
  def cv_FMT_MATLAB, do: 1
  def cv_FMT_CSV, do: 2
  def cv_FMT_PYTHON, do: 3
  def cv_FMT_NUMPY, do: 4
  def cv_FMT_C, do: 5
  def cv_INT, do: 0
  def cv_BOOLEAN, do: 1
  def cv_REAL, do: 2
  def cv_STRING, do: 3
  def cv_MAT, do: 4
  def cv_MAT_VECTOR, do: 5
  def cv_ALGORITHM, do: 6
  def cv_FLOAT, do: 7
  def cv_UNSIGNED_INT, do: 8
  def cv_UINT64, do: 9
  def cv_UCHAR, do: 11
  def cv_SCALAR, do: 12
  def cv_StsOk, do: 0
  def cv_StsBackTrace, do: -1
  def cv_StsError, do: -2
  def cv_StsInternal, do: -3
  def cv_StsNoMem, do: -4
  def cv_StsBadArg, do: -5
  def cv_StsBadFunc, do: -6
  def cv_StsNoConv, do: -7
  def cv_StsAutoTrace, do: -8
  def cv_HeaderIsNull, do: -9
  def cv_BadImageSize, do: -10
  def cv_BadOffset, do: -11
  def cv_BadDataPtr, do: -12
  def cv_BadStep, do: -13
  def cv_BadModelOrChSeq, do: -14
  def cv_BadNumChannels, do: -15
  def cv_BadNumChannel1U, do: -16
  def cv_BadDepth, do: -17
  def cv_BadAlphaChannel, do: -18
  def cv_BadOrder, do: -19
  def cv_BadOrigin, do: -20
  def cv_BadAlign, do: -21
  def cv_BadCallBack, do: -22
  def cv_BadTileSize, do: -23
  def cv_BadCOI, do: -24
  def cv_BadROISize, do: -25
  def cv_MaskIsTiled, do: -26
  def cv_StsNullPtr, do: -27
  def cv_StsVecLengthErr, do: -28
  def cv_StsFilterStructContentErr, do: -29
  def cv_StsKernelStructContentErr, do: -30
  def cv_StsFilterOffsetErr, do: -31
  def cv_StsBadSize, do: -201
  def cv_StsDivByZero, do: -202
  def cv_StsInplaceNotSupported, do: -203
  def cv_StsObjectNotFound, do: -204
  def cv_StsUnmatchedFormats, do: -205
  def cv_StsBadFlag, do: -206
  def cv_StsBadPoint, do: -207
  def cv_StsBadMask, do: -208
  def cv_StsUnmatchedSizes, do: -209
  def cv_StsUnsupportedFormat, do: -210
  def cv_StsOutOfRange, do: -211
  def cv_StsParseError, do: -212
  def cv_StsNotImplemented, do: -213
  def cv_StsBadMemBlock, do: -214
  def cv_StsAssert, do: -215
  def cv_GpuNotSupported, do: -216
  def cv_GpuApiCallError, do: -217
  def cv_OpenGlNotSupported, do: -218
  def cv_OpenGlApiCallError, do: -219
  def cv_OpenCLApiCallError, do: -220
  def cv_OpenCLDoubleNotSupported, do: -221
  def cv_OpenCLInitError, do: -222
  def cv_OpenCLNoAMDBlasFft, do: -223
  def cv_DECOMP_LU, do: 0
  def cv_DECOMP_SVD, do: 1
  def cv_DECOMP_EIG, do: 2
  def cv_DECOMP_CHOLESKY, do: 3
  def cv_DECOMP_QR, do: 4
  def cv_DECOMP_NORMAL, do: 16
  def cv_NORM_INF, do: 1
  def cv_NORM_L1, do: 2
  def cv_NORM_L2, do: 4
  def cv_NORM_L2SQR, do: 5
  def cv_NORM_HAMMING, do: 6
  def cv_NORM_HAMMING2, do: 7
  def cv_NORM_TYPE_MASK, do: 7
  def cv_NORM_RELATIVE, do: 8
  def cv_NORM_MINMAX, do: 32
  def cv_CMP_EQ, do: 0
  def cv_CMP_GT, do: 1
  def cv_CMP_GE, do: 2
  def cv_CMP_LT, do: 3
  def cv_CMP_LE, do: 4
  def cv_CMP_NE, do: 5
  def cv_GEMM_1_T, do: 1
  def cv_GEMM_2_T, do: 2
  def cv_GEMM_3_T, do: 4
  def cv_DFT_INVERSE, do: 1
  def cv_DFT_SCALE, do: 2
  def cv_DFT_ROWS, do: 4
  def cv_DFT_COMPLEX_OUTPUT, do: 16
  def cv_DFT_REAL_OUTPUT, do: 32
  def cv_DFT_COMPLEX_INPUT, do: 64
  def cv_DCT_INVERSE, do: cv_DFT_INVERSE()
  def cv_DCT_ROWS, do: cv_DFT_ROWS()
  def cv_BORDER_CONSTANT, do: 0
  def cv_BORDER_REPLICATE, do: 1
  def cv_BORDER_REFLECT, do: 2
  def cv_BORDER_WRAP, do: 3
  def cv_BORDER_REFLECT_101, do: 4
  def cv_BORDER_TRANSPARENT, do: 5
  def cv_BORDER_REFLECT101, do: cv_BORDER_REFLECT_101()
  def cv_BORDER_DEFAULT, do: cv_BORDER_REFLECT_101()
  def cv_BORDER_ISOLATED, do: 16
  def cv_TEST_CUSTOM, do: 0
  def cv_TEST_EQ, do: 1
  def cv_TEST_NE, do: 2
  def cv_TEST_LE, do: 3
  def cv_TEST_LT, do: 4
  def cv_TEST_GE, do: 5
  def cv_TEST_GT, do: 6
  def cv_PAGE_LOCKED, do: 1
  def cv_SHARED, do: 2
  def cv_WRITE_COMBINED, do: 4
  def cv_DEFAULT, do: 0
  def cv_BLOCKING_SYNC, do: 1
  def cv_DISABLE_TIMING, do: 2
  def cv_INTERPROCESS, do: 4
  def cv_FEATURE_SET_COMPUTE_10, do: 10
  def cv_FEATURE_SET_COMPUTE_11, do: 11
  def cv_FEATURE_SET_COMPUTE_12, do: 12
  def cv_FEATURE_SET_COMPUTE_13, do: 13
  def cv_FEATURE_SET_COMPUTE_20, do: 20
  def cv_FEATURE_SET_COMPUTE_21, do: 21
  def cv_FEATURE_SET_COMPUTE_30, do: 30
  def cv_FEATURE_SET_COMPUTE_32, do: 32
  def cv_FEATURE_SET_COMPUTE_35, do: 35
  def cv_FEATURE_SET_COMPUTE_50, do: 50
  def cv_GLOBAL_ATOMICS, do: cv_FEATURE_SET_COMPUTE_11()
  def cv_SHARED_ATOMICS, do: cv_FEATURE_SET_COMPUTE_12()
  def cv_NATIVE_DOUBLE, do: cv_FEATURE_SET_COMPUTE_13()
  def cv_WARP_SHUFFLE_FUNCTIONS, do: cv_FEATURE_SET_COMPUTE_30()
  def cv_DYNAMIC_PARALLELISM, do: cv_FEATURE_SET_COMPUTE_35()
  def cv_ComputeModeDefault, do: 0
  def cv_ComputeModeExclusive, do: 1
  def cv_ComputeModeProhibited, do: 2
  def cv_ComputeModeExclusiveProcess, do: 3
  def cv_ACCESS_READ, do: bsl(1, 24)
  def cv_ACCESS_WRITE, do: bsl(1, 25)
  def cv_ACCESS_RW, do: bsl(3, 24)
  def cv_ACCESS_MASK, do: cv_ACCESS_RW()
  def cv_ACCESS_FAST, do: bsl(1, 26)
  def cv_KIND_SHIFT, do: 16
  def cv_FIXED_TYPE, do: bsl(32768, cv_KIND_SHIFT())
  def cv_FIXED_SIZE, do: bsl(16384, cv_KIND_SHIFT())
  def cv_KIND_MASK, do: bsl(31, cv_KIND_SHIFT())
  def cv_NONE, do: bsl(0, cv_KIND_SHIFT())
  def cv_InputArray_MAT, do: bsl(1, cv_KIND_SHIFT())
  def cv_MATX, do: bsl(2, cv_KIND_SHIFT())
  def cv_STD_VECTOR, do: bsl(3, cv_KIND_SHIFT())
  def cv_STD_VECTOR_VECTOR, do: bsl(4, cv_KIND_SHIFT())
  def cv_STD_VECTOR_MAT, do: bsl(5, cv_KIND_SHIFT())
  def cv_EXPR, do: bsl(6, cv_KIND_SHIFT())
  def cv_OPENGL_BUFFER, do: bsl(7, cv_KIND_SHIFT())
  def cv_CUDA_HOST_MEM, do: bsl(8, cv_KIND_SHIFT())
  def cv_CUDA_GPU_MAT, do: bsl(9, cv_KIND_SHIFT())
  def cv_UMAT, do: bsl(10, cv_KIND_SHIFT())
  def cv_STD_VECTOR_UMAT, do: bsl(11, cv_KIND_SHIFT())
  def cv_STD_BOOL_VECTOR, do: bsl(12, cv_KIND_SHIFT())
  def cv_STD_VECTOR_CUDA_GPU_MAT, do: bsl(13, cv_KIND_SHIFT())
  def cv_STD_ARRAY, do: bsl(14, cv_KIND_SHIFT())
  def cv_STD_ARRAY_MAT, do: bsl(15, cv_KIND_SHIFT())
  def cv_DEPTH_MASK_8U, do: bsl(1, 0)
  def cv_DEPTH_MASK_8S, do: bsl(1, 1)
  def cv_DEPTH_MASK_16U, do: bsl(1, 2)
  def cv_DEPTH_MASK_16S, do: bsl(1, 3)
  def cv_DEPTH_MASK_32S, do: bsl(1, 4)
  def cv_DEPTH_MASK_32F, do: bsl(1, 5)
  def cv_DEPTH_MASK_64F, do: bsl(1, 6)
  def cv_DEPTH_MASK_16F, do: bsl(1, 7)
  def cv_DEPTH_MASK_ALL, do: (bsl(cv_DEPTH_MASK_64F(), 1) - 1)
  def cv_DEPTH_MASK_ALL_BUT_8S, do: band(cv_DEPTH_MASK_ALL(), bnot(cv_DEPTH_MASK_8S()))
  def cv_DEPTH_MASK_ALL_16F, do: (bsl(cv_DEPTH_MASK_16F(), 1) - 1)
  def cv_DEPTH_MASK_FLT, do: (cv_DEPTH_MASK_32F() + cv_DEPTH_MASK_64F())
  def cv_USAGE_DEFAULT, do: 0
  def cv_USAGE_ALLOCATE_HOST_MEMORY, do: bsl(1, 0)
  def cv_USAGE_ALLOCATE_DEVICE_MEMORY, do: bsl(1, 1)
  def cv_USAGE_ALLOCATE_SHARED_MEMORY, do: bsl(1, 2)
  def cv_UMAT_USAGE_FLAGS_32BIT, do: 2147483647
  def cv_COPY_ON_MAP, do: 1
  def cv_HOST_COPY_OBSOLETE, do: 2
  def cv_DEVICE_COPY_OBSOLETE, do: 4
  def cv_TEMP_UMAT, do: 8
  def cv_TEMP_COPIED_UMAT, do: 24
  def cv_USER_ALLOCATED, do: 32
  def cv_DEVICE_MEM_MAPPED, do: 64
  def cv_ASYNC_CLEANUP, do: 128
  def cv_MAGIC_VAL, do: 1124007936
  def cv_AUTO_STEP, do: 0
  def cv_MAGIC_MASK, do: 4294901760
  def cv_TYPE_MASK, do: 4095
  def cv_DEPTH_MASK, do: 7
  def cv_SparseMat_MAGIC_VAL, do: 1123876864
  def cv_MAX_DIM, do: 32
  def cv_HASH_SCALE, do: 1540483477
  def cv_HASH_BIT, do: 2147483648
  def cv_TYPE_DEFAULT, do: bsl(1, 0)
  def cv_TYPE_CPU, do: bsl(1, 1)
  def cv_TYPE_GPU, do: bsl(1, 2)
  def cv_TYPE_ACCELERATOR, do: bsl(1, 3)
  def cv_TYPE_DGPU, do: (cv_TYPE_GPU() + bsl(1, 16))
  def cv_TYPE_IGPU, do: (cv_TYPE_GPU() + bsl(1, 17))
  def cv_TYPE_ALL, do: 4294967295
  def cv_FP_DENORM, do: bsl(1, 0)
  def cv_FP_INF_NAN, do: bsl(1, 1)
  def cv_FP_ROUND_TO_NEAREST, do: bsl(1, 2)
  def cv_FP_ROUND_TO_ZERO, do: bsl(1, 3)
  def cv_FP_ROUND_TO_INF, do: bsl(1, 4)
  def cv_FP_FMA, do: bsl(1, 5)
  def cv_FP_SOFT_FLOAT, do: bsl(1, 6)
  def cv_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT, do: bsl(1, 7)
  def cv_EXEC_KERNEL, do: bsl(1, 0)
  def cv_EXEC_NATIVE_KERNEL, do: bsl(1, 1)
  def cv_NO_CACHE, do: 0
  def cv_READ_ONLY_CACHE, do: 1
  def cv_READ_WRITE_CACHE, do: 2
  def cv_NO_LOCAL_MEM, do: 0
  def cv_LOCAL_IS_LOCAL, do: 1
  def cv_LOCAL_IS_GLOBAL, do: 2
  def cv_UNKNOWN_VENDOR, do: 0
  def cv_VENDOR_AMD, do: 1
  def cv_VENDOR_INTEL, do: 2
  def cv_VENDOR_NVIDIA, do: 3
  def cv_LOCAL, do: 1
  def cv_READ_ONLY, do: 2
  def cv_WRITE_ONLY, do: 4
  def cv_READ_WRITE, do: 6
  def cv_CONSTANT, do: 8
  def cv_PTR_ONLY, do: 16
  def cv_NO_SIZE, do: 256
  def cv_OCL_VECTOR_OWN, do: 0
  def cv_OCL_VECTOR_MAX, do: 1
  def cv_OCL_VECTOR_DEFAULT, do: cv_OCL_VECTOR_OWN()
  def cv_ARRAY_BUFFER, do: 34962
  def cv_ELEMENT_ARRAY_BUFFER, do: 34963
  def cv_PIXEL_PACK_BUFFER, do: 35051
  def cv_PIXEL_UNPACK_BUFFER, do: 35052
  def cv_Buffer_READ_ONLY, do: 35000
  def cv_Buffer_WRITE_ONLY, do: 35001
  def cv_Buffer_READ_WRITE, do: 35002
  def cv_Texture2D_NONE, do: 0
  def cv_DEPTH_COMPONENT, do: 6402
  def cv_RGB, do: 6407
  def cv_RGBA, do: 6408
  def cv_POINTS, do: 0
  def cv_LINES, do: 1
  def cv_LINE_LOOP, do: 2
  def cv_LINE_STRIP, do: 3
  def cv_TRIANGLES, do: 4
  def cv_TRIANGLE_STRIP, do: 5
  def cv_TRIANGLE_FAN, do: 6
  def cv_QUADS, do: 7
  def cv_QUAD_STRIP, do: 8
  def cv_POLYGON, do: 9
  def cv_SOLVELP_LOST, do: -3
  def cv_SOLVELP_UNBOUNDED, do: -2
  def cv_SOLVELP_UNFEASIBLE, do: -1
  def cv_SOLVELP_SINGLE, do: 0
  def cv_SOLVELP_MULTI, do: 1
  def cv_READ, do: 0
  def cv_WRITE, do: 1
  def cv_APPEND, do: 2
  def cv_MEMORY, do: 4
  def cv_FORMAT_MASK, do: bsl(7, 3)
  def cv_FORMAT_AUTO, do: 0
  def cv_FORMAT_XML, do: bsl(1, 3)
  def cv_FORMAT_YAML, do: bsl(2, 3)
  def cv_FORMAT_JSON, do: bsl(3, 3)
  def cv_BASE64, do: 64
  def cv_WRITE_BASE64, do: bor(cv_BASE64(), cv_WRITE())
  def cv_UNDEFINED, do: 0
  def cv_VALUE_EXPECTED, do: 1
  def cv_NAME_EXPECTED, do: 2
  def cv_INSIDE_MAP, do: 4
  def cv_FileNode_NONE, do: 0
  def cv_FileNode_INT, do: 1
  def cv_FileNode_FLOAT, do: cv_REAL()
  def cv_STR, do: 3
  def cv_FileNode_STRING, do: cv_STR()
  def cv_SEQ, do: 4
  def cv_MAP, do: 5
  def cv_FileNode_TYPE_MASK, do: 7
  def cv_FLOW, do: 8
  def cv_FileNode_UNIFORM, do: 8
  def cv_EMPTY, do: 16
  def cv_NAMED, do: 32
  def cv_QUAT_ASSUME_NOT_UNIT, do: 0
  def cv_QUAT_ASSUME_UNIT, do: 1
  def cv_INT_XYZ, do: 0
  def cv_INT_XZY, do: 1
  def cv_INT_YXZ, do: 2
  def cv_INT_YZX, do: 3
  def cv_INT_ZXY, do: 4
  def cv_INT_ZYX, do: 5
  def cv_INT_XYX, do: 6
  def cv_INT_XZX, do: 7
  def cv_INT_YXY, do: 8
  def cv_INT_YZY, do: 9
  def cv_INT_ZXZ, do: 10
  def cv_INT_ZYZ, do: 11
  def cv_EXT_XYZ, do: 12
  def cv_EXT_XZY, do: 13
  def cv_EXT_YXZ, do: 14
  def cv_EXT_YZX, do: 15
  def cv_EXT_ZXY, do: 16
  def cv_EXT_ZYX, do: 17
  def cv_EXT_XYX, do: 18
  def cv_EXT_XZX, do: 19
  def cv_EXT_YXY, do: 20
  def cv_EXT_YZY, do: 21
  def cv_EXT_ZXZ, do: 22
  def cv_EXT_ZYZ, do: 23
  def cv_EULER_ANGLES_MAX_VALUE, do: 24
  def cv_COUNT, do: 1
  def cv_MAX_ITER, do: cv_COUNT()
  def cv_EPS, do: 2
  def cv_FLANN_INDEX_TYPE_8U, do: 0
  def cv_FLANN_INDEX_TYPE_8S, do: 1
  def cv_FLANN_INDEX_TYPE_16U, do: 2
  def cv_FLANN_INDEX_TYPE_16S, do: 3
  def cv_FLANN_INDEX_TYPE_32S, do: 4
  def cv_FLANN_INDEX_TYPE_32F, do: 5
  def cv_FLANN_INDEX_TYPE_64F, do: 6
  def cv_FLANN_INDEX_TYPE_STRING, do: (6 + 1)
  def cv_FLANN_INDEX_TYPE_BOOL, do: (6 + 2)
  def cv_FLANN_INDEX_TYPE_ALGORITHM, do: (6 + 3)
  def cv_LAST_VALUE_FLANN_INDEX_TYPE, do: cv_FLANN_INDEX_TYPE_ALGORITHM()
  def cv_FILTER_SCHARR, do: -1
  def cv_MORPH_ERODE, do: 0
  def cv_MORPH_DILATE, do: 1
  def cv_MORPH_OPEN, do: 2
  def cv_MORPH_CLOSE, do: 3
  def cv_MORPH_GRADIENT, do: 4
  def cv_MORPH_TOPHAT, do: 5
  def cv_MORPH_BLACKHAT, do: 6
  def cv_MORPH_HITMISS, do: 7
  def cv_MORPH_RECT, do: 0
  def cv_MORPH_CROSS, do: 1
  def cv_MORPH_ELLIPSE, do: 2
  def cv_INTER_NEAREST, do: 0
  def cv_INTER_LINEAR, do: 1
  def cv_INTER_CUBIC, do: 2
  def cv_INTER_AREA, do: 3
  def cv_INTER_LANCZOS4, do: 4
  def cv_INTER_LINEAR_EXACT, do: 5
  def cv_INTER_NEAREST_EXACT, do: 6
  def cv_INTER_MAX, do: 7
  def cv_WARP_FILL_OUTLIERS, do: 8
  def cv_WARP_INVERSE_MAP, do: 16
  def cv_WARP_RELATIVE_MAP, do: 32
  def cv_WARP_POLAR_LINEAR, do: 0
  def cv_WARP_POLAR_LOG, do: 256
  def cv_INTER_BITS, do: 5
  def cv_INTER_BITS2, do: (cv_INTER_BITS() * 2)
  def cv_INTER_TAB_SIZE, do: bsl(1, cv_INTER_BITS())
  def cv_INTER_TAB_SIZE2, do: (cv_INTER_TAB_SIZE() * cv_INTER_TAB_SIZE())
  def cv_DIST_USER, do: -1
  def cv_DIST_L1, do: 1
  def cv_DIST_L2, do: 2
  def cv_DIST_C, do: 3
  def cv_DIST_L12, do: 4
  def cv_DIST_FAIR, do: 5
  def cv_DIST_WELSCH, do: 6
  def cv_DIST_HUBER, do: 7
  def cv_DIST_MASK_3, do: 3
  def cv_DIST_MASK_5, do: 5
  def cv_DIST_MASK_PRECISE, do: 0
  def cv_THRESH_BINARY, do: 0
  def cv_THRESH_BINARY_INV, do: 1
  def cv_THRESH_TRUNC, do: 2
  def cv_THRESH_TOZERO, do: 3
  def cv_THRESH_TOZERO_INV, do: 4
  def cv_THRESH_MASK, do: 7
  def cv_THRESH_OTSU, do: 8
  def cv_THRESH_TRIANGLE, do: 16
  def cv_ADAPTIVE_THRESH_MEAN_C, do: 0
  def cv_ADAPTIVE_THRESH_GAUSSIAN_C, do: 1
  def cv_GC_BGD, do: 0
  def cv_GC_FGD, do: 1
  def cv_GC_PR_BGD, do: 2
  def cv_GC_PR_FGD, do: 3
  def cv_GC_INIT_WITH_RECT, do: 0
  def cv_GC_INIT_WITH_MASK, do: 1
  def cv_GC_EVAL, do: 2
  def cv_GC_EVAL_FREEZE_MODEL, do: 3
  def cv_DIST_LABEL_CCOMP, do: 0
  def cv_DIST_LABEL_PIXEL, do: 1
  def cv_FLOODFILL_FIXED_RANGE, do: bsl(1, 16)
  def cv_FLOODFILL_MASK_ONLY, do: bsl(1, 17)
  def cv_CC_STAT_LEFT, do: 0
  def cv_CC_STAT_TOP, do: 1
  def cv_CC_STAT_WIDTH, do: 2
  def cv_CC_STAT_HEIGHT, do: 3
  def cv_CC_STAT_AREA, do: 4
  def cv_CC_STAT_MAX, do: 5
  def cv_CCL_DEFAULT, do: -1
  def cv_CCL_WU, do: 0
  def cv_CCL_GRANA, do: 1
  def cv_CCL_BOLELLI, do: 2
  def cv_CCL_SAUF, do: 3
  def cv_CCL_BBDT, do: 4
  def cv_CCL_SPAGHETTI, do: 5
  def cv_RETR_EXTERNAL, do: 0
  def cv_RETR_LIST, do: 1
  def cv_RETR_CCOMP, do: 2
  def cv_RETR_TREE, do: 3
  def cv_RETR_FLOODFILL, do: 4
  def cv_CHAIN_APPROX_NONE, do: 1
  def cv_CHAIN_APPROX_SIMPLE, do: 2
  def cv_CHAIN_APPROX_TC89_L1, do: 3
  def cv_CHAIN_APPROX_TC89_KCOS, do: 4
  def cv_CONTOURS_MATCH_I1, do: 1
  def cv_CONTOURS_MATCH_I2, do: 2
  def cv_CONTOURS_MATCH_I3, do: 3
  def cv_HOUGH_STANDARD, do: 0
  def cv_HOUGH_PROBABILISTIC, do: 1
  def cv_HOUGH_MULTI_SCALE, do: 2
  def cv_HOUGH_GRADIENT, do: 3
  def cv_HOUGH_GRADIENT_ALT, do: 4
  def cv_LSD_REFINE_NONE, do: 0
  def cv_LSD_REFINE_STD, do: 1
  def cv_LSD_REFINE_ADV, do: 2
  def cv_HISTCMP_CORREL, do: 0
  def cv_HISTCMP_CHISQR, do: 1
  def cv_HISTCMP_INTERSECT, do: 2
  def cv_HISTCMP_BHATTACHARYYA, do: 3
  def cv_HISTCMP_HELLINGER, do: cv_HISTCMP_BHATTACHARYYA()
  def cv_HISTCMP_CHISQR_ALT, do: 4
  def cv_HISTCMP_KL_DIV, do: 5
  def cv_COLOR_BGR2BGRA, do: 0
  def cv_COLOR_RGB2RGBA, do: cv_COLOR_BGR2BGRA()
  def cv_COLOR_BGRA2BGR, do: 1
  def cv_COLOR_RGBA2RGB, do: cv_COLOR_BGRA2BGR()
  def cv_COLOR_BGR2RGBA, do: 2
  def cv_COLOR_RGB2BGRA, do: cv_COLOR_BGR2RGBA()
  def cv_COLOR_RGBA2BGR, do: 3
  def cv_COLOR_BGRA2RGB, do: cv_COLOR_RGBA2BGR()
  def cv_COLOR_BGR2RGB, do: 4
  def cv_COLOR_RGB2BGR, do: cv_COLOR_BGR2RGB()
  def cv_COLOR_BGRA2RGBA, do: 5
  def cv_COLOR_RGBA2BGRA, do: cv_COLOR_BGRA2RGBA()
  def cv_COLOR_BGR2GRAY, do: 6
  def cv_COLOR_RGB2GRAY, do: 7
  def cv_COLOR_GRAY2BGR, do: 8
  def cv_COLOR_GRAY2RGB, do: cv_COLOR_GRAY2BGR()
  def cv_COLOR_GRAY2BGRA, do: 9
  def cv_COLOR_GRAY2RGBA, do: cv_COLOR_GRAY2BGRA()
  def cv_COLOR_BGRA2GRAY, do: 10
  def cv_COLOR_RGBA2GRAY, do: 11
  def cv_COLOR_BGR2BGR565, do: 12
  def cv_COLOR_RGB2BGR565, do: 13
  def cv_COLOR_BGR5652BGR, do: 14
  def cv_COLOR_BGR5652RGB, do: 15
  def cv_COLOR_BGRA2BGR565, do: 16
  def cv_COLOR_RGBA2BGR565, do: 17
  def cv_COLOR_BGR5652BGRA, do: 18
  def cv_COLOR_BGR5652RGBA, do: 19
  def cv_COLOR_GRAY2BGR565, do: 20
  def cv_COLOR_BGR5652GRAY, do: 21
  def cv_COLOR_BGR2BGR555, do: 22
  def cv_COLOR_RGB2BGR555, do: 23
  def cv_COLOR_BGR5552BGR, do: 24
  def cv_COLOR_BGR5552RGB, do: 25
  def cv_COLOR_BGRA2BGR555, do: 26
  def cv_COLOR_RGBA2BGR555, do: 27
  def cv_COLOR_BGR5552BGRA, do: 28
  def cv_COLOR_BGR5552RGBA, do: 29
  def cv_COLOR_GRAY2BGR555, do: 30
  def cv_COLOR_BGR5552GRAY, do: 31
  def cv_COLOR_BGR2XYZ, do: 32
  def cv_COLOR_RGB2XYZ, do: 33
  def cv_COLOR_XYZ2BGR, do: 34
  def cv_COLOR_XYZ2RGB, do: 35
  def cv_COLOR_BGR2YCrCb, do: 36
  def cv_COLOR_RGB2YCrCb, do: 37
  def cv_COLOR_YCrCb2BGR, do: 38
  def cv_COLOR_YCrCb2RGB, do: 39
  def cv_COLOR_BGR2HSV, do: 40
  def cv_COLOR_RGB2HSV, do: 41
  def cv_COLOR_BGR2Lab, do: 44
  def cv_COLOR_RGB2Lab, do: 45
  def cv_COLOR_BGR2Luv, do: 50
  def cv_COLOR_RGB2Luv, do: 51
  def cv_COLOR_BGR2HLS, do: 52
  def cv_COLOR_RGB2HLS, do: 53
  def cv_COLOR_HSV2BGR, do: 54
  def cv_COLOR_HSV2RGB, do: 55
  def cv_COLOR_Lab2BGR, do: 56
  def cv_COLOR_Lab2RGB, do: 57
  def cv_COLOR_Luv2BGR, do: 58
  def cv_COLOR_Luv2RGB, do: 59
  def cv_COLOR_HLS2BGR, do: 60
  def cv_COLOR_HLS2RGB, do: 61
  def cv_COLOR_BGR2HSV_FULL, do: 66
  def cv_COLOR_RGB2HSV_FULL, do: 67
  def cv_COLOR_BGR2HLS_FULL, do: 68
  def cv_COLOR_RGB2HLS_FULL, do: 69
  def cv_COLOR_HSV2BGR_FULL, do: 70
  def cv_COLOR_HSV2RGB_FULL, do: 71
  def cv_COLOR_HLS2BGR_FULL, do: 72
  def cv_COLOR_HLS2RGB_FULL, do: 73
  def cv_COLOR_LBGR2Lab, do: 74
  def cv_COLOR_LRGB2Lab, do: 75
  def cv_COLOR_LBGR2Luv, do: 76
  def cv_COLOR_LRGB2Luv, do: 77
  def cv_COLOR_Lab2LBGR, do: 78
  def cv_COLOR_Lab2LRGB, do: 79
  def cv_COLOR_Luv2LBGR, do: 80
  def cv_COLOR_Luv2LRGB, do: 81
  def cv_COLOR_BGR2YUV, do: 82
  def cv_COLOR_RGB2YUV, do: 83
  def cv_COLOR_YUV2BGR, do: 84
  def cv_COLOR_YUV2RGB, do: 85
  def cv_COLOR_YUV2RGB_NV12, do: 90
  def cv_COLOR_YUV2BGR_NV12, do: 91
  def cv_COLOR_YUV2RGB_NV21, do: 92
  def cv_COLOR_YUV2BGR_NV21, do: 93
  def cv_COLOR_YUV420sp2RGB, do: cv_COLOR_YUV2RGB_NV21()
  def cv_COLOR_YUV420sp2BGR, do: cv_COLOR_YUV2BGR_NV21()
  def cv_COLOR_YUV2RGBA_NV12, do: 94
  def cv_COLOR_YUV2BGRA_NV12, do: 95
  def cv_COLOR_YUV2RGBA_NV21, do: 96
  def cv_COLOR_YUV2BGRA_NV21, do: 97
  def cv_COLOR_YUV420sp2RGBA, do: cv_COLOR_YUV2RGBA_NV21()
  def cv_COLOR_YUV420sp2BGRA, do: cv_COLOR_YUV2BGRA_NV21()
  def cv_COLOR_YUV2RGB_YV12, do: 98
  def cv_COLOR_YUV2BGR_YV12, do: 99
  def cv_COLOR_YUV2RGB_IYUV, do: 100
  def cv_COLOR_YUV2BGR_IYUV, do: 101
  def cv_COLOR_YUV2RGB_I420, do: cv_COLOR_YUV2RGB_IYUV()
  def cv_COLOR_YUV2BGR_I420, do: cv_COLOR_YUV2BGR_IYUV()
  def cv_COLOR_YUV420p2RGB, do: cv_COLOR_YUV2RGB_YV12()
  def cv_COLOR_YUV420p2BGR, do: cv_COLOR_YUV2BGR_YV12()
  def cv_COLOR_YUV2RGBA_YV12, do: 102
  def cv_COLOR_YUV2BGRA_YV12, do: 103
  def cv_COLOR_YUV2RGBA_IYUV, do: 104
  def cv_COLOR_YUV2BGRA_IYUV, do: 105
  def cv_COLOR_YUV2RGBA_I420, do: cv_COLOR_YUV2RGBA_IYUV()
  def cv_COLOR_YUV2BGRA_I420, do: cv_COLOR_YUV2BGRA_IYUV()
  def cv_COLOR_YUV420p2RGBA, do: cv_COLOR_YUV2RGBA_YV12()
  def cv_COLOR_YUV420p2BGRA, do: cv_COLOR_YUV2BGRA_YV12()
  def cv_COLOR_YUV2GRAY_420, do: 106
  def cv_COLOR_YUV2GRAY_NV21, do: cv_COLOR_YUV2GRAY_420()
  def cv_COLOR_YUV2GRAY_NV12, do: cv_COLOR_YUV2GRAY_420()
  def cv_COLOR_YUV2GRAY_YV12, do: cv_COLOR_YUV2GRAY_420()
  def cv_COLOR_YUV2GRAY_IYUV, do: cv_COLOR_YUV2GRAY_420()
  def cv_COLOR_YUV2GRAY_I420, do: cv_COLOR_YUV2GRAY_420()
  def cv_COLOR_YUV420sp2GRAY, do: cv_COLOR_YUV2GRAY_420()
  def cv_COLOR_YUV420p2GRAY, do: cv_COLOR_YUV2GRAY_420()
  def cv_COLOR_YUV2RGB_UYVY, do: 107
  def cv_COLOR_YUV2BGR_UYVY, do: 108
  def cv_COLOR_YUV2RGB_Y422, do: cv_COLOR_YUV2RGB_UYVY()
  def cv_COLOR_YUV2BGR_Y422, do: cv_COLOR_YUV2BGR_UYVY()
  def cv_COLOR_YUV2RGB_UYNV, do: cv_COLOR_YUV2RGB_UYVY()
  def cv_COLOR_YUV2BGR_UYNV, do: cv_COLOR_YUV2BGR_UYVY()
  def cv_COLOR_YUV2RGBA_UYVY, do: 111
  def cv_COLOR_YUV2BGRA_UYVY, do: 112
  def cv_COLOR_YUV2RGBA_Y422, do: cv_COLOR_YUV2RGBA_UYVY()
  def cv_COLOR_YUV2BGRA_Y422, do: cv_COLOR_YUV2BGRA_UYVY()
  def cv_COLOR_YUV2RGBA_UYNV, do: cv_COLOR_YUV2RGBA_UYVY()
  def cv_COLOR_YUV2BGRA_UYNV, do: cv_COLOR_YUV2BGRA_UYVY()
  def cv_COLOR_YUV2RGB_YUY2, do: 115
  def cv_COLOR_YUV2BGR_YUY2, do: 116
  def cv_COLOR_YUV2RGB_YVYU, do: 117
  def cv_COLOR_YUV2BGR_YVYU, do: 118
  def cv_COLOR_YUV2RGB_YUYV, do: cv_COLOR_YUV2RGB_YUY2()
  def cv_COLOR_YUV2BGR_YUYV, do: cv_COLOR_YUV2BGR_YUY2()
  def cv_COLOR_YUV2RGB_YUNV, do: cv_COLOR_YUV2RGB_YUY2()
  def cv_COLOR_YUV2BGR_YUNV, do: cv_COLOR_YUV2BGR_YUY2()
  def cv_COLOR_YUV2RGBA_YUY2, do: 119
  def cv_COLOR_YUV2BGRA_YUY2, do: 120
  def cv_COLOR_YUV2RGBA_YVYU, do: 121
  def cv_COLOR_YUV2BGRA_YVYU, do: 122
  def cv_COLOR_YUV2RGBA_YUYV, do: cv_COLOR_YUV2RGBA_YUY2()
  def cv_COLOR_YUV2BGRA_YUYV, do: cv_COLOR_YUV2BGRA_YUY2()
  def cv_COLOR_YUV2RGBA_YUNV, do: cv_COLOR_YUV2RGBA_YUY2()
  def cv_COLOR_YUV2BGRA_YUNV, do: cv_COLOR_YUV2BGRA_YUY2()
  def cv_COLOR_YUV2GRAY_UYVY, do: 123
  def cv_COLOR_YUV2GRAY_YUY2, do: 124
  def cv_COLOR_YUV2GRAY_Y422, do: cv_COLOR_YUV2GRAY_UYVY()
  def cv_COLOR_YUV2GRAY_UYNV, do: cv_COLOR_YUV2GRAY_UYVY()
  def cv_COLOR_YUV2GRAY_YVYU, do: cv_COLOR_YUV2GRAY_YUY2()
  def cv_COLOR_YUV2GRAY_YUYV, do: cv_COLOR_YUV2GRAY_YUY2()
  def cv_COLOR_YUV2GRAY_YUNV, do: cv_COLOR_YUV2GRAY_YUY2()
  def cv_COLOR_RGBA2mRGBA, do: 125
  def cv_COLOR_mRGBA2RGBA, do: 126
  def cv_COLOR_RGB2YUV_I420, do: 127
  def cv_COLOR_BGR2YUV_I420, do: 128
  def cv_COLOR_RGB2YUV_IYUV, do: cv_COLOR_RGB2YUV_I420()
  def cv_COLOR_BGR2YUV_IYUV, do: cv_COLOR_BGR2YUV_I420()
  def cv_COLOR_RGBA2YUV_I420, do: 129
  def cv_COLOR_BGRA2YUV_I420, do: 130
  def cv_COLOR_RGBA2YUV_IYUV, do: cv_COLOR_RGBA2YUV_I420()
  def cv_COLOR_BGRA2YUV_IYUV, do: cv_COLOR_BGRA2YUV_I420()
  def cv_COLOR_RGB2YUV_YV12, do: 131
  def cv_COLOR_BGR2YUV_YV12, do: 132
  def cv_COLOR_RGBA2YUV_YV12, do: 133
  def cv_COLOR_BGRA2YUV_YV12, do: 134
  def cv_COLOR_BayerBG2BGR, do: 46
  def cv_COLOR_BayerGB2BGR, do: 47
  def cv_COLOR_BayerRG2BGR, do: 48
  def cv_COLOR_BayerGR2BGR, do: 49
  def cv_COLOR_BayerRGGB2BGR, do: cv_COLOR_BayerBG2BGR()
  def cv_COLOR_BayerGRBG2BGR, do: cv_COLOR_BayerGB2BGR()
  def cv_COLOR_BayerBGGR2BGR, do: cv_COLOR_BayerRG2BGR()
  def cv_COLOR_BayerGBRG2BGR, do: cv_COLOR_BayerGR2BGR()
  def cv_COLOR_BayerRGGB2RGB, do: cv_COLOR_BayerBGGR2BGR()
  def cv_COLOR_BayerGRBG2RGB, do: cv_COLOR_BayerGBRG2BGR()
  def cv_COLOR_BayerBGGR2RGB, do: cv_COLOR_BayerRGGB2BGR()
  def cv_COLOR_BayerGBRG2RGB, do: cv_COLOR_BayerGRBG2BGR()
  def cv_COLOR_BayerBG2RGB, do: cv_COLOR_BayerRG2BGR()
  def cv_COLOR_BayerGB2RGB, do: cv_COLOR_BayerGR2BGR()
  def cv_COLOR_BayerRG2RGB, do: cv_COLOR_BayerBG2BGR()
  def cv_COLOR_BayerGR2RGB, do: cv_COLOR_BayerGB2BGR()
  def cv_COLOR_BayerBG2GRAY, do: 86
  def cv_COLOR_BayerGB2GRAY, do: 87
  def cv_COLOR_BayerRG2GRAY, do: 88
  def cv_COLOR_BayerGR2GRAY, do: 89
  def cv_COLOR_BayerRGGB2GRAY, do: cv_COLOR_BayerBG2GRAY()
  def cv_COLOR_BayerGRBG2GRAY, do: cv_COLOR_BayerGB2GRAY()
  def cv_COLOR_BayerBGGR2GRAY, do: cv_COLOR_BayerRG2GRAY()
  def cv_COLOR_BayerGBRG2GRAY, do: cv_COLOR_BayerGR2GRAY()
  def cv_COLOR_BayerBG2BGR_VNG, do: 62
  def cv_COLOR_BayerGB2BGR_VNG, do: 63
  def cv_COLOR_BayerRG2BGR_VNG, do: 64
  def cv_COLOR_BayerGR2BGR_VNG, do: 65
  def cv_COLOR_BayerRGGB2BGR_VNG, do: cv_COLOR_BayerBG2BGR_VNG()
  def cv_COLOR_BayerGRBG2BGR_VNG, do: cv_COLOR_BayerGB2BGR_VNG()
  def cv_COLOR_BayerBGGR2BGR_VNG, do: cv_COLOR_BayerRG2BGR_VNG()
  def cv_COLOR_BayerGBRG2BGR_VNG, do: cv_COLOR_BayerGR2BGR_VNG()
  def cv_COLOR_BayerRGGB2RGB_VNG, do: cv_COLOR_BayerBGGR2BGR_VNG()
  def cv_COLOR_BayerGRBG2RGB_VNG, do: cv_COLOR_BayerGBRG2BGR_VNG()
  def cv_COLOR_BayerBGGR2RGB_VNG, do: cv_COLOR_BayerRGGB2BGR_VNG()
  def cv_COLOR_BayerGBRG2RGB_VNG, do: cv_COLOR_BayerGRBG2BGR_VNG()
  def cv_COLOR_BayerBG2RGB_VNG, do: cv_COLOR_BayerRG2BGR_VNG()
  def cv_COLOR_BayerGB2RGB_VNG, do: cv_COLOR_BayerGR2BGR_VNG()
  def cv_COLOR_BayerRG2RGB_VNG, do: cv_COLOR_BayerBG2BGR_VNG()
  def cv_COLOR_BayerGR2RGB_VNG, do: cv_COLOR_BayerGB2BGR_VNG()
  def cv_COLOR_BayerBG2BGR_EA, do: 135
  def cv_COLOR_BayerGB2BGR_EA, do: 136
  def cv_COLOR_BayerRG2BGR_EA, do: 137
  def cv_COLOR_BayerGR2BGR_EA, do: 138
  def cv_COLOR_BayerRGGB2BGR_EA, do: cv_COLOR_BayerBG2BGR_EA()
  def cv_COLOR_BayerGRBG2BGR_EA, do: cv_COLOR_BayerGB2BGR_EA()
  def cv_COLOR_BayerBGGR2BGR_EA, do: cv_COLOR_BayerRG2BGR_EA()
  def cv_COLOR_BayerGBRG2BGR_EA, do: cv_COLOR_BayerGR2BGR_EA()
  def cv_COLOR_BayerRGGB2RGB_EA, do: cv_COLOR_BayerBGGR2BGR_EA()
  def cv_COLOR_BayerGRBG2RGB_EA, do: cv_COLOR_BayerGBRG2BGR_EA()
  def cv_COLOR_BayerBGGR2RGB_EA, do: cv_COLOR_BayerRGGB2BGR_EA()
  def cv_COLOR_BayerGBRG2RGB_EA, do: cv_COLOR_BayerGRBG2BGR_EA()
  def cv_COLOR_BayerBG2RGB_EA, do: cv_COLOR_BayerRG2BGR_EA()
  def cv_COLOR_BayerGB2RGB_EA, do: cv_COLOR_BayerGR2BGR_EA()
  def cv_COLOR_BayerRG2RGB_EA, do: cv_COLOR_BayerBG2BGR_EA()
  def cv_COLOR_BayerGR2RGB_EA, do: cv_COLOR_BayerGB2BGR_EA()
  def cv_COLOR_BayerBG2BGRA, do: 139
  def cv_COLOR_BayerGB2BGRA, do: 140
  def cv_COLOR_BayerRG2BGRA, do: 141
  def cv_COLOR_BayerGR2BGRA, do: 142
  def cv_COLOR_BayerRGGB2BGRA, do: cv_COLOR_BayerBG2BGRA()
  def cv_COLOR_BayerGRBG2BGRA, do: cv_COLOR_BayerGB2BGRA()
  def cv_COLOR_BayerBGGR2BGRA, do: cv_COLOR_BayerRG2BGRA()
  def cv_COLOR_BayerGBRG2BGRA, do: cv_COLOR_BayerGR2BGRA()
  def cv_COLOR_BayerRGGB2RGBA, do: cv_COLOR_BayerBGGR2BGRA()
  def cv_COLOR_BayerGRBG2RGBA, do: cv_COLOR_BayerGBRG2BGRA()
  def cv_COLOR_BayerBGGR2RGBA, do: cv_COLOR_BayerRGGB2BGRA()
  def cv_COLOR_BayerGBRG2RGBA, do: cv_COLOR_BayerGRBG2BGRA()
  def cv_COLOR_BayerBG2RGBA, do: cv_COLOR_BayerRG2BGRA()
  def cv_COLOR_BayerGB2RGBA, do: cv_COLOR_BayerGR2BGRA()
  def cv_COLOR_BayerRG2RGBA, do: cv_COLOR_BayerBG2BGRA()
  def cv_COLOR_BayerGR2RGBA, do: cv_COLOR_BayerGB2BGRA()
  def cv_COLOR_RGB2YUV_UYVY, do: 143
  def cv_COLOR_BGR2YUV_UYVY, do: 144
  def cv_COLOR_RGB2YUV_Y422, do: cv_COLOR_RGB2YUV_UYVY()
  def cv_COLOR_BGR2YUV_Y422, do: cv_COLOR_BGR2YUV_UYVY()
  def cv_COLOR_RGB2YUV_UYNV, do: cv_COLOR_RGB2YUV_UYVY()
  def cv_COLOR_BGR2YUV_UYNV, do: cv_COLOR_BGR2YUV_UYVY()
  def cv_COLOR_RGBA2YUV_UYVY, do: 145
  def cv_COLOR_BGRA2YUV_UYVY, do: 146
  def cv_COLOR_RGBA2YUV_Y422, do: cv_COLOR_RGBA2YUV_UYVY()
  def cv_COLOR_BGRA2YUV_Y422, do: cv_COLOR_BGRA2YUV_UYVY()
  def cv_COLOR_RGBA2YUV_UYNV, do: cv_COLOR_RGBA2YUV_UYVY()
  def cv_COLOR_BGRA2YUV_UYNV, do: cv_COLOR_BGRA2YUV_UYVY()
  def cv_COLOR_RGB2YUV_YUY2, do: 147
  def cv_COLOR_BGR2YUV_YUY2, do: 148
  def cv_COLOR_RGB2YUV_YVYU, do: 149
  def cv_COLOR_BGR2YUV_YVYU, do: 150
  def cv_COLOR_RGB2YUV_YUYV, do: cv_COLOR_RGB2YUV_YUY2()
  def cv_COLOR_BGR2YUV_YUYV, do: cv_COLOR_BGR2YUV_YUY2()
  def cv_COLOR_RGB2YUV_YUNV, do: cv_COLOR_RGB2YUV_YUY2()
  def cv_COLOR_BGR2YUV_YUNV, do: cv_COLOR_BGR2YUV_YUY2()
  def cv_COLOR_RGBA2YUV_YUY2, do: 151
  def cv_COLOR_BGRA2YUV_YUY2, do: 152
  def cv_COLOR_RGBA2YUV_YVYU, do: 153
  def cv_COLOR_BGRA2YUV_YVYU, do: 154
  def cv_COLOR_RGBA2YUV_YUYV, do: cv_COLOR_RGBA2YUV_YUY2()
  def cv_COLOR_BGRA2YUV_YUYV, do: cv_COLOR_BGRA2YUV_YUY2()
  def cv_COLOR_RGBA2YUV_YUNV, do: cv_COLOR_RGBA2YUV_YUY2()
  def cv_COLOR_BGRA2YUV_YUNV, do: cv_COLOR_BGRA2YUV_YUY2()
  def cv_COLOR_COLORCVT_MAX, do: 155
  def cv_INTERSECT_NONE, do: 0
  def cv_INTERSECT_PARTIAL, do: 1
  def cv_INTERSECT_FULL, do: 2
  def cv_FILLED, do: -1
  def cv_LINE_4, do: 4
  def cv_LINE_8, do: 8
  def cv_LINE_AA, do: 16
  def cv_FONT_HERSHEY_SIMPLEX, do: 0
  def cv_FONT_HERSHEY_PLAIN, do: 1
  def cv_FONT_HERSHEY_DUPLEX, do: 2
  def cv_FONT_HERSHEY_COMPLEX, do: 3
  def cv_FONT_HERSHEY_TRIPLEX, do: 4
  def cv_FONT_HERSHEY_COMPLEX_SMALL, do: 5
  def cv_FONT_HERSHEY_SCRIPT_SIMPLEX, do: 6
  def cv_FONT_HERSHEY_SCRIPT_COMPLEX, do: 7
  def cv_FONT_ITALIC, do: 16
  def cv_MARKER_CROSS, do: 0
  def cv_MARKER_TILTED_CROSS, do: 1
  def cv_MARKER_STAR, do: 2
  def cv_MARKER_DIAMOND, do: 3
  def cv_MARKER_SQUARE, do: 4
  def cv_MARKER_TRIANGLE_UP, do: 5
  def cv_MARKER_TRIANGLE_DOWN, do: 6
  def cv_PTLOC_ERROR, do: -2
  def cv_PTLOC_OUTSIDE_RECT, do: -1
  def cv_PTLOC_INSIDE, do: 0
  def cv_PTLOC_VERTEX, do: 1
  def cv_PTLOC_ON_EDGE, do: 2
  def cv_NEXT_AROUND_ORG, do: 0
  def cv_NEXT_AROUND_DST, do: 34
  def cv_PREV_AROUND_ORG, do: 17
  def cv_PREV_AROUND_DST, do: 51
  def cv_NEXT_AROUND_LEFT, do: 19
  def cv_NEXT_AROUND_RIGHT, do: 49
  def cv_PREV_AROUND_LEFT, do: 32
  def cv_PREV_AROUND_RIGHT, do: 2
  def cv_TM_SQDIFF, do: 0
  def cv_TM_SQDIFF_NORMED, do: 1
  def cv_TM_CCORR, do: 2
  def cv_TM_CCORR_NORMED, do: 3
  def cv_TM_CCOEFF, do: 4
  def cv_TM_CCOEFF_NORMED, do: 5
  def cv_COLORMAP_AUTUMN, do: 0
  def cv_COLORMAP_BONE, do: 1
  def cv_COLORMAP_JET, do: 2
  def cv_COLORMAP_WINTER, do: 3
  def cv_COLORMAP_RAINBOW, do: 4
  def cv_COLORMAP_OCEAN, do: 5
  def cv_COLORMAP_SUMMER, do: 6
  def cv_COLORMAP_SPRING, do: 7
  def cv_COLORMAP_COOL, do: 8
  def cv_COLORMAP_HSV, do: 9
  def cv_COLORMAP_PINK, do: 10
  def cv_COLORMAP_HOT, do: 11
  def cv_COLORMAP_PARULA, do: 12
  def cv_COLORMAP_MAGMA, do: 13
  def cv_COLORMAP_INFERNO, do: 14
  def cv_COLORMAP_PLASMA, do: 15
  def cv_COLORMAP_VIRIDIS, do: 16
  def cv_COLORMAP_CIVIDIS, do: 17
  def cv_COLORMAP_TWILIGHT, do: 18
  def cv_COLORMAP_TWILIGHT_SHIFTED, do: 19
  def cv_COLORMAP_TURBO, do: 20
  def cv_COLORMAP_DEEPGREEN, do: 21
  def cv_VAR_NUMERICAL, do: 0
  def cv_VAR_ORDERED, do: 0
  def cv_VAR_CATEGORICAL, do: 1
  def cv_TEST_ERROR, do: 0
  def cv_TRAIN_ERROR, do: 1
  def cv_ROW_SAMPLE, do: 0
  def cv_COL_SAMPLE, do: 1
  def cv_UPDATE_MODEL, do: 1
  def cv_RAW_OUTPUT, do: 1
  def cv_COMPRESSED_INPUT, do: 2
  def cv_PREPROCESSED_INPUT, do: 4
  def cv_BRUTE_FORCE, do: 1
  def cv_KDTREE, do: 2
  def cv_C_SVC, do: 100
  def cv_NU_SVC, do: 101
  def cv_ONE_CLASS, do: 102
  def cv_EPS_SVR, do: 103
  def cv_NU_SVR, do: 104
  def cv_CUSTOM, do: -1
  def cv_LINEAR, do: 0
  def cv_POLY, do: 1
  def cv_RBF, do: 2
  def cv_SIGMOID, do: 3
  def cv_CHI2, do: 4
  def cv_INTER, do: 5
  def cv_C, do: 0
  def cv_GAMMA, do: 1
  def cv_P, do: 2
  def cv_NU, do: 3
  def cv_COEF, do: 4
  def cv_DEGREE, do: 5
  def cv_COV_MAT_SPHERICAL, do: 0
  def cv_COV_MAT_DIAGONAL, do: 1
  def cv_COV_MAT_GENERIC, do: 2
  def cv_COV_MAT_DEFAULT, do: cv_COV_MAT_DIAGONAL()
  def cv_DEFAULT_NCLUSTERS, do: 5
  def cv_DEFAULT_MAX_ITERS, do: 100
  def cv_START_E_STEP, do: 1
  def cv_START_M_STEP, do: 2
  def cv_START_AUTO_STEP, do: 0
  def cv_PREDICT_AUTO, do: 0
  def cv_PREDICT_SUM, do: bsl(1, 8)
  def cv_PREDICT_MAX_VOTE, do: bsl(2, 8)
  def cv_PREDICT_MASK, do: bsl(3, 8)
  def cv_DISCRETE, do: 0
  def cv_Boost_REAL, do: 1
  def cv_LOGIT, do: 2
  def cv_GENTLE, do: 3
  def cv_BACKPROP, do: 0
  def cv_RPROP, do: 1
  def cv_ANNEAL, do: 2
  def cv_IDENTITY, do: 0
  def cv_SIGMOID_SYM, do: 1
  def cv_GAUSSIAN, do: 2
  def cv_RELU, do: 3
  def cv_LEAKYRELU, do: 4
  def cv_UPDATE_WEIGHTS, do: 1
  def cv_NO_INPUT_SCALE, do: 2
  def cv_NO_OUTPUT_SCALE, do: 4
  def cv_REG_DISABLE, do: -1
  def cv_REG_L1, do: 0
  def cv_REG_L2, do: 1
  def cv_BATCH, do: 0
  def cv_MINI_BATCH, do: 1
  def cv_SGD, do: 0
  def cv_ASGD, do: 1
  def cv_SOFT_MARGIN, do: 0
  def cv_HARD_MARGIN, do: 1
  def cv_COLOR_BayerBG2BGR_MHT, do: 256
  def cv_COLOR_BayerGB2BGR_MHT, do: 257
  def cv_COLOR_BayerRG2BGR_MHT, do: 258
  def cv_COLOR_BayerGR2BGR_MHT, do: 259
  def cv_COLOR_BayerBG2RGB_MHT, do: cv_COLOR_BayerRG2BGR_MHT()
  def cv_COLOR_BayerGB2RGB_MHT, do: cv_COLOR_BayerGR2BGR_MHT()
  def cv_COLOR_BayerRG2RGB_MHT, do: cv_COLOR_BayerBG2BGR_MHT()
  def cv_COLOR_BayerGR2RGB_MHT, do: cv_COLOR_BayerGB2BGR_MHT()
  def cv_COLOR_BayerBG2GRAY_MHT, do: 260
  def cv_COLOR_BayerGB2GRAY_MHT, do: 261
  def cv_COLOR_BayerRG2GRAY_MHT, do: 262
  def cv_COLOR_BayerGR2GRAY_MHT, do: 263
  def cv_ALPHA_OVER, do: 0
  def cv_ALPHA_IN, do: 1
  def cv_ALPHA_OUT, do: 2
  def cv_ALPHA_ATOP, do: 3
  def cv_ALPHA_XOR, do: 4
  def cv_ALPHA_PLUS, do: 5
  def cv_ALPHA_OVER_PREMUL, do: 6
  def cv_ALPHA_IN_PREMUL, do: 7
  def cv_ALPHA_OUT_PREMUL, do: 8
  def cv_ALPHA_ATOP_PREMUL, do: 9
  def cv_ALPHA_XOR_PREMUL, do: 10
  def cv_ALPHA_PLUS_PREMUL, do: 11
  def cv_ALPHA_PREMUL, do: 12
  def cv_CCL_BKE, do: 0
  def cv_FIRST_ORDER_MOMENTS, do: 1
  def cv_SECOND_ORDER_MOMENTS, do: 2
  def cv_THIRD_ORDER_MOMENTS, do: 3
  def cv_DNN_BACKEND_DEFAULT, do: 0
  def cv_DNN_BACKEND_HALIDE, do: (0 + 1)
  def cv_DNN_BACKEND_INFERENCE_ENGINE, do: (0 + 2)
  def cv_DNN_BACKEND_OPENCV, do: (0 + 3)
  def cv_DNN_BACKEND_VKCOM, do: (0 + 4)
  def cv_DNN_BACKEND_CUDA, do: (0 + 5)
  def cv_DNN_BACKEND_WEBNN, do: (0 + 6)
  def cv_DNN_BACKEND_TIMVX, do: (0 + 7)
  def cv_DNN_BACKEND_CANN, do: (0 + 8)
  def cv_DNN_TARGET_CPU, do: 0
  def cv_DNN_TARGET_OPENCL, do: (0 + 1)
  def cv_DNN_TARGET_OPENCL_FP16, do: (0 + 2)
  def cv_DNN_TARGET_MYRIAD, do: (0 + 3)
  def cv_DNN_TARGET_VULKAN, do: (0 + 4)
  def cv_DNN_TARGET_FPGA, do: (0 + 5)
  def cv_DNN_TARGET_CUDA, do: (0 + 6)
  def cv_DNN_TARGET_CUDA_FP16, do: (0 + 7)
  def cv_DNN_TARGET_HDDL, do: (0 + 8)
  def cv_DNN_TARGET_NPU, do: (0 + 9)
  def cv_DNN_TARGET_CPU_FP16, do: (0 + 10)
  def cv_DNN_LAYOUT_UNKNOWN, do: 0
  def cv_DNN_LAYOUT_ND, do: 1
  def cv_DNN_LAYOUT_NCHW, do: 2
  def cv_DNN_LAYOUT_NCDHW, do: 3
  def cv_DNN_LAYOUT_NHWC, do: 4
  def cv_DNN_LAYOUT_NDHWC, do: 5
  def cv_DNN_LAYOUT_PLANAR, do: 6
  def cv_DNN_PMODE_NULL, do: 0
  def cv_DNN_PMODE_CROP_CENTER, do: 1
  def cv_DNN_PMODE_LETTERBOX, do: 2
  def cv_SOFTNMS_LINEAR, do: 1
  def cv_SOFTNMS_GAUSSIAN, do: 2
  def cv_HARRIS_SCORE, do: 0
  def cv_FAST_SCORE, do: 1
  def cv_TYPE_5_8, do: 0
  def cv_TYPE_7_12, do: 1
  def cv_TYPE_9_16, do: 2
  def cv_THRESHOLD, do: 10000
  def cv_NONMAX_SUPPRESSION, do: 10001
  def cv_FAST_N, do: 10002
  def cv_AGAST_5_8, do: 0
  def cv_AGAST_7_12d, do: 1
  def cv_AGAST_7_12s, do: 2
  def cv_OAST_9_16, do: 3
  def cv_DIFF_PM_G1, do: 0
  def cv_DIFF_PM_G2, do: 1
  def cv_DIFF_WEICKERT, do: 2
  def cv_DIFF_CHARBONNIER, do: 3
  def cv_DESCRIPTOR_KAZE_UPRIGHT, do: 2
  def cv_DESCRIPTOR_KAZE, do: 3
  def cv_DESCRIPTOR_MLDB_UPRIGHT, do: 4
  def cv_DESCRIPTOR_MLDB, do: 5
  def cv_FLANNBASED, do: 1
  def cv_BRUTEFORCE, do: 2
  def cv_BRUTEFORCE_L1, do: 3
  def cv_BRUTEFORCE_HAMMING, do: 4
  def cv_BRUTEFORCE_HAMMINGLUT, do: 5
  def cv_BRUTEFORCE_SL2, do: 6
  def cv_DRAW_OVER_OUTIMG, do: 1
  def cv_NOT_DRAW_SINGLE_POINTS, do: 2
  def cv_DRAW_RICH_KEYPOINTS, do: 4
  def cv_ft_LINEAR, do: 1
  def cv_SINUS, do: 2
  def cv_ONE_STEP, do: 1
  def cv_MULTI_STEP, do: 2
  def cv_ITERATIVE, do: 3
  def cv_BLOCK_MEAN_HASH_MODE_0, do: 0
  def cv_BLOCK_MEAN_HASH_MODE_1, do: 1
  def cv_IMREAD_UNCHANGED, do: -1
  def cv_IMREAD_GRAYSCALE, do: 0
  def cv_IMREAD_COLOR, do: 1
  def cv_IMREAD_ANYDEPTH, do: 2
  def cv_IMREAD_ANYCOLOR, do: 4
  def cv_IMREAD_LOAD_GDAL, do: 8
  def cv_IMREAD_REDUCED_GRAYSCALE_2, do: 16
  def cv_IMREAD_REDUCED_COLOR_2, do: 17
  def cv_IMREAD_REDUCED_GRAYSCALE_4, do: 32
  def cv_IMREAD_REDUCED_COLOR_4, do: 33
  def cv_IMREAD_REDUCED_GRAYSCALE_8, do: 64
  def cv_IMREAD_REDUCED_COLOR_8, do: 65
  def cv_IMREAD_IGNORE_ORIENTATION, do: 128
  def cv_IMWRITE_JPEG_QUALITY, do: 1
  def cv_IMWRITE_JPEG_PROGRESSIVE, do: 2
  def cv_IMWRITE_JPEG_OPTIMIZE, do: 3
  def cv_IMWRITE_JPEG_RST_INTERVAL, do: 4
  def cv_IMWRITE_JPEG_LUMA_QUALITY, do: 5
  def cv_IMWRITE_JPEG_CHROMA_QUALITY, do: 6
  def cv_IMWRITE_JPEG_SAMPLING_FACTOR, do: 7
  def cv_IMWRITE_PNG_COMPRESSION, do: 16
  def cv_IMWRITE_PNG_STRATEGY, do: 17
  def cv_IMWRITE_PNG_BILEVEL, do: 18
  def cv_IMWRITE_PXM_BINARY, do: 32
  def cv_IMWRITE_EXR_TYPE, do: (bsl(3, 4) + 0)
  def cv_IMWRITE_EXR_COMPRESSION, do: (bsl(3, 4) + 1)
  def cv_IMWRITE_EXR_DWA_COMPRESSION_LEVEL, do: (bsl(3, 4) + 2)
  def cv_IMWRITE_WEBP_QUALITY, do: 64
  def cv_IMWRITE_HDR_COMPRESSION, do: (bsl(5, 4) + 0)
  def cv_IMWRITE_PAM_TUPLETYPE, do: 128
  def cv_IMWRITE_TIFF_RESUNIT, do: 256
  def cv_IMWRITE_TIFF_XDPI, do: 257
  def cv_IMWRITE_TIFF_YDPI, do: 258
  def cv_IMWRITE_TIFF_COMPRESSION, do: 259
  def cv_IMWRITE_TIFF_ROWSPERSTRIP, do: 278
  def cv_IMWRITE_TIFF_PREDICTOR, do: 317
  def cv_IMWRITE_JPEG2000_COMPRESSION_X1000, do: 272
  def cv_IMWRITE_AVIF_QUALITY, do: 512
  def cv_IMWRITE_AVIF_DEPTH, do: 513
  def cv_IMWRITE_AVIF_SPEED, do: 514
  def cv_IMWRITE_JPEG_SAMPLING_FACTOR_411, do: 4264209
  def cv_IMWRITE_JPEG_SAMPLING_FACTOR_420, do: 2232593
  def cv_IMWRITE_JPEG_SAMPLING_FACTOR_422, do: 2167057
  def cv_IMWRITE_JPEG_SAMPLING_FACTOR_440, do: 1184017
  def cv_IMWRITE_JPEG_SAMPLING_FACTOR_444, do: 1118481
  def cv_IMWRITE_TIFF_COMPRESSION_NONE, do: 1
  def cv_IMWRITE_TIFF_COMPRESSION_CCITTRLE, do: 2
  def cv_IMWRITE_TIFF_COMPRESSION_CCITTFAX3, do: 3
  def cv_IMWRITE_TIFF_COMPRESSION_CCITT_T4, do: 3
  def cv_IMWRITE_TIFF_COMPRESSION_CCITTFAX4, do: 4
  def cv_IMWRITE_TIFF_COMPRESSION_CCITT_T6, do: 4
  def cv_IMWRITE_TIFF_COMPRESSION_LZW, do: 5
  def cv_IMWRITE_TIFF_COMPRESSION_OJPEG, do: 6
  def cv_IMWRITE_TIFF_COMPRESSION_JPEG, do: 7
  def cv_IMWRITE_TIFF_COMPRESSION_T85, do: 9
  def cv_IMWRITE_TIFF_COMPRESSION_T43, do: 10
  def cv_IMWRITE_TIFF_COMPRESSION_NEXT, do: 32766
  def cv_IMWRITE_TIFF_COMPRESSION_CCITTRLEW, do: 32771
  def cv_IMWRITE_TIFF_COMPRESSION_PACKBITS, do: 32773
  def cv_IMWRITE_TIFF_COMPRESSION_THUNDERSCAN, do: 32809
  def cv_IMWRITE_TIFF_COMPRESSION_IT8CTPAD, do: 32895
  def cv_IMWRITE_TIFF_COMPRESSION_IT8LW, do: 32896
  def cv_IMWRITE_TIFF_COMPRESSION_IT8MP, do: 32897
  def cv_IMWRITE_TIFF_COMPRESSION_IT8BL, do: 32898
  def cv_IMWRITE_TIFF_COMPRESSION_PIXARFILM, do: 32908
  def cv_IMWRITE_TIFF_COMPRESSION_PIXARLOG, do: 32909
  def cv_IMWRITE_TIFF_COMPRESSION_DEFLATE, do: 32946
  def cv_IMWRITE_TIFF_COMPRESSION_ADOBE_DEFLATE, do: 8
  def cv_IMWRITE_TIFF_COMPRESSION_DCS, do: 32947
  def cv_IMWRITE_TIFF_COMPRESSION_JBIG, do: 34661
  def cv_IMWRITE_TIFF_COMPRESSION_SGILOG, do: 34676
  def cv_IMWRITE_TIFF_COMPRESSION_SGILOG24, do: 34677
  def cv_IMWRITE_TIFF_COMPRESSION_JP2000, do: 34712
  def cv_IMWRITE_TIFF_COMPRESSION_LERC, do: 34887
  def cv_IMWRITE_TIFF_COMPRESSION_LZMA, do: 34925
  def cv_IMWRITE_TIFF_COMPRESSION_ZSTD, do: 50000
  def cv_IMWRITE_TIFF_COMPRESSION_WEBP, do: 50001
  def cv_IMWRITE_TIFF_COMPRESSION_JXL, do: 50002
  def cv_IMWRITE_TIFF_PREDICTOR_NONE, do: 1
  def cv_IMWRITE_TIFF_PREDICTOR_HORIZONTAL, do: 2
  def cv_IMWRITE_TIFF_PREDICTOR_FLOATINGPOINT, do: 3
  def cv_IMWRITE_EXR_TYPE_HALF, do: 1
  def cv_IMWRITE_EXR_TYPE_FLOAT, do: 2
  def cv_IMWRITE_EXR_COMPRESSION_NO, do: 0
  def cv_IMWRITE_EXR_COMPRESSION_RLE, do: 1
  def cv_IMWRITE_EXR_COMPRESSION_ZIPS, do: 2
  def cv_IMWRITE_EXR_COMPRESSION_ZIP, do: 3
  def cv_IMWRITE_EXR_COMPRESSION_PIZ, do: 4
  def cv_IMWRITE_EXR_COMPRESSION_PXR24, do: 5
  def cv_IMWRITE_EXR_COMPRESSION_B44, do: 6
  def cv_IMWRITE_EXR_COMPRESSION_B44A, do: 7
  def cv_IMWRITE_EXR_COMPRESSION_DWAA, do: 8
  def cv_IMWRITE_EXR_COMPRESSION_DWAB, do: 9
  def cv_IMWRITE_PNG_STRATEGY_DEFAULT, do: 0
  def cv_IMWRITE_PNG_STRATEGY_FILTERED, do: 1
  def cv_IMWRITE_PNG_STRATEGY_HUFFMAN_ONLY, do: 2
  def cv_IMWRITE_PNG_STRATEGY_RLE, do: 3
  def cv_IMWRITE_PNG_STRATEGY_FIXED, do: 4
  def cv_IMWRITE_PAM_FORMAT_NULL, do: 0
  def cv_IMWRITE_PAM_FORMAT_BLACKANDWHITE, do: 1
  def cv_IMWRITE_PAM_FORMAT_GRAYSCALE, do: 2
  def cv_IMWRITE_PAM_FORMAT_GRAYSCALE_ALPHA, do: 3
  def cv_IMWRITE_PAM_FORMAT_RGB, do: 4
  def cv_IMWRITE_PAM_FORMAT_RGB_ALPHA, do: 5
  def cv_IMWRITE_HDR_COMPRESSION_NONE, do: 0
  def cv_IMWRITE_HDR_COMPRESSION_RLE, do: 1
  def cv_INPAINT_NS, do: 0
  def cv_INPAINT_TELEA, do: 1
  def cv_LDR_SIZE, do: 256
  def cv_NORMAL_CLONE, do: 1
  def cv_MIXED_CLONE, do: 2
  def cv_MONOCHROME_TRANSFER, do: 3
  def cv_RECURS_FILTER, do: 1
  def cv_NORMCONV_FILTER, do: 2
  def cv_ERFILTER_NM_RGBLGrad, do: 0
  def cv_ERFILTER_NM_IHSGrad, do: 1
  def cv_ERGROUPING_ORIENTATION_HORIZ, do: 0
  def cv_ERGROUPING_ORIENTATION_ANY, do: 1
  def cv_OCR_LEVEL_WORD, do: 0
  def cv_OCR_LEVEL_TEXTLINE, do: 1
  def cv_PSM_OSD_ONLY, do: 0
  def cv_PSM_AUTO_OSD, do: 1
  def cv_PSM_AUTO_ONLY, do: 2
  def cv_PSM_AUTO, do: 3
  def cv_PSM_SINGLE_COLUMN, do: 4
  def cv_PSM_SINGLE_BLOCK_VERT_TEXT, do: 5
  def cv_PSM_SINGLE_BLOCK, do: 6
  def cv_PSM_SINGLE_LINE, do: 7
  def cv_PSM_SINGLE_WORD, do: 8
  def cv_PSM_CIRCLE_WORD, do: 9
  def cv_PSM_SINGLE_CHAR, do: 10
  def cv_OEM_TESSERACT_ONLY, do: 0
  def cv_OEM_CUBE_ONLY, do: 1
  def cv_OEM_TESSERACT_CUBE_COMBINED, do: 2
  def cv_OEM_DEFAULT, do: 3
  def cv_OCR_DECODER_VITERBI, do: 0
  def cv_OCR_KNN_CLASSIFIER, do: 0
  def cv_OCR_CNN_CLASSIFIER, do: 1
  def cv_CAP_ANY, do: 0
  def cv_CAP_VFW, do: 200
  def cv_CAP_V4L, do: 200
  def cv_CAP_V4L2, do: cv_CAP_V4L()
  def cv_CAP_FIREWIRE, do: 300
  def cv_CAP_FIREWARE, do: cv_CAP_FIREWIRE()
  def cv_CAP_IEEE1394, do: cv_CAP_FIREWIRE()
  def cv_CAP_DC1394, do: cv_CAP_FIREWIRE()
  def cv_CAP_CMU1394, do: cv_CAP_FIREWIRE()
  def cv_CAP_QT, do: 500
  def cv_CAP_UNICAP, do: 600
  def cv_CAP_DSHOW, do: 700
  def cv_CAP_PVAPI, do: 800
  def cv_CAP_OPENNI, do: 900
  def cv_CAP_OPENNI_ASUS, do: 910
  def cv_CAP_ANDROID, do: 1000
  def cv_CAP_XIAPI, do: 1100
  def cv_CAP_AVFOUNDATION, do: 1200
  def cv_CAP_GIGANETIX, do: 1300
  def cv_CAP_MSMF, do: 1400
  def cv_CAP_WINRT, do: 1410
  def cv_CAP_INTELPERC, do: 1500
  def cv_CAP_REALSENSE, do: 1500
  def cv_CAP_OPENNI2, do: 1600
  def cv_CAP_OPENNI2_ASUS, do: 1610
  def cv_CAP_OPENNI2_ASTRA, do: 1620
  def cv_CAP_GPHOTO2, do: 1700
  def cv_CAP_GSTREAMER, do: 1800
  def cv_CAP_FFMPEG, do: 1900
  def cv_CAP_IMAGES, do: 2000
  def cv_CAP_ARAVIS, do: 2100
  def cv_CAP_OPENCV_MJPEG, do: 2200
  def cv_CAP_INTEL_MFX, do: 2300
  def cv_CAP_XINE, do: 2400
  def cv_CAP_UEYE, do: 2500
  def cv_CAP_OBSENSOR, do: 2600
  def cv_CAP_PROP_POS_MSEC, do: 0
  def cv_CAP_PROP_POS_FRAMES, do: 1
  def cv_CAP_PROP_POS_AVI_RATIO, do: 2
  def cv_CAP_PROP_FRAME_WIDTH, do: 3
  def cv_CAP_PROP_FRAME_HEIGHT, do: 4
  def cv_CAP_PROP_FPS, do: 5
  def cv_CAP_PROP_FOURCC, do: 6
  def cv_CAP_PROP_FRAME_COUNT, do: 7
  def cv_CAP_PROP_FORMAT, do: 8
  def cv_CAP_PROP_MODE, do: 9
  def cv_CAP_PROP_BRIGHTNESS, do: 10
  def cv_CAP_PROP_CONTRAST, do: 11
  def cv_CAP_PROP_SATURATION, do: 12
  def cv_CAP_PROP_HUE, do: 13
  def cv_CAP_PROP_GAIN, do: 14
  def cv_CAP_PROP_EXPOSURE, do: 15
  def cv_CAP_PROP_CONVERT_RGB, do: 16
  def cv_CAP_PROP_WHITE_BALANCE_BLUE_U, do: 17
  def cv_CAP_PROP_RECTIFICATION, do: 18
  def cv_CAP_PROP_MONOCHROME, do: 19
  def cv_CAP_PROP_SHARPNESS, do: 20
  def cv_CAP_PROP_AUTO_EXPOSURE, do: 21
  def cv_CAP_PROP_GAMMA, do: 22
  def cv_CAP_PROP_TEMPERATURE, do: 23
  def cv_CAP_PROP_TRIGGER, do: 24
  def cv_CAP_PROP_TRIGGER_DELAY, do: 25
  def cv_CAP_PROP_WHITE_BALANCE_RED_V, do: 26
  def cv_CAP_PROP_ZOOM, do: 27
  def cv_CAP_PROP_FOCUS, do: 28
  def cv_CAP_PROP_GUID, do: 29
  def cv_CAP_PROP_ISO_SPEED, do: 30
  def cv_CAP_PROP_BACKLIGHT, do: 32
  def cv_CAP_PROP_PAN, do: 33
  def cv_CAP_PROP_TILT, do: 34
  def cv_CAP_PROP_ROLL, do: 35
  def cv_CAP_PROP_IRIS, do: 36
  def cv_CAP_PROP_SETTINGS, do: 37
  def cv_CAP_PROP_BUFFERSIZE, do: 38
  def cv_CAP_PROP_AUTOFOCUS, do: 39
  def cv_CAP_PROP_SAR_NUM, do: 40
  def cv_CAP_PROP_SAR_DEN, do: 41
  def cv_CAP_PROP_BACKEND, do: 42
  def cv_CAP_PROP_CHANNEL, do: 43
  def cv_CAP_PROP_AUTO_WB, do: 44
  def cv_CAP_PROP_WB_TEMPERATURE, do: 45
  def cv_CAP_PROP_CODEC_PIXEL_FORMAT, do: 46
  def cv_CAP_PROP_BITRATE, do: 47
  def cv_CAP_PROP_ORIENTATION_META, do: 48
  def cv_CAP_PROP_ORIENTATION_AUTO, do: 49
  def cv_CAP_PROP_HW_ACCELERATION, do: 50
  def cv_CAP_PROP_HW_DEVICE, do: 51
  def cv_CAP_PROP_HW_ACCELERATION_USE_OPENCL, do: 52
  def cv_CAP_PROP_OPEN_TIMEOUT_MSEC, do: 53
  def cv_CAP_PROP_READ_TIMEOUT_MSEC, do: 54
  def cv_CAP_PROP_STREAM_OPEN_TIME_USEC, do: 55
  def cv_CAP_PROP_VIDEO_TOTAL_CHANNELS, do: 56
  def cv_CAP_PROP_VIDEO_STREAM, do: 57
  def cv_CAP_PROP_AUDIO_STREAM, do: 58
  def cv_CAP_PROP_AUDIO_POS, do: 59
  def cv_CAP_PROP_AUDIO_SHIFT_NSEC, do: 60
  def cv_CAP_PROP_AUDIO_DATA_DEPTH, do: 61
  def cv_CAP_PROP_AUDIO_SAMPLES_PER_SECOND, do: 62
  def cv_CAP_PROP_AUDIO_BASE_INDEX, do: 63
  def cv_CAP_PROP_AUDIO_TOTAL_CHANNELS, do: 64
  def cv_CAP_PROP_AUDIO_TOTAL_STREAMS, do: 65
  def cv_CAP_PROP_AUDIO_SYNCHRONIZE, do: 66
  def cv_CAP_PROP_LRF_HAS_KEY_FRAME, do: 67
  def cv_CAP_PROP_CODEC_EXTRADATA_INDEX, do: 68
  def cv_CAP_PROP_FRAME_TYPE, do: 69
  def cv_CAP_PROP_N_THREADS, do: 70
  def cv_VIDEOWRITER_PROP_QUALITY, do: 1
  def cv_VIDEOWRITER_PROP_FRAMEBYTES, do: 2
  def cv_VIDEOWRITER_PROP_NSTRIPES, do: 3
  def cv_VIDEOWRITER_PROP_IS_COLOR, do: 4
  def cv_VIDEOWRITER_PROP_DEPTH, do: 5
  def cv_VIDEOWRITER_PROP_HW_ACCELERATION, do: 6
  def cv_VIDEOWRITER_PROP_HW_DEVICE, do: 7
  def cv_VIDEOWRITER_PROP_HW_ACCELERATION_USE_OPENCL, do: 8
  def cv_VIDEOWRITER_PROP_RAW_VIDEO, do: 9
  def cv_VIDEOWRITER_PROP_KEY_INTERVAL, do: 10
  def cv_VIDEOWRITER_PROP_KEY_FLAG, do: 11
  def cv_VIDEO_ACCELERATION_NONE, do: 0
  def cv_VIDEO_ACCELERATION_ANY, do: 1
  def cv_VIDEO_ACCELERATION_D3D11, do: 2
  def cv_VIDEO_ACCELERATION_VAAPI, do: 3
  def cv_VIDEO_ACCELERATION_MFX, do: 4
  def cv_CAP_PROP_DC1394_OFF, do: -4
  def cv_CAP_PROP_DC1394_MODE_MANUAL, do: -3
  def cv_CAP_PROP_DC1394_MODE_AUTO, do: -2
  def cv_CAP_PROP_DC1394_MODE_ONE_PUSH_AUTO, do: -1
  def cv_CAP_PROP_DC1394_MAX, do: 31
  def cv_CAP_OPENNI_DEPTH_GENERATOR, do: bsl(1, 31)
  def cv_CAP_OPENNI_IMAGE_GENERATOR, do: bsl(1, 30)
  def cv_CAP_OPENNI_IR_GENERATOR, do: bsl(1, 29)
  def cv_CAP_OPENNI_GENERATORS_MASK, do: ((cv_CAP_OPENNI_DEPTH_GENERATOR() + cv_CAP_OPENNI_IMAGE_GENERATOR()) + cv_CAP_OPENNI_IR_GENERATOR())
  def cv_CAP_PROP_OPENNI_OUTPUT_MODE, do: 100
  def cv_CAP_PROP_OPENNI_FRAME_MAX_DEPTH, do: 101
  def cv_CAP_PROP_OPENNI_BASELINE, do: 102
  def cv_CAP_PROP_OPENNI_FOCAL_LENGTH, do: 103
  def cv_CAP_PROP_OPENNI_REGISTRATION, do: 104
  def cv_CAP_PROP_OPENNI_REGISTRATION_ON, do: cv_CAP_PROP_OPENNI_REGISTRATION()
  def cv_CAP_PROP_OPENNI_APPROX_FRAME_SYNC, do: 105
  def cv_CAP_PROP_OPENNI_MAX_BUFFER_SIZE, do: 106
  def cv_CAP_PROP_OPENNI_CIRCLE_BUFFER, do: 107
  def cv_CAP_PROP_OPENNI_MAX_TIME_DURATION, do: 108
  def cv_CAP_PROP_OPENNI_GENERATOR_PRESENT, do: 109
  def cv_CAP_PROP_OPENNI2_SYNC, do: 110
  def cv_CAP_PROP_OPENNI2_MIRROR, do: 111
  def cv_CAP_OPENNI_IMAGE_GENERATOR_PRESENT, do: (cv_CAP_OPENNI_IMAGE_GENERATOR() + cv_CAP_PROP_OPENNI_GENERATOR_PRESENT())
  def cv_CAP_OPENNI_IMAGE_GENERATOR_OUTPUT_MODE, do: (cv_CAP_OPENNI_IMAGE_GENERATOR() + cv_CAP_PROP_OPENNI_OUTPUT_MODE())
  def cv_CAP_OPENNI_DEPTH_GENERATOR_PRESENT, do: (cv_CAP_OPENNI_DEPTH_GENERATOR() + cv_CAP_PROP_OPENNI_GENERATOR_PRESENT())
  def cv_CAP_OPENNI_DEPTH_GENERATOR_BASELINE, do: (cv_CAP_OPENNI_DEPTH_GENERATOR() + cv_CAP_PROP_OPENNI_BASELINE())
  def cv_CAP_OPENNI_DEPTH_GENERATOR_FOCAL_LENGTH, do: (cv_CAP_OPENNI_DEPTH_GENERATOR() + cv_CAP_PROP_OPENNI_FOCAL_LENGTH())
  def cv_CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION, do: (cv_CAP_OPENNI_DEPTH_GENERATOR() + cv_CAP_PROP_OPENNI_REGISTRATION())
  def cv_CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION_ON, do: cv_CAP_OPENNI_DEPTH_GENERATOR_REGISTRATION()
  def cv_CAP_OPENNI_IR_GENERATOR_PRESENT, do: (cv_CAP_OPENNI_IR_GENERATOR() + cv_CAP_PROP_OPENNI_GENERATOR_PRESENT())
  def cv_CAP_OPENNI_DEPTH_MAP, do: 0
  def cv_CAP_OPENNI_POINT_CLOUD_MAP, do: 1
  def cv_CAP_OPENNI_DISPARITY_MAP, do: 2
  def cv_CAP_OPENNI_DISPARITY_MAP_32F, do: 3
  def cv_CAP_OPENNI_VALID_DEPTH_MASK, do: 4
  def cv_CAP_OPENNI_BGR_IMAGE, do: 5
  def cv_CAP_OPENNI_GRAY_IMAGE, do: 6
  def cv_CAP_OPENNI_IR_IMAGE, do: 7
  def cv_CAP_OPENNI_VGA_30HZ, do: 0
  def cv_CAP_OPENNI_SXGA_15HZ, do: 1
  def cv_CAP_OPENNI_SXGA_30HZ, do: 2
  def cv_CAP_OPENNI_QVGA_30HZ, do: 3
  def cv_CAP_OPENNI_QVGA_60HZ, do: 4
  def cv_CAP_PROP_GSTREAMER_QUEUE_LENGTH, do: 200
  def cv_CAP_PROP_PVAPI_MULTICASTIP, do: 300
  def cv_CAP_PROP_PVAPI_FRAMESTARTTRIGGERMODE, do: 301
  def cv_CAP_PROP_PVAPI_DECIMATIONHORIZONTAL, do: 302
  def cv_CAP_PROP_PVAPI_DECIMATIONVERTICAL, do: 303
  def cv_CAP_PROP_PVAPI_BINNINGX, do: 304
  def cv_CAP_PROP_PVAPI_BINNINGY, do: 305
  def cv_CAP_PROP_PVAPI_PIXELFORMAT, do: 306
  def cv_CAP_PVAPI_FSTRIGMODE_FREERUN, do: 0
  def cv_CAP_PVAPI_FSTRIGMODE_SYNCIN1, do: 1
  def cv_CAP_PVAPI_FSTRIGMODE_SYNCIN2, do: 2
  def cv_CAP_PVAPI_FSTRIGMODE_FIXEDRATE, do: 3
  def cv_CAP_PVAPI_FSTRIGMODE_SOFTWARE, do: 4
  def cv_CAP_PVAPI_DECIMATION_OFF, do: 1
  def cv_CAP_PVAPI_DECIMATION_2OUTOF4, do: 2
  def cv_CAP_PVAPI_DECIMATION_2OUTOF8, do: 4
  def cv_CAP_PVAPI_DECIMATION_2OUTOF16, do: 8
  def cv_CAP_PVAPI_PIXELFORMAT_MONO8, do: 1
  def cv_CAP_PVAPI_PIXELFORMAT_MONO16, do: 2
  def cv_CAP_PVAPI_PIXELFORMAT_BAYER8, do: 3
  def cv_CAP_PVAPI_PIXELFORMAT_BAYER16, do: 4
  def cv_CAP_PVAPI_PIXELFORMAT_RGB24, do: 5
  def cv_CAP_PVAPI_PIXELFORMAT_BGR24, do: 6
  def cv_CAP_PVAPI_PIXELFORMAT_RGBA32, do: 7
  def cv_CAP_PVAPI_PIXELFORMAT_BGRA32, do: 8
  def cv_CAP_PROP_XI_DOWNSAMPLING, do: 400
  def cv_CAP_PROP_XI_DATA_FORMAT, do: 401
  def cv_CAP_PROP_XI_OFFSET_X, do: 402
  def cv_CAP_PROP_XI_OFFSET_Y, do: 403
  def cv_CAP_PROP_XI_TRG_SOURCE, do: 404
  def cv_CAP_PROP_XI_TRG_SOFTWARE, do: 405
  def cv_CAP_PROP_XI_GPI_SELECTOR, do: 406
  def cv_CAP_PROP_XI_GPI_MODE, do: 407
  def cv_CAP_PROP_XI_GPI_LEVEL, do: 408
  def cv_CAP_PROP_XI_GPO_SELECTOR, do: 409
  def cv_CAP_PROP_XI_GPO_MODE, do: 410
  def cv_CAP_PROP_XI_LED_SELECTOR, do: 411
  def cv_CAP_PROP_XI_LED_MODE, do: 412
  def cv_CAP_PROP_XI_MANUAL_WB, do: 413
  def cv_CAP_PROP_XI_AUTO_WB, do: 414
  def cv_CAP_PROP_XI_AEAG, do: 415
  def cv_CAP_PROP_XI_EXP_PRIORITY, do: 416
  def cv_CAP_PROP_XI_AE_MAX_LIMIT, do: 417
  def cv_CAP_PROP_XI_AG_MAX_LIMIT, do: 418
  def cv_CAP_PROP_XI_AEAG_LEVEL, do: 419
  def cv_CAP_PROP_XI_TIMEOUT, do: 420
  def cv_CAP_PROP_XI_EXPOSURE, do: 421
  def cv_CAP_PROP_XI_EXPOSURE_BURST_COUNT, do: 422
  def cv_CAP_PROP_XI_GAIN_SELECTOR, do: 423
  def cv_CAP_PROP_XI_GAIN, do: 424
  def cv_CAP_PROP_XI_DOWNSAMPLING_TYPE, do: 426
  def cv_CAP_PROP_XI_BINNING_SELECTOR, do: 427
  def cv_CAP_PROP_XI_BINNING_VERTICAL, do: 428
  def cv_CAP_PROP_XI_BINNING_HORIZONTAL, do: 429
  def cv_CAP_PROP_XI_BINNING_PATTERN, do: 430
  def cv_CAP_PROP_XI_DECIMATION_SELECTOR, do: 431
  def cv_CAP_PROP_XI_DECIMATION_VERTICAL, do: 432
  def cv_CAP_PROP_XI_DECIMATION_HORIZONTAL, do: 433
  def cv_CAP_PROP_XI_DECIMATION_PATTERN, do: 434
  def cv_CAP_PROP_XI_TEST_PATTERN_GENERATOR_SELECTOR, do: 587
  def cv_CAP_PROP_XI_TEST_PATTERN, do: 588
  def cv_CAP_PROP_XI_IMAGE_DATA_FORMAT, do: 435
  def cv_CAP_PROP_XI_SHUTTER_TYPE, do: 436
  def cv_CAP_PROP_XI_SENSOR_TAPS, do: 437
  def cv_CAP_PROP_XI_AEAG_ROI_OFFSET_X, do: 439
  def cv_CAP_PROP_XI_AEAG_ROI_OFFSET_Y, do: 440
  def cv_CAP_PROP_XI_AEAG_ROI_WIDTH, do: 441
  def cv_CAP_PROP_XI_AEAG_ROI_HEIGHT, do: 442
  def cv_CAP_PROP_XI_BPC, do: 445
  def cv_CAP_PROP_XI_WB_KR, do: 448
  def cv_CAP_PROP_XI_WB_KG, do: 449
  def cv_CAP_PROP_XI_WB_KB, do: 450
  def cv_CAP_PROP_XI_WIDTH, do: 451
  def cv_CAP_PROP_XI_HEIGHT, do: 452
  def cv_CAP_PROP_XI_REGION_SELECTOR, do: 589
  def cv_CAP_PROP_XI_REGION_MODE, do: 595
  def cv_CAP_PROP_XI_LIMIT_BANDWIDTH, do: 459
  def cv_CAP_PROP_XI_SENSOR_DATA_BIT_DEPTH, do: 460
  def cv_CAP_PROP_XI_OUTPUT_DATA_BIT_DEPTH, do: 461
  def cv_CAP_PROP_XI_IMAGE_DATA_BIT_DEPTH, do: 462
  def cv_CAP_PROP_XI_OUTPUT_DATA_PACKING, do: 463
  def cv_CAP_PROP_XI_OUTPUT_DATA_PACKING_TYPE, do: 464
  def cv_CAP_PROP_XI_IS_COOLED, do: 465
  def cv_CAP_PROP_XI_COOLING, do: 466
  def cv_CAP_PROP_XI_TARGET_TEMP, do: 467
  def cv_CAP_PROP_XI_CHIP_TEMP, do: 468
  def cv_CAP_PROP_XI_HOUS_TEMP, do: 469
  def cv_CAP_PROP_XI_HOUS_BACK_SIDE_TEMP, do: 590
  def cv_CAP_PROP_XI_SENSOR_BOARD_TEMP, do: 596
  def cv_CAP_PROP_XI_CMS, do: 470
  def cv_CAP_PROP_XI_APPLY_CMS, do: 471
  def cv_CAP_PROP_XI_IMAGE_IS_COLOR, do: 474
  def cv_CAP_PROP_XI_COLOR_FILTER_ARRAY, do: 475
  def cv_CAP_PROP_XI_GAMMAY, do: 476
  def cv_CAP_PROP_XI_GAMMAC, do: 477
  def cv_CAP_PROP_XI_SHARPNESS, do: 478
  def cv_CAP_PROP_XI_CC_MATRIX_00, do: 479
  def cv_CAP_PROP_XI_CC_MATRIX_01, do: 480
  def cv_CAP_PROP_XI_CC_MATRIX_02, do: 481
  def cv_CAP_PROP_XI_CC_MATRIX_03, do: 482
  def cv_CAP_PROP_XI_CC_MATRIX_10, do: 483
  def cv_CAP_PROP_XI_CC_MATRIX_11, do: 484
  def cv_CAP_PROP_XI_CC_MATRIX_12, do: 485
  def cv_CAP_PROP_XI_CC_MATRIX_13, do: 486
  def cv_CAP_PROP_XI_CC_MATRIX_20, do: 487
  def cv_CAP_PROP_XI_CC_MATRIX_21, do: 488
  def cv_CAP_PROP_XI_CC_MATRIX_22, do: 489
  def cv_CAP_PROP_XI_CC_MATRIX_23, do: 490
  def cv_CAP_PROP_XI_CC_MATRIX_30, do: 491
  def cv_CAP_PROP_XI_CC_MATRIX_31, do: 492
  def cv_CAP_PROP_XI_CC_MATRIX_32, do: 493
  def cv_CAP_PROP_XI_CC_MATRIX_33, do: 494
  def cv_CAP_PROP_XI_DEFAULT_CC_MATRIX, do: 495
  def cv_CAP_PROP_XI_TRG_SELECTOR, do: 498
  def cv_CAP_PROP_XI_ACQ_FRAME_BURST_COUNT, do: 499
  def cv_CAP_PROP_XI_DEBOUNCE_EN, do: 507
  def cv_CAP_PROP_XI_DEBOUNCE_T0, do: 508
  def cv_CAP_PROP_XI_DEBOUNCE_T1, do: 509
  def cv_CAP_PROP_XI_DEBOUNCE_POL, do: 510
  def cv_CAP_PROP_XI_LENS_MODE, do: 511
  def cv_CAP_PROP_XI_LENS_APERTURE_VALUE, do: 512
  def cv_CAP_PROP_XI_LENS_FOCUS_MOVEMENT_VALUE, do: 513
  def cv_CAP_PROP_XI_LENS_FOCUS_MOVE, do: 514
  def cv_CAP_PROP_XI_LENS_FOCUS_DISTANCE, do: 515
  def cv_CAP_PROP_XI_LENS_FOCAL_LENGTH, do: 516
  def cv_CAP_PROP_XI_LENS_FEATURE_SELECTOR, do: 517
  def cv_CAP_PROP_XI_LENS_FEATURE, do: 518
  def cv_CAP_PROP_XI_DEVICE_MODEL_ID, do: 521
  def cv_CAP_PROP_XI_DEVICE_SN, do: 522
  def cv_CAP_PROP_XI_IMAGE_DATA_FORMAT_RGB32_ALPHA, do: 529
  def cv_CAP_PROP_XI_IMAGE_PAYLOAD_SIZE, do: 530
  def cv_CAP_PROP_XI_TRANSPORT_PIXEL_FORMAT, do: 531
  def cv_CAP_PROP_XI_SENSOR_CLOCK_FREQ_HZ, do: 532
  def cv_CAP_PROP_XI_SENSOR_CLOCK_FREQ_INDEX, do: 533
  def cv_CAP_PROP_XI_SENSOR_OUTPUT_CHANNEL_COUNT, do: 534
  def cv_CAP_PROP_XI_FRAMERATE, do: 535
  def cv_CAP_PROP_XI_COUNTER_SELECTOR, do: 536
  def cv_CAP_PROP_XI_COUNTER_VALUE, do: 537
  def cv_CAP_PROP_XI_ACQ_TIMING_MODE, do: 538
  def cv_CAP_PROP_XI_AVAILABLE_BANDWIDTH, do: 539
  def cv_CAP_PROP_XI_BUFFER_POLICY, do: 540
  def cv_CAP_PROP_XI_LUT_EN, do: 541
  def cv_CAP_PROP_XI_LUT_INDEX, do: 542
  def cv_CAP_PROP_XI_LUT_VALUE, do: 543
  def cv_CAP_PROP_XI_TRG_DELAY, do: 544
  def cv_CAP_PROP_XI_TS_RST_MODE, do: 545
  def cv_CAP_PROP_XI_TS_RST_SOURCE, do: 546
  def cv_CAP_PROP_XI_IS_DEVICE_EXIST, do: 547
  def cv_CAP_PROP_XI_ACQ_BUFFER_SIZE, do: 548
  def cv_CAP_PROP_XI_ACQ_BUFFER_SIZE_UNIT, do: 549
  def cv_CAP_PROP_XI_ACQ_TRANSPORT_BUFFER_SIZE, do: 550
  def cv_CAP_PROP_XI_BUFFERS_QUEUE_SIZE, do: 551
  def cv_CAP_PROP_XI_ACQ_TRANSPORT_BUFFER_COMMIT, do: 552
  def cv_CAP_PROP_XI_RECENT_FRAME, do: 553
  def cv_CAP_PROP_XI_DEVICE_RESET, do: 554
  def cv_CAP_PROP_XI_COLUMN_FPN_CORRECTION, do: 555
  def cv_CAP_PROP_XI_ROW_FPN_CORRECTION, do: 591
  def cv_CAP_PROP_XI_SENSOR_MODE, do: 558
  def cv_CAP_PROP_XI_HDR, do: 559
  def cv_CAP_PROP_XI_HDR_KNEEPOINT_COUNT, do: 560
  def cv_CAP_PROP_XI_HDR_T1, do: 561
  def cv_CAP_PROP_XI_HDR_T2, do: 562
  def cv_CAP_PROP_XI_KNEEPOINT1, do: 563
  def cv_CAP_PROP_XI_KNEEPOINT2, do: 564
  def cv_CAP_PROP_XI_IMAGE_BLACK_LEVEL, do: 565
  def cv_CAP_PROP_XI_HW_REVISION, do: 571
  def cv_CAP_PROP_XI_DEBUG_LEVEL, do: 572
  def cv_CAP_PROP_XI_AUTO_BANDWIDTH_CALCULATION, do: 573
  def cv_CAP_PROP_XI_FFS_FILE_ID, do: 594
  def cv_CAP_PROP_XI_FFS_FILE_SIZE, do: 580
  def cv_CAP_PROP_XI_FREE_FFS_SIZE, do: 581
  def cv_CAP_PROP_XI_USED_FFS_SIZE, do: 582
  def cv_CAP_PROP_XI_FFS_ACCESS_KEY, do: 583
  def cv_CAP_PROP_XI_SENSOR_FEATURE_SELECTOR, do: 585
  def cv_CAP_PROP_XI_SENSOR_FEATURE_VALUE, do: 586
  def cv_CAP_PROP_ARAVIS_AUTOTRIGGER, do: 600
  def cv_CAP_PROP_IOS_DEVICE_FOCUS, do: 9001
  def cv_CAP_PROP_IOS_DEVICE_EXPOSURE, do: 9002
  def cv_CAP_PROP_IOS_DEVICE_FLASH, do: 9003
  def cv_CAP_PROP_IOS_DEVICE_WHITEBALANCE, do: 9004
  def cv_CAP_PROP_IOS_DEVICE_TORCH, do: 9005
  def cv_CAP_PROP_GIGA_FRAME_OFFSET_X, do: 10001
  def cv_CAP_PROP_GIGA_FRAME_OFFSET_Y, do: 10002
  def cv_CAP_PROP_GIGA_FRAME_WIDTH_MAX, do: 10003
  def cv_CAP_PROP_GIGA_FRAME_HEIGH_MAX, do: 10004
  def cv_CAP_PROP_GIGA_FRAME_SENS_WIDTH, do: 10005
  def cv_CAP_PROP_GIGA_FRAME_SENS_HEIGH, do: 10006
  def cv_CAP_PROP_INTELPERC_PROFILE_COUNT, do: 11001
  def cv_CAP_PROP_INTELPERC_PROFILE_IDX, do: 11002
  def cv_CAP_PROP_INTELPERC_DEPTH_LOW_CONFIDENCE_VALUE, do: 11003
  def cv_CAP_PROP_INTELPERC_DEPTH_SATURATION_VALUE, do: 11004
  def cv_CAP_PROP_INTELPERC_DEPTH_CONFIDENCE_THRESHOLD, do: 11005
  def cv_CAP_PROP_INTELPERC_DEPTH_FOCAL_LENGTH_HORZ, do: 11006
  def cv_CAP_PROP_INTELPERC_DEPTH_FOCAL_LENGTH_VERT, do: 11007
  def cv_CAP_INTELPERC_DEPTH_GENERATOR, do: bsl(1, 29)
  def cv_CAP_INTELPERC_IMAGE_GENERATOR, do: bsl(1, 28)
  def cv_CAP_INTELPERC_IR_GENERATOR, do: bsl(1, 27)
  def cv_CAP_INTELPERC_GENERATORS_MASK, do: ((cv_CAP_INTELPERC_DEPTH_GENERATOR() + cv_CAP_INTELPERC_IMAGE_GENERATOR()) + cv_CAP_INTELPERC_IR_GENERATOR())
  def cv_CAP_INTELPERC_DEPTH_MAP, do: 0
  def cv_CAP_INTELPERC_UVDEPTH_MAP, do: 1
  def cv_CAP_INTELPERC_IR_MAP, do: 2
  def cv_CAP_INTELPERC_IMAGE, do: 3
  def cv_CAP_PROP_GPHOTO2_PREVIEW, do: 17001
  def cv_CAP_PROP_GPHOTO2_WIDGET_ENUMERATE, do: 17002
  def cv_CAP_PROP_GPHOTO2_RELOAD_CONFIG, do: 17003
  def cv_CAP_PROP_GPHOTO2_RELOAD_ON_CHANGE, do: 17004
  def cv_CAP_PROP_GPHOTO2_COLLECT_MSGS, do: 17005
  def cv_CAP_PROP_GPHOTO2_FLUSH_MSGS, do: 17006
  def cv_CAP_PROP_SPEED, do: 17007
  def cv_CAP_PROP_APERTURE, do: 17008
  def cv_CAP_PROP_EXPOSUREPROGRAM, do: 17009
  def cv_CAP_PROP_VIEWFINDER, do: 17010
  def cv_CAP_PROP_IMAGES_BASE, do: 18000
  def cv_CAP_PROP_IMAGES_LAST, do: 19000
  def cv_CAP_OBSENSOR_DEPTH_MAP, do: 0
  def cv_CAP_OBSENSOR_BGR_IMAGE, do: 1
  def cv_CAP_OBSENSOR_IR_IMAGE, do: 2
  def cv_CAP_OBSENSOR_DEPTH_GENERATOR, do: bsl(1, 29)
  def cv_CAP_OBSENSOR_IMAGE_GENERATOR, do: bsl(1, 28)
  def cv_CAP_OBSENSOR_IR_GENERATOR, do: bsl(1, 27)
  def cv_CAP_OBSENSOR_GENERATORS_MASK, do: ((cv_CAP_OBSENSOR_DEPTH_GENERATOR() + cv_CAP_OBSENSOR_IMAGE_GENERATOR()) + cv_CAP_OBSENSOR_IR_GENERATOR())
  def cv_CAP_PROP_OBSENSOR_INTRINSIC_FX, do: 26001
  def cv_CAP_PROP_OBSENSOR_INTRINSIC_FY, do: 26002
  def cv_CAP_PROP_OBSENSOR_INTRINSIC_CX, do: 26003
  def cv_CAP_PROP_OBSENSOR_INTRINSIC_CY, do: 26004
  def cv_HAAR, do: 0
  def cv_BM3D_STEPALL, do: 0
  def cv_BM3D_STEP1, do: 1
  def cv_BM3D_STEP2, do: 2
  def cv_INPAINT_SHIFTMAP, do: 0
  def cv_INPAINT_FSR_BEST, do: 1
  def cv_INPAINT_FSR_FAST, do: 2
  def cv_LMEDS, do: 4
  def cv_RANSAC, do: 8
  def cv_RHO, do: 16
  def cv_USAC_DEFAULT, do: 32
  def cv_USAC_PARALLEL, do: 33
  def cv_USAC_FM_8PTS, do: 34
  def cv_USAC_FAST, do: 35
  def cv_USAC_ACCURATE, do: 36
  def cv_USAC_PROSAC, do: 37
  def cv_USAC_MAGSAC, do: 38
  def cv_SOLVEPNP_ITERATIVE, do: 0
  def cv_SOLVEPNP_EPNP, do: 1
  def cv_SOLVEPNP_P3P, do: 2
  def cv_SOLVEPNP_DLS, do: 3
  def cv_SOLVEPNP_UPNP, do: 4
  def cv_SOLVEPNP_AP3P, do: 5
  def cv_SOLVEPNP_IPPE, do: 6
  def cv_SOLVEPNP_IPPE_SQUARE, do: 7
  def cv_SOLVEPNP_SQPNP, do: 8
  def cv_SOLVEPNP_MAX_COUNT, do: (8 + 1)
  def cv_CALIB_CB_ADAPTIVE_THRESH, do: 1
  def cv_CALIB_CB_NORMALIZE_IMAGE, do: 2
  def cv_CALIB_CB_FILTER_QUADS, do: 4
  def cv_CALIB_CB_FAST_CHECK, do: 8
  def cv_CALIB_CB_EXHAUSTIVE, do: 16
  def cv_CALIB_CB_ACCURACY, do: 32
  def cv_CALIB_CB_LARGER, do: 64
  def cv_CALIB_CB_MARKER, do: 128
  def cv_CALIB_CB_PLAIN, do: 256
  def cv_CALIB_CB_SYMMETRIC_GRID, do: 1
  def cv_CALIB_CB_ASYMMETRIC_GRID, do: 2
  def cv_CALIB_CB_CLUSTERING, do: 4
  def cv_CALIB_NINTRINSIC, do: 18
  def cv_CALIB_USE_INTRINSIC_GUESS, do: 1
  def cv_CALIB_FIX_ASPECT_RATIO, do: 2
  def cv_CALIB_FIX_PRINCIPAL_POINT, do: 4
  def cv_CALIB_ZERO_TANGENT_DIST, do: 8
  def cv_CALIB_FIX_FOCAL_LENGTH, do: 16
  def cv_CALIB_FIX_K1, do: 32
  def cv_CALIB_FIX_K2, do: 64
  def cv_CALIB_FIX_K3, do: 128
  def cv_CALIB_FIX_K4, do: 2048
  def cv_CALIB_FIX_K5, do: 4096
  def cv_CALIB_FIX_K6, do: 8192
  def cv_CALIB_RATIONAL_MODEL, do: 16384
  def cv_CALIB_THIN_PRISM_MODEL, do: 32768
  def cv_CALIB_FIX_S1_S2_S3_S4, do: 65536
  def cv_CALIB_TILTED_MODEL, do: 262144
  def cv_CALIB_FIX_TAUX_TAUY, do: 524288
  def cv_CALIB_USE_QR, do: 1048576
  def cv_CALIB_FIX_TANGENT_DIST, do: 2097152
  def cv_CALIB_FIX_INTRINSIC, do: 256
  def cv_CALIB_SAME_FOCAL_LENGTH, do: 512
  def cv_CALIB_ZERO_DISPARITY, do: 1024
  def cv_CALIB_USE_LU, do: bsl(1, 17)
  def cv_CALIB_USE_EXTRINSIC_GUESS, do: bsl(1, 22)
  def cv_FM_7POINT, do: 1
  def cv_FM_8POINT, do: 2
  def cv_FM_LMEDS, do: 4
  def cv_FM_RANSAC, do: 8
  def cv_CALIB_HAND_EYE_TSAI, do: 0
  def cv_CALIB_HAND_EYE_PARK, do: 1
  def cv_CALIB_HAND_EYE_HORAUD, do: 2
  def cv_CALIB_HAND_EYE_ANDREFF, do: 3
  def cv_CALIB_HAND_EYE_DANIILIDIS, do: 4
  def cv_CALIB_ROBOT_WORLD_HAND_EYE_SHAH, do: 0
  def cv_CALIB_ROBOT_WORLD_HAND_EYE_LI, do: 1
  def cv_SAMPLING_UNIFORM, do: 0
  def cv_SAMPLING_PROGRESSIVE_NAPSAC, do: 1
  def cv_SAMPLING_NAPSAC, do: 2
  def cv_SAMPLING_PROSAC, do: 3
  def cv_LOCAL_OPTIM_NULL, do: 0
  def cv_LOCAL_OPTIM_INNER_LO, do: 1
  def cv_LOCAL_OPTIM_INNER_AND_ITER_LO, do: 2
  def cv_LOCAL_OPTIM_GC, do: 3
  def cv_LOCAL_OPTIM_SIGMA, do: 4
  def cv_SCORE_METHOD_RANSAC, do: 0
  def cv_SCORE_METHOD_MSAC, do: 1
  def cv_SCORE_METHOD_MAGSAC, do: 2
  def cv_SCORE_METHOD_LMEDS, do: 3
  def cv_NEIGH_FLANN_KNN, do: 0
  def cv_NEIGH_GRID, do: 1
  def cv_NEIGH_FLANN_RADIUS, do: 2
  def cv_NONE_POLISHER, do: 0
  def cv_LSQ_POLISHER, do: 1
  def cv_MAGSAC, do: 2
  def cv_COV_POLISHER, do: 3
  def cv_SYMMETRIC_GRID, do: 0
  def cv_ASYMMETRIC_GRID, do: 1
  def cv_DISP_SHIFT, do: 4
  def cv_DISP_SCALE, do: bsl(1, cv_DISP_SHIFT())
  def cv_PREFILTER_NORMALIZED_RESPONSE, do: 0
  def cv_PREFILTER_XSOBEL, do: 1
  def cv_MODE_SGBM, do: 0
  def cv_MODE_HH, do: 1
  def cv_MODE_SGBM_3WAY, do: 2
  def cv_MODE_HH4, do: 3
  def cv_PROJ_SPHERICAL_ORTHO, do: 0
  def cv_PROJ_SPHERICAL_EQRECT, do: 1
  def cv_fisheye_CALIB_USE_INTRINSIC_GUESS, do: bsl(1, 0)
  def cv_CALIB_RECOMPUTE_EXTRINSIC, do: bsl(1, 1)
  def cv_CALIB_CHECK_COND, do: bsl(1, 2)
  def cv_CALIB_FIX_SKEW, do: bsl(1, 3)
  def cv_fisheye_CALIB_FIX_K1, do: bsl(1, 4)
  def cv_fisheye_CALIB_FIX_K2, do: bsl(1, 5)
  def cv_fisheye_CALIB_FIX_K3, do: bsl(1, 6)
  def cv_fisheye_CALIB_FIX_K4, do: bsl(1, 7)
  def cv_fisheye_CALIB_FIX_INTRINSIC, do: bsl(1, 8)
  def cv_fisheye_CALIB_FIX_PRINCIPAL_POINT, do: bsl(1, 9)
  def cv_fisheye_CALIB_ZERO_DISPARITY, do: bsl(1, 10)
  def cv_fisheye_CALIB_FIX_FOCAL_LENGTH, do: bsl(1, 11)
  def cv_MPEG1, do: 0
  def cv_MPEG2, do: (0 + 1)
  def cv_MPEG4, do: (0 + 2)
  def cv_VC1, do: (0 + 3)
  def cv_H264, do: (0 + 4)
  def cv_JPEG, do: (0 + 5)
  def cv_H264_SVC, do: (0 + 6)
  def cv_H264_MVC, do: (0 + 7)
  def cv_HEVC, do: (0 + 8)
  def cv_VP8, do: (0 + 9)
  def cv_VP9, do: (0 + 10)
  def cv_AV1, do: (0 + 11)
  def cv_NumCodecs, do: (0 + 12)
  def cv_Uncompressed_YUV420, do: bor(bor(bor(bsl(73, 24), bsl(89, 16)), bsl(85, 8)), 86)
  def cv_Uncompressed_YV12, do: bor(bor(bor(bsl(89, 24), bsl(86, 16)), bsl(1, 8)), 2)
  def cv_Uncompressed_NV12, do: bor(bor(bor(bsl(78, 24), bsl(86, 16)), bsl(1, 8)), 2)
  def cv_Uncompressed_YUYV, do: bor(bor(bor(bsl(89, 24), bsl(85, 16)), bsl(89, 8)), 86)
  def cv_Uncompressed_UYVY, do: bor(bor(bor(bsl(85, 24), bsl(89, 16)), bsl(86, 8)), 89)
  def cv_BGRA, do: 1
  def cv_BGR, do: 2
  def cv_GRAY, do: 3
  def cv_NV_NV12, do: 4
  def cv_ColorFormat_RGB, do: 5
  def cv_ColorFormat_RGBA, do: 6
  def cv_NV_YV12, do: 8
  def cv_NV_IYUV, do: 9
  def cv_NV_YUV444, do: 10
  def cv_NV_AYUV, do: 11
  def cv_PROP_NOT_SUPPORTED, do: (11 + 1)
  def cv_ENC_PARAMS_RC_CONSTQP, do: 0
  def cv_ENC_PARAMS_RC_VBR, do: 1
  def cv_ENC_PARAMS_RC_CBR, do: 2
  def cv_ENC_MULTI_PASS_DISABLED, do: 0
  def cv_ENC_TWO_PASS_QUARTER_RESOLUTION, do: 1
  def cv_ENC_TWO_PASS_FULL_RESOLUTION, do: 2
  def cv_ENC_CODEC_PROFILE_AUTOSELECT, do: 0
  def cv_ENC_H264_PROFILE_BASELINE, do: 1
  def cv_ENC_H264_PROFILE_MAIN, do: 2
  def cv_ENC_H264_PROFILE_HIGH, do: 3
  def cv_ENC_H264_PROFILE_HIGH_444, do: 4
  def cv_ENC_H264_PROFILE_STEREO, do: 5
  def cv_ENC_H264_PROFILE_PROGRESSIVE_HIGH, do: 6
  def cv_ENC_H264_PROFILE_CONSTRAINED_HIGH, do: 7
  def cv_ENC_HEVC_PROFILE_MAIN, do: 8
  def cv_ENC_HEVC_PROFILE_MAIN10, do: 9
  def cv_ENC_HEVC_PROFILE_FREXT, do: 10
  def cv_ENC_PRESET_P1, do: 1
  def cv_ENC_PRESET_P2, do: 2
  def cv_ENC_PRESET_P3, do: 3
  def cv_ENC_PRESET_P4, do: 4
  def cv_ENC_PRESET_P5, do: 5
  def cv_ENC_PRESET_P6, do: 6
  def cv_ENC_PRESET_P7, do: 7
  def cv_ENC_TUNING_INFO_UNDEFINED, do: 0
  def cv_ENC_TUNING_INFO_HIGH_QUALITY, do: 1
  def cv_ENC_TUNING_INFO_LOW_LATENCY, do: 2
  def cv_ENC_TUNING_INFO_ULTRA_LOW_LATENCY, do: 3
  def cv_ENC_TUNING_INFO_LOSSLESS, do: 4
  def cv_ENC_TUNING_INFO_COUNT, do: (4 + 1)
  def cv_Monochrome, do: 0
  def cv_YUV420, do: (0 + 1)
  def cv_YUV422, do: (0 + 2)
  def cv_YUV444, do: (0 + 3)
  def cv_NumFormats, do: (0 + 4)
  def cv_Weave, do: 0
  def cv_Bob, do: 1
  def cv_Adaptive, do: 2
  def cv_PROP_DECODED_FRAME_IDX, do: 0
  def cv_PROP_EXTRA_DATA_INDEX, do: 1
  def cv_PROP_RAW_PACKAGES_BASE_INDEX, do: 2
  def cv_PROP_NUMBER_OF_RAW_PACKAGES_SINCE_LAST_GRAB, do: 3
  def cv_PROP_RAW_MODE, do: 4
  def cv_PROP_LRF_HAS_KEY_FRAME, do: 5
  def cv_PROP_COLOR_FORMAT, do: 6
  def cv_PROP_UDP_SOURCE, do: 7
  def cv_PROP_ALLOW_FRAME_DROP, do: 8
  def cv_VideoReaderProps_PROP_NOT_SUPPORTED, do: (8 + 1)
  def cv_WINDOW_NORMAL, do: 0
  def cv_WINDOW_AUTOSIZE, do: 1
  def cv_WINDOW_OPENGL, do: 4096
  def cv_WINDOW_FULLSCREEN, do: 1
  def cv_WINDOW_FREERATIO, do: 256
  def cv_WINDOW_KEEPRATIO, do: 0
  def cv_WINDOW_GUI_EXPANDED, do: 0
  def cv_WINDOW_GUI_NORMAL, do: 16
  def cv_WND_PROP_FULLSCREEN, do: 0
  def cv_WND_PROP_AUTOSIZE, do: 1
  def cv_WND_PROP_ASPECT_RATIO, do: 2
  def cv_WND_PROP_OPENGL, do: 3
  def cv_WND_PROP_VISIBLE, do: 4
  def cv_WND_PROP_TOPMOST, do: 5
  def cv_WND_PROP_VSYNC, do: 6
  def cv_EVENT_MOUSEMOVE, do: 0
  def cv_EVENT_LBUTTONDOWN, do: 1
  def cv_EVENT_RBUTTONDOWN, do: 2
  def cv_EVENT_MBUTTONDOWN, do: 3
  def cv_EVENT_LBUTTONUP, do: 4
  def cv_EVENT_RBUTTONUP, do: 5
  def cv_EVENT_MBUTTONUP, do: 6
  def cv_EVENT_LBUTTONDBLCLK, do: 7
  def cv_EVENT_RBUTTONDBLCLK, do: 8
  def cv_EVENT_MBUTTONDBLCLK, do: 9
  def cv_EVENT_MOUSEWHEEL, do: 10
  def cv_EVENT_MOUSEHWHEEL, do: 11
  def cv_EVENT_FLAG_LBUTTON, do: 1
  def cv_EVENT_FLAG_RBUTTON, do: 2
  def cv_EVENT_FLAG_MBUTTON, do: 4
  def cv_EVENT_FLAG_CTRLKEY, do: 8
  def cv_EVENT_FLAG_SHIFTKEY, do: 16
  def cv_EVENT_FLAG_ALTKEY, do: 32
  def cv_QT_FONT_LIGHT, do: 25
  def cv_QT_FONT_NORMAL, do: 50
  def cv_QT_FONT_DEMIBOLD, do: 63
  def cv_QT_FONT_BOLD, do: 75
  def cv_QT_FONT_BLACK, do: 87
  def cv_QT_STYLE_NORMAL, do: 0
  def cv_QT_STYLE_ITALIC, do: 1
  def cv_QT_STYLE_OBLIQUE, do: 2
  def cv_QT_PUSH_BUTTON, do: 0
  def cv_QT_CHECKBOX, do: 1
  def cv_QT_RADIOBOX, do: 2
  def cv_QT_NEW_BUTTONBAR, do: 1024
  def cv_CCM_3x3, do: 0
  def cv_CCM_4x3, do: 1
  def cv_INITIAL_METHOD_WHITE_BALANCE, do: 0
  def cv_INITIAL_METHOD_LEAST_SQUARE, do: 1
  def cv_COLORCHECKER_Macbeth, do: 0
  def cv_COLORCHECKER_Vinyl, do: 1
  def cv_COLORCHECKER_DigitalSG, do: 2
  def cv_COLOR_SPACE_sRGB, do: 0
  def cv_COLOR_SPACE_sRGBL, do: 1
  def cv_COLOR_SPACE_AdobeRGB, do: 2
  def cv_COLOR_SPACE_AdobeRGBL, do: 3
  def cv_COLOR_SPACE_WideGamutRGB, do: 4
  def cv_COLOR_SPACE_WideGamutRGBL, do: 5
  def cv_COLOR_SPACE_ProPhotoRGB, do: 6
  def cv_COLOR_SPACE_ProPhotoRGBL, do: 7
  def cv_COLOR_SPACE_DCI_P3_RGB, do: 8
  def cv_COLOR_SPACE_DCI_P3_RGBL, do: 9
  def cv_COLOR_SPACE_AppleRGB, do: 10
  def cv_COLOR_SPACE_AppleRGBL, do: 11
  def cv_COLOR_SPACE_REC_709_RGB, do: 12
  def cv_COLOR_SPACE_REC_709_RGBL, do: 13
  def cv_COLOR_SPACE_REC_2020_RGB, do: 14
  def cv_COLOR_SPACE_REC_2020_RGBL, do: 15
  def cv_COLOR_SPACE_XYZ_D65_2, do: 16
  def cv_COLOR_SPACE_XYZ_D65_10, do: 17
  def cv_COLOR_SPACE_XYZ_D50_2, do: 18
  def cv_COLOR_SPACE_XYZ_D50_10, do: 19
  def cv_COLOR_SPACE_XYZ_A_2, do: 20
  def cv_COLOR_SPACE_XYZ_A_10, do: 21
  def cv_COLOR_SPACE_XYZ_D55_2, do: 22
  def cv_COLOR_SPACE_XYZ_D55_10, do: 23
  def cv_COLOR_SPACE_XYZ_D75_2, do: 24
  def cv_COLOR_SPACE_XYZ_D75_10, do: 25
  def cv_COLOR_SPACE_XYZ_E_2, do: 26
  def cv_COLOR_SPACE_XYZ_E_10, do: 27
  def cv_COLOR_SPACE_Lab_D65_2, do: 28
  def cv_COLOR_SPACE_Lab_D65_10, do: 29
  def cv_COLOR_SPACE_Lab_D50_2, do: 30
  def cv_COLOR_SPACE_Lab_D50_10, do: 31
  def cv_COLOR_SPACE_Lab_A_2, do: 32
  def cv_COLOR_SPACE_Lab_A_10, do: 33
  def cv_COLOR_SPACE_Lab_D55_2, do: 34
  def cv_COLOR_SPACE_Lab_D55_10, do: 35
  def cv_COLOR_SPACE_Lab_D75_2, do: 36
  def cv_COLOR_SPACE_Lab_D75_10, do: 37
  def cv_COLOR_SPACE_Lab_E_2, do: 38
  def cv_COLOR_SPACE_Lab_E_10, do: 39
  def cv_LINEARIZATION_IDENTITY, do: 0
  def cv_LINEARIZATION_GAMMA, do: 1
  def cv_LINEARIZATION_COLORPOLYFIT, do: 2
  def cv_LINEARIZATION_COLORLOGPOLYFIT, do: 3
  def cv_LINEARIZATION_GRAYPOLYFIT, do: 4
  def cv_LINEARIZATION_GRAYLOGPOLYFIT, do: 5
  def cv_DISTANCE_CIE76, do: 0
  def cv_DISTANCE_CIE94_GRAPHIC_ARTS, do: 1
  def cv_DISTANCE_CIE94_TEXTILES, do: 2
  def cv_DISTANCE_CIE2000, do: 3
  def cv_DISTANCE_CMC_1TO1, do: 4
  def cv_DISTANCE_CMC_2TO1, do: 5
  def cv_DISTANCE_RGB, do: 6
  def cv_DISTANCE_RGBL, do: 7
  def cv_MCC24, do: 0
  def cv_SG140, do: (0 + 1)
  def cv_VINYL18, do: (0 + 2)
  def cv_CASCADE_DO_CANNY_PRUNING, do: 1
  def cv_CASCADE_SCALE_IMAGE, do: 2
  def cv_CASCADE_FIND_BIGGEST_OBJECT, do: 4
  def cv_CASCADE_DO_ROUGH_SEARCH, do: 8
  def cv_L2Hys, do: 0
  def cv_DEFAULT_NLEVELS, do: 64
  def cv_DESCR_FORMAT_COL_BY_COL, do: 0
  def cv_DESCR_FORMAT_ROW_BY_ROW, do: 1
  def cv_MODE_AUTO, do: -1
  def cv_MODE_NUMERIC, do: 1
  def cv_MODE_ALPHANUMERIC, do: 2
  def cv_MODE_BYTE, do: 4
  def cv_MODE_ECI, do: 7
  def cv_MODE_KANJI, do: 8
  def cv_MODE_STRUCTURED_APPEND, do: 3
  def cv_CORRECT_LEVEL_L, do: 0
  def cv_CORRECT_LEVEL_M, do: 1
  def cv_CORRECT_LEVEL_Q, do: 2
  def cv_CORRECT_LEVEL_H, do: 3
  def cv_ECI_UTF8, do: 26
  def cv_CORNER_REFINE_NONE, do: 0
  def cv_CORNER_REFINE_SUBPIX, do: 1
  def cv_CORNER_REFINE_CONTOUR, do: 2
  def cv_CORNER_REFINE_APRILTAG, do: 3
  def cv_DICT_4X4_50, do: 0
  def cv_DICT_4X4_100, do: (0 + 1)
  def cv_DICT_4X4_250, do: (0 + 2)
  def cv_DICT_4X4_1000, do: (0 + 3)
  def cv_DICT_5X5_50, do: (0 + 4)
  def cv_DICT_5X5_100, do: (0 + 5)
  def cv_DICT_5X5_250, do: (0 + 6)
  def cv_DICT_5X5_1000, do: (0 + 7)
  def cv_DICT_6X6_50, do: (0 + 8)
  def cv_DICT_6X6_100, do: (0 + 9)
  def cv_DICT_6X6_250, do: (0 + 10)
  def cv_DICT_6X6_1000, do: (0 + 11)
  def cv_DICT_7X7_50, do: (0 + 12)
  def cv_DICT_7X7_100, do: (0 + 13)
  def cv_DICT_7X7_250, do: (0 + 14)
  def cv_DICT_7X7_1000, do: (0 + 15)
  def cv_DICT_ARUCO_ORIGINAL, do: (0 + 16)
  def cv_DICT_APRILTAG_16h5, do: (0 + 17)
  def cv_DICT_APRILTAG_25h9, do: (0 + 18)
  def cv_DICT_APRILTAG_36h10, do: (0 + 19)
  def cv_DICT_APRILTAG_36h11, do: (0 + 20)
  def cv_DICT_ARUCO_MIP_36h12, do: (0 + 21)
  def cv_FR_COSINE, do: 0
  def cv_FR_NORM_L2, do: 1
  def cv_RGBD_NORMALS_METHOD_FALS, do: 0
  def cv_RGBD_NORMALS_METHOD_LINEMOD, do: 1
  def cv_RGBD_NORMALS_METHOD_SRI, do: 2
  def cv_DEPTH_CLEANER_NIL, do: 0
  def cv_RGBD_PLANE_METHOD_DEFAULT, do: 0
  def cv_CACHE_SRC, do: 1
  def cv_CACHE_DST, do: 2
  def cv_CACHE_ALL, do: (cv_CACHE_SRC() + cv_CACHE_DST())
  def cv_ROTATION, do: 1
  def cv_TRANSLATION, do: 2
  def cv_RIGID_BODY_MOTION, do: 4
  def cv_TSDF, do: 0
  def cv_HASHTSDF, do: 1
  def cv_COLOREDTSDF, do: 2
  def cv_FTP, do: 0
  def cv_PSP, do: 1
  def cv_FAPS, do: 2
  def cv_DECODE_3D_UNDERWORLD, do: 0
  def cv_MODE_INIT_POS, do: 1
  def cv_MODE_INIT_NEG, do: 2
  def cv_MODE_TRACK_POS, do: 3
  def cv_MODE_TRACK_NEG, do: 4
  def cv_MODE_DETECT, do: 5
  def cv_OPTFLOW_USE_INITIAL_FLOW, do: 4
  def cv_OPTFLOW_LK_GET_MIN_EIGENVALS, do: 8
  def cv_OPTFLOW_FARNEBACK_GAUSSIAN, do: 256
  def cv_MOTION_TRANSLATION, do: 0
  def cv_MOTION_EUCLIDEAN, do: 1
  def cv_MOTION_AFFINE, do: 2
  def cv_MOTION_HOMOGRAPHY, do: 3
  def cv_PRESET_ULTRAFAST, do: 0
  def cv_PRESET_FAST, do: 1
  def cv_PRESET_MEDIUM, do: 2
  def cv_SIZE_512_BITS, do: 100
  def cv_SIZE_256_BITS, do: 101
  def cv_TEBLID_SIZE_256_BITS, do: 102
  def cv_TEBLID_SIZE_512_BITS, do: 103
  def cv_NRM_NONE, do: 100
  def cv_NRM_PARTIAL, do: 101
  def cv_NRM_FULL, do: 102
  def cv_NRM_SIFT, do: 103
  def cv_L0_25, do: 0
  def cv_L0_5, do: 1
  def cv_L1, do: 2
  def cv_L2, do: 3
  def cv_L2SQUARED, do: 4
  def cv_L5, do: 5
  def cv_L_INFINITY, do: 6
  def cv_REGULAR, do: 1
  def cv_PCTSignatures_NORMAL, do: 2
  def cv_MINUS, do: 0
  def cv_PCTSignatures_GAUSSIAN, do: 1
  def cv_HEURISTIC, do: 2
  def cv_X_ROW, do: 0
  def cv_Y_ROW, do: (0 + 1)
  def cv_LAPLACIAN_ROW, do: (0 + 2)
  def cv_OCTAVE_ROW, do: (0 + 3)
  def cv_SIZE_ROW, do: (0 + 4)
  def cv_ANGLE_ROW, do: (0 + 5)
  def cv_HESSIAN_ROW, do: (0 + 6)
  def cv_ROWS_COUNT, do: (0 + 7)
  def cv_THINNING_ZHANGSUEN, do: 0
  def cv_THINNING_GUOHALL, do: 1
  def cv_BINARIZATION_NIBLACK, do: 0
  def cv_BINARIZATION_SAUVOLA, do: 1
  def cv_BINARIZATION_WOLF, do: 2
  def cv_BINARIZATION_NICK, do: 3
  def cv_PREWITT, do: 0
  def cv_SOBEL, do: 1
  def cv_SCHARR, do: 2
  def cv_LSD, do: 3
  def cv_DTF_NC, do: 0
  def cv_DTF_IC, do: 1
  def cv_DTF_RF, do: 2
  def cv_GUIDED_FILTER, do: 3
  def cv_AM_FILTER, do: 4
  def cv_ARO_0_45, do: 0
  def cv_ARO_45_90, do: 1
  def cv_ARO_90_135, do: 2
  def cv_ARO_315_0, do: 3
  def cv_ARO_315_45, do: 4
  def cv_ARO_45_135, do: 5
  def cv_ARO_315_135, do: 6
  def cv_ARO_CTR_HOR, do: 7
  def cv_ARO_CTR_VER, do: 8
  def cv_FHT_MIN, do: 0
  def cv_FHT_MAX, do: 1
  def cv_FHT_ADD, do: 2
  def cv_FHT_AVE, do: 3
  def cv_HDO_RAW, do: 0
  def cv_HDO_DESKEW, do: 1
  def cv_SLIC, do: 100
  def cv_SLICO, do: 101
  def cv_MSLIC, do: 102
  def cv_WMF_EXP, do: 1
  def cv_WMF_IV1, do: bsl(1, 1)
  def cv_WMF_IV2, do: bsl(1, 2)
  def cv_WMF_COS, do: bsl(1, 3)
  def cv_WMF_JAC, do: bsl(1, 4)
  def cv_WMF_OFF, do: bsl(1, 5)
  def cv_ARUCO_CCW_CENTER, do: 0
  def cv_ARUCO_CW_TOP_LEFT_CORNER, do: 1
  def cv_LSBP_CAMERA_MOTION_COMPENSATION_NONE, do: 0
  def cv_LSBP_CAMERA_MOTION_COMPENSATION_LK, do: (0 + 1)
  def cv_RETINA_COLOR_RANDOM, do: 0
  def cv_RETINA_COLOR_DIAGONAL, do: 1
  def cv_RETINA_COLOR_BAYER, do: 2
  def cv_PINHOLE, do: 0
  def cv_OMNIDIRECTIONAL, do: 1
  def cv_CALIB_USE_GUESS, do: 1
  def cv_omnidir_CALIB_FIX_SKEW, do: 2
  def cv_omnidir_CALIB_FIX_K1, do: 4
  def cv_omnidir_CALIB_FIX_K2, do: 8
  def cv_CALIB_FIX_P1, do: 16
  def cv_CALIB_FIX_P2, do: 32
  def cv_CALIB_FIX_XI, do: 64
  def cv_CALIB_FIX_GAMMA, do: 128
  def cv_CALIB_FIX_CENTER, do: 256
  def cv_RECTIFY_PERSPECTIVE, do: 1
  def cv_RECTIFY_CYLINDRICAL, do: 2
  def cv_RECTIFY_LONGLATI, do: 3
  def cv_RECTIFY_STEREOGRAPHIC, do: 4
  def cv_XYZRGB, do: 1
  def cv_XYZ, do: 2
  def cv_OK, do: 0
  def cv_ERR_NEED_MORE_IMGS, do: 1
  def cv_ERR_HOMOGRAPHY_EST_FAIL, do: 2
  def cv_ERR_CAMERA_PARAMS_ADJUST_FAIL, do: 3
  def cv_PANORAMA, do: 0
  def cv_SCANS, do: 1
  def cv_NO, do: 0
  def cv_FEATHER, do: 1
  def cv_MULTI_BAND, do: 2
  def cv_GAIN, do: 1
  def cv_GAIN_BLOCKS, do: 2
  def cv_CHANNELS, do: 3
  def cv_CHANNELS_BLOCKS, do: 4
  def cv_WAVE_CORRECT_HORIZ, do: 0
  def cv_WAVE_CORRECT_VERT, do: 1
  def cv_WAVE_CORRECT_AUTO, do: 2
  def cv_VORONOI_SEAM, do: 1
  def cv_DP_SEAM, do: 2
  def cv_COLOR, do: 0
  def cv_COLOR_GRAD, do: 1
  def cv_COST_COLOR, do: 0
  def cv_COST_COLOR_GRAD, do: 1
  def cv_AS_IS, do: 0
  def cv_CROP, do: 1
  def cv_TrackerKCF_GRAY, do: bsl(1, 0)
  def cv_CN, do: bsl(1, 1)
  def cv_TrackerKCF_CUSTOM, do: bsl(1, 2)
  def cv_LBP, do: 1
  def cv_HOG, do: 2
  def cv_MODE_POSITIVE, do: 1
  def cv_MODE_NEGATIVE, do: 2
  def cv_MODE_CLASSIFY, do: 3
  def cv_CV_SPECKLE_REMOVAL_ALGORITHM, do: 0
  def cv_CV_SPECKLE_REMOVAL_AVG_ALGORITHM, do: 1
  def cv_CV_QUADRATIC_INTERPOLATION, do: 0
  def cv_CV_SIMETRICV_INTERPOLATION, do: 1
  def cv_CV_DENSE_CENSUS, do: 0
  def cv_CV_SPARSE_CENSUS, do: 1
  def cv_CV_CS_CENSUS, do: 2
  def cv_CV_MODIFIED_CS_CENSUS, do: 3
  def cv_CV_MODIFIED_CENSUS_TRANSFORM, do: 4
  def cv_CV_MEAN_VARIATION, do: 5
  def cv_CV_STAR_KERNEL, do: 6

end
