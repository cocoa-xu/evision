defmodule Evision.Error.Code do
  @type enum :: integer()
  @doc enum: true
  def cv_StsOk, do: 0
  @doc enum: true
  def cv_StsBackTrace, do: -1
  @doc enum: true
  def cv_StsError, do: -2
  @doc enum: true
  def cv_StsInternal, do: -3
  @doc enum: true
  def cv_StsNoMem, do: -4
  @doc enum: true
  def cv_StsBadArg, do: -5
  @doc enum: true
  def cv_StsBadFunc, do: -6
  @doc enum: true
  def cv_StsNoConv, do: -7
  @doc enum: true
  def cv_StsAutoTrace, do: -8
  @doc enum: true
  def cv_HeaderIsNull, do: -9
  @doc enum: true
  def cv_BadImageSize, do: -10
  @doc enum: true
  def cv_BadOffset, do: -11
  @doc enum: true
  def cv_BadDataPtr, do: -12
  @doc enum: true
  def cv_BadStep, do: -13
  @doc enum: true
  def cv_BadModelOrChSeq, do: -14
  @doc enum: true
  def cv_BadNumChannels, do: -15
  @doc enum: true
  def cv_BadNumChannel1U, do: -16
  @doc enum: true
  def cv_BadDepth, do: -17
  @doc enum: true
  def cv_BadAlphaChannel, do: -18
  @doc enum: true
  def cv_BadOrder, do: -19
  @doc enum: true
  def cv_BadOrigin, do: -20
  @doc enum: true
  def cv_BadAlign, do: -21
  @doc enum: true
  def cv_BadCallBack, do: -22
  @doc enum: true
  def cv_BadTileSize, do: -23
  @doc enum: true
  def cv_BadCOI, do: -24
  @doc enum: true
  def cv_BadROISize, do: -25
  @doc enum: true
  def cv_MaskIsTiled, do: -26
  @doc enum: true
  def cv_StsNullPtr, do: -27
  @doc enum: true
  def cv_StsVecLengthErr, do: -28
  @doc enum: true
  def cv_StsFilterStructContentErr, do: -29
  @doc enum: true
  def cv_StsKernelStructContentErr, do: -30
  @doc enum: true
  def cv_StsFilterOffsetErr, do: -31
  @doc enum: true
  def cv_StsBadSize, do: -201
  @doc enum: true
  def cv_StsDivByZero, do: -202
  @doc enum: true
  def cv_StsInplaceNotSupported, do: -203
  @doc enum: true
  def cv_StsObjectNotFound, do: -204
  @doc enum: true
  def cv_StsUnmatchedFormats, do: -205
  @doc enum: true
  def cv_StsBadFlag, do: -206
  @doc enum: true
  def cv_StsBadPoint, do: -207
  @doc enum: true
  def cv_StsBadMask, do: -208
  @doc enum: true
  def cv_StsUnmatchedSizes, do: -209
  @doc enum: true
  def cv_StsUnsupportedFormat, do: -210
  @doc enum: true
  def cv_StsOutOfRange, do: -211
  @doc enum: true
  def cv_StsParseError, do: -212
  @doc enum: true
  def cv_StsNotImplemented, do: -213
  @doc enum: true
  def cv_StsBadMemBlock, do: -214
  @doc enum: true
  def cv_StsAssert, do: -215
  @doc enum: true
  def cv_GpuNotSupported, do: -216
  @doc enum: true
  def cv_GpuApiCallError, do: -217
  @doc enum: true
  def cv_OpenGlNotSupported, do: -218
  @doc enum: true
  def cv_OpenGlApiCallError, do: -219
  @doc enum: true
  def cv_OpenCLApiCallError, do: -220
  @doc enum: true
  def cv_OpenCLDoubleNotSupported, do: -221
  @doc enum: true
  def cv_OpenCLInitError, do: -222
  @doc enum: true
  def cv_OpenCLNoAMDBlasFft, do: -223
end
