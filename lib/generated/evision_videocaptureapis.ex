defmodule Evision.VideoCaptureAPIs do
  @type enum :: integer()
  @doc enum: true
  def cv_CAP_ANY, do: 0
  @doc enum: true
  def cv_CAP_VFW, do: 200
  @doc enum: true
  def cv_CAP_V4L, do: 200
  @doc enum: true
  def cv_CAP_V4L2, do: cv_CAP_V4L()
  @doc enum: true
  def cv_CAP_FIREWIRE, do: 300
  @doc enum: true
  def cv_CAP_FIREWARE, do: cv_CAP_FIREWIRE()
  @doc enum: true
  def cv_CAP_IEEE1394, do: cv_CAP_FIREWIRE()
  @doc enum: true
  def cv_CAP_DC1394, do: cv_CAP_FIREWIRE()
  @doc enum: true
  def cv_CAP_CMU1394, do: cv_CAP_FIREWIRE()
  @doc enum: true
  def cv_CAP_QT, do: 500
  @doc enum: true
  def cv_CAP_UNICAP, do: 600
  @doc enum: true
  def cv_CAP_DSHOW, do: 700
  @doc enum: true
  def cv_CAP_PVAPI, do: 800
  @doc enum: true
  def cv_CAP_OPENNI, do: 900
  @doc enum: true
  def cv_CAP_OPENNI_ASUS, do: 910
  @doc enum: true
  def cv_CAP_ANDROID, do: 1000
  @doc enum: true
  def cv_CAP_XIAPI, do: 1100
  @doc enum: true
  def cv_CAP_AVFOUNDATION, do: 1200
  @doc enum: true
  def cv_CAP_GIGANETIX, do: 1300
  @doc enum: true
  def cv_CAP_MSMF, do: 1400
  @doc enum: true
  def cv_CAP_WINRT, do: 1410
  @doc enum: true
  def cv_CAP_INTELPERC, do: 1500
  @doc enum: true
  def cv_CAP_REALSENSE, do: 1500
  @doc enum: true
  def cv_CAP_OPENNI2, do: 1600
  @doc enum: true
  def cv_CAP_OPENNI2_ASUS, do: 1610
  @doc enum: true
  def cv_CAP_OPENNI2_ASTRA, do: 1620
  @doc enum: true
  def cv_CAP_GPHOTO2, do: 1700
  @doc enum: true
  def cv_CAP_GSTREAMER, do: 1800
  @doc enum: true
  def cv_CAP_FFMPEG, do: 1900
  @doc enum: true
  def cv_CAP_IMAGES, do: 2000
  @doc enum: true
  def cv_CAP_ARAVIS, do: 2100
  @doc enum: true
  def cv_CAP_OPENCV_MJPEG, do: 2200
  @doc enum: true
  def cv_CAP_INTEL_MFX, do: 2300
  @doc enum: true
  def cv_CAP_XINE, do: 2400
  @doc enum: true
  def cv_CAP_UEYE, do: 2500
  @doc enum: true
  def cv_CAP_OBSENSOR, do: 2600
end
