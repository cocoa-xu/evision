defmodule Evision.VideoCaptureProperties do
  @type enum :: integer()
  @doc enum: true
  def cv_CAP_PROP_POS_MSEC, do: 0
  @doc enum: true
  def cv_CAP_PROP_POS_FRAMES, do: 1
  @doc enum: true
  def cv_CAP_PROP_POS_AVI_RATIO, do: 2
  @doc enum: true
  def cv_CAP_PROP_FRAME_WIDTH, do: 3
  @doc enum: true
  def cv_CAP_PROP_FRAME_HEIGHT, do: 4
  @doc enum: true
  def cv_CAP_PROP_FPS, do: 5
  @doc enum: true
  def cv_CAP_PROP_FOURCC, do: 6
  @doc enum: true
  def cv_CAP_PROP_FRAME_COUNT, do: 7
  @doc enum: true
  def cv_CAP_PROP_FORMAT, do: 8
  @doc enum: true
  def cv_CAP_PROP_MODE, do: 9
  @doc enum: true
  def cv_CAP_PROP_BRIGHTNESS, do: 10
  @doc enum: true
  def cv_CAP_PROP_CONTRAST, do: 11
  @doc enum: true
  def cv_CAP_PROP_SATURATION, do: 12
  @doc enum: true
  def cv_CAP_PROP_HUE, do: 13
  @doc enum: true
  def cv_CAP_PROP_GAIN, do: 14
  @doc enum: true
  def cv_CAP_PROP_EXPOSURE, do: 15
  @doc enum: true
  def cv_CAP_PROP_CONVERT_RGB, do: 16
  @doc enum: true
  def cv_CAP_PROP_WHITE_BALANCE_BLUE_U, do: 17
  @doc enum: true
  def cv_CAP_PROP_RECTIFICATION, do: 18
  @doc enum: true
  def cv_CAP_PROP_MONOCHROME, do: 19
  @doc enum: true
  def cv_CAP_PROP_SHARPNESS, do: 20
  @doc enum: true
  def cv_CAP_PROP_AUTO_EXPOSURE, do: 21
  @doc enum: true
  def cv_CAP_PROP_GAMMA, do: 22
  @doc enum: true
  def cv_CAP_PROP_TEMPERATURE, do: 23
  @doc enum: true
  def cv_CAP_PROP_TRIGGER, do: 24
  @doc enum: true
  def cv_CAP_PROP_TRIGGER_DELAY, do: 25
  @doc enum: true
  def cv_CAP_PROP_WHITE_BALANCE_RED_V, do: 26
  @doc enum: true
  def cv_CAP_PROP_ZOOM, do: 27
  @doc enum: true
  def cv_CAP_PROP_FOCUS, do: 28
  @doc enum: true
  def cv_CAP_PROP_GUID, do: 29
  @doc enum: true
  def cv_CAP_PROP_ISO_SPEED, do: 30
  @doc enum: true
  def cv_CAP_PROP_BACKLIGHT, do: 32
  @doc enum: true
  def cv_CAP_PROP_PAN, do: 33
  @doc enum: true
  def cv_CAP_PROP_TILT, do: 34
  @doc enum: true
  def cv_CAP_PROP_ROLL, do: 35
  @doc enum: true
  def cv_CAP_PROP_IRIS, do: 36
  @doc enum: true
  def cv_CAP_PROP_SETTINGS, do: 37
  @doc enum: true
  def cv_CAP_PROP_BUFFERSIZE, do: 38
  @doc enum: true
  def cv_CAP_PROP_AUTOFOCUS, do: 39
  @doc enum: true
  def cv_CAP_PROP_SAR_NUM, do: 40
  @doc enum: true
  def cv_CAP_PROP_SAR_DEN, do: 41
  @doc enum: true
  def cv_CAP_PROP_BACKEND, do: 42
  @doc enum: true
  def cv_CAP_PROP_CHANNEL, do: 43
  @doc enum: true
  def cv_CAP_PROP_AUTO_WB, do: 44
  @doc enum: true
  def cv_CAP_PROP_WB_TEMPERATURE, do: 45
  @doc enum: true
  def cv_CAP_PROP_CODEC_PIXEL_FORMAT, do: 46
  @doc enum: true
  def cv_CAP_PROP_BITRATE, do: 47
  @doc enum: true
  def cv_CAP_PROP_ORIENTATION_META, do: 48
  @doc enum: true
  def cv_CAP_PROP_ORIENTATION_AUTO, do: 49
  @doc enum: true
  def cv_CAP_PROP_HW_ACCELERATION, do: 50
  @doc enum: true
  def cv_CAP_PROP_HW_DEVICE, do: 51
  @doc enum: true
  def cv_CAP_PROP_HW_ACCELERATION_USE_OPENCL, do: 52
  @doc enum: true
  def cv_CAP_PROP_OPEN_TIMEOUT_MSEC, do: 53
  @doc enum: true
  def cv_CAP_PROP_READ_TIMEOUT_MSEC, do: 54
  @doc enum: true
  def cv_CAP_PROP_STREAM_OPEN_TIME_USEC, do: 55
  @doc enum: true
  def cv_CAP_PROP_VIDEO_TOTAL_CHANNELS, do: 56
  @doc enum: true
  def cv_CAP_PROP_VIDEO_STREAM, do: 57
  @doc enum: true
  def cv_CAP_PROP_AUDIO_STREAM, do: 58
  @doc enum: true
  def cv_CAP_PROP_AUDIO_POS, do: 59
  @doc enum: true
  def cv_CAP_PROP_AUDIO_SHIFT_NSEC, do: 60
  @doc enum: true
  def cv_CAP_PROP_AUDIO_DATA_DEPTH, do: 61
  @doc enum: true
  def cv_CAP_PROP_AUDIO_SAMPLES_PER_SECOND, do: 62
  @doc enum: true
  def cv_CAP_PROP_AUDIO_BASE_INDEX, do: 63
  @doc enum: true
  def cv_CAP_PROP_AUDIO_TOTAL_CHANNELS, do: 64
  @doc enum: true
  def cv_CAP_PROP_AUDIO_TOTAL_STREAMS, do: 65
  @doc enum: true
  def cv_CAP_PROP_AUDIO_SYNCHRONIZE, do: 66
  @doc enum: true
  def cv_CAP_PROP_LRF_HAS_KEY_FRAME, do: 67
  @doc enum: true
  def cv_CAP_PROP_CODEC_EXTRADATA_INDEX, do: 68
  @doc enum: true
  def cv_CAP_PROP_FRAME_TYPE, do: 69
  @doc enum: true
  def cv_CAP_PROP_N_THREADS, do: 70
end
