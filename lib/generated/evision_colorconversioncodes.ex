defmodule Evision.ColorConversionCodes do
  @type enum :: integer()
  @doc enum: true
  def cv_COLOR_BGR2BGRA, do: 0
  @doc enum: true
  def cv_COLOR_RGB2RGBA, do: cv_COLOR_BGR2BGRA()
  @doc enum: true
  def cv_COLOR_BGRA2BGR, do: 1
  @doc enum: true
  def cv_COLOR_RGBA2RGB, do: cv_COLOR_BGRA2BGR()
  @doc enum: true
  def cv_COLOR_BGR2RGBA, do: 2
  @doc enum: true
  def cv_COLOR_RGB2BGRA, do: cv_COLOR_BGR2RGBA()
  @doc enum: true
  def cv_COLOR_RGBA2BGR, do: 3
  @doc enum: true
  def cv_COLOR_BGRA2RGB, do: cv_COLOR_RGBA2BGR()
  @doc enum: true
  def cv_COLOR_BGR2RGB, do: 4
  @doc enum: true
  def cv_COLOR_RGB2BGR, do: cv_COLOR_BGR2RGB()
  @doc enum: true
  def cv_COLOR_BGRA2RGBA, do: 5
  @doc enum: true
  def cv_COLOR_RGBA2BGRA, do: cv_COLOR_BGRA2RGBA()
  @doc enum: true
  def cv_COLOR_BGR2GRAY, do: 6
  @doc enum: true
  def cv_COLOR_RGB2GRAY, do: 7
  @doc enum: true
  def cv_COLOR_GRAY2BGR, do: 8
  @doc enum: true
  def cv_COLOR_GRAY2RGB, do: cv_COLOR_GRAY2BGR()
  @doc enum: true
  def cv_COLOR_GRAY2BGRA, do: 9
  @doc enum: true
  def cv_COLOR_GRAY2RGBA, do: cv_COLOR_GRAY2BGRA()
  @doc enum: true
  def cv_COLOR_BGRA2GRAY, do: 10
  @doc enum: true
  def cv_COLOR_RGBA2GRAY, do: 11
  @doc enum: true
  def cv_COLOR_BGR2BGR565, do: 12
  @doc enum: true
  def cv_COLOR_RGB2BGR565, do: 13
  @doc enum: true
  def cv_COLOR_BGR5652BGR, do: 14
  @doc enum: true
  def cv_COLOR_BGR5652RGB, do: 15
  @doc enum: true
  def cv_COLOR_BGRA2BGR565, do: 16
  @doc enum: true
  def cv_COLOR_RGBA2BGR565, do: 17
  @doc enum: true
  def cv_COLOR_BGR5652BGRA, do: 18
  @doc enum: true
  def cv_COLOR_BGR5652RGBA, do: 19
  @doc enum: true
  def cv_COLOR_GRAY2BGR565, do: 20
  @doc enum: true
  def cv_COLOR_BGR5652GRAY, do: 21
  @doc enum: true
  def cv_COLOR_BGR2BGR555, do: 22
  @doc enum: true
  def cv_COLOR_RGB2BGR555, do: 23
  @doc enum: true
  def cv_COLOR_BGR5552BGR, do: 24
  @doc enum: true
  def cv_COLOR_BGR5552RGB, do: 25
  @doc enum: true
  def cv_COLOR_BGRA2BGR555, do: 26
  @doc enum: true
  def cv_COLOR_RGBA2BGR555, do: 27
  @doc enum: true
  def cv_COLOR_BGR5552BGRA, do: 28
  @doc enum: true
  def cv_COLOR_BGR5552RGBA, do: 29
  @doc enum: true
  def cv_COLOR_GRAY2BGR555, do: 30
  @doc enum: true
  def cv_COLOR_BGR5552GRAY, do: 31
  @doc enum: true
  def cv_COLOR_BGR2XYZ, do: 32
  @doc enum: true
  def cv_COLOR_RGB2XYZ, do: 33
  @doc enum: true
  def cv_COLOR_XYZ2BGR, do: 34
  @doc enum: true
  def cv_COLOR_XYZ2RGB, do: 35
  @doc enum: true
  def cv_COLOR_BGR2YCrCb, do: 36
  @doc enum: true
  def cv_COLOR_RGB2YCrCb, do: 37
  @doc enum: true
  def cv_COLOR_YCrCb2BGR, do: 38
  @doc enum: true
  def cv_COLOR_YCrCb2RGB, do: 39
  @doc enum: true
  def cv_COLOR_BGR2HSV, do: 40
  @doc enum: true
  def cv_COLOR_RGB2HSV, do: 41
  @doc enum: true
  def cv_COLOR_BGR2Lab, do: 44
  @doc enum: true
  def cv_COLOR_RGB2Lab, do: 45
  @doc enum: true
  def cv_COLOR_BGR2Luv, do: 50
  @doc enum: true
  def cv_COLOR_RGB2Luv, do: 51
  @doc enum: true
  def cv_COLOR_BGR2HLS, do: 52
  @doc enum: true
  def cv_COLOR_RGB2HLS, do: 53
  @doc enum: true
  def cv_COLOR_HSV2BGR, do: 54
  @doc enum: true
  def cv_COLOR_HSV2RGB, do: 55
  @doc enum: true
  def cv_COLOR_Lab2BGR, do: 56
  @doc enum: true
  def cv_COLOR_Lab2RGB, do: 57
  @doc enum: true
  def cv_COLOR_Luv2BGR, do: 58
  @doc enum: true
  def cv_COLOR_Luv2RGB, do: 59
  @doc enum: true
  def cv_COLOR_HLS2BGR, do: 60
  @doc enum: true
  def cv_COLOR_HLS2RGB, do: 61
  @doc enum: true
  def cv_COLOR_BGR2HSV_FULL, do: 66
  @doc enum: true
  def cv_COLOR_RGB2HSV_FULL, do: 67
  @doc enum: true
  def cv_COLOR_BGR2HLS_FULL, do: 68
  @doc enum: true
  def cv_COLOR_RGB2HLS_FULL, do: 69
  @doc enum: true
  def cv_COLOR_HSV2BGR_FULL, do: 70
  @doc enum: true
  def cv_COLOR_HSV2RGB_FULL, do: 71
  @doc enum: true
  def cv_COLOR_HLS2BGR_FULL, do: 72
  @doc enum: true
  def cv_COLOR_HLS2RGB_FULL, do: 73
  @doc enum: true
  def cv_COLOR_LBGR2Lab, do: 74
  @doc enum: true
  def cv_COLOR_LRGB2Lab, do: 75
  @doc enum: true
  def cv_COLOR_LBGR2Luv, do: 76
  @doc enum: true
  def cv_COLOR_LRGB2Luv, do: 77
  @doc enum: true
  def cv_COLOR_Lab2LBGR, do: 78
  @doc enum: true
  def cv_COLOR_Lab2LRGB, do: 79
  @doc enum: true
  def cv_COLOR_Luv2LBGR, do: 80
  @doc enum: true
  def cv_COLOR_Luv2LRGB, do: 81
  @doc enum: true
  def cv_COLOR_BGR2YUV, do: 82
  @doc enum: true
  def cv_COLOR_RGB2YUV, do: 83
  @doc enum: true
  def cv_COLOR_YUV2BGR, do: 84
  @doc enum: true
  def cv_COLOR_YUV2RGB, do: 85
  @doc enum: true
  def cv_COLOR_YUV2RGB_NV12, do: 90
  @doc enum: true
  def cv_COLOR_YUV2BGR_NV12, do: 91
  @doc enum: true
  def cv_COLOR_YUV2RGB_NV21, do: 92
  @doc enum: true
  def cv_COLOR_YUV2BGR_NV21, do: 93
  @doc enum: true
  def cv_COLOR_YUV420sp2RGB, do: cv_COLOR_YUV2RGB_NV21()
  @doc enum: true
  def cv_COLOR_YUV420sp2BGR, do: cv_COLOR_YUV2BGR_NV21()
  @doc enum: true
  def cv_COLOR_YUV2RGBA_NV12, do: 94
  @doc enum: true
  def cv_COLOR_YUV2BGRA_NV12, do: 95
  @doc enum: true
  def cv_COLOR_YUV2RGBA_NV21, do: 96
  @doc enum: true
  def cv_COLOR_YUV2BGRA_NV21, do: 97
  @doc enum: true
  def cv_COLOR_YUV420sp2RGBA, do: cv_COLOR_YUV2RGBA_NV21()
  @doc enum: true
  def cv_COLOR_YUV420sp2BGRA, do: cv_COLOR_YUV2BGRA_NV21()
  @doc enum: true
  def cv_COLOR_YUV2RGB_YV12, do: 98
  @doc enum: true
  def cv_COLOR_YUV2BGR_YV12, do: 99
  @doc enum: true
  def cv_COLOR_YUV2RGB_IYUV, do: 100
  @doc enum: true
  def cv_COLOR_YUV2BGR_IYUV, do: 101
  @doc enum: true
  def cv_COLOR_YUV2RGB_I420, do: cv_COLOR_YUV2RGB_IYUV()
  @doc enum: true
  def cv_COLOR_YUV2BGR_I420, do: cv_COLOR_YUV2BGR_IYUV()
  @doc enum: true
  def cv_COLOR_YUV420p2RGB, do: cv_COLOR_YUV2RGB_YV12()
  @doc enum: true
  def cv_COLOR_YUV420p2BGR, do: cv_COLOR_YUV2BGR_YV12()
  @doc enum: true
  def cv_COLOR_YUV2RGBA_YV12, do: 102
  @doc enum: true
  def cv_COLOR_YUV2BGRA_YV12, do: 103
  @doc enum: true
  def cv_COLOR_YUV2RGBA_IYUV, do: 104
  @doc enum: true
  def cv_COLOR_YUV2BGRA_IYUV, do: 105
  @doc enum: true
  def cv_COLOR_YUV2RGBA_I420, do: cv_COLOR_YUV2RGBA_IYUV()
  @doc enum: true
  def cv_COLOR_YUV2BGRA_I420, do: cv_COLOR_YUV2BGRA_IYUV()
  @doc enum: true
  def cv_COLOR_YUV420p2RGBA, do: cv_COLOR_YUV2RGBA_YV12()
  @doc enum: true
  def cv_COLOR_YUV420p2BGRA, do: cv_COLOR_YUV2BGRA_YV12()
  @doc enum: true
  def cv_COLOR_YUV2GRAY_420, do: 106
  @doc enum: true
  def cv_COLOR_YUV2GRAY_NV21, do: cv_COLOR_YUV2GRAY_420()
  @doc enum: true
  def cv_COLOR_YUV2GRAY_NV12, do: cv_COLOR_YUV2GRAY_420()
  @doc enum: true
  def cv_COLOR_YUV2GRAY_YV12, do: cv_COLOR_YUV2GRAY_420()
  @doc enum: true
  def cv_COLOR_YUV2GRAY_IYUV, do: cv_COLOR_YUV2GRAY_420()
  @doc enum: true
  def cv_COLOR_YUV2GRAY_I420, do: cv_COLOR_YUV2GRAY_420()
  @doc enum: true
  def cv_COLOR_YUV420sp2GRAY, do: cv_COLOR_YUV2GRAY_420()
  @doc enum: true
  def cv_COLOR_YUV420p2GRAY, do: cv_COLOR_YUV2GRAY_420()
  @doc enum: true
  def cv_COLOR_YUV2RGB_UYVY, do: 107
  @doc enum: true
  def cv_COLOR_YUV2BGR_UYVY, do: 108
  @doc enum: true
  def cv_COLOR_YUV2RGB_Y422, do: cv_COLOR_YUV2RGB_UYVY()
  @doc enum: true
  def cv_COLOR_YUV2BGR_Y422, do: cv_COLOR_YUV2BGR_UYVY()
  @doc enum: true
  def cv_COLOR_YUV2RGB_UYNV, do: cv_COLOR_YUV2RGB_UYVY()
  @doc enum: true
  def cv_COLOR_YUV2BGR_UYNV, do: cv_COLOR_YUV2BGR_UYVY()
  @doc enum: true
  def cv_COLOR_YUV2RGBA_UYVY, do: 111
  @doc enum: true
  def cv_COLOR_YUV2BGRA_UYVY, do: 112
  @doc enum: true
  def cv_COLOR_YUV2RGBA_Y422, do: cv_COLOR_YUV2RGBA_UYVY()
  @doc enum: true
  def cv_COLOR_YUV2BGRA_Y422, do: cv_COLOR_YUV2BGRA_UYVY()
  @doc enum: true
  def cv_COLOR_YUV2RGBA_UYNV, do: cv_COLOR_YUV2RGBA_UYVY()
  @doc enum: true
  def cv_COLOR_YUV2BGRA_UYNV, do: cv_COLOR_YUV2BGRA_UYVY()
  @doc enum: true
  def cv_COLOR_YUV2RGB_YUY2, do: 115
  @doc enum: true
  def cv_COLOR_YUV2BGR_YUY2, do: 116
  @doc enum: true
  def cv_COLOR_YUV2RGB_YVYU, do: 117
  @doc enum: true
  def cv_COLOR_YUV2BGR_YVYU, do: 118
  @doc enum: true
  def cv_COLOR_YUV2RGB_YUYV, do: cv_COLOR_YUV2RGB_YUY2()
  @doc enum: true
  def cv_COLOR_YUV2BGR_YUYV, do: cv_COLOR_YUV2BGR_YUY2()
  @doc enum: true
  def cv_COLOR_YUV2RGB_YUNV, do: cv_COLOR_YUV2RGB_YUY2()
  @doc enum: true
  def cv_COLOR_YUV2BGR_YUNV, do: cv_COLOR_YUV2BGR_YUY2()
  @doc enum: true
  def cv_COLOR_YUV2RGBA_YUY2, do: 119
  @doc enum: true
  def cv_COLOR_YUV2BGRA_YUY2, do: 120
  @doc enum: true
  def cv_COLOR_YUV2RGBA_YVYU, do: 121
  @doc enum: true
  def cv_COLOR_YUV2BGRA_YVYU, do: 122
  @doc enum: true
  def cv_COLOR_YUV2RGBA_YUYV, do: cv_COLOR_YUV2RGBA_YUY2()
  @doc enum: true
  def cv_COLOR_YUV2BGRA_YUYV, do: cv_COLOR_YUV2BGRA_YUY2()
  @doc enum: true
  def cv_COLOR_YUV2RGBA_YUNV, do: cv_COLOR_YUV2RGBA_YUY2()
  @doc enum: true
  def cv_COLOR_YUV2BGRA_YUNV, do: cv_COLOR_YUV2BGRA_YUY2()
  @doc enum: true
  def cv_COLOR_YUV2GRAY_UYVY, do: 123
  @doc enum: true
  def cv_COLOR_YUV2GRAY_YUY2, do: 124
  @doc enum: true
  def cv_COLOR_YUV2GRAY_Y422, do: cv_COLOR_YUV2GRAY_UYVY()
  @doc enum: true
  def cv_COLOR_YUV2GRAY_UYNV, do: cv_COLOR_YUV2GRAY_UYVY()
  @doc enum: true
  def cv_COLOR_YUV2GRAY_YVYU, do: cv_COLOR_YUV2GRAY_YUY2()
  @doc enum: true
  def cv_COLOR_YUV2GRAY_YUYV, do: cv_COLOR_YUV2GRAY_YUY2()
  @doc enum: true
  def cv_COLOR_YUV2GRAY_YUNV, do: cv_COLOR_YUV2GRAY_YUY2()
  @doc enum: true
  def cv_COLOR_RGBA2mRGBA, do: 125
  @doc enum: true
  def cv_COLOR_mRGBA2RGBA, do: 126
  @doc enum: true
  def cv_COLOR_RGB2YUV_I420, do: 127
  @doc enum: true
  def cv_COLOR_BGR2YUV_I420, do: 128
  @doc enum: true
  def cv_COLOR_RGB2YUV_IYUV, do: cv_COLOR_RGB2YUV_I420()
  @doc enum: true
  def cv_COLOR_BGR2YUV_IYUV, do: cv_COLOR_BGR2YUV_I420()
  @doc enum: true
  def cv_COLOR_RGBA2YUV_I420, do: 129
  @doc enum: true
  def cv_COLOR_BGRA2YUV_I420, do: 130
  @doc enum: true
  def cv_COLOR_RGBA2YUV_IYUV, do: cv_COLOR_RGBA2YUV_I420()
  @doc enum: true
  def cv_COLOR_BGRA2YUV_IYUV, do: cv_COLOR_BGRA2YUV_I420()
  @doc enum: true
  def cv_COLOR_RGB2YUV_YV12, do: 131
  @doc enum: true
  def cv_COLOR_BGR2YUV_YV12, do: 132
  @doc enum: true
  def cv_COLOR_RGBA2YUV_YV12, do: 133
  @doc enum: true
  def cv_COLOR_BGRA2YUV_YV12, do: 134
  @doc enum: true
  def cv_COLOR_BayerBG2BGR, do: 46
  @doc enum: true
  def cv_COLOR_BayerGB2BGR, do: 47
  @doc enum: true
  def cv_COLOR_BayerRG2BGR, do: 48
  @doc enum: true
  def cv_COLOR_BayerGR2BGR, do: 49
  @doc enum: true
  def cv_COLOR_BayerRGGB2BGR, do: cv_COLOR_BayerBG2BGR()
  @doc enum: true
  def cv_COLOR_BayerGRBG2BGR, do: cv_COLOR_BayerGB2BGR()
  @doc enum: true
  def cv_COLOR_BayerBGGR2BGR, do: cv_COLOR_BayerRG2BGR()
  @doc enum: true
  def cv_COLOR_BayerGBRG2BGR, do: cv_COLOR_BayerGR2BGR()
  @doc enum: true
  def cv_COLOR_BayerRGGB2RGB, do: cv_COLOR_BayerBGGR2BGR()
  @doc enum: true
  def cv_COLOR_BayerGRBG2RGB, do: cv_COLOR_BayerGBRG2BGR()
  @doc enum: true
  def cv_COLOR_BayerBGGR2RGB, do: cv_COLOR_BayerRGGB2BGR()
  @doc enum: true
  def cv_COLOR_BayerGBRG2RGB, do: cv_COLOR_BayerGRBG2BGR()
  @doc enum: true
  def cv_COLOR_BayerBG2RGB, do: cv_COLOR_BayerRG2BGR()
  @doc enum: true
  def cv_COLOR_BayerGB2RGB, do: cv_COLOR_BayerGR2BGR()
  @doc enum: true
  def cv_COLOR_BayerRG2RGB, do: cv_COLOR_BayerBG2BGR()
  @doc enum: true
  def cv_COLOR_BayerGR2RGB, do: cv_COLOR_BayerGB2BGR()
  @doc enum: true
  def cv_COLOR_BayerBG2GRAY, do: 86
  @doc enum: true
  def cv_COLOR_BayerGB2GRAY, do: 87
  @doc enum: true
  def cv_COLOR_BayerRG2GRAY, do: 88
  @doc enum: true
  def cv_COLOR_BayerGR2GRAY, do: 89
  @doc enum: true
  def cv_COLOR_BayerRGGB2GRAY, do: cv_COLOR_BayerBG2GRAY()
  @doc enum: true
  def cv_COLOR_BayerGRBG2GRAY, do: cv_COLOR_BayerGB2GRAY()
  @doc enum: true
  def cv_COLOR_BayerBGGR2GRAY, do: cv_COLOR_BayerRG2GRAY()
  @doc enum: true
  def cv_COLOR_BayerGBRG2GRAY, do: cv_COLOR_BayerGR2GRAY()
  @doc enum: true
  def cv_COLOR_BayerBG2BGR_VNG, do: 62
  @doc enum: true
  def cv_COLOR_BayerGB2BGR_VNG, do: 63
  @doc enum: true
  def cv_COLOR_BayerRG2BGR_VNG, do: 64
  @doc enum: true
  def cv_COLOR_BayerGR2BGR_VNG, do: 65
  @doc enum: true
  def cv_COLOR_BayerRGGB2BGR_VNG, do: cv_COLOR_BayerBG2BGR_VNG()
  @doc enum: true
  def cv_COLOR_BayerGRBG2BGR_VNG, do: cv_COLOR_BayerGB2BGR_VNG()
  @doc enum: true
  def cv_COLOR_BayerBGGR2BGR_VNG, do: cv_COLOR_BayerRG2BGR_VNG()
  @doc enum: true
  def cv_COLOR_BayerGBRG2BGR_VNG, do: cv_COLOR_BayerGR2BGR_VNG()
  @doc enum: true
  def cv_COLOR_BayerRGGB2RGB_VNG, do: cv_COLOR_BayerBGGR2BGR_VNG()
  @doc enum: true
  def cv_COLOR_BayerGRBG2RGB_VNG, do: cv_COLOR_BayerGBRG2BGR_VNG()
  @doc enum: true
  def cv_COLOR_BayerBGGR2RGB_VNG, do: cv_COLOR_BayerRGGB2BGR_VNG()
  @doc enum: true
  def cv_COLOR_BayerGBRG2RGB_VNG, do: cv_COLOR_BayerGRBG2BGR_VNG()
  @doc enum: true
  def cv_COLOR_BayerBG2RGB_VNG, do: cv_COLOR_BayerRG2BGR_VNG()
  @doc enum: true
  def cv_COLOR_BayerGB2RGB_VNG, do: cv_COLOR_BayerGR2BGR_VNG()
  @doc enum: true
  def cv_COLOR_BayerRG2RGB_VNG, do: cv_COLOR_BayerBG2BGR_VNG()
  @doc enum: true
  def cv_COLOR_BayerGR2RGB_VNG, do: cv_COLOR_BayerGB2BGR_VNG()
  @doc enum: true
  def cv_COLOR_BayerBG2BGR_EA, do: 135
  @doc enum: true
  def cv_COLOR_BayerGB2BGR_EA, do: 136
  @doc enum: true
  def cv_COLOR_BayerRG2BGR_EA, do: 137
  @doc enum: true
  def cv_COLOR_BayerGR2BGR_EA, do: 138
  @doc enum: true
  def cv_COLOR_BayerRGGB2BGR_EA, do: cv_COLOR_BayerBG2BGR_EA()
  @doc enum: true
  def cv_COLOR_BayerGRBG2BGR_EA, do: cv_COLOR_BayerGB2BGR_EA()
  @doc enum: true
  def cv_COLOR_BayerBGGR2BGR_EA, do: cv_COLOR_BayerRG2BGR_EA()
  @doc enum: true
  def cv_COLOR_BayerGBRG2BGR_EA, do: cv_COLOR_BayerGR2BGR_EA()
  @doc enum: true
  def cv_COLOR_BayerRGGB2RGB_EA, do: cv_COLOR_BayerBGGR2BGR_EA()
  @doc enum: true
  def cv_COLOR_BayerGRBG2RGB_EA, do: cv_COLOR_BayerGBRG2BGR_EA()
  @doc enum: true
  def cv_COLOR_BayerBGGR2RGB_EA, do: cv_COLOR_BayerRGGB2BGR_EA()
  @doc enum: true
  def cv_COLOR_BayerGBRG2RGB_EA, do: cv_COLOR_BayerGRBG2BGR_EA()
  @doc enum: true
  def cv_COLOR_BayerBG2RGB_EA, do: cv_COLOR_BayerRG2BGR_EA()
  @doc enum: true
  def cv_COLOR_BayerGB2RGB_EA, do: cv_COLOR_BayerGR2BGR_EA()
  @doc enum: true
  def cv_COLOR_BayerRG2RGB_EA, do: cv_COLOR_BayerBG2BGR_EA()
  @doc enum: true
  def cv_COLOR_BayerGR2RGB_EA, do: cv_COLOR_BayerGB2BGR_EA()
  @doc enum: true
  def cv_COLOR_BayerBG2BGRA, do: 139
  @doc enum: true
  def cv_COLOR_BayerGB2BGRA, do: 140
  @doc enum: true
  def cv_COLOR_BayerRG2BGRA, do: 141
  @doc enum: true
  def cv_COLOR_BayerGR2BGRA, do: 142
  @doc enum: true
  def cv_COLOR_BayerRGGB2BGRA, do: cv_COLOR_BayerBG2BGRA()
  @doc enum: true
  def cv_COLOR_BayerGRBG2BGRA, do: cv_COLOR_BayerGB2BGRA()
  @doc enum: true
  def cv_COLOR_BayerBGGR2BGRA, do: cv_COLOR_BayerRG2BGRA()
  @doc enum: true
  def cv_COLOR_BayerGBRG2BGRA, do: cv_COLOR_BayerGR2BGRA()
  @doc enum: true
  def cv_COLOR_BayerRGGB2RGBA, do: cv_COLOR_BayerBGGR2BGRA()
  @doc enum: true
  def cv_COLOR_BayerGRBG2RGBA, do: cv_COLOR_BayerGBRG2BGRA()
  @doc enum: true
  def cv_COLOR_BayerBGGR2RGBA, do: cv_COLOR_BayerRGGB2BGRA()
  @doc enum: true
  def cv_COLOR_BayerGBRG2RGBA, do: cv_COLOR_BayerGRBG2BGRA()
  @doc enum: true
  def cv_COLOR_BayerBG2RGBA, do: cv_COLOR_BayerRG2BGRA()
  @doc enum: true
  def cv_COLOR_BayerGB2RGBA, do: cv_COLOR_BayerGR2BGRA()
  @doc enum: true
  def cv_COLOR_BayerRG2RGBA, do: cv_COLOR_BayerBG2BGRA()
  @doc enum: true
  def cv_COLOR_BayerGR2RGBA, do: cv_COLOR_BayerGB2BGRA()
  @doc enum: true
  def cv_COLOR_RGB2YUV_UYVY, do: 143
  @doc enum: true
  def cv_COLOR_BGR2YUV_UYVY, do: 144
  @doc enum: true
  def cv_COLOR_RGB2YUV_Y422, do: cv_COLOR_RGB2YUV_UYVY()
  @doc enum: true
  def cv_COLOR_BGR2YUV_Y422, do: cv_COLOR_BGR2YUV_UYVY()
  @doc enum: true
  def cv_COLOR_RGB2YUV_UYNV, do: cv_COLOR_RGB2YUV_UYVY()
  @doc enum: true
  def cv_COLOR_BGR2YUV_UYNV, do: cv_COLOR_BGR2YUV_UYVY()
  @doc enum: true
  def cv_COLOR_RGBA2YUV_UYVY, do: 145
  @doc enum: true
  def cv_COLOR_BGRA2YUV_UYVY, do: 146
  @doc enum: true
  def cv_COLOR_RGBA2YUV_Y422, do: cv_COLOR_RGBA2YUV_UYVY()
  @doc enum: true
  def cv_COLOR_BGRA2YUV_Y422, do: cv_COLOR_BGRA2YUV_UYVY()
  @doc enum: true
  def cv_COLOR_RGBA2YUV_UYNV, do: cv_COLOR_RGBA2YUV_UYVY()
  @doc enum: true
  def cv_COLOR_BGRA2YUV_UYNV, do: cv_COLOR_BGRA2YUV_UYVY()
  @doc enum: true
  def cv_COLOR_RGB2YUV_YUY2, do: 147
  @doc enum: true
  def cv_COLOR_BGR2YUV_YUY2, do: 148
  @doc enum: true
  def cv_COLOR_RGB2YUV_YVYU, do: 149
  @doc enum: true
  def cv_COLOR_BGR2YUV_YVYU, do: 150
  @doc enum: true
  def cv_COLOR_RGB2YUV_YUYV, do: cv_COLOR_RGB2YUV_YUY2()
  @doc enum: true
  def cv_COLOR_BGR2YUV_YUYV, do: cv_COLOR_BGR2YUV_YUY2()
  @doc enum: true
  def cv_COLOR_RGB2YUV_YUNV, do: cv_COLOR_RGB2YUV_YUY2()
  @doc enum: true
  def cv_COLOR_BGR2YUV_YUNV, do: cv_COLOR_BGR2YUV_YUY2()
  @doc enum: true
  def cv_COLOR_RGBA2YUV_YUY2, do: 151
  @doc enum: true
  def cv_COLOR_BGRA2YUV_YUY2, do: 152
  @doc enum: true
  def cv_COLOR_RGBA2YUV_YVYU, do: 153
  @doc enum: true
  def cv_COLOR_BGRA2YUV_YVYU, do: 154
  @doc enum: true
  def cv_COLOR_RGBA2YUV_YUYV, do: cv_COLOR_RGBA2YUV_YUY2()
  @doc enum: true
  def cv_COLOR_BGRA2YUV_YUYV, do: cv_COLOR_BGRA2YUV_YUY2()
  @doc enum: true
  def cv_COLOR_RGBA2YUV_YUNV, do: cv_COLOR_RGBA2YUV_YUY2()
  @doc enum: true
  def cv_COLOR_BGRA2YUV_YUNV, do: cv_COLOR_BGRA2YUV_YUY2()
  @doc enum: true
  def cv_COLOR_COLORCVT_MAX, do: 155
end
