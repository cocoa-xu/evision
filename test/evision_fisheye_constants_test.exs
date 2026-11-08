defmodule Evision.FishEye.Constants.Test do
  use ExUnit.Case, async: true

  @flags ~w(USE_INTRINSIC_GUESS RECOMPUTE_EXTRINSIC CHECK_COND FIX_SKEW FIX_K1 FIX_K2 FIX_K3 FIX_K4
            FIX_INTRINSIC FIX_PRINCIPAL_POINT ZERO_DISPARITY FIX_FOCAL_LENGTH)

  test "fisheye calibration flags alias the cv::CALIB_* constants" do
    for flag <- @flags do
      fun = :"cv_CALIB_#{flag}"
      assert apply(Evision.FishEye, fun, []) == apply(Evision.Constant, fun, [])
    end
  end
end
