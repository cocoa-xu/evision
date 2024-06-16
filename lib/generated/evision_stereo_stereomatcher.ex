defmodule Evision.Stereo.StereoMatcher do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_DISP_SHIFT, do: 4
  @doc enum: true
  def cv_DISP_SCALE, do: bsl(1, cv_DISP_SHIFT())
end
