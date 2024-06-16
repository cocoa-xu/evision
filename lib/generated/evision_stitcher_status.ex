defmodule Evision.Stitcher.Status do
  @type enum :: integer()
  @doc enum: true
  def cv_OK, do: 0
  @doc enum: true
  def cv_ERR_NEED_MORE_IMGS, do: 1
  @doc enum: true
  def cv_ERR_HOMOGRAPHY_EST_FAIL, do: 2
  @doc enum: true
  def cv_ERR_CAMERA_PARAMS_ADJUST_FAIL, do: 3
end
