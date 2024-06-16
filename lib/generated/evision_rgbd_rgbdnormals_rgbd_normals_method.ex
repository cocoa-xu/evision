defmodule Evision.RGBD.RgbdNormals.RGBD_NORMALS_METHOD do
  @type enum :: integer()
  @doc enum: true
  def cv_RGBD_NORMALS_METHOD_FALS, do: 0
  @doc enum: true
  def cv_RGBD_NORMALS_METHOD_LINEMOD, do: 1
  @doc enum: true
  def cv_RGBD_NORMALS_METHOD_SRI, do: 2
end
