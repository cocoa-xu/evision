defmodule Evision.CUDA.SURF_CUDA.KeypointLayout do
  @type enum :: integer()
  @doc enum: true
  def cv_X_ROW, do: 0
  @doc enum: true
  def cv_Y_ROW, do: (0 + 1)
  @doc enum: true
  def cv_LAPLACIAN_ROW, do: (0 + 2)
  @doc enum: true
  def cv_OCTAVE_ROW, do: (0 + 3)
  @doc enum: true
  def cv_SIZE_ROW, do: (0 + 4)
  @doc enum: true
  def cv_ANGLE_ROW, do: (0 + 5)
  @doc enum: true
  def cv_HESSIAN_ROW, do: (0 + 6)
  @doc enum: true
  def cv_ROWS_COUNT, do: (0 + 7)
end
