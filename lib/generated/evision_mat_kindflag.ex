defmodule Evision.Mat.KindFlag do
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_KIND_SHIFT, do: 16
  @doc enum: true
  def cv_FIXED_TYPE, do: bsl(32768, cv_KIND_SHIFT())
  @doc enum: true
  def cv_FIXED_SIZE, do: bsl(16384, cv_KIND_SHIFT())
  @doc enum: true
  def cv_KIND_MASK, do: bsl(31, cv_KIND_SHIFT())
  @doc enum: true
  def cv_NONE, do: bsl(0, cv_KIND_SHIFT())
  @doc enum: true
  def cv_MAT, do: bsl(1, cv_KIND_SHIFT())
  @doc enum: true
  def cv_MATX, do: bsl(2, cv_KIND_SHIFT())
  @doc enum: true
  def cv_STD_VECTOR, do: bsl(3, cv_KIND_SHIFT())
  @doc enum: true
  def cv_STD_VECTOR_VECTOR, do: bsl(4, cv_KIND_SHIFT())
  @doc enum: true
  def cv_STD_VECTOR_MAT, do: bsl(5, cv_KIND_SHIFT())
  @doc enum: true
  def cv_EXPR, do: bsl(6, cv_KIND_SHIFT())
  @doc enum: true
  def cv_OPENGL_BUFFER, do: bsl(7, cv_KIND_SHIFT())
  @doc enum: true
  def cv_CUDA_HOST_MEM, do: bsl(8, cv_KIND_SHIFT())
  @doc enum: true
  def cv_CUDA_GPU_MAT, do: bsl(9, cv_KIND_SHIFT())
  @doc enum: true
  def cv_UMAT, do: bsl(10, cv_KIND_SHIFT())
  @doc enum: true
  def cv_STD_VECTOR_UMAT, do: bsl(11, cv_KIND_SHIFT())
  @doc enum: true
  def cv_STD_BOOL_VECTOR, do: bsl(12, cv_KIND_SHIFT())
  @doc enum: true
  def cv_STD_VECTOR_CUDA_GPU_MAT, do: bsl(13, cv_KIND_SHIFT())
  @doc enum: true
  def cv_STD_ARRAY, do: bsl(14, cv_KIND_SHIFT())
  @doc enum: true
  def cv_STD_ARRAY_MAT, do: bsl(15, cv_KIND_SHIFT())
end
