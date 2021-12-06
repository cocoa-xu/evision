defmodule OpenCV.Mat do
  @moduledoc false

  def type(mat) do
    :erl_cv_nif.evision_cv_mat_type([img: mat])
  end

  def shape(mat) do
    :erl_cv_nif.evision_cv_mat_shape([img: mat])
  end

  def clone(mat) do
    :erl_cv_nif.evision_cv_mat_clone([img: mat])
  end

  def to_binary(mat) do
    :erl_cv_nif.evision_cv_mat_to_binary([img: mat])
  end
end
