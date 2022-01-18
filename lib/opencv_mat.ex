defmodule OpenCV.Mat do
  @moduledoc false

  @doc namespace: :"cv.Mat"
  def type(mat) do
    :erl_cv_nif.evision_cv_mat_type(img: mat)
  end

  @doc namespace: :"cv.Mat"
  def shape(mat) do
    :erl_cv_nif.evision_cv_mat_shape(img: mat)
  end

  @doc namespace: :"cv.Mat"
  def clone(mat) do
    :erl_cv_nif.evision_cv_mat_clone(img: mat)
  end

  @doc namespace: :"cv.Mat"
  def to_binary(mat) do
    :erl_cv_nif.evision_cv_mat_to_binary(img: mat)
  end

  @doc """
  Create Mat from binary (pixel) data

  @param binary, the binary pixel data
  @param type={t, l}, `type` is one of [{:u, 8}, {:s, 8}, {:u, 16}, {:s, 16}, {:s, 32}, {:f, 32}, {:f, 64}]
  @param cols, number of cols
  @param rows, number of rows
  @param channels, number of channels, only valid if in [1, 3, 4]
  """
  @doc namespace: :"cv.Mat"
  def from_binary(binary, _type = {t, l}, cols, rows, channels)
      when is_binary(binary) and is_integer(cols) and is_integer(rows) and is_integer(channels) and
             is_atom(t) and is_integer(l) do
    :erl_cv_nif.evision_cv_mat_from_binary(
      binary: binary,
      t: t,
      l: l,
      cols: cols,
      rows: rows,
      channels: channels
    )
  end

  @doc namespace: :"cv.Mat"
  def from_binary(binary, _type = {t, l}, cols, rows, channels)
      when is_binary(binary) and is_integer(cols) and is_integer(rows) and is_integer(channels) and
             is_atom(t) and is_integer(l) do
    :erl_cv_nif.evision_cv_mat_from_binary(
      binary: binary,
      t: t,
      l: l,
      cols: cols,
      rows: rows,
      channels: channels
    )
  end
end
