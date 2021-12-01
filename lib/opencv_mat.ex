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

  # Nx related
  def to_nx(mat) do
    {:ok, mat_type} = type(mat)
    {:ok, mat_shape} = shape(mat)
    case to_binary(mat) do
      {:ok, bin} ->
        bin
        |> Nx.from_binary(mat_type)
        |> Nx.reshape(mat_shape)
      {:error, reason} ->
        {:error, reason}
      _ ->
        {:error, "unknown error"}
    end
  end
end
