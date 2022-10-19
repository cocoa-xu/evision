defmodule Evision.Nx do
  @moduledoc false

  @spec to_nx(Evision.Mat.t(), module()) :: Nx.Tensor.t() | {:error, String.t()}
  def to_nx(mat, backend \\ Evision.Backend) when is_struct(mat, Evision.Mat) do
    Evision.Mat.to_nx(mat, backend)
  end

  @spec to_mat(Nx.t()) :: Evision.Mat.t() | {:error, String.t()}
  def to_mat(t) when is_struct(t, Nx.Tensor) do
    Evision.Mat.from_nx(t)
  end

  @spec to_mat(Nx.Tensor.t(), any) :: Evision.Mat.t() | {:error, String.t()}
  def to_mat(t, as_shape) when is_struct(t, Nx.Tensor) do
    Evision.Mat.from_nx(t, as_shape)
  end

  @spec to_mat(
          binary(),
          Evision.Mat.mat_type(),
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer()
        ) :: Evision.Mat.maybe_mat_out()
  def to_mat(binary, type, rows, cols, channels) do
    Evision.Mat.from_binary(binary, type, rows, cols, channels)
  end

  @spec to_mat_2d(Nx.t()) :: Evision.Mat.t() | {:error, String.t()}
  def to_mat_2d(t) do
    Evision.Mat.from_nx_2d(t)
  end
end
