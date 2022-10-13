defmodule Evision.Nx do
  @moduledoc """
  Conversion functions between `Evision.Mat` and `Nx.tensor`.
  """

  unless Code.ensure_loaded?(Nx) do
    @compile {:no_warn_undefined, Nx}
  end

  @doc """
  Transform an `Evision.Mat` reference to `Nx.tensor`.

  The resulting tensor is in the shape `{height, width, channels}`.

  ### Example

  ```elixir
  iex> mat = %Mat{} = Evision.imread("/path/to/exist/img.png")
  iex> nx_tensor = Evision.Nx.to_nx(mat)
  #Nx.Tensor<
    u8[1080][1920][3]
    [[ ... pixel data ... ]]
  >
  ```

  """
  @doc namespace: :external
  @spec to_nx(Evision.Mat.t(), module()) :: Nx.Tensor.t() | {:error, String.t()}
  def to_nx(mat, backend \\ Evision.Backend) when is_struct(mat, Evision.Mat) do
    with mat_type <- Evision.Mat.type(mat),
         mat_shape <- Evision.Mat.shape(mat),
         {:not_empty_shape, true} <- {:not_empty_shape, tuple_size(mat_shape) > 0},
         {:not_error, true, _} <- {:not_error, elem(mat_shape, 0) != :error, mat_shape},
         bin <- Evision.Mat.to_binary(mat),
         {:is_binary, true, _} <- {:is_binary, is_binary(bin), bin} do
      Nx.reshape(Nx.from_binary(bin, mat_type, backend: backend), mat_shape)
    else
      {:error, reason} ->
        {:error, reason}

      {:not_empty_shape, false} ->
        {:error, "shape is {}"}

      {:not_error, false, error} ->
        error

      {:is_binary, false, error} ->
        error
    end
  end

  @doc """
  Converts a tensor from `Nx.Tensor` to `Evision.Mat`.
  """
  @doc namespace: :external
  @spec to_mat(Nx.t()) :: Evision.Mat.t() | {:error, String.t()}
  def to_mat(t) when is_struct(t, Nx.Tensor) do
    case Nx.shape(t) do
      {} ->
        Evision.Mat.from_binary_by_shape(Nx.to_binary(t), Nx.type(t), {1})

      shape ->
        Evision.Mat.from_binary_by_shape(Nx.to_binary(t), Nx.type(t), shape)
    end
  end

  @doc namespace: :external
  @spec to_mat(Nx.Tensor.t(), any) :: Evision.Mat.t() | {:error, String.t()}
  def to_mat(t, as_shape) when is_struct(t, Nx.Tensor) do
    case Nx.shape(t) do
      {} ->
        Evision.Mat.from_binary_by_shape(Nx.to_binary(t), Nx.type(t), {1})

      shape ->
        if Tuple.product(shape) == Tuple.product(as_shape) do
          Evision.Mat.from_binary_by_shape(Nx.to_binary(t), Nx.type(t), as_shape)
        else
          {:error,
           "cannot convert tensor(#{inspect(shape)}) to mat as shape #{inspect(as_shape)}"}
        end
    end
  end

  @doc namespace: :external
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

  @doc namespace: :external
  @doc """
  Converts a tensor from `Nx.Tensor` to `Evision.Mat`.

  If the tuple size of the shape is 3, the resulting `Evision.Mat` will be a `c`-channel 2D image,
  where `c` is the last number in the shape tuple.

  If the tuple size of the shape is 2, the resulting `Evision.Mat` will be a 1-channel 2D image.

  Otherwise, it's not possible to convert the tensor to a 2D image.
  """
  @spec to_mat_2d(Nx.t()) :: Evision.Mat.t() | {:error, String.t()}
  def to_mat_2d(t) do
    case Nx.shape(t) do
      {height, width} ->
        Evision.Mat.from_binary(Nx.to_binary(t), Nx.type(t), height, width, 1)

      {height, width, channels} ->
        Evision.Mat.from_binary(Nx.to_binary(t), Nx.type(t), height, width, channels)

      shape ->
        {:error, "Cannot convert tensor(#{inspect(shape)}) to a 2D image"}
    end
  end
end
