defmodule OpenCV.Nx do
  @moduledoc """
  OpenCV mat to Nx tensor. 

  `:nx` is an optional dependency, so if you want to use
  functions in `OpenCV.Nx`, you need to add it to the dependency
  list.
  """

  @doc """
  Transform an OpenCV.Mat `#Reference` to `#Nx.tensor`
  ### Example
  ```elixir
  {:ok, mat} = OpenCV.imread("/path/to/exist/img.png")
  nx_tensor = OpenCV.Nx.to_nx(mat)
   #Nx.Tensor<
      u8[1080][1920][3]
      [[ ... pixel data ... ]]
   >
  ```
  """
  @doc namespace: :external
  @spec to_mat(reference()) :: {:ok, reference()} | {:error, String.t()}
  def to_nx(mat) do
    if Code.ensure_loaded?(Nx) do
      {:ok, mat_type} = OpenCV.Mat.type(mat)
      {:ok, mat_shape} = OpenCV.Mat.shape(mat)

      case OpenCV.Mat.to_binary(mat) do
        {:ok, bin} ->
          bin
          |> Nx.from_binary(mat_type)
          |> Nx.reshape(mat_shape)

        {:error, reason} ->
          {:error, List.to_string(reason)}

        _ ->
          {:error, "unknown error"}
      end
    else
      {:error, ":nx is missing"}
    end
  end

  @doc """
  Converts a tensor of `Nx` to `Mat` of evision (OpenCV).
  """
  @doc namespace: :external
  @spec to_mat(Nx.t()) :: {:ok, reference()} | {:error, String.t()}
  def to_mat(nil, _), do: {:error, "tensor is nil"}

  def to_mat(t) do
    if Code.ensure_loaded?(Nx) do
      {rows, cols, channels} = Nx.shape(t)
      to_mat(Nx.to_binary(t), Nx.type(t), cols, rows, channels)
    else
      {:error, ":nx is missing"}
    end
  end

  @doc false
  @spec to_mat(
          binary(),
          {atom(), pos_integer()},
          pos_integer(),
          pos_integer(),
          pos_integer()
        ) :: {:ok, reference()} | {:error, charlist()}
  def to_mat(binary, type, cols, rows, channels) do
    case OpenCV.Mat.from_binary(binary, type, cols, rows, channels) do
      {:ok, mat} ->
        {:ok, mat}

      {:error, reason} ->
        {:error, List.to_string(reason)}

      _ ->
        {:error, "unknown error"}
    end
  end
end
