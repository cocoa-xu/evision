defmodule OpenCV.Nx do
  @moduledoc false

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
  @doc namespace: :"external.Nx"
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
          {:error, reason}

        _ ->
          {:error, "unknown error"}
      end
    else
      {:error, ":nx is missing"}
    end
  end
end
