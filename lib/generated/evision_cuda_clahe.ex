defmodule Evision.CUDA.CLAHE do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.CLAHE` struct.

  - **ref**. `reference()`

    The underlying erlang resource variable.

  """
  @type t :: %__MODULE__{
    ref: reference()
  }
  @enforce_keys [:ref]
  defstruct [:ref]
  alias __MODULE__, as: T

  @doc false
  def to_struct({:ok, %{class: Evision.CUDA.CLAHE, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.CLAHE, ref: ref}) do
    %T{
      ref: ref
    }
  end

  @doc false
  def to_struct(ret) do
    Evision.Internal.Structurise.to_struct(ret)
  end
  
  @doc false
  def from_struct(%T{ref: ref}) do
    ref
  end

  @doc """
  #### Variant 1:
  Equalizes the histogram of a grayscale image using Contrast Limited Adaptive Histogram Equalization.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CLAHE.t()`
  - **src**: `Evision.Mat`.

    Source image with CV_8UC1 type.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  apply(src, stream[, dst]) -> dst
  ```
  #### Variant 2:
  Equalizes the histogram of a grayscale image using Contrast Limited Adaptive Histogram Equalization.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CLAHE.t()`
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image with CV_8UC1 type.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  apply(src, stream[, dst]) -> dst
  ```

  """
  @spec apply(Evision.CUDA.CUDA.CLAHE.t(), Evision.Mat.maybe_mat_in(), Evision.CUDA.Stream.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, src, stream, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_struct(stream, Evision.CUDA.Stream) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_CLAHE_apply(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec apply(Evision.CUDA.CUDA.CLAHE.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.Stream.t(), [{atom(), term()},...] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def apply(self, src, stream, opts) when is_struct(src, Evision.CUDA.GpuMat) and is_struct(stream, Evision.CUDA.Stream) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_CLAHE_apply(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Equalizes the histogram of a grayscale image using Contrast Limited Adaptive Histogram Equalization.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CLAHE.t()`
  - **src**: `Evision.Mat`.

    Source image with CV_8UC1 type.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  apply(src, stream[, dst]) -> dst
  ```
  #### Variant 2:
  Equalizes the histogram of a grayscale image using Contrast Limited Adaptive Histogram Equalization.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CLAHE.t()`
  - **src**: `Evision.CUDA.GpuMat.t()`.

    Source image with CV_8UC1 type.

  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **dst**: `Evision.CUDA.GpuMat.t()`.

    Destination image.

  Python prototype (for reference only):
  ```python3
  apply(src, stream[, dst]) -> dst
  ```

  """
  @spec apply(Evision.CUDA.CUDA.CLAHE.t(), Evision.Mat.maybe_mat_in(), Evision.CUDA.Stream.t()) :: Evision.Mat.t() | {:error, String.t()}
  def apply(self, src, stream) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_struct(stream, Evision.CUDA.Stream)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_CLAHE_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec apply(Evision.CUDA.CUDA.CLAHE.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.Stream.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def apply(self, src, stream) when is_struct(src, Evision.CUDA.GpuMat) and is_struct(stream, Evision.CUDA.Stream)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_CLAHE_apply(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
