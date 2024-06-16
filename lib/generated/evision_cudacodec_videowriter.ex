defmodule Evision.CUDACodec.VideoWriter do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDACodec.VideoWriter` struct.

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
  def to_struct({:ok, %{class: Evision.CUDACodec.VideoWriter, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDACodec.VideoWriter, ref: ref}) do
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
  Retrieve the encoding parameters.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoWriter.t()`

  ##### Return
  - **retval**: `EncoderParams`

  Python prototype (for reference only):
  ```python3
  getEncoderParams() -> retval
  ```
  """
  @spec getEncoderParams(Evision.CUDACodec.CUDACodec.VideoWriter.t()) :: Evision.CUDACodec.EncoderParams.t() | {:error, String.t()}
  def getEncoderParams(self) do
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_VideoWriter_getEncoderParams(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Waits until the encoding process has finished before calling EncoderCallback::onEncodingFinished().

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoWriter.t()`

  Python prototype (for reference only):
  ```python3
  release() -> None
  ```
  """
  @spec release(Evision.CUDACodec.CUDACodec.VideoWriter.t()) :: Evision.CUDACodec.CUDACodec.VideoWriter.t() | {:error, String.t()}
  def release(self) do
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_VideoWriter_release(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Writes the next video frame.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoWriter.t()`
  - **frame**: `Evision.Mat`.

    The framet to be written.

  The method encodes the specified image to a video stream. The image must have the same size and the same
  surface format as has been specified when opening the video writer.

  Python prototype (for reference only):
  ```python3
  write(frame) -> None
  ```
  #### Variant 2:
  Writes the next video frame.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoWriter.t()`
  - **frame**: `Evision.CUDA.GpuMat.t()`.

    The framet to be written.

  The method encodes the specified image to a video stream. The image must have the same size and the same
  surface format as has been specified when opening the video writer.

  Python prototype (for reference only):
  ```python3
  write(frame) -> None
  ```

  """
  @spec write(Evision.CUDACodec.CUDACodec.VideoWriter.t(), Evision.Mat.maybe_mat_in()) :: Evision.CUDACodec.CUDACodec.VideoWriter.t() | {:error, String.t()}
  def write(self, frame) when (is_struct(frame, Evision.Mat) or is_struct(frame, Nx.Tensor) or is_number(frame) or is_tuple(frame))
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame)
    ]
    :evision_nif.cudacodec_cudacodec_VideoWriter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec write(Evision.CUDACodec.CUDACodec.VideoWriter.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDACodec.CUDACodec.VideoWriter.t() | {:error, String.t()}
  def write(self, frame) when is_struct(frame, Evision.CUDA.GpuMat)
  do
    positional = [
      frame: Evision.Internal.Structurise.from_struct(frame)
    ]
    :evision_nif.cudacodec_cudacodec_VideoWriter_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
