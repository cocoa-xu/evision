defmodule Evision.CUDA.Stream do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.Stream` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.Stream, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.Stream, ref: ref}) do
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
  creates a new Stream using the cudaFlags argument to determine the behaviors of the stream

  ##### Positional Arguments
  - **cudaFlags**: `size_t`

  ##### Return
  - **self**: `Evision.CUDA.Stream.t()`

  **Note**: The cudaFlags parameter is passed to the underlying api cudaStreamCreateWithFlags() and
  supports the same parameter values.
  ```
  // creates an OpenCV cuda::Stream that manages an asynchronous, non-blocking,
  // non-default CUDA stream
  cv::cuda::Stream cvStream(cudaStreamNonBlocking);
  ```

  Python prototype (for reference only):
  ```python3
  Stream(cudaFlags) -> <cuda_Stream object>
  ```
  #### Variant 2:
  Stream

  ##### Positional Arguments
  - **allocator**: `GpuMat::Allocator`

  ##### Return
  - **self**: `Evision.CUDA.Stream.t()`

  Python prototype (for reference only):
  ```python3
  Stream(allocator) -> <cuda_Stream object>
  ```

  """
  @spec stream(integer()) :: Evision.CUDA.Stream.t() | {:error, String.t()}
  def stream(cudaFlags) when is_integer(cudaFlags)
  do
    positional = [
      cudaFlags: Evision.Internal.Structurise.from_struct(cudaFlags)
    ]
    :evision_nif.cuda_cuda_Stream_Stream(positional)
    |> to_struct()
  end
  @spec stream(reference()) :: Evision.CUDA.Stream.t() | {:error, String.t()}
  def stream(allocator) when is_struct(allocator, Evision.CUDA.GpuMat_Allocator)
  do
    positional = [
      allocator: Evision.Internal.Structurise.from_struct(allocator)
    ]
    :evision_nif.cuda_cuda_Stream_Stream(positional)
    |> to_struct()
  end

  @doc """
  Stream
  ##### Return
  - **self**: `Evision.CUDA.Stream.t()`

  Python prototype (for reference only):
  ```python3
  Stream() -> <cuda_Stream object>
  ```
  """
  @spec stream() :: Evision.CUDA.Stream.t() | {:error, String.t()}
  def stream() do
    positional = [
    ]
    :evision_nif.cuda_cuda_Stream_Stream(positional)
    |> to_struct()
  end

  @doc """
  Adds a callback to be called on the host after all currently enqueued items in the stream have
  completed.

  ##### Return
  - **retval**: `Evision.CUDA.Stream.t()`

  **Note**: Callbacks must not make any CUDA API calls. Callbacks must not perform any synchronization
  that may depend on outstanding device work or other callbacks that are not mandated to run earlier.
  Callbacks without a mandated order (in independent streams) execute in undefined order and may be
  serialized.

  Python prototype (for reference only):
  ```python3
  Null() -> retval
  ```
  """
  @spec null() :: Evision.CUDA.Stream.t() | {:error, String.t()}
  def null() do
    positional = [
    ]
    :evision_nif.cuda_cuda_Stream_Null_static(positional)
    |> to_struct()
  end

  @doc """
  cudaPtr

  ##### Positional Arguments
  - **self**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **retval**: `void*`

  Python prototype (for reference only):
  ```python3
  cudaPtr() -> retval
  ```
  """
  @spec cudaPtr(Evision.CUDA.Stream.t()) :: :ok | {:error, String.t()}
  def cudaPtr(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_Stream_cudaPtr(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the current stream queue is finished. Otherwise, it returns false.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  queryIfComplete() -> retval
  ```
  """
  @spec queryIfComplete(Evision.CUDA.Stream.t()) :: boolean() | {:error, String.t()}
  def queryIfComplete(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_Stream_queryIfComplete(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Makes a compute stream wait on an event.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.Stream.t()`
  - **event**: `Evision.CUDA.Event.t()`

  Python prototype (for reference only):
  ```python3
  waitEvent(event) -> None
  ```
  """
  @spec waitEvent(Evision.CUDA.Stream.t(), Evision.CUDA.Event.t()) :: Evision.CUDA.Stream.t() | {:error, String.t()}
  def waitEvent(self, event) when is_struct(event, Evision.CUDA.Event)
  do
    positional = [
      event: Evision.Internal.Structurise.from_struct(event)
    ]
    :evision_nif.cuda_cuda_Stream_waitEvent(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Blocks the current CPU thread until all operations in the stream are complete.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.Stream.t()`

  Python prototype (for reference only):
  ```python3
  waitForCompletion() -> None
  ```
  """
  @spec waitForCompletion(Evision.CUDA.Stream.t()) :: Evision.CUDA.Stream.t() | {:error, String.t()}
  def waitForCompletion(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_Stream_waitForCompletion(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
