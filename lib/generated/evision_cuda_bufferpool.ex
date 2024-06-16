defmodule Evision.CUDA.BufferPool do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.BufferPool` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.BufferPool, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.BufferPool, ref: ref}) do
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
  BufferPool

  ##### Positional Arguments
  - **stream**: `Evision.CUDA.Stream.t()`

  ##### Return
  - **self**: `Evision.CUDA.BufferPool.t()`

  Python prototype (for reference only):
  ```python3
  BufferPool(stream) -> <cuda_BufferPool object>
  ```
  """
  @spec bufferPool(Evision.CUDA.Stream.t()) :: Evision.CUDA.BufferPool.t() | {:error, String.t()}
  def bufferPool(stream) when is_struct(stream, Evision.CUDA.Stream)
  do
    positional = [
      stream: Evision.Internal.Structurise.from_struct(stream)
    ]
    :evision_nif.cuda_cuda_BufferPool_BufferPool(positional)
    |> to_struct()
  end

  @doc """
  getAllocator

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BufferPool.t()`

  ##### Return
  - **retval**: `GpuMat::Allocator`

  Python prototype (for reference only):
  ```python3
  getAllocator() -> retval
  ```
  """
  @spec getAllocator(Evision.CUDA.BufferPool.t()) :: reference() | {:error, String.t()}
  def getAllocator(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_BufferPool_getAllocator(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBuffer

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BufferPool.t()`
  - **rows**: `integer()`
  - **cols**: `integer()`
  - **type**: `integer()`

  ##### Return
  - **retval**: `Evision.CUDA.GpuMat.t()`

  Python prototype (for reference only):
  ```python3
  getBuffer(rows, cols, type) -> retval
  ```
  """
  @spec getBuffer(Evision.CUDA.BufferPool.t(), integer(), integer(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def getBuffer(self, rows, cols, type) when is_integer(rows) and is_integer(cols) and is_integer(type)
  do
    positional = [
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_cuda_BufferPool_getBuffer(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBuffer

  ##### Positional Arguments
  - **self**: `Evision.CUDA.BufferPool.t()`
  - **size**: `Size`
  - **type**: `integer()`

  ##### Return
  - **retval**: `Evision.CUDA.GpuMat.t()`

  Python prototype (for reference only):
  ```python3
  getBuffer(size, type) -> retval
  ```
  """
  @spec getBuffer(Evision.CUDA.BufferPool.t(), {number(), number()}, integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def getBuffer(self, size, type) when is_tuple(size) and is_integer(type)
  do
    positional = [
      size: Evision.Internal.Structurise.from_struct(size),
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.cuda_cuda_BufferPool_getBuffer(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
