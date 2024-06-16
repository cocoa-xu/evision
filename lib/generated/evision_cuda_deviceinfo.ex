defmodule Evision.CUDA.DeviceInfo do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.DeviceInfo` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.DeviceInfo, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.DeviceInfo, ref: ref}) do
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
  The constructors.

  ##### Positional Arguments
  - **device_id**: `integer()`.

    System index of the CUDA device starting with 0.

  ##### Return
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  Constructs the DeviceInfo object for the specified device. If device_id parameter is missed, it
  constructs an object for the current device.

  Python prototype (for reference only):
  ```python3
  DeviceInfo(device_id) -> <cuda_DeviceInfo object>
  ```
  """
  @spec deviceInfo(integer()) :: Evision.CUDA.DeviceInfo.t() | {:error, String.t()}
  def deviceInfo(device_id) when is_integer(device_id)
  do
    positional = [
      device_id: Evision.Internal.Structurise.from_struct(device_id)
    ]
    :evision_nif.cuda_cuda_DeviceInfo_DeviceInfo(positional)
    |> to_struct()
  end

  @doc """
  DeviceInfo
  ##### Return
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  Python prototype (for reference only):
  ```python3
  DeviceInfo() -> <cuda_DeviceInfo object>
  ```
  """
  @spec deviceInfo() :: Evision.CUDA.DeviceInfo.t() | {:error, String.t()}
  def deviceInfo() do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_DeviceInfo(positional)
    |> to_struct()
  end

  @doc """
  ECCEnabled

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  ECCEnabled() -> retval
  ```
  """
  @spec eccEnabled(Evision.CUDA.DeviceInfo.t()) :: boolean() | {:error, String.t()}
  def eccEnabled(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_ECCEnabled(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  asyncEngineCount

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  asyncEngineCount() -> retval
  ```
  """
  @spec asyncEngineCount(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def asyncEngineCount(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_asyncEngineCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  canMapHostMemory

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  canMapHostMemory() -> retval
  ```
  """
  @spec canMapHostMemory(Evision.CUDA.DeviceInfo.t()) :: boolean() | {:error, String.t()}
  def canMapHostMemory(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_canMapHostMemory(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  clockRate

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  clockRate() -> retval
  ```
  """
  @spec clockRate(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def clockRate(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_clockRate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  computeMode

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `DeviceInfo::ComputeMode`

  Python prototype (for reference only):
  ```python3
  computeMode() -> retval
  ```
  """
  @spec computeMode(Evision.CUDA.DeviceInfo.t()) :: Evision.CUDA.DeviceInfo.ComputeMode.enum() | {:error, String.t()}
  def computeMode(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_computeMode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  concurrentKernels

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  concurrentKernels() -> retval
  ```
  """
  @spec concurrentKernels(Evision.CUDA.DeviceInfo.t()) :: boolean() | {:error, String.t()}
  def concurrentKernels(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_concurrentKernels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns system index of the CUDA device starting with 0.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  deviceID() -> retval
  ```
  """
  @spec deviceID(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def deviceID(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_deviceID(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  freeMemory

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  freeMemory() -> retval
  ```
  """
  @spec freeMemory(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def freeMemory(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_freeMemory(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  integrated

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  integrated() -> retval
  ```
  """
  @spec integrated(Evision.CUDA.DeviceInfo.t()) :: boolean() | {:error, String.t()}
  def integrated(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_integrated(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Checks the CUDA module and device compatibility.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `bool`

  This function returns true if the CUDA module can be run on the specified device. Otherwise, it
  returns false .

  Python prototype (for reference only):
  ```python3
  isCompatible() -> retval
  ```
  """
  @spec isCompatible(Evision.CUDA.DeviceInfo.t()) :: boolean() | {:error, String.t()}
  def isCompatible(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_isCompatible(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  kernelExecTimeoutEnabled

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  kernelExecTimeoutEnabled() -> retval
  ```
  """
  @spec kernelExecTimeoutEnabled(Evision.CUDA.DeviceInfo.t()) :: boolean() | {:error, String.t()}
  def kernelExecTimeoutEnabled(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_kernelExecTimeoutEnabled(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  l2CacheSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  l2CacheSize() -> retval
  ```
  """
  @spec l2CacheSize(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def l2CacheSize(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_l2CacheSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  majorVersion

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  majorVersion() -> retval
  ```
  """
  @spec majorVersion(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def majorVersion(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_majorVersion(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxGridSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec3i`

  Python prototype (for reference only):
  ```python3
  maxGridSize() -> retval
  ```
  """
  @spec maxGridSize(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer(), integer()} | {:error, String.t()}
  def maxGridSize(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxGridSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxSurface1D

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  maxSurface1D() -> retval
  ```
  """
  @spec maxSurface1D(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def maxSurface1D(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxSurface1D(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxSurface1DLayered

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec2i`

  Python prototype (for reference only):
  ```python3
  maxSurface1DLayered() -> retval
  ```
  """
  @spec maxSurface1DLayered(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer()} | {:error, String.t()}
  def maxSurface1DLayered(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxSurface1DLayered(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxSurface2D

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec2i`

  Python prototype (for reference only):
  ```python3
  maxSurface2D() -> retval
  ```
  """
  @spec maxSurface2D(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer()} | {:error, String.t()}
  def maxSurface2D(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxSurface2D(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxSurface2DLayered

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec3i`

  Python prototype (for reference only):
  ```python3
  maxSurface2DLayered() -> retval
  ```
  """
  @spec maxSurface2DLayered(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer(), integer()} | {:error, String.t()}
  def maxSurface2DLayered(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxSurface2DLayered(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxSurface3D

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec3i`

  Python prototype (for reference only):
  ```python3
  maxSurface3D() -> retval
  ```
  """
  @spec maxSurface3D(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer(), integer()} | {:error, String.t()}
  def maxSurface3D(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxSurface3D(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxSurfaceCubemap

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  maxSurfaceCubemap() -> retval
  ```
  """
  @spec maxSurfaceCubemap(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def maxSurfaceCubemap(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxSurfaceCubemap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxSurfaceCubemapLayered

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec2i`

  Python prototype (for reference only):
  ```python3
  maxSurfaceCubemapLayered() -> retval
  ```
  """
  @spec maxSurfaceCubemapLayered(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer()} | {:error, String.t()}
  def maxSurfaceCubemapLayered(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxSurfaceCubemapLayered(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxTexture1D

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  maxTexture1D() -> retval
  ```
  """
  @spec maxTexture1D(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def maxTexture1D(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxTexture1D(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxTexture1DLayered

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec2i`

  Python prototype (for reference only):
  ```python3
  maxTexture1DLayered() -> retval
  ```
  """
  @spec maxTexture1DLayered(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer()} | {:error, String.t()}
  def maxTexture1DLayered(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxTexture1DLayered(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxTexture1DLinear

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  maxTexture1DLinear() -> retval
  ```
  """
  @spec maxTexture1DLinear(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def maxTexture1DLinear(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxTexture1DLinear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxTexture1DMipmap

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  maxTexture1DMipmap() -> retval
  ```
  """
  @spec maxTexture1DMipmap(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def maxTexture1DMipmap(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxTexture1DMipmap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxTexture2D

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec2i`

  Python prototype (for reference only):
  ```python3
  maxTexture2D() -> retval
  ```
  """
  @spec maxTexture2D(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer()} | {:error, String.t()}
  def maxTexture2D(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxTexture2D(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxTexture2DGather

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec2i`

  Python prototype (for reference only):
  ```python3
  maxTexture2DGather() -> retval
  ```
  """
  @spec maxTexture2DGather(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer()} | {:error, String.t()}
  def maxTexture2DGather(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxTexture2DGather(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxTexture2DLayered

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec3i`

  Python prototype (for reference only):
  ```python3
  maxTexture2DLayered() -> retval
  ```
  """
  @spec maxTexture2DLayered(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer(), integer()} | {:error, String.t()}
  def maxTexture2DLayered(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxTexture2DLayered(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxTexture2DLinear

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec3i`

  Python prototype (for reference only):
  ```python3
  maxTexture2DLinear() -> retval
  ```
  """
  @spec maxTexture2DLinear(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer(), integer()} | {:error, String.t()}
  def maxTexture2DLinear(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxTexture2DLinear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxTexture2DMipmap

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec2i`

  Python prototype (for reference only):
  ```python3
  maxTexture2DMipmap() -> retval
  ```
  """
  @spec maxTexture2DMipmap(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer()} | {:error, String.t()}
  def maxTexture2DMipmap(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxTexture2DMipmap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxTexture3D

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec3i`

  Python prototype (for reference only):
  ```python3
  maxTexture3D() -> retval
  ```
  """
  @spec maxTexture3D(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer(), integer()} | {:error, String.t()}
  def maxTexture3D(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxTexture3D(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxTextureCubemap

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  maxTextureCubemap() -> retval
  ```
  """
  @spec maxTextureCubemap(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def maxTextureCubemap(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxTextureCubemap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxTextureCubemapLayered

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec2i`

  Python prototype (for reference only):
  ```python3
  maxTextureCubemapLayered() -> retval
  ```
  """
  @spec maxTextureCubemapLayered(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer()} | {:error, String.t()}
  def maxTextureCubemapLayered(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxTextureCubemapLayered(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxThreadsDim

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `Vec3i`

  Python prototype (for reference only):
  ```python3
  maxThreadsDim() -> retval
  ```
  """
  @spec maxThreadsDim(Evision.CUDA.DeviceInfo.t()) :: {integer(), integer(), integer()} | {:error, String.t()}
  def maxThreadsDim(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxThreadsDim(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxThreadsPerBlock

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  maxThreadsPerBlock() -> retval
  ```
  """
  @spec maxThreadsPerBlock(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def maxThreadsPerBlock(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxThreadsPerBlock(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  maxThreadsPerMultiProcessor

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  maxThreadsPerMultiProcessor() -> retval
  ```
  """
  @spec maxThreadsPerMultiProcessor(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def maxThreadsPerMultiProcessor(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_maxThreadsPerMultiProcessor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  memPitch

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  memPitch() -> retval
  ```
  """
  @spec memPitch(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def memPitch(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_memPitch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  memoryBusWidth

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  memoryBusWidth() -> retval
  ```
  """
  @spec memoryBusWidth(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def memoryBusWidth(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_memoryBusWidth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  memoryClockRate

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  memoryClockRate() -> retval
  ```
  """
  @spec memoryClockRate(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def memoryClockRate(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_memoryClockRate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  minorVersion

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  minorVersion() -> retval
  ```
  """
  @spec minorVersion(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def minorVersion(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_minorVersion(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  multiProcessorCount

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  multiProcessorCount() -> retval
  ```
  """
  @spec multiProcessorCount(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def multiProcessorCount(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_multiProcessorCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  pciBusID

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  pciBusID() -> retval
  ```
  """
  @spec pciBusID(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def pciBusID(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_pciBusID(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  pciDeviceID

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  pciDeviceID() -> retval
  ```
  """
  @spec pciDeviceID(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def pciDeviceID(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_pciDeviceID(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  pciDomainID

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  pciDomainID() -> retval
  ```
  """
  @spec pciDomainID(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def pciDomainID(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_pciDomainID(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  queryMemory

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`
  - **totalMemory**: `size_t`
  - **freeMemory**: `size_t`

  Python prototype (for reference only):
  ```python3
  queryMemory(totalMemory, freeMemory) -> None
  ```
  """
  @spec queryMemory(Evision.CUDA.DeviceInfo.t(), integer(), integer()) :: Evision.CUDA.DeviceInfo.t() | {:error, String.t()}
  def queryMemory(self, totalMemory, freeMemory) when is_integer(totalMemory) and is_integer(freeMemory)
  do
    positional = [
      totalMemory: Evision.Internal.Structurise.from_struct(totalMemory),
      freeMemory: Evision.Internal.Structurise.from_struct(freeMemory)
    ]
    :evision_nif.cuda_cuda_DeviceInfo_queryMemory(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  regsPerBlock

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  regsPerBlock() -> retval
  ```
  """
  @spec regsPerBlock(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def regsPerBlock(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_regsPerBlock(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  sharedMemPerBlock

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  sharedMemPerBlock() -> retval
  ```
  """
  @spec sharedMemPerBlock(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def sharedMemPerBlock(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_sharedMemPerBlock(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  surfaceAlignment

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  surfaceAlignment() -> retval
  ```
  """
  @spec surfaceAlignment(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def surfaceAlignment(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_surfaceAlignment(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  tccDriver

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  tccDriver() -> retval
  ```
  """
  @spec tccDriver(Evision.CUDA.DeviceInfo.t()) :: boolean() | {:error, String.t()}
  def tccDriver(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_tccDriver(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  textureAlignment

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  textureAlignment() -> retval
  ```
  """
  @spec textureAlignment(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def textureAlignment(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_textureAlignment(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  texturePitchAlignment

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  texturePitchAlignment() -> retval
  ```
  """
  @spec texturePitchAlignment(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def texturePitchAlignment(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_texturePitchAlignment(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  totalConstMem

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  totalConstMem() -> retval
  ```
  """
  @spec totalConstMem(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def totalConstMem(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_totalConstMem(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  totalGlobalMem

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  totalGlobalMem() -> retval
  ```
  """
  @spec totalGlobalMem(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def totalGlobalMem(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_totalGlobalMem(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  totalMemory

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `size_t`

  Python prototype (for reference only):
  ```python3
  totalMemory() -> retval
  ```
  """
  @spec totalMemory(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def totalMemory(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_totalMemory(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  unifiedAddressing

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  unifiedAddressing() -> retval
  ```
  """
  @spec unifiedAddressing(Evision.CUDA.DeviceInfo.t()) :: boolean() | {:error, String.t()}
  def unifiedAddressing(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_unifiedAddressing(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  warpSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DeviceInfo.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  warpSize() -> retval
  ```
  """
  @spec warpSize(Evision.CUDA.DeviceInfo.t()) :: integer() | {:error, String.t()}
  def warpSize(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DeviceInfo_warpSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
