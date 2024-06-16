defmodule Evision.VideoIORegistry do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `VideoIORegistry` struct.

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
  def to_struct({:ok, %{class: Evision.VideoIORegistry, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.VideoIORegistry, ref: ref}) do
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
  Returns backend API name or "UnknownVideoAPI(xxx)"

  ##### Positional Arguments
  - **api**: `VideoCaptureAPIs`.

    backend ID (#VideoCaptureAPIs)

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  getBackendName(api) -> retval
  ```
  """
  @spec getBackendName(Evision.VideoCaptureAPIs.enum()) :: binary() | {:error, String.t()}
  def getBackendName(api) when is_integer(api)
  do
    positional = [
      api: Evision.Internal.Structurise.from_struct(api)
    ]
    :evision_nif.videoio_registry_getBackendName(positional)
    |> to_struct()
  end

  @doc """
  Returns list of all available backends
  ##### Return
  - **retval**: `[VideoCaptureAPIs]`

  Python prototype (for reference only):
  ```python3
  getBackends() -> retval
  ```
  """
  @spec getBackends() :: list(Evision.VideoCaptureAPIs.enum()) | {:error, String.t()}
  def getBackends() do
    positional = [
    ]
    :evision_nif.videoio_registry_getBackends(positional)
    |> to_struct()
  end

  @doc """
  Returns description and ABI/API version of videoio plugin's camera interface

  ##### Positional Arguments
  - **api**: `VideoCaptureAPIs`

  ##### Return
  - **retval**: `string`
  - **version_ABI**: `integer()`
  - **version_API**: `integer()`

  Python prototype (for reference only):
  ```python3
  getCameraBackendPluginVersion(api) -> retval, version_ABI, version_API
  ```
  """
  @spec getCameraBackendPluginVersion(Evision.VideoCaptureAPIs.enum()) :: {binary(), integer(), integer()} | {:error, String.t()}
  def getCameraBackendPluginVersion(api) when is_integer(api)
  do
    positional = [
      api: Evision.Internal.Structurise.from_struct(api)
    ]
    :evision_nif.videoio_registry_getCameraBackendPluginVersion(positional)
    |> to_struct()
  end

  @doc """
  Returns list of available backends which works via `cv::VideoCapture(int index)`
  ##### Return
  - **retval**: `[VideoCaptureAPIs]`

  Python prototype (for reference only):
  ```python3
  getCameraBackends() -> retval
  ```
  """
  @spec getCameraBackends() :: list(Evision.VideoCaptureAPIs.enum()) | {:error, String.t()}
  def getCameraBackends() do
    positional = [
    ]
    :evision_nif.videoio_registry_getCameraBackends(positional)
    |> to_struct()
  end

  @doc """
  Returns description and ABI/API version of videoio plugin's stream capture interface

  ##### Positional Arguments
  - **api**: `VideoCaptureAPIs`

  ##### Return
  - **retval**: `string`
  - **version_ABI**: `integer()`
  - **version_API**: `integer()`

  Python prototype (for reference only):
  ```python3
  getStreamBackendPluginVersion(api) -> retval, version_ABI, version_API
  ```
  """
  @spec getStreamBackendPluginVersion(Evision.VideoCaptureAPIs.enum()) :: {binary(), integer(), integer()} | {:error, String.t()}
  def getStreamBackendPluginVersion(api) when is_integer(api)
  do
    positional = [
      api: Evision.Internal.Structurise.from_struct(api)
    ]
    :evision_nif.videoio_registry_getStreamBackendPluginVersion(positional)
    |> to_struct()
  end

  @doc """
  Returns list of available backends which works via `cv::VideoCapture(filename)`
  ##### Return
  - **retval**: `[VideoCaptureAPIs]`

  Python prototype (for reference only):
  ```python3
  getStreamBackends() -> retval
  ```
  """
  @spec getStreamBackends() :: list(Evision.VideoCaptureAPIs.enum()) | {:error, String.t()}
  def getStreamBackends() do
    positional = [
    ]
    :evision_nif.videoio_registry_getStreamBackends(positional)
    |> to_struct()
  end

  @doc """
  Returns description and ABI/API version of videoio plugin's writer interface

  ##### Positional Arguments
  - **api**: `VideoCaptureAPIs`

  ##### Return
  - **retval**: `string`
  - **version_ABI**: `integer()`
  - **version_API**: `integer()`

  Python prototype (for reference only):
  ```python3
  getWriterBackendPluginVersion(api) -> retval, version_ABI, version_API
  ```
  """
  @spec getWriterBackendPluginVersion(Evision.VideoCaptureAPIs.enum()) :: {binary(), integer(), integer()} | {:error, String.t()}
  def getWriterBackendPluginVersion(api) when is_integer(api)
  do
    positional = [
      api: Evision.Internal.Structurise.from_struct(api)
    ]
    :evision_nif.videoio_registry_getWriterBackendPluginVersion(positional)
    |> to_struct()
  end

  @doc """
  Returns list of available backends which works via `cv::VideoWriter()`
  ##### Return
  - **retval**: `[VideoCaptureAPIs]`

  Python prototype (for reference only):
  ```python3
  getWriterBackends() -> retval
  ```
  """
  @spec getWriterBackends() :: list(Evision.VideoCaptureAPIs.enum()) | {:error, String.t()}
  def getWriterBackends() do
    positional = [
    ]
    :evision_nif.videoio_registry_getWriterBackends(positional)
    |> to_struct()
  end

  @doc """
  Returns true if backend is available

  ##### Positional Arguments
  - **api**: `VideoCaptureAPIs`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  hasBackend(api) -> retval
  ```
  """
  @spec hasBackend(Evision.VideoCaptureAPIs.enum()) :: boolean() | {:error, String.t()}
  def hasBackend(api) when is_integer(api)
  do
    positional = [
      api: Evision.Internal.Structurise.from_struct(api)
    ]
    :evision_nif.videoio_registry_hasBackend(positional)
    |> to_struct()
  end

  @doc """
  Returns true if backend is built in (false if backend is used as plugin)

  ##### Positional Arguments
  - **api**: `VideoCaptureAPIs`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isBackendBuiltIn(api) -> retval
  ```
  """
  @spec isBackendBuiltIn(Evision.VideoCaptureAPIs.enum()) :: boolean() | {:error, String.t()}
  def isBackendBuiltIn(api) when is_integer(api)
  do
    positional = [
      api: Evision.Internal.Structurise.from_struct(api)
    ]
    :evision_nif.videoio_registry_isBackendBuiltIn(positional)
    |> to_struct()
  end
end
