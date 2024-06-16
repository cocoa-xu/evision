defmodule Evision.CalibrateRobertson do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CalibrateRobertson` struct.

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
  def to_struct({:ok, %{class: Evision.CalibrateRobertson, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CalibrateRobertson, ref: ref}) do
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
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CalibrateRobertson.t()) :: Evision.CalibrateRobertson.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.calibrateRobertson_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CalibrateRobertson.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.calibrateRobertson_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CalibrateRobertson.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.calibrateRobertson_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxIter

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxIter() -> retval
  ```
  """
  @spec getMaxIter(Evision.CalibrateRobertson.t()) :: integer() | {:error, String.t()}
  def getMaxIter(self) do
    positional = [
    ]
    :evision_nif.calibrateRobertson_getMaxIter(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRadiance

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getRadiance() -> retval
  ```
  """
  @spec getRadiance(Evision.CalibrateRobertson.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getRadiance(self) do
    positional = [
    ]
    :evision_nif.calibrateRobertson_getRadiance(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getThreshold

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getThreshold() -> retval
  ```
  """
  @spec getThreshold(Evision.CalibrateRobertson.t()) :: number() | {:error, String.t()}
  def getThreshold(self) do
    positional = [
    ]
    :evision_nif.calibrateRobertson_getThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Recovers inverse camera response.

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`
  - **src**: `[Evision.Mat]`.

    vector of input images

  - **times**: `Evision.Mat`.

    vector of exposure time values for each image

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    256x1 matrix with inverse camera response function

  Python prototype (for reference only):
  ```python3
  process(src, times[, dst]) -> dst
  ```
  """
  @spec process(Evision.CalibrateRobertson.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src, times, opts) when is_list(src) and (is_struct(times, Evision.Mat) or is_struct(times, Nx.Tensor) or is_number(times) or is_tuple(times)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      times: Evision.Internal.Structurise.from_struct(times)
    ]
    :evision_nif.calibrateRobertson_process(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Recovers inverse camera response.

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`
  - **src**: `[Evision.Mat]`.

    vector of input images

  - **times**: `Evision.Mat`.

    vector of exposure time values for each image

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    256x1 matrix with inverse camera response function

  Python prototype (for reference only):
  ```python3
  process(src, times[, dst]) -> dst
  ```
  """
  @spec process(Evision.CalibrateRobertson.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src, times) when is_list(src) and (is_struct(times, Evision.Mat) or is_struct(times, Nx.Tensor) or is_number(times) or is_tuple(times))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      times: Evision.Internal.Structurise.from_struct(times)
    ]
    :evision_nif.calibrateRobertson_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CalibrateRobertson.t(), Evision.FileNode.t()) :: Evision.CalibrateRobertson.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.calibrateRobertson_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CalibrateRobertson.t(), binary()) :: Evision.CalibrateRobertson.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.calibrateRobertson_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxIter

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`
  - **max_iter**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxIter(max_iter) -> None
  ```
  """
  @spec setMaxIter(Evision.CalibrateRobertson.t(), integer()) :: Evision.CalibrateRobertson.t() | {:error, String.t()}
  def setMaxIter(self, max_iter) when is_integer(max_iter)
  do
    positional = [
      max_iter: Evision.Internal.Structurise.from_struct(max_iter)
    ]
    :evision_nif.calibrateRobertson_setMaxIter(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setThreshold

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`
  - **threshold**: `float`

  Python prototype (for reference only):
  ```python3
  setThreshold(threshold) -> None
  ```
  """
  @spec setThreshold(Evision.CalibrateRobertson.t(), number()) :: Evision.CalibrateRobertson.t() | {:error, String.t()}
  def setThreshold(self, threshold) when is_float(threshold)
  do
    positional = [
      threshold: Evision.Internal.Structurise.from_struct(threshold)
    ]
    :evision_nif.calibrateRobertson_setThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CalibrateRobertson.t(), Evision.FileStorage.t(), binary()) :: Evision.CalibrateRobertson.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.calibrateRobertson_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CalibrateRobertson.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CalibrateRobertson.t(), Evision.FileStorage.t()) :: Evision.CalibrateRobertson.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.calibrateRobertson_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
