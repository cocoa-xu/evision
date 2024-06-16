defmodule Evision.CalibrateDebevec do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CalibrateDebevec` struct.

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
  def to_struct({:ok, %{class: Evision.CalibrateDebevec, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CalibrateDebevec, ref: ref}) do
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
  - **self**: `Evision.CalibrateDebevec.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CalibrateDebevec.t()) :: Evision.CalibrateDebevec.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.calibrateDebevec_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CalibrateDebevec.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.calibrateDebevec_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CalibrateDebevec.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.calibrateDebevec_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getLambda

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getLambda() -> retval
  ```
  """
  @spec getLambda(Evision.CalibrateDebevec.t()) :: number() | {:error, String.t()}
  def getLambda(self) do
    positional = [
    ]
    :evision_nif.calibrateDebevec_getLambda(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRandom

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getRandom() -> retval
  ```
  """
  @spec getRandom(Evision.CalibrateDebevec.t()) :: boolean() | {:error, String.t()}
  def getRandom(self) do
    positional = [
    ]
    :evision_nif.calibrateDebevec_getRandom(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSamples

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getSamples() -> retval
  ```
  """
  @spec getSamples(Evision.CalibrateDebevec.t()) :: integer() | {:error, String.t()}
  def getSamples(self) do
    positional = [
    ]
    :evision_nif.calibrateDebevec_getSamples(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Recovers inverse camera response.

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`
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
  @spec process(Evision.CalibrateDebevec.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src, times, opts) when is_list(src) and (is_struct(times, Evision.Mat) or is_struct(times, Nx.Tensor) or is_number(times) or is_tuple(times)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      times: Evision.Internal.Structurise.from_struct(times)
    ]
    :evision_nif.calibrateDebevec_process(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Recovers inverse camera response.

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`
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
  @spec process(Evision.CalibrateDebevec.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def process(self, src, times) when is_list(src) and (is_struct(times, Evision.Mat) or is_struct(times, Nx.Tensor) or is_number(times) or is_tuple(times))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      times: Evision.Internal.Structurise.from_struct(times)
    ]
    :evision_nif.calibrateDebevec_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CalibrateDebevec.t(), Evision.FileNode.t()) :: Evision.CalibrateDebevec.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.calibrateDebevec_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CalibrateDebevec.t(), binary()) :: Evision.CalibrateDebevec.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.calibrateDebevec_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setLambda

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`
  - **lambda**: `float`

  Python prototype (for reference only):
  ```python3
  setLambda(lambda) -> None
  ```
  """
  @spec setLambda(Evision.CalibrateDebevec.t(), number()) :: Evision.CalibrateDebevec.t() | {:error, String.t()}
  def setLambda(self, lambda) when is_float(lambda)
  do
    positional = [
      lambda: Evision.Internal.Structurise.from_struct(lambda)
    ]
    :evision_nif.calibrateDebevec_setLambda(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRandom

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`
  - **random**: `bool`

  Python prototype (for reference only):
  ```python3
  setRandom(random) -> None
  ```
  """
  @spec setRandom(Evision.CalibrateDebevec.t(), boolean()) :: Evision.CalibrateDebevec.t() | {:error, String.t()}
  def setRandom(self, random) when is_boolean(random)
  do
    positional = [
      random: Evision.Internal.Structurise.from_struct(random)
    ]
    :evision_nif.calibrateDebevec_setRandom(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSamples

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`
  - **samples**: `integer()`

  Python prototype (for reference only):
  ```python3
  setSamples(samples) -> None
  ```
  """
  @spec setSamples(Evision.CalibrateDebevec.t(), integer()) :: Evision.CalibrateDebevec.t() | {:error, String.t()}
  def setSamples(self, samples) when is_integer(samples)
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.calibrateDebevec_setSamples(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CalibrateDebevec.t(), Evision.FileStorage.t(), binary()) :: Evision.CalibrateDebevec.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.calibrateDebevec_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CalibrateDebevec.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CalibrateDebevec.t(), Evision.FileStorage.t()) :: Evision.CalibrateDebevec.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.calibrateDebevec_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
