defmodule Evision.PhaseUnwrapping.PhaseUnwrapping do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `PhaseUnwrapping.PhaseUnwrapping` struct.

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
  def to_struct({:ok, %{class: Evision.PhaseUnwrapping.PhaseUnwrapping, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.PhaseUnwrapping.PhaseUnwrapping, ref: ref}) do
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
  - **self**: `Evision.PhaseUnwrapping.PhaseUnwrapping.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.PhaseUnwrapping.PhaseUnwrapping.t()) :: Evision.PhaseUnwrapping.PhaseUnwrapping.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.phase_unwrapping_PhaseUnwrapping_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.PhaseUnwrapping.PhaseUnwrapping.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.PhaseUnwrapping.PhaseUnwrapping.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.phase_unwrapping_PhaseUnwrapping_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.PhaseUnwrapping.PhaseUnwrapping.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.PhaseUnwrapping.PhaseUnwrapping.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.phase_unwrapping_PhaseUnwrapping_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.PhaseUnwrapping.PhaseUnwrapping.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.PhaseUnwrapping.PhaseUnwrapping.t(), Evision.FileNode.t()) :: Evision.PhaseUnwrapping.PhaseUnwrapping.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.phase_unwrapping_PhaseUnwrapping_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.PhaseUnwrapping.PhaseUnwrapping.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.PhaseUnwrapping.PhaseUnwrapping.t(), binary()) :: Evision.PhaseUnwrapping.PhaseUnwrapping.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.phase_unwrapping_PhaseUnwrapping_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Unwraps a 2D phase map.

  ##### Positional Arguments
  - **self**: `Evision.PhaseUnwrapping.PhaseUnwrapping.t()`
  - **wrappedPhaseMap**: `Evision.Mat`.

    The wrapped phase map of type CV_32FC1 that needs to be unwrapped.

  ##### Keyword Arguments
  - **shadowMask**: `Evision.Mat`.

    Optional CV_8UC1 mask image used when some pixels do not hold any phase information in the wrapped phase map.

  ##### Return
  - **unwrappedPhaseMap**: `Evision.Mat.t()`.

    The unwrapped phase map.

  Python prototype (for reference only):
  ```python3
  unwrapPhaseMap(wrappedPhaseMap[, unwrappedPhaseMap[, shadowMask]]) -> unwrappedPhaseMap
  ```
  """
  @spec unwrapPhaseMap(Evision.PhaseUnwrapping.PhaseUnwrapping.t(), Evision.Mat.maybe_mat_in(), [{:shadowMask, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def unwrapPhaseMap(self, wrappedPhaseMap, opts) when (is_struct(wrappedPhaseMap, Evision.Mat) or is_struct(wrappedPhaseMap, Nx.Tensor) or is_number(wrappedPhaseMap) or is_tuple(wrappedPhaseMap)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:shadowMask])
    positional = [
      wrappedPhaseMap: Evision.Internal.Structurise.from_struct(wrappedPhaseMap)
    ]
    :evision_nif.phase_unwrapping_phase_unwrapping_PhaseUnwrapping_unwrapPhaseMap(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Unwraps a 2D phase map.

  ##### Positional Arguments
  - **self**: `Evision.PhaseUnwrapping.PhaseUnwrapping.t()`
  - **wrappedPhaseMap**: `Evision.Mat`.

    The wrapped phase map of type CV_32FC1 that needs to be unwrapped.

  ##### Keyword Arguments
  - **shadowMask**: `Evision.Mat`.

    Optional CV_8UC1 mask image used when some pixels do not hold any phase information in the wrapped phase map.

  ##### Return
  - **unwrappedPhaseMap**: `Evision.Mat.t()`.

    The unwrapped phase map.

  Python prototype (for reference only):
  ```python3
  unwrapPhaseMap(wrappedPhaseMap[, unwrappedPhaseMap[, shadowMask]]) -> unwrappedPhaseMap
  ```
  """
  @spec unwrapPhaseMap(Evision.PhaseUnwrapping.PhaseUnwrapping.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def unwrapPhaseMap(self, wrappedPhaseMap) when (is_struct(wrappedPhaseMap, Evision.Mat) or is_struct(wrappedPhaseMap, Nx.Tensor) or is_number(wrappedPhaseMap) or is_tuple(wrappedPhaseMap))
  do
    positional = [
      wrappedPhaseMap: Evision.Internal.Structurise.from_struct(wrappedPhaseMap)
    ]
    :evision_nif.phase_unwrapping_phase_unwrapping_PhaseUnwrapping_unwrapPhaseMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.PhaseUnwrapping.PhaseUnwrapping.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.PhaseUnwrapping.PhaseUnwrapping.t(), Evision.FileStorage.t(), binary()) :: Evision.PhaseUnwrapping.PhaseUnwrapping.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.phase_unwrapping_PhaseUnwrapping_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.PhaseUnwrapping.PhaseUnwrapping.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.PhaseUnwrapping.PhaseUnwrapping.t(), Evision.FileStorage.t()) :: Evision.PhaseUnwrapping.PhaseUnwrapping.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.phase_unwrapping_PhaseUnwrapping_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
