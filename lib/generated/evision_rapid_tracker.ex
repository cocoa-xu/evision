defmodule Evision.Rapid.Tracker do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Rapid.Tracker` struct.

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
  def to_struct({:ok, %{class: Evision.Rapid.Tracker, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Rapid.Tracker, ref: ref}) do
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
  - **self**: `Evision.Rapid.Tracker.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Rapid.Tracker.t()) :: Evision.Rapid.Tracker.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.rapid_Tracker_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  clearState

  ##### Positional Arguments
  - **self**: `Evision.Rapid.Tracker.t()`

  Python prototype (for reference only):
  ```python3
  clearState() -> None
  ```
  """
  @spec clearState(Evision.Rapid.Tracker.t()) :: Evision.Rapid.Tracker.t() | {:error, String.t()}
  def clearState(self) do
    positional = [
    ]
    :evision_nif.rapid_rapid_Tracker_clearState(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  compute

  ##### Positional Arguments
  - **self**: `Evision.Rapid.Tracker.t()`
  - **img**: `Evision.Mat`
  - **num**: `integer()`
  - **len**: `integer()`
  - **k**: `Evision.Mat`

  ##### Keyword Arguments
  - **termcrit**: `TermCriteria`.

  ##### Return
  - **retval**: `float`
  - **rvec**: `Evision.Mat.t()`
  - **tvec**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  compute(img, num, len, K, rvec, tvec[, termcrit]) -> retval, rvec, tvec
  ```
  """
  @spec compute(Evision.Rapid.Tracker.t(), Evision.Mat.maybe_mat_in(), integer(), integer(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:termcrit, term()}] | nil) :: {number(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, img, num, len, k, rvec, tvec, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and is_integer(num) and is_integer(len) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(rvec, Evision.Mat) or is_struct(rvec, Nx.Tensor) or is_number(rvec) or is_tuple(rvec)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:termcrit])
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      num: Evision.Internal.Structurise.from_struct(num),
      len: Evision.Internal.Structurise.from_struct(len),
      k: Evision.Internal.Structurise.from_struct(k),
      rvec: Evision.Internal.Structurise.from_struct(rvec),
      tvec: Evision.Internal.Structurise.from_struct(tvec)
    ]
    :evision_nif.rapid_rapid_Tracker_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  compute

  ##### Positional Arguments
  - **self**: `Evision.Rapid.Tracker.t()`
  - **img**: `Evision.Mat`
  - **num**: `integer()`
  - **len**: `integer()`
  - **k**: `Evision.Mat`

  ##### Keyword Arguments
  - **termcrit**: `TermCriteria`.

  ##### Return
  - **retval**: `float`
  - **rvec**: `Evision.Mat.t()`
  - **tvec**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  compute(img, num, len, K, rvec, tvec[, termcrit]) -> retval, rvec, tvec
  ```
  """
  @spec compute(Evision.Rapid.Tracker.t(), Evision.Mat.maybe_mat_in(), integer(), integer(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, img, num, len, k, rvec, tvec) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and is_integer(num) and is_integer(len) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(rvec, Evision.Mat) or is_struct(rvec, Nx.Tensor) or is_number(rvec) or is_tuple(rvec)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      num: Evision.Internal.Structurise.from_struct(num),
      len: Evision.Internal.Structurise.from_struct(len),
      k: Evision.Internal.Structurise.from_struct(k),
      rvec: Evision.Internal.Structurise.from_struct(rvec),
      tvec: Evision.Internal.Structurise.from_struct(tvec)
    ]
    :evision_nif.rapid_rapid_Tracker_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.Rapid.Tracker.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Rapid.Tracker.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.rapid_Tracker_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.Rapid.Tracker.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.Rapid.Tracker.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.rapid_Tracker_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.Rapid.Tracker.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.Rapid.Tracker.t(), Evision.FileNode.t()) :: Evision.Rapid.Tracker.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.rapid_Tracker_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.Rapid.Tracker.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.Rapid.Tracker.t(), binary()) :: Evision.Rapid.Tracker.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.rapid_Tracker_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.Rapid.Tracker.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.Rapid.Tracker.t(), Evision.FileStorage.t(), binary()) :: Evision.Rapid.Tracker.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.rapid_Tracker_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.Rapid.Tracker.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.Rapid.Tracker.t(), Evision.FileStorage.t()) :: Evision.Rapid.Tracker.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.rapid_Tracker_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
