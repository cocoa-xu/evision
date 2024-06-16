defmodule Evision.SparseOpticalFlow do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `SparseOpticalFlow` struct.

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
  def to_struct({:ok, %{class: Evision.SparseOpticalFlow, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.SparseOpticalFlow, ref: ref}) do
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
  Calculates a sparse optical flow.

  ##### Positional Arguments
  - **self**: `Evision.SparseOpticalFlow.t()`
  - **prevImg**: `Evision.Mat`.

    First input image.

  - **nextImg**: `Evision.Mat`.

    Second input image of the same size and the same type as prevImg.

  - **prevPts**: `Evision.Mat`.

    Vector of 2D points for which the flow needs to be found.

  ##### Return
  - **nextPts**: `Evision.Mat.t()`.

    Output vector of 2D points containing the calculated new positions of input features in the second image.

  - **status**: `Evision.Mat.t()`.

    Output status vector. Each element of the vector is set to 1 if the
    flow for the corresponding features has been found. Otherwise, it is set to 0.

  - **err**: `Evision.Mat.t()`.

    Optional output vector that contains error response for each point (inverse confidence).

  Python prototype (for reference only):
  ```python3
  calc(prevImg, nextImg, prevPts, nextPts[, status[, err]]) -> nextPts, status, err
  ```
  """
  @spec calc(Evision.SparseOpticalFlow.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def calc(self, prevImg, nextImg, prevPts, nextPts, opts) when (is_struct(prevImg, Evision.Mat) or is_struct(prevImg, Nx.Tensor) or is_number(prevImg) or is_tuple(prevImg)) and (is_struct(nextImg, Evision.Mat) or is_struct(nextImg, Nx.Tensor) or is_number(nextImg) or is_tuple(nextImg)) and (is_struct(prevPts, Evision.Mat) or is_struct(prevPts, Nx.Tensor) or is_number(prevPts) or is_tuple(prevPts)) and (is_struct(nextPts, Evision.Mat) or is_struct(nextPts, Nx.Tensor) or is_number(nextPts) or is_tuple(nextPts)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      prevImg: Evision.Internal.Structurise.from_struct(prevImg),
      nextImg: Evision.Internal.Structurise.from_struct(nextImg),
      prevPts: Evision.Internal.Structurise.from_struct(prevPts),
      nextPts: Evision.Internal.Structurise.from_struct(nextPts)
    ]
    :evision_nif.sparseOpticalFlow_calc(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Calculates a sparse optical flow.

  ##### Positional Arguments
  - **self**: `Evision.SparseOpticalFlow.t()`
  - **prevImg**: `Evision.Mat`.

    First input image.

  - **nextImg**: `Evision.Mat`.

    Second input image of the same size and the same type as prevImg.

  - **prevPts**: `Evision.Mat`.

    Vector of 2D points for which the flow needs to be found.

  ##### Return
  - **nextPts**: `Evision.Mat.t()`.

    Output vector of 2D points containing the calculated new positions of input features in the second image.

  - **status**: `Evision.Mat.t()`.

    Output status vector. Each element of the vector is set to 1 if the
    flow for the corresponding features has been found. Otherwise, it is set to 0.

  - **err**: `Evision.Mat.t()`.

    Optional output vector that contains error response for each point (inverse confidence).

  Python prototype (for reference only):
  ```python3
  calc(prevImg, nextImg, prevPts, nextPts[, status[, err]]) -> nextPts, status, err
  ```
  """
  @spec calc(Evision.SparseOpticalFlow.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def calc(self, prevImg, nextImg, prevPts, nextPts) when (is_struct(prevImg, Evision.Mat) or is_struct(prevImg, Nx.Tensor) or is_number(prevImg) or is_tuple(prevImg)) and (is_struct(nextImg, Evision.Mat) or is_struct(nextImg, Nx.Tensor) or is_number(nextImg) or is_tuple(nextImg)) and (is_struct(prevPts, Evision.Mat) or is_struct(prevPts, Nx.Tensor) or is_number(prevPts) or is_tuple(prevPts)) and (is_struct(nextPts, Evision.Mat) or is_struct(nextPts, Nx.Tensor) or is_number(nextPts) or is_tuple(nextPts))
  do
    positional = [
      prevImg: Evision.Internal.Structurise.from_struct(prevImg),
      nextImg: Evision.Internal.Structurise.from_struct(nextImg),
      prevPts: Evision.Internal.Structurise.from_struct(prevPts),
      nextPts: Evision.Internal.Structurise.from_struct(nextPts)
    ]
    :evision_nif.sparseOpticalFlow_calc(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.SparseOpticalFlow.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.SparseOpticalFlow.t()) :: Evision.SparseOpticalFlow.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.sparseOpticalFlow_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.SparseOpticalFlow.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.SparseOpticalFlow.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.sparseOpticalFlow_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.SparseOpticalFlow.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.SparseOpticalFlow.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.sparseOpticalFlow_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.SparseOpticalFlow.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.SparseOpticalFlow.t(), Evision.FileNode.t()) :: Evision.SparseOpticalFlow.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.sparseOpticalFlow_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.SparseOpticalFlow.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.SparseOpticalFlow.t(), binary()) :: Evision.SparseOpticalFlow.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.sparseOpticalFlow_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.SparseOpticalFlow.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.SparseOpticalFlow.t(), Evision.FileStorage.t(), binary()) :: Evision.SparseOpticalFlow.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.sparseOpticalFlow_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.SparseOpticalFlow.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.SparseOpticalFlow.t(), Evision.FileStorage.t()) :: Evision.SparseOpticalFlow.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.sparseOpticalFlow_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
