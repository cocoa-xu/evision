defmodule Evision.GeneralizedHough do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `GeneralizedHough` struct.

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
  def to_struct({:ok, %{class: Evision.GeneralizedHough, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.GeneralizedHough, ref: ref}) do
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
  - **self**: `Evision.GeneralizedHough.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.GeneralizedHough.t()) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.generalizedHough_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detect

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **edges**: `Evision.Mat`
  - **dx**: `Evision.Mat`
  - **dy**: `Evision.Mat`

  ##### Return
  - **positions**: `Evision.Mat.t()`.
  - **votes**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  detect(edges, dx, dy[, positions[, votes]]) -> positions, votes
  ```
  """
  @spec detect(Evision.GeneralizedHough.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detect(self, edges, dx, dy, opts) when (is_struct(edges, Evision.Mat) or is_struct(edges, Nx.Tensor) or is_number(edges) or is_tuple(edges)) and (is_struct(dx, Evision.Mat) or is_struct(dx, Nx.Tensor) or is_number(dx) or is_tuple(dx)) and (is_struct(dy, Evision.Mat) or is_struct(dy, Nx.Tensor) or is_number(dy) or is_tuple(dy)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      edges: Evision.Internal.Structurise.from_struct(edges),
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy)
    ]
    :evision_nif.generalizedHough_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detect

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **edges**: `Evision.Mat`
  - **dx**: `Evision.Mat`
  - **dy**: `Evision.Mat`

  ##### Return
  - **positions**: `Evision.Mat.t()`.
  - **votes**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  detect(edges, dx, dy[, positions[, votes]]) -> positions, votes
  ```
  """
  @spec detect(Evision.GeneralizedHough.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detect(self, edges, dx, dy) when (is_struct(edges, Evision.Mat) or is_struct(edges, Nx.Tensor) or is_number(edges) or is_tuple(edges)) and (is_struct(dx, Evision.Mat) or is_struct(dx, Nx.Tensor) or is_number(dx) or is_tuple(dx)) and (is_struct(dy, Evision.Mat) or is_struct(dy, Nx.Tensor) or is_number(dy) or is_tuple(dy))
  do
    positional = [
      edges: Evision.Internal.Structurise.from_struct(edges),
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy)
    ]
    :evision_nif.generalizedHough_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detect

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **image**: `Evision.Mat`

  ##### Return
  - **positions**: `Evision.Mat.t()`.
  - **votes**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  detect(image[, positions[, votes]]) -> positions, votes
  ```
  """
  @spec detect(Evision.GeneralizedHough.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.generalizedHough_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detect

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **image**: `Evision.Mat`

  ##### Return
  - **positions**: `Evision.Mat.t()`.
  - **votes**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  detect(image[, positions[, votes]]) -> positions, votes
  ```
  """
  @spec detect(Evision.GeneralizedHough.t(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.generalizedHough_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.GeneralizedHough.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.generalizedHough_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCannyHighThresh

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getCannyHighThresh() -> retval
  ```
  """
  @spec getCannyHighThresh(Evision.GeneralizedHough.t()) :: integer() | {:error, String.t()}
  def getCannyHighThresh(self) do
    positional = [
    ]
    :evision_nif.generalizedHough_getCannyHighThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCannyLowThresh

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getCannyLowThresh() -> retval
  ```
  """
  @spec getCannyLowThresh(Evision.GeneralizedHough.t()) :: integer() | {:error, String.t()}
  def getCannyLowThresh(self) do
    positional = [
    ]
    :evision_nif.generalizedHough_getCannyLowThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.GeneralizedHough.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.generalizedHough_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDp

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getDp() -> retval
  ```
  """
  @spec getDp(Evision.GeneralizedHough.t()) :: number() | {:error, String.t()}
  def getDp(self) do
    positional = [
    ]
    :evision_nif.generalizedHough_getDp(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxBufferSize

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxBufferSize() -> retval
  ```
  """
  @spec getMaxBufferSize(Evision.GeneralizedHough.t()) :: integer() | {:error, String.t()}
  def getMaxBufferSize(self) do
    positional = [
    ]
    :evision_nif.generalizedHough_getMaxBufferSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinDist

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMinDist() -> retval
  ```
  """
  @spec getMinDist(Evision.GeneralizedHough.t()) :: number() | {:error, String.t()}
  def getMinDist(self) do
    positional = [
    ]
    :evision_nif.generalizedHough_getMinDist(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.GeneralizedHough.t(), Evision.FileNode.t()) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.generalizedHough_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.GeneralizedHough.t(), binary()) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.generalizedHough_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCannyHighThresh

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **cannyHighThresh**: `integer()`

  Python prototype (for reference only):
  ```python3
  setCannyHighThresh(cannyHighThresh) -> None
  ```
  """
  @spec setCannyHighThresh(Evision.GeneralizedHough.t(), integer()) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def setCannyHighThresh(self, cannyHighThresh) when is_integer(cannyHighThresh)
  do
    positional = [
      cannyHighThresh: Evision.Internal.Structurise.from_struct(cannyHighThresh)
    ]
    :evision_nif.generalizedHough_setCannyHighThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCannyLowThresh

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **cannyLowThresh**: `integer()`

  Python prototype (for reference only):
  ```python3
  setCannyLowThresh(cannyLowThresh) -> None
  ```
  """
  @spec setCannyLowThresh(Evision.GeneralizedHough.t(), integer()) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def setCannyLowThresh(self, cannyLowThresh) when is_integer(cannyLowThresh)
  do
    positional = [
      cannyLowThresh: Evision.Internal.Structurise.from_struct(cannyLowThresh)
    ]
    :evision_nif.generalizedHough_setCannyLowThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDp

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **dp**: `double`

  Python prototype (for reference only):
  ```python3
  setDp(dp) -> None
  ```
  """
  @spec setDp(Evision.GeneralizedHough.t(), number()) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def setDp(self, dp) when is_number(dp)
  do
    positional = [
      dp: Evision.Internal.Structurise.from_struct(dp)
    ]
    :evision_nif.generalizedHough_setDp(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxBufferSize

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **maxBufferSize**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxBufferSize(maxBufferSize) -> None
  ```
  """
  @spec setMaxBufferSize(Evision.GeneralizedHough.t(), integer()) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def setMaxBufferSize(self, maxBufferSize) when is_integer(maxBufferSize)
  do
    positional = [
      maxBufferSize: Evision.Internal.Structurise.from_struct(maxBufferSize)
    ]
    :evision_nif.generalizedHough_setMaxBufferSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinDist

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **minDist**: `double`

  Python prototype (for reference only):
  ```python3
  setMinDist(minDist) -> None
  ```
  """
  @spec setMinDist(Evision.GeneralizedHough.t(), number()) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def setMinDist(self, minDist) when is_number(minDist)
  do
    positional = [
      minDist: Evision.Internal.Structurise.from_struct(minDist)
    ]
    :evision_nif.generalizedHough_setMinDist(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTemplate

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **edges**: `Evision.Mat`
  - **dx**: `Evision.Mat`
  - **dy**: `Evision.Mat`

  ##### Keyword Arguments
  - **templCenter**: `Point`.

  Python prototype (for reference only):
  ```python3
  setTemplate(edges, dx, dy[, templCenter]) -> None
  ```
  """
  @spec setTemplate(Evision.GeneralizedHough.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:templCenter, term()}] | nil) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def setTemplate(self, edges, dx, dy, opts) when (is_struct(edges, Evision.Mat) or is_struct(edges, Nx.Tensor) or is_number(edges) or is_tuple(edges)) and (is_struct(dx, Evision.Mat) or is_struct(dx, Nx.Tensor) or is_number(dx) or is_tuple(dx)) and (is_struct(dy, Evision.Mat) or is_struct(dy, Nx.Tensor) or is_number(dy) or is_tuple(dy)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:templCenter])
    positional = [
      edges: Evision.Internal.Structurise.from_struct(edges),
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy)
    ]
    :evision_nif.generalizedHough_setTemplate(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  setTemplate

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **edges**: `Evision.Mat`
  - **dx**: `Evision.Mat`
  - **dy**: `Evision.Mat`

  ##### Keyword Arguments
  - **templCenter**: `Point`.

  Python prototype (for reference only):
  ```python3
  setTemplate(edges, dx, dy[, templCenter]) -> None
  ```
  """
  @spec setTemplate(Evision.GeneralizedHough.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def setTemplate(self, edges, dx, dy) when (is_struct(edges, Evision.Mat) or is_struct(edges, Nx.Tensor) or is_number(edges) or is_tuple(edges)) and (is_struct(dx, Evision.Mat) or is_struct(dx, Nx.Tensor) or is_number(dx) or is_tuple(dx)) and (is_struct(dy, Evision.Mat) or is_struct(dy, Nx.Tensor) or is_number(dy) or is_tuple(dy))
  do
    positional = [
      edges: Evision.Internal.Structurise.from_struct(edges),
      dx: Evision.Internal.Structurise.from_struct(dx),
      dy: Evision.Internal.Structurise.from_struct(dy)
    ]
    :evision_nif.generalizedHough_setTemplate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTemplate

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **templ**: `Evision.Mat`

  ##### Keyword Arguments
  - **templCenter**: `Point`.

  Python prototype (for reference only):
  ```python3
  setTemplate(templ[, templCenter]) -> None
  ```
  """
  @spec setTemplate(Evision.GeneralizedHough.t(), Evision.Mat.maybe_mat_in(), [{:templCenter, term()}] | nil) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def setTemplate(self, templ, opts) when (is_struct(templ, Evision.Mat) or is_struct(templ, Nx.Tensor) or is_number(templ) or is_tuple(templ)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:templCenter])
    positional = [
      templ: Evision.Internal.Structurise.from_struct(templ)
    ]
    :evision_nif.generalizedHough_setTemplate(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  setTemplate

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **templ**: `Evision.Mat`

  ##### Keyword Arguments
  - **templCenter**: `Point`.

  Python prototype (for reference only):
  ```python3
  setTemplate(templ[, templCenter]) -> None
  ```
  """
  @spec setTemplate(Evision.GeneralizedHough.t(), Evision.Mat.maybe_mat_in()) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def setTemplate(self, templ) when (is_struct(templ, Evision.Mat) or is_struct(templ, Nx.Tensor) or is_number(templ) or is_tuple(templ))
  do
    positional = [
      templ: Evision.Internal.Structurise.from_struct(templ)
    ]
    :evision_nif.generalizedHough_setTemplate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.GeneralizedHough.t(), Evision.FileStorage.t(), binary()) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.generalizedHough_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHough.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.GeneralizedHough.t(), Evision.FileStorage.t()) :: Evision.GeneralizedHough.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.generalizedHough_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
