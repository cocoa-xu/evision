defmodule Evision.Face.LBPHFaceRecognizer do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Face.LBPHFaceRecognizer` struct.

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
  def to_struct({:ok, %{class: Evision.Face.LBPHFaceRecognizer, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Face.LBPHFaceRecognizer, ref: ref}) do
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
  create
  ##### Keyword Arguments
  - **radius**: `integer()`.

    The radius used for building the Circular Local Binary Pattern. The greater the
    radius, the smoother the image but more spatial information you can get.

  - **neighbors**: `integer()`.

    The number of sample points to build a Circular Local Binary Pattern from. An
    appropriate value is to use `8` sample points. Keep in mind: the more sample points you include,
    the higher the computational cost.

  - **grid_x**: `integer()`.

    The number of cells in the horizontal direction, 8 is a common value used in
    publications. The more cells, the finer the grid, the higher the dimensionality of the resulting
    feature vector.

  - **grid_y**: `integer()`.

    The number of cells in the vertical direction, 8 is a common value used in
    publications. The more cells, the finer the grid, the higher the dimensionality of the resulting
    feature vector.

  - **threshold**: `double`.

    The threshold applied in the prediction. If the distance to the nearest neighbor
    is larger than the threshold, this method returns -1.

  ##### Return
  - **retval**: `LBPHFaceRecognizer`

  ### Notes:
  - The Circular Local Binary Patterns (used in training and prediction) expect the data given as
    grayscale images, use cvtColor to convert between the color spaces.

  - This model supports updating.

  ### Model internal data:
  - radius see LBPHFaceRecognizer::create.
  - neighbors see LBPHFaceRecognizer::create.
  - grid_x see LLBPHFaceRecognizer::create.
  - grid_y see LBPHFaceRecognizer::create.
  - threshold see LBPHFaceRecognizer::create.
  - histograms Local Binary Patterns Histograms calculated from the given training data (empty if
    none was given).

  - labels Labels corresponding to the calculated Local Binary Patterns Histograms.

  Python prototype (for reference only):
  ```python3
  create([, radius[, neighbors[, grid_x[, grid_y[, threshold]]]]]) -> retval
  ```
  """
  @spec create([{:grid_x, term()} | {:grid_y, term()} | {:neighbors, term()} | {:radius, term()} | {:threshold, term()}] | nil) :: Evision.Face.LBPHFaceRecognizer.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:grid_x, :grid_y, :neighbors, :radius, :threshold])
    positional = [
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **radius**: `integer()`.

    The radius used for building the Circular Local Binary Pattern. The greater the
    radius, the smoother the image but more spatial information you can get.

  - **neighbors**: `integer()`.

    The number of sample points to build a Circular Local Binary Pattern from. An
    appropriate value is to use `8` sample points. Keep in mind: the more sample points you include,
    the higher the computational cost.

  - **grid_x**: `integer()`.

    The number of cells in the horizontal direction, 8 is a common value used in
    publications. The more cells, the finer the grid, the higher the dimensionality of the resulting
    feature vector.

  - **grid_y**: `integer()`.

    The number of cells in the vertical direction, 8 is a common value used in
    publications. The more cells, the finer the grid, the higher the dimensionality of the resulting
    feature vector.

  - **threshold**: `double`.

    The threshold applied in the prediction. If the distance to the nearest neighbor
    is larger than the threshold, this method returns -1.

  ##### Return
  - **retval**: `LBPHFaceRecognizer`

  ### Notes:
  - The Circular Local Binary Patterns (used in training and prediction) expect the data given as
    grayscale images, use cvtColor to convert between the color spaces.

  - This model supports updating.

  ### Model internal data:
  - radius see LBPHFaceRecognizer::create.
  - neighbors see LBPHFaceRecognizer::create.
  - grid_x see LLBPHFaceRecognizer::create.
  - grid_y see LBPHFaceRecognizer::create.
  - threshold see LBPHFaceRecognizer::create.
  - histograms Local Binary Patterns Histograms calculated from the given training data (empty if
    none was given).

  - labels Labels corresponding to the calculated Local Binary Patterns Histograms.

  Python prototype (for reference only):
  ```python3
  create([, radius[, neighbors[, grid_x[, grid_y[, threshold]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.Face.LBPHFaceRecognizer.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_create_static(positional)
    |> to_struct()
  end

  @doc """
  getGridX

  ##### Positional Arguments
  - **self**: `Evision.Face.LBPHFaceRecognizer.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setGridX/2`

  Python prototype (for reference only):
  ```python3
  getGridX() -> retval
  ```
  """
  @spec getGridX(Evision.Face.LBPHFaceRecognizer.t()) :: integer() | {:error, String.t()}
  def getGridX(self) do
    positional = [
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_getGridX(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getGridY

  ##### Positional Arguments
  - **self**: `Evision.Face.LBPHFaceRecognizer.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setGridY/2`

  Python prototype (for reference only):
  ```python3
  getGridY() -> retval
  ```
  """
  @spec getGridY(Evision.Face.LBPHFaceRecognizer.t()) :: integer() | {:error, String.t()}
  def getGridY(self) do
    positional = [
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_getGridY(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getHistograms

  ##### Positional Arguments
  - **self**: `Evision.Face.LBPHFaceRecognizer.t()`

  ##### Return
  - **retval**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  getHistograms() -> retval
  ```
  """
  @spec getHistograms(Evision.Face.LBPHFaceRecognizer.t()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getHistograms(self) do
    positional = [
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_getHistograms(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getLabels

  ##### Positional Arguments
  - **self**: `Evision.Face.LBPHFaceRecognizer.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getLabels() -> retval
  ```
  """
  @spec getLabels(Evision.Face.LBPHFaceRecognizer.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getLabels(self) do
    positional = [
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_getLabels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNeighbors

  ##### Positional Arguments
  - **self**: `Evision.Face.LBPHFaceRecognizer.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setNeighbors/2`

  Python prototype (for reference only):
  ```python3
  getNeighbors() -> retval
  ```
  """
  @spec getNeighbors(Evision.Face.LBPHFaceRecognizer.t()) :: integer() | {:error, String.t()}
  def getNeighbors(self) do
    positional = [
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_getNeighbors(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRadius

  ##### Positional Arguments
  - **self**: `Evision.Face.LBPHFaceRecognizer.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setRadius/2`

  Python prototype (for reference only):
  ```python3
  getRadius() -> retval
  ```
  """
  @spec getRadius(Evision.Face.LBPHFaceRecognizer.t()) :: integer() | {:error, String.t()}
  def getRadius(self) do
    positional = [
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_getRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getThreshold

  ##### Positional Arguments
  - **self**: `Evision.Face.LBPHFaceRecognizer.t()`

  ##### Return
  - **retval**: `double`

  @see `setThreshold/2`

  Python prototype (for reference only):
  ```python3
  getThreshold() -> retval
  ```
  """
  @spec getThreshold(Evision.Face.LBPHFaceRecognizer.t()) :: number() | {:error, String.t()}
  def getThreshold(self) do
    positional = [
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_getThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setGridX

  ##### Positional Arguments
  - **self**: `Evision.Face.LBPHFaceRecognizer.t()`
  - **val**: `integer()`

  @see `getGridX/1`

  Python prototype (for reference only):
  ```python3
  setGridX(val) -> None
  ```
  """
  @spec setGridX(Evision.Face.LBPHFaceRecognizer.t(), integer()) :: Evision.Face.LBPHFaceRecognizer.t() | {:error, String.t()}
  def setGridX(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_setGridX(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setGridY

  ##### Positional Arguments
  - **self**: `Evision.Face.LBPHFaceRecognizer.t()`
  - **val**: `integer()`

  @see `getGridY/1`

  Python prototype (for reference only):
  ```python3
  setGridY(val) -> None
  ```
  """
  @spec setGridY(Evision.Face.LBPHFaceRecognizer.t(), integer()) :: Evision.Face.LBPHFaceRecognizer.t() | {:error, String.t()}
  def setGridY(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_setGridY(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNeighbors

  ##### Positional Arguments
  - **self**: `Evision.Face.LBPHFaceRecognizer.t()`
  - **val**: `integer()`

  @see `getNeighbors/1`

  Python prototype (for reference only):
  ```python3
  setNeighbors(val) -> None
  ```
  """
  @spec setNeighbors(Evision.Face.LBPHFaceRecognizer.t(), integer()) :: Evision.Face.LBPHFaceRecognizer.t() | {:error, String.t()}
  def setNeighbors(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_setNeighbors(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRadius

  ##### Positional Arguments
  - **self**: `Evision.Face.LBPHFaceRecognizer.t()`
  - **val**: `integer()`

  @see `getRadius/1`

  Python prototype (for reference only):
  ```python3
  setRadius(val) -> None
  ```
  """
  @spec setRadius(Evision.Face.LBPHFaceRecognizer.t(), integer()) :: Evision.Face.LBPHFaceRecognizer.t() | {:error, String.t()}
  def setRadius(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_setRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setThreshold

  ##### Positional Arguments
  - **self**: `Evision.Face.LBPHFaceRecognizer.t()`
  - **val**: `double`

  @see `getThreshold/1`

  Python prototype (for reference only):
  ```python3
  setThreshold(val) -> None
  ```
  """
  @spec setThreshold(Evision.Face.LBPHFaceRecognizer.t(), number()) :: Evision.Face.LBPHFaceRecognizer.t() | {:error, String.t()}
  def setThreshold(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.face_face_LBPHFaceRecognizer_setThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
