defmodule Evision.Plot.Plot2d do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Plot.Plot2d` struct.

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
  def to_struct({:ok, %{class: Evision.Plot.Plot2d, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Plot.Plot2d, ref: ref}) do
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
  - **self**: `Evision.Plot.Plot2d.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Plot.Plot2d.t()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.plot_Plot2d_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Creates Plot2d object

  ##### Positional Arguments
  - **dataX**: `Evision.Mat`.

    \\f$1xN\\f$ or \\f$Nx1\\f$ matrix \\f$X\\f$ values of points to plot.

  - **dataY**: `Evision.Mat`.

    \\f$1xN\\f$ or \\f$Nx1\\f$ matrix containing \\f$Y\\f$ values of points to plot.

  ##### Return
  - **retval**: `Plot2d`

  Python prototype (for reference only):
  ```python3
  create(dataX, dataY) -> retval
  ```
  """
  @spec create(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def create(dataX, dataY) when (is_struct(dataX, Evision.Mat) or is_struct(dataX, Nx.Tensor) or is_number(dataX) or is_tuple(dataX)) and (is_struct(dataY, Evision.Mat) or is_struct(dataY, Nx.Tensor) or is_number(dataY) or is_tuple(dataY))
  do
    positional = [
      dataX: Evision.Internal.Structurise.from_struct(dataX),
      dataY: Evision.Internal.Structurise.from_struct(dataY)
    ]
    :evision_nif.plot_plot_Plot2d_create_static(positional)
    |> to_struct()
  end

  @doc """
  Creates Plot2d object

  ##### Positional Arguments
  - **data**: `Evision.Mat`.

    \\f$1xN\\f$ or \\f$Nx1\\f$ matrix containing \\f$Y\\f$ values of points to plot. \\f$X\\f$ values
    will be equal to indexes of correspondind elements in data matrix.

  ##### Return
  - **retval**: `Plot2d`

  Python prototype (for reference only):
  ```python3
  create(data) -> retval
  ```
  """
  @spec create(Evision.Mat.maybe_mat_in()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def create(data) when (is_struct(data, Evision.Mat) or is_struct(data, Nx.Tensor) or is_number(data) or is_tuple(data))
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data)
    ]
    :evision_nif.plot_plot_Plot2d_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Plot.Plot2d.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.plot_Plot2d_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.Plot.Plot2d.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.plot_Plot2d_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.Plot.Plot2d.t(), Evision.FileNode.t()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.plot_Plot2d_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  render

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`

  ##### Return
  - **plotResult**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  render([, _plotResult]) -> _plotResult
  ```
  """
  @spec render(Evision.Plot.Plot2d.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def render(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.plot_plot_Plot2d_render(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  render

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`

  ##### Return
  - **plotResult**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  render([, _plotResult]) -> _plotResult
  ```
  """
  @spec render(Evision.Plot.Plot2d.t()) :: Evision.Mat.t() | {:error, String.t()}
  def render(self) do
    positional = [
    ]
    :evision_nif.plot_plot_Plot2d_render(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.Plot.Plot2d.t(), binary()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.plot_Plot2d_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setGridLinesNumber

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **gridLinesNumber**: `integer()`

  Python prototype (for reference only):
  ```python3
  setGridLinesNumber(gridLinesNumber) -> None
  ```
  """
  @spec setGridLinesNumber(Evision.Plot.Plot2d.t(), integer()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setGridLinesNumber(self, gridLinesNumber) when is_integer(gridLinesNumber)
  do
    positional = [
      gridLinesNumber: Evision.Internal.Structurise.from_struct(gridLinesNumber)
    ]
    :evision_nif.plot_plot_Plot2d_setGridLinesNumber(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setInvertOrientation

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **invertOrientation**: `bool`

  Python prototype (for reference only):
  ```python3
  setInvertOrientation(_invertOrientation) -> None
  ```
  """
  @spec setInvertOrientation(Evision.Plot.Plot2d.t(), boolean()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setInvertOrientation(self, invertOrientation) when is_boolean(invertOrientation)
  do
    positional = [
      invertOrientation: Evision.Internal.Structurise.from_struct(invertOrientation)
    ]
    :evision_nif.plot_plot_Plot2d_setInvertOrientation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxX

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **plotMaxX**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxX(_plotMaxX) -> None
  ```
  """
  @spec setMaxX(Evision.Plot.Plot2d.t(), number()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setMaxX(self, plotMaxX) when is_number(plotMaxX)
  do
    positional = [
      plotMaxX: Evision.Internal.Structurise.from_struct(plotMaxX)
    ]
    :evision_nif.plot_plot_Plot2d_setMaxX(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxY

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **plotMaxY**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxY(_plotMaxY) -> None
  ```
  """
  @spec setMaxY(Evision.Plot.Plot2d.t(), number()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setMaxY(self, plotMaxY) when is_number(plotMaxY)
  do
    positional = [
      plotMaxY: Evision.Internal.Structurise.from_struct(plotMaxY)
    ]
    :evision_nif.plot_plot_Plot2d_setMaxY(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinX

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **plotMinX**: `double`

  Python prototype (for reference only):
  ```python3
  setMinX(_plotMinX) -> None
  ```
  """
  @spec setMinX(Evision.Plot.Plot2d.t(), number()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setMinX(self, plotMinX) when is_number(plotMinX)
  do
    positional = [
      plotMinX: Evision.Internal.Structurise.from_struct(plotMinX)
    ]
    :evision_nif.plot_plot_Plot2d_setMinX(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinY

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **plotMinY**: `double`

  Python prototype (for reference only):
  ```python3
  setMinY(_plotMinY) -> None
  ```
  """
  @spec setMinY(Evision.Plot.Plot2d.t(), number()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setMinY(self, plotMinY) when is_number(plotMinY)
  do
    positional = [
      plotMinY: Evision.Internal.Structurise.from_struct(plotMinY)
    ]
    :evision_nif.plot_plot_Plot2d_setMinY(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Switches data visualization mode

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **needPlotLine**: `bool`.

    if true then neighbour plot points will be connected by lines.
    In other case data will be plotted as a set of standalone points.

  Python prototype (for reference only):
  ```python3
  setNeedPlotLine(_needPlotLine) -> None
  ```
  """
  @spec setNeedPlotLine(Evision.Plot.Plot2d.t(), boolean()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setNeedPlotLine(self, needPlotLine) when is_boolean(needPlotLine)
  do
    positional = [
      needPlotLine: Evision.Internal.Structurise.from_struct(needPlotLine)
    ]
    :evision_nif.plot_plot_Plot2d_setNeedPlotLine(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPlotAxisColor

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **plotAxisColor**: `Evision.scalar()`

  Python prototype (for reference only):
  ```python3
  setPlotAxisColor(_plotAxisColor) -> None
  ```
  """
  @spec setPlotAxisColor(Evision.Plot.Plot2d.t(), Evision.scalar()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setPlotAxisColor(self, plotAxisColor) when (is_number(plotAxisColor) or is_tuple(plotAxisColor))
  do
    positional = [
      plotAxisColor: Evision.Internal.Structurise.from_struct(plotAxisColor)
    ]
    :evision_nif.plot_plot_Plot2d_setPlotAxisColor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPlotBackgroundColor

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **plotBackgroundColor**: `Evision.scalar()`

  Python prototype (for reference only):
  ```python3
  setPlotBackgroundColor(_plotBackgroundColor) -> None
  ```
  """
  @spec setPlotBackgroundColor(Evision.Plot.Plot2d.t(), Evision.scalar()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setPlotBackgroundColor(self, plotBackgroundColor) when (is_number(plotBackgroundColor) or is_tuple(plotBackgroundColor))
  do
    positional = [
      plotBackgroundColor: Evision.Internal.Structurise.from_struct(plotBackgroundColor)
    ]
    :evision_nif.plot_plot_Plot2d_setPlotBackgroundColor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPlotGridColor

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **plotGridColor**: `Evision.scalar()`

  Python prototype (for reference only):
  ```python3
  setPlotGridColor(_plotGridColor) -> None
  ```
  """
  @spec setPlotGridColor(Evision.Plot.Plot2d.t(), Evision.scalar()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setPlotGridColor(self, plotGridColor) when (is_number(plotGridColor) or is_tuple(plotGridColor))
  do
    positional = [
      plotGridColor: Evision.Internal.Structurise.from_struct(plotGridColor)
    ]
    :evision_nif.plot_plot_Plot2d_setPlotGridColor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPlotLineColor

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **plotLineColor**: `Evision.scalar()`

  Python prototype (for reference only):
  ```python3
  setPlotLineColor(_plotLineColor) -> None
  ```
  """
  @spec setPlotLineColor(Evision.Plot.Plot2d.t(), Evision.scalar()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setPlotLineColor(self, plotLineColor) when (is_number(plotLineColor) or is_tuple(plotLineColor))
  do
    positional = [
      plotLineColor: Evision.Internal.Structurise.from_struct(plotLineColor)
    ]
    :evision_nif.plot_plot_Plot2d_setPlotLineColor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPlotLineWidth

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **plotLineWidth**: `integer()`

  Python prototype (for reference only):
  ```python3
  setPlotLineWidth(_plotLineWidth) -> None
  ```
  """
  @spec setPlotLineWidth(Evision.Plot.Plot2d.t(), integer()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setPlotLineWidth(self, plotLineWidth) when is_integer(plotLineWidth)
  do
    positional = [
      plotLineWidth: Evision.Internal.Structurise.from_struct(plotLineWidth)
    ]
    :evision_nif.plot_plot_Plot2d_setPlotLineWidth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPlotSize

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **plotSizeWidth**: `integer()`
  - **plotSizeHeight**: `integer()`

  Python prototype (for reference only):
  ```python3
  setPlotSize(_plotSizeWidth, _plotSizeHeight) -> None
  ```
  """
  @spec setPlotSize(Evision.Plot.Plot2d.t(), integer(), integer()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setPlotSize(self, plotSizeWidth, plotSizeHeight) when is_integer(plotSizeWidth) and is_integer(plotSizeHeight)
  do
    positional = [
      plotSizeWidth: Evision.Internal.Structurise.from_struct(plotSizeWidth),
      plotSizeHeight: Evision.Internal.Structurise.from_struct(plotSizeHeight)
    ]
    :evision_nif.plot_plot_Plot2d_setPlotSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPlotTextColor

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **plotTextColor**: `Evision.scalar()`

  Python prototype (for reference only):
  ```python3
  setPlotTextColor(_plotTextColor) -> None
  ```
  """
  @spec setPlotTextColor(Evision.Plot.Plot2d.t(), Evision.scalar()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setPlotTextColor(self, plotTextColor) when (is_number(plotTextColor) or is_tuple(plotTextColor))
  do
    positional = [
      plotTextColor: Evision.Internal.Structurise.from_struct(plotTextColor)
    ]
    :evision_nif.plot_plot_Plot2d_setPlotTextColor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the index of a point which coordinates will be printed on the top left corner of the plot (if ShowText flag is true).

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **pointIdx**: `integer()`.

    index of the required point in data array.

  Python prototype (for reference only):
  ```python3
  setPointIdxToPrint(pointIdx) -> None
  ```
  """
  @spec setPointIdxToPrint(Evision.Plot.Plot2d.t(), integer()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setPointIdxToPrint(self, pointIdx) when is_integer(pointIdx)
  do
    positional = [
      pointIdx: Evision.Internal.Structurise.from_struct(pointIdx)
    ]
    :evision_nif.plot_plot_Plot2d_setPointIdxToPrint(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setShowGrid

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **needShowGrid**: `bool`

  Python prototype (for reference only):
  ```python3
  setShowGrid(needShowGrid) -> None
  ```
  """
  @spec setShowGrid(Evision.Plot.Plot2d.t(), boolean()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setShowGrid(self, needShowGrid) when is_boolean(needShowGrid)
  do
    positional = [
      needShowGrid: Evision.Internal.Structurise.from_struct(needShowGrid)
    ]
    :evision_nif.plot_plot_Plot2d_setShowGrid(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setShowText

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **needShowText**: `bool`

  Python prototype (for reference only):
  ```python3
  setShowText(needShowText) -> None
  ```
  """
  @spec setShowText(Evision.Plot.Plot2d.t(), boolean()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def setShowText(self, needShowText) when is_boolean(needShowText)
  do
    positional = [
      needShowText: Evision.Internal.Structurise.from_struct(needShowText)
    ]
    :evision_nif.plot_plot_Plot2d_setShowText(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.Plot.Plot2d.t(), Evision.FileStorage.t(), binary()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.plot_Plot2d_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.Plot.Plot2d.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.Plot.Plot2d.t(), Evision.FileStorage.t()) :: Evision.Plot.Plot2d.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.plot_Plot2d_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
