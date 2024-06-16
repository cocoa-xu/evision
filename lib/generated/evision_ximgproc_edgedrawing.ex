defmodule Evision.XImgProc.EdgeDrawing do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.EdgeDrawing` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.EdgeDrawing, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.EdgeDrawing, ref: ref}) do
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
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.EdgeDrawing.t()) :: Evision.XImgProc.EdgeDrawing.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_EdgeDrawing_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Detects edges in a grayscale image and prepares them to detect lines and ellipses.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`
  - **src**: `Evision.Mat`.

    8-bit, single-channel, grayscale input image.

  Python prototype (for reference only):
  ```python3
  detectEdges(src) -> None
  ```
  """
  @spec detectEdges(Evision.XImgProc.EdgeDrawing.t(), Evision.Mat.maybe_mat_in()) :: Evision.XImgProc.EdgeDrawing.t() | {:error, String.t()}
  def detectEdges(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_ximgproc_EdgeDrawing_detectEdges(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Detects circles and ellipses.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`

  ##### Return
  - **ellipses**: `Evision.Mat.t()`.

    output Vec<6d> contains center point and perimeter for circles, center point, axes and angle for ellipses.

  **Note**: you should call detectEdges() before calling this function.

  Python prototype (for reference only):
  ```python3
  detectEllipses([, ellipses]) -> ellipses
  ```
  """
  @spec detectEllipses(Evision.XImgProc.EdgeDrawing.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def detectEllipses(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeDrawing_detectEllipses(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Detects circles and ellipses.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`

  ##### Return
  - **ellipses**: `Evision.Mat.t()`.

    output Vec<6d> contains center point and perimeter for circles, center point, axes and angle for ellipses.

  **Note**: you should call detectEdges() before calling this function.

  Python prototype (for reference only):
  ```python3
  detectEllipses([, ellipses]) -> ellipses
  ```
  """
  @spec detectEllipses(Evision.XImgProc.EdgeDrawing.t()) :: Evision.Mat.t() | {:error, String.t()}
  def detectEllipses(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeDrawing_detectEllipses(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Detects lines.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`

  ##### Return
  - **lines**: `Evision.Mat.t()`.

    output Vec<4f> contains the start point and the end point of detected lines.

  **Note**: you should call detectEdges() before calling this function.

  Python prototype (for reference only):
  ```python3
  detectLines([, lines]) -> lines
  ```
  """
  @spec detectLines(Evision.XImgProc.EdgeDrawing.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def detectLines(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeDrawing_detectLines(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Detects lines.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`

  ##### Return
  - **lines**: `Evision.Mat.t()`.

    output Vec<4f> contains the start point and the end point of detected lines.

  **Note**: you should call detectEdges() before calling this function.

  Python prototype (for reference only):
  ```python3
  detectLines([, lines]) -> lines
  ```
  """
  @spec detectLines(Evision.XImgProc.EdgeDrawing.t()) :: Evision.Mat.t() | {:error, String.t()}
  def detectLines(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeDrawing_detectLines(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.EdgeDrawing.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_EdgeDrawing_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.EdgeDrawing.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_EdgeDrawing_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  returns Edge Image prepared by detectEdges() function.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    returns 8-bit, single-channel output image.

  Python prototype (for reference only):
  ```python3
  getEdgeImage([, dst]) -> dst
  ```
  """
  @spec getEdgeImage(Evision.XImgProc.EdgeDrawing.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getEdgeImage(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeDrawing_getEdgeImage(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  returns Edge Image prepared by detectEdges() function.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    returns 8-bit, single-channel output image.

  Python prototype (for reference only):
  ```python3
  getEdgeImage([, dst]) -> dst
  ```
  """
  @spec getEdgeImage(Evision.XImgProc.EdgeDrawing.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getEdgeImage(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeDrawing_getEdgeImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  returns Gradient Image prepared by detectEdges() function.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    returns 16-bit, single-channel output image.

  Python prototype (for reference only):
  ```python3
  getGradientImage([, dst]) -> dst
  ```
  """
  @spec getGradientImage(Evision.XImgProc.EdgeDrawing.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getGradientImage(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeDrawing_getGradientImage(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  returns Gradient Image prepared by detectEdges() function.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    returns 16-bit, single-channel output image.

  Python prototype (for reference only):
  ```python3
  getGradientImage([, dst]) -> dst
  ```
  """
  @spec getGradientImage(Evision.XImgProc.EdgeDrawing.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getGradientImage(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeDrawing_getGradientImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns for each line found in detectLines() its edge segment index in getSegments()

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`

  ##### Return
  - **retval**: `[integer()]`

  Python prototype (for reference only):
  ```python3
  getSegmentIndicesOfLines() -> retval
  ```
  """
  @spec getSegmentIndicesOfLines(Evision.XImgProc.EdgeDrawing.t()) :: list(integer()) | {:error, String.t()}
  def getSegmentIndicesOfLines(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeDrawing_getSegmentIndicesOfLines(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns std::vector<std::vector<Point>> of detected edge segments, see detectEdges()

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`

  ##### Return
  - **retval**: `[[Point]]`

  Python prototype (for reference only):
  ```python3
  getSegments() -> retval
  ```
  """
  @spec getSegments(Evision.XImgProc.EdgeDrawing.t()) :: list(list({number(), number()})) | {:error, String.t()}
  def getSegments(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_EdgeDrawing_getSegments(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.EdgeDrawing.t(), Evision.FileNode.t()) :: Evision.XImgProc.EdgeDrawing.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_EdgeDrawing_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.EdgeDrawing.t(), binary()) :: Evision.XImgProc.EdgeDrawing.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_EdgeDrawing_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  sets parameters.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`
  - **parameters**: `Evision.XImgProc.EdgeDrawing.Params.t()`.

    Parameters of the algorithm

  this function is meant to be used for parameter setting in other languages than c++ like python.

  Python prototype (for reference only):
  ```python3
  setParams(parameters) -> None
  ```
  """
  @spec setParams(Evision.XImgProc.EdgeDrawing.t(), Evision.XImgProc.EdgeDrawing.Params.t()) :: Evision.XImgProc.EdgeDrawing.t() | {:error, String.t()}
  def setParams(self, parameters) when is_struct(parameters, Evision.XImgProc.EdgeDrawing.Params)
  do
    positional = [
      parameters: Evision.Internal.Structurise.from_struct(parameters)
    ]
    :evision_nif.ximgproc_ximgproc_EdgeDrawing_setParams(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.EdgeDrawing.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.EdgeDrawing.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_EdgeDrawing_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.EdgeDrawing.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.EdgeDrawing.t(), Evision.FileStorage.t()) :: Evision.XImgProc.EdgeDrawing.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_EdgeDrawing_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
