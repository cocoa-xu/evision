defmodule Evision.XImgProc.FastLineDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.FastLineDetector` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.FastLineDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.FastLineDetector, ref: ref}) do
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
  - **self**: `Evision.XImgProc.FastLineDetector.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.FastLineDetector.t()) :: Evision.XImgProc.FastLineDetector.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_FastLineDetector_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Finds lines in the input image.
  This is the output of the default parameters of the algorithm on the above
  shown image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastLineDetector.t()`
  - **image**: `Evision.Mat`.

    A grayscale (CV_8UC1) input image. If only a roi needs to be
    selected, use: `fld_ptr-\\>detect(image(roi), lines, ...);
    lines += Scalar(roi.x, roi.y, roi.x, roi.y);`

  ##### Return
  - **lines**: `Evision.Mat.t()`.

    A vector of Vec4f elements specifying the beginning
    and ending point of a line.  Where Vec4f is (x1, y1, x2, y2), point
    1 is the start, point 2 - end. Returned lines are directed so that the
    brighter side is on their left.

  ![image](pics/corridor_fld.jpg)

  Python prototype (for reference only):
  ```python3
  detect(image[, lines]) -> lines
  ```
  """
  @spec detect(Evision.XImgProc.FastLineDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.ximgproc_ximgproc_FastLineDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Finds lines in the input image.
  This is the output of the default parameters of the algorithm on the above
  shown image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastLineDetector.t()`
  - **image**: `Evision.Mat`.

    A grayscale (CV_8UC1) input image. If only a roi needs to be
    selected, use: `fld_ptr-\\>detect(image(roi), lines, ...);
    lines += Scalar(roi.x, roi.y, roi.x, roi.y);`

  ##### Return
  - **lines**: `Evision.Mat.t()`.

    A vector of Vec4f elements specifying the beginning
    and ending point of a line.  Where Vec4f is (x1, y1, x2, y2), point
    1 is the start, point 2 - end. Returned lines are directed so that the
    brighter side is on their left.

  ![image](pics/corridor_fld.jpg)

  Python prototype (for reference only):
  ```python3
  detect(image[, lines]) -> lines
  ```
  """
  @spec detect(Evision.XImgProc.FastLineDetector.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.ximgproc_ximgproc_FastLineDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Draws the line segments on a given image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastLineDetector.t()`
  - **lines**: `Evision.Mat`.

    A vector of the lines that needed to be drawn.

  ##### Keyword Arguments
  - **draw_arrow**: `bool`.

    If true, arrow heads will be drawn.

  - **linecolor**: `Evision.scalar()`.

    Line color.

  - **linethickness**: `integer()`.

    Line thickness.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    The image, where the lines will be drawn. Should be bigger
    or equal to the image, where the lines were found.

  Python prototype (for reference only):
  ```python3
  drawSegments(image, lines[, draw_arrow[, linecolor[, linethickness]]]) -> image
  ```
  """
  @spec drawSegments(Evision.XImgProc.FastLineDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:draw_arrow, term()} | {:linecolor, term()} | {:linethickness, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawSegments(self, image, lines, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(lines, Evision.Mat) or is_struct(lines, Nx.Tensor) or is_number(lines) or is_tuple(lines)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:draw_arrow, :linecolor, :linethickness])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      lines: Evision.Internal.Structurise.from_struct(lines)
    ]
    :evision_nif.ximgproc_ximgproc_FastLineDetector_drawSegments(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Draws the line segments on a given image.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastLineDetector.t()`
  - **lines**: `Evision.Mat`.

    A vector of the lines that needed to be drawn.

  ##### Keyword Arguments
  - **draw_arrow**: `bool`.

    If true, arrow heads will be drawn.

  - **linecolor**: `Evision.scalar()`.

    Line color.

  - **linethickness**: `integer()`.

    Line thickness.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    The image, where the lines will be drawn. Should be bigger
    or equal to the image, where the lines were found.

  Python prototype (for reference only):
  ```python3
  drawSegments(image, lines[, draw_arrow[, linecolor[, linethickness]]]) -> image
  ```
  """
  @spec drawSegments(Evision.XImgProc.FastLineDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def drawSegments(self, image, lines) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(lines, Evision.Mat) or is_struct(lines, Nx.Tensor) or is_number(lines) or is_tuple(lines))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      lines: Evision.Internal.Structurise.from_struct(lines)
    ]
    :evision_nif.ximgproc_ximgproc_FastLineDetector_drawSegments(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastLineDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.FastLineDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_FastLineDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastLineDetector.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.FastLineDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_FastLineDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastLineDetector.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.FastLineDetector.t(), Evision.FileNode.t()) :: Evision.XImgProc.FastLineDetector.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_FastLineDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastLineDetector.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.FastLineDetector.t(), binary()) :: Evision.XImgProc.FastLineDetector.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_FastLineDetector_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastLineDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.FastLineDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.FastLineDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_FastLineDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.FastLineDetector.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.FastLineDetector.t(), Evision.FileStorage.t()) :: Evision.XImgProc.FastLineDetector.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_FastLineDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
