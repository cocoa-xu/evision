defmodule Evision.LineSegmentDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineSegmentDetector` struct.

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
  def to_struct({:ok, %{class: Evision.LineSegmentDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineSegmentDetector, ref: ref}) do
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
  - **self**: `Evision.LineSegmentDetector.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.LineSegmentDetector.t()) :: Evision.LineSegmentDetector.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.lineSegmentDetector_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Draws two groups of lines in blue and red, counting the non overlapping (mismatching) pixels.

  ##### Positional Arguments
  - **self**: `Evision.LineSegmentDetector.t()`
  - **size**: `Size`.

    The size of the image, where lines1 and lines2 were found.

  - **lines1**: `Evision.Mat`.

    The first group of lines that needs to be drawn. It is visualized in blue color.

  - **lines2**: `Evision.Mat`.

    The second group of lines. They visualized in red color.

  ##### Return
  - **retval**: `integer()`
  - **image**: `Evision.Mat.t()`.

    Optional image, where the lines will be drawn. The image should be color(3-channel)
    in order for lines1 and lines2 to be drawn in the above mentioned colors.

  Python prototype (for reference only):
  ```python3
  compareSegments(size, lines1, lines2[, image]) -> retval, image
  ```
  """
  @spec compareSegments(Evision.LineSegmentDetector.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {integer(), Evision.Mat.t()} | {:error, String.t()}
  def compareSegments(self, size, lines1, lines2, opts) when is_tuple(size) and (is_struct(lines1, Evision.Mat) or is_struct(lines1, Nx.Tensor) or is_number(lines1) or is_tuple(lines1)) and (is_struct(lines2, Evision.Mat) or is_struct(lines2, Nx.Tensor) or is_number(lines2) or is_tuple(lines2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      size: Evision.Internal.Structurise.from_struct(size),
      lines1: Evision.Internal.Structurise.from_struct(lines1),
      lines2: Evision.Internal.Structurise.from_struct(lines2)
    ]
    :evision_nif.lineSegmentDetector_compareSegments(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Draws two groups of lines in blue and red, counting the non overlapping (mismatching) pixels.

  ##### Positional Arguments
  - **self**: `Evision.LineSegmentDetector.t()`
  - **size**: `Size`.

    The size of the image, where lines1 and lines2 were found.

  - **lines1**: `Evision.Mat`.

    The first group of lines that needs to be drawn. It is visualized in blue color.

  - **lines2**: `Evision.Mat`.

    The second group of lines. They visualized in red color.

  ##### Return
  - **retval**: `integer()`
  - **image**: `Evision.Mat.t()`.

    Optional image, where the lines will be drawn. The image should be color(3-channel)
    in order for lines1 and lines2 to be drawn in the above mentioned colors.

  Python prototype (for reference only):
  ```python3
  compareSegments(size, lines1, lines2[, image]) -> retval, image
  ```
  """
  @spec compareSegments(Evision.LineSegmentDetector.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {integer(), Evision.Mat.t()} | {:error, String.t()}
  def compareSegments(self, size, lines1, lines2) when is_tuple(size) and (is_struct(lines1, Evision.Mat) or is_struct(lines1, Nx.Tensor) or is_number(lines1) or is_tuple(lines1)) and (is_struct(lines2, Evision.Mat) or is_struct(lines2, Nx.Tensor) or is_number(lines2) or is_tuple(lines2))
  do
    positional = [
      size: Evision.Internal.Structurise.from_struct(size),
      lines1: Evision.Internal.Structurise.from_struct(lines1),
      lines2: Evision.Internal.Structurise.from_struct(lines2)
    ]
    :evision_nif.lineSegmentDetector_compareSegments(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Finds lines in the input image.

  ##### Positional Arguments
  - **self**: `Evision.LineSegmentDetector.t()`
  - **image**: `Evision.Mat`.

    A grayscale (CV_8UC1) input image. If only a roi needs to be selected, use:
    `lsd_ptr-\\>detect(image(roi), lines, ...); lines += Scalar(roi.x, roi.y, roi.x, roi.y);`

  ##### Return
  - **lines**: `Evision.Mat.t()`.

    A vector of Vec4f elements specifying the beginning and ending point of a line. Where
    Vec4f is (x1, y1, x2, y2), point 1 is the start, point 2 - end. Returned lines are strictly
    oriented depending on the gradient.

  - **width**: `Evision.Mat.t()`.

    Vector of widths of the regions, where the lines are found. E.g. Width of line.

  - **prec**: `Evision.Mat.t()`.

    Vector of precisions with which the lines are found.

  - **nfa**: `Evision.Mat.t()`.

    Vector containing number of false alarms in the line region, with precision of 10%. The
    bigger the value, logarithmically better the detection.
    - -1 corresponds to 10 mean false alarms
    - 0 corresponds to 1 mean false alarm
    - 1 corresponds to 0.1 mean false alarms
      This vector will be calculated only when the objects type is #LSD_REFINE_ADV.

  This is the output of the default parameters of the algorithm on the above shown image.
  ![image](pics/building_lsd.png)

  Python prototype (for reference only):
  ```python3
  detect(image[, lines[, width[, prec[, nfa]]]]) -> lines, width, prec, nfa
  ```
  """
  @spec detect(Evision.LineSegmentDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.lineSegmentDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Finds lines in the input image.

  ##### Positional Arguments
  - **self**: `Evision.LineSegmentDetector.t()`
  - **image**: `Evision.Mat`.

    A grayscale (CV_8UC1) input image. If only a roi needs to be selected, use:
    `lsd_ptr-\\>detect(image(roi), lines, ...); lines += Scalar(roi.x, roi.y, roi.x, roi.y);`

  ##### Return
  - **lines**: `Evision.Mat.t()`.

    A vector of Vec4f elements specifying the beginning and ending point of a line. Where
    Vec4f is (x1, y1, x2, y2), point 1 is the start, point 2 - end. Returned lines are strictly
    oriented depending on the gradient.

  - **width**: `Evision.Mat.t()`.

    Vector of widths of the regions, where the lines are found. E.g. Width of line.

  - **prec**: `Evision.Mat.t()`.

    Vector of precisions with which the lines are found.

  - **nfa**: `Evision.Mat.t()`.

    Vector containing number of false alarms in the line region, with precision of 10%. The
    bigger the value, logarithmically better the detection.
    - -1 corresponds to 10 mean false alarms
    - 0 corresponds to 1 mean false alarm
    - 1 corresponds to 0.1 mean false alarms
      This vector will be calculated only when the objects type is #LSD_REFINE_ADV.

  This is the output of the default parameters of the algorithm on the above shown image.
  ![image](pics/building_lsd.png)

  Python prototype (for reference only):
  ```python3
  detect(image[, lines[, width[, prec[, nfa]]]]) -> lines, width, prec, nfa
  ```
  """
  @spec detect(Evision.LineSegmentDetector.t(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.lineSegmentDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Draws the line segments on a given image.

  ##### Positional Arguments
  - **self**: `Evision.LineSegmentDetector.t()`
  - **lines**: `Evision.Mat`.

    A vector of the lines that needed to be drawn.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    The image, where the lines will be drawn. Should be bigger or equal to the image,
    where the lines were found.

  Python prototype (for reference only):
  ```python3
  drawSegments(image, lines) -> image
  ```
  """
  @spec drawSegments(Evision.LineSegmentDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def drawSegments(self, image, lines) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(lines, Evision.Mat) or is_struct(lines, Nx.Tensor) or is_number(lines) or is_tuple(lines))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      lines: Evision.Internal.Structurise.from_struct(lines)
    ]
    :evision_nif.lineSegmentDetector_drawSegments(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.LineSegmentDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.LineSegmentDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.lineSegmentDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.LineSegmentDetector.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.LineSegmentDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.lineSegmentDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.LineSegmentDetector.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.LineSegmentDetector.t(), Evision.FileNode.t()) :: Evision.LineSegmentDetector.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.lineSegmentDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.LineSegmentDetector.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.LineSegmentDetector.t(), binary()) :: Evision.LineSegmentDetector.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.lineSegmentDetector_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.LineSegmentDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.LineSegmentDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.LineSegmentDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.lineSegmentDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.LineSegmentDetector.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.LineSegmentDetector.t(), Evision.FileStorage.t()) :: Evision.LineSegmentDetector.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.lineSegmentDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
