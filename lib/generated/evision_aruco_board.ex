defmodule Evision.ArUco.Board do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ArUco.Board` struct.

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
  def to_struct({:ok, %{class: Evision.ArUco.Board, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ArUco.Board, ref: ref}) do
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
  Common Board constructor

  ##### Positional Arguments
  - **objPoints**: `[Evision.Mat]`.

    array of object points of all the marker corners in the board

  - **dictionary**: `Dictionary`.

    the dictionary of markers employed for this board

  - **ids**: `Evision.Mat`.

    vector of the identifiers of the markers in the board

  ##### Return
  - **self**: `Board`

  Python prototype (for reference only):
  ```python3
  Board(objPoints, dictionary, ids) -> <aruco_Board object>
  ```
  """
  @spec board(list(Evision.Mat.maybe_mat_in()), Evision.ArUco.Dictionary.t(), Evision.Mat.maybe_mat_in()) :: Evision.ArUco.Board.t() | {:error, String.t()}
  def board(objPoints, dictionary, ids) when is_list(objPoints) and is_struct(dictionary, Evision.ArUco.Dictionary) and (is_struct(ids, Evision.Mat) or is_struct(ids, Nx.Tensor) or is_number(ids) or is_tuple(ids))
  do
    positional = [
      objPoints: Evision.Internal.Structurise.from_struct(objPoints),
      dictionary: Evision.Internal.Structurise.from_struct(dictionary),
      ids: Evision.Internal.Structurise.from_struct(ids)
    ]
    :evision_nif.aruco_aruco_Board_Board(positional)
    |> to_struct()
  end

  @doc """
  Draw a planar board

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Board.t()`
  - **outSize**: `Size`.

    size of the output image in pixels.

  ##### Keyword Arguments
  - **marginSize**: `integer()`.

    minimum margins (in pixels) of the board in the output image

  - **borderBits**: `integer()`.

    width of the marker borders.

  ##### Return
  - **img**: `Evision.Mat.t()`.

    output image with the board. The size of this image will be outSize
    and the board will be on the center, keeping the board proportions.

   This function return the image of the board, ready to be printed.

  Python prototype (for reference only):
  ```python3
  generateImage(outSize[, img[, marginSize[, borderBits]]]) -> img
  ```
  """
  @spec generateImage(Evision.ArUco.Board.t(), {number(), number()}, [{:borderBits, term()} | {:marginSize, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def generateImage(self, outSize, opts) when is_tuple(outSize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderBits, :marginSize])
    positional = [
      outSize: Evision.Internal.Structurise.from_struct(outSize)
    ]
    :evision_nif.aruco_aruco_Board_generateImage(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Draw a planar board

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Board.t()`
  - **outSize**: `Size`.

    size of the output image in pixels.

  ##### Keyword Arguments
  - **marginSize**: `integer()`.

    minimum margins (in pixels) of the board in the output image

  - **borderBits**: `integer()`.

    width of the marker borders.

  ##### Return
  - **img**: `Evision.Mat.t()`.

    output image with the board. The size of this image will be outSize
    and the board will be on the center, keeping the board proportions.

   This function return the image of the board, ready to be printed.

  Python prototype (for reference only):
  ```python3
  generateImage(outSize[, img[, marginSize[, borderBits]]]) -> img
  ```
  """
  @spec generateImage(Evision.ArUco.Board.t(), {number(), number()}) :: Evision.Mat.t() | {:error, String.t()}
  def generateImage(self, outSize) when is_tuple(outSize)
  do
    positional = [
      outSize: Evision.Internal.Structurise.from_struct(outSize)
    ]
    :evision_nif.aruco_aruco_Board_generateImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  return the Dictionary of markers employed for this board

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Board.t()`

  ##### Return
  - **retval**: `Dictionary`

  Python prototype (for reference only):
  ```python3
  getDictionary() -> retval
  ```
  """
  @spec getDictionary(Evision.ArUco.Board.t()) :: Evision.ArUco.Dictionary.t() | {:error, String.t()}
  def getDictionary(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_Board_getDictionary(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  vector of the identifiers of the markers in the board (should be the same size as objPoints)

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Board.t()`

  ##### Return
  - **retval**: `[integer()]`

  @return vector of the identifiers of the markers

  Python prototype (for reference only):
  ```python3
  getIds() -> retval
  ```
  """
  @spec getIds(Evision.ArUco.Board.t()) :: list(integer()) | {:error, String.t()}
  def getIds(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_Board_getIds(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  return array of object points of all the marker corners in the board.

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Board.t()`

  ##### Return
  - **retval**: `[[Point3f]]`

   Each marker include its 4 corners in this order:
  - objPoints[i][0] - left-top point of i-th marker
  - objPoints[i][1] - right-top point of i-th marker
  - objPoints[i][2] - right-bottom point of i-th marker
  - objPoints[i][3] - left-bottom point of i-th marker

   Markers are placed in a certain order - row by row, left to right in every row. For M markers, the size is Mx4.

  Python prototype (for reference only):
  ```python3
  getObjPoints() -> retval
  ```
  """
  @spec getObjPoints(Evision.ArUco.Board.t()) :: list(list({number(), number(), number()})) | {:error, String.t()}
  def getObjPoints(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_Board_getObjPoints(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  get coordinate of the bottom right corner of the board, is set when calling the function create()

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Board.t()`

  ##### Return
  - **retval**: `Point3f`

  Python prototype (for reference only):
  ```python3
  getRightBottomCorner() -> retval
  ```
  """
  @spec getRightBottomCorner(Evision.ArUco.Board.t()) :: {number(), number(), number()} | {:error, String.t()}
  def getRightBottomCorner(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_Board_getRightBottomCorner(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Given a board configuration and a set of detected markers, returns the corresponding
  image points and object points, can be used in solvePnP()

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Board.t()`
  - **detectedCorners**: `[Evision.Mat]`.

    List of detected marker corners of the board.
    For cv::Board and cv::GridBoard the method expects std::vector<std::vector<Point2f>> or std::vector<Mat> with Aruco marker corners.
    For cv::CharucoBoard the method expects std::vector<Point2f> or Mat with ChAruco corners (chess board corners matched with Aruco markers).

  - **detectedIds**: `Evision.Mat`.

    List of identifiers for each marker or charuco corner.
    For any Board class the method expects std::vector<int> or Mat.

  ##### Return
  - **objPoints**: `Evision.Mat.t()`.

    Vector of marker points in the board coordinate space.
    For any Board class the method expects std::vector<cv::Point3f> objectPoints or cv::Mat

  - **imgPoints**: `Evision.Mat.t()`.

    Vector of marker points in the image coordinate space.
    For any Board class the method expects std::vector<cv::Point2f> objectPoints or cv::Mat

  @sa solvePnP

  Python prototype (for reference only):
  ```python3
  matchImagePoints(detectedCorners, detectedIds[, objPoints[, imgPoints]]) -> objPoints, imgPoints
  ```
  """
  @spec matchImagePoints(Evision.ArUco.Board.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def matchImagePoints(self, detectedCorners, detectedIds, opts) when is_list(detectedCorners) and (is_struct(detectedIds, Evision.Mat) or is_struct(detectedIds, Nx.Tensor) or is_number(detectedIds) or is_tuple(detectedIds)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      detectedCorners: Evision.Internal.Structurise.from_struct(detectedCorners),
      detectedIds: Evision.Internal.Structurise.from_struct(detectedIds)
    ]
    :evision_nif.aruco_aruco_Board_matchImagePoints(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Given a board configuration and a set of detected markers, returns the corresponding
  image points and object points, can be used in solvePnP()

  ##### Positional Arguments
  - **self**: `Evision.ArUco.Board.t()`
  - **detectedCorners**: `[Evision.Mat]`.

    List of detected marker corners of the board.
    For cv::Board and cv::GridBoard the method expects std::vector<std::vector<Point2f>> or std::vector<Mat> with Aruco marker corners.
    For cv::CharucoBoard the method expects std::vector<Point2f> or Mat with ChAruco corners (chess board corners matched with Aruco markers).

  - **detectedIds**: `Evision.Mat`.

    List of identifiers for each marker or charuco corner.
    For any Board class the method expects std::vector<int> or Mat.

  ##### Return
  - **objPoints**: `Evision.Mat.t()`.

    Vector of marker points in the board coordinate space.
    For any Board class the method expects std::vector<cv::Point3f> objectPoints or cv::Mat

  - **imgPoints**: `Evision.Mat.t()`.

    Vector of marker points in the image coordinate space.
    For any Board class the method expects std::vector<cv::Point2f> objectPoints or cv::Mat

  @sa solvePnP

  Python prototype (for reference only):
  ```python3
  matchImagePoints(detectedCorners, detectedIds[, objPoints[, imgPoints]]) -> objPoints, imgPoints
  ```
  """
  @spec matchImagePoints(Evision.ArUco.Board.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def matchImagePoints(self, detectedCorners, detectedIds) when is_list(detectedCorners) and (is_struct(detectedIds, Evision.Mat) or is_struct(detectedIds, Nx.Tensor) or is_number(detectedIds) or is_tuple(detectedIds))
  do
    positional = [
      detectedCorners: Evision.Internal.Structurise.from_struct(detectedCorners),
      detectedIds: Evision.Internal.Structurise.from_struct(detectedIds)
    ]
    :evision_nif.aruco_aruco_Board_matchImagePoints(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
