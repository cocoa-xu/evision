defmodule Evision.ArUco.CharucoDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ArUco.CharucoDetector` struct.

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
  def to_struct({:ok, %{class: Evision.ArUco.CharucoDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ArUco.CharucoDetector, ref: ref}) do
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
  Basic CharucoDetector constructor

  ##### Positional Arguments
  - **board**: `Evision.ArUco.CharucoBoard`.

    ChAruco board

  ##### Keyword Arguments
  - **charucoParams**: `CharucoParameters`.

    charuco detection parameters

  - **detectorParams**: `DetectorParameters`.

    marker detection parameters

  - **refineParams**: `RefineParameters`.

    marker refine detection parameters

  ##### Return
  - **self**: `CharucoDetector`

  Python prototype (for reference only):
  ```python3
  CharucoDetector(board[, charucoParams[, detectorParams[, refineParams]]]) -> <aruco_CharucoDetector object>
  ```
  """
  @spec charucoDetector(Evision.ArUco.CharucoBoard.t(), [{:charucoParams, term()} | {:detectorParams, term()} | {:refineParams, term()}] | nil) :: Evision.ArUco.CharucoDetector.t() | {:error, String.t()}
  def charucoDetector(board, opts) when is_struct(board, Evision.ArUco.CharucoBoard) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:charucoParams, :detectorParams, :refineParams])
    positional = [
      board: Evision.Internal.Structurise.from_struct(board)
    ]
    :evision_nif.aruco_aruco_CharucoDetector_CharucoDetector(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Basic CharucoDetector constructor

  ##### Positional Arguments
  - **board**: `Evision.ArUco.CharucoBoard`.

    ChAruco board

  ##### Keyword Arguments
  - **charucoParams**: `CharucoParameters`.

    charuco detection parameters

  - **detectorParams**: `DetectorParameters`.

    marker detection parameters

  - **refineParams**: `RefineParameters`.

    marker refine detection parameters

  ##### Return
  - **self**: `CharucoDetector`

  Python prototype (for reference only):
  ```python3
  CharucoDetector(board[, charucoParams[, detectorParams[, refineParams]]]) -> <aruco_CharucoDetector object>
  ```
  """
  @spec charucoDetector(Evision.ArUco.CharucoBoard.t()) :: Evision.ArUco.CharucoDetector.t() | {:error, String.t()}
  def charucoDetector(board) when is_struct(board, Evision.ArUco.CharucoBoard)
  do
    positional = [
      board: Evision.Internal.Structurise.from_struct(board)
    ]
    :evision_nif.aruco_aruco_CharucoDetector_CharucoDetector(positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.ArUco.CharucoDetector.t()) :: Evision.ArUco.CharucoDetector.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.aruco_CharucoDetector_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detect aruco markers and interpolate position of ChArUco board corners

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`
  - **image**: `Evision.Mat`.

    input image necesary for corner refinement. Note that markers are not detected and
    should be sent in corners and ids parameters.

  ##### Return
  - **charucoCorners**: `Evision.Mat.t()`.

    interpolated chessboard corners.

  - **charucoIds**: `Evision.Mat.t()`.

    interpolated chessboard corners identifiers.

  - **markerCorners**: `[Evision.Mat]`.

    vector of already detected markers corners. For each marker, its four
    corners are provided, (e.g std::vector<std::vector<cv::Point2f> > ). For N detected markers, the
    dimensions of this array should be Nx4. The order of the corners should be clockwise.
    If markerCorners and markerCorners are empty, the function detect aruco markers and ids.

  - **markerIds**: `Evision.Mat.t()`.

    list of identifiers for each marker in corners.
    If markerCorners and markerCorners are empty, the function detect aruco markers and ids.

   This function receives the detected markers and returns the 2D position of the chessboard corners
   from a ChArUco board using the detected Aruco markers.
   If markerCorners and markerCorners are empty, the detectMarkers() will run and detect aruco markers and ids.
   If camera parameters are provided, the process is based in an approximated pose estimation, else it is based on local homography.
   Only visible corners are returned. For each corner, its corresponding identifier is also returned in charucoIds.
  @sa findChessboardCorners
  **Note**: After OpenCV 4.6.0, there was an incompatible change in the ChArUco pattern generation algorithm for even row counts.
   Use cv::aruco::CharucoBoard::setLegacyPattern() to ensure compatibility with patterns created using OpenCV versions prior to 4.6.0.
   For more information, see the issue: https://github.com/opencv/opencv/issues/23152

  Python prototype (for reference only):
  ```python3
  detectBoard(image[, charucoCorners[, charucoIds[, markerCorners[, markerIds]]]]) -> charucoCorners, charucoIds, markerCorners, markerIds
  ```
  """
  @spec detectBoard(Evision.ArUco.CharucoDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectBoard(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.aruco_aruco_CharucoDetector_detectBoard(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detect aruco markers and interpolate position of ChArUco board corners

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`
  - **image**: `Evision.Mat`.

    input image necesary for corner refinement. Note that markers are not detected and
    should be sent in corners and ids parameters.

  ##### Return
  - **charucoCorners**: `Evision.Mat.t()`.

    interpolated chessboard corners.

  - **charucoIds**: `Evision.Mat.t()`.

    interpolated chessboard corners identifiers.

  - **markerCorners**: `[Evision.Mat]`.

    vector of already detected markers corners. For each marker, its four
    corners are provided, (e.g std::vector<std::vector<cv::Point2f> > ). For N detected markers, the
    dimensions of this array should be Nx4. The order of the corners should be clockwise.
    If markerCorners and markerCorners are empty, the function detect aruco markers and ids.

  - **markerIds**: `Evision.Mat.t()`.

    list of identifiers for each marker in corners.
    If markerCorners and markerCorners are empty, the function detect aruco markers and ids.

   This function receives the detected markers and returns the 2D position of the chessboard corners
   from a ChArUco board using the detected Aruco markers.
   If markerCorners and markerCorners are empty, the detectMarkers() will run and detect aruco markers and ids.
   If camera parameters are provided, the process is based in an approximated pose estimation, else it is based on local homography.
   Only visible corners are returned. For each corner, its corresponding identifier is also returned in charucoIds.
  @sa findChessboardCorners
  **Note**: After OpenCV 4.6.0, there was an incompatible change in the ChArUco pattern generation algorithm for even row counts.
   Use cv::aruco::CharucoBoard::setLegacyPattern() to ensure compatibility with patterns created using OpenCV versions prior to 4.6.0.
   For more information, see the issue: https://github.com/opencv/opencv/issues/23152

  Python prototype (for reference only):
  ```python3
  detectBoard(image[, charucoCorners[, charucoIds[, markerCorners[, markerIds]]]]) -> charucoCorners, charucoIds, markerCorners, markerIds
  ```
  """
  @spec detectBoard(Evision.ArUco.CharucoDetector.t(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectBoard(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.aruco_aruco_CharucoDetector_detectBoard(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Detect ChArUco Diamond markers

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`
  - **image**: `Evision.Mat`.

    input image necessary for corner subpixel.

  ##### Return
  - **diamondCorners**: `[Evision.Mat]`.

    output list of detected diamond corners (4 corners per diamond). The order
    is the same than in marker corners: top left, top right, bottom right and bottom left. Similar
    format than the corners returned by detectMarkers (e.g std::vector<std::vector<cv::Point2f> > ).

  - **diamondIds**: `Evision.Mat.t()`.

    ids of the diamonds in diamondCorners. The id of each diamond is in fact of
    type Vec4i, so each diamond has 4 ids, which are the ids of the aruco markers composing the
    diamond.

  - **markerCorners**: `[Evision.Mat]`.

    list of detected marker corners from detectMarkers function.
    If markerCorners and markerCorners are empty, the function detect aruco markers and ids.

  - **markerIds**: `Evision.Mat.t()`.

    list of marker ids in markerCorners.
    If markerCorners and markerCorners are empty, the function detect aruco markers and ids.

   This function detects Diamond markers from the previous detected ArUco markers. The diamonds
   are returned in the diamondCorners and diamondIds parameters. If camera calibration parameters
   are provided, the diamond search is based on reprojection. If not, diamond search is based on
   homography. Homography is faster than reprojection, but less accurate.

  Python prototype (for reference only):
  ```python3
  detectDiamonds(image[, diamondCorners[, diamondIds[, markerCorners[, markerIds]]]]) -> diamondCorners, diamondIds, markerCorners, markerIds
  ```
  """
  @spec detectDiamonds(Evision.ArUco.CharucoDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {list(Evision.Mat.t()), Evision.Mat.t(), list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectDiamonds(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.aruco_aruco_CharucoDetector_detectDiamonds(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Detect ChArUco Diamond markers

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`
  - **image**: `Evision.Mat`.

    input image necessary for corner subpixel.

  ##### Return
  - **diamondCorners**: `[Evision.Mat]`.

    output list of detected diamond corners (4 corners per diamond). The order
    is the same than in marker corners: top left, top right, bottom right and bottom left. Similar
    format than the corners returned by detectMarkers (e.g std::vector<std::vector<cv::Point2f> > ).

  - **diamondIds**: `Evision.Mat.t()`.

    ids of the diamonds in diamondCorners. The id of each diamond is in fact of
    type Vec4i, so each diamond has 4 ids, which are the ids of the aruco markers composing the
    diamond.

  - **markerCorners**: `[Evision.Mat]`.

    list of detected marker corners from detectMarkers function.
    If markerCorners and markerCorners are empty, the function detect aruco markers and ids.

  - **markerIds**: `Evision.Mat.t()`.

    list of marker ids in markerCorners.
    If markerCorners and markerCorners are empty, the function detect aruco markers and ids.

   This function detects Diamond markers from the previous detected ArUco markers. The diamonds
   are returned in the diamondCorners and diamondIds parameters. If camera calibration parameters
   are provided, the diamond search is based on reprojection. If not, diamond search is based on
   homography. Homography is faster than reprojection, but less accurate.

  Python prototype (for reference only):
  ```python3
  detectDiamonds(image[, diamondCorners[, diamondIds[, markerCorners[, markerIds]]]]) -> diamondCorners, diamondIds, markerCorners, markerIds
  ```
  """
  @spec detectDiamonds(Evision.ArUco.CharucoDetector.t(), Evision.Mat.maybe_mat_in()) :: {list(Evision.Mat.t()), Evision.Mat.t(), list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectDiamonds(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.aruco_aruco_CharucoDetector_detectDiamonds(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.ArUco.CharucoDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.aruco_CharucoDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBoard

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`

  ##### Return
  - **retval**: `Evision.ArUco.CharucoBoard.t()`

  Python prototype (for reference only):
  ```python3
  getBoard() -> retval
  ```
  """
  @spec getBoard(Evision.ArUco.CharucoDetector.t()) :: Evision.ArUco.CharucoBoard.t() | {:error, String.t()}
  def getBoard(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_CharucoDetector_getBoard(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCharucoParameters

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`

  ##### Return
  - **retval**: `CharucoParameters`

  Python prototype (for reference only):
  ```python3
  getCharucoParameters() -> retval
  ```
  """
  @spec getCharucoParameters(Evision.ArUco.CharucoDetector.t()) :: Evision.ArUco.CharucoParameters.t() | {:error, String.t()}
  def getCharucoParameters(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_CharucoDetector_getCharucoParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.ArUco.CharucoDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.aruco_CharucoDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDetectorParameters

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`

  ##### Return
  - **retval**: `DetectorParameters`

  Python prototype (for reference only):
  ```python3
  getDetectorParameters() -> retval
  ```
  """
  @spec getDetectorParameters(Evision.ArUco.CharucoDetector.t()) :: Evision.ArUco.DetectorParameters.t() | {:error, String.t()}
  def getDetectorParameters(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_CharucoDetector_getDetectorParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRefineParameters

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`

  ##### Return
  - **retval**: `RefineParameters`

  Python prototype (for reference only):
  ```python3
  getRefineParameters() -> retval
  ```
  """
  @spec getRefineParameters(Evision.ArUco.CharucoDetector.t()) :: Evision.ArUco.RefineParameters.t() | {:error, String.t()}
  def getRefineParameters(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_CharucoDetector_getRefineParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.ArUco.CharucoDetector.t(), Evision.FileNode.t()) :: Evision.ArUco.CharucoDetector.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.aruco_CharucoDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.ArUco.CharucoDetector.t(), binary()) :: Evision.ArUco.CharucoDetector.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.aruco_CharucoDetector_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBoard

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`
  - **board**: `Evision.ArUco.CharucoBoard`

  Python prototype (for reference only):
  ```python3
  setBoard(board) -> None
  ```
  """
  @spec setBoard(Evision.ArUco.CharucoDetector.t(), Evision.ArUco.CharucoBoard.t()) :: Evision.ArUco.CharucoDetector.t() | {:error, String.t()}
  def setBoard(self, board) when is_struct(board, Evision.ArUco.CharucoBoard)
  do
    positional = [
      board: Evision.Internal.Structurise.from_struct(board)
    ]
    :evision_nif.aruco_aruco_CharucoDetector_setBoard(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCharucoParameters

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`
  - **charucoParameters**: `CharucoParameters`

  Python prototype (for reference only):
  ```python3
  setCharucoParameters(charucoParameters) -> None
  ```
  """
  @spec setCharucoParameters(Evision.ArUco.CharucoDetector.t(), Evision.ArUco.CharucoParameters.t()) :: Evision.ArUco.CharucoDetector.t() | {:error, String.t()}
  def setCharucoParameters(self, charucoParameters) when is_struct(charucoParameters, Evision.ArUco.CharucoParameters)
  do
    positional = [
      charucoParameters: Evision.Internal.Structurise.from_struct(charucoParameters)
    ]
    :evision_nif.aruco_aruco_CharucoDetector_setCharucoParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDetectorParameters

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`
  - **detectorParameters**: `DetectorParameters`

  Python prototype (for reference only):
  ```python3
  setDetectorParameters(detectorParameters) -> None
  ```
  """
  @spec setDetectorParameters(Evision.ArUco.CharucoDetector.t(), Evision.ArUco.DetectorParameters.t()) :: Evision.ArUco.CharucoDetector.t() | {:error, String.t()}
  def setDetectorParameters(self, detectorParameters) when is_struct(detectorParameters, Evision.ArUco.DetectorParameters)
  do
    positional = [
      detectorParameters: Evision.Internal.Structurise.from_struct(detectorParameters)
    ]
    :evision_nif.aruco_aruco_CharucoDetector_setDetectorParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRefineParameters

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`
  - **refineParameters**: `RefineParameters`

  Python prototype (for reference only):
  ```python3
  setRefineParameters(refineParameters) -> None
  ```
  """
  @spec setRefineParameters(Evision.ArUco.CharucoDetector.t(), Evision.ArUco.RefineParameters.t()) :: Evision.ArUco.CharucoDetector.t() | {:error, String.t()}
  def setRefineParameters(self, refineParameters) when is_struct(refineParameters, Evision.ArUco.RefineParameters)
  do
    positional = [
      refineParameters: Evision.Internal.Structurise.from_struct(refineParameters)
    ]
    :evision_nif.aruco_aruco_CharucoDetector_setRefineParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.ArUco.CharucoDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.ArUco.CharucoDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.aruco_CharucoDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.ArUco.CharucoDetector.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.ArUco.CharucoDetector.t(), Evision.FileStorage.t()) :: Evision.ArUco.CharucoDetector.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.aruco_CharucoDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
