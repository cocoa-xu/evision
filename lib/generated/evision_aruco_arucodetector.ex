defmodule Evision.ArUco.ArucoDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ArUco.ArucoDetector` struct.

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
  def to_struct({:ok, %{class: Evision.ArUco.ArucoDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ArUco.ArucoDetector, ref: ref}) do
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
  Basic ArucoDetector constructor
  ##### Keyword Arguments
  - **dictionary**: `Dictionary`.

    indicates the type of markers that will be searched

  - **detectorParams**: `DetectorParameters`.

    marker detection parameters

  - **refineParams**: `RefineParameters`.

    marker refine detection parameters

  ##### Return
  - **self**: `Evision.ArUco.ArucoDetector.t()`

  Python prototype (for reference only):
  ```python3
  ArucoDetector([, dictionary[, detectorParams[, refineParams]]]) -> <aruco_ArucoDetector object>
  ```
  """
  @spec arucoDetector([{:detectorParams, term()} | {:dictionary, term()} | {:refineParams, term()}] | nil) :: Evision.ArUco.ArucoDetector.t() | {:error, String.t()}
  def arucoDetector(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:detectorParams, :dictionary, :refineParams])
    positional = [
    ]
    :evision_nif.aruco_aruco_ArucoDetector_ArucoDetector(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Basic ArucoDetector constructor
  ##### Keyword Arguments
  - **dictionary**: `Dictionary`.

    indicates the type of markers that will be searched

  - **detectorParams**: `DetectorParameters`.

    marker detection parameters

  - **refineParams**: `RefineParameters`.

    marker refine detection parameters

  ##### Return
  - **self**: `Evision.ArUco.ArucoDetector.t()`

  Python prototype (for reference only):
  ```python3
  ArucoDetector([, dictionary[, detectorParams[, refineParams]]]) -> <aruco_ArucoDetector object>
  ```
  """
  @spec arucoDetector() :: Evision.ArUco.ArucoDetector.t() | {:error, String.t()}
  def arucoDetector() do
    positional = [
    ]
    :evision_nif.aruco_aruco_ArucoDetector_ArucoDetector(positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.ArUco.ArucoDetector.t()) :: Evision.ArUco.ArucoDetector.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.aruco_ArucoDetector_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Basic marker detection

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`
  - **image**: `Evision.Mat`.

    input image

  ##### Return
  - **corners**: `[Evision.Mat]`.

    vector of detected marker corners. For each marker, its four corners
    are provided, (e.g std::vector<std::vector<cv::Point2f> > ). For N detected markers,
    the dimensions of this array is Nx4. The order of the corners is clockwise.

  - **ids**: `Evision.Mat.t()`.

    vector of identifiers of the detected markers. The identifier is of type int
    (e.g. std::vector<int>). For N detected markers, the size of ids is also N.
    The identifiers have the same order than the markers in the imgPoints array.

  - **rejectedImgPoints**: `[Evision.Mat]`.

    contains the imgPoints of those squares whose inner code has not a
    correct codification. Useful for debugging purposes.

   Performs marker detection in the input image. Only markers included in the specific dictionary
   are searched. For each detected marker, it returns the 2D position of its corner in the image
   and its corresponding identifier.
   Note that this function does not perform pose estimation.
  **Note**: The function does not correct lens distortion or takes it into account. It's recommended to undistort
   input image with corresponding camera model, if camera parameters are known
  @sa undistort, estimatePoseSingleMarkers,  estimatePoseBoard

  Python prototype (for reference only):
  ```python3
  detectMarkers(image[, corners[, ids[, rejectedImgPoints]]]) -> corners, ids, rejectedImgPoints
  ```
  """
  @spec detectMarkers(Evision.ArUco.ArucoDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {list(Evision.Mat.t()), Evision.Mat.t(), list(Evision.Mat.t())} | {:error, String.t()}
  def detectMarkers(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.aruco_aruco_ArucoDetector_detectMarkers(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Basic marker detection

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`
  - **image**: `Evision.Mat`.

    input image

  ##### Return
  - **corners**: `[Evision.Mat]`.

    vector of detected marker corners. For each marker, its four corners
    are provided, (e.g std::vector<std::vector<cv::Point2f> > ). For N detected markers,
    the dimensions of this array is Nx4. The order of the corners is clockwise.

  - **ids**: `Evision.Mat.t()`.

    vector of identifiers of the detected markers. The identifier is of type int
    (e.g. std::vector<int>). For N detected markers, the size of ids is also N.
    The identifiers have the same order than the markers in the imgPoints array.

  - **rejectedImgPoints**: `[Evision.Mat]`.

    contains the imgPoints of those squares whose inner code has not a
    correct codification. Useful for debugging purposes.

   Performs marker detection in the input image. Only markers included in the specific dictionary
   are searched. For each detected marker, it returns the 2D position of its corner in the image
   and its corresponding identifier.
   Note that this function does not perform pose estimation.
  **Note**: The function does not correct lens distortion or takes it into account. It's recommended to undistort
   input image with corresponding camera model, if camera parameters are known
  @sa undistort, estimatePoseSingleMarkers,  estimatePoseBoard

  Python prototype (for reference only):
  ```python3
  detectMarkers(image[, corners[, ids[, rejectedImgPoints]]]) -> corners, ids, rejectedImgPoints
  ```
  """
  @spec detectMarkers(Evision.ArUco.ArucoDetector.t(), Evision.Mat.maybe_mat_in()) :: {list(Evision.Mat.t()), Evision.Mat.t(), list(Evision.Mat.t())} | {:error, String.t()}
  def detectMarkers(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.aruco_aruco_ArucoDetector_detectMarkers(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.ArUco.ArucoDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.aruco_ArucoDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.ArUco.ArucoDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.aruco_ArucoDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDetectorParameters

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`

  ##### Return
  - **retval**: `DetectorParameters`

  Python prototype (for reference only):
  ```python3
  getDetectorParameters() -> retval
  ```
  """
  @spec getDetectorParameters(Evision.ArUco.ArucoDetector.t()) :: Evision.ArUco.DetectorParameters.t() | {:error, String.t()}
  def getDetectorParameters(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_ArucoDetector_getDetectorParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDictionary

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`

  ##### Return
  - **retval**: `Dictionary`

  Python prototype (for reference only):
  ```python3
  getDictionary() -> retval
  ```
  """
  @spec getDictionary(Evision.ArUco.ArucoDetector.t()) :: Evision.ArUco.Dictionary.t() | {:error, String.t()}
  def getDictionary(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_ArucoDetector_getDictionary(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRefineParameters

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`

  ##### Return
  - **retval**: `RefineParameters`

  Python prototype (for reference only):
  ```python3
  getRefineParameters() -> retval
  ```
  """
  @spec getRefineParameters(Evision.ArUco.ArucoDetector.t()) :: Evision.ArUco.RefineParameters.t() | {:error, String.t()}
  def getRefineParameters(self) do
    positional = [
    ]
    :evision_nif.aruco_aruco_ArucoDetector_getRefineParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.ArUco.ArucoDetector.t(), Evision.FileNode.t()) :: Evision.ArUco.ArucoDetector.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.aruco_aruco_ArucoDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Refine not detected markers based on the already detected and the board layout

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`
  - **image**: `Evision.Mat`.

    input image

  - **board**: `Board`.

    layout of markers in the board.

  ##### Keyword Arguments
  - **cameraMatrix**: `Evision.Mat`.

    optional input 3x3 floating-point camera matrix
    \\f$A = \\vecthreethree{f_x}{0}{c_x}{0}{f_y}{c_y}{0}{0}{1}\\f$

  - **distCoeffs**: `Evision.Mat`.

    optional vector of distortion coefficients
    \\f$(k_1, k_2, p_1, p_2[, k_3[, k_4, k_5, k_6],[s_1, s_2, s_3, s_4]])\\f$ of 4, 5, 8 or 12 elements

  ##### Return
  - **detectedCorners**: `[Evision.Mat]`.

    vector of already detected marker corners.

  - **detectedIds**: `Evision.Mat.t()`.

    vector of already detected marker identifiers.

  - **rejectedCorners**: `[Evision.Mat]`.

    vector of rejected candidates during the marker detection process.

  - **recoveredIdxs**: `Evision.Mat.t()`.

    Optional array to returns the indexes of the recovered candidates in the
    original rejectedCorners array.

   This function tries to find markers that were not detected in the basic detecMarkers function.
   First, based on the current detected marker and the board layout, the function interpolates
   the position of the missing markers. Then it tries to find correspondence between the reprojected
   markers and the rejected candidates based on the minRepDistance and errorCorrectionRate parameters.
   If camera parameters and distortion coefficients are provided, missing markers are reprojected
   using projectPoint function. If not, missing marker projections are interpolated using global
   homography, and all the marker corners in the board must have the same Z coordinate.

  Python prototype (for reference only):
  ```python3
  refineDetectedMarkers(image, board, detectedCorners, detectedIds, rejectedCorners[, cameraMatrix[, distCoeffs[, recoveredIdxs]]]) -> detectedCorners, detectedIds, rejectedCorners, recoveredIdxs
  ```
  """
  @spec refineDetectedMarkers(Evision.ArUco.ArucoDetector.t(), Evision.Mat.maybe_mat_in(), Evision.ArUco.Board.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), list(Evision.Mat.maybe_mat_in()), [{:cameraMatrix, term()} | {:distCoeffs, term()}] | nil) :: {list(Evision.Mat.t()), Evision.Mat.t(), list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def refineDetectedMarkers(self, image, board, detectedCorners, detectedIds, rejectedCorners, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_struct(board, Evision.ArUco.Board) and is_list(detectedCorners) and (is_struct(detectedIds, Evision.Mat) or is_struct(detectedIds, Nx.Tensor) or is_number(detectedIds) or is_tuple(detectedIds)) and is_list(rejectedCorners) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:cameraMatrix, :distCoeffs])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      board: Evision.Internal.Structurise.from_struct(board),
      detectedCorners: Evision.Internal.Structurise.from_struct(detectedCorners),
      detectedIds: Evision.Internal.Structurise.from_struct(detectedIds),
      rejectedCorners: Evision.Internal.Structurise.from_struct(rejectedCorners)
    ]
    :evision_nif.aruco_aruco_ArucoDetector_refineDetectedMarkers(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Refine not detected markers based on the already detected and the board layout

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`
  - **image**: `Evision.Mat`.

    input image

  - **board**: `Board`.

    layout of markers in the board.

  ##### Keyword Arguments
  - **cameraMatrix**: `Evision.Mat`.

    optional input 3x3 floating-point camera matrix
    \\f$A = \\vecthreethree{f_x}{0}{c_x}{0}{f_y}{c_y}{0}{0}{1}\\f$

  - **distCoeffs**: `Evision.Mat`.

    optional vector of distortion coefficients
    \\f$(k_1, k_2, p_1, p_2[, k_3[, k_4, k_5, k_6],[s_1, s_2, s_3, s_4]])\\f$ of 4, 5, 8 or 12 elements

  ##### Return
  - **detectedCorners**: `[Evision.Mat]`.

    vector of already detected marker corners.

  - **detectedIds**: `Evision.Mat.t()`.

    vector of already detected marker identifiers.

  - **rejectedCorners**: `[Evision.Mat]`.

    vector of rejected candidates during the marker detection process.

  - **recoveredIdxs**: `Evision.Mat.t()`.

    Optional array to returns the indexes of the recovered candidates in the
    original rejectedCorners array.

   This function tries to find markers that were not detected in the basic detecMarkers function.
   First, based on the current detected marker and the board layout, the function interpolates
   the position of the missing markers. Then it tries to find correspondence between the reprojected
   markers and the rejected candidates based on the minRepDistance and errorCorrectionRate parameters.
   If camera parameters and distortion coefficients are provided, missing markers are reprojected
   using projectPoint function. If not, missing marker projections are interpolated using global
   homography, and all the marker corners in the board must have the same Z coordinate.

  Python prototype (for reference only):
  ```python3
  refineDetectedMarkers(image, board, detectedCorners, detectedIds, rejectedCorners[, cameraMatrix[, distCoeffs[, recoveredIdxs]]]) -> detectedCorners, detectedIds, rejectedCorners, recoveredIdxs
  ```
  """
  @spec refineDetectedMarkers(Evision.ArUco.ArucoDetector.t(), Evision.Mat.maybe_mat_in(), Evision.ArUco.Board.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), list(Evision.Mat.maybe_mat_in())) :: {list(Evision.Mat.t()), Evision.Mat.t(), list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def refineDetectedMarkers(self, image, board, detectedCorners, detectedIds, rejectedCorners) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_struct(board, Evision.ArUco.Board) and is_list(detectedCorners) and (is_struct(detectedIds, Evision.Mat) or is_struct(detectedIds, Nx.Tensor) or is_number(detectedIds) or is_tuple(detectedIds)) and is_list(rejectedCorners)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      board: Evision.Internal.Structurise.from_struct(board),
      detectedCorners: Evision.Internal.Structurise.from_struct(detectedCorners),
      detectedIds: Evision.Internal.Structurise.from_struct(detectedIds),
      rejectedCorners: Evision.Internal.Structurise.from_struct(rejectedCorners)
    ]
    :evision_nif.aruco_aruco_ArucoDetector_refineDetectedMarkers(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.ArUco.ArucoDetector.t(), binary()) :: Evision.ArUco.ArucoDetector.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.aruco_ArucoDetector_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDetectorParameters

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`
  - **detectorParameters**: `DetectorParameters`

  Python prototype (for reference only):
  ```python3
  setDetectorParameters(detectorParameters) -> None
  ```
  """
  @spec setDetectorParameters(Evision.ArUco.ArucoDetector.t(), Evision.ArUco.DetectorParameters.t()) :: Evision.ArUco.ArucoDetector.t() | {:error, String.t()}
  def setDetectorParameters(self, detectorParameters) when is_struct(detectorParameters, Evision.ArUco.DetectorParameters)
  do
    positional = [
      detectorParameters: Evision.Internal.Structurise.from_struct(detectorParameters)
    ]
    :evision_nif.aruco_aruco_ArucoDetector_setDetectorParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDictionary

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`
  - **dictionary**: `Dictionary`

  Python prototype (for reference only):
  ```python3
  setDictionary(dictionary) -> None
  ```
  """
  @spec setDictionary(Evision.ArUco.ArucoDetector.t(), Evision.ArUco.Dictionary.t()) :: Evision.ArUco.ArucoDetector.t() | {:error, String.t()}
  def setDictionary(self, dictionary) when is_struct(dictionary, Evision.ArUco.Dictionary)
  do
    positional = [
      dictionary: Evision.Internal.Structurise.from_struct(dictionary)
    ]
    :evision_nif.aruco_aruco_ArucoDetector_setDictionary(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRefineParameters

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`
  - **refineParameters**: `RefineParameters`

  Python prototype (for reference only):
  ```python3
  setRefineParameters(refineParameters) -> None
  ```
  """
  @spec setRefineParameters(Evision.ArUco.ArucoDetector.t(), Evision.ArUco.RefineParameters.t()) :: Evision.ArUco.ArucoDetector.t() | {:error, String.t()}
  def setRefineParameters(self, refineParameters) when is_struct(refineParameters, Evision.ArUco.RefineParameters)
  do
    positional = [
      refineParameters: Evision.Internal.Structurise.from_struct(refineParameters)
    ]
    :evision_nif.aruco_aruco_ArucoDetector_setRefineParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  simplified API for language bindings

  ##### Positional Arguments
  - **self**: `Evision.ArUco.ArucoDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.ArUco.ArucoDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.ArUco.ArucoDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.aruco_aruco_ArucoDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
