defmodule Evision.ArUco do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ArUco` struct.

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
  def to_struct({:ok, %{class: Evision.ArUco, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ArUco, ref: ref}) do
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
  It's the same function as #calibrateCameraAruco but without calibration error estimation.

  ##### Positional Arguments
  - **corners**: `[Evision.Mat]`
  - **ids**: `Evision.Mat`
  - **counter**: `Evision.Mat`
  - **board**: `Board`
  - **imageSize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **criteria**: `TermCriteria`.

  ##### Return
  - **retval**: `double`
  - **cameraMatrix**: `Evision.Mat.t()`
  - **distCoeffs**: `Evision.Mat.t()`
  - **rvecs**: `[Evision.Mat]`.
  - **tvecs**: `[Evision.Mat]`.

  Has overloading in C++

  @deprecated Use Board::matchImagePoints and cv::solvePnP

  Python prototype (for reference only):
  ```python3
  calibrateCameraAruco(corners, ids, counter, board, imageSize, cameraMatrix, distCoeffs[, rvecs[, tvecs[, flags[, criteria]]]]) -> retval, cameraMatrix, distCoeffs, rvecs, tvecs
  ```
  """
  @spec calibrateCameraAruco(list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.ArUco.Board.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:criteria, term()} | {:flags, term()}] | nil) :: {number(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t())} | {:error, String.t()}
  def calibrateCameraAruco(corners, ids, counter, board, imageSize, cameraMatrix, distCoeffs, opts) when is_list(corners) and (is_struct(ids, Evision.Mat) or is_struct(ids, Nx.Tensor) or is_number(ids) or is_tuple(ids)) and (is_struct(counter, Evision.Mat) or is_struct(counter, Nx.Tensor) or is_number(counter) or is_tuple(counter)) and (is_reference(board) or is_struct(board)) and is_tuple(imageSize) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :flags])
    positional = [
      corners: Evision.Internal.Structurise.from_struct(corners),
      ids: Evision.Internal.Structurise.from_struct(ids),
      counter: Evision.Internal.Structurise.from_struct(counter),
      board: Evision.Internal.Structurise.from_struct(board),
      imageSize: Evision.Internal.Structurise.from_struct(imageSize),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs)
    ]
    :evision_nif.aruco_calibrateCameraAruco(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  It's the same function as #calibrateCameraAruco but without calibration error estimation.

  ##### Positional Arguments
  - **corners**: `[Evision.Mat]`
  - **ids**: `Evision.Mat`
  - **counter**: `Evision.Mat`
  - **board**: `Board`
  - **imageSize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **criteria**: `TermCriteria`.

  ##### Return
  - **retval**: `double`
  - **cameraMatrix**: `Evision.Mat.t()`
  - **distCoeffs**: `Evision.Mat.t()`
  - **rvecs**: `[Evision.Mat]`.
  - **tvecs**: `[Evision.Mat]`.

  Has overloading in C++

  @deprecated Use Board::matchImagePoints and cv::solvePnP

  Python prototype (for reference only):
  ```python3
  calibrateCameraAruco(corners, ids, counter, board, imageSize, cameraMatrix, distCoeffs[, rvecs[, tvecs[, flags[, criteria]]]]) -> retval, cameraMatrix, distCoeffs, rvecs, tvecs
  ```
  """
  @spec calibrateCameraAruco(list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.ArUco.Board.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t())} | {:error, String.t()}
  def calibrateCameraAruco(corners, ids, counter, board, imageSize, cameraMatrix, distCoeffs) when is_list(corners) and (is_struct(ids, Evision.Mat) or is_struct(ids, Nx.Tensor) or is_number(ids) or is_tuple(ids)) and (is_struct(counter, Evision.Mat) or is_struct(counter, Nx.Tensor) or is_number(counter) or is_tuple(counter)) and (is_reference(board) or is_struct(board)) and is_tuple(imageSize) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs))
  do
    positional = [
      corners: Evision.Internal.Structurise.from_struct(corners),
      ids: Evision.Internal.Structurise.from_struct(ids),
      counter: Evision.Internal.Structurise.from_struct(counter),
      board: Evision.Internal.Structurise.from_struct(board),
      imageSize: Evision.Internal.Structurise.from_struct(imageSize),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs)
    ]
    :evision_nif.aruco_calibrateCameraAruco(positional)
    |> to_struct()
  end

  @doc """
  Calibrate a camera using aruco markers

  ##### Positional Arguments
  - **corners**: `[Evision.Mat]`.

    vector of detected marker corners in all frames.
    The corners should have the same format returned by detectMarkers (see #detectMarkers).

  - **ids**: `Evision.Mat`.

    list of identifiers for each marker in corners

  - **counter**: `Evision.Mat`.

    number of markers in each frame so that corners and ids can be split

  - **board**: `Board`.

    Marker Board layout

  - **imageSize**: `Size`.

    Size of the image used only to initialize the intrinsic camera matrix.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    flags Different flags  for the calibration process (see #calibrateCamera for details).

  - **criteria**: `TermCriteria`.

    Termination criteria for the iterative optimization algorithm.

  ##### Return
  - **retval**: `double`
  - **cameraMatrix**: `Evision.Mat.t()`.

    Output 3x3 floating-point camera matrix
    \\f$A = \\vecthreethree{f_x}{0}{c_x}{0}{f_y}{c_y}{0}{0}{1}\\f$ . If CV\\_CALIB\\_USE\\_INTRINSIC\\_GUESS
    and/or CV_CALIB_FIX_ASPECT_RATIO are specified, some or all of fx, fy, cx, cy must be
    initialized before calling the function.

  - **distCoeffs**: `Evision.Mat.t()`.

    Output vector of distortion coefficients
    \\f$(k_1, k_2, p_1, p_2[, k_3[, k_4, k_5, k_6],[s_1, s_2, s_3, s_4]])\\f$ of 4, 5, 8 or 12 elements

  - **rvecs**: `[Evision.Mat]`.

    Output vector of rotation vectors (see Rodrigues ) estimated for each board view
    (e.g. std::vector<cv::Mat>>). That is, each k-th rotation vector together with the corresponding
    k-th translation vector (see the next output parameter description) brings the board pattern
    from the model coordinate space (in which object points are specified) to the world coordinate
    space, that is, a real position of the board pattern in the k-th pattern view (k=0.. *M* -1).

  - **tvecs**: `[Evision.Mat]`.

    Output vector of translation vectors estimated for each pattern view.

  - **stdDeviationsIntrinsics**: `Evision.Mat.t()`.

    Output vector of standard deviations estimated for intrinsic parameters.
    Order of deviations values:
    \\f$(f_x, f_y, c_x, c_y, k_1, k_2, p_1, p_2, k_3, k_4, k_5, k_6 , s_1, s_2, s_3,
    s_4, \\tau_x, \\tau_y)\\f$ If one of parameters is not estimated, it's deviation is equals to zero.

  - **stdDeviationsExtrinsics**: `Evision.Mat.t()`.

    Output vector of standard deviations estimated for extrinsic parameters.
    Order of deviations values: \\f$(R_1, T_1, \\dotsc , R_M, T_M)\\f$ where M is number of pattern views,
    \\f$R_i, T_i\\f$ are concatenated 1x3 vectors.

  - **perViewErrors**: `Evision.Mat.t()`.

    Output vector of average re-projection errors estimated for each pattern view.

   This function calibrates a camera using an Aruco Board. The function receives a list of
   detected markers from several views of the Board. The process is similar to the chessboard
   calibration in calibrateCamera(). The function returns the final re-projection error.
  @deprecated Use Board::matchImagePoints and cv::solvePnP

  Python prototype (for reference only):
  ```python3
  calibrateCameraArucoExtended(corners, ids, counter, board, imageSize, cameraMatrix, distCoeffs[, rvecs[, tvecs[, stdDeviationsIntrinsics[, stdDeviationsExtrinsics[, perViewErrors[, flags[, criteria]]]]]]]) -> retval, cameraMatrix, distCoeffs, rvecs, tvecs, stdDeviationsIntrinsics, stdDeviationsExtrinsics, perViewErrors
  ```
  """
  @spec calibrateCameraArucoExtended(list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.ArUco.Board.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:criteria, term()} | {:flags, term()}] | nil) :: {number(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t()), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def calibrateCameraArucoExtended(corners, ids, counter, board, imageSize, cameraMatrix, distCoeffs, opts) when is_list(corners) and (is_struct(ids, Evision.Mat) or is_struct(ids, Nx.Tensor) or is_number(ids) or is_tuple(ids)) and (is_struct(counter, Evision.Mat) or is_struct(counter, Nx.Tensor) or is_number(counter) or is_tuple(counter)) and (is_reference(board) or is_struct(board)) and is_tuple(imageSize) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :flags])
    positional = [
      corners: Evision.Internal.Structurise.from_struct(corners),
      ids: Evision.Internal.Structurise.from_struct(ids),
      counter: Evision.Internal.Structurise.from_struct(counter),
      board: Evision.Internal.Structurise.from_struct(board),
      imageSize: Evision.Internal.Structurise.from_struct(imageSize),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs)
    ]
    :evision_nif.aruco_calibrateCameraArucoExtended(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Calibrate a camera using aruco markers

  ##### Positional Arguments
  - **corners**: `[Evision.Mat]`.

    vector of detected marker corners in all frames.
    The corners should have the same format returned by detectMarkers (see #detectMarkers).

  - **ids**: `Evision.Mat`.

    list of identifiers for each marker in corners

  - **counter**: `Evision.Mat`.

    number of markers in each frame so that corners and ids can be split

  - **board**: `Board`.

    Marker Board layout

  - **imageSize**: `Size`.

    Size of the image used only to initialize the intrinsic camera matrix.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    flags Different flags  for the calibration process (see #calibrateCamera for details).

  - **criteria**: `TermCriteria`.

    Termination criteria for the iterative optimization algorithm.

  ##### Return
  - **retval**: `double`
  - **cameraMatrix**: `Evision.Mat.t()`.

    Output 3x3 floating-point camera matrix
    \\f$A = \\vecthreethree{f_x}{0}{c_x}{0}{f_y}{c_y}{0}{0}{1}\\f$ . If CV\\_CALIB\\_USE\\_INTRINSIC\\_GUESS
    and/or CV_CALIB_FIX_ASPECT_RATIO are specified, some or all of fx, fy, cx, cy must be
    initialized before calling the function.

  - **distCoeffs**: `Evision.Mat.t()`.

    Output vector of distortion coefficients
    \\f$(k_1, k_2, p_1, p_2[, k_3[, k_4, k_5, k_6],[s_1, s_2, s_3, s_4]])\\f$ of 4, 5, 8 or 12 elements

  - **rvecs**: `[Evision.Mat]`.

    Output vector of rotation vectors (see Rodrigues ) estimated for each board view
    (e.g. std::vector<cv::Mat>>). That is, each k-th rotation vector together with the corresponding
    k-th translation vector (see the next output parameter description) brings the board pattern
    from the model coordinate space (in which object points are specified) to the world coordinate
    space, that is, a real position of the board pattern in the k-th pattern view (k=0.. *M* -1).

  - **tvecs**: `[Evision.Mat]`.

    Output vector of translation vectors estimated for each pattern view.

  - **stdDeviationsIntrinsics**: `Evision.Mat.t()`.

    Output vector of standard deviations estimated for intrinsic parameters.
    Order of deviations values:
    \\f$(f_x, f_y, c_x, c_y, k_1, k_2, p_1, p_2, k_3, k_4, k_5, k_6 , s_1, s_2, s_3,
    s_4, \\tau_x, \\tau_y)\\f$ If one of parameters is not estimated, it's deviation is equals to zero.

  - **stdDeviationsExtrinsics**: `Evision.Mat.t()`.

    Output vector of standard deviations estimated for extrinsic parameters.
    Order of deviations values: \\f$(R_1, T_1, \\dotsc , R_M, T_M)\\f$ where M is number of pattern views,
    \\f$R_i, T_i\\f$ are concatenated 1x3 vectors.

  - **perViewErrors**: `Evision.Mat.t()`.

    Output vector of average re-projection errors estimated for each pattern view.

   This function calibrates a camera using an Aruco Board. The function receives a list of
   detected markers from several views of the Board. The process is similar to the chessboard
   calibration in calibrateCamera(). The function returns the final re-projection error.
  @deprecated Use Board::matchImagePoints and cv::solvePnP

  Python prototype (for reference only):
  ```python3
  calibrateCameraArucoExtended(corners, ids, counter, board, imageSize, cameraMatrix, distCoeffs[, rvecs[, tvecs[, stdDeviationsIntrinsics[, stdDeviationsExtrinsics[, perViewErrors[, flags[, criteria]]]]]]]) -> retval, cameraMatrix, distCoeffs, rvecs, tvecs, stdDeviationsIntrinsics, stdDeviationsExtrinsics, perViewErrors
  ```
  """
  @spec calibrateCameraArucoExtended(list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.ArUco.Board.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t()), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def calibrateCameraArucoExtended(corners, ids, counter, board, imageSize, cameraMatrix, distCoeffs) when is_list(corners) and (is_struct(ids, Evision.Mat) or is_struct(ids, Nx.Tensor) or is_number(ids) or is_tuple(ids)) and (is_struct(counter, Evision.Mat) or is_struct(counter, Nx.Tensor) or is_number(counter) or is_tuple(counter)) and (is_reference(board) or is_struct(board)) and is_tuple(imageSize) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs))
  do
    positional = [
      corners: Evision.Internal.Structurise.from_struct(corners),
      ids: Evision.Internal.Structurise.from_struct(ids),
      counter: Evision.Internal.Structurise.from_struct(counter),
      board: Evision.Internal.Structurise.from_struct(board),
      imageSize: Evision.Internal.Structurise.from_struct(imageSize),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs)
    ]
    :evision_nif.aruco_calibrateCameraArucoExtended(positional)
    |> to_struct()
  end

  @doc """
  It's the same function as #calibrateCameraCharuco but without calibration error estimation.

  ##### Positional Arguments
  - **charucoCorners**: `[Evision.Mat]`
  - **charucoIds**: `[Evision.Mat]`
  - **board**: `Evision.ArUco.CharucoBoard`
  - **imageSize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **criteria**: `TermCriteria`.

  ##### Return
  - **retval**: `double`
  - **cameraMatrix**: `Evision.Mat.t()`
  - **distCoeffs**: `Evision.Mat.t()`
  - **rvecs**: `[Evision.Mat]`.
  - **tvecs**: `[Evision.Mat]`.

  @deprecated Use CharucoBoard::matchImagePoints and cv::solvePnP

  Python prototype (for reference only):
  ```python3
  calibrateCameraCharuco(charucoCorners, charucoIds, board, imageSize, cameraMatrix, distCoeffs[, rvecs[, tvecs[, flags[, criteria]]]]) -> retval, cameraMatrix, distCoeffs, rvecs, tvecs
  ```
  """
  @spec calibrateCameraCharuco(list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), Evision.ArUco.CharucoBoard.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:criteria, term()} | {:flags, term()}] | nil) :: {number(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t())} | {:error, String.t()}
  def calibrateCameraCharuco(charucoCorners, charucoIds, board, imageSize, cameraMatrix, distCoeffs, opts) when is_list(charucoCorners) and is_list(charucoIds) and is_struct(board, Evision.ArUco.CharucoBoard) and is_tuple(imageSize) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :flags])
    positional = [
      charucoCorners: Evision.Internal.Structurise.from_struct(charucoCorners),
      charucoIds: Evision.Internal.Structurise.from_struct(charucoIds),
      board: Evision.Internal.Structurise.from_struct(board),
      imageSize: Evision.Internal.Structurise.from_struct(imageSize),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs)
    ]
    :evision_nif.aruco_calibrateCameraCharuco(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  It's the same function as #calibrateCameraCharuco but without calibration error estimation.

  ##### Positional Arguments
  - **charucoCorners**: `[Evision.Mat]`
  - **charucoIds**: `[Evision.Mat]`
  - **board**: `Evision.ArUco.CharucoBoard`
  - **imageSize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **criteria**: `TermCriteria`.

  ##### Return
  - **retval**: `double`
  - **cameraMatrix**: `Evision.Mat.t()`
  - **distCoeffs**: `Evision.Mat.t()`
  - **rvecs**: `[Evision.Mat]`.
  - **tvecs**: `[Evision.Mat]`.

  @deprecated Use CharucoBoard::matchImagePoints and cv::solvePnP

  Python prototype (for reference only):
  ```python3
  calibrateCameraCharuco(charucoCorners, charucoIds, board, imageSize, cameraMatrix, distCoeffs[, rvecs[, tvecs[, flags[, criteria]]]]) -> retval, cameraMatrix, distCoeffs, rvecs, tvecs
  ```
  """
  @spec calibrateCameraCharuco(list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), Evision.ArUco.CharucoBoard.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t())} | {:error, String.t()}
  def calibrateCameraCharuco(charucoCorners, charucoIds, board, imageSize, cameraMatrix, distCoeffs) when is_list(charucoCorners) and is_list(charucoIds) and is_struct(board, Evision.ArUco.CharucoBoard) and is_tuple(imageSize) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs))
  do
    positional = [
      charucoCorners: Evision.Internal.Structurise.from_struct(charucoCorners),
      charucoIds: Evision.Internal.Structurise.from_struct(charucoIds),
      board: Evision.Internal.Structurise.from_struct(board),
      imageSize: Evision.Internal.Structurise.from_struct(imageSize),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs)
    ]
    :evision_nif.aruco_calibrateCameraCharuco(positional)
    |> to_struct()
  end

  @doc """
  Calibrate a camera using Charuco corners

  ##### Positional Arguments
  - **charucoCorners**: `[Evision.Mat]`.

    vector of detected charuco corners per frame

  - **charucoIds**: `[Evision.Mat]`.

    list of identifiers for each corner in charucoCorners per frame

  - **board**: `Evision.ArUco.CharucoBoard`.

    Marker Board layout

  - **imageSize**: `Size`.

    input image size

  ##### Keyword Arguments
  - **flags**: `integer()`.

    flags Different flags  for the calibration process (see #calibrateCamera for details).

  - **criteria**: `TermCriteria`.

    Termination criteria for the iterative optimization algorithm.

  ##### Return
  - **retval**: `double`
  - **cameraMatrix**: `Evision.Mat.t()`.

    Output 3x3 floating-point camera matrix
    \\f$A = \\vecthreethree{f_x}{0}{c_x}{0}{f_y}{c_y}{0}{0}{1}\\f$ . If CV\\_CALIB\\_USE\\_INTRINSIC\\_GUESS
    and/or CV_CALIB_FIX_ASPECT_RATIO are specified, some or all of fx, fy, cx, cy must be
    initialized before calling the function.

  - **distCoeffs**: `Evision.Mat.t()`.

    Output vector of distortion coefficients
    \\f$(k_1, k_2, p_1, p_2[, k_3[, k_4, k_5, k_6],[s_1, s_2, s_3, s_4]])\\f$ of 4, 5, 8 or 12 elements

  - **rvecs**: `[Evision.Mat]`.

    Output vector of rotation vectors (see Rodrigues ) estimated for each board view
    (e.g. std::vector<cv::Mat>>). That is, each k-th rotation vector together with the corresponding
    k-th translation vector (see the next output parameter description) brings the board pattern
    from the model coordinate space (in which object points are specified) to the world coordinate
    space, that is, a real position of the board pattern in the k-th pattern view (k=0.. *M* -1).

  - **tvecs**: `[Evision.Mat]`.

    Output vector of translation vectors estimated for each pattern view.

  - **stdDeviationsIntrinsics**: `Evision.Mat.t()`.

    Output vector of standard deviations estimated for intrinsic parameters.
    Order of deviations values:
    \\f$(f_x, f_y, c_x, c_y, k_1, k_2, p_1, p_2, k_3, k_4, k_5, k_6 , s_1, s_2, s_3,
    s_4, \\tau_x, \\tau_y)\\f$ If one of parameters is not estimated, it's deviation is equals to zero.

  - **stdDeviationsExtrinsics**: `Evision.Mat.t()`.

    Output vector of standard deviations estimated for extrinsic parameters.
    Order of deviations values: \\f$(R_1, T_1, \\dotsc , R_M, T_M)\\f$ where M is number of pattern views,
    \\f$R_i, T_i\\f$ are concatenated 1x3 vectors.

  - **perViewErrors**: `Evision.Mat.t()`.

    Output vector of average re-projection errors estimated for each pattern view.

   This function calibrates a camera using a set of corners of a  Charuco Board. The function
   receives a list of detected corners and its identifiers from several views of the Board.
   The function returns the final re-projection error.
  @deprecated Use CharucoBoard::matchImagePoints and cv::solvePnP

  Python prototype (for reference only):
  ```python3
  calibrateCameraCharucoExtended(charucoCorners, charucoIds, board, imageSize, cameraMatrix, distCoeffs[, rvecs[, tvecs[, stdDeviationsIntrinsics[, stdDeviationsExtrinsics[, perViewErrors[, flags[, criteria]]]]]]]) -> retval, cameraMatrix, distCoeffs, rvecs, tvecs, stdDeviationsIntrinsics, stdDeviationsExtrinsics, perViewErrors
  ```
  """
  @spec calibrateCameraCharucoExtended(list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), Evision.ArUco.CharucoBoard.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:criteria, term()} | {:flags, term()}] | nil) :: {number(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t()), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def calibrateCameraCharucoExtended(charucoCorners, charucoIds, board, imageSize, cameraMatrix, distCoeffs, opts) when is_list(charucoCorners) and is_list(charucoIds) and is_struct(board, Evision.ArUco.CharucoBoard) and is_tuple(imageSize) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :flags])
    positional = [
      charucoCorners: Evision.Internal.Structurise.from_struct(charucoCorners),
      charucoIds: Evision.Internal.Structurise.from_struct(charucoIds),
      board: Evision.Internal.Structurise.from_struct(board),
      imageSize: Evision.Internal.Structurise.from_struct(imageSize),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs)
    ]
    :evision_nif.aruco_calibrateCameraCharucoExtended(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Calibrate a camera using Charuco corners

  ##### Positional Arguments
  - **charucoCorners**: `[Evision.Mat]`.

    vector of detected charuco corners per frame

  - **charucoIds**: `[Evision.Mat]`.

    list of identifiers for each corner in charucoCorners per frame

  - **board**: `Evision.ArUco.CharucoBoard`.

    Marker Board layout

  - **imageSize**: `Size`.

    input image size

  ##### Keyword Arguments
  - **flags**: `integer()`.

    flags Different flags  for the calibration process (see #calibrateCamera for details).

  - **criteria**: `TermCriteria`.

    Termination criteria for the iterative optimization algorithm.

  ##### Return
  - **retval**: `double`
  - **cameraMatrix**: `Evision.Mat.t()`.

    Output 3x3 floating-point camera matrix
    \\f$A = \\vecthreethree{f_x}{0}{c_x}{0}{f_y}{c_y}{0}{0}{1}\\f$ . If CV\\_CALIB\\_USE\\_INTRINSIC\\_GUESS
    and/or CV_CALIB_FIX_ASPECT_RATIO are specified, some or all of fx, fy, cx, cy must be
    initialized before calling the function.

  - **distCoeffs**: `Evision.Mat.t()`.

    Output vector of distortion coefficients
    \\f$(k_1, k_2, p_1, p_2[, k_3[, k_4, k_5, k_6],[s_1, s_2, s_3, s_4]])\\f$ of 4, 5, 8 or 12 elements

  - **rvecs**: `[Evision.Mat]`.

    Output vector of rotation vectors (see Rodrigues ) estimated for each board view
    (e.g. std::vector<cv::Mat>>). That is, each k-th rotation vector together with the corresponding
    k-th translation vector (see the next output parameter description) brings the board pattern
    from the model coordinate space (in which object points are specified) to the world coordinate
    space, that is, a real position of the board pattern in the k-th pattern view (k=0.. *M* -1).

  - **tvecs**: `[Evision.Mat]`.

    Output vector of translation vectors estimated for each pattern view.

  - **stdDeviationsIntrinsics**: `Evision.Mat.t()`.

    Output vector of standard deviations estimated for intrinsic parameters.
    Order of deviations values:
    \\f$(f_x, f_y, c_x, c_y, k_1, k_2, p_1, p_2, k_3, k_4, k_5, k_6 , s_1, s_2, s_3,
    s_4, \\tau_x, \\tau_y)\\f$ If one of parameters is not estimated, it's deviation is equals to zero.

  - **stdDeviationsExtrinsics**: `Evision.Mat.t()`.

    Output vector of standard deviations estimated for extrinsic parameters.
    Order of deviations values: \\f$(R_1, T_1, \\dotsc , R_M, T_M)\\f$ where M is number of pattern views,
    \\f$R_i, T_i\\f$ are concatenated 1x3 vectors.

  - **perViewErrors**: `Evision.Mat.t()`.

    Output vector of average re-projection errors estimated for each pattern view.

   This function calibrates a camera using a set of corners of a  Charuco Board. The function
   receives a list of detected corners and its identifiers from several views of the Board.
   The function returns the final re-projection error.
  @deprecated Use CharucoBoard::matchImagePoints and cv::solvePnP

  Python prototype (for reference only):
  ```python3
  calibrateCameraCharucoExtended(charucoCorners, charucoIds, board, imageSize, cameraMatrix, distCoeffs[, rvecs[, tvecs[, stdDeviationsIntrinsics[, stdDeviationsExtrinsics[, perViewErrors[, flags[, criteria]]]]]]]) -> retval, cameraMatrix, distCoeffs, rvecs, tvecs, stdDeviationsIntrinsics, stdDeviationsExtrinsics, perViewErrors
  ```
  """
  @spec calibrateCameraCharucoExtended(list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), Evision.ArUco.CharucoBoard.t(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t()), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def calibrateCameraCharucoExtended(charucoCorners, charucoIds, board, imageSize, cameraMatrix, distCoeffs) when is_list(charucoCorners) and is_list(charucoIds) and is_struct(board, Evision.ArUco.CharucoBoard) and is_tuple(imageSize) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs))
  do
    positional = [
      charucoCorners: Evision.Internal.Structurise.from_struct(charucoCorners),
      charucoIds: Evision.Internal.Structurise.from_struct(charucoIds),
      board: Evision.Internal.Structurise.from_struct(board),
      imageSize: Evision.Internal.Structurise.from_struct(imageSize),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs)
    ]
    :evision_nif.aruco_calibrateCameraCharucoExtended(positional)
    |> to_struct()
  end

  @doc """
  Detect ChArUco Diamond markers

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    input image necessary for corner subpixel.

  - **markerCorners**: `[Evision.Mat]`.

    list of detected marker corners from detectMarkers function.

  - **markerIds**: `Evision.Mat`.

    list of marker ids in markerCorners.

  - **squareMarkerLengthRate**: `float`.

    rate between square and marker length:
    squareMarkerLengthRate = squareLength/markerLength. The real units are not necessary.

  ##### Keyword Arguments
  - **cameraMatrix**: `Evision.Mat`.

    Optional camera calibration matrix.

  - **distCoeffs**: `Evision.Mat`.

    Optional camera distortion coefficients.

  - **dictionary**: `Dictionary`.

    dictionary of markers indicating the type of markers.

  ##### Return
  - **diamondCorners**: `[Evision.Mat]`.

    output list of detected diamond corners (4 corners per diamond). The order
    is the same than in marker corners: top left, top right, bottom right and bottom left. Similar
    format than the corners returned by detectMarkers (e.g std::vector<std::vector<cv::Point2f> > ).

  - **diamondIds**: `Evision.Mat.t()`.

    ids of the diamonds in diamondCorners. The id of each diamond is in fact of
    type Vec4i, so each diamond has 4 ids, which are the ids of the aruco markers composing the
    diamond.

   This function detects Diamond markers from the previous detected ArUco markers. The diamonds
   are returned in the diamondCorners and diamondIds parameters. If camera calibration parameters
   are provided, the diamond search is based on reprojection. If not, diamond search is based on
   homography. Homography is faster than reprojection, but less accurate.
  @deprecated Use CharucoDetector::detectDiamonds

  Python prototype (for reference only):
  ```python3
  detectCharucoDiamond(image, markerCorners, markerIds, squareMarkerLengthRate[, diamondCorners[, diamondIds[, cameraMatrix[, distCoeffs[, dictionary]]]]]) -> diamondCorners, diamondIds
  ```
  """
  @spec detectCharucoDiamond(Evision.Mat.maybe_mat_in(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), number(), [{:cameraMatrix, term()} | {:dictionary, term()} | {:distCoeffs, term()}] | nil) :: {list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectCharucoDiamond(image, markerCorners, markerIds, squareMarkerLengthRate, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(markerCorners) and (is_struct(markerIds, Evision.Mat) or is_struct(markerIds, Nx.Tensor) or is_number(markerIds) or is_tuple(markerIds)) and is_float(squareMarkerLengthRate) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:cameraMatrix, :dictionary, :distCoeffs])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      markerCorners: Evision.Internal.Structurise.from_struct(markerCorners),
      markerIds: Evision.Internal.Structurise.from_struct(markerIds),
      squareMarkerLengthRate: Evision.Internal.Structurise.from_struct(squareMarkerLengthRate)
    ]
    :evision_nif.aruco_detectCharucoDiamond(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Detect ChArUco Diamond markers

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    input image necessary for corner subpixel.

  - **markerCorners**: `[Evision.Mat]`.

    list of detected marker corners from detectMarkers function.

  - **markerIds**: `Evision.Mat`.

    list of marker ids in markerCorners.

  - **squareMarkerLengthRate**: `float`.

    rate between square and marker length:
    squareMarkerLengthRate = squareLength/markerLength. The real units are not necessary.

  ##### Keyword Arguments
  - **cameraMatrix**: `Evision.Mat`.

    Optional camera calibration matrix.

  - **distCoeffs**: `Evision.Mat`.

    Optional camera distortion coefficients.

  - **dictionary**: `Dictionary`.

    dictionary of markers indicating the type of markers.

  ##### Return
  - **diamondCorners**: `[Evision.Mat]`.

    output list of detected diamond corners (4 corners per diamond). The order
    is the same than in marker corners: top left, top right, bottom right and bottom left. Similar
    format than the corners returned by detectMarkers (e.g std::vector<std::vector<cv::Point2f> > ).

  - **diamondIds**: `Evision.Mat.t()`.

    ids of the diamonds in diamondCorners. The id of each diamond is in fact of
    type Vec4i, so each diamond has 4 ids, which are the ids of the aruco markers composing the
    diamond.

   This function detects Diamond markers from the previous detected ArUco markers. The diamonds
   are returned in the diamondCorners and diamondIds parameters. If camera calibration parameters
   are provided, the diamond search is based on reprojection. If not, diamond search is based on
   homography. Homography is faster than reprojection, but less accurate.
  @deprecated Use CharucoDetector::detectDiamonds

  Python prototype (for reference only):
  ```python3
  detectCharucoDiamond(image, markerCorners, markerIds, squareMarkerLengthRate[, diamondCorners[, diamondIds[, cameraMatrix[, distCoeffs[, dictionary]]]]]) -> diamondCorners, diamondIds
  ```
  """
  @spec detectCharucoDiamond(Evision.Mat.maybe_mat_in(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), number()) :: {list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectCharucoDiamond(image, markerCorners, markerIds, squareMarkerLengthRate) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(markerCorners) and (is_struct(markerIds, Evision.Mat) or is_struct(markerIds, Nx.Tensor) or is_number(markerIds) or is_tuple(markerIds)) and is_float(squareMarkerLengthRate)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      markerCorners: Evision.Internal.Structurise.from_struct(markerCorners),
      markerIds: Evision.Internal.Structurise.from_struct(markerIds),
      squareMarkerLengthRate: Evision.Internal.Structurise.from_struct(squareMarkerLengthRate)
    ]
    :evision_nif.aruco_detectCharucoDiamond(positional)
    |> to_struct()
  end

  @doc """
  detect markers

  ##### Positional Arguments
  - **image**: `Evision.Mat`
  - **dictionary**: `Dictionary`

  ##### Keyword Arguments
  - **parameters**: `DetectorParameters`.

  ##### Return
  - **corners**: `[Evision.Mat]`.
  - **ids**: `Evision.Mat.t()`.
  - **rejectedImgPoints**: `[Evision.Mat]`.

  @deprecated Use class ArucoDetector::detectMarkers

  Python prototype (for reference only):
  ```python3
  detectMarkers(image, dictionary[, corners[, ids[, parameters[, rejectedImgPoints]]]]) -> corners, ids, rejectedImgPoints
  ```
  """
  @spec detectMarkers(Evision.Mat.maybe_mat_in(), Evision.ArUco.Dictionary.t(), [{:parameters, term()}] | nil) :: {list(Evision.Mat.t()), Evision.Mat.t(), list(Evision.Mat.t())} | {:error, String.t()}
  def detectMarkers(image, dictionary, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_reference(dictionary) or is_struct(dictionary)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:parameters])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      dictionary: Evision.Internal.Structurise.from_struct(dictionary)
    ]
    :evision_nif.aruco_detectMarkers(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detect markers

  ##### Positional Arguments
  - **image**: `Evision.Mat`
  - **dictionary**: `Dictionary`

  ##### Keyword Arguments
  - **parameters**: `DetectorParameters`.

  ##### Return
  - **corners**: `[Evision.Mat]`.
  - **ids**: `Evision.Mat.t()`.
  - **rejectedImgPoints**: `[Evision.Mat]`.

  @deprecated Use class ArucoDetector::detectMarkers

  Python prototype (for reference only):
  ```python3
  detectMarkers(image, dictionary[, corners[, ids[, parameters[, rejectedImgPoints]]]]) -> corners, ids, rejectedImgPoints
  ```
  """
  @spec detectMarkers(Evision.Mat.maybe_mat_in(), Evision.ArUco.Dictionary.t()) :: {list(Evision.Mat.t()), Evision.Mat.t(), list(Evision.Mat.t())} | {:error, String.t()}
  def detectMarkers(image, dictionary) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_reference(dictionary) or is_struct(dictionary))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      dictionary: Evision.Internal.Structurise.from_struct(dictionary)
    ]
    :evision_nif.aruco_detectMarkers(positional)
    |> to_struct()
  end

  @doc """
  Draw a ChArUco Diamond marker

  ##### Positional Arguments
  - **dictionary**: `Dictionary`.

    dictionary of markers indicating the type of markers.

  - **ids**: `Vec4i`.

    list of 4 ids for each ArUco marker in the ChArUco marker.

  - **squareLength**: `integer()`.

    size of the chessboard squares in pixels.

  - **markerLength**: `integer()`.

    size of the markers in pixels.

  ##### Keyword Arguments
  - **marginSize**: `integer()`.

    minimum margins (in pixels) of the marker in the output image

  - **borderBits**: `integer()`.

    width of the marker borders.

  ##### Return
  - **img**: `Evision.Mat.t()`.

    output image with the marker. The size of this image will be
    3*squareLength + 2*marginSize,.

   This function return the image of a ChArUco marker, ready to be printed.
  @deprecated Use CharucoBoard::generateImage()

  Python prototype (for reference only):
  ```python3
  drawCharucoDiamond(dictionary, ids, squareLength, markerLength[, img[, marginSize[, borderBits]]]) -> img
  ```
  """
  @spec drawCharucoDiamond(Evision.ArUco.Dictionary.t(), {integer(), integer(), integer(), integer()}, integer(), integer(), [{:borderBits, term()} | {:marginSize, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawCharucoDiamond(dictionary, ids, squareLength, markerLength, opts) when (is_reference(dictionary) or is_struct(dictionary)) and is_integer(squareLength) and is_integer(markerLength) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderBits, :marginSize])
    positional = [
      dictionary: Evision.Internal.Structurise.from_struct(dictionary),
      ids: Evision.Internal.Structurise.from_struct(ids),
      squareLength: Evision.Internal.Structurise.from_struct(squareLength),
      markerLength: Evision.Internal.Structurise.from_struct(markerLength)
    ]
    :evision_nif.aruco_drawCharucoDiamond(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Draw a ChArUco Diamond marker

  ##### Positional Arguments
  - **dictionary**: `Dictionary`.

    dictionary of markers indicating the type of markers.

  - **ids**: `Vec4i`.

    list of 4 ids for each ArUco marker in the ChArUco marker.

  - **squareLength**: `integer()`.

    size of the chessboard squares in pixels.

  - **markerLength**: `integer()`.

    size of the markers in pixels.

  ##### Keyword Arguments
  - **marginSize**: `integer()`.

    minimum margins (in pixels) of the marker in the output image

  - **borderBits**: `integer()`.

    width of the marker borders.

  ##### Return
  - **img**: `Evision.Mat.t()`.

    output image with the marker. The size of this image will be
    3*squareLength + 2*marginSize,.

   This function return the image of a ChArUco marker, ready to be printed.
  @deprecated Use CharucoBoard::generateImage()

  Python prototype (for reference only):
  ```python3
  drawCharucoDiamond(dictionary, ids, squareLength, markerLength[, img[, marginSize[, borderBits]]]) -> img
  ```
  """
  @spec drawCharucoDiamond(Evision.ArUco.Dictionary.t(), {integer(), integer(), integer(), integer()}, integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def drawCharucoDiamond(dictionary, ids, squareLength, markerLength) when (is_reference(dictionary) or is_struct(dictionary)) and is_integer(squareLength) and is_integer(markerLength)
  do
    positional = [
      dictionary: Evision.Internal.Structurise.from_struct(dictionary),
      ids: Evision.Internal.Structurise.from_struct(ids),
      squareLength: Evision.Internal.Structurise.from_struct(squareLength),
      markerLength: Evision.Internal.Structurise.from_struct(markerLength)
    ]
    :evision_nif.aruco_drawCharucoDiamond(positional)
    |> to_struct()
  end

  @doc """
  Draws a set of Charuco corners

  ##### Positional Arguments
  - **charucoCorners**: `Evision.Mat`.

    vector of detected charuco corners

  ##### Keyword Arguments
  - **charucoIds**: `Evision.Mat`.

    list of identifiers for each corner in charucoCorners

  - **cornerColor**: `Evision.scalar()`.

    color of the square surrounding each corner

  ##### Return
  - **image**: `Evision.Mat.t()`.

    input/output image. It must have 1 or 3 channels. The number of channels is not
    altered.

   This function draws a set of detected Charuco corners. If identifiers vector is provided, it also
   draws the id of each corner.

  Python prototype (for reference only):
  ```python3
  drawDetectedCornersCharuco(image, charucoCorners[, charucoIds[, cornerColor]]) -> image
  ```
  """
  @spec drawDetectedCornersCharuco(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:charucoIds, term()} | {:cornerColor, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawDetectedCornersCharuco(image, charucoCorners, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(charucoCorners, Evision.Mat) or is_struct(charucoCorners, Nx.Tensor) or is_number(charucoCorners) or is_tuple(charucoCorners)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:charucoIds, :cornerColor])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      charucoCorners: Evision.Internal.Structurise.from_struct(charucoCorners)
    ]
    :evision_nif.aruco_drawDetectedCornersCharuco(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Draws a set of Charuco corners

  ##### Positional Arguments
  - **charucoCorners**: `Evision.Mat`.

    vector of detected charuco corners

  ##### Keyword Arguments
  - **charucoIds**: `Evision.Mat`.

    list of identifiers for each corner in charucoCorners

  - **cornerColor**: `Evision.scalar()`.

    color of the square surrounding each corner

  ##### Return
  - **image**: `Evision.Mat.t()`.

    input/output image. It must have 1 or 3 channels. The number of channels is not
    altered.

   This function draws a set of detected Charuco corners. If identifiers vector is provided, it also
   draws the id of each corner.

  Python prototype (for reference only):
  ```python3
  drawDetectedCornersCharuco(image, charucoCorners[, charucoIds[, cornerColor]]) -> image
  ```
  """
  @spec drawDetectedCornersCharuco(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def drawDetectedCornersCharuco(image, charucoCorners) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(charucoCorners, Evision.Mat) or is_struct(charucoCorners, Nx.Tensor) or is_number(charucoCorners) or is_tuple(charucoCorners))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      charucoCorners: Evision.Internal.Structurise.from_struct(charucoCorners)
    ]
    :evision_nif.aruco_drawDetectedCornersCharuco(positional)
    |> to_struct()
  end

  @doc """
  Draw a set of detected ChArUco Diamond markers

  ##### Positional Arguments
  - **diamondCorners**: `[Evision.Mat]`.

    positions of diamond corners in the same format returned by
    detectCharucoDiamond(). (e.g std::vector<std::vector<cv::Point2f> > ). For N detected markers,
    the dimensions of this array should be Nx4. The order of the corners should be clockwise.

  ##### Keyword Arguments
  - **diamondIds**: `Evision.Mat`.

    vector of identifiers for diamonds in diamondCorners, in the same format
    returned by detectCharucoDiamond() (e.g. std::vector<Vec4i>).
    Optional, if not provided, ids are not painted.

  - **borderColor**: `Evision.scalar()`.

    color of marker borders. Rest of colors (text color and first corner color)
    are calculated based on this one.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    input/output image. It must have 1 or 3 channels. The number of channels is not
    altered.

   Given an array of detected diamonds, this functions draws them in the image. The marker borders
   are painted and the markers identifiers if provided.
   Useful for debugging purposes.

  Python prototype (for reference only):
  ```python3
  drawDetectedDiamonds(image, diamondCorners[, diamondIds[, borderColor]]) -> image
  ```
  """
  @spec drawDetectedDiamonds(Evision.Mat.maybe_mat_in(), list(Evision.Mat.maybe_mat_in()), [{:borderColor, term()} | {:diamondIds, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawDetectedDiamonds(image, diamondCorners, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(diamondCorners) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderColor, :diamondIds])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      diamondCorners: Evision.Internal.Structurise.from_struct(diamondCorners)
    ]
    :evision_nif.aruco_drawDetectedDiamonds(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Draw a set of detected ChArUco Diamond markers

  ##### Positional Arguments
  - **diamondCorners**: `[Evision.Mat]`.

    positions of diamond corners in the same format returned by
    detectCharucoDiamond(). (e.g std::vector<std::vector<cv::Point2f> > ). For N detected markers,
    the dimensions of this array should be Nx4. The order of the corners should be clockwise.

  ##### Keyword Arguments
  - **diamondIds**: `Evision.Mat`.

    vector of identifiers for diamonds in diamondCorners, in the same format
    returned by detectCharucoDiamond() (e.g. std::vector<Vec4i>).
    Optional, if not provided, ids are not painted.

  - **borderColor**: `Evision.scalar()`.

    color of marker borders. Rest of colors (text color and first corner color)
    are calculated based on this one.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    input/output image. It must have 1 or 3 channels. The number of channels is not
    altered.

   Given an array of detected diamonds, this functions draws them in the image. The marker borders
   are painted and the markers identifiers if provided.
   Useful for debugging purposes.

  Python prototype (for reference only):
  ```python3
  drawDetectedDiamonds(image, diamondCorners[, diamondIds[, borderColor]]) -> image
  ```
  """
  @spec drawDetectedDiamonds(Evision.Mat.maybe_mat_in(), list(Evision.Mat.maybe_mat_in())) :: Evision.Mat.t() | {:error, String.t()}
  def drawDetectedDiamonds(image, diamondCorners) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(diamondCorners)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      diamondCorners: Evision.Internal.Structurise.from_struct(diamondCorners)
    ]
    :evision_nif.aruco_drawDetectedDiamonds(positional)
    |> to_struct()
  end

  @doc """
  Draw detected markers in image

  ##### Positional Arguments
  - **corners**: `[Evision.Mat]`.

    positions of marker corners on input image.
    (e.g std::vector<std::vector<cv::Point2f> > ). For N detected markers, the dimensions of
    this array should be Nx4. The order of the corners should be clockwise.

  ##### Keyword Arguments
  - **ids**: `Evision.Mat`.

    vector of identifiers for markers in markersCorners .
    Optional, if not provided, ids are not painted.

  - **borderColor**: `Evision.scalar()`.

    color of marker borders. Rest of colors (text color and first corner color)
    are calculated based on this one to improve visualization.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    input/output image. It must have 1 or 3 channels. The number of channels is not altered.

   Given an array of detected marker corners and its corresponding ids, this functions draws
   the markers in the image. The marker borders are painted and the markers identifiers if provided.
   Useful for debugging purposes.

  Python prototype (for reference only):
  ```python3
  drawDetectedMarkers(image, corners[, ids[, borderColor]]) -> image
  ```
  """
  @spec drawDetectedMarkers(Evision.Mat.maybe_mat_in(), list(Evision.Mat.maybe_mat_in()), [{:borderColor, term()} | {:ids, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawDetectedMarkers(image, corners, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(corners) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderColor, :ids])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      corners: Evision.Internal.Structurise.from_struct(corners)
    ]
    :evision_nif.aruco_drawDetectedMarkers(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Draw detected markers in image

  ##### Positional Arguments
  - **corners**: `[Evision.Mat]`.

    positions of marker corners on input image.
    (e.g std::vector<std::vector<cv::Point2f> > ). For N detected markers, the dimensions of
    this array should be Nx4. The order of the corners should be clockwise.

  ##### Keyword Arguments
  - **ids**: `Evision.Mat`.

    vector of identifiers for markers in markersCorners .
    Optional, if not provided, ids are not painted.

  - **borderColor**: `Evision.scalar()`.

    color of marker borders. Rest of colors (text color and first corner color)
    are calculated based on this one to improve visualization.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    input/output image. It must have 1 or 3 channels. The number of channels is not altered.

   Given an array of detected marker corners and its corresponding ids, this functions draws
   the markers in the image. The marker borders are painted and the markers identifiers if provided.
   Useful for debugging purposes.

  Python prototype (for reference only):
  ```python3
  drawDetectedMarkers(image, corners[, ids[, borderColor]]) -> image
  ```
  """
  @spec drawDetectedMarkers(Evision.Mat.maybe_mat_in(), list(Evision.Mat.maybe_mat_in())) :: Evision.Mat.t() | {:error, String.t()}
  def drawDetectedMarkers(image, corners) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(corners)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      corners: Evision.Internal.Structurise.from_struct(corners)
    ]
    :evision_nif.aruco_drawDetectedMarkers(positional)
    |> to_struct()
  end

  @doc """
  draw planar board

  ##### Positional Arguments
  - **board**: `Board`
  - **outSize**: `Size`
  - **marginSize**: `integer()`
  - **borderBits**: `integer()`

  ##### Return
  - **img**: `Evision.Mat.t()`.

  @deprecated Use Board::generateImage

  Python prototype (for reference only):
  ```python3
  drawPlanarBoard(board, outSize, marginSize, borderBits[, img]) -> img
  ```
  """
  @spec drawPlanarBoard(Evision.ArUco.Board.t(), {number(), number()}, integer(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawPlanarBoard(board, outSize, marginSize, borderBits, opts) when (is_reference(board) or is_struct(board)) and is_tuple(outSize) and is_integer(marginSize) and is_integer(borderBits) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      board: Evision.Internal.Structurise.from_struct(board),
      outSize: Evision.Internal.Structurise.from_struct(outSize),
      marginSize: Evision.Internal.Structurise.from_struct(marginSize),
      borderBits: Evision.Internal.Structurise.from_struct(borderBits)
    ]
    :evision_nif.aruco_drawPlanarBoard(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  draw planar board

  ##### Positional Arguments
  - **board**: `Board`
  - **outSize**: `Size`
  - **marginSize**: `integer()`
  - **borderBits**: `integer()`

  ##### Return
  - **img**: `Evision.Mat.t()`.

  @deprecated Use Board::generateImage

  Python prototype (for reference only):
  ```python3
  drawPlanarBoard(board, outSize, marginSize, borderBits[, img]) -> img
  ```
  """
  @spec drawPlanarBoard(Evision.ArUco.Board.t(), {number(), number()}, integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def drawPlanarBoard(board, outSize, marginSize, borderBits) when (is_reference(board) or is_struct(board)) and is_tuple(outSize) and is_integer(marginSize) and is_integer(borderBits)
  do
    positional = [
      board: Evision.Internal.Structurise.from_struct(board),
      outSize: Evision.Internal.Structurise.from_struct(outSize),
      marginSize: Evision.Internal.Structurise.from_struct(marginSize),
      borderBits: Evision.Internal.Structurise.from_struct(borderBits)
    ]
    :evision_nif.aruco_drawPlanarBoard(positional)
    |> to_struct()
  end

  @doc """
  estimatePoseBoard

  ##### Positional Arguments
  - **corners**: `[Evision.Mat]`
  - **ids**: `Evision.Mat`
  - **board**: `Board`
  - **cameraMatrix**: `Evision.Mat`
  - **distCoeffs**: `Evision.Mat`

  ##### Keyword Arguments
  - **useExtrinsicGuess**: `bool`.

  ##### Return
  - **retval**: `integer()`
  - **rvec**: `Evision.Mat.t()`
  - **tvec**: `Evision.Mat.t()`

  @deprecated Use Board::matchImagePoints and cv::solvePnP

  Python prototype (for reference only):
  ```python3
  estimatePoseBoard(corners, ids, board, cameraMatrix, distCoeffs, rvec, tvec[, useExtrinsicGuess]) -> retval, rvec, tvec
  ```
  """
  @spec estimatePoseBoard(list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.ArUco.Board.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:useExtrinsicGuess, term()}] | nil) :: {integer(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def estimatePoseBoard(corners, ids, board, cameraMatrix, distCoeffs, rvec, tvec, opts) when is_list(corners) and (is_struct(ids, Evision.Mat) or is_struct(ids, Nx.Tensor) or is_number(ids) or is_tuple(ids)) and (is_reference(board) or is_struct(board)) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs)) and (is_struct(rvec, Evision.Mat) or is_struct(rvec, Nx.Tensor) or is_number(rvec) or is_tuple(rvec)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:useExtrinsicGuess])
    positional = [
      corners: Evision.Internal.Structurise.from_struct(corners),
      ids: Evision.Internal.Structurise.from_struct(ids),
      board: Evision.Internal.Structurise.from_struct(board),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs),
      rvec: Evision.Internal.Structurise.from_struct(rvec),
      tvec: Evision.Internal.Structurise.from_struct(tvec)
    ]
    :evision_nif.aruco_estimatePoseBoard(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  estimatePoseBoard

  ##### Positional Arguments
  - **corners**: `[Evision.Mat]`
  - **ids**: `Evision.Mat`
  - **board**: `Board`
  - **cameraMatrix**: `Evision.Mat`
  - **distCoeffs**: `Evision.Mat`

  ##### Keyword Arguments
  - **useExtrinsicGuess**: `bool`.

  ##### Return
  - **retval**: `integer()`
  - **rvec**: `Evision.Mat.t()`
  - **tvec**: `Evision.Mat.t()`

  @deprecated Use Board::matchImagePoints and cv::solvePnP

  Python prototype (for reference only):
  ```python3
  estimatePoseBoard(corners, ids, board, cameraMatrix, distCoeffs, rvec, tvec[, useExtrinsicGuess]) -> retval, rvec, tvec
  ```
  """
  @spec estimatePoseBoard(list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.ArUco.Board.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {integer(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def estimatePoseBoard(corners, ids, board, cameraMatrix, distCoeffs, rvec, tvec) when is_list(corners) and (is_struct(ids, Evision.Mat) or is_struct(ids, Nx.Tensor) or is_number(ids) or is_tuple(ids)) and (is_reference(board) or is_struct(board)) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs)) and (is_struct(rvec, Evision.Mat) or is_struct(rvec, Nx.Tensor) or is_number(rvec) or is_tuple(rvec)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec))
  do
    positional = [
      corners: Evision.Internal.Structurise.from_struct(corners),
      ids: Evision.Internal.Structurise.from_struct(ids),
      board: Evision.Internal.Structurise.from_struct(board),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs),
      rvec: Evision.Internal.Structurise.from_struct(rvec),
      tvec: Evision.Internal.Structurise.from_struct(tvec)
    ]
    :evision_nif.aruco_estimatePoseBoard(positional)
    |> to_struct()
  end

  @doc """
  Pose estimation for a ChArUco board given some of their corners

  ##### Positional Arguments
  - **charucoCorners**: `Evision.Mat`.

    vector of detected charuco corners

  - **charucoIds**: `Evision.Mat`.

    list of identifiers for each corner in charucoCorners

  - **board**: `Evision.ArUco.CharucoBoard`.

    layout of ChArUco board.

  - **cameraMatrix**: `Evision.Mat`.

    input 3x3 floating-point camera matrix
    \\f$A = \\vecthreethree{f_x}{0}{c_x}{0}{f_y}{c_y}{0}{0}{1}\\f$

  - **distCoeffs**: `Evision.Mat`.

    vector of distortion coefficients
    \\f$(k_1, k_2, p_1, p_2[, k_3[, k_4, k_5, k_6],[s_1, s_2, s_3, s_4]])\\f$ of 4, 5, 8 or 12 elements

  ##### Keyword Arguments
  - **useExtrinsicGuess**: `bool`.

    defines whether initial guess for \\b rvec and \\b tvec will be used or not.

  ##### Return
  - **retval**: `bool`
  - **rvec**: `Evision.Mat.t()`.

    Output vector (e.g. cv::Mat) corresponding to the rotation vector of the board
    (see cv::Rodrigues).

  - **tvec**: `Evision.Mat.t()`.

    Output vector (e.g. cv::Mat) corresponding to the translation vector of the board.

   This function estimates a Charuco board pose from some detected corners.
   The function checks if the input corners are enough and valid to perform pose estimation.
   If pose estimation is valid, returns true, else returns false.
  @deprecated Use CharucoBoard::matchImagePoints and cv::solvePnP
  @sa use cv::drawFrameAxes to get world coordinate system axis for object points

  Python prototype (for reference only):
  ```python3
  estimatePoseCharucoBoard(charucoCorners, charucoIds, board, cameraMatrix, distCoeffs, rvec, tvec[, useExtrinsicGuess]) -> retval, rvec, tvec
  ```
  """
  @spec estimatePoseCharucoBoard(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.ArUco.CharucoBoard.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:useExtrinsicGuess, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | false | {:error, String.t()}
  def estimatePoseCharucoBoard(charucoCorners, charucoIds, board, cameraMatrix, distCoeffs, rvec, tvec, opts) when (is_struct(charucoCorners, Evision.Mat) or is_struct(charucoCorners, Nx.Tensor) or is_number(charucoCorners) or is_tuple(charucoCorners)) and (is_struct(charucoIds, Evision.Mat) or is_struct(charucoIds, Nx.Tensor) or is_number(charucoIds) or is_tuple(charucoIds)) and is_struct(board, Evision.ArUco.CharucoBoard) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs)) and (is_struct(rvec, Evision.Mat) or is_struct(rvec, Nx.Tensor) or is_number(rvec) or is_tuple(rvec)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:useExtrinsicGuess])
    positional = [
      charucoCorners: Evision.Internal.Structurise.from_struct(charucoCorners),
      charucoIds: Evision.Internal.Structurise.from_struct(charucoIds),
      board: Evision.Internal.Structurise.from_struct(board),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs),
      rvec: Evision.Internal.Structurise.from_struct(rvec),
      tvec: Evision.Internal.Structurise.from_struct(tvec)
    ]
    :evision_nif.aruco_estimatePoseCharucoBoard(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Pose estimation for a ChArUco board given some of their corners

  ##### Positional Arguments
  - **charucoCorners**: `Evision.Mat`.

    vector of detected charuco corners

  - **charucoIds**: `Evision.Mat`.

    list of identifiers for each corner in charucoCorners

  - **board**: `Evision.ArUco.CharucoBoard`.

    layout of ChArUco board.

  - **cameraMatrix**: `Evision.Mat`.

    input 3x3 floating-point camera matrix
    \\f$A = \\vecthreethree{f_x}{0}{c_x}{0}{f_y}{c_y}{0}{0}{1}\\f$

  - **distCoeffs**: `Evision.Mat`.

    vector of distortion coefficients
    \\f$(k_1, k_2, p_1, p_2[, k_3[, k_4, k_5, k_6],[s_1, s_2, s_3, s_4]])\\f$ of 4, 5, 8 or 12 elements

  ##### Keyword Arguments
  - **useExtrinsicGuess**: `bool`.

    defines whether initial guess for \\b rvec and \\b tvec will be used or not.

  ##### Return
  - **retval**: `bool`
  - **rvec**: `Evision.Mat.t()`.

    Output vector (e.g. cv::Mat) corresponding to the rotation vector of the board
    (see cv::Rodrigues).

  - **tvec**: `Evision.Mat.t()`.

    Output vector (e.g. cv::Mat) corresponding to the translation vector of the board.

   This function estimates a Charuco board pose from some detected corners.
   The function checks if the input corners are enough and valid to perform pose estimation.
   If pose estimation is valid, returns true, else returns false.
  @deprecated Use CharucoBoard::matchImagePoints and cv::solvePnP
  @sa use cv::drawFrameAxes to get world coordinate system axis for object points

  Python prototype (for reference only):
  ```python3
  estimatePoseCharucoBoard(charucoCorners, charucoIds, board, cameraMatrix, distCoeffs, rvec, tvec[, useExtrinsicGuess]) -> retval, rvec, tvec
  ```
  """
  @spec estimatePoseCharucoBoard(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.ArUco.CharucoBoard.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | false | {:error, String.t()}
  def estimatePoseCharucoBoard(charucoCorners, charucoIds, board, cameraMatrix, distCoeffs, rvec, tvec) when (is_struct(charucoCorners, Evision.Mat) or is_struct(charucoCorners, Nx.Tensor) or is_number(charucoCorners) or is_tuple(charucoCorners)) and (is_struct(charucoIds, Evision.Mat) or is_struct(charucoIds, Nx.Tensor) or is_number(charucoIds) or is_tuple(charucoIds)) and is_struct(board, Evision.ArUco.CharucoBoard) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs)) and (is_struct(rvec, Evision.Mat) or is_struct(rvec, Nx.Tensor) or is_number(rvec) or is_tuple(rvec)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec))
  do
    positional = [
      charucoCorners: Evision.Internal.Structurise.from_struct(charucoCorners),
      charucoIds: Evision.Internal.Structurise.from_struct(charucoIds),
      board: Evision.Internal.Structurise.from_struct(board),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs),
      rvec: Evision.Internal.Structurise.from_struct(rvec),
      tvec: Evision.Internal.Structurise.from_struct(tvec)
    ]
    :evision_nif.aruco_estimatePoseCharucoBoard(positional)
    |> to_struct()
  end

  @doc """
  estimatePoseSingleMarkers

  ##### Positional Arguments
  - **corners**: `[Evision.Mat]`
  - **markerLength**: `float`
  - **cameraMatrix**: `Evision.Mat`
  - **distCoeffs**: `Evision.Mat`

  ##### Keyword Arguments
  - **estimateParameters**: `EstimateParameters`.

  ##### Return
  - **rvecs**: `Evision.Mat.t()`.
  - **tvecs**: `Evision.Mat.t()`.
  - **objPoints**: `Evision.Mat.t()`.

  @deprecated Use cv::solvePnP

  Python prototype (for reference only):
  ```python3
  estimatePoseSingleMarkers(corners, markerLength, cameraMatrix, distCoeffs[, rvecs[, tvecs[, objPoints[, estimateParameters]]]]) -> rvecs, tvecs, objPoints
  ```
  """
  @spec estimatePoseSingleMarkers(list(Evision.Mat.maybe_mat_in()), number(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:estimateParameters, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def estimatePoseSingleMarkers(corners, markerLength, cameraMatrix, distCoeffs, opts) when is_list(corners) and is_float(markerLength) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:estimateParameters])
    positional = [
      corners: Evision.Internal.Structurise.from_struct(corners),
      markerLength: Evision.Internal.Structurise.from_struct(markerLength),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs)
    ]
    :evision_nif.aruco_estimatePoseSingleMarkers(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  estimatePoseSingleMarkers

  ##### Positional Arguments
  - **corners**: `[Evision.Mat]`
  - **markerLength**: `float`
  - **cameraMatrix**: `Evision.Mat`
  - **distCoeffs**: `Evision.Mat`

  ##### Keyword Arguments
  - **estimateParameters**: `EstimateParameters`.

  ##### Return
  - **rvecs**: `Evision.Mat.t()`.
  - **tvecs**: `Evision.Mat.t()`.
  - **objPoints**: `Evision.Mat.t()`.

  @deprecated Use cv::solvePnP

  Python prototype (for reference only):
  ```python3
  estimatePoseSingleMarkers(corners, markerLength, cameraMatrix, distCoeffs[, rvecs[, tvecs[, objPoints[, estimateParameters]]]]) -> rvecs, tvecs, objPoints
  ```
  """
  @spec estimatePoseSingleMarkers(list(Evision.Mat.maybe_mat_in()), number(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def estimatePoseSingleMarkers(corners, markerLength, cameraMatrix, distCoeffs) when is_list(corners) and is_float(markerLength) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs))
  do
    positional = [
      corners: Evision.Internal.Structurise.from_struct(corners),
      markerLength: Evision.Internal.Structurise.from_struct(markerLength),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs)
    ]
    :evision_nif.aruco_estimatePoseSingleMarkers(positional)
    |> to_struct()
  end

  @doc """
  Extend base dictionary by new nMarkers

  ##### Positional Arguments
  - **nMarkers**: `integer()`.

    number of markers in the dictionary

  - **markerSize**: `integer()`.

    number of bits per dimension of each markers

  ##### Keyword Arguments
  - **baseDictionary**: `Dictionary`.

    Include the markers in this dictionary at the beginning (optional)

  - **randomSeed**: `integer()`.

    a user supplied seed for theRNG()

  ##### Return
  - **retval**: `Dictionary`

   This function creates a new dictionary composed by nMarkers markers and each markers composed
   by markerSize x markerSize bits. If baseDictionary is provided, its markers are directly
   included and the rest are generated based on them. If the size of baseDictionary is higher
   than nMarkers, only the first nMarkers in baseDictionary are taken and no new marker is added.

  Python prototype (for reference only):
  ```python3
  extendDictionary(nMarkers, markerSize[, baseDictionary[, randomSeed]]) -> retval
  ```
  """
  @spec extendDictionary(integer(), integer(), [{:baseDictionary, term()} | {:randomSeed, term()}] | nil) :: Evision.ArUco.Dictionary.t() | {:error, String.t()}
  def extendDictionary(nMarkers, markerSize, opts) when is_integer(nMarkers) and is_integer(markerSize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:baseDictionary, :randomSeed])
    positional = [
      nMarkers: Evision.Internal.Structurise.from_struct(nMarkers),
      markerSize: Evision.Internal.Structurise.from_struct(markerSize)
    ]
    :evision_nif.aruco_extendDictionary(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Extend base dictionary by new nMarkers

  ##### Positional Arguments
  - **nMarkers**: `integer()`.

    number of markers in the dictionary

  - **markerSize**: `integer()`.

    number of bits per dimension of each markers

  ##### Keyword Arguments
  - **baseDictionary**: `Dictionary`.

    Include the markers in this dictionary at the beginning (optional)

  - **randomSeed**: `integer()`.

    a user supplied seed for theRNG()

  ##### Return
  - **retval**: `Dictionary`

   This function creates a new dictionary composed by nMarkers markers and each markers composed
   by markerSize x markerSize bits. If baseDictionary is provided, its markers are directly
   included and the rest are generated based on them. If the size of baseDictionary is higher
   than nMarkers, only the first nMarkers in baseDictionary are taken and no new marker is added.

  Python prototype (for reference only):
  ```python3
  extendDictionary(nMarkers, markerSize[, baseDictionary[, randomSeed]]) -> retval
  ```
  """
  @spec extendDictionary(integer(), integer()) :: Evision.ArUco.Dictionary.t() | {:error, String.t()}
  def extendDictionary(nMarkers, markerSize) when is_integer(nMarkers) and is_integer(markerSize)
  do
    positional = [
      nMarkers: Evision.Internal.Structurise.from_struct(nMarkers),
      markerSize: Evision.Internal.Structurise.from_struct(markerSize)
    ]
    :evision_nif.aruco_extendDictionary(positional)
    |> to_struct()
  end

  @doc """
  Generate a canonical marker image

  ##### Positional Arguments
  - **dictionary**: `Dictionary`.

    dictionary of markers indicating the type of markers

  - **id**: `integer()`.

    identifier of the marker that will be returned. It has to be a valid id in the specified dictionary.

  - **sidePixels**: `integer()`.

    size of the image in pixels

  ##### Keyword Arguments
  - **borderBits**: `integer()`.

    width of the marker border.

  ##### Return
  - **img**: `Evision.Mat.t()`.

    output image with the marker

   This function returns a marker image in its canonical form (i.e. ready to be printed)

  Python prototype (for reference only):
  ```python3
  generateImageMarker(dictionary, id, sidePixels[, img[, borderBits]]) -> img
  ```
  """
  @spec generateImageMarker(Evision.ArUco.Dictionary.t(), integer(), integer(), [{:borderBits, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def generateImageMarker(dictionary, id, sidePixels, opts) when is_integer(id) and is_integer(sidePixels) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderBits])
    positional = [
      dictionary: Evision.Internal.Structurise.from_struct(dictionary),
      id: Evision.Internal.Structurise.from_struct(id),
      sidePixels: Evision.Internal.Structurise.from_struct(sidePixels)
    ]
    :evision_nif.aruco_generateImageMarker(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Generate a canonical marker image

  ##### Positional Arguments
  - **dictionary**: `Dictionary`.

    dictionary of markers indicating the type of markers

  - **id**: `integer()`.

    identifier of the marker that will be returned. It has to be a valid id in the specified dictionary.

  - **sidePixels**: `integer()`.

    size of the image in pixels

  ##### Keyword Arguments
  - **borderBits**: `integer()`.

    width of the marker border.

  ##### Return
  - **img**: `Evision.Mat.t()`.

    output image with the marker

   This function returns a marker image in its canonical form (i.e. ready to be printed)

  Python prototype (for reference only):
  ```python3
  generateImageMarker(dictionary, id, sidePixels[, img[, borderBits]]) -> img
  ```
  """
  @spec generateImageMarker(Evision.ArUco.Dictionary.t(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def generateImageMarker(dictionary, id, sidePixels) when is_integer(id) and is_integer(sidePixels)
  do
    positional = [
      dictionary: Evision.Internal.Structurise.from_struct(dictionary),
      id: Evision.Internal.Structurise.from_struct(id),
      sidePixels: Evision.Internal.Structurise.from_struct(sidePixels)
    ]
    :evision_nif.aruco_generateImageMarker(positional)
    |> to_struct()
  end

  @doc """
  get board object and image points

  ##### Positional Arguments
  - **board**: `Board`
  - **detectedCorners**: `[Evision.Mat]`
  - **detectedIds**: `Evision.Mat`

  ##### Return
  - **objPoints**: `Evision.Mat.t()`.
  - **imgPoints**: `Evision.Mat.t()`.

  @deprecated Use Board::matchImagePoints

  Python prototype (for reference only):
  ```python3
  getBoardObjectAndImagePoints(board, detectedCorners, detectedIds[, objPoints[, imgPoints]]) -> objPoints, imgPoints
  ```
  """
  @spec getBoardObjectAndImagePoints(Evision.ArUco.Board.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getBoardObjectAndImagePoints(board, detectedCorners, detectedIds, opts) when (is_reference(board) or is_struct(board)) and is_list(detectedCorners) and (is_struct(detectedIds, Evision.Mat) or is_struct(detectedIds, Nx.Tensor) or is_number(detectedIds) or is_tuple(detectedIds)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      board: Evision.Internal.Structurise.from_struct(board),
      detectedCorners: Evision.Internal.Structurise.from_struct(detectedCorners),
      detectedIds: Evision.Internal.Structurise.from_struct(detectedIds)
    ]
    :evision_nif.aruco_getBoardObjectAndImagePoints(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  get board object and image points

  ##### Positional Arguments
  - **board**: `Board`
  - **detectedCorners**: `[Evision.Mat]`
  - **detectedIds**: `Evision.Mat`

  ##### Return
  - **objPoints**: `Evision.Mat.t()`.
  - **imgPoints**: `Evision.Mat.t()`.

  @deprecated Use Board::matchImagePoints

  Python prototype (for reference only):
  ```python3
  getBoardObjectAndImagePoints(board, detectedCorners, detectedIds[, objPoints[, imgPoints]]) -> objPoints, imgPoints
  ```
  """
  @spec getBoardObjectAndImagePoints(Evision.ArUco.Board.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getBoardObjectAndImagePoints(board, detectedCorners, detectedIds) when (is_reference(board) or is_struct(board)) and is_list(detectedCorners) and (is_struct(detectedIds, Evision.Mat) or is_struct(detectedIds, Nx.Tensor) or is_number(detectedIds) or is_tuple(detectedIds))
  do
    positional = [
      board: Evision.Internal.Structurise.from_struct(board),
      detectedCorners: Evision.Internal.Structurise.from_struct(detectedCorners),
      detectedIds: Evision.Internal.Structurise.from_struct(detectedIds)
    ]
    :evision_nif.aruco_getBoardObjectAndImagePoints(positional)
    |> to_struct()
  end

  @doc """
  Returns one of the predefined dictionaries referenced by DICT_*.

  ##### Positional Arguments
  - **dict**: `integer()`

  ##### Return
  - **retval**: `Dictionary`

  Python prototype (for reference only):
  ```python3
  getPredefinedDictionary(dict) -> retval
  ```
  """
  @spec getPredefinedDictionary(integer()) :: Evision.ArUco.Dictionary.t() | {:error, String.t()}
  def getPredefinedDictionary(dict) when is_integer(dict)
  do
    positional = [
      dict: Evision.Internal.Structurise.from_struct(dict)
    ]
    :evision_nif.aruco_getPredefinedDictionary(positional)
    |> to_struct()
  end

  @doc """
  Interpolate position of ChArUco board corners

  ##### Positional Arguments
  - **markerCorners**: `[Evision.Mat]`.

    vector of already detected markers corners. For each marker, its four
    corners are provided, (e.g std::vector<std::vector<cv::Point2f> > ). For N detected markers, the
    dimensions of this array should be Nx4. The order of the corners should be clockwise.

  - **markerIds**: `Evision.Mat`.

    list of identifiers for each marker in corners

  - **image**: `Evision.Mat`.

    input image necesary for corner refinement. Note that markers are not detected and
    should be sent in corners and ids parameters.

  - **board**: `Evision.ArUco.CharucoBoard`.

    layout of ChArUco board.

  ##### Keyword Arguments
  - **cameraMatrix**: `Evision.Mat`.

    optional 3x3 floating-point camera matrix
    \\f$A = \\vecthreethree{f_x}{0}{c_x}{0}{f_y}{c_y}{0}{0}{1}\\f$

  - **distCoeffs**: `Evision.Mat`.

    optional vector of distortion coefficients
    \\f$(k_1, k_2, p_1, p_2[, k_3[, k_4, k_5, k_6],[s_1, s_2, s_3, s_4]])\\f$ of 4, 5, 8 or 12 elements

  - **minMarkers**: `integer()`.

    number of adjacent markers that must be detected to return a charuco corner

  ##### Return
  - **retval**: `integer()`
  - **charucoCorners**: `Evision.Mat.t()`.

    interpolated chessboard corners

  - **charucoIds**: `Evision.Mat.t()`.

    interpolated chessboard corners identifiers

   This function receives the detected markers and returns the 2D position of the chessboard corners
   from a ChArUco board using the detected Aruco markers. If camera parameters are provided,
   the process is based in an approximated pose estimation, else it is based on local homography.
   Only visible corners are returned. For each corner, its corresponding identifier is
   also returned in charucoIds.
   The function returns the number of interpolated corners.
  @deprecated Use CharucoDetector::detectBoard

  Python prototype (for reference only):
  ```python3
  interpolateCornersCharuco(markerCorners, markerIds, image, board[, charucoCorners[, charucoIds[, cameraMatrix[, distCoeffs[, minMarkers]]]]]) -> retval, charucoCorners, charucoIds
  ```
  """
  @spec interpolateCornersCharuco(list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.ArUco.CharucoBoard.t(), [{:cameraMatrix, term()} | {:distCoeffs, term()} | {:minMarkers, term()}] | nil) :: {integer(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def interpolateCornersCharuco(markerCorners, markerIds, image, board, opts) when is_list(markerCorners) and (is_struct(markerIds, Evision.Mat) or is_struct(markerIds, Nx.Tensor) or is_number(markerIds) or is_tuple(markerIds)) and (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_struct(board, Evision.ArUco.CharucoBoard) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:cameraMatrix, :distCoeffs, :minMarkers])
    positional = [
      markerCorners: Evision.Internal.Structurise.from_struct(markerCorners),
      markerIds: Evision.Internal.Structurise.from_struct(markerIds),
      image: Evision.Internal.Structurise.from_struct(image),
      board: Evision.Internal.Structurise.from_struct(board)
    ]
    :evision_nif.aruco_interpolateCornersCharuco(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Interpolate position of ChArUco board corners

  ##### Positional Arguments
  - **markerCorners**: `[Evision.Mat]`.

    vector of already detected markers corners. For each marker, its four
    corners are provided, (e.g std::vector<std::vector<cv::Point2f> > ). For N detected markers, the
    dimensions of this array should be Nx4. The order of the corners should be clockwise.

  - **markerIds**: `Evision.Mat`.

    list of identifiers for each marker in corners

  - **image**: `Evision.Mat`.

    input image necesary for corner refinement. Note that markers are not detected and
    should be sent in corners and ids parameters.

  - **board**: `Evision.ArUco.CharucoBoard`.

    layout of ChArUco board.

  ##### Keyword Arguments
  - **cameraMatrix**: `Evision.Mat`.

    optional 3x3 floating-point camera matrix
    \\f$A = \\vecthreethree{f_x}{0}{c_x}{0}{f_y}{c_y}{0}{0}{1}\\f$

  - **distCoeffs**: `Evision.Mat`.

    optional vector of distortion coefficients
    \\f$(k_1, k_2, p_1, p_2[, k_3[, k_4, k_5, k_6],[s_1, s_2, s_3, s_4]])\\f$ of 4, 5, 8 or 12 elements

  - **minMarkers**: `integer()`.

    number of adjacent markers that must be detected to return a charuco corner

  ##### Return
  - **retval**: `integer()`
  - **charucoCorners**: `Evision.Mat.t()`.

    interpolated chessboard corners

  - **charucoIds**: `Evision.Mat.t()`.

    interpolated chessboard corners identifiers

   This function receives the detected markers and returns the 2D position of the chessboard corners
   from a ChArUco board using the detected Aruco markers. If camera parameters are provided,
   the process is based in an approximated pose estimation, else it is based on local homography.
   Only visible corners are returned. For each corner, its corresponding identifier is
   also returned in charucoIds.
   The function returns the number of interpolated corners.
  @deprecated Use CharucoDetector::detectBoard

  Python prototype (for reference only):
  ```python3
  interpolateCornersCharuco(markerCorners, markerIds, image, board[, charucoCorners[, charucoIds[, cameraMatrix[, distCoeffs[, minMarkers]]]]]) -> retval, charucoCorners, charucoIds
  ```
  """
  @spec interpolateCornersCharuco(list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.ArUco.CharucoBoard.t()) :: {integer(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def interpolateCornersCharuco(markerCorners, markerIds, image, board) when is_list(markerCorners) and (is_struct(markerIds, Evision.Mat) or is_struct(markerIds, Nx.Tensor) or is_number(markerIds) or is_tuple(markerIds)) and (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_struct(board, Evision.ArUco.CharucoBoard)
  do
    positional = [
      markerCorners: Evision.Internal.Structurise.from_struct(markerCorners),
      markerIds: Evision.Internal.Structurise.from_struct(markerIds),
      image: Evision.Internal.Structurise.from_struct(image),
      board: Evision.Internal.Structurise.from_struct(board)
    ]
    :evision_nif.aruco_interpolateCornersCharuco(positional)
    |> to_struct()
  end

  @doc """
  refine detected markers

  ##### Positional Arguments
  - **image**: `Evision.Mat`
  - **board**: `Board`

  ##### Keyword Arguments
  - **cameraMatrix**: `Evision.Mat`.
  - **distCoeffs**: `Evision.Mat`.
  - **minRepDistance**: `float`.
  - **errorCorrectionRate**: `float`.
  - **checkAllOrders**: `bool`.
  - **parameters**: `DetectorParameters`.

  ##### Return
  - **detectedCorners**: `[Evision.Mat]`
  - **detectedIds**: `Evision.Mat.t()`
  - **rejectedCorners**: `[Evision.Mat]`
  - **recoveredIdxs**: `Evision.Mat.t()`.

  @deprecated Use class ArucoDetector::refineDetectedMarkers

  Python prototype (for reference only):
  ```python3
  refineDetectedMarkers(image, board, detectedCorners, detectedIds, rejectedCorners[, cameraMatrix[, distCoeffs[, minRepDistance[, errorCorrectionRate[, checkAllOrders[, recoveredIdxs[, parameters]]]]]]]) -> detectedCorners, detectedIds, rejectedCorners, recoveredIdxs
  ```
  """
  @spec refineDetectedMarkers(Evision.Mat.maybe_mat_in(), Evision.ArUco.Board.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), list(Evision.Mat.maybe_mat_in()), [{:cameraMatrix, term()} | {:checkAllOrders, term()} | {:distCoeffs, term()} | {:errorCorrectionRate, term()} | {:minRepDistance, term()} | {:parameters, term()}] | nil) :: {list(Evision.Mat.t()), Evision.Mat.t(), list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def refineDetectedMarkers(image, board, detectedCorners, detectedIds, rejectedCorners, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_reference(board) or is_struct(board)) and is_list(detectedCorners) and (is_struct(detectedIds, Evision.Mat) or is_struct(detectedIds, Nx.Tensor) or is_number(detectedIds) or is_tuple(detectedIds)) and is_list(rejectedCorners) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:cameraMatrix, :checkAllOrders, :distCoeffs, :errorCorrectionRate, :minRepDistance, :parameters])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      board: Evision.Internal.Structurise.from_struct(board),
      detectedCorners: Evision.Internal.Structurise.from_struct(detectedCorners),
      detectedIds: Evision.Internal.Structurise.from_struct(detectedIds),
      rejectedCorners: Evision.Internal.Structurise.from_struct(rejectedCorners)
    ]
    :evision_nif.aruco_refineDetectedMarkers(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  refine detected markers

  ##### Positional Arguments
  - **image**: `Evision.Mat`
  - **board**: `Board`

  ##### Keyword Arguments
  - **cameraMatrix**: `Evision.Mat`.
  - **distCoeffs**: `Evision.Mat`.
  - **minRepDistance**: `float`.
  - **errorCorrectionRate**: `float`.
  - **checkAllOrders**: `bool`.
  - **parameters**: `DetectorParameters`.

  ##### Return
  - **detectedCorners**: `[Evision.Mat]`
  - **detectedIds**: `Evision.Mat.t()`
  - **rejectedCorners**: `[Evision.Mat]`
  - **recoveredIdxs**: `Evision.Mat.t()`.

  @deprecated Use class ArucoDetector::refineDetectedMarkers

  Python prototype (for reference only):
  ```python3
  refineDetectedMarkers(image, board, detectedCorners, detectedIds, rejectedCorners[, cameraMatrix[, distCoeffs[, minRepDistance[, errorCorrectionRate[, checkAllOrders[, recoveredIdxs[, parameters]]]]]]]) -> detectedCorners, detectedIds, rejectedCorners, recoveredIdxs
  ```
  """
  @spec refineDetectedMarkers(Evision.Mat.maybe_mat_in(), Evision.ArUco.Board.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), list(Evision.Mat.maybe_mat_in())) :: {list(Evision.Mat.t()), Evision.Mat.t(), list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def refineDetectedMarkers(image, board, detectedCorners, detectedIds, rejectedCorners) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_reference(board) or is_struct(board)) and is_list(detectedCorners) and (is_struct(detectedIds, Evision.Mat) or is_struct(detectedIds, Nx.Tensor) or is_number(detectedIds) or is_tuple(detectedIds)) and is_list(rejectedCorners)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      board: Evision.Internal.Structurise.from_struct(board),
      detectedCorners: Evision.Internal.Structurise.from_struct(detectedCorners),
      detectedIds: Evision.Internal.Structurise.from_struct(detectedIds),
      rejectedCorners: Evision.Internal.Structurise.from_struct(rejectedCorners)
    ]
    :evision_nif.aruco_refineDetectedMarkers(positional)
    |> to_struct()
  end

  @doc """
  testCharucoCornersCollinear

  ##### Positional Arguments
  - **board**: `Evision.ArUco.CharucoBoard`
  - **charucoIds**: `Evision.Mat`

  ##### Return
  - **retval**: `bool`

  @deprecated Use CharucoBoard::checkCharucoCornersCollinear

  Python prototype (for reference only):
  ```python3
  testCharucoCornersCollinear(board, charucoIds) -> retval
  ```
  """
  @spec testCharucoCornersCollinear(Evision.ArUco.CharucoBoard.t(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def testCharucoCornersCollinear(board, charucoIds) when is_struct(board, Evision.ArUco.CharucoBoard) and (is_struct(charucoIds, Evision.Mat) or is_struct(charucoIds, Nx.Tensor) or is_number(charucoIds) or is_tuple(charucoIds))
  do
    positional = [
      board: Evision.Internal.Structurise.from_struct(board),
      charucoIds: Evision.Internal.Structurise.from_struct(charucoIds)
    ]
    :evision_nif.aruco_testCharucoCornersCollinear(positional)
    |> to_struct()
  end
end
