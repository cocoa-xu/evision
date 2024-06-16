defmodule Evision.FishEye do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `FishEye` struct.

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
  def to_struct({:ok, %{class: Evision.FishEye, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.FishEye, ref: ref}) do
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
  import Bitwise

  @type enum :: integer()
  @doc enum: true
  def cv_CALIB_USE_INTRINSIC_GUESS, do: bsl(1, 0)
  @doc enum: true
  def cv_CALIB_RECOMPUTE_EXTRINSIC, do: bsl(1, 1)
  @doc enum: true
  def cv_CALIB_CHECK_COND, do: bsl(1, 2)
  @doc enum: true
  def cv_CALIB_FIX_SKEW, do: bsl(1, 3)
  @doc enum: true
  def cv_CALIB_FIX_K1, do: bsl(1, 4)
  @doc enum: true
  def cv_CALIB_FIX_K2, do: bsl(1, 5)
  @doc enum: true
  def cv_CALIB_FIX_K3, do: bsl(1, 6)
  @doc enum: true
  def cv_CALIB_FIX_K4, do: bsl(1, 7)
  @doc enum: true
  def cv_CALIB_FIX_INTRINSIC, do: bsl(1, 8)
  @doc enum: true
  def cv_CALIB_FIX_PRINCIPAL_POINT, do: bsl(1, 9)
  @doc enum: true
  def cv_CALIB_ZERO_DISPARITY, do: bsl(1, 10)
  @doc enum: true
  def cv_CALIB_FIX_FOCAL_LENGTH, do: bsl(1, 11)


  @doc """
  Performs camera calibration

  ##### Positional Arguments
  - **objectPoints**: `[Evision.Mat]`.

    vector of vectors of calibration pattern points in the calibration pattern
    coordinate space.

  - **imagePoints**: `[Evision.Mat]`.

    vector of vectors of the projections of calibration pattern points.
    imagePoints.size() and objectPoints.size() and imagePoints[i].size() must be equal to
    objectPoints[i].size() for each i.

  - **image_size**: `Size`.

    Size of the image used only to initialize the camera intrinsic matrix.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Different flags that may be zero or a combination of the following values:
    - @ref fisheye::CALIB_USE_INTRINSIC_GUESS  cameraMatrix contains valid initial values of
      fx, fy, cx, cy that are optimized further. Otherwise, (cx, cy) is initially set to the image
      center ( imageSize is used), and focal distances are computed in a least-squares fashion.
    - @ref fisheye::CALIB_RECOMPUTE_EXTRINSIC  Extrinsic will be recomputed after each iteration
      of intrinsic optimization.
    - @ref fisheye::CALIB_CHECK_COND  The functions will check validity of condition number.
    - @ref fisheye::CALIB_FIX_SKEW  Skew coefficient (alpha) is set to zero and stay zero.
    - @ref fisheye::CALIB_FIX_K1,..., @ref fisheye::CALIB_FIX_K4 Selected distortion coefficients
      are set to zeros and stay zero.
    - @ref fisheye::CALIB_FIX_PRINCIPAL_POINT  The principal point is not changed during the global
      optimization. It stays at the center or at a different location specified when @ref fisheye::CALIB_USE_INTRINSIC_GUESS is set too.
    - @ref fisheye::CALIB_FIX_FOCAL_LENGTH The focal length is not changed during the global
      optimization. It is the \\f$max(width,height)/\\pi\\f$ or the provided \\f$f_x\\f$, \\f$f_y\\f$ when @ref fisheye::CALIB_USE_INTRINSIC_GUESS is set too.

  - **criteria**: `TermCriteria`.

    Termination criteria for the iterative optimization algorithm.

  ##### Return
  - **retval**: `double`
  - **k**: `Evision.Mat.t()`.

    Output 3x3 floating-point camera intrinsic matrix
    \\f$\\cameramatrix{A}\\f$ . If

  - **d**: `Evision.Mat.t()`.

    Output vector of distortion coefficients \\f$\\distcoeffsfisheye\\f$.

  - **rvecs**: `[Evision.Mat]`.

    Output vector of rotation vectors (see Rodrigues ) estimated for each pattern view.
    That is, each k-th rotation vector together with the corresponding k-th translation vector (see
    the next output parameter description) brings the calibration pattern from the model coordinate
    space (in which object points are specified) to the world coordinate space, that is, a real
    position of the calibration pattern in the k-th pattern view (k=0.. *M* -1).

  - **tvecs**: `[Evision.Mat]`.

    Output vector of translation vectors estimated for each pattern view.

  @ref fisheye::CALIB_USE_INTRINSIC_GUESS is specified, some or all of fx, fy, cx, cy must be
  initialized before calling the function.

  Python prototype (for reference only):
  ```python3
  calibrate(objectPoints, imagePoints, image_size, K, D[, rvecs[, tvecs[, flags[, criteria]]]]) -> retval, K, D, rvecs, tvecs
  ```
  """
  @spec calibrate(list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:criteria, term()} | {:flags, term()}] | nil) :: {number(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t())} | {:error, String.t()}
  def calibrate(objectPoints, imagePoints, image_size, k, d, opts) when is_list(objectPoints) and is_list(imagePoints) and is_tuple(image_size) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :flags])
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      imagePoints: Evision.Internal.Structurise.from_struct(imagePoints),
      image_size: Evision.Internal.Structurise.from_struct(image_size),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d)
    ]
    :evision_nif.fisheye_calibrate(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Performs camera calibration

  ##### Positional Arguments
  - **objectPoints**: `[Evision.Mat]`.

    vector of vectors of calibration pattern points in the calibration pattern
    coordinate space.

  - **imagePoints**: `[Evision.Mat]`.

    vector of vectors of the projections of calibration pattern points.
    imagePoints.size() and objectPoints.size() and imagePoints[i].size() must be equal to
    objectPoints[i].size() for each i.

  - **image_size**: `Size`.

    Size of the image used only to initialize the camera intrinsic matrix.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Different flags that may be zero or a combination of the following values:
    - @ref fisheye::CALIB_USE_INTRINSIC_GUESS  cameraMatrix contains valid initial values of
      fx, fy, cx, cy that are optimized further. Otherwise, (cx, cy) is initially set to the image
      center ( imageSize is used), and focal distances are computed in a least-squares fashion.
    - @ref fisheye::CALIB_RECOMPUTE_EXTRINSIC  Extrinsic will be recomputed after each iteration
      of intrinsic optimization.
    - @ref fisheye::CALIB_CHECK_COND  The functions will check validity of condition number.
    - @ref fisheye::CALIB_FIX_SKEW  Skew coefficient (alpha) is set to zero and stay zero.
    - @ref fisheye::CALIB_FIX_K1,..., @ref fisheye::CALIB_FIX_K4 Selected distortion coefficients
      are set to zeros and stay zero.
    - @ref fisheye::CALIB_FIX_PRINCIPAL_POINT  The principal point is not changed during the global
      optimization. It stays at the center or at a different location specified when @ref fisheye::CALIB_USE_INTRINSIC_GUESS is set too.
    - @ref fisheye::CALIB_FIX_FOCAL_LENGTH The focal length is not changed during the global
      optimization. It is the \\f$max(width,height)/\\pi\\f$ or the provided \\f$f_x\\f$, \\f$f_y\\f$ when @ref fisheye::CALIB_USE_INTRINSIC_GUESS is set too.

  - **criteria**: `TermCriteria`.

    Termination criteria for the iterative optimization algorithm.

  ##### Return
  - **retval**: `double`
  - **k**: `Evision.Mat.t()`.

    Output 3x3 floating-point camera intrinsic matrix
    \\f$\\cameramatrix{A}\\f$ . If

  - **d**: `Evision.Mat.t()`.

    Output vector of distortion coefficients \\f$\\distcoeffsfisheye\\f$.

  - **rvecs**: `[Evision.Mat]`.

    Output vector of rotation vectors (see Rodrigues ) estimated for each pattern view.
    That is, each k-th rotation vector together with the corresponding k-th translation vector (see
    the next output parameter description) brings the calibration pattern from the model coordinate
    space (in which object points are specified) to the world coordinate space, that is, a real
    position of the calibration pattern in the k-th pattern view (k=0.. *M* -1).

  - **tvecs**: `[Evision.Mat]`.

    Output vector of translation vectors estimated for each pattern view.

  @ref fisheye::CALIB_USE_INTRINSIC_GUESS is specified, some or all of fx, fy, cx, cy must be
  initialized before calling the function.

  Python prototype (for reference only):
  ```python3
  calibrate(objectPoints, imagePoints, image_size, K, D[, rvecs[, tvecs[, flags[, criteria]]]]) -> retval, K, D, rvecs, tvecs
  ```
  """
  @spec calibrate(list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t())} | {:error, String.t()}
  def calibrate(objectPoints, imagePoints, image_size, k, d) when is_list(objectPoints) and is_list(imagePoints) and is_tuple(image_size) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d))
  do
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      imagePoints: Evision.Internal.Structurise.from_struct(imagePoints),
      image_size: Evision.Internal.Structurise.from_struct(image_size),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d)
    ]
    :evision_nif.fisheye_calibrate(positional)
    |> to_struct()
  end

  @doc """
  Distorts 2D points using fisheye model.

  ##### Positional Arguments
  - **undistorted**: `Evision.Mat`.

    Array of object points, 1xN/Nx1 2-channel (or vector\\<Point2f\\> ), where N is
    the number of points in the view.

  - **k**: `Evision.Mat`.

    Camera intrinsic matrix \\f$cameramatrix{K}\\f$.

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$\\distcoeffsfisheye\\f$.

  ##### Keyword Arguments
  - **alpha**: `double`.

    The skew coefficient.

  ##### Return
  - **distorted**: `Evision.Mat.t()`.

    Output array of image points, 1xN/Nx1 2-channel, or vector\\<Point2f\\> .

  Note that the function assumes the camera intrinsic matrix of the undistorted points to be identity.
  This means if you want to distort image points you have to multiply them with \\f$K^{-1}\\f$.

  Python prototype (for reference only):
  ```python3
  distortPoints(undistorted, K, D[, distorted[, alpha]]) -> distorted
  ```
  """
  @spec distortPoints(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:alpha, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def distortPoints(undistorted, k, d, opts) when (is_struct(undistorted, Evision.Mat) or is_struct(undistorted, Nx.Tensor) or is_number(undistorted) or is_tuple(undistorted)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:alpha])
    positional = [
      undistorted: Evision.Internal.Structurise.from_struct(undistorted),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d)
    ]
    :evision_nif.fisheye_distortPoints(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Distorts 2D points using fisheye model.

  ##### Positional Arguments
  - **undistorted**: `Evision.Mat`.

    Array of object points, 1xN/Nx1 2-channel (or vector\\<Point2f\\> ), where N is
    the number of points in the view.

  - **k**: `Evision.Mat`.

    Camera intrinsic matrix \\f$cameramatrix{K}\\f$.

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$\\distcoeffsfisheye\\f$.

  ##### Keyword Arguments
  - **alpha**: `double`.

    The skew coefficient.

  ##### Return
  - **distorted**: `Evision.Mat.t()`.

    Output array of image points, 1xN/Nx1 2-channel, or vector\\<Point2f\\> .

  Note that the function assumes the camera intrinsic matrix of the undistorted points to be identity.
  This means if you want to distort image points you have to multiply them with \\f$K^{-1}\\f$.

  Python prototype (for reference only):
  ```python3
  distortPoints(undistorted, K, D[, distorted[, alpha]]) -> distorted
  ```
  """
  @spec distortPoints(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def distortPoints(undistorted, k, d) when (is_struct(undistorted, Evision.Mat) or is_struct(undistorted, Nx.Tensor) or is_number(undistorted) or is_tuple(undistorted)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d))
  do
    positional = [
      undistorted: Evision.Internal.Structurise.from_struct(undistorted),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d)
    ]
    :evision_nif.fisheye_distortPoints(positional)
    |> to_struct()
  end

  @doc """
  Estimates new camera intrinsic matrix for undistortion or rectification.

  ##### Positional Arguments
  - **k**: `Evision.Mat`.

    Camera intrinsic matrix \\f$cameramatrix{K}\\f$.

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$\\distcoeffsfisheye\\f$.

  - **image_size**: `Size`.

    Size of the image

  - **r**: `Evision.Mat`.

    Rectification transformation in the object space: 3x3 1-channel, or vector: 3x1/1x3
    1-channel or 1x1 3-channel

  ##### Keyword Arguments
  - **balance**: `double`.

    Sets the new focal length in range between the min focal length and the max focal
    length. Balance is in range of [0, 1].

  - **new_size**: `Size`.

    the new size

  - **fov_scale**: `double`.

    Divisor for new focal length.

  ##### Return
  - **p**: `Evision.Mat.t()`.

    New camera intrinsic matrix (3x3) or new projection matrix (3x4)

  Python prototype (for reference only):
  ```python3
  estimateNewCameraMatrixForUndistortRectify(K, D, image_size, R[, P[, balance[, new_size[, fov_scale]]]]) -> P
  ```
  """
  @spec estimateNewCameraMatrixForUndistortRectify(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, Evision.Mat.maybe_mat_in(), [{:balance, term()} | {:fov_scale, term()} | {:new_size, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def estimateNewCameraMatrixForUndistortRectify(k, d, image_size, r, opts) when (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and is_tuple(image_size) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:balance, :fov_scale, :new_size])
    positional = [
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d),
      image_size: Evision.Internal.Structurise.from_struct(image_size),
      r: Evision.Internal.Structurise.from_struct(r)
    ]
    :evision_nif.fisheye_estimateNewCameraMatrixForUndistortRectify(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Estimates new camera intrinsic matrix for undistortion or rectification.

  ##### Positional Arguments
  - **k**: `Evision.Mat`.

    Camera intrinsic matrix \\f$cameramatrix{K}\\f$.

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$\\distcoeffsfisheye\\f$.

  - **image_size**: `Size`.

    Size of the image

  - **r**: `Evision.Mat`.

    Rectification transformation in the object space: 3x3 1-channel, or vector: 3x1/1x3
    1-channel or 1x1 3-channel

  ##### Keyword Arguments
  - **balance**: `double`.

    Sets the new focal length in range between the min focal length and the max focal
    length. Balance is in range of [0, 1].

  - **new_size**: `Size`.

    the new size

  - **fov_scale**: `double`.

    Divisor for new focal length.

  ##### Return
  - **p**: `Evision.Mat.t()`.

    New camera intrinsic matrix (3x3) or new projection matrix (3x4)

  Python prototype (for reference only):
  ```python3
  estimateNewCameraMatrixForUndistortRectify(K, D, image_size, R[, P[, balance[, new_size[, fov_scale]]]]) -> P
  ```
  """
  @spec estimateNewCameraMatrixForUndistortRectify(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def estimateNewCameraMatrixForUndistortRectify(k, d, image_size, r) when (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and is_tuple(image_size) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r))
  do
    positional = [
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d),
      image_size: Evision.Internal.Structurise.from_struct(image_size),
      r: Evision.Internal.Structurise.from_struct(r)
    ]
    :evision_nif.fisheye_estimateNewCameraMatrixForUndistortRectify(positional)
    |> to_struct()
  end

  @doc """
  Computes undistortion and rectification maps for image transform by #remap. If D is empty zero
  distortion is used, if R or P is empty identity matrixes are used.

  ##### Positional Arguments
  - **k**: `Evision.Mat`.

    Camera intrinsic matrix \\f$cameramatrix{K}\\f$.

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$\\distcoeffsfisheye\\f$.

  - **r**: `Evision.Mat`.

    Rectification transformation in the object space: 3x3 1-channel, or vector: 3x1/1x3
    1-channel or 1x1 3-channel

  - **p**: `Evision.Mat`.

    New camera intrinsic matrix (3x3) or new projection matrix (3x4)

  - **size**: `Size`.

    Undistorted image size.

  - **m1type**: `integer()`.

    Type of the first output map that can be CV_32FC1 or CV_16SC2 . See #convertMaps
    for details.

  ##### Return
  - **map1**: `Evision.Mat.t()`.

    The first output map.

  - **map2**: `Evision.Mat.t()`.

    The second output map.

  Python prototype (for reference only):
  ```python3
  initUndistortRectifyMap(K, D, R, P, size, m1type[, map1[, map2]]) -> map1, map2
  ```
  """
  @spec initUndistortRectifyMap(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, integer(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def initUndistortRectifyMap(k, d, r, p, size, m1type, opts) when (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and (is_struct(p, Evision.Mat) or is_struct(p, Nx.Tensor) or is_number(p) or is_tuple(p)) and is_tuple(size) and is_integer(m1type) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d),
      r: Evision.Internal.Structurise.from_struct(r),
      p: Evision.Internal.Structurise.from_struct(p),
      size: Evision.Internal.Structurise.from_struct(size),
      m1type: Evision.Internal.Structurise.from_struct(m1type)
    ]
    :evision_nif.fisheye_initUndistortRectifyMap(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes undistortion and rectification maps for image transform by #remap. If D is empty zero
  distortion is used, if R or P is empty identity matrixes are used.

  ##### Positional Arguments
  - **k**: `Evision.Mat`.

    Camera intrinsic matrix \\f$cameramatrix{K}\\f$.

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$\\distcoeffsfisheye\\f$.

  - **r**: `Evision.Mat`.

    Rectification transformation in the object space: 3x3 1-channel, or vector: 3x1/1x3
    1-channel or 1x1 3-channel

  - **p**: `Evision.Mat`.

    New camera intrinsic matrix (3x3) or new projection matrix (3x4)

  - **size**: `Size`.

    Undistorted image size.

  - **m1type**: `integer()`.

    Type of the first output map that can be CV_32FC1 or CV_16SC2 . See #convertMaps
    for details.

  ##### Return
  - **map1**: `Evision.Mat.t()`.

    The first output map.

  - **map2**: `Evision.Mat.t()`.

    The second output map.

  Python prototype (for reference only):
  ```python3
  initUndistortRectifyMap(K, D, R, P, size, m1type[, map1[, map2]]) -> map1, map2
  ```
  """
  @spec initUndistortRectifyMap(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, integer()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def initUndistortRectifyMap(k, d, r, p, size, m1type) when (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and (is_struct(p, Evision.Mat) or is_struct(p, Nx.Tensor) or is_number(p) or is_tuple(p)) and is_tuple(size) and is_integer(m1type)
  do
    positional = [
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d),
      r: Evision.Internal.Structurise.from_struct(r),
      p: Evision.Internal.Structurise.from_struct(p),
      size: Evision.Internal.Structurise.from_struct(size),
      m1type: Evision.Internal.Structurise.from_struct(m1type)
    ]
    :evision_nif.fisheye_initUndistortRectifyMap(positional)
    |> to_struct()
  end

  @doc """
  projectPoints

  ##### Positional Arguments
  - **objectPoints**: `Evision.Mat`
  - **rvec**: `Evision.Mat`
  - **tvec**: `Evision.Mat`
  - **k**: `Evision.Mat`
  - **d**: `Evision.Mat`

  ##### Keyword Arguments
  - **alpha**: `double`.

  ##### Return
  - **imagePoints**: `Evision.Mat.t()`.
  - **jacobian**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  projectPoints(objectPoints, rvec, tvec, K, D[, imagePoints[, alpha[, jacobian]]]) -> imagePoints, jacobian
  ```
  """
  @spec projectPoints(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:alpha, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def projectPoints(objectPoints, rvec, tvec, k, d, opts) when (is_struct(objectPoints, Evision.Mat) or is_struct(objectPoints, Nx.Tensor) or is_number(objectPoints) or is_tuple(objectPoints)) and (is_struct(rvec, Evision.Mat) or is_struct(rvec, Nx.Tensor) or is_number(rvec) or is_tuple(rvec)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:alpha])
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      rvec: Evision.Internal.Structurise.from_struct(rvec),
      tvec: Evision.Internal.Structurise.from_struct(tvec),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d)
    ]
    :evision_nif.fisheye_projectPoints(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  projectPoints

  ##### Positional Arguments
  - **objectPoints**: `Evision.Mat`
  - **rvec**: `Evision.Mat`
  - **tvec**: `Evision.Mat`
  - **k**: `Evision.Mat`
  - **d**: `Evision.Mat`

  ##### Keyword Arguments
  - **alpha**: `double`.

  ##### Return
  - **imagePoints**: `Evision.Mat.t()`.
  - **jacobian**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  projectPoints(objectPoints, rvec, tvec, K, D[, imagePoints[, alpha[, jacobian]]]) -> imagePoints, jacobian
  ```
  """
  @spec projectPoints(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def projectPoints(objectPoints, rvec, tvec, k, d) when (is_struct(objectPoints, Evision.Mat) or is_struct(objectPoints, Nx.Tensor) or is_number(objectPoints) or is_tuple(objectPoints)) and (is_struct(rvec, Evision.Mat) or is_struct(rvec, Nx.Tensor) or is_number(rvec) or is_tuple(rvec)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d))
  do
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      rvec: Evision.Internal.Structurise.from_struct(rvec),
      tvec: Evision.Internal.Structurise.from_struct(tvec),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d)
    ]
    :evision_nif.fisheye_projectPoints(positional)
    |> to_struct()
  end

  @doc """
  Finds an object pose from 3D-2D point correspondences for fisheye camera moodel.

  ##### Positional Arguments
  - **objectPoints**: `Evision.Mat`.

    Array of object points in the object coordinate space, Nx3 1-channel or
    1xN/Nx1 3-channel, where N is the number of points. vector\\<Point3d\\> can be also passed here.

  - **imagePoints**: `Evision.Mat`.

    Array of corresponding image points, Nx2 1-channel or 1xN/Nx1 2-channel,
    where N is the number of points. vector\\<Point2d\\> can be also passed here.

  - **cameraMatrix**: `Evision.Mat`.

    Input camera intrinsic matrix \\f$\\cameramatrix{A}\\f$ .

  - **distCoeffs**: `Evision.Mat`.

    Input vector of distortion coefficients (4x1/1x4).

  ##### Keyword Arguments
  - **useExtrinsicGuess**: `bool`.

    Parameter used for #SOLVEPNP_ITERATIVE. If true (1), the function uses
    the provided rvec and tvec values as initial approximations of the rotation and translation
    vectors, respectively, and further optimizes them.

  - **flags**: `integer()`.

    Method for solving a PnP problem: see @ref calib3d_solvePnP_flags
    This function returns the rotation and the translation vectors that transform a 3D point expressed in the object
    coordinate frame to the camera coordinate frame, using different methods:
    - P3P methods (@ref SOLVEPNP_P3P, @ref SOLVEPNP_AP3P): need 4 input points to return a unique solution.
    - @ref SOLVEPNP_IPPE Input points must be >= 4 and object points must be coplanar.
    - @ref SOLVEPNP_IPPE_SQUARE Special case suitable for marker pose estimation.
      Number of input points must be 4. Object points must be defined in the following order:
    - point 0: [-squareLength / 2,  squareLength / 2, 0]
    - point 1: [ squareLength / 2,  squareLength / 2, 0]
    - point 2: [ squareLength / 2, -squareLength / 2, 0]
    - point 3: [-squareLength / 2, -squareLength / 2, 0]
    - for all the other flags, number of input points must be >= 4 and object points can be in any configuration.

  - **criteria**: `TermCriteria`.

    Termination criteria for internal undistortPoints call.
    The function interally undistorts points with @ref undistortPoints and call @ref cv::solvePnP,
    thus the input are very similar. Check there and Perspective-n-Points is described in @ref calib3d_solvePnP
    for more information.

  ##### Return
  - **retval**: `bool`
  - **rvec**: `Evision.Mat.t()`.

    Output rotation vector (see @ref Rodrigues ) that, together with tvec, brings points from
    the model coordinate system to the camera coordinate system.

  - **tvec**: `Evision.Mat.t()`.

    Output translation vector.

  Python prototype (for reference only):
  ```python3
  solvePnP(objectPoints, imagePoints, cameraMatrix, distCoeffs[, rvec[, tvec[, useExtrinsicGuess[, flags[, criteria]]]]]) -> retval, rvec, tvec
  ```
  """
  @spec solvePnP(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:criteria, term()} | {:flags, term()} | {:useExtrinsicGuess, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | false | {:error, String.t()}
  def solvePnP(objectPoints, imagePoints, cameraMatrix, distCoeffs, opts) when (is_struct(objectPoints, Evision.Mat) or is_struct(objectPoints, Nx.Tensor) or is_number(objectPoints) or is_tuple(objectPoints)) and (is_struct(imagePoints, Evision.Mat) or is_struct(imagePoints, Nx.Tensor) or is_number(imagePoints) or is_tuple(imagePoints)) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :flags, :useExtrinsicGuess])
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      imagePoints: Evision.Internal.Structurise.from_struct(imagePoints),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs)
    ]
    :evision_nif.fisheye_solvePnP(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Finds an object pose from 3D-2D point correspondences for fisheye camera moodel.

  ##### Positional Arguments
  - **objectPoints**: `Evision.Mat`.

    Array of object points in the object coordinate space, Nx3 1-channel or
    1xN/Nx1 3-channel, where N is the number of points. vector\\<Point3d\\> can be also passed here.

  - **imagePoints**: `Evision.Mat`.

    Array of corresponding image points, Nx2 1-channel or 1xN/Nx1 2-channel,
    where N is the number of points. vector\\<Point2d\\> can be also passed here.

  - **cameraMatrix**: `Evision.Mat`.

    Input camera intrinsic matrix \\f$\\cameramatrix{A}\\f$ .

  - **distCoeffs**: `Evision.Mat`.

    Input vector of distortion coefficients (4x1/1x4).

  ##### Keyword Arguments
  - **useExtrinsicGuess**: `bool`.

    Parameter used for #SOLVEPNP_ITERATIVE. If true (1), the function uses
    the provided rvec and tvec values as initial approximations of the rotation and translation
    vectors, respectively, and further optimizes them.

  - **flags**: `integer()`.

    Method for solving a PnP problem: see @ref calib3d_solvePnP_flags
    This function returns the rotation and the translation vectors that transform a 3D point expressed in the object
    coordinate frame to the camera coordinate frame, using different methods:
    - P3P methods (@ref SOLVEPNP_P3P, @ref SOLVEPNP_AP3P): need 4 input points to return a unique solution.
    - @ref SOLVEPNP_IPPE Input points must be >= 4 and object points must be coplanar.
    - @ref SOLVEPNP_IPPE_SQUARE Special case suitable for marker pose estimation.
      Number of input points must be 4. Object points must be defined in the following order:
    - point 0: [-squareLength / 2,  squareLength / 2, 0]
    - point 1: [ squareLength / 2,  squareLength / 2, 0]
    - point 2: [ squareLength / 2, -squareLength / 2, 0]
    - point 3: [-squareLength / 2, -squareLength / 2, 0]
    - for all the other flags, number of input points must be >= 4 and object points can be in any configuration.

  - **criteria**: `TermCriteria`.

    Termination criteria for internal undistortPoints call.
    The function interally undistorts points with @ref undistortPoints and call @ref cv::solvePnP,
    thus the input are very similar. Check there and Perspective-n-Points is described in @ref calib3d_solvePnP
    for more information.

  ##### Return
  - **retval**: `bool`
  - **rvec**: `Evision.Mat.t()`.

    Output rotation vector (see @ref Rodrigues ) that, together with tvec, brings points from
    the model coordinate system to the camera coordinate system.

  - **tvec**: `Evision.Mat.t()`.

    Output translation vector.

  Python prototype (for reference only):
  ```python3
  solvePnP(objectPoints, imagePoints, cameraMatrix, distCoeffs[, rvec[, tvec[, useExtrinsicGuess[, flags[, criteria]]]]]) -> retval, rvec, tvec
  ```
  """
  @spec solvePnP(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | false | {:error, String.t()}
  def solvePnP(objectPoints, imagePoints, cameraMatrix, distCoeffs) when (is_struct(objectPoints, Evision.Mat) or is_struct(objectPoints, Nx.Tensor) or is_number(objectPoints) or is_tuple(objectPoints)) and (is_struct(imagePoints, Evision.Mat) or is_struct(imagePoints, Nx.Tensor) or is_number(imagePoints) or is_tuple(imagePoints)) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeffs, Evision.Mat) or is_struct(distCoeffs, Nx.Tensor) or is_number(distCoeffs) or is_tuple(distCoeffs))
  do
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      imagePoints: Evision.Internal.Structurise.from_struct(imagePoints),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeffs: Evision.Internal.Structurise.from_struct(distCoeffs)
    ]
    :evision_nif.fisheye_solvePnP(positional)
    |> to_struct()
  end

  @doc """
  stereoCalibrate

  ##### Positional Arguments
  - **objectPoints**: `[Evision.Mat]`
  - **imagePoints1**: `[Evision.Mat]`
  - **imagePoints2**: `[Evision.Mat]`
  - **imageSize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **criteria**: `TermCriteria`.

  ##### Return
  - **retval**: `double`
  - **k1**: `Evision.Mat.t()`
  - **d1**: `Evision.Mat.t()`
  - **k2**: `Evision.Mat.t()`
  - **d2**: `Evision.Mat.t()`
  - **r**: `Evision.Mat.t()`.
  - **t**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  stereoCalibrate(objectPoints, imagePoints1, imagePoints2, K1, D1, K2, D2, imageSize[, R[, T[, flags[, criteria]]]]) -> retval, K1, D1, K2, D2, R, T
  ```
  """
  @spec stereoCalibrate(list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, [{:criteria, term()} | {:flags, term()}] | nil) :: {number(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def stereoCalibrate(objectPoints, imagePoints1, imagePoints2, k1, d1, k2, d2, imageSize, opts) when is_list(objectPoints) and is_list(imagePoints1) and is_list(imagePoints2) and (is_struct(k1, Evision.Mat) or is_struct(k1, Nx.Tensor) or is_number(k1) or is_tuple(k1)) and (is_struct(d1, Evision.Mat) or is_struct(d1, Nx.Tensor) or is_number(d1) or is_tuple(d1)) and (is_struct(k2, Evision.Mat) or is_struct(k2, Nx.Tensor) or is_number(k2) or is_tuple(k2)) and (is_struct(d2, Evision.Mat) or is_struct(d2, Nx.Tensor) or is_number(d2) or is_tuple(d2)) and is_tuple(imageSize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :flags])
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      imagePoints1: Evision.Internal.Structurise.from_struct(imagePoints1),
      imagePoints2: Evision.Internal.Structurise.from_struct(imagePoints2),
      k1: Evision.Internal.Structurise.from_struct(k1),
      d1: Evision.Internal.Structurise.from_struct(d1),
      k2: Evision.Internal.Structurise.from_struct(k2),
      d2: Evision.Internal.Structurise.from_struct(d2),
      imageSize: Evision.Internal.Structurise.from_struct(imageSize)
    ]
    :evision_nif.fisheye_stereoCalibrate(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  stereoCalibrate

  ##### Positional Arguments
  - **objectPoints**: `[Evision.Mat]`
  - **imagePoints1**: `[Evision.Mat]`
  - **imagePoints2**: `[Evision.Mat]`
  - **imageSize**: `Size`

  ##### Keyword Arguments
  - **flags**: `integer()`.
  - **criteria**: `TermCriteria`.

  ##### Return
  - **retval**: `double`
  - **k1**: `Evision.Mat.t()`
  - **d1**: `Evision.Mat.t()`
  - **k2**: `Evision.Mat.t()`
  - **d2**: `Evision.Mat.t()`
  - **r**: `Evision.Mat.t()`.
  - **t**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  stereoCalibrate(objectPoints, imagePoints1, imagePoints2, K1, D1, K2, D2, imageSize[, R[, T[, flags[, criteria]]]]) -> retval, K1, D1, K2, D2, R, T
  ```
  """
  @spec stereoCalibrate(list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}) :: {number(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def stereoCalibrate(objectPoints, imagePoints1, imagePoints2, k1, d1, k2, d2, imageSize) when is_list(objectPoints) and is_list(imagePoints1) and is_list(imagePoints2) and (is_struct(k1, Evision.Mat) or is_struct(k1, Nx.Tensor) or is_number(k1) or is_tuple(k1)) and (is_struct(d1, Evision.Mat) or is_struct(d1, Nx.Tensor) or is_number(d1) or is_tuple(d1)) and (is_struct(k2, Evision.Mat) or is_struct(k2, Nx.Tensor) or is_number(k2) or is_tuple(k2)) and (is_struct(d2, Evision.Mat) or is_struct(d2, Nx.Tensor) or is_number(d2) or is_tuple(d2)) and is_tuple(imageSize)
  do
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      imagePoints1: Evision.Internal.Structurise.from_struct(imagePoints1),
      imagePoints2: Evision.Internal.Structurise.from_struct(imagePoints2),
      k1: Evision.Internal.Structurise.from_struct(k1),
      d1: Evision.Internal.Structurise.from_struct(d1),
      k2: Evision.Internal.Structurise.from_struct(k2),
      d2: Evision.Internal.Structurise.from_struct(d2),
      imageSize: Evision.Internal.Structurise.from_struct(imageSize)
    ]
    :evision_nif.fisheye_stereoCalibrate(positional)
    |> to_struct()
  end

  @doc """
  Stereo rectification for fisheye camera model

  ##### Positional Arguments
  - **k1**: `Evision.Mat`.

    First camera intrinsic matrix.

  - **d1**: `Evision.Mat`.

    First camera distortion parameters.

  - **k2**: `Evision.Mat`.

    Second camera intrinsic matrix.

  - **d2**: `Evision.Mat`.

    Second camera distortion parameters.

  - **imageSize**: `Size`.

    Size of the image used for stereo calibration.

  - **r**: `Evision.Mat`.

    Rotation matrix between the coordinate systems of the first and the second
    cameras.

  - **tvec**: `Evision.Mat`.

    Translation vector between coordinate systems of the cameras.

  - **flags**: `integer()`.

    Operation flags that may be zero or @ref fisheye::CALIB_ZERO_DISPARITY . If the flag is set,
    the function makes the principal points of each camera have the same pixel coordinates in the
    rectified views. And if the flag is not set, the function may still shift the images in the
    horizontal or vertical direction (depending on the orientation of epipolar lines) to maximize the
    useful image area.

  ##### Keyword Arguments
  - **newImageSize**: `Size`.

    New image resolution after rectification. The same size should be passed to
    #initUndistortRectifyMap (see the stereo_calib.cpp sample in OpenCV samples directory). When (0,0)
    is passed (default), it is set to the original imageSize . Setting it to larger value can help you
    preserve details in the original image, especially when there is a big radial distortion.

  - **balance**: `double`.

    Sets the new focal length in range between the min focal length and the max focal
    length. Balance is in range of [0, 1].

  - **fov_scale**: `double`.

    Divisor for new focal length.

  ##### Return
  - **r1**: `Evision.Mat.t()`.

    Output 3x3 rectification transform (rotation matrix) for the first camera.

  - **r2**: `Evision.Mat.t()`.

    Output 3x3 rectification transform (rotation matrix) for the second camera.

  - **p1**: `Evision.Mat.t()`.

    Output 3x4 projection matrix in the new (rectified) coordinate systems for the first
    camera.

  - **p2**: `Evision.Mat.t()`.

    Output 3x4 projection matrix in the new (rectified) coordinate systems for the second
    camera.

  - **q**: `Evision.Mat.t()`.

    Output \\f$4 \\times 4\\f$ disparity-to-depth mapping matrix (see #reprojectImageTo3D ).

  Python prototype (for reference only):
  ```python3
  stereoRectify(K1, D1, K2, D2, imageSize, R, tvec, flags[, R1[, R2[, P1[, P2[, Q[, newImageSize[, balance[, fov_scale]]]]]]]]) -> R1, R2, P1, P2, Q
  ```
  """
  @spec stereoRectify(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{:balance, term()} | {:fov_scale, term()} | {:newImageSize, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def stereoRectify(k1, d1, k2, d2, imageSize, r, tvec, flags, opts) when (is_struct(k1, Evision.Mat) or is_struct(k1, Nx.Tensor) or is_number(k1) or is_tuple(k1)) and (is_struct(d1, Evision.Mat) or is_struct(d1, Nx.Tensor) or is_number(d1) or is_tuple(d1)) and (is_struct(k2, Evision.Mat) or is_struct(k2, Nx.Tensor) or is_number(k2) or is_tuple(k2)) and (is_struct(d2, Evision.Mat) or is_struct(d2, Nx.Tensor) or is_number(d2) or is_tuple(d2)) and is_tuple(imageSize) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec)) and is_integer(flags) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:balance, :fov_scale, :newImageSize])
    positional = [
      k1: Evision.Internal.Structurise.from_struct(k1),
      d1: Evision.Internal.Structurise.from_struct(d1),
      k2: Evision.Internal.Structurise.from_struct(k2),
      d2: Evision.Internal.Structurise.from_struct(d2),
      imageSize: Evision.Internal.Structurise.from_struct(imageSize),
      r: Evision.Internal.Structurise.from_struct(r),
      tvec: Evision.Internal.Structurise.from_struct(tvec),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.fisheye_stereoRectify(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Stereo rectification for fisheye camera model

  ##### Positional Arguments
  - **k1**: `Evision.Mat`.

    First camera intrinsic matrix.

  - **d1**: `Evision.Mat`.

    First camera distortion parameters.

  - **k2**: `Evision.Mat`.

    Second camera intrinsic matrix.

  - **d2**: `Evision.Mat`.

    Second camera distortion parameters.

  - **imageSize**: `Size`.

    Size of the image used for stereo calibration.

  - **r**: `Evision.Mat`.

    Rotation matrix between the coordinate systems of the first and the second
    cameras.

  - **tvec**: `Evision.Mat`.

    Translation vector between coordinate systems of the cameras.

  - **flags**: `integer()`.

    Operation flags that may be zero or @ref fisheye::CALIB_ZERO_DISPARITY . If the flag is set,
    the function makes the principal points of each camera have the same pixel coordinates in the
    rectified views. And if the flag is not set, the function may still shift the images in the
    horizontal or vertical direction (depending on the orientation of epipolar lines) to maximize the
    useful image area.

  ##### Keyword Arguments
  - **newImageSize**: `Size`.

    New image resolution after rectification. The same size should be passed to
    #initUndistortRectifyMap (see the stereo_calib.cpp sample in OpenCV samples directory). When (0,0)
    is passed (default), it is set to the original imageSize . Setting it to larger value can help you
    preserve details in the original image, especially when there is a big radial distortion.

  - **balance**: `double`.

    Sets the new focal length in range between the min focal length and the max focal
    length. Balance is in range of [0, 1].

  - **fov_scale**: `double`.

    Divisor for new focal length.

  ##### Return
  - **r1**: `Evision.Mat.t()`.

    Output 3x3 rectification transform (rotation matrix) for the first camera.

  - **r2**: `Evision.Mat.t()`.

    Output 3x3 rectification transform (rotation matrix) for the second camera.

  - **p1**: `Evision.Mat.t()`.

    Output 3x4 projection matrix in the new (rectified) coordinate systems for the first
    camera.

  - **p2**: `Evision.Mat.t()`.

    Output 3x4 projection matrix in the new (rectified) coordinate systems for the second
    camera.

  - **q**: `Evision.Mat.t()`.

    Output \\f$4 \\times 4\\f$ disparity-to-depth mapping matrix (see #reprojectImageTo3D ).

  Python prototype (for reference only):
  ```python3
  stereoRectify(K1, D1, K2, D2, imageSize, R, tvec, flags[, R1[, R2[, P1[, P2[, Q[, newImageSize[, balance[, fov_scale]]]]]]]]) -> R1, R2, P1, P2, Q
  ```
  """
  @spec stereoRectify(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def stereoRectify(k1, d1, k2, d2, imageSize, r, tvec, flags) when (is_struct(k1, Evision.Mat) or is_struct(k1, Nx.Tensor) or is_number(k1) or is_tuple(k1)) and (is_struct(d1, Evision.Mat) or is_struct(d1, Nx.Tensor) or is_number(d1) or is_tuple(d1)) and (is_struct(k2, Evision.Mat) or is_struct(k2, Nx.Tensor) or is_number(k2) or is_tuple(k2)) and (is_struct(d2, Evision.Mat) or is_struct(d2, Nx.Tensor) or is_number(d2) or is_tuple(d2)) and is_tuple(imageSize) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec)) and is_integer(flags)
  do
    positional = [
      k1: Evision.Internal.Structurise.from_struct(k1),
      d1: Evision.Internal.Structurise.from_struct(d1),
      k2: Evision.Internal.Structurise.from_struct(k2),
      d2: Evision.Internal.Structurise.from_struct(d2),
      imageSize: Evision.Internal.Structurise.from_struct(imageSize),
      r: Evision.Internal.Structurise.from_struct(r),
      tvec: Evision.Internal.Structurise.from_struct(tvec),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.fisheye_stereoRectify(positional)
    |> to_struct()
  end

  @doc """
  Transforms an image to compensate for fisheye lens distortion.

  ##### Positional Arguments
  - **distorted**: `Evision.Mat`.

    image with fisheye lens distortion.

  - **k**: `Evision.Mat`.

    Camera intrinsic matrix \\f$cameramatrix{K}\\f$.

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$\\distcoeffsfisheye\\f$.

  ##### Keyword Arguments
  - **knew**: `Evision.Mat`.

    Camera intrinsic matrix of the distorted image. By default, it is the identity matrix but you
    may additionally scale and shift the result by using a different matrix.

  - **new_size**: `Size`.

    the new size

  ##### Return
  - **undistorted**: `Evision.Mat.t()`.

    Output image with compensated fisheye lens distortion.

  The function transforms an image to compensate radial and tangential lens distortion.
  The function is simply a combination of #fisheye::initUndistortRectifyMap (with unity R ) and #remap
  (with bilinear interpolation). See the former function for details of the transformation being
  performed.
  See below the results of undistortImage.
  - a\\) result of undistort of perspective camera model (all possible coefficients (k_1, k_2, k_3,
    k_4, k_5, k_6) of distortion were optimized under calibration)

  - b\\) result of #fisheye::undistortImage of fisheye camera model (all possible coefficients (k_1, k_2,
    k_3, k_4) of fisheye distortion were optimized under calibration)

  - c\\) original image was captured with fisheye lens

  Pictures a) and b) almost the same. But if we consider points of image located far from the center
  of image, we can notice that on image a) these points are distorted.
  ![image](pics/fisheye_undistorted.jpg)

  Python prototype (for reference only):
  ```python3
  undistortImage(distorted, K, D[, undistorted[, Knew[, new_size]]]) -> undistorted
  ```
  """
  @spec undistortImage(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:knew, term()} | {:new_size, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def undistortImage(distorted, k, d, opts) when (is_struct(distorted, Evision.Mat) or is_struct(distorted, Nx.Tensor) or is_number(distorted) or is_tuple(distorted)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:knew, :new_size])
    positional = [
      distorted: Evision.Internal.Structurise.from_struct(distorted),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d)
    ]
    :evision_nif.fisheye_undistortImage(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Transforms an image to compensate for fisheye lens distortion.

  ##### Positional Arguments
  - **distorted**: `Evision.Mat`.

    image with fisheye lens distortion.

  - **k**: `Evision.Mat`.

    Camera intrinsic matrix \\f$cameramatrix{K}\\f$.

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$\\distcoeffsfisheye\\f$.

  ##### Keyword Arguments
  - **knew**: `Evision.Mat`.

    Camera intrinsic matrix of the distorted image. By default, it is the identity matrix but you
    may additionally scale and shift the result by using a different matrix.

  - **new_size**: `Size`.

    the new size

  ##### Return
  - **undistorted**: `Evision.Mat.t()`.

    Output image with compensated fisheye lens distortion.

  The function transforms an image to compensate radial and tangential lens distortion.
  The function is simply a combination of #fisheye::initUndistortRectifyMap (with unity R ) and #remap
  (with bilinear interpolation). See the former function for details of the transformation being
  performed.
  See below the results of undistortImage.
  - a\\) result of undistort of perspective camera model (all possible coefficients (k_1, k_2, k_3,
    k_4, k_5, k_6) of distortion were optimized under calibration)

  - b\\) result of #fisheye::undistortImage of fisheye camera model (all possible coefficients (k_1, k_2,
    k_3, k_4) of fisheye distortion were optimized under calibration)

  - c\\) original image was captured with fisheye lens

  Pictures a) and b) almost the same. But if we consider points of image located far from the center
  of image, we can notice that on image a) these points are distorted.
  ![image](pics/fisheye_undistorted.jpg)

  Python prototype (for reference only):
  ```python3
  undistortImage(distorted, K, D[, undistorted[, Knew[, new_size]]]) -> undistorted
  ```
  """
  @spec undistortImage(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def undistortImage(distorted, k, d) when (is_struct(distorted, Evision.Mat) or is_struct(distorted, Nx.Tensor) or is_number(distorted) or is_tuple(distorted)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d))
  do
    positional = [
      distorted: Evision.Internal.Structurise.from_struct(distorted),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d)
    ]
    :evision_nif.fisheye_undistortImage(positional)
    |> to_struct()
  end

  @doc """
  Undistorts 2D points using fisheye model

  ##### Positional Arguments
  - **distorted**: `Evision.Mat`.

    Array of object points, 1xN/Nx1 2-channel (or vector\\<Point2f\\> ), where N is the
    number of points in the view.

  - **k**: `Evision.Mat`.

    Camera intrinsic matrix \\f$cameramatrix{K}\\f$.

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$\\distcoeffsfisheye\\f$.

  ##### Keyword Arguments
  - **r**: `Evision.Mat`.

    Rectification transformation in the object space: 3x3 1-channel, or vector: 3x1/1x3
    1-channel or 1x1 3-channel

  - **p**: `Evision.Mat`.

    New camera intrinsic matrix (3x3) or new projection matrix (3x4)

  - **criteria**: `TermCriteria`.

    Termination criteria

  ##### Return
  - **undistorted**: `Evision.Mat.t()`.

    Output array of image points, 1xN/Nx1 2-channel, or vector\\<Point2f\\> .

  Python prototype (for reference only):
  ```python3
  undistortPoints(distorted, K, D[, undistorted[, R[, P[, criteria]]]]) -> undistorted
  ```
  """
  @spec undistortPoints(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:criteria, term()} | {:p, term()} | {:r, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def undistortPoints(distorted, k, d, opts) when (is_struct(distorted, Evision.Mat) or is_struct(distorted, Nx.Tensor) or is_number(distorted) or is_tuple(distorted)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:criteria, :p, :r])
    positional = [
      distorted: Evision.Internal.Structurise.from_struct(distorted),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d)
    ]
    :evision_nif.fisheye_undistortPoints(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Undistorts 2D points using fisheye model

  ##### Positional Arguments
  - **distorted**: `Evision.Mat`.

    Array of object points, 1xN/Nx1 2-channel (or vector\\<Point2f\\> ), where N is the
    number of points in the view.

  - **k**: `Evision.Mat`.

    Camera intrinsic matrix \\f$cameramatrix{K}\\f$.

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$\\distcoeffsfisheye\\f$.

  ##### Keyword Arguments
  - **r**: `Evision.Mat`.

    Rectification transformation in the object space: 3x3 1-channel, or vector: 3x1/1x3
    1-channel or 1x1 3-channel

  - **p**: `Evision.Mat`.

    New camera intrinsic matrix (3x3) or new projection matrix (3x4)

  - **criteria**: `TermCriteria`.

    Termination criteria

  ##### Return
  - **undistorted**: `Evision.Mat.t()`.

    Output array of image points, 1xN/Nx1 2-channel, or vector\\<Point2f\\> .

  Python prototype (for reference only):
  ```python3
  undistortPoints(distorted, K, D[, undistorted[, R[, P[, criteria]]]]) -> undistorted
  ```
  """
  @spec undistortPoints(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def undistortPoints(distorted, k, d) when (is_struct(distorted, Evision.Mat) or is_struct(distorted, Nx.Tensor) or is_number(distorted) or is_tuple(distorted)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d))
  do
    positional = [
      distorted: Evision.Internal.Structurise.from_struct(distorted),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d)
    ]
    :evision_nif.fisheye_undistortPoints(positional)
    |> to_struct()
  end
end
