defmodule Evision.Omnidir do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Omnidir` struct.

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
  def to_struct({:ok, %{class: Evision.Omnidir, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Omnidir, ref: ref}) do
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
  @type enum :: integer()
  @doc enum: true
  def cv_CALIB_USE_GUESS, do: 1
  @doc enum: true
  def cv_CALIB_FIX_SKEW, do: 2
  @doc enum: true
  def cv_CALIB_FIX_K1, do: 4
  @doc enum: true
  def cv_CALIB_FIX_K2, do: 8
  @doc enum: true
  def cv_CALIB_FIX_P1, do: 16
  @doc enum: true
  def cv_CALIB_FIX_P2, do: 32
  @doc enum: true
  def cv_CALIB_FIX_XI, do: 64
  @doc enum: true
  def cv_CALIB_FIX_GAMMA, do: 128
  @doc enum: true
  def cv_CALIB_FIX_CENTER, do: 256
  @doc enum: true
  def cv_RECTIFY_PERSPECTIVE, do: 1
  @doc enum: true
  def cv_RECTIFY_CYLINDRICAL, do: 2
  @doc enum: true
  def cv_RECTIFY_LONGLATI, do: 3
  @doc enum: true
  def cv_RECTIFY_STEREOGRAPHIC, do: 4
  @doc enum: true
  def cv_XYZRGB, do: 1
  @doc enum: true
  def cv_XYZ, do: 2


  @doc """
  Perform omnidirectional camera calibration, the default depth of outputs is CV_64F.

  ##### Positional Arguments
  - **objectPoints**: `[Evision.Mat]`.

    Vector of vector of Vec3f object points in world (pattern) coordinate.
    It also can be vector of Mat with size 1xN/Nx1 and type CV_32FC3. Data with depth of 64_F is also acceptable.

  - **imagePoints**: `[Evision.Mat]`.

    Vector of vector of Vec2f corresponding image points of objectPoints. It must be the same
    size and the same type with objectPoints.

  - **size**: `Size`.

    Image size of calibration images.

  - **flags**: `integer()`.

    The flags that control calibrate

  - **criteria**: `TermCriteria`.

    Termination criteria for optimization

  ##### Return
  - **retval**: `double`
  - **k**: `Evision.Mat.t()`.

    Output calibrated camera matrix.

  - **xi**: `Evision.Mat.t()`.

    Output parameter xi for CMei's model

  - **d**: `Evision.Mat.t()`.

    Output distortion parameters \\f$(k_1, k_2, p_1, p_2)\\f$

  - **rvecs**: `[Evision.Mat]`.

    Output rotations for each calibration images

  - **tvecs**: `[Evision.Mat]`.

    Output translation for each calibration images

  - **idx**: `Evision.Mat.t()`.

    Indices of images that pass initialization, which are really used in calibration. So the size of rvecs is the
    same as idx.total().

  Python prototype (for reference only):
  ```python3
  calibrate(objectPoints, imagePoints, size, K, xi, D, flags, criteria[, rvecs[, tvecs[, idx]]]) -> retval, K, xi, D, rvecs, tvecs, idx
  ```
  """
  @spec calibrate(list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), {integer(), integer(), number()}, [{atom(), term()},...] | nil) :: {number(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def calibrate(objectPoints, imagePoints, size, k, xi, d, flags, criteria, opts) when is_list(objectPoints) and is_list(imagePoints) and is_tuple(size) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(xi, Evision.Mat) or is_struct(xi, Nx.Tensor) or is_number(xi) or is_tuple(xi)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and is_integer(flags) and is_tuple(criteria) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      imagePoints: Evision.Internal.Structurise.from_struct(imagePoints),
      size: Evision.Internal.Structurise.from_struct(size),
      k: Evision.Internal.Structurise.from_struct(k),
      xi: Evision.Internal.Structurise.from_struct(xi),
      d: Evision.Internal.Structurise.from_struct(d),
      flags: Evision.Internal.Structurise.from_struct(flags),
      criteria: Evision.Internal.Structurise.from_struct(criteria)
    ]
    :evision_nif.omnidir_calibrate(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Perform omnidirectional camera calibration, the default depth of outputs is CV_64F.

  ##### Positional Arguments
  - **objectPoints**: `[Evision.Mat]`.

    Vector of vector of Vec3f object points in world (pattern) coordinate.
    It also can be vector of Mat with size 1xN/Nx1 and type CV_32FC3. Data with depth of 64_F is also acceptable.

  - **imagePoints**: `[Evision.Mat]`.

    Vector of vector of Vec2f corresponding image points of objectPoints. It must be the same
    size and the same type with objectPoints.

  - **size**: `Size`.

    Image size of calibration images.

  - **flags**: `integer()`.

    The flags that control calibrate

  - **criteria**: `TermCriteria`.

    Termination criteria for optimization

  ##### Return
  - **retval**: `double`
  - **k**: `Evision.Mat.t()`.

    Output calibrated camera matrix.

  - **xi**: `Evision.Mat.t()`.

    Output parameter xi for CMei's model

  - **d**: `Evision.Mat.t()`.

    Output distortion parameters \\f$(k_1, k_2, p_1, p_2)\\f$

  - **rvecs**: `[Evision.Mat]`.

    Output rotations for each calibration images

  - **tvecs**: `[Evision.Mat]`.

    Output translation for each calibration images

  - **idx**: `Evision.Mat.t()`.

    Indices of images that pass initialization, which are really used in calibration. So the size of rvecs is the
    same as idx.total().

  Python prototype (for reference only):
  ```python3
  calibrate(objectPoints, imagePoints, size, K, xi, D, flags, criteria[, rvecs[, tvecs[, idx]]]) -> retval, K, xi, D, rvecs, tvecs, idx
  ```
  """
  @spec calibrate(list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), {integer(), integer(), number()}) :: {number(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def calibrate(objectPoints, imagePoints, size, k, xi, d, flags, criteria) when is_list(objectPoints) and is_list(imagePoints) and is_tuple(size) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(xi, Evision.Mat) or is_struct(xi, Nx.Tensor) or is_number(xi) or is_tuple(xi)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and is_integer(flags) and is_tuple(criteria)
  do
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      imagePoints: Evision.Internal.Structurise.from_struct(imagePoints),
      size: Evision.Internal.Structurise.from_struct(size),
      k: Evision.Internal.Structurise.from_struct(k),
      xi: Evision.Internal.Structurise.from_struct(xi),
      d: Evision.Internal.Structurise.from_struct(d),
      flags: Evision.Internal.Structurise.from_struct(flags),
      criteria: Evision.Internal.Structurise.from_struct(criteria)
    ]
    :evision_nif.omnidir_calibrate(positional)
    |> to_struct()
  end

  @doc """
  Computes undistortion and rectification maps for omnidirectional camera image transform by a rotation R.
  It output two maps that are used for cv::remap(). If D is empty then zero distortion is used,
  if R or P is empty then identity matrices are used.

  ##### Positional Arguments
  - **k**: `Evision.Mat`.

    Camera matrix \\f$K = \\vecthreethree{f_x}{s}{c_x}{0}{f_y}{c_y}{0}{0}{_1}\\f$, with depth CV_32F or CV_64F

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$(k_1, k_2, p_1, p_2)\\f$, with depth CV_32F or CV_64F

  - **xi**: `Evision.Mat`.

    The parameter xi for CMei's model

  - **r**: `Evision.Mat`.

    Rotation transform between the original and object space : 3x3 1-channel, or vector: 3x1/1x3, with depth CV_32F or CV_64F

  - **p**: `Evision.Mat`.

    New camera matrix (3x3) or new projection matrix (3x4)

  - **size**: `Size`.

    Undistorted image size.

  - **m1type**: `integer()`.

    Type of the first output map that can be CV_32FC1 or CV_16SC2 . See convertMaps()
    for details.

  - **flags**: `integer()`.

    Flags indicates the rectification type,  RECTIFY_PERSPECTIVE, RECTIFY_CYLINDRICAL, RECTIFY_LONGLATI and RECTIFY_STEREOGRAPHIC
    are supported.

  ##### Return
  - **map1**: `Evision.Mat.t()`.

    The first output map.

  - **map2**: `Evision.Mat.t()`.

    The second output map.

  Python prototype (for reference only):
  ```python3
  initUndistortRectifyMap(K, D, xi, R, P, size, m1type, flags[, map1[, map2]]) -> map1, map2
  ```
  """
  @spec initUndistortRectifyMap(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, integer(), integer(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def initUndistortRectifyMap(k, d, xi, r, p, size, m1type, flags, opts) when (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (is_struct(xi, Evision.Mat) or is_struct(xi, Nx.Tensor) or is_number(xi) or is_tuple(xi)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and (is_struct(p, Evision.Mat) or is_struct(p, Nx.Tensor) or is_number(p) or is_tuple(p)) and is_tuple(size) and is_integer(m1type) and is_integer(flags) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d),
      xi: Evision.Internal.Structurise.from_struct(xi),
      r: Evision.Internal.Structurise.from_struct(r),
      p: Evision.Internal.Structurise.from_struct(p),
      size: Evision.Internal.Structurise.from_struct(size),
      m1type: Evision.Internal.Structurise.from_struct(m1type),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.omnidir_initUndistortRectifyMap(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes undistortion and rectification maps for omnidirectional camera image transform by a rotation R.
  It output two maps that are used for cv::remap(). If D is empty then zero distortion is used,
  if R or P is empty then identity matrices are used.

  ##### Positional Arguments
  - **k**: `Evision.Mat`.

    Camera matrix \\f$K = \\vecthreethree{f_x}{s}{c_x}{0}{f_y}{c_y}{0}{0}{_1}\\f$, with depth CV_32F or CV_64F

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$(k_1, k_2, p_1, p_2)\\f$, with depth CV_32F or CV_64F

  - **xi**: `Evision.Mat`.

    The parameter xi for CMei's model

  - **r**: `Evision.Mat`.

    Rotation transform between the original and object space : 3x3 1-channel, or vector: 3x1/1x3, with depth CV_32F or CV_64F

  - **p**: `Evision.Mat`.

    New camera matrix (3x3) or new projection matrix (3x4)

  - **size**: `Size`.

    Undistorted image size.

  - **m1type**: `integer()`.

    Type of the first output map that can be CV_32FC1 or CV_16SC2 . See convertMaps()
    for details.

  - **flags**: `integer()`.

    Flags indicates the rectification type,  RECTIFY_PERSPECTIVE, RECTIFY_CYLINDRICAL, RECTIFY_LONGLATI and RECTIFY_STEREOGRAPHIC
    are supported.

  ##### Return
  - **map1**: `Evision.Mat.t()`.

    The first output map.

  - **map2**: `Evision.Mat.t()`.

    The second output map.

  Python prototype (for reference only):
  ```python3
  initUndistortRectifyMap(K, D, xi, R, P, size, m1type, flags[, map1[, map2]]) -> map1, map2
  ```
  """
  @spec initUndistortRectifyMap(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, integer(), integer()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def initUndistortRectifyMap(k, d, xi, r, p, size, m1type, flags) when (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (is_struct(xi, Evision.Mat) or is_struct(xi, Nx.Tensor) or is_number(xi) or is_tuple(xi)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and (is_struct(p, Evision.Mat) or is_struct(p, Nx.Tensor) or is_number(p) or is_tuple(p)) and is_tuple(size) and is_integer(m1type) and is_integer(flags)
  do
    positional = [
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d),
      xi: Evision.Internal.Structurise.from_struct(xi),
      r: Evision.Internal.Structurise.from_struct(r),
      p: Evision.Internal.Structurise.from_struct(p),
      size: Evision.Internal.Structurise.from_struct(size),
      m1type: Evision.Internal.Structurise.from_struct(m1type),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.omnidir_initUndistortRectifyMap(positional)
    |> to_struct()
  end

  @doc """
  Projects points for omnidirectional camera using CMei's model

  ##### Positional Arguments
  - **objectPoints**: `Evision.Mat`.

    Object points in world coordinate, vector of vector of Vec3f or Mat of
    1xN/Nx1 3-channel of type CV_32F and N is the number of points. 64F is also acceptable.

  - **rvec**: `Evision.Mat`.

    vector of rotation between world coordinate and camera coordinate, i.e., om

  - **tvec**: `Evision.Mat`.

    vector of translation between pattern coordinate and camera coordinate

  - **k**: `Evision.Mat`.

    Camera matrix \\f$K = \\vecthreethree{f_x}{s}{c_x}{0}{f_y}{c_y}{0}{0}{_1}\\f$.

  - **xi**: `double`.

    The parameter xi for CMei's model

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$(k_1, k_2, p_1, p_2)\\f$.

  ##### Return
  - **imagePoints**: `Evision.Mat.t()`.

    Output array of image points, vector of vector of Vec2f or
    1xN/Nx1 2-channel of type CV_32F. 64F is also acceptable.

  - **jacobian**: `Evision.Mat.t()`.

    Optional output 2Nx16 of type CV_64F jacobian matrix, contains the derivatives of
    image pixel points wrt parameters including \\f$om, T, f_x, f_y, s, c_x, c_y, xi, k_1, k_2, p_1, p_2\\f$.
    This matrix will be used in calibration by optimization.

  The function projects object 3D points of world coordinate to image pixels, parameter by intrinsic
  and extrinsic parameters. Also, it optionally compute a by-product: the jacobian matrix containing
  contains the derivatives of image pixel points wrt intrinsic and extrinsic parameters.

  Python prototype (for reference only):
  ```python3
  projectPoints(objectPoints, rvec, tvec, K, xi, D[, imagePoints[, jacobian]]) -> imagePoints, jacobian
  ```
  """
  @spec projectPoints(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def projectPoints(objectPoints, rvec, tvec, k, xi, d, opts) when (is_struct(objectPoints, Evision.Mat) or is_struct(objectPoints, Nx.Tensor) or is_number(objectPoints) or is_tuple(objectPoints)) and (is_struct(rvec, Evision.Mat) or is_struct(rvec, Nx.Tensor) or is_number(rvec) or is_tuple(rvec)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and is_number(xi) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      rvec: Evision.Internal.Structurise.from_struct(rvec),
      tvec: Evision.Internal.Structurise.from_struct(tvec),
      k: Evision.Internal.Structurise.from_struct(k),
      xi: Evision.Internal.Structurise.from_struct(xi),
      d: Evision.Internal.Structurise.from_struct(d)
    ]
    :evision_nif.omnidir_projectPoints(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Projects points for omnidirectional camera using CMei's model

  ##### Positional Arguments
  - **objectPoints**: `Evision.Mat`.

    Object points in world coordinate, vector of vector of Vec3f or Mat of
    1xN/Nx1 3-channel of type CV_32F and N is the number of points. 64F is also acceptable.

  - **rvec**: `Evision.Mat`.

    vector of rotation between world coordinate and camera coordinate, i.e., om

  - **tvec**: `Evision.Mat`.

    vector of translation between pattern coordinate and camera coordinate

  - **k**: `Evision.Mat`.

    Camera matrix \\f$K = \\vecthreethree{f_x}{s}{c_x}{0}{f_y}{c_y}{0}{0}{_1}\\f$.

  - **xi**: `double`.

    The parameter xi for CMei's model

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$(k_1, k_2, p_1, p_2)\\f$.

  ##### Return
  - **imagePoints**: `Evision.Mat.t()`.

    Output array of image points, vector of vector of Vec2f or
    1xN/Nx1 2-channel of type CV_32F. 64F is also acceptable.

  - **jacobian**: `Evision.Mat.t()`.

    Optional output 2Nx16 of type CV_64F jacobian matrix, contains the derivatives of
    image pixel points wrt parameters including \\f$om, T, f_x, f_y, s, c_x, c_y, xi, k_1, k_2, p_1, p_2\\f$.
    This matrix will be used in calibration by optimization.

  The function projects object 3D points of world coordinate to image pixels, parameter by intrinsic
  and extrinsic parameters. Also, it optionally compute a by-product: the jacobian matrix containing
  contains the derivatives of image pixel points wrt intrinsic and extrinsic parameters.

  Python prototype (for reference only):
  ```python3
  projectPoints(objectPoints, rvec, tvec, K, xi, D[, imagePoints[, jacobian]]) -> imagePoints, jacobian
  ```
  """
  @spec projectPoints(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def projectPoints(objectPoints, rvec, tvec, k, xi, d) when (is_struct(objectPoints, Evision.Mat) or is_struct(objectPoints, Nx.Tensor) or is_number(objectPoints) or is_tuple(objectPoints)) and (is_struct(rvec, Evision.Mat) or is_struct(rvec, Nx.Tensor) or is_number(rvec) or is_tuple(rvec)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and is_number(xi) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d))
  do
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      rvec: Evision.Internal.Structurise.from_struct(rvec),
      tvec: Evision.Internal.Structurise.from_struct(tvec),
      k: Evision.Internal.Structurise.from_struct(k),
      xi: Evision.Internal.Structurise.from_struct(xi),
      d: Evision.Internal.Structurise.from_struct(d)
    ]
    :evision_nif.omnidir_projectPoints(positional)
    |> to_struct()
  end

  @doc """
  Stereo calibration for omnidirectional camera model. It computes the intrinsic parameters for two
  cameras and the extrinsic parameters between two cameras. The default depth of outputs is CV_64F.

  ##### Positional Arguments
  - **imageSize1**: `Size`.

    Image size of calibration images of the first camera.

  - **imageSize2**: `Size`.

    Image size of calibration images of the second camera.

  - **flags**: `integer()`.

    The flags that control stereoCalibrate

  - **criteria**: `TermCriteria`.

    Termination criteria for optimization

  ##### Return
  - **retval**: `double`
  - **objectPoints**: `[Evision.Mat]`.

    Object points in world (pattern) coordinate. Its type is vector<vector<Vec3f> >.
    It also can be vector of Mat with size 1xN/Nx1 and type CV_32FC3. Data with depth of 64_F is also acceptable.

  - **imagePoints1**: `[Evision.Mat]`.

    The corresponding image points of the first camera, with type vector<vector<Vec2f> >.
    It must be the same size and the same type as objectPoints.

  - **imagePoints2**: `[Evision.Mat]`.

    The corresponding image points of the second camera, with type vector<vector<Vec2f> >.
    It must be the same size and the same type as objectPoints.

  - **k1**: `Evision.Mat.t()`.

    Output camera matrix for the first camera.

  - **xi1**: `Evision.Mat.t()`.

    Output parameter xi of Mei's model for the first camera

  - **d1**: `Evision.Mat.t()`.

    Output distortion parameters \\f$(k_1, k_2, p_1, p_2)\\f$ for the first camera

  - **k2**: `Evision.Mat.t()`.

    Output camera matrix for the first camera.

  - **xi2**: `Evision.Mat.t()`.

    Output parameter xi of CMei's model for the second camera

  - **d2**: `Evision.Mat.t()`.

    Output distortion parameters \\f$(k_1, k_2, p_1, p_2)\\f$ for the second camera

  - **rvec**: `Evision.Mat.t()`.

    Output rotation between the first and second camera

  - **tvec**: `Evision.Mat.t()`.

    Output translation between the first and second camera

  - **rvecsL**: `[Evision.Mat]`.

    Output rotation for each image of the first camera

  - **tvecsL**: `[Evision.Mat]`.

    Output translation for each image of the first camera

  - **idx**: `Evision.Mat.t()`.

    Indices of image pairs that pass initialization, which are really used in calibration. So the size of rvecs is the
    same as idx.total().

  @

  Python prototype (for reference only):
  ```python3
  stereoCalibrate(objectPoints, imagePoints1, imagePoints2, imageSize1, imageSize2, K1, xi1, D1, K2, xi2, D2, flags, criteria[, rvec[, tvec[, rvecsL[, tvecsL[, idx]]]]]) -> retval, objectPoints, imagePoints1, imagePoints2, K1, xi1, D1, K2, xi2, D2, rvec, tvec, rvecsL, tvecsL, idx
  ```
  """
  @spec stereoCalibrate(list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), {number(), number()}, {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), {integer(), integer(), number()}, [{atom(), term()},...] | nil) :: {number(), list(Evision.Mat.t()), list(Evision.Mat.t()), list(Evision.Mat.t()), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def stereoCalibrate(objectPoints, imagePoints1, imagePoints2, imageSize1, imageSize2, k1, xi1, d1, k2, xi2, d2, flags, criteria, opts) when is_list(objectPoints) and is_list(imagePoints1) and is_list(imagePoints2) and is_tuple(imageSize1) and is_tuple(imageSize2) and (is_struct(k1, Evision.Mat) or is_struct(k1, Nx.Tensor) or is_number(k1) or is_tuple(k1)) and (is_struct(xi1, Evision.Mat) or is_struct(xi1, Nx.Tensor) or is_number(xi1) or is_tuple(xi1)) and (is_struct(d1, Evision.Mat) or is_struct(d1, Nx.Tensor) or is_number(d1) or is_tuple(d1)) and (is_struct(k2, Evision.Mat) or is_struct(k2, Nx.Tensor) or is_number(k2) or is_tuple(k2)) and (is_struct(xi2, Evision.Mat) or is_struct(xi2, Nx.Tensor) or is_number(xi2) or is_tuple(xi2)) and (is_struct(d2, Evision.Mat) or is_struct(d2, Nx.Tensor) or is_number(d2) or is_tuple(d2)) and is_integer(flags) and is_tuple(criteria) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      imagePoints1: Evision.Internal.Structurise.from_struct(imagePoints1),
      imagePoints2: Evision.Internal.Structurise.from_struct(imagePoints2),
      imageSize1: Evision.Internal.Structurise.from_struct(imageSize1),
      imageSize2: Evision.Internal.Structurise.from_struct(imageSize2),
      k1: Evision.Internal.Structurise.from_struct(k1),
      xi1: Evision.Internal.Structurise.from_struct(xi1),
      d1: Evision.Internal.Structurise.from_struct(d1),
      k2: Evision.Internal.Structurise.from_struct(k2),
      xi2: Evision.Internal.Structurise.from_struct(xi2),
      d2: Evision.Internal.Structurise.from_struct(d2),
      flags: Evision.Internal.Structurise.from_struct(flags),
      criteria: Evision.Internal.Structurise.from_struct(criteria)
    ]
    :evision_nif.omnidir_stereoCalibrate(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Stereo calibration for omnidirectional camera model. It computes the intrinsic parameters for two
  cameras and the extrinsic parameters between two cameras. The default depth of outputs is CV_64F.

  ##### Positional Arguments
  - **imageSize1**: `Size`.

    Image size of calibration images of the first camera.

  - **imageSize2**: `Size`.

    Image size of calibration images of the second camera.

  - **flags**: `integer()`.

    The flags that control stereoCalibrate

  - **criteria**: `TermCriteria`.

    Termination criteria for optimization

  ##### Return
  - **retval**: `double`
  - **objectPoints**: `[Evision.Mat]`.

    Object points in world (pattern) coordinate. Its type is vector<vector<Vec3f> >.
    It also can be vector of Mat with size 1xN/Nx1 and type CV_32FC3. Data with depth of 64_F is also acceptable.

  - **imagePoints1**: `[Evision.Mat]`.

    The corresponding image points of the first camera, with type vector<vector<Vec2f> >.
    It must be the same size and the same type as objectPoints.

  - **imagePoints2**: `[Evision.Mat]`.

    The corresponding image points of the second camera, with type vector<vector<Vec2f> >.
    It must be the same size and the same type as objectPoints.

  - **k1**: `Evision.Mat.t()`.

    Output camera matrix for the first camera.

  - **xi1**: `Evision.Mat.t()`.

    Output parameter xi of Mei's model for the first camera

  - **d1**: `Evision.Mat.t()`.

    Output distortion parameters \\f$(k_1, k_2, p_1, p_2)\\f$ for the first camera

  - **k2**: `Evision.Mat.t()`.

    Output camera matrix for the first camera.

  - **xi2**: `Evision.Mat.t()`.

    Output parameter xi of CMei's model for the second camera

  - **d2**: `Evision.Mat.t()`.

    Output distortion parameters \\f$(k_1, k_2, p_1, p_2)\\f$ for the second camera

  - **rvec**: `Evision.Mat.t()`.

    Output rotation between the first and second camera

  - **tvec**: `Evision.Mat.t()`.

    Output translation between the first and second camera

  - **rvecsL**: `[Evision.Mat]`.

    Output rotation for each image of the first camera

  - **tvecsL**: `[Evision.Mat]`.

    Output translation for each image of the first camera

  - **idx**: `Evision.Mat.t()`.

    Indices of image pairs that pass initialization, which are really used in calibration. So the size of rvecs is the
    same as idx.total().

  @

  Python prototype (for reference only):
  ```python3
  stereoCalibrate(objectPoints, imagePoints1, imagePoints2, imageSize1, imageSize2, K1, xi1, D1, K2, xi2, D2, flags, criteria[, rvec[, tvec[, rvecsL[, tvecsL[, idx]]]]]) -> retval, objectPoints, imagePoints1, imagePoints2, K1, xi1, D1, K2, xi2, D2, rvec, tvec, rvecsL, tvecsL, idx
  ```
  """
  @spec stereoCalibrate(list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), {number(), number()}, {number(), number()}, Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), {integer(), integer(), number()}) :: {number(), list(Evision.Mat.t()), list(Evision.Mat.t()), list(Evision.Mat.t()), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), list(Evision.Mat.t()), list(Evision.Mat.t()), Evision.Mat.t()} | {:error, String.t()}
  def stereoCalibrate(objectPoints, imagePoints1, imagePoints2, imageSize1, imageSize2, k1, xi1, d1, k2, xi2, d2, flags, criteria) when is_list(objectPoints) and is_list(imagePoints1) and is_list(imagePoints2) and is_tuple(imageSize1) and is_tuple(imageSize2) and (is_struct(k1, Evision.Mat) or is_struct(k1, Nx.Tensor) or is_number(k1) or is_tuple(k1)) and (is_struct(xi1, Evision.Mat) or is_struct(xi1, Nx.Tensor) or is_number(xi1) or is_tuple(xi1)) and (is_struct(d1, Evision.Mat) or is_struct(d1, Nx.Tensor) or is_number(d1) or is_tuple(d1)) and (is_struct(k2, Evision.Mat) or is_struct(k2, Nx.Tensor) or is_number(k2) or is_tuple(k2)) and (is_struct(xi2, Evision.Mat) or is_struct(xi2, Nx.Tensor) or is_number(xi2) or is_tuple(xi2)) and (is_struct(d2, Evision.Mat) or is_struct(d2, Nx.Tensor) or is_number(d2) or is_tuple(d2)) and is_integer(flags) and is_tuple(criteria)
  do
    positional = [
      objectPoints: Evision.Internal.Structurise.from_struct(objectPoints),
      imagePoints1: Evision.Internal.Structurise.from_struct(imagePoints1),
      imagePoints2: Evision.Internal.Structurise.from_struct(imagePoints2),
      imageSize1: Evision.Internal.Structurise.from_struct(imageSize1),
      imageSize2: Evision.Internal.Structurise.from_struct(imageSize2),
      k1: Evision.Internal.Structurise.from_struct(k1),
      xi1: Evision.Internal.Structurise.from_struct(xi1),
      d1: Evision.Internal.Structurise.from_struct(d1),
      k2: Evision.Internal.Structurise.from_struct(k2),
      xi2: Evision.Internal.Structurise.from_struct(xi2),
      d2: Evision.Internal.Structurise.from_struct(d2),
      flags: Evision.Internal.Structurise.from_struct(flags),
      criteria: Evision.Internal.Structurise.from_struct(criteria)
    ]
    :evision_nif.omnidir_stereoCalibrate(positional)
    |> to_struct()
  end

  @doc """
  Stereo 3D reconstruction from a pair of images

  ##### Positional Arguments
  - **image1**: `Evision.Mat`.

    The first input image

  - **image2**: `Evision.Mat`.

    The second input image

  - **k1**: `Evision.Mat`.

    Input camera matrix of the first camera

  - **d1**: `Evision.Mat`.

    Input distortion parameters \\f$(k_1, k_2, p_1, p_2)\\f$ for the first camera

  - **xi1**: `Evision.Mat`.

    Input parameter xi for the first camera for CMei's model

  - **k2**: `Evision.Mat`.

    Input camera matrix of the second camera

  - **d2**: `Evision.Mat`.

    Input distortion parameters \\f$(k_1, k_2, p_1, p_2)\\f$ for the second camera

  - **xi2**: `Evision.Mat`.

    Input parameter xi for the second camera for CMei's model

  - **r**: `Evision.Mat`.

    Rotation between the first and second camera

  - **t**: `Evision.Mat`.

    Translation between the first and second camera

  - **flag**: `integer()`.

    Flag of rectification type, RECTIFY_PERSPECTIVE or RECTIFY_LONGLATI

  - **numDisparities**: `integer()`.

    The parameter 'numDisparities' in StereoSGBM, see StereoSGBM for details.

  - **sADWindowSize**: `integer()`.

    The parameter 'SADWindowSize' in StereoSGBM, see StereoSGBM for details.

  ##### Keyword Arguments
  - **newSize**: `Size`.

    Image size of rectified image, see omnidir::undistortImage

  - **knew**: `Evision.Mat`.

    New camera matrix of rectified image, see omnidir::undistortImage

  - **pointType**: `integer()`.

    Point cloud type, it can be XYZRGB or XYZ

  ##### Return
  - **disparity**: `Evision.Mat.t()`.

    Disparity map generated by stereo matching

  - **image1Rec**: `Evision.Mat.t()`.

    Rectified image of the first image

  - **image2Rec**: `Evision.Mat.t()`.

    rectified image of the second image

  - **pointCloud**: `Evision.Mat.t()`.

    Point cloud of 3D reconstruction, with type CV_64FC3

  Python prototype (for reference only):
  ```python3
  stereoReconstruct(image1, image2, K1, D1, xi1, K2, D2, xi2, R, T, flag, numDisparities, SADWindowSize[, disparity[, image1Rec[, image2Rec[, newSize[, Knew[, pointCloud[, pointType]]]]]]]) -> disparity, image1Rec, image2Rec, pointCloud
  ```
  """
  @spec stereoReconstruct(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), integer(), integer(), [{:knew, term()} | {:newSize, term()} | {:pointType, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def stereoReconstruct(image1, image2, k1, d1, xi1, k2, d2, xi2, r, t, flag, numDisparities, sADWindowSize, opts) when (is_struct(image1, Evision.Mat) or is_struct(image1, Nx.Tensor) or is_number(image1) or is_tuple(image1)) and (is_struct(image2, Evision.Mat) or is_struct(image2, Nx.Tensor) or is_number(image2) or is_tuple(image2)) and (is_struct(k1, Evision.Mat) or is_struct(k1, Nx.Tensor) or is_number(k1) or is_tuple(k1)) and (is_struct(d1, Evision.Mat) or is_struct(d1, Nx.Tensor) or is_number(d1) or is_tuple(d1)) and (is_struct(xi1, Evision.Mat) or is_struct(xi1, Nx.Tensor) or is_number(xi1) or is_tuple(xi1)) and (is_struct(k2, Evision.Mat) or is_struct(k2, Nx.Tensor) or is_number(k2) or is_tuple(k2)) and (is_struct(d2, Evision.Mat) or is_struct(d2, Nx.Tensor) or is_number(d2) or is_tuple(d2)) and (is_struct(xi2, Evision.Mat) or is_struct(xi2, Nx.Tensor) or is_number(xi2) or is_tuple(xi2)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and (is_struct(t, Evision.Mat) or is_struct(t, Nx.Tensor) or is_number(t) or is_tuple(t)) and is_integer(flag) and is_integer(numDisparities) and is_integer(sADWindowSize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:knew, :newSize, :pointType])
    positional = [
      image1: Evision.Internal.Structurise.from_struct(image1),
      image2: Evision.Internal.Structurise.from_struct(image2),
      k1: Evision.Internal.Structurise.from_struct(k1),
      d1: Evision.Internal.Structurise.from_struct(d1),
      xi1: Evision.Internal.Structurise.from_struct(xi1),
      k2: Evision.Internal.Structurise.from_struct(k2),
      d2: Evision.Internal.Structurise.from_struct(d2),
      xi2: Evision.Internal.Structurise.from_struct(xi2),
      r: Evision.Internal.Structurise.from_struct(r),
      t: Evision.Internal.Structurise.from_struct(t),
      flag: Evision.Internal.Structurise.from_struct(flag),
      numDisparities: Evision.Internal.Structurise.from_struct(numDisparities),
      sADWindowSize: Evision.Internal.Structurise.from_struct(sADWindowSize)
    ]
    :evision_nif.omnidir_stereoReconstruct(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Stereo 3D reconstruction from a pair of images

  ##### Positional Arguments
  - **image1**: `Evision.Mat`.

    The first input image

  - **image2**: `Evision.Mat`.

    The second input image

  - **k1**: `Evision.Mat`.

    Input camera matrix of the first camera

  - **d1**: `Evision.Mat`.

    Input distortion parameters \\f$(k_1, k_2, p_1, p_2)\\f$ for the first camera

  - **xi1**: `Evision.Mat`.

    Input parameter xi for the first camera for CMei's model

  - **k2**: `Evision.Mat`.

    Input camera matrix of the second camera

  - **d2**: `Evision.Mat`.

    Input distortion parameters \\f$(k_1, k_2, p_1, p_2)\\f$ for the second camera

  - **xi2**: `Evision.Mat`.

    Input parameter xi for the second camera for CMei's model

  - **r**: `Evision.Mat`.

    Rotation between the first and second camera

  - **t**: `Evision.Mat`.

    Translation between the first and second camera

  - **flag**: `integer()`.

    Flag of rectification type, RECTIFY_PERSPECTIVE or RECTIFY_LONGLATI

  - **numDisparities**: `integer()`.

    The parameter 'numDisparities' in StereoSGBM, see StereoSGBM for details.

  - **sADWindowSize**: `integer()`.

    The parameter 'SADWindowSize' in StereoSGBM, see StereoSGBM for details.

  ##### Keyword Arguments
  - **newSize**: `Size`.

    Image size of rectified image, see omnidir::undistortImage

  - **knew**: `Evision.Mat`.

    New camera matrix of rectified image, see omnidir::undistortImage

  - **pointType**: `integer()`.

    Point cloud type, it can be XYZRGB or XYZ

  ##### Return
  - **disparity**: `Evision.Mat.t()`.

    Disparity map generated by stereo matching

  - **image1Rec**: `Evision.Mat.t()`.

    Rectified image of the first image

  - **image2Rec**: `Evision.Mat.t()`.

    rectified image of the second image

  - **pointCloud**: `Evision.Mat.t()`.

    Point cloud of 3D reconstruction, with type CV_64FC3

  Python prototype (for reference only):
  ```python3
  stereoReconstruct(image1, image2, K1, D1, xi1, K2, D2, xi2, R, T, flag, numDisparities, SADWindowSize[, disparity[, image1Rec[, image2Rec[, newSize[, Knew[, pointCloud[, pointType]]]]]]]) -> disparity, image1Rec, image2Rec, pointCloud
  ```
  """
  @spec stereoReconstruct(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), integer(), integer()) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def stereoReconstruct(image1, image2, k1, d1, xi1, k2, d2, xi2, r, t, flag, numDisparities, sADWindowSize) when (is_struct(image1, Evision.Mat) or is_struct(image1, Nx.Tensor) or is_number(image1) or is_tuple(image1)) and (is_struct(image2, Evision.Mat) or is_struct(image2, Nx.Tensor) or is_number(image2) or is_tuple(image2)) and (is_struct(k1, Evision.Mat) or is_struct(k1, Nx.Tensor) or is_number(k1) or is_tuple(k1)) and (is_struct(d1, Evision.Mat) or is_struct(d1, Nx.Tensor) or is_number(d1) or is_tuple(d1)) and (is_struct(xi1, Evision.Mat) or is_struct(xi1, Nx.Tensor) or is_number(xi1) or is_tuple(xi1)) and (is_struct(k2, Evision.Mat) or is_struct(k2, Nx.Tensor) or is_number(k2) or is_tuple(k2)) and (is_struct(d2, Evision.Mat) or is_struct(d2, Nx.Tensor) or is_number(d2) or is_tuple(d2)) and (is_struct(xi2, Evision.Mat) or is_struct(xi2, Nx.Tensor) or is_number(xi2) or is_tuple(xi2)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and (is_struct(t, Evision.Mat) or is_struct(t, Nx.Tensor) or is_number(t) or is_tuple(t)) and is_integer(flag) and is_integer(numDisparities) and is_integer(sADWindowSize)
  do
    positional = [
      image1: Evision.Internal.Structurise.from_struct(image1),
      image2: Evision.Internal.Structurise.from_struct(image2),
      k1: Evision.Internal.Structurise.from_struct(k1),
      d1: Evision.Internal.Structurise.from_struct(d1),
      xi1: Evision.Internal.Structurise.from_struct(xi1),
      k2: Evision.Internal.Structurise.from_struct(k2),
      d2: Evision.Internal.Structurise.from_struct(d2),
      xi2: Evision.Internal.Structurise.from_struct(xi2),
      r: Evision.Internal.Structurise.from_struct(r),
      t: Evision.Internal.Structurise.from_struct(t),
      flag: Evision.Internal.Structurise.from_struct(flag),
      numDisparities: Evision.Internal.Structurise.from_struct(numDisparities),
      sADWindowSize: Evision.Internal.Structurise.from_struct(sADWindowSize)
    ]
    :evision_nif.omnidir_stereoReconstruct(positional)
    |> to_struct()
  end

  @doc """
  Stereo rectification for omnidirectional camera model. It computes the rectification rotations for two cameras

  ##### Positional Arguments
  - **r**: `Evision.Mat`.

    Rotation between the first and second camera

  - **t**: `Evision.Mat`.

    Translation between the first and second camera

  ##### Return
  - **r1**: `Evision.Mat.t()`.

    Output 3x3 rotation matrix for the first camera

  - **r2**: `Evision.Mat.t()`.

    Output 3x3 rotation matrix for the second camera

  Python prototype (for reference only):
  ```python3
  stereoRectify(R, T[, R1[, R2]]) -> R1, R2
  ```
  """
  @spec stereoRectify(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def stereoRectify(r, t, opts) when (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and (is_struct(t, Evision.Mat) or is_struct(t, Nx.Tensor) or is_number(t) or is_tuple(t)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      r: Evision.Internal.Structurise.from_struct(r),
      t: Evision.Internal.Structurise.from_struct(t)
    ]
    :evision_nif.omnidir_stereoRectify(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Stereo rectification for omnidirectional camera model. It computes the rectification rotations for two cameras

  ##### Positional Arguments
  - **r**: `Evision.Mat`.

    Rotation between the first and second camera

  - **t**: `Evision.Mat`.

    Translation between the first and second camera

  ##### Return
  - **r1**: `Evision.Mat.t()`.

    Output 3x3 rotation matrix for the first camera

  - **r2**: `Evision.Mat.t()`.

    Output 3x3 rotation matrix for the second camera

  Python prototype (for reference only):
  ```python3
  stereoRectify(R, T[, R1[, R2]]) -> R1, R2
  ```
  """
  @spec stereoRectify(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def stereoRectify(r, t) when (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and (is_struct(t, Evision.Mat) or is_struct(t, Nx.Tensor) or is_number(t) or is_tuple(t))
  do
    positional = [
      r: Evision.Internal.Structurise.from_struct(r),
      t: Evision.Internal.Structurise.from_struct(t)
    ]
    :evision_nif.omnidir_stereoRectify(positional)
    |> to_struct()
  end

  @doc """
  Undistort omnidirectional images to perspective images

  ##### Positional Arguments
  - **distorted**: `Evision.Mat`.

    The input omnidirectional image.

  - **k**: `Evision.Mat`.

    Camera matrix \\f$K = \\vecthreethree{f_x}{s}{c_x}{0}{f_y}{c_y}{0}{0}{_1}\\f$.

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$(k_1, k_2, p_1, p_2)\\f$.

  - **xi**: `Evision.Mat`.

    The parameter xi for CMei's model.

  - **flags**: `integer()`.

    Flags indicates the rectification type,  RECTIFY_PERSPECTIVE, RECTIFY_CYLINDRICAL, RECTIFY_LONGLATI and RECTIFY_STEREOGRAPHIC

  ##### Keyword Arguments
  - **knew**: `Evision.Mat`.

    Camera matrix of the distorted image. If it is not assigned, it is just K.

  - **new_size**: `Size`.

    The new image size. By default, it is the size of distorted.

  - **r**: `Evision.Mat`.

    Rotation matrix between the input and output images. By default, it is identity matrix.

  ##### Return
  - **undistorted**: `Evision.Mat.t()`.

    The output undistorted image.

  Python prototype (for reference only):
  ```python3
  undistortImage(distorted, K, D, xi, flags[, undistorted[, Knew[, new_size[, R]]]]) -> undistorted
  ```
  """
  @spec undistortImage(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{:knew, term()} | {:new_size, term()} | {:r, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def undistortImage(distorted, k, d, xi, flags, opts) when (is_struct(distorted, Evision.Mat) or is_struct(distorted, Nx.Tensor) or is_number(distorted) or is_tuple(distorted)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (is_struct(xi, Evision.Mat) or is_struct(xi, Nx.Tensor) or is_number(xi) or is_tuple(xi)) and is_integer(flags) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:knew, :new_size, :r])
    positional = [
      distorted: Evision.Internal.Structurise.from_struct(distorted),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d),
      xi: Evision.Internal.Structurise.from_struct(xi),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.omnidir_undistortImage(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Undistort omnidirectional images to perspective images

  ##### Positional Arguments
  - **distorted**: `Evision.Mat`.

    The input omnidirectional image.

  - **k**: `Evision.Mat`.

    Camera matrix \\f$K = \\vecthreethree{f_x}{s}{c_x}{0}{f_y}{c_y}{0}{0}{_1}\\f$.

  - **d**: `Evision.Mat`.

    Input vector of distortion coefficients \\f$(k_1, k_2, p_1, p_2)\\f$.

  - **xi**: `Evision.Mat`.

    The parameter xi for CMei's model.

  - **flags**: `integer()`.

    Flags indicates the rectification type,  RECTIFY_PERSPECTIVE, RECTIFY_CYLINDRICAL, RECTIFY_LONGLATI and RECTIFY_STEREOGRAPHIC

  ##### Keyword Arguments
  - **knew**: `Evision.Mat`.

    Camera matrix of the distorted image. If it is not assigned, it is just K.

  - **new_size**: `Size`.

    The new image size. By default, it is the size of distorted.

  - **r**: `Evision.Mat`.

    Rotation matrix between the input and output images. By default, it is identity matrix.

  ##### Return
  - **undistorted**: `Evision.Mat.t()`.

    The output undistorted image.

  Python prototype (for reference only):
  ```python3
  undistortImage(distorted, K, D, xi, flags[, undistorted[, Knew[, new_size[, R]]]]) -> undistorted
  ```
  """
  @spec undistortImage(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def undistortImage(distorted, k, d, xi, flags) when (is_struct(distorted, Evision.Mat) or is_struct(distorted, Nx.Tensor) or is_number(distorted) or is_tuple(distorted)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (is_struct(xi, Evision.Mat) or is_struct(xi, Nx.Tensor) or is_number(xi) or is_tuple(xi)) and is_integer(flags)
  do
    positional = [
      distorted: Evision.Internal.Structurise.from_struct(distorted),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d),
      xi: Evision.Internal.Structurise.from_struct(xi),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.omnidir_undistortImage(positional)
    |> to_struct()
  end

  @doc """
  Undistort 2D image points for omnidirectional camera using CMei's model

  ##### Positional Arguments
  - **distorted**: `Evision.Mat`.

    Array of distorted image points, vector of Vec2f
    or 1xN/Nx1 2-channel Mat of type CV_32F, 64F depth is also acceptable

  - **k**: `Evision.Mat`.

    Camera matrix \\f$K = \\vecthreethree{f_x}{s}{c_x}{0}{f_y}{c_y}{0}{0}{_1}\\f$.

  - **d**: `Evision.Mat`.

    Distortion coefficients \\f$(k_1, k_2, p_1, p_2)\\f$.

  - **xi**: `Evision.Mat`.

    The parameter xi for CMei's model

  - **r**: `Evision.Mat`.

    Rotation trainsform between the original and object space : 3x3 1-channel, or vector: 3x1/1x3
    1-channel or 1x1 3-channel

  ##### Return
  - **undistorted**: `Evision.Mat.t()`.

    array of normalized object points, vector of Vec2f/Vec2d or 1xN/Nx1 2-channel Mat with the same
    depth of distorted points.

  Python prototype (for reference only):
  ```python3
  undistortPoints(distorted, K, D, xi, R[, undistorted]) -> undistorted
  ```
  """
  @spec undistortPoints(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def undistortPoints(distorted, k, d, xi, r, opts) when (is_struct(distorted, Evision.Mat) or is_struct(distorted, Nx.Tensor) or is_number(distorted) or is_tuple(distorted)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (is_struct(xi, Evision.Mat) or is_struct(xi, Nx.Tensor) or is_number(xi) or is_tuple(xi)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      distorted: Evision.Internal.Structurise.from_struct(distorted),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d),
      xi: Evision.Internal.Structurise.from_struct(xi),
      r: Evision.Internal.Structurise.from_struct(r)
    ]
    :evision_nif.omnidir_undistortPoints(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Undistort 2D image points for omnidirectional camera using CMei's model

  ##### Positional Arguments
  - **distorted**: `Evision.Mat`.

    Array of distorted image points, vector of Vec2f
    or 1xN/Nx1 2-channel Mat of type CV_32F, 64F depth is also acceptable

  - **k**: `Evision.Mat`.

    Camera matrix \\f$K = \\vecthreethree{f_x}{s}{c_x}{0}{f_y}{c_y}{0}{0}{_1}\\f$.

  - **d**: `Evision.Mat`.

    Distortion coefficients \\f$(k_1, k_2, p_1, p_2)\\f$.

  - **xi**: `Evision.Mat`.

    The parameter xi for CMei's model

  - **r**: `Evision.Mat`.

    Rotation trainsform between the original and object space : 3x3 1-channel, or vector: 3x1/1x3
    1-channel or 1x1 3-channel

  ##### Return
  - **undistorted**: `Evision.Mat.t()`.

    array of normalized object points, vector of Vec2f/Vec2d or 1xN/Nx1 2-channel Mat with the same
    depth of distorted points.

  Python prototype (for reference only):
  ```python3
  undistortPoints(distorted, K, D, xi, R[, undistorted]) -> undistorted
  ```
  """
  @spec undistortPoints(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def undistortPoints(distorted, k, d, xi, r) when (is_struct(distorted, Evision.Mat) or is_struct(distorted, Nx.Tensor) or is_number(distorted) or is_tuple(distorted)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(d, Evision.Mat) or is_struct(d, Nx.Tensor) or is_number(d) or is_tuple(d)) and (is_struct(xi, Evision.Mat) or is_struct(xi, Nx.Tensor) or is_number(xi) or is_tuple(xi)) and (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r))
  do
    positional = [
      distorted: Evision.Internal.Structurise.from_struct(distorted),
      k: Evision.Internal.Structurise.from_struct(k),
      d: Evision.Internal.Structurise.from_struct(d),
      xi: Evision.Internal.Structurise.from_struct(xi),
      r: Evision.Internal.Structurise.from_struct(r)
    ]
    :evision_nif.omnidir_undistortPoints(positional)
    |> to_struct()
  end
end
