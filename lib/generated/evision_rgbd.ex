defmodule Evision.RGBD do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `RGBD` struct.

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
  def to_struct({:ok, %{class: Evision.RGBD, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.RGBD, ref: ref}) do
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
  depthTo3d

  ##### Positional Arguments
  - **depth**: `Evision.Mat`.

    the depth image (if given as short int CV_U, it is assumed to be the depth in millimeters
    (as done with the Microsoft Kinect), otherwise, if given as CV_32F or CV_64F, it is assumed in meters)

  - **k**: `Evision.Mat`.

    The calibration matrix

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    the mask of the points to consider (can be empty)

  ##### Return
  - **points3d**: `Evision.Mat.t()`.

    the resulting 3d points. They are of depth the same as `depth` if it is CV_32F or CV_64F, and the
    depth of `K` if `depth` is of depth CV_U

  Converts a depth image to an organized set of 3d points.
   The coordinate system is x pointing left, y down and z away from the camera

  Python prototype (for reference only):
  ```python3
  depthTo3d(depth, K[, points3d[, mask]]) -> points3d
  ```
  """
  @spec depthTo3d(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def depthTo3d(depth, k, opts) when (is_struct(depth, Evision.Mat) or is_struct(depth, Nx.Tensor) or is_number(depth) or is_tuple(depth)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      depth: Evision.Internal.Structurise.from_struct(depth),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.rgbd_depthTo3d(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  depthTo3d

  ##### Positional Arguments
  - **depth**: `Evision.Mat`.

    the depth image (if given as short int CV_U, it is assumed to be the depth in millimeters
    (as done with the Microsoft Kinect), otherwise, if given as CV_32F or CV_64F, it is assumed in meters)

  - **k**: `Evision.Mat`.

    The calibration matrix

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    the mask of the points to consider (can be empty)

  ##### Return
  - **points3d**: `Evision.Mat.t()`.

    the resulting 3d points. They are of depth the same as `depth` if it is CV_32F or CV_64F, and the
    depth of `K` if `depth` is of depth CV_U

  Converts a depth image to an organized set of 3d points.
   The coordinate system is x pointing left, y down and z away from the camera

  Python prototype (for reference only):
  ```python3
  depthTo3d(depth, K[, points3d[, mask]]) -> points3d
  ```
  """
  @spec depthTo3d(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def depthTo3d(depth, k) when (is_struct(depth, Evision.Mat) or is_struct(depth, Nx.Tensor) or is_number(depth) or is_tuple(depth)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k))
  do
    positional = [
      depth: Evision.Internal.Structurise.from_struct(depth),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.rgbd_depthTo3d(positional)
    |> to_struct()
  end

  @doc """
  depthTo3dSparse

  ##### Positional Arguments
  - **depth**: `Evision.Mat`.

    the depth image

  - **in_K**: `Evision.Mat`.

  - **in_points**: `Evision.Mat`.

    the list of xy coordinates

  ##### Return
  - **points3d**: `Evision.Mat.t()`.

    the resulting 3d points

  Python prototype (for reference only):
  ```python3
  depthTo3dSparse(depth, in_K, in_points[, points3d]) -> points3d
  ```
  """
  @spec depthTo3dSparse(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def depthTo3dSparse(depth, in_K, in_points, opts) when (is_struct(depth, Evision.Mat) or is_struct(depth, Nx.Tensor) or is_number(depth) or is_tuple(depth)) and (is_struct(in_K, Evision.Mat) or is_struct(in_K, Nx.Tensor) or is_number(in_K) or is_tuple(in_K)) and (is_struct(in_points, Evision.Mat) or is_struct(in_points, Nx.Tensor) or is_number(in_points) or is_tuple(in_points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      depth: Evision.Internal.Structurise.from_struct(depth),
      in_K: Evision.Internal.Structurise.from_struct(in_K),
      in_points: Evision.Internal.Structurise.from_struct(in_points)
    ]
    :evision_nif.rgbd_depthTo3dSparse(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  depthTo3dSparse

  ##### Positional Arguments
  - **depth**: `Evision.Mat`.

    the depth image

  - **in_K**: `Evision.Mat`.

  - **in_points**: `Evision.Mat`.

    the list of xy coordinates

  ##### Return
  - **points3d**: `Evision.Mat.t()`.

    the resulting 3d points

  Python prototype (for reference only):
  ```python3
  depthTo3dSparse(depth, in_K, in_points[, points3d]) -> points3d
  ```
  """
  @spec depthTo3dSparse(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def depthTo3dSparse(depth, in_K, in_points) when (is_struct(depth, Evision.Mat) or is_struct(depth, Nx.Tensor) or is_number(depth) or is_tuple(depth)) and (is_struct(in_K, Evision.Mat) or is_struct(in_K, Nx.Tensor) or is_number(in_K) or is_tuple(in_K)) and (is_struct(in_points, Evision.Mat) or is_struct(in_points, Nx.Tensor) or is_number(in_points) or is_tuple(in_points))
  do
    positional = [
      depth: Evision.Internal.Structurise.from_struct(depth),
      in_K: Evision.Internal.Structurise.from_struct(in_K),
      in_points: Evision.Internal.Structurise.from_struct(in_points)
    ]
    :evision_nif.rgbd_depthTo3dSparse(positional)
    |> to_struct()
  end

  @doc """
  registerDepth

  ##### Positional Arguments
  - **unregisteredCameraMatrix**: `Evision.Mat`.

    the camera matrix of the depth camera

  - **registeredCameraMatrix**: `Evision.Mat`.

    the camera matrix of the external camera

  - **registeredDistCoeffs**: `Evision.Mat`.

    the distortion coefficients of the external camera

  - **rt**: `Evision.Mat`.

    the rigid body transform between the cameras. Transforms points from depth camera frame to external camera frame.

  - **unregisteredDepth**: `Evision.Mat`.

    the input depth data

  - **outputImagePlaneSize**: `Size`.

    the image plane dimensions of the external camera (width, height)

  ##### Keyword Arguments
  - **depthDilation**: `bool`.

    whether or not the depth is dilated to avoid holes and occlusion errors (optional)

  ##### Return
  - **registeredDepth**: `Evision.Mat.t()`.

    the result of transforming the depth into the external camera

  Registers depth data to an external camera
   Registration is performed by creating a depth cloud, transforming the cloud by
   the rigid body transformation between the cameras, and then projecting the
   transformed points into the RGB camera.
   uv_rgb = K_rgb * [R | t] * z * inv(K_ir) * uv_ir
   Currently does not check for negative depth values.

  Python prototype (for reference only):
  ```python3
  registerDepth(unregisteredCameraMatrix, registeredCameraMatrix, registeredDistCoeffs, Rt, unregisteredDepth, outputImagePlaneSize[, registeredDepth[, depthDilation]]) -> registeredDepth
  ```
  """
  @spec registerDepth(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, [{:depthDilation, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def registerDepth(unregisteredCameraMatrix, registeredCameraMatrix, registeredDistCoeffs, rt, unregisteredDepth, outputImagePlaneSize, opts) when (is_struct(unregisteredCameraMatrix, Evision.Mat) or is_struct(unregisteredCameraMatrix, Nx.Tensor) or is_number(unregisteredCameraMatrix) or is_tuple(unregisteredCameraMatrix)) and (is_struct(registeredCameraMatrix, Evision.Mat) or is_struct(registeredCameraMatrix, Nx.Tensor) or is_number(registeredCameraMatrix) or is_tuple(registeredCameraMatrix)) and (is_struct(registeredDistCoeffs, Evision.Mat) or is_struct(registeredDistCoeffs, Nx.Tensor) or is_number(registeredDistCoeffs) or is_tuple(registeredDistCoeffs)) and (is_struct(rt, Evision.Mat) or is_struct(rt, Nx.Tensor) or is_number(rt) or is_tuple(rt)) and (is_struct(unregisteredDepth, Evision.Mat) or is_struct(unregisteredDepth, Nx.Tensor) or is_number(unregisteredDepth) or is_tuple(unregisteredDepth)) and is_tuple(outputImagePlaneSize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:depthDilation])
    positional = [
      unregisteredCameraMatrix: Evision.Internal.Structurise.from_struct(unregisteredCameraMatrix),
      registeredCameraMatrix: Evision.Internal.Structurise.from_struct(registeredCameraMatrix),
      registeredDistCoeffs: Evision.Internal.Structurise.from_struct(registeredDistCoeffs),
      rt: Evision.Internal.Structurise.from_struct(rt),
      unregisteredDepth: Evision.Internal.Structurise.from_struct(unregisteredDepth),
      outputImagePlaneSize: Evision.Internal.Structurise.from_struct(outputImagePlaneSize)
    ]
    :evision_nif.rgbd_registerDepth(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  registerDepth

  ##### Positional Arguments
  - **unregisteredCameraMatrix**: `Evision.Mat`.

    the camera matrix of the depth camera

  - **registeredCameraMatrix**: `Evision.Mat`.

    the camera matrix of the external camera

  - **registeredDistCoeffs**: `Evision.Mat`.

    the distortion coefficients of the external camera

  - **rt**: `Evision.Mat`.

    the rigid body transform between the cameras. Transforms points from depth camera frame to external camera frame.

  - **unregisteredDepth**: `Evision.Mat`.

    the input depth data

  - **outputImagePlaneSize**: `Size`.

    the image plane dimensions of the external camera (width, height)

  ##### Keyword Arguments
  - **depthDilation**: `bool`.

    whether or not the depth is dilated to avoid holes and occlusion errors (optional)

  ##### Return
  - **registeredDepth**: `Evision.Mat.t()`.

    the result of transforming the depth into the external camera

  Registers depth data to an external camera
   Registration is performed by creating a depth cloud, transforming the cloud by
   the rigid body transformation between the cameras, and then projecting the
   transformed points into the RGB camera.
   uv_rgb = K_rgb * [R | t] * z * inv(K_ir) * uv_ir
   Currently does not check for negative depth values.

  Python prototype (for reference only):
  ```python3
  registerDepth(unregisteredCameraMatrix, registeredCameraMatrix, registeredDistCoeffs, Rt, unregisteredDepth, outputImagePlaneSize[, registeredDepth[, depthDilation]]) -> registeredDepth
  ```
  """
  @spec registerDepth(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}) :: Evision.Mat.t() | {:error, String.t()}
  def registerDepth(unregisteredCameraMatrix, registeredCameraMatrix, registeredDistCoeffs, rt, unregisteredDepth, outputImagePlaneSize) when (is_struct(unregisteredCameraMatrix, Evision.Mat) or is_struct(unregisteredCameraMatrix, Nx.Tensor) or is_number(unregisteredCameraMatrix) or is_tuple(unregisteredCameraMatrix)) and (is_struct(registeredCameraMatrix, Evision.Mat) or is_struct(registeredCameraMatrix, Nx.Tensor) or is_number(registeredCameraMatrix) or is_tuple(registeredCameraMatrix)) and (is_struct(registeredDistCoeffs, Evision.Mat) or is_struct(registeredDistCoeffs, Nx.Tensor) or is_number(registeredDistCoeffs) or is_tuple(registeredDistCoeffs)) and (is_struct(rt, Evision.Mat) or is_struct(rt, Nx.Tensor) or is_number(rt) or is_tuple(rt)) and (is_struct(unregisteredDepth, Evision.Mat) or is_struct(unregisteredDepth, Nx.Tensor) or is_number(unregisteredDepth) or is_tuple(unregisteredDepth)) and is_tuple(outputImagePlaneSize)
  do
    positional = [
      unregisteredCameraMatrix: Evision.Internal.Structurise.from_struct(unregisteredCameraMatrix),
      registeredCameraMatrix: Evision.Internal.Structurise.from_struct(registeredCameraMatrix),
      registeredDistCoeffs: Evision.Internal.Structurise.from_struct(registeredDistCoeffs),
      rt: Evision.Internal.Structurise.from_struct(rt),
      unregisteredDepth: Evision.Internal.Structurise.from_struct(unregisteredDepth),
      outputImagePlaneSize: Evision.Internal.Structurise.from_struct(outputImagePlaneSize)
    ]
    :evision_nif.rgbd_registerDepth(positional)
    |> to_struct()
  end

  @doc """
  rescaleDepth

  ##### Positional Arguments
  - **arg_in**: `Evision.Mat`.

    the depth image (if given as short int CV_U, it is assumed to be the depth in millimeters
    (as done with the Microsoft Kinect), it is assumed in meters)

  - **depth**: `integer()`.

    the desired output depth (floats or double)

  ##### Keyword Arguments
  - **depth_factor**: `double`.

    (optional) factor by which depth is converted to distance (by default = 1000.0 for Kinect sensor)

  ##### Return
  - **out**: `Evision.Mat.t()`.

    The rescaled float depth image

  If the input image is of type CV_16UC1 (like the Kinect one), the image is converted to floats, divided
   by depth_factor to get a depth in meters, and the values 0 are converted to std::numeric_limits<float>::quiet_NaN()
   Otherwise, the image is simply converted to floats

  Python prototype (for reference only):
  ```python3
  rescaleDepth(in, depth[, out[, depth_factor]]) -> out
  ```
  """
  @spec rescaleDepth(Evision.Mat.maybe_mat_in(), integer(), [{:depth_factor, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def rescaleDepth(arg_in, depth, opts) when (is_struct(arg_in, Evision.Mat) or is_struct(arg_in, Nx.Tensor) or is_number(arg_in) or is_tuple(arg_in)) and is_integer(depth) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:depth_factor])
    positional = [
      arg_in: Evision.Internal.Structurise.from_struct(arg_in),
      depth: Evision.Internal.Structurise.from_struct(depth)
    ]
    :evision_nif.rgbd_rescaleDepth(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  rescaleDepth

  ##### Positional Arguments
  - **arg_in**: `Evision.Mat`.

    the depth image (if given as short int CV_U, it is assumed to be the depth in millimeters
    (as done with the Microsoft Kinect), it is assumed in meters)

  - **depth**: `integer()`.

    the desired output depth (floats or double)

  ##### Keyword Arguments
  - **depth_factor**: `double`.

    (optional) factor by which depth is converted to distance (by default = 1000.0 for Kinect sensor)

  ##### Return
  - **out**: `Evision.Mat.t()`.

    The rescaled float depth image

  If the input image is of type CV_16UC1 (like the Kinect one), the image is converted to floats, divided
   by depth_factor to get a depth in meters, and the values 0 are converted to std::numeric_limits<float>::quiet_NaN()
   Otherwise, the image is simply converted to floats

  Python prototype (for reference only):
  ```python3
  rescaleDepth(in, depth[, out[, depth_factor]]) -> out
  ```
  """
  @spec rescaleDepth(Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def rescaleDepth(arg_in, depth) when (is_struct(arg_in, Evision.Mat) or is_struct(arg_in, Nx.Tensor) or is_number(arg_in) or is_tuple(arg_in)) and is_integer(depth)
  do
    positional = [
      arg_in: Evision.Internal.Structurise.from_struct(arg_in),
      depth: Evision.Internal.Structurise.from_struct(depth)
    ]
    :evision_nif.rgbd_rescaleDepth(positional)
    |> to_struct()
  end

  @doc """
  warpFrame

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    The image (of CV_8UC1 or CV_8UC3 type)

  - **depth**: `Evision.Mat`.

    The depth (of type used in depthTo3d fuction)

  - **mask**: `Evision.Mat`.

    The mask of used pixels (of CV_8UC1), it can be empty

  - **rt**: `Evision.Mat`.

    The transformation that will be applied to the 3d points computed from the depth

  - **cameraMatrix**: `Evision.Mat`.

    Camera matrix

  - **distCoeff**: `Evision.Mat`.

    Distortion coefficients

  ##### Return
  - **warpedImage**: `Evision.Mat.t()`.

    The warped image.

  - **warpedDepth**: `Evision.Mat.t()`.

    The warped depth.

  - **warpedMask**: `Evision.Mat.t()`.

    The warped mask.

  Warp the image: compute 3d points from the depth, transform them using given transformation,
   then project color point cloud to an image plane.
   This function can be used to visualize results of the Odometry algorithm.

  Python prototype (for reference only):
  ```python3
  warpFrame(image, depth, mask, Rt, cameraMatrix, distCoeff[, warpedImage[, warpedDepth[, warpedMask]]]) -> warpedImage, warpedDepth, warpedMask
  ```
  """
  @spec warpFrame(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def warpFrame(image, depth, mask, rt, cameraMatrix, distCoeff, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(depth, Evision.Mat) or is_struct(depth, Nx.Tensor) or is_number(depth) or is_tuple(depth)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (is_struct(rt, Evision.Mat) or is_struct(rt, Nx.Tensor) or is_number(rt) or is_tuple(rt)) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeff, Evision.Mat) or is_struct(distCoeff, Nx.Tensor) or is_number(distCoeff) or is_tuple(distCoeff)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      depth: Evision.Internal.Structurise.from_struct(depth),
      mask: Evision.Internal.Structurise.from_struct(mask),
      rt: Evision.Internal.Structurise.from_struct(rt),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeff: Evision.Internal.Structurise.from_struct(distCoeff)
    ]
    :evision_nif.rgbd_warpFrame(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  warpFrame

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    The image (of CV_8UC1 or CV_8UC3 type)

  - **depth**: `Evision.Mat`.

    The depth (of type used in depthTo3d fuction)

  - **mask**: `Evision.Mat`.

    The mask of used pixels (of CV_8UC1), it can be empty

  - **rt**: `Evision.Mat`.

    The transformation that will be applied to the 3d points computed from the depth

  - **cameraMatrix**: `Evision.Mat`.

    Camera matrix

  - **distCoeff**: `Evision.Mat`.

    Distortion coefficients

  ##### Return
  - **warpedImage**: `Evision.Mat.t()`.

    The warped image.

  - **warpedDepth**: `Evision.Mat.t()`.

    The warped depth.

  - **warpedMask**: `Evision.Mat.t()`.

    The warped mask.

  Warp the image: compute 3d points from the depth, transform them using given transformation,
   then project color point cloud to an image plane.
   This function can be used to visualize results of the Odometry algorithm.

  Python prototype (for reference only):
  ```python3
  warpFrame(image, depth, mask, Rt, cameraMatrix, distCoeff[, warpedImage[, warpedDepth[, warpedMask]]]) -> warpedImage, warpedDepth, warpedMask
  ```
  """
  @spec warpFrame(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def warpFrame(image, depth, mask, rt, cameraMatrix, distCoeff) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(depth, Evision.Mat) or is_struct(depth, Nx.Tensor) or is_number(depth) or is_tuple(depth)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (is_struct(rt, Evision.Mat) or is_struct(rt, Nx.Tensor) or is_number(rt) or is_tuple(rt)) and (is_struct(cameraMatrix, Evision.Mat) or is_struct(cameraMatrix, Nx.Tensor) or is_number(cameraMatrix) or is_tuple(cameraMatrix)) and (is_struct(distCoeff, Evision.Mat) or is_struct(distCoeff, Nx.Tensor) or is_number(distCoeff) or is_tuple(distCoeff))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      depth: Evision.Internal.Structurise.from_struct(depth),
      mask: Evision.Internal.Structurise.from_struct(mask),
      rt: Evision.Internal.Structurise.from_struct(rt),
      cameraMatrix: Evision.Internal.Structurise.from_struct(cameraMatrix),
      distCoeff: Evision.Internal.Structurise.from_struct(distCoeff)
    ]
    :evision_nif.rgbd_warpFrame(positional)
    |> to_struct()
  end
end
