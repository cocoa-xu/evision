defmodule Evision.ColoredKinFu.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ColoredKinFu.Params` struct.

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
  def to_struct({:ok, %{class: Evision.ColoredKinFu.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ColoredKinFu.Params, ref: ref}) do
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
  Constructor for Params
  Sets the initial pose of the TSDF volume.

  ##### Positional Arguments
  - **volumeInitialPose**: `Evision.Mat`.

    4 by 4 Homogeneous Transform matrix to set the intial pose of TSDF volume

  ##### Return
  - **self**: `Params`

  Python prototype (for reference only):
  ```python3
  Params(volumeInitialPose) -> <colored_kinfu_Params object>
  ```
  """
  @spec params(Evision.Mat.t()) :: Evision.ColoredKinFu.Params.t() | {:error, String.t()}
  def params(volumeInitialPose) when (is_struct(volumeInitialPose, Evision.Mat) or is_struct(volumeInitialPose, Nx.Tensor) or is_number(volumeInitialPose) or is_tuple(volumeInitialPose))
  do
    positional = [
      volumeInitialPose: Evision.Internal.Structurise.from_struct(volumeInitialPose)
    ]
    :evision_nif.colored_kinfu_colored_kinfu_Params_Params(positional)
    |> to_struct()
  end

  @doc """
  Constructor for Params
  Sets the initial pose of the TSDF volume.

  ##### Positional Arguments
  - **volumeInitialPoseRot**: `Evision.Mat`.

    rotation matrix

  - **volumeInitialPoseTransl**: `Vec3f`.

    translation vector

  ##### Return
  - **self**: `Params`

  Python prototype (for reference only):
  ```python3
  Params(volumeInitialPoseRot, volumeInitialPoseTransl) -> <colored_kinfu_Params object>
  ```
  """
  @spec params(Evision.Mat.t(), {number(), number(), number()}) :: Evision.ColoredKinFu.Params.t() | {:error, String.t()}
  def params(volumeInitialPoseRot, volumeInitialPoseTransl) when (is_struct(volumeInitialPoseRot, Evision.Mat) or is_struct(volumeInitialPoseRot, Nx.Tensor) or is_number(volumeInitialPoseRot) or is_tuple(volumeInitialPoseRot))
  do
    positional = [
      volumeInitialPoseRot: Evision.Internal.Structurise.from_struct(volumeInitialPoseRot),
      volumeInitialPoseTransl: Evision.Internal.Structurise.from_struct(volumeInitialPoseTransl)
    ]
    :evision_nif.colored_kinfu_colored_kinfu_Params_Params(positional)
    |> to_struct()
  end

  @doc """
  Params
  ##### Return
  - **self**: `Params`

  Python prototype (for reference only):
  ```python3
  Params() -> <colored_kinfu_Params object>
  ```
  """
  @spec params() :: Evision.ColoredKinFu.Params.t() | {:error, String.t()}
  def params() do
    positional = [
    ]
    :evision_nif.colored_kinfu_colored_kinfu_Params_Params(positional)
    |> to_struct()
  end

  @doc """
  Coarse parameters
  A set of parameters which provides better speed, can fail to match frames
  in case of rapid sensor motion.

  ##### Return
  - **retval**: `Params`

  Python prototype (for reference only):
  ```python3
  coarseParams() -> retval
  ```
  """
  @spec coarseParams() :: Evision.ColoredKinFu.Params.t() | {:error, String.t()}
  def coarseParams() do
    positional = [
    ]
    :evision_nif.colored_kinfu_colored_kinfu_Params_coarseParams_static(positional)
    |> to_struct()
  end

  @doc """
  ColoredTSDF parameters
  A set of parameters suitable for use with HashTSDFVolume

  ##### Positional Arguments
  - **isCoarse**: `bool`

  ##### Return
  - **retval**: `Params`

  Python prototype (for reference only):
  ```python3
  coloredTSDFParams(isCoarse) -> retval
  ```
  """
  @spec coloredTSDFParams(boolean()) :: Evision.ColoredKinFu.Params.t() | {:error, String.t()}
  def coloredTSDFParams(isCoarse) when is_boolean(isCoarse)
  do
    positional = [
      isCoarse: Evision.Internal.Structurise.from_struct(isCoarse)
    ]
    :evision_nif.colored_kinfu_colored_kinfu_Params_coloredTSDFParams_static(positional)
    |> to_struct()
  end

  @doc """
  Default parameters
  A set of parameters which provides better model quality, can be very slow.

  ##### Return
  - **retval**: `Params`

  Python prototype (for reference only):
  ```python3
  defaultParams() -> retval
  ```
  """
  @spec defaultParams() :: Evision.ColoredKinFu.Params.t() | {:error, String.t()}
  def defaultParams() do
    positional = [
    ]
    :evision_nif.colored_kinfu_colored_kinfu_Params_defaultParams_static(positional)
    |> to_struct()
  end

  @doc """
  HashTSDF parameters
  A set of parameters suitable for use with HashTSDFVolume

  ##### Positional Arguments
  - **isCoarse**: `bool`

  ##### Return
  - **retval**: `Params`

  Python prototype (for reference only):
  ```python3
  hashTSDFParams(isCoarse) -> retval
  ```
  """
  @spec hashTSDFParams(boolean()) :: Evision.ColoredKinFu.Params.t() | {:error, String.t()}
  def hashTSDFParams(isCoarse) when is_boolean(isCoarse)
  do
    positional = [
      isCoarse: Evision.Internal.Structurise.from_struct(isCoarse)
    ]
    :evision_nif.colored_kinfu_colored_kinfu_Params_hashTSDFParams_static(positional)
    |> to_struct()
  end

  @doc """
  Set Initial Volume Pose
  Sets the initial pose of the TSDF volume.

  ##### Positional Arguments
  - **self**: `Evision.ColoredKinFu.Params.t()`
  - **homogen_tf**: `Evision.Mat`.

    4 by 4 Homogeneous Transform matrix to set the intial pose of TSDF volume

  Python prototype (for reference only):
  ```python3
  setInitialVolumePose(homogen_tf) -> None
  ```
  """
  @spec setInitialVolumePose(Evision.ColoredKinFu.Params.t(), Evision.Mat.t()) :: Evision.ColoredKinFu.Params.t() | {:error, String.t()}
  def setInitialVolumePose(self, homogen_tf) when (is_struct(homogen_tf, Evision.Mat) or is_struct(homogen_tf, Nx.Tensor) or is_number(homogen_tf) or is_tuple(homogen_tf))
  do
    positional = [
      homogen_tf: Evision.Internal.Structurise.from_struct(homogen_tf)
    ]
    :evision_nif.colored_kinfu_colored_kinfu_Params_setInitialVolumePose(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set Initial Volume Pose
  Sets the initial pose of the TSDF volume.

  ##### Positional Arguments
  - **self**: `Evision.ColoredKinFu.Params.t()`
  - **r**: `Evision.Mat`.

    rotation matrix

  - **t**: `Vec3f`.

    translation vector

  Python prototype (for reference only):
  ```python3
  setInitialVolumePose(R, t) -> None
  ```
  """
  @spec setInitialVolumePose(Evision.ColoredKinFu.Params.t(), Evision.Mat.t(), {number(), number(), number()}) :: Evision.ColoredKinFu.Params.t() | {:error, String.t()}
  def setInitialVolumePose(self, r, t) when (is_struct(r, Evision.Mat) or is_struct(r, Nx.Tensor) or is_number(r) or is_tuple(r))
  do
    positional = [
      r: Evision.Internal.Structurise.from_struct(r),
      t: Evision.Internal.Structurise.from_struct(t)
    ]
    :evision_nif.colored_kinfu_colored_kinfu_Params_setInitialVolumePose(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_bilateral_kernel_size(Evision.ColoredKinFu.Params.t()) :: integer()
  def get_bilateral_kernel_size(self) do
    :evision_nif.colored_kinfu_Params_get_bilateral_kernel_size(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_bilateral_kernel_size(Evision.ColoredKinFu.Params.t(), integer()) :: Evision.ColoredKinFu.Params.t()
  def set_bilateral_kernel_size(self, prop) do
    :evision_nif.colored_kinfu_Params_set_bilateral_kernel_size(
        Evision.Internal.Structurise.from_struct(self),
        [bilateral_kernel_size: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_bilateral_sigma_depth(Evision.ColoredKinFu.Params.t()) :: number()
  def get_bilateral_sigma_depth(self) do
    :evision_nif.colored_kinfu_Params_get_bilateral_sigma_depth(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_bilateral_sigma_depth(Evision.ColoredKinFu.Params.t(), number()) :: Evision.ColoredKinFu.Params.t()
  def set_bilateral_sigma_depth(self, prop) do
    :evision_nif.colored_kinfu_Params_set_bilateral_sigma_depth(
        Evision.Internal.Structurise.from_struct(self),
        [bilateral_sigma_depth: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_bilateral_sigma_spatial(Evision.ColoredKinFu.Params.t()) :: number()
  def get_bilateral_sigma_spatial(self) do
    :evision_nif.colored_kinfu_Params_get_bilateral_sigma_spatial(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_bilateral_sigma_spatial(Evision.ColoredKinFu.Params.t(), number()) :: Evision.ColoredKinFu.Params.t()
  def set_bilateral_sigma_spatial(self, prop) do
    :evision_nif.colored_kinfu_Params_set_bilateral_sigma_spatial(
        Evision.Internal.Structurise.from_struct(self),
        [bilateral_sigma_spatial: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_depthFactor(Evision.ColoredKinFu.Params.t()) :: number()
  def get_depthFactor(self) do
    :evision_nif.colored_kinfu_Params_get_depthFactor(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_depthFactor(Evision.ColoredKinFu.Params.t(), number()) :: Evision.ColoredKinFu.Params.t()
  def set_depthFactor(self, prop) do
    :evision_nif.colored_kinfu_Params_set_depthFactor(
        Evision.Internal.Structurise.from_struct(self),
        [depthFactor: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_frameSize(Evision.ColoredKinFu.Params.t()) :: {number(), number()}
  def get_frameSize(self) do
    :evision_nif.colored_kinfu_Params_get_frameSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_frameSize(Evision.ColoredKinFu.Params.t(), {number(), number()}) :: Evision.ColoredKinFu.Params.t()
  def set_frameSize(self, prop) do
    :evision_nif.colored_kinfu_Params_set_frameSize(
        Evision.Internal.Structurise.from_struct(self),
        [frameSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_icpAngleThresh(Evision.ColoredKinFu.Params.t()) :: number()
  def get_icpAngleThresh(self) do
    :evision_nif.colored_kinfu_Params_get_icpAngleThresh(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_icpAngleThresh(Evision.ColoredKinFu.Params.t(), number()) :: Evision.ColoredKinFu.Params.t()
  def set_icpAngleThresh(self, prop) do
    :evision_nif.colored_kinfu_Params_set_icpAngleThresh(
        Evision.Internal.Structurise.from_struct(self),
        [icpAngleThresh: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_icpDistThresh(Evision.ColoredKinFu.Params.t()) :: number()
  def get_icpDistThresh(self) do
    :evision_nif.colored_kinfu_Params_get_icpDistThresh(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_icpDistThresh(Evision.ColoredKinFu.Params.t(), number()) :: Evision.ColoredKinFu.Params.t()
  def set_icpDistThresh(self, prop) do
    :evision_nif.colored_kinfu_Params_set_icpDistThresh(
        Evision.Internal.Structurise.from_struct(self),
        [icpDistThresh: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_icpIterations(Evision.ColoredKinFu.Params.t()) :: list(integer())
  def get_icpIterations(self) do
    :evision_nif.colored_kinfu_Params_get_icpIterations(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_icpIterations(Evision.ColoredKinFu.Params.t(), list(integer())) :: Evision.ColoredKinFu.Params.t()
  def set_icpIterations(self, prop) do
    :evision_nif.colored_kinfu_Params_set_icpIterations(
        Evision.Internal.Structurise.from_struct(self),
        [icpIterations: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_intr(Evision.ColoredKinFu.Params.t()) :: Evision.Mat.t()
  def get_intr(self) do
    :evision_nif.colored_kinfu_Params_get_intr(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_intr(Evision.ColoredKinFu.Params.t(), Evision.Mat.t()) :: Evision.ColoredKinFu.Params.t()
  def set_intr(self, prop) do
    :evision_nif.colored_kinfu_Params_set_intr(
        Evision.Internal.Structurise.from_struct(self),
        [intr: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_lightPose(Evision.ColoredKinFu.Params.t()) :: {number(), number(), number()}
  def get_lightPose(self) do
    :evision_nif.colored_kinfu_Params_get_lightPose(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_lightPose(Evision.ColoredKinFu.Params.t(), {number(), number(), number()}) :: Evision.ColoredKinFu.Params.t()
  def set_lightPose(self, prop) do
    :evision_nif.colored_kinfu_Params_set_lightPose(
        Evision.Internal.Structurise.from_struct(self),
        [lightPose: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_pyramidLevels(Evision.ColoredKinFu.Params.t()) :: integer()
  def get_pyramidLevels(self) do
    :evision_nif.colored_kinfu_Params_get_pyramidLevels(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_pyramidLevels(Evision.ColoredKinFu.Params.t(), integer()) :: Evision.ColoredKinFu.Params.t()
  def set_pyramidLevels(self, prop) do
    :evision_nif.colored_kinfu_Params_set_pyramidLevels(
        Evision.Internal.Structurise.from_struct(self),
        [pyramidLevels: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_raycast_step_factor(Evision.ColoredKinFu.Params.t()) :: number()
  def get_raycast_step_factor(self) do
    :evision_nif.colored_kinfu_Params_get_raycast_step_factor(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_raycast_step_factor(Evision.ColoredKinFu.Params.t(), number()) :: Evision.ColoredKinFu.Params.t()
  def set_raycast_step_factor(self, prop) do
    :evision_nif.colored_kinfu_Params_set_raycast_step_factor(
        Evision.Internal.Structurise.from_struct(self),
        [raycast_step_factor: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_rgb_frameSize(Evision.ColoredKinFu.Params.t()) :: {number(), number()}
  def get_rgb_frameSize(self) do
    :evision_nif.colored_kinfu_Params_get_rgb_frameSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_rgb_frameSize(Evision.ColoredKinFu.Params.t(), {number(), number()}) :: Evision.ColoredKinFu.Params.t()
  def set_rgb_frameSize(self, prop) do
    :evision_nif.colored_kinfu_Params_set_rgb_frameSize(
        Evision.Internal.Structurise.from_struct(self),
        [rgb_frameSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_rgb_intr(Evision.ColoredKinFu.Params.t()) :: Evision.Mat.t()
  def get_rgb_intr(self) do
    :evision_nif.colored_kinfu_Params_get_rgb_intr(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_rgb_intr(Evision.ColoredKinFu.Params.t(), Evision.Mat.t()) :: Evision.ColoredKinFu.Params.t()
  def set_rgb_intr(self, prop) do
    :evision_nif.colored_kinfu_Params_set_rgb_intr(
        Evision.Internal.Structurise.from_struct(self),
        [rgb_intr: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_truncateThreshold(Evision.ColoredKinFu.Params.t()) :: number()
  def get_truncateThreshold(self) do
    :evision_nif.colored_kinfu_Params_get_truncateThreshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_truncateThreshold(Evision.ColoredKinFu.Params.t(), number()) :: Evision.ColoredKinFu.Params.t()
  def set_truncateThreshold(self, prop) do
    :evision_nif.colored_kinfu_Params_set_truncateThreshold(
        Evision.Internal.Structurise.from_struct(self),
        [truncateThreshold: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_tsdf_max_weight(Evision.ColoredKinFu.Params.t()) :: integer()
  def get_tsdf_max_weight(self) do
    :evision_nif.colored_kinfu_Params_get_tsdf_max_weight(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_tsdf_max_weight(Evision.ColoredKinFu.Params.t(), integer()) :: Evision.ColoredKinFu.Params.t()
  def set_tsdf_max_weight(self, prop) do
    :evision_nif.colored_kinfu_Params_set_tsdf_max_weight(
        Evision.Internal.Structurise.from_struct(self),
        [tsdf_max_weight: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_tsdf_min_camera_movement(Evision.ColoredKinFu.Params.t()) :: number()
  def get_tsdf_min_camera_movement(self) do
    :evision_nif.colored_kinfu_Params_get_tsdf_min_camera_movement(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_tsdf_min_camera_movement(Evision.ColoredKinFu.Params.t(), number()) :: Evision.ColoredKinFu.Params.t()
  def set_tsdf_min_camera_movement(self, prop) do
    :evision_nif.colored_kinfu_Params_set_tsdf_min_camera_movement(
        Evision.Internal.Structurise.from_struct(self),
        [tsdf_min_camera_movement: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_tsdf_trunc_dist(Evision.ColoredKinFu.Params.t()) :: number()
  def get_tsdf_trunc_dist(self) do
    :evision_nif.colored_kinfu_Params_get_tsdf_trunc_dist(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_tsdf_trunc_dist(Evision.ColoredKinFu.Params.t(), number()) :: Evision.ColoredKinFu.Params.t()
  def set_tsdf_trunc_dist(self, prop) do
    :evision_nif.colored_kinfu_Params_set_tsdf_trunc_dist(
        Evision.Internal.Structurise.from_struct(self),
        [tsdf_trunc_dist: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_volumeDims(Evision.ColoredKinFu.Params.t()) :: {integer(), integer(), integer()}
  def get_volumeDims(self) do
    :evision_nif.colored_kinfu_Params_get_volumeDims(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_volumeDims(Evision.ColoredKinFu.Params.t(), {integer(), integer(), integer()}) :: Evision.ColoredKinFu.Params.t()
  def set_volumeDims(self, prop) do
    :evision_nif.colored_kinfu_Params_set_volumeDims(
        Evision.Internal.Structurise.from_struct(self),
        [volumeDims: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_volumeType(Evision.ColoredKinFu.Params.t()) :: Evision.KinFu.VolumeType.enum()
  def get_volumeType(self) do
    :evision_nif.colored_kinfu_Params_get_volumeType(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_volumeType(Evision.ColoredKinFu.Params.t(), Evision.KinFu.VolumeType.enum()) :: Evision.ColoredKinFu.Params.t()
  def set_volumeType(self, prop) do
    :evision_nif.colored_kinfu_Params_set_volumeType(
        Evision.Internal.Structurise.from_struct(self),
        [volumeType: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_voxelSize(Evision.ColoredKinFu.Params.t()) :: number()
  def get_voxelSize(self) do
    :evision_nif.colored_kinfu_Params_get_voxelSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_voxelSize(Evision.ColoredKinFu.Params.t(), number()) :: Evision.ColoredKinFu.Params.t()
  def set_voxelSize(self, prop) do
    :evision_nif.colored_kinfu_Params_set_voxelSize(
        Evision.Internal.Structurise.from_struct(self),
        [voxelSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
