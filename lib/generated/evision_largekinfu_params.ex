defmodule Evision.LargeKinfu.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LargeKinfu.Params` struct.

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
  def to_struct({:ok, %{class: Evision.LargeKinfu.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LargeKinfu.Params, ref: ref}) do
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
  @spec coarseParams() :: Evision.LargeKinfu.Params.t() | {:error, String.t()}
  def coarseParams() do
    positional = [
    ]
    :evision_nif.large_kinfu_large_kinfu_Params_coarseParams_static(positional)
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
  @spec defaultParams() :: Evision.LargeKinfu.Params.t() | {:error, String.t()}
  def defaultParams() do
    positional = [
    ]
    :evision_nif.large_kinfu_large_kinfu_Params_defaultParams_static(positional)
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
  @spec hashTSDFParams(boolean()) :: Evision.LargeKinfu.Params.t() | {:error, String.t()}
  def hashTSDFParams(isCoarse) when is_boolean(isCoarse)
  do
    positional = [
      isCoarse: Evision.Internal.Structurise.from_struct(isCoarse)
    ]
    :evision_nif.large_kinfu_large_kinfu_Params_hashTSDFParams_static(positional)
    |> to_struct()
  end
  @spec get_bilateral_kernel_size(Evision.LargeKinfu.Params.t()) :: integer()
  def get_bilateral_kernel_size(self) do
    :evision_nif.large_kinfu_Params_get_bilateral_kernel_size(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_bilateral_kernel_size(Evision.LargeKinfu.Params.t(), integer()) :: Evision.LargeKinfu.Params.t()
  def set_bilateral_kernel_size(self, prop) do
    :evision_nif.large_kinfu_Params_set_bilateral_kernel_size(
        Evision.Internal.Structurise.from_struct(self),
        [bilateral_kernel_size: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_bilateral_sigma_depth(Evision.LargeKinfu.Params.t()) :: number()
  def get_bilateral_sigma_depth(self) do
    :evision_nif.large_kinfu_Params_get_bilateral_sigma_depth(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_bilateral_sigma_depth(Evision.LargeKinfu.Params.t(), number()) :: Evision.LargeKinfu.Params.t()
  def set_bilateral_sigma_depth(self, prop) do
    :evision_nif.large_kinfu_Params_set_bilateral_sigma_depth(
        Evision.Internal.Structurise.from_struct(self),
        [bilateral_sigma_depth: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_bilateral_sigma_spatial(Evision.LargeKinfu.Params.t()) :: number()
  def get_bilateral_sigma_spatial(self) do
    :evision_nif.large_kinfu_Params_get_bilateral_sigma_spatial(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_bilateral_sigma_spatial(Evision.LargeKinfu.Params.t(), number()) :: Evision.LargeKinfu.Params.t()
  def set_bilateral_sigma_spatial(self, prop) do
    :evision_nif.large_kinfu_Params_set_bilateral_sigma_spatial(
        Evision.Internal.Structurise.from_struct(self),
        [bilateral_sigma_spatial: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_depthFactor(Evision.LargeKinfu.Params.t()) :: number()
  def get_depthFactor(self) do
    :evision_nif.large_kinfu_Params_get_depthFactor(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_depthFactor(Evision.LargeKinfu.Params.t(), number()) :: Evision.LargeKinfu.Params.t()
  def set_depthFactor(self, prop) do
    :evision_nif.large_kinfu_Params_set_depthFactor(
        Evision.Internal.Structurise.from_struct(self),
        [depthFactor: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_frameSize(Evision.LargeKinfu.Params.t()) :: {number(), number()}
  def get_frameSize(self) do
    :evision_nif.large_kinfu_Params_get_frameSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_frameSize(Evision.LargeKinfu.Params.t(), {number(), number()}) :: Evision.LargeKinfu.Params.t()
  def set_frameSize(self, prop) do
    :evision_nif.large_kinfu_Params_set_frameSize(
        Evision.Internal.Structurise.from_struct(self),
        [frameSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_icpAngleThresh(Evision.LargeKinfu.Params.t()) :: number()
  def get_icpAngleThresh(self) do
    :evision_nif.large_kinfu_Params_get_icpAngleThresh(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_icpAngleThresh(Evision.LargeKinfu.Params.t(), number()) :: Evision.LargeKinfu.Params.t()
  def set_icpAngleThresh(self, prop) do
    :evision_nif.large_kinfu_Params_set_icpAngleThresh(
        Evision.Internal.Structurise.from_struct(self),
        [icpAngleThresh: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_icpDistThresh(Evision.LargeKinfu.Params.t()) :: number()
  def get_icpDistThresh(self) do
    :evision_nif.large_kinfu_Params_get_icpDistThresh(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_icpDistThresh(Evision.LargeKinfu.Params.t(), number()) :: Evision.LargeKinfu.Params.t()
  def set_icpDistThresh(self, prop) do
    :evision_nif.large_kinfu_Params_set_icpDistThresh(
        Evision.Internal.Structurise.from_struct(self),
        [icpDistThresh: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_icpIterations(Evision.LargeKinfu.Params.t()) :: list(integer())
  def get_icpIterations(self) do
    :evision_nif.large_kinfu_Params_get_icpIterations(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_icpIterations(Evision.LargeKinfu.Params.t(), list(integer())) :: Evision.LargeKinfu.Params.t()
  def set_icpIterations(self, prop) do
    :evision_nif.large_kinfu_Params_set_icpIterations(
        Evision.Internal.Structurise.from_struct(self),
        [icpIterations: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_intr(Evision.LargeKinfu.Params.t()) :: Evision.Mat.t()
  def get_intr(self) do
    :evision_nif.large_kinfu_Params_get_intr(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_intr(Evision.LargeKinfu.Params.t(), Evision.Mat.t()) :: Evision.LargeKinfu.Params.t()
  def set_intr(self, prop) do
    :evision_nif.large_kinfu_Params_set_intr(
        Evision.Internal.Structurise.from_struct(self),
        [intr: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_lightPose(Evision.LargeKinfu.Params.t()) :: {number(), number(), number()}
  def get_lightPose(self) do
    :evision_nif.large_kinfu_Params_get_lightPose(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_lightPose(Evision.LargeKinfu.Params.t(), {number(), number(), number()}) :: Evision.LargeKinfu.Params.t()
  def set_lightPose(self, prop) do
    :evision_nif.large_kinfu_Params_set_lightPose(
        Evision.Internal.Structurise.from_struct(self),
        [lightPose: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_pyramidLevels(Evision.LargeKinfu.Params.t()) :: integer()
  def get_pyramidLevels(self) do
    :evision_nif.large_kinfu_Params_get_pyramidLevels(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_pyramidLevels(Evision.LargeKinfu.Params.t(), integer()) :: Evision.LargeKinfu.Params.t()
  def set_pyramidLevels(self, prop) do
    :evision_nif.large_kinfu_Params_set_pyramidLevels(
        Evision.Internal.Structurise.from_struct(self),
        [pyramidLevels: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_rgb_intr(Evision.LargeKinfu.Params.t()) :: Evision.Mat.t()
  def get_rgb_intr(self) do
    :evision_nif.large_kinfu_Params_get_rgb_intr(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_rgb_intr(Evision.LargeKinfu.Params.t(), Evision.Mat.t()) :: Evision.LargeKinfu.Params.t()
  def set_rgb_intr(self, prop) do
    :evision_nif.large_kinfu_Params_set_rgb_intr(
        Evision.Internal.Structurise.from_struct(self),
        [rgb_intr: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_truncateThreshold(Evision.LargeKinfu.Params.t()) :: number()
  def get_truncateThreshold(self) do
    :evision_nif.large_kinfu_Params_get_truncateThreshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_truncateThreshold(Evision.LargeKinfu.Params.t(), number()) :: Evision.LargeKinfu.Params.t()
  def set_truncateThreshold(self, prop) do
    :evision_nif.large_kinfu_Params_set_truncateThreshold(
        Evision.Internal.Structurise.from_struct(self),
        [truncateThreshold: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_tsdf_min_camera_movement(Evision.LargeKinfu.Params.t()) :: number()
  def get_tsdf_min_camera_movement(self) do
    :evision_nif.large_kinfu_Params_get_tsdf_min_camera_movement(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_tsdf_min_camera_movement(Evision.LargeKinfu.Params.t(), number()) :: Evision.LargeKinfu.Params.t()
  def set_tsdf_min_camera_movement(self, prop) do
    :evision_nif.large_kinfu_Params_set_tsdf_min_camera_movement(
        Evision.Internal.Structurise.from_struct(self),
        [tsdf_min_camera_movement: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
