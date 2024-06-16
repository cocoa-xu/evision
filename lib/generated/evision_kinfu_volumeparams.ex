defmodule Evision.KinFu.VolumeParams do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `KinFu.VolumeParams` struct.

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
  def to_struct({:ok, %{class: Evision.KinFu.VolumeParams, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.KinFu.VolumeParams, ref: ref}) do
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
  Coarse set of parameters that provides relatively higher performance
  at the cost of reconstrution quality.

  ##### Positional Arguments
  - **volumeType**: `VolumeType`

  ##### Return
  - **retval**: `VolumeParams`

  Python prototype (for reference only):
  ```python3
  coarseParams(_volumeType) -> retval
  ```
  """
  @spec coarseParams(Evision.KinFu.VolumeType.enum()) :: Evision.KinFu.VolumeParams.t() | {:error, String.t()}
  def coarseParams(volumeType) when is_integer(volumeType)
  do
    positional = [
      volumeType: Evision.Internal.Structurise.from_struct(volumeType)
    ]
    :evision_nif.kinfu_kinfu_VolumeParams_coarseParams_static(positional)
    |> to_struct()
  end

  @doc """
  Default set of parameters that provide higher quality reconstruction
  at the cost of slow performance.

  ##### Positional Arguments
  - **volumeType**: `VolumeType`

  ##### Return
  - **retval**: `VolumeParams`

  Python prototype (for reference only):
  ```python3
  defaultParams(_volumeType) -> retval
  ```
  """
  @spec defaultParams(Evision.KinFu.VolumeType.enum()) :: Evision.KinFu.VolumeParams.t() | {:error, String.t()}
  def defaultParams(volumeType) when is_integer(volumeType)
  do
    positional = [
      volumeType: Evision.Internal.Structurise.from_struct(volumeType)
    ]
    :evision_nif.kinfu_kinfu_VolumeParams_defaultParams_static(positional)
    |> to_struct()
  end
  @spec get_depthTruncThreshold(Evision.KinFu.VolumeParams.t()) :: number()
  def get_depthTruncThreshold(self) do
    :evision_nif.kinfu_VolumeParams_get_depthTruncThreshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_depthTruncThreshold(Evision.KinFu.VolumeParams.t(), number()) :: Evision.KinFu.VolumeParams.t()
  def set_depthTruncThreshold(self, prop) do
    :evision_nif.kinfu_VolumeParams_set_depthTruncThreshold(
        Evision.Internal.Structurise.from_struct(self),
        [depthTruncThreshold: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxWeight(Evision.KinFu.VolumeParams.t()) :: integer()
  def get_maxWeight(self) do
    :evision_nif.kinfu_VolumeParams_get_maxWeight(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxWeight(Evision.KinFu.VolumeParams.t(), integer()) :: Evision.KinFu.VolumeParams.t()
  def set_maxWeight(self, prop) do
    :evision_nif.kinfu_VolumeParams_set_maxWeight(
        Evision.Internal.Structurise.from_struct(self),
        [maxWeight: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_raycastStepFactor(Evision.KinFu.VolumeParams.t()) :: number()
  def get_raycastStepFactor(self) do
    :evision_nif.kinfu_VolumeParams_get_raycastStepFactor(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_raycastStepFactor(Evision.KinFu.VolumeParams.t(), number()) :: Evision.KinFu.VolumeParams.t()
  def set_raycastStepFactor(self, prop) do
    :evision_nif.kinfu_VolumeParams_set_raycastStepFactor(
        Evision.Internal.Structurise.from_struct(self),
        [raycastStepFactor: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_resolution(Evision.KinFu.VolumeParams.t()) :: {integer(), integer(), integer()}
  def get_resolution(self) do
    :evision_nif.kinfu_VolumeParams_get_resolution(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_resolution(Evision.KinFu.VolumeParams.t(), {integer(), integer(), integer()}) :: Evision.KinFu.VolumeParams.t()
  def set_resolution(self, prop) do
    :evision_nif.kinfu_VolumeParams_set_resolution(
        Evision.Internal.Structurise.from_struct(self),
        [resolution: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_tsdfTruncDist(Evision.KinFu.VolumeParams.t()) :: number()
  def get_tsdfTruncDist(self) do
    :evision_nif.kinfu_VolumeParams_get_tsdfTruncDist(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_tsdfTruncDist(Evision.KinFu.VolumeParams.t(), number()) :: Evision.KinFu.VolumeParams.t()
  def set_tsdfTruncDist(self, prop) do
    :evision_nif.kinfu_VolumeParams_set_tsdfTruncDist(
        Evision.Internal.Structurise.from_struct(self),
        [tsdfTruncDist: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_type(Evision.KinFu.VolumeParams.t()) :: Evision.KinFu.VolumeType.enum()
  def get_type(self) do
    :evision_nif.kinfu_VolumeParams_get_type(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_type(Evision.KinFu.VolumeParams.t(), Evision.KinFu.VolumeType.enum()) :: Evision.KinFu.VolumeParams.t()
  def set_type(self, prop) do
    :evision_nif.kinfu_VolumeParams_set_type(
        Evision.Internal.Structurise.from_struct(self),
        [type: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_voxelSize(Evision.KinFu.VolumeParams.t()) :: number()
  def get_voxelSize(self) do
    :evision_nif.kinfu_VolumeParams_get_voxelSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_voxelSize(Evision.KinFu.VolumeParams.t(), number()) :: Evision.KinFu.VolumeParams.t()
  def set_voxelSize(self, prop) do
    :evision_nif.kinfu_VolumeParams_set_voxelSize(
        Evision.Internal.Structurise.from_struct(self),
        [voxelSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
