defmodule Evision.KinFu do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `KinFu` struct.

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
  def to_struct({:ok, %{class: Evision.KinFu, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.KinFu, ref: ref}) do
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
  makeVolume

  ##### Positional Arguments
  - **volumeType**: `VolumeType`
  - **voxelSize**: `float`
  - **pose**: `Evision.Mat`
  - **raycastStepFactor**: `float`
  - **truncDist**: `float`
  - **maxWeight**: `integer()`
  - **truncateThreshold**: `float`
  - **resolution**: `Vec3i`

  ##### Return
  - **retval**: `Volume`

  Python prototype (for reference only):
  ```python3
  makeVolume(_volumeType, _voxelSize, _pose, _raycastStepFactor, _truncDist, _maxWeight, _truncateThreshold, _resolution) -> retval
  ```
  """
  @spec makeVolume(Evision.KinFu.VolumeType.enum(), number(), Evision.Mat.t(), number(), number(), integer(), number(), {integer(), integer(), integer()}) :: Evision.KinFu.Volume.t() | {:error, String.t()}
  def makeVolume(volumeType, voxelSize, pose, raycastStepFactor, truncDist, maxWeight, truncateThreshold, resolution) when is_integer(volumeType) and is_float(voxelSize) and (is_struct(pose, Evision.Mat) or is_struct(pose, Nx.Tensor) or is_number(pose) or is_tuple(pose)) and is_float(raycastStepFactor) and is_float(truncDist) and is_integer(maxWeight) and is_float(truncateThreshold)
  do
    positional = [
      volumeType: Evision.Internal.Structurise.from_struct(volumeType),
      voxelSize: Evision.Internal.Structurise.from_struct(voxelSize),
      pose: Evision.Internal.Structurise.from_struct(pose),
      raycastStepFactor: Evision.Internal.Structurise.from_struct(raycastStepFactor),
      truncDist: Evision.Internal.Structurise.from_struct(truncDist),
      maxWeight: Evision.Internal.Structurise.from_struct(maxWeight),
      truncateThreshold: Evision.Internal.Structurise.from_struct(truncateThreshold),
      resolution: Evision.Internal.Structurise.from_struct(resolution)
    ]
    :evision_nif.kinfu_makeVolume(positional)
    |> to_struct()
  end
end
