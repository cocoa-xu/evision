defmodule Evision.PPFMatch3D.Pose3D do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `PPFMatch3D.Pose3D` struct.

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
  def to_struct({:ok, %{class: Evision.PPFMatch3D.Pose3D, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.PPFMatch3D.Pose3D, ref: ref}) do
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
  Pose3D

  ##### Positional Arguments
  - **alpha**: `double`

  ##### Keyword Arguments
  - **modelIndex**: `size_t`.
  - **numVotes**: `size_t`.

  ##### Return
  - **self**: `Evision.PPFMatch3D.Pose3D.t()`

  Python prototype (for reference only):
  ```python3
  Pose3D(Alpha[, ModelIndex[, NumVotes]]) -> <ppf_match_3d_Pose3D object>
  ```
  """
  @spec pose3D(number(), [{:modelIndex, term()} | {:numVotes, term()}] | nil) :: Evision.PPFMatch3D.Pose3D.t() | {:error, String.t()}
  def pose3D(alpha, opts) when is_number(alpha) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:modelIndex, :numVotes])
    positional = [
      alpha: Evision.Internal.Structurise.from_struct(alpha)
    ]
    :evision_nif.ppf_match_3d_ppf_match_3d_Pose3D_Pose3D(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Pose3D

  ##### Positional Arguments
  - **alpha**: `double`

  ##### Keyword Arguments
  - **modelIndex**: `size_t`.
  - **numVotes**: `size_t`.

  ##### Return
  - **self**: `Evision.PPFMatch3D.Pose3D.t()`

  Python prototype (for reference only):
  ```python3
  Pose3D(Alpha[, ModelIndex[, NumVotes]]) -> <ppf_match_3d_Pose3D object>
  ```
  """
  @spec pose3D(number()) :: Evision.PPFMatch3D.Pose3D.t() | {:error, String.t()}
  def pose3D(alpha) when is_number(alpha)
  do
    positional = [
      alpha: Evision.Internal.Structurise.from_struct(alpha)
    ]
    :evision_nif.ppf_match_3d_ppf_match_3d_Pose3D_Pose3D(positional)
    |> to_struct()
  end

  @doc """
  Pose3D
  ##### Return
  - **self**: `Evision.PPFMatch3D.Pose3D.t()`

  Python prototype (for reference only):
  ```python3
  Pose3D() -> <ppf_match_3d_Pose3D object>
  ```
  """
  @spec pose3D() :: Evision.PPFMatch3D.Pose3D.t() | {:error, String.t()}
  def pose3D() do
    positional = [
    ]
    :evision_nif.ppf_match_3d_ppf_match_3d_Pose3D_Pose3D(positional)
    |> to_struct()
  end

  @doc """
  appendPose

  ##### Positional Arguments
  - **self**: `Evision.PPFMatch3D.Pose3D.t()`
  - **incrementalPose**: `Evision.Mat`

    \\brief Left multiplies the existing pose in order to update the transformation
    \\param [in] IncrementalPose New pose to apply

  Python prototype (for reference only):
  ```python3
  appendPose(IncrementalPose) -> None
  ```
  """
  @spec appendPose(Evision.PPFMatch3D.Pose3D.t(), Evision.Mat.t()) :: Evision.PPFMatch3D.Pose3D.t() | {:error, String.t()}
  def appendPose(self, incrementalPose) when (is_struct(incrementalPose, Evision.Mat) or is_struct(incrementalPose, Nx.Tensor) or is_number(incrementalPose) or is_tuple(incrementalPose))
  do
    positional = [
      incrementalPose: Evision.Internal.Structurise.from_struct(incrementalPose)
    ]
    :evision_nif.ppf_match_3d_ppf_match_3d_Pose3D_appendPose(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  printPose

  ##### Positional Arguments
  - **self**: `Evision.PPFMatch3D.Pose3D.t()`

  Python prototype (for reference only):
  ```python3
  printPose() -> None
  ```
  """
  @spec printPose(Evision.PPFMatch3D.Pose3D.t()) :: Evision.PPFMatch3D.Pose3D.t() | {:error, String.t()}
  def printPose(self) do
    positional = [
    ]
    :evision_nif.ppf_match_3d_ppf_match_3d_Pose3D_printPose(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  updatePose

  ##### Positional Arguments
  - **self**: `Evision.PPFMatch3D.Pose3D.t()`
  - **newR**: `Evision.Mat`
  - **newT**: `Vec3d`

    \\brief Updates the pose with the new one

  Python prototype (for reference only):
  ```python3
  updatePose(NewR, NewT) -> None
  ```
  """
  @spec updatePose(Evision.PPFMatch3D.Pose3D.t(), Evision.Mat.t(), {number(), number(), number()}) :: Evision.PPFMatch3D.Pose3D.t() | {:error, String.t()}
  def updatePose(self, newR, newT) when (is_struct(newR, Evision.Mat) or is_struct(newR, Nx.Tensor) or is_number(newR) or is_tuple(newR))
  do
    positional = [
      newR: Evision.Internal.Structurise.from_struct(newR),
      newT: Evision.Internal.Structurise.from_struct(newT)
    ]
    :evision_nif.ppf_match_3d_ppf_match_3d_Pose3D_updatePose(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  updatePose

  ##### Positional Arguments
  - **self**: `Evision.PPFMatch3D.Pose3D.t()`
  - **newPose**: `Evision.Mat`

    \\brief Updates the pose with the new one
    \\param [in] NewPose New pose to overwrite

  Python prototype (for reference only):
  ```python3
  updatePose(NewPose) -> None
  ```
  """
  @spec updatePose(Evision.PPFMatch3D.Pose3D.t(), Evision.Mat.t()) :: Evision.PPFMatch3D.Pose3D.t() | {:error, String.t()}
  def updatePose(self, newPose) when (is_struct(newPose, Evision.Mat) or is_struct(newPose, Nx.Tensor) or is_number(newPose) or is_tuple(newPose))
  do
    positional = [
      newPose: Evision.Internal.Structurise.from_struct(newPose)
    ]
    :evision_nif.ppf_match_3d_ppf_match_3d_Pose3D_updatePose(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  updatePoseQuat

  ##### Positional Arguments
  - **self**: `Evision.PPFMatch3D.Pose3D.t()`
  - **q**: `Vec4d`
  - **newT**: `Vec3d`

    \\brief Updates the pose with the new one, but this time using quaternions to represent rotation

  Python prototype (for reference only):
  ```python3
  updatePoseQuat(Q, NewT) -> None
  ```
  """
  @spec updatePoseQuat(Evision.PPFMatch3D.Pose3D.t(), {number(), number(), number(), number()}, {number(), number(), number()}) :: Evision.PPFMatch3D.Pose3D.t() | {:error, String.t()}
  def updatePoseQuat(self, q, newT) do
    positional = [
      q: Evision.Internal.Structurise.from_struct(q),
      newT: Evision.Internal.Structurise.from_struct(newT)
    ]
    :evision_nif.ppf_match_3d_ppf_match_3d_Pose3D_updatePoseQuat(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_alpha(Evision.PPFMatch3D.Pose3D.t()) :: number()
  def get_alpha(self) do
    :evision_nif.ppf_match_3d_Pose3D_get_alpha(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_angle(Evision.PPFMatch3D.Pose3D.t()) :: number()
  def get_angle(self) do
    :evision_nif.ppf_match_3d_Pose3D_get_angle(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_modelIndex(Evision.PPFMatch3D.Pose3D.t()) :: integer()
  def get_modelIndex(self) do
    :evision_nif.ppf_match_3d_Pose3D_get_modelIndex(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_numVotes(Evision.PPFMatch3D.Pose3D.t()) :: integer()
  def get_numVotes(self) do
    :evision_nif.ppf_match_3d_Pose3D_get_numVotes(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_pose(Evision.PPFMatch3D.Pose3D.t()) :: Evision.Mat.t()
  def get_pose(self) do
    :evision_nif.ppf_match_3d_Pose3D_get_pose(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_q(Evision.PPFMatch3D.Pose3D.t()) :: {number(), number(), number(), number()}
  def get_q(self) do
    :evision_nif.ppf_match_3d_Pose3D_get_q(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_residual(Evision.PPFMatch3D.Pose3D.t()) :: number()
  def get_residual(self) do
    :evision_nif.ppf_match_3d_Pose3D_get_residual(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_t(Evision.PPFMatch3D.Pose3D.t()) :: {number(), number(), number()}
  def get_t(self) do
    :evision_nif.ppf_match_3d_Pose3D_get_t(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
end
