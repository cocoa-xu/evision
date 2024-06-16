defmodule Evision.PPFMatch3D.ICP do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `PPFMatch3D.ICP` struct.

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
  def to_struct({:ok, %{class: Evision.PPFMatch3D.ICP, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.PPFMatch3D.ICP, ref: ref}) do
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
  ICP

  ##### Positional Arguments
  - **iterations**: `integer()`

  ##### Keyword Arguments
  - **tolerence**: `float`.
  - **rejectionScale**: `float`.
  - **numLevels**: `integer()`.
  - **sampleType**: `integer()`.
  - **numMaxCorr**: `integer()`.

  ##### Return
  - **self**: `ICP`

    \\brief ICP constructor with default arguments.

  Python prototype (for reference only):
  ```python3
  ICP(iterations[, tolerence[, rejectionScale[, numLevels[, sampleType[, numMaxCorr]]]]]) -> <ppf_match_3d_ICP object>
  ```
  """
  @spec iCP(integer(), [{:numLevels, term()} | {:numMaxCorr, term()} | {:rejectionScale, term()} | {:sampleType, term()} | {:tolerence, term()}] | nil) :: Evision.PPFMatch3D.ICP.t() | {:error, String.t()}
  def iCP(iterations, opts) when is_integer(iterations) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:numLevels, :numMaxCorr, :rejectionScale, :sampleType, :tolerence])
    positional = [
      iterations: Evision.Internal.Structurise.from_struct(iterations)
    ]
    :evision_nif.ppf_match_3d_ppf_match_3d_ICP_ICP(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  ICP

  ##### Positional Arguments
  - **iterations**: `integer()`

  ##### Keyword Arguments
  - **tolerence**: `float`.
  - **rejectionScale**: `float`.
  - **numLevels**: `integer()`.
  - **sampleType**: `integer()`.
  - **numMaxCorr**: `integer()`.

  ##### Return
  - **self**: `ICP`

    \\brief ICP constructor with default arguments.

  Python prototype (for reference only):
  ```python3
  ICP(iterations[, tolerence[, rejectionScale[, numLevels[, sampleType[, numMaxCorr]]]]]) -> <ppf_match_3d_ICP object>
  ```
  """
  @spec iCP(integer()) :: Evision.PPFMatch3D.ICP.t() | {:error, String.t()}
  def iCP(iterations) when is_integer(iterations)
  do
    positional = [
      iterations: Evision.Internal.Structurise.from_struct(iterations)
    ]
    :evision_nif.ppf_match_3d_ppf_match_3d_ICP_ICP(positional)
    |> to_struct()
  end

  @doc """
  ICP
  ##### Return
  - **self**: `ICP`

  Python prototype (for reference only):
  ```python3
  ICP() -> <ppf_match_3d_ICP object>
  ```
  """
  @spec iCP() :: Evision.PPFMatch3D.ICP.t() | {:error, String.t()}
  def iCP() do
    positional = [
    ]
    :evision_nif.ppf_match_3d_ppf_match_3d_ICP_ICP(positional)
    |> to_struct()
  end

  @doc """
  registerModelToScene

  ##### Positional Arguments
  - **self**: `Evision.PPFMatch3D.ICP.t()`
  - **srcPC**: `Evision.Mat`
  - **dstPC**: `Evision.Mat`

  ##### Return
  - **retval**: `integer()`
  - **poses**: `[Evision.PPFMatch3D.Pose3D]`

    \\brief Perform registration with multiple initial poses

    \\details It is assumed that the model is registered on the scene. Scene remains static, while the model transforms. The output poses transform the models onto the scene. Because of the point to plane minimization, the scene is expected to have the normals available. Expected to have the normals (Nx6).

  Python prototype (for reference only):
  ```python3
  registerModelToScene(srcPC, dstPC, poses) -> retval, poses
  ```
  """
  @spec registerModelToScene(Evision.PPFMatch3D.ICP.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), list(Evision.PPFMatch3D.Pose3D.t())) :: {integer(), list(Evision.PPFMatch3D.Pose3D.t())} | {:error, String.t()}
  def registerModelToScene(self, srcPC, dstPC, poses) when (is_struct(srcPC, Evision.Mat) or is_struct(srcPC, Nx.Tensor) or is_number(srcPC) or is_tuple(srcPC)) and (is_struct(dstPC, Evision.Mat) or is_struct(dstPC, Nx.Tensor) or is_number(dstPC) or is_tuple(dstPC)) and is_list(poses)
  do
    positional = [
      srcPC: Evision.Internal.Structurise.from_struct(srcPC),
      dstPC: Evision.Internal.Structurise.from_struct(dstPC),
      poses: Evision.Internal.Structurise.from_struct(poses)
    ]
    :evision_nif.ppf_match_3d_ppf_match_3d_ICP_registerModelToScene(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  registerModelToScene

  ##### Positional Arguments
  - **self**: `Evision.PPFMatch3D.ICP.t()`
  - **srcPC**: `Evision.Mat`
  - **dstPC**: `Evision.Mat`

  ##### Return
  - **retval**: `integer()`
  - **residual**: `double`
  - **pose**: `Evision.Mat.t()`

    \\brief Perform registration

    \\details It is assumed that the model is registered on the scene. Scene remains static, while the model transforms. The output poses transform the models onto the scene. Because of the point to plane minimization, the scene is expected to have the normals available. Expected to have the normals (Nx6).

  Python prototype (for reference only):
  ```python3
  registerModelToScene(srcPC, dstPC) -> retval, residual, pose
  ```
  """
  @spec registerModelToScene(Evision.PPFMatch3D.ICP.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {integer(), number(), Evision.Mat.t()} | {:error, String.t()}
  def registerModelToScene(self, srcPC, dstPC) when (is_struct(srcPC, Evision.Mat) or is_struct(srcPC, Nx.Tensor) or is_number(srcPC) or is_tuple(srcPC)) and (is_struct(dstPC, Evision.Mat) or is_struct(dstPC, Nx.Tensor) or is_number(dstPC) or is_tuple(dstPC))
  do
    positional = [
      srcPC: Evision.Internal.Structurise.from_struct(srcPC),
      dstPC: Evision.Internal.Structurise.from_struct(dstPC)
    ]
    :evision_nif.ppf_match_3d_ppf_match_3d_ICP_registerModelToScene(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
