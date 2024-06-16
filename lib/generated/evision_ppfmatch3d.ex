defmodule Evision.PPFMatch3D do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `PPFMatch3D` struct.

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
  def to_struct({:ok, %{class: Evision.PPFMatch3D, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.PPFMatch3D, ref: ref}) do
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
  addNoisePC

  ##### Positional Arguments
  - **pc**: `Evision.Mat`
  - **scale**: `double`

  ##### Return
  - **retval**: `Evision.Mat.t()`

    Adds a uniform noise in the given scale to the input point cloud

  Python prototype (for reference only):
  ```python3
  addNoisePC(pc, scale) -> retval
  ```
  """
  @spec addNoisePC(Evision.Mat.maybe_mat_in(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def addNoisePC(pc, scale) when (is_struct(pc, Evision.Mat) or is_struct(pc, Nx.Tensor) or is_number(pc) or is_tuple(pc)) and is_number(scale)
  do
    positional = [
      pc: Evision.Internal.Structurise.from_struct(pc),
      scale: Evision.Internal.Structurise.from_struct(scale)
    ]
    :evision_nif.ppf_match_3d_addNoisePC(positional)
    |> to_struct()
  end

  @doc """
  Compute the normals of an arbitrary point cloud
  computeNormalsPC3d uses a plane fitting approach to smoothly compute
  local normals. Normals are obtained through the eigenvector of the covariance
  matrix, corresponding to the smallest eigen value.
  If PCNormals is provided to be an Nx6 matrix, then no new allocation
  is made, instead the existing memory is overwritten.

  ##### Positional Arguments
  - **pC**: `Evision.Mat`
  - **numNeighbors**: `integer()`
  - **flipViewpoint**: `bool`
  - **viewpoint**: `Vec3f`

  ##### Return
  - **retval**: `integer()`
  - **pCNormals**: `Evision.Mat.t()`.

  @return Returns 0 on success

  Python prototype (for reference only):
  ```python3
  computeNormalsPC3d(PC, NumNeighbors, FlipViewpoint, viewpoint[, PCNormals]) -> retval, PCNormals
  ```
  """
  @spec computeNormalsPC3d(Evision.Mat.maybe_mat_in(), integer(), boolean(), {number(), number(), number()}, [{atom(), term()},...] | nil) :: {integer(), Evision.Mat.t()} | {:error, String.t()}
  def computeNormalsPC3d(pC, numNeighbors, flipViewpoint, viewpoint, opts) when (is_struct(pC, Evision.Mat) or is_struct(pC, Nx.Tensor) or is_number(pC) or is_tuple(pC)) and is_integer(numNeighbors) and is_boolean(flipViewpoint) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      pC: Evision.Internal.Structurise.from_struct(pC),
      numNeighbors: Evision.Internal.Structurise.from_struct(numNeighbors),
      flipViewpoint: Evision.Internal.Structurise.from_struct(flipViewpoint),
      viewpoint: Evision.Internal.Structurise.from_struct(viewpoint)
    ]
    :evision_nif.ppf_match_3d_computeNormalsPC3d(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Compute the normals of an arbitrary point cloud
  computeNormalsPC3d uses a plane fitting approach to smoothly compute
  local normals. Normals are obtained through the eigenvector of the covariance
  matrix, corresponding to the smallest eigen value.
  If PCNormals is provided to be an Nx6 matrix, then no new allocation
  is made, instead the existing memory is overwritten.

  ##### Positional Arguments
  - **pC**: `Evision.Mat`
  - **numNeighbors**: `integer()`
  - **flipViewpoint**: `bool`
  - **viewpoint**: `Vec3f`

  ##### Return
  - **retval**: `integer()`
  - **pCNormals**: `Evision.Mat.t()`.

  @return Returns 0 on success

  Python prototype (for reference only):
  ```python3
  computeNormalsPC3d(PC, NumNeighbors, FlipViewpoint, viewpoint[, PCNormals]) -> retval, PCNormals
  ```
  """
  @spec computeNormalsPC3d(Evision.Mat.maybe_mat_in(), integer(), boolean(), {number(), number(), number()}) :: {integer(), Evision.Mat.t()} | {:error, String.t()}
  def computeNormalsPC3d(pC, numNeighbors, flipViewpoint, viewpoint) when (is_struct(pC, Evision.Mat) or is_struct(pC, Nx.Tensor) or is_number(pC) or is_tuple(pC)) and is_integer(numNeighbors) and is_boolean(flipViewpoint)
  do
    positional = [
      pC: Evision.Internal.Structurise.from_struct(pC),
      numNeighbors: Evision.Internal.Structurise.from_struct(numNeighbors),
      flipViewpoint: Evision.Internal.Structurise.from_struct(flipViewpoint),
      viewpoint: Evision.Internal.Structurise.from_struct(viewpoint)
    ]
    :evision_nif.ppf_match_3d_computeNormalsPC3d(positional)
    |> to_struct()
  end

  @doc """
  getRandomPose

  ##### Positional Arguments
  - **pose**: `Evision.Mat`

    Generate a random 4x4 pose matrix

  Python prototype (for reference only):
  ```python3
  getRandomPose(Pose) -> None
  ```
  """
  @spec getRandomPose(Evision.Mat.t()) :: :ok | {:error, String.t()}
  def getRandomPose(pose) when (is_struct(pose, Evision.Mat) or is_struct(pose, Nx.Tensor) or is_number(pose) or is_tuple(pose))
  do
    positional = [
      pose: Evision.Internal.Structurise.from_struct(pose)
    ]
    :evision_nif.ppf_match_3d_getRandomPose(positional)
    |> to_struct()
  end

  @doc """
  Load a PLY file

  ##### Positional Arguments
  - **fileName**: `c_string`

  ##### Keyword Arguments
  - **withNormals**: `integer()`.

  ##### Return
  - **retval**: `Evision.Mat.t()`

  @return Returns the matrix on successful load

  Python prototype (for reference only):
  ```python3
  loadPLYSimple(fileName[, withNormals]) -> retval
  ```
  """
  @spec loadPLYSimple(binary(), [{:withNormals, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def loadPLYSimple(fileName, opts) when is_binary(fileName) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:withNormals])
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.ppf_match_3d_loadPLYSimple(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Load a PLY file

  ##### Positional Arguments
  - **fileName**: `c_string`

  ##### Keyword Arguments
  - **withNormals**: `integer()`.

  ##### Return
  - **retval**: `Evision.Mat.t()`

  @return Returns the matrix on successful load

  Python prototype (for reference only):
  ```python3
  loadPLYSimple(fileName[, withNormals]) -> retval
  ```
  """
  @spec loadPLYSimple(binary()) :: Evision.Mat.t() | {:error, String.t()}
  def loadPLYSimple(fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.ppf_match_3d_loadPLYSimple(positional)
    |> to_struct()
  end

  @doc """
  samplePCByQuantization

  ##### Positional Arguments
  - **pc**: `Evision.Mat`
  - **xrange**: `Vec2f`
  - **yrange**: `Vec2f`
  - **zrange**: `Vec2f`
  - **sample_step_relative**: `float`

  ##### Keyword Arguments
  - **weightByCenter**: `integer()`.

  ##### Return
  - **retval**: `Evision.Mat.t()`

    Sample a point cloud using uniform steps
  @return Sampled point cloud

  Python prototype (for reference only):
  ```python3
  samplePCByQuantization(pc, xrange, yrange, zrange, sample_step_relative[, weightByCenter]) -> retval
  ```
  """
  @spec samplePCByQuantization(Evision.Mat.maybe_mat_in(), {number(), number()}, {number(), number()}, {number(), number()}, number(), [{:weightByCenter, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def samplePCByQuantization(pc, xrange, yrange, zrange, sample_step_relative, opts) when (is_struct(pc, Evision.Mat) or is_struct(pc, Nx.Tensor) or is_number(pc) or is_tuple(pc)) and is_float(sample_step_relative) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:weightByCenter])
    positional = [
      pc: Evision.Internal.Structurise.from_struct(pc),
      xrange: Evision.Internal.Structurise.from_struct(xrange),
      yrange: Evision.Internal.Structurise.from_struct(yrange),
      zrange: Evision.Internal.Structurise.from_struct(zrange),
      sample_step_relative: Evision.Internal.Structurise.from_struct(sample_step_relative)
    ]
    :evision_nif.ppf_match_3d_samplePCByQuantization(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  samplePCByQuantization

  ##### Positional Arguments
  - **pc**: `Evision.Mat`
  - **xrange**: `Vec2f`
  - **yrange**: `Vec2f`
  - **zrange**: `Vec2f`
  - **sample_step_relative**: `float`

  ##### Keyword Arguments
  - **weightByCenter**: `integer()`.

  ##### Return
  - **retval**: `Evision.Mat.t()`

    Sample a point cloud using uniform steps
  @return Sampled point cloud

  Python prototype (for reference only):
  ```python3
  samplePCByQuantization(pc, xrange, yrange, zrange, sample_step_relative[, weightByCenter]) -> retval
  ```
  """
  @spec samplePCByQuantization(Evision.Mat.maybe_mat_in(), {number(), number()}, {number(), number()}, {number(), number()}, number()) :: Evision.Mat.t() | {:error, String.t()}
  def samplePCByQuantization(pc, xrange, yrange, zrange, sample_step_relative) when (is_struct(pc, Evision.Mat) or is_struct(pc, Nx.Tensor) or is_number(pc) or is_tuple(pc)) and is_float(sample_step_relative)
  do
    positional = [
      pc: Evision.Internal.Structurise.from_struct(pc),
      xrange: Evision.Internal.Structurise.from_struct(xrange),
      yrange: Evision.Internal.Structurise.from_struct(yrange),
      zrange: Evision.Internal.Structurise.from_struct(zrange),
      sample_step_relative: Evision.Internal.Structurise.from_struct(sample_step_relative)
    ]
    :evision_nif.ppf_match_3d_samplePCByQuantization(positional)
    |> to_struct()
  end

  @doc """
  transformPCPose

  ##### Positional Arguments
  - **pc**: `Evision.Mat`
  - **pose**: `Evision.Mat`

  ##### Return
  - **retval**: `Evision.Mat.t()`

    Transforms the point cloud with a given a homogeneous 4x4 pose matrix (in double precision)
  @return Transformed point cloud

  Python prototype (for reference only):
  ```python3
  transformPCPose(pc, Pose) -> retval
  ```
  """
  @spec transformPCPose(Evision.Mat.maybe_mat_in(), Evision.Mat.t()) :: Evision.Mat.t() | {:error, String.t()}
  def transformPCPose(pc, pose) when (is_struct(pc, Evision.Mat) or is_struct(pc, Nx.Tensor) or is_number(pc) or is_tuple(pc)) and (is_struct(pose, Evision.Mat) or is_struct(pose, Nx.Tensor) or is_number(pose) or is_tuple(pose))
  do
    positional = [
      pc: Evision.Internal.Structurise.from_struct(pc),
      pose: Evision.Internal.Structurise.from_struct(pose)
    ]
    :evision_nif.ppf_match_3d_transformPCPose(positional)
    |> to_struct()
  end

  @doc """
  Write a point cloud to PLY file

  ##### Positional Arguments
  - **pC**: `Evision.Mat`
  - **fileName**: `c_string`

  Python prototype (for reference only):
  ```python3
  writePLY(PC, fileName) -> None
  ```
  """
  @spec writePLY(Evision.Mat.maybe_mat_in(), binary()) :: :ok | {:error, String.t()}
  def writePLY(pC, fileName) when (is_struct(pC, Evision.Mat) or is_struct(pC, Nx.Tensor) or is_number(pC) or is_tuple(pC)) and is_binary(fileName)
  do
    positional = [
      pC: Evision.Internal.Structurise.from_struct(pC),
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.ppf_match_3d_writePLY(positional)
    |> to_struct()
  end

  @doc """
  Used for debbuging pruposes, writes a point cloud to a PLY file with the tip
  of the normal vectors as visible red points

  ##### Positional Arguments
  - **pC**: `Evision.Mat`
  - **fileName**: `c_string`

  Python prototype (for reference only):
  ```python3
  writePLYVisibleNormals(PC, fileName) -> None
  ```
  """
  @spec writePLYVisibleNormals(Evision.Mat.maybe_mat_in(), binary()) :: :ok | {:error, String.t()}
  def writePLYVisibleNormals(pC, fileName) when (is_struct(pC, Evision.Mat) or is_struct(pC, Nx.Tensor) or is_number(pC) or is_tuple(pC)) and is_binary(fileName)
  do
    positional = [
      pC: Evision.Internal.Structurise.from_struct(pC),
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.ppf_match_3d_writePLYVisibleNormals(positional)
    |> to_struct()
  end
end
