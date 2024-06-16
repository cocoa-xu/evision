defmodule Evision.KinFu.KinFu do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `KinFu.KinFu` struct.

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
  def to_struct({:ok, %{class: Evision.KinFu.KinFu, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.KinFu.KinFu, ref: ref}) do
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
  create

  ##### Positional Arguments
  - **params**: `Params`

  ##### Return
  - **retval**: `KinFu`

  Python prototype (for reference only):
  ```python3
  create(_params) -> retval
  ```
  """
  @spec create(Evision.KinFu.Params.t()) :: Evision.KinFu.KinFu.t() | {:error, String.t()}
  def create(params) when is_struct(params, Evision.KinFu.Params)
  do
    positional = [
      params: Evision.Internal.Structurise.from_struct(params)
    ]
    :evision_nif.kinfu_kinfu_KinFu_create_static(positional)
    |> to_struct()
  end

  @doc """
  Gets points and normals of current 3d mesh

  ##### Positional Arguments
  - **self**: `Evision.KinFu.KinFu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.

    vector of points which are 4-float vectors

  - **normals**: `Evision.Mat.t()`.

    vector of normals which are 4-float vectors

  The order of normals corresponds to order of points.
  The order of points is undefined.

  Python prototype (for reference only):
  ```python3
  getCloud([, points[, normals]]) -> points, normals
  ```
  """
  @spec getCloud(Evision.KinFu.t(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getCloud(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.kinfu_kinfu_KinFu_getCloud(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Gets points and normals of current 3d mesh

  ##### Positional Arguments
  - **self**: `Evision.KinFu.KinFu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.

    vector of points which are 4-float vectors

  - **normals**: `Evision.Mat.t()`.

    vector of normals which are 4-float vectors

  The order of normals corresponds to order of points.
  The order of points is undefined.

  Python prototype (for reference only):
  ```python3
  getCloud([, points[, normals]]) -> points, normals
  ```
  """
  @spec getCloud(Evision.KinFu.t()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getCloud(self) do
    positional = [
    ]
    :evision_nif.kinfu_kinfu_KinFu_getCloud(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Calculates normals for given points

  ##### Positional Arguments
  - **self**: `Evision.KinFu.KinFu.t()`
  - **points**: `Evision.Mat`.

    input vector of points which are 4-float vectors

  ##### Return
  - **normals**: `Evision.Mat.t()`.

    output vector of corresponding normals which are 4-float vectors

  Python prototype (for reference only):
  ```python3
  getNormals(points[, normals]) -> normals
  ```
  """
  @spec getNormals(Evision.KinFu.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getNormals(self, points, opts) when (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.kinfu_kinfu_KinFu_getNormals(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Calculates normals for given points

  ##### Positional Arguments
  - **self**: `Evision.KinFu.KinFu.t()`
  - **points**: `Evision.Mat`.

    input vector of points which are 4-float vectors

  ##### Return
  - **normals**: `Evision.Mat.t()`.

    output vector of corresponding normals which are 4-float vectors

  Python prototype (for reference only):
  ```python3
  getNormals(points[, normals]) -> normals
  ```
  """
  @spec getNormals(Evision.KinFu.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def getNormals(self, points) when (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.kinfu_kinfu_KinFu_getNormals(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Gets points of current 3d mesh

  ##### Positional Arguments
  - **self**: `Evision.KinFu.KinFu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.

    vector of points which are 4-float vectors

  The order of points is undefined.

  Python prototype (for reference only):
  ```python3
  getPoints([, points]) -> points
  ```
  """
  @spec getPoints(Evision.KinFu.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getPoints(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.kinfu_kinfu_KinFu_getPoints(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Gets points of current 3d mesh

  ##### Positional Arguments
  - **self**: `Evision.KinFu.KinFu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.

    vector of points which are 4-float vectors

  The order of points is undefined.

  Python prototype (for reference only):
  ```python3
  getPoints([, points]) -> points
  ```
  """
  @spec getPoints(Evision.KinFu.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getPoints(self) do
    positional = [
    ]
    :evision_nif.kinfu_kinfu_KinFu_getPoints(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Renders a volume into an image

  ##### Positional Arguments
  - **self**: `Evision.KinFu.KinFu.t()`
  - **cameraPose**: `Evision.Mat`.

    pose of camera to render from. If empty then render from current pose
    which is a last frame camera pose.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    resulting image

  Renders a 0-surface of TSDF using Phong shading into a CV_8UC4 Mat.
  Light pose is fixed in KinFu params.

  Python prototype (for reference only):
  ```python3
  render(cameraPose[, image]) -> image
  ```
  """
  @spec render(Evision.KinFu.t(), Evision.Mat.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def render(self, cameraPose, opts) when (is_struct(cameraPose, Evision.Mat) or is_struct(cameraPose, Nx.Tensor) or is_number(cameraPose) or is_tuple(cameraPose)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      cameraPose: Evision.Internal.Structurise.from_struct(cameraPose)
    ]
    :evision_nif.kinfu_kinfu_KinFu_render(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Renders a volume into an image

  ##### Positional Arguments
  - **self**: `Evision.KinFu.KinFu.t()`
  - **cameraPose**: `Evision.Mat`.

    pose of camera to render from. If empty then render from current pose
    which is a last frame camera pose.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    resulting image

  Renders a 0-surface of TSDF using Phong shading into a CV_8UC4 Mat.
  Light pose is fixed in KinFu params.

  Python prototype (for reference only):
  ```python3
  render(cameraPose[, image]) -> image
  ```
  #### Variant 2:
  Renders a volume into an image

  ##### Positional Arguments
  - **self**: `Evision.KinFu.KinFu.t()`

  ##### Return
  - **image**: `Evision.Mat.t()`.

    resulting image

  Renders a 0-surface of TSDF using Phong shading into a CV_8UC4 Mat.
  Light pose is fixed in KinFu params.

  Python prototype (for reference only):
  ```python3
  render([, image]) -> image
  ```

  """
  @spec render(Evision.KinFu.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def render(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.kinfu_kinfu_KinFu_render(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec render(Evision.KinFu.t(), Evision.Mat.t()) :: Evision.Mat.t() | {:error, String.t()}
  def render(self, cameraPose) when (is_struct(cameraPose, Evision.Mat) or is_struct(cameraPose, Nx.Tensor) or is_number(cameraPose) or is_tuple(cameraPose))
  do
    positional = [
      cameraPose: Evision.Internal.Structurise.from_struct(cameraPose)
    ]
    :evision_nif.kinfu_kinfu_KinFu_render(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Renders a volume into an image

  ##### Positional Arguments
  - **self**: `Evision.KinFu.KinFu.t()`

  ##### Return
  - **image**: `Evision.Mat.t()`.

    resulting image

  Renders a 0-surface of TSDF using Phong shading into a CV_8UC4 Mat.
  Light pose is fixed in KinFu params.

  Python prototype (for reference only):
  ```python3
  render([, image]) -> image
  ```
  """
  @spec render(Evision.KinFu.t()) :: Evision.Mat.t() | {:error, String.t()}
  def render(self) do
    positional = [
    ]
    :evision_nif.kinfu_kinfu_KinFu_render(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Resets the algorithm

  ##### Positional Arguments
  - **self**: `Evision.KinFu.KinFu.t()`

  Clears current model and resets a pose.

  Python prototype (for reference only):
  ```python3
  reset() -> None
  ```
  """
  @spec reset(Evision.KinFu.t()) :: Evision.KinFu.t() | {:error, String.t()}
  def reset(self) do
    positional = [
    ]
    :evision_nif.kinfu_kinfu_KinFu_reset(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Process next depth frame

  ##### Positional Arguments
  - **self**: `Evision.KinFu.KinFu.t()`
  - **depth**: `Evision.Mat`.

    one-channel image which size and depth scale is described in algorithm's parameters

  ##### Return
  - **retval**: `bool`

  Integrates depth into voxel space with respect to its ICP-calculated pose.
  Input image is converted to CV_32F internally if has another type.
  @return true if succeeded to align new frame with current scene, false if opposite

  Python prototype (for reference only):
  ```python3
  update(depth) -> retval
  ```
  """
  @spec update(Evision.KinFu.t(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def update(self, depth) when (is_struct(depth, Evision.Mat) or is_struct(depth, Nx.Tensor) or is_number(depth) or is_tuple(depth))
  do
    positional = [
      depth: Evision.Internal.Structurise.from_struct(depth)
    ]
    :evision_nif.kinfu_kinfu_KinFu_update(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
