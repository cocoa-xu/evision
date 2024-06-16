defmodule Evision.DynaFu.DynaFu do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `DynaFu.DynaFu` struct.

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
  def to_struct({:ok, %{class: Evision.DynaFu.DynaFu, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.DynaFu.DynaFu, ref: ref}) do
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
  - **params**: `kinfu::Params`

  ##### Return
  - **retval**: `DynaFu`

  Python prototype (for reference only):
  ```python3
  create(_params) -> retval
  ```
  """
  @spec create(Evision.KinFu.Params.t()) :: Evision.DynaFu.DynaFu.t() | {:error, String.t()}
  def create(params) when is_struct(params, Evision.KinFu.Params)
  do
    positional = [
      params: Evision.Internal.Structurise.from_struct(params)
    ]
    :evision_nif.dynafu_dynafu_DynaFu_create_static(positional)
    |> to_struct()
  end

  @doc """
  Gets points and normals of current 3d mesh

  ##### Positional Arguments
  - **self**: `Evision.DynaFu.DynaFu.t()`

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
  @spec getCloud(Evision.DynaFu.t(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getCloud(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.dynafu_dynafu_DynaFu_getCloud(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Gets points and normals of current 3d mesh

  ##### Positional Arguments
  - **self**: `Evision.DynaFu.DynaFu.t()`

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
  @spec getCloud(Evision.DynaFu.t()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getCloud(self) do
    positional = [
    ]
    :evision_nif.dynafu_dynafu_DynaFu_getCloud(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Calculates normals for given points

  ##### Positional Arguments
  - **self**: `Evision.DynaFu.DynaFu.t()`
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
  @spec getNormals(Evision.DynaFu.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getNormals(self, points, opts) when (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.dynafu_dynafu_DynaFu_getNormals(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Calculates normals for given points

  ##### Positional Arguments
  - **self**: `Evision.DynaFu.DynaFu.t()`
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
  @spec getNormals(Evision.DynaFu.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def getNormals(self, points) when (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.dynafu_dynafu_DynaFu_getNormals(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Gets points of current 3d mesh

  ##### Positional Arguments
  - **self**: `Evision.DynaFu.DynaFu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.

    vector of points which are 4-float vectors

  The order of points is undefined.

  Python prototype (for reference only):
  ```python3
  getPoints([, points]) -> points
  ```
  """
  @spec getPoints(Evision.DynaFu.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getPoints(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.dynafu_dynafu_DynaFu_getPoints(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Gets points of current 3d mesh

  ##### Positional Arguments
  - **self**: `Evision.DynaFu.DynaFu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.

    vector of points which are 4-float vectors

  The order of points is undefined.

  Python prototype (for reference only):
  ```python3
  getPoints([, points]) -> points
  ```
  """
  @spec getPoints(Evision.DynaFu.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getPoints(self) do
    positional = [
    ]
    :evision_nif.dynafu_dynafu_DynaFu_getPoints(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Renders a volume into an image

  ##### Positional Arguments
  - **self**: `Evision.DynaFu.DynaFu.t()`

  ##### Keyword Arguments
  - **cameraPose**: `Evision.Mat`.

    pose of camera to render from. If empty then render from current pose
    which is a last frame camera pose.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    resulting image

  Renders a 0-surface of TSDF using Phong shading into a CV_8UC4 Mat.
  Light pose is fixed in DynaFu params.

  Python prototype (for reference only):
  ```python3
  render([, image[, cameraPose]]) -> image
  ```
  """
  @spec render(Evision.DynaFu.t(), [{:cameraPose, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def render(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:cameraPose])
    positional = [
    ]
    :evision_nif.dynafu_dynafu_DynaFu_render(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Renders a volume into an image

  ##### Positional Arguments
  - **self**: `Evision.DynaFu.DynaFu.t()`

  ##### Keyword Arguments
  - **cameraPose**: `Evision.Mat`.

    pose of camera to render from. If empty then render from current pose
    which is a last frame camera pose.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    resulting image

  Renders a 0-surface of TSDF using Phong shading into a CV_8UC4 Mat.
  Light pose is fixed in DynaFu params.

  Python prototype (for reference only):
  ```python3
  render([, image[, cameraPose]]) -> image
  ```
  """
  @spec render(Evision.DynaFu.t()) :: Evision.Mat.t() | {:error, String.t()}
  def render(self) do
    positional = [
    ]
    :evision_nif.dynafu_dynafu_DynaFu_render(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Resets the algorithm

  ##### Positional Arguments
  - **self**: `Evision.DynaFu.DynaFu.t()`

  Clears current model and resets a pose.

  Python prototype (for reference only):
  ```python3
  reset() -> None
  ```
  """
  @spec reset(Evision.DynaFu.t()) :: Evision.DynaFu.t() | {:error, String.t()}
  def reset(self) do
    positional = [
    ]
    :evision_nif.dynafu_dynafu_DynaFu_reset(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Process next depth frame

  ##### Positional Arguments
  - **self**: `Evision.DynaFu.DynaFu.t()`
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
  @spec update(Evision.DynaFu.t(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def update(self, depth) when (is_struct(depth, Evision.Mat) or is_struct(depth, Nx.Tensor) or is_number(depth) or is_tuple(depth))
  do
    positional = [
      depth: Evision.Internal.Structurise.from_struct(depth)
    ]
    :evision_nif.dynafu_dynafu_DynaFu_update(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
