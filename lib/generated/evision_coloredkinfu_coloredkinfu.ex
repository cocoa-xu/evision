defmodule Evision.ColoredKinFu.ColoredKinFu do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ColoredKinFu.ColoredKinFu` struct.

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
  def to_struct({:ok, %{class: Evision.ColoredKinFu.ColoredKinFu, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ColoredKinFu.ColoredKinFu, ref: ref}) do
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
  - **retval**: `Evision.ColoredKinFu.t()`

  Python prototype (for reference only):
  ```python3
  create(_params) -> retval
  ```
  """
  @spec create(Evision.ColoredKinFu.Params.t()) :: Evision.ColoredKinFu.t() | {:error, String.t()}
  def create(params) when is_struct(params, Evision.ColoredKinFu.Params)
  do
    positional = [
      params: Evision.Internal.Structurise.from_struct(params)
    ]
    :evision_nif.colored_kinfu_colored_kinfu_ColoredKinFu_create_static(positional)
    |> to_struct()
  end

  @doc """
  Gets points, normals and colors of current 3d mesh

  ##### Positional Arguments
  - **self**: `Evision.ColoredKinFu.ColoredKinFu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.

    vector of points which are 4-float vectors

  - **normals**: `Evision.Mat.t()`.

    vector of normals which are 4-float vectors

  - **colors**: `Evision.Mat.t()`.

    vector of colors which are 4-float vectors

  The order of normals corresponds to order of points.
  The order of points is undefined.

  Python prototype (for reference only):
  ```python3
  getCloud([, points[, normals[, colors]]]) -> points, normals, colors
  ```
  """
  @spec getCloud(Evision.ColoredKinFu.t(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getCloud(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.colored_kinfu_colored_kinfu_ColoredKinFu_getCloud(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Gets points, normals and colors of current 3d mesh

  ##### Positional Arguments
  - **self**: `Evision.ColoredKinFu.ColoredKinFu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.

    vector of points which are 4-float vectors

  - **normals**: `Evision.Mat.t()`.

    vector of normals which are 4-float vectors

  - **colors**: `Evision.Mat.t()`.

    vector of colors which are 4-float vectors

  The order of normals corresponds to order of points.
  The order of points is undefined.

  Python prototype (for reference only):
  ```python3
  getCloud([, points[, normals[, colors]]]) -> points, normals, colors
  ```
  """
  @spec getCloud(Evision.ColoredKinFu.t()) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getCloud(self) do
    positional = [
    ]
    :evision_nif.colored_kinfu_colored_kinfu_ColoredKinFu_getCloud(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Calculates normals for given points

  ##### Positional Arguments
  - **self**: `Evision.ColoredKinFu.ColoredKinFu.t()`
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
  @spec getNormals(Evision.ColoredKinFu.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getNormals(self, points, opts) when (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.colored_kinfu_colored_kinfu_ColoredKinFu_getNormals(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Calculates normals for given points

  ##### Positional Arguments
  - **self**: `Evision.ColoredKinFu.ColoredKinFu.t()`
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
  @spec getNormals(Evision.ColoredKinFu.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def getNormals(self, points) when (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.colored_kinfu_colored_kinfu_ColoredKinFu_getNormals(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Gets points of current 3d mesh

  ##### Positional Arguments
  - **self**: `Evision.ColoredKinFu.ColoredKinFu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.

    vector of points which are 4-float vectors

  The order of points is undefined.

  Python prototype (for reference only):
  ```python3
  getPoints([, points]) -> points
  ```
  """
  @spec getPoints(Evision.ColoredKinFu.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getPoints(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.colored_kinfu_colored_kinfu_ColoredKinFu_getPoints(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Gets points of current 3d mesh

  ##### Positional Arguments
  - **self**: `Evision.ColoredKinFu.ColoredKinFu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.

    vector of points which are 4-float vectors

  The order of points is undefined.

  Python prototype (for reference only):
  ```python3
  getPoints([, points]) -> points
  ```
  """
  @spec getPoints(Evision.ColoredKinFu.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getPoints(self) do
    positional = [
    ]
    :evision_nif.colored_kinfu_colored_kinfu_ColoredKinFu_getPoints(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Renders a volume into an image

  ##### Positional Arguments
  - **self**: `Evision.ColoredKinFu.ColoredKinFu.t()`
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
  @spec render(Evision.ColoredKinFu.t(), Evision.Mat.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def render(self, cameraPose, opts) when (is_struct(cameraPose, Evision.Mat) or is_struct(cameraPose, Nx.Tensor) or is_number(cameraPose) or is_tuple(cameraPose)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      cameraPose: Evision.Internal.Structurise.from_struct(cameraPose)
    ]
    :evision_nif.colored_kinfu_colored_kinfu_ColoredKinFu_render(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Renders a volume into an image

  ##### Positional Arguments
  - **self**: `Evision.ColoredKinFu.ColoredKinFu.t()`
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
  - **self**: `Evision.ColoredKinFu.ColoredKinFu.t()`

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
  @spec render(Evision.ColoredKinFu.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def render(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.colored_kinfu_colored_kinfu_ColoredKinFu_render(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec render(Evision.ColoredKinFu.t(), Evision.Mat.t()) :: Evision.Mat.t() | {:error, String.t()}
  def render(self, cameraPose) when (is_struct(cameraPose, Evision.Mat) or is_struct(cameraPose, Nx.Tensor) or is_number(cameraPose) or is_tuple(cameraPose))
  do
    positional = [
      cameraPose: Evision.Internal.Structurise.from_struct(cameraPose)
    ]
    :evision_nif.colored_kinfu_colored_kinfu_ColoredKinFu_render(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Renders a volume into an image

  ##### Positional Arguments
  - **self**: `Evision.ColoredKinFu.ColoredKinFu.t()`

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
  @spec render(Evision.ColoredKinFu.t()) :: Evision.Mat.t() | {:error, String.t()}
  def render(self) do
    positional = [
    ]
    :evision_nif.colored_kinfu_colored_kinfu_ColoredKinFu_render(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Resets the algorithm

  ##### Positional Arguments
  - **self**: `Evision.ColoredKinFu.ColoredKinFu.t()`

  Clears current model and resets a pose.

  Python prototype (for reference only):
  ```python3
  reset() -> None
  ```
  """
  @spec reset(Evision.ColoredKinFu.t()) :: Evision.ColoredKinFu.t() | {:error, String.t()}
  def reset(self) do
    positional = [
    ]
    :evision_nif.colored_kinfu_colored_kinfu_ColoredKinFu_reset(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Process next depth frame

  ##### Positional Arguments
  - **self**: `Evision.ColoredKinFu.ColoredKinFu.t()`
  - **depth**: `Evision.Mat`.

    input Mat of depth frame

  - **rgb**: `Evision.Mat`.

    input Mat of rgb (colored) frame

  ##### Return
  - **retval**: `bool`

  @return true if succeeded to align new frame with current scene, false if opposite

  Python prototype (for reference only):
  ```python3
  update(depth, rgb) -> retval
  ```
  """
  @spec update(Evision.ColoredKinFu.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def update(self, depth, rgb) when (is_struct(depth, Evision.Mat) or is_struct(depth, Nx.Tensor) or is_number(depth) or is_tuple(depth)) and (is_struct(rgb, Evision.Mat) or is_struct(rgb, Nx.Tensor) or is_number(rgb) or is_tuple(rgb))
  do
    positional = [
      depth: Evision.Internal.Structurise.from_struct(depth),
      rgb: Evision.Internal.Structurise.from_struct(rgb)
    ]
    :evision_nif.colored_kinfu_colored_kinfu_ColoredKinFu_update(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
