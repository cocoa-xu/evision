defmodule Evision.LargeKinfu.LargeKinfu do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LargeKinfu.LargeKinfu` struct.

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
  def to_struct({:ok, %{class: Evision.LargeKinfu.LargeKinfu, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LargeKinfu.LargeKinfu, ref: ref}) do
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
  - **retval**: `Evision.LargeKinfu.t()`

  Python prototype (for reference only):
  ```python3
  create(_params) -> retval
  ```
  """
  @spec create(Evision.LargeKinfu.Params.t()) :: Evision.LargeKinfu.t() | {:error, String.t()}
  def create(params) when is_struct(params, Evision.LargeKinfu.Params)
  do
    positional = [
      params: Evision.Internal.Structurise.from_struct(params)
    ]
    :evision_nif.large_kinfu_large_kinfu_LargeKinfu_create_static(positional)
    |> to_struct()
  end

  @doc """
  getCloud

  ##### Positional Arguments
  - **self**: `Evision.LargeKinfu.LargeKinfu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.
  - **normals**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getCloud([, points[, normals]]) -> points, normals
  ```
  """
  @spec getCloud(Evision.LargeKinfu.t(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getCloud(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.large_kinfu_large_kinfu_LargeKinfu_getCloud(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getCloud

  ##### Positional Arguments
  - **self**: `Evision.LargeKinfu.LargeKinfu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.
  - **normals**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getCloud([, points[, normals]]) -> points, normals
  ```
  """
  @spec getCloud(Evision.LargeKinfu.t()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getCloud(self) do
    positional = [
    ]
    :evision_nif.large_kinfu_large_kinfu_LargeKinfu_getCloud(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNormals

  ##### Positional Arguments
  - **self**: `Evision.LargeKinfu.LargeKinfu.t()`
  - **points**: `Evision.Mat`

  ##### Return
  - **normals**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getNormals(points[, normals]) -> normals
  ```
  """
  @spec getNormals(Evision.LargeKinfu.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getNormals(self, points, opts) when (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.large_kinfu_large_kinfu_LargeKinfu_getNormals(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getNormals

  ##### Positional Arguments
  - **self**: `Evision.LargeKinfu.LargeKinfu.t()`
  - **points**: `Evision.Mat`

  ##### Return
  - **normals**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getNormals(points[, normals]) -> normals
  ```
  """
  @spec getNormals(Evision.LargeKinfu.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def getNormals(self, points) when (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.large_kinfu_large_kinfu_LargeKinfu_getNormals(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getPoints

  ##### Positional Arguments
  - **self**: `Evision.LargeKinfu.LargeKinfu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getPoints([, points]) -> points
  ```
  """
  @spec getPoints(Evision.LargeKinfu.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getPoints(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.large_kinfu_large_kinfu_LargeKinfu_getPoints(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getPoints

  ##### Positional Arguments
  - **self**: `Evision.LargeKinfu.LargeKinfu.t()`

  ##### Return
  - **points**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getPoints([, points]) -> points
  ```
  """
  @spec getPoints(Evision.LargeKinfu.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getPoints(self) do
    positional = [
    ]
    :evision_nif.large_kinfu_large_kinfu_LargeKinfu_getPoints(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  render

  ##### Positional Arguments
  - **self**: `Evision.LargeKinfu.LargeKinfu.t()`
  - **cameraPose**: `Evision.Mat`

  ##### Return
  - **image**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  render(cameraPose[, image]) -> image
  ```
  """
  @spec render(Evision.LargeKinfu.t(), Evision.Mat.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def render(self, cameraPose, opts) when (is_struct(cameraPose, Evision.Mat) or is_struct(cameraPose, Nx.Tensor) or is_number(cameraPose) or is_tuple(cameraPose)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      cameraPose: Evision.Internal.Structurise.from_struct(cameraPose)
    ]
    :evision_nif.large_kinfu_large_kinfu_LargeKinfu_render(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  render

  ##### Positional Arguments
  - **self**: `Evision.LargeKinfu.LargeKinfu.t()`
  - **cameraPose**: `Evision.Mat`

  ##### Return
  - **image**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  render(cameraPose[, image]) -> image
  ```
  #### Variant 2:
  render

  ##### Positional Arguments
  - **self**: `Evision.LargeKinfu.LargeKinfu.t()`

  ##### Return
  - **image**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  render([, image]) -> image
  ```

  """
  @spec render(Evision.LargeKinfu.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def render(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.large_kinfu_large_kinfu_LargeKinfu_render(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec render(Evision.LargeKinfu.t(), Evision.Mat.t()) :: Evision.Mat.t() | {:error, String.t()}
  def render(self, cameraPose) when (is_struct(cameraPose, Evision.Mat) or is_struct(cameraPose, Nx.Tensor) or is_number(cameraPose) or is_tuple(cameraPose))
  do
    positional = [
      cameraPose: Evision.Internal.Structurise.from_struct(cameraPose)
    ]
    :evision_nif.large_kinfu_large_kinfu_LargeKinfu_render(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  render

  ##### Positional Arguments
  - **self**: `Evision.LargeKinfu.LargeKinfu.t()`

  ##### Return
  - **image**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  render([, image]) -> image
  ```
  """
  @spec render(Evision.LargeKinfu.t()) :: Evision.Mat.t() | {:error, String.t()}
  def render(self) do
    positional = [
    ]
    :evision_nif.large_kinfu_large_kinfu_LargeKinfu_render(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  reset

  ##### Positional Arguments
  - **self**: `Evision.LargeKinfu.LargeKinfu.t()`

  Python prototype (for reference only):
  ```python3
  reset() -> None
  ```
  """
  @spec reset(Evision.LargeKinfu.t()) :: Evision.LargeKinfu.t() | {:error, String.t()}
  def reset(self) do
    positional = [
    ]
    :evision_nif.large_kinfu_large_kinfu_LargeKinfu_reset(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  update

  ##### Positional Arguments
  - **self**: `Evision.LargeKinfu.LargeKinfu.t()`
  - **depth**: `Evision.Mat`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  update(depth) -> retval
  ```
  """
  @spec update(Evision.LargeKinfu.t(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def update(self, depth) when (is_struct(depth, Evision.Mat) or is_struct(depth, Nx.Tensor) or is_number(depth) or is_tuple(depth))
  do
    positional = [
      depth: Evision.Internal.Structurise.from_struct(depth)
    ]
    :evision_nif.large_kinfu_large_kinfu_LargeKinfu_update(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
