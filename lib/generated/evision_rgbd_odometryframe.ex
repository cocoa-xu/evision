defmodule Evision.RGBD.OdometryFrame do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `RGBD.OdometryFrame` struct.

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
  def to_struct({:ok, %{class: Evision.RGBD.OdometryFrame, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.RGBD.OdometryFrame, ref: ref}) do
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
  @type enum :: integer()
  @doc enum: true
  def cv_CACHE_SRC, do: 1
  @doc enum: true
  def cv_CACHE_DST, do: 2
  @doc enum: true
  def cv_CACHE_ALL, do: (cv_CACHE_SRC() + cv_CACHE_DST())


  @doc """
  create
  ##### Keyword Arguments
  - **image**: `Evision.Mat`.
  - **depth**: `Evision.Mat`.
  - **mask**: `Evision.Mat`.
  - **normals**: `Evision.Mat`.
  - **iD**: `integer()`.

  ##### Return
  - **retval**: `OdometryFrame`

  Python prototype (for reference only):
  ```python3
  create([, image[, depth[, mask[, normals[, ID]]]]]) -> retval
  ```
  """
  @spec create([{:depth, term()} | {:iD, term()} | {:image, term()} | {:mask, term()} | {:normals, term()}] | nil) :: Evision.RGBD.OdometryFrame.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:depth, :iD, :image, :mask, :normals])
    positional = [
    ]
    :evision_nif.rgbd_rgbd_OdometryFrame_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **image**: `Evision.Mat`.
  - **depth**: `Evision.Mat`.
  - **mask**: `Evision.Mat`.
  - **normals**: `Evision.Mat`.
  - **iD**: `integer()`.

  ##### Return
  - **retval**: `OdometryFrame`

  Python prototype (for reference only):
  ```python3
  create([, image[, depth[, mask[, normals[, ID]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.RGBD.OdometryFrame.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_OdometryFrame_create_static(positional)
    |> to_struct()
  end

  @doc """
  release

  ##### Positional Arguments
  - **self**: `Evision.RGBD.OdometryFrame.t()`

  Python prototype (for reference only):
  ```python3
  release() -> None
  ```
  """
  @spec release(Evision.RGBD.OdometryFrame.t()) :: Evision.RGBD.OdometryFrame.t() | {:error, String.t()}
  def release(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_OdometryFrame_release(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  releasePyramids

  ##### Positional Arguments
  - **self**: `Evision.RGBD.OdometryFrame.t()`

  Python prototype (for reference only):
  ```python3
  releasePyramids() -> None
  ```
  """
  @spec releasePyramids(Evision.RGBD.OdometryFrame.t()) :: Evision.RGBD.OdometryFrame.t() | {:error, String.t()}
  def releasePyramids(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_OdometryFrame_releasePyramids(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_pyramidCloud(Evision.RGBD.OdometryFrame.t()) :: list(Evision.Mat.t())
  def get_pyramidCloud(self) do
    :evision_nif.rgbd_OdometryFrame_get_pyramidCloud(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_pyramidDepth(Evision.RGBD.OdometryFrame.t()) :: list(Evision.Mat.t())
  def get_pyramidDepth(self) do
    :evision_nif.rgbd_OdometryFrame_get_pyramidDepth(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_pyramidImage(Evision.RGBD.OdometryFrame.t()) :: list(Evision.Mat.t())
  def get_pyramidImage(self) do
    :evision_nif.rgbd_OdometryFrame_get_pyramidImage(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_pyramidMask(Evision.RGBD.OdometryFrame.t()) :: list(Evision.Mat.t())
  def get_pyramidMask(self) do
    :evision_nif.rgbd_OdometryFrame_get_pyramidMask(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_pyramidNormals(Evision.RGBD.OdometryFrame.t()) :: list(Evision.Mat.t())
  def get_pyramidNormals(self) do
    :evision_nif.rgbd_OdometryFrame_get_pyramidNormals(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_pyramidNormalsMask(Evision.RGBD.OdometryFrame.t()) :: list(Evision.Mat.t())
  def get_pyramidNormalsMask(self) do
    :evision_nif.rgbd_OdometryFrame_get_pyramidNormalsMask(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_pyramidTexturedMask(Evision.RGBD.OdometryFrame.t()) :: list(Evision.Mat.t())
  def get_pyramidTexturedMask(self) do
    :evision_nif.rgbd_OdometryFrame_get_pyramidTexturedMask(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_pyramid_dI_dx(Evision.RGBD.OdometryFrame.t()) :: list(Evision.Mat.t())
  def get_pyramid_dI_dx(self) do
    :evision_nif.rgbd_OdometryFrame_get_pyramid_dI_dx(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_pyramid_dI_dy(Evision.RGBD.OdometryFrame.t()) :: list(Evision.Mat.t())
  def get_pyramid_dI_dy(self) do
    :evision_nif.rgbd_OdometryFrame_get_pyramid_dI_dy(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
end
