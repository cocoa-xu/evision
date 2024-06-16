defmodule Evision.RGBD.RgbdFrame do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `RGBD.RgbdFrame` struct.

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
  def to_struct({:ok, %{class: Evision.RGBD.RgbdFrame, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.RGBD.RgbdFrame, ref: ref}) do
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
  ##### Keyword Arguments
  - **image**: `Evision.Mat`.
  - **depth**: `Evision.Mat`.
  - **mask**: `Evision.Mat`.
  - **normals**: `Evision.Mat`.
  - **iD**: `integer()`.

  ##### Return
  - **retval**: `RgbdFrame`

  Python prototype (for reference only):
  ```python3
  create([, image[, depth[, mask[, normals[, ID]]]]]) -> retval
  ```
  """
  @spec create([{:depth, term()} | {:iD, term()} | {:image, term()} | {:mask, term()} | {:normals, term()}] | nil) :: Evision.RGBD.RgbdFrame.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:depth, :iD, :image, :mask, :normals])
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdFrame_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
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
  - **retval**: `RgbdFrame`

  Python prototype (for reference only):
  ```python3
  create([, image[, depth[, mask[, normals[, ID]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.RGBD.RgbdFrame.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdFrame_create_static(positional)
    |> to_struct()
  end

  @doc """
  release

  ##### Positional Arguments
  - **self**: `Evision.RGBD.RgbdFrame.t()`

  Python prototype (for reference only):
  ```python3
  release() -> None
  ```
  """
  @spec release(Evision.RGBD.RgbdFrame.t()) :: Evision.RGBD.RgbdFrame.t() | {:error, String.t()}
  def release(self) do
    positional = [
    ]
    :evision_nif.rgbd_rgbd_RgbdFrame_release(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_ID(Evision.RGBD.RgbdFrame.t()) :: integer()
  def get_ID(self) do
    :evision_nif.rgbd_RgbdFrame_get_ID(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_depth(Evision.RGBD.RgbdFrame.t()) :: Evision.Mat.t()
  def get_depth(self) do
    :evision_nif.rgbd_RgbdFrame_get_depth(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_image(Evision.RGBD.RgbdFrame.t()) :: Evision.Mat.t()
  def get_image(self) do
    :evision_nif.rgbd_RgbdFrame_get_image(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_mask(Evision.RGBD.RgbdFrame.t()) :: Evision.Mat.t()
  def get_mask(self) do
    :evision_nif.rgbd_RgbdFrame_get_mask(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_normals(Evision.RGBD.RgbdFrame.t()) :: Evision.Mat.t()
  def get_normals(self) do
    :evision_nif.rgbd_RgbdFrame_get_normals(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
end
