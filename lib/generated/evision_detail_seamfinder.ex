defmodule Evision.Detail.SeamFinder do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.SeamFinder` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.SeamFinder, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.SeamFinder, ref: ref}) do
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
  def cv_NO, do: 0
  @doc enum: true
  def cv_VORONOI_SEAM, do: 1
  @doc enum: true
  def cv_DP_SEAM, do: 2


  @doc """
  createDefault

  ##### Positional Arguments
  - **type**: `integer()`

  ##### Return
  - **retval**: `Evision.Detail.SeamFinder.t()`

  Python prototype (for reference only):
  ```python3
  createDefault(type) -> retval
  ```
  """
  @spec createDefault(integer()) :: Evision.Detail.SeamFinder.t() | {:error, String.t()}
  def createDefault(type) when is_integer(type)
  do
    positional = [
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.detail_detail_SeamFinder_createDefault_static(positional)
    |> to_struct()
  end

  @doc """
  Estimates seams.

  ##### Positional Arguments
  - **self**: `Evision.Detail.SeamFinder.t()`
  - **src**: `[Evision.Mat]`.

    Source images

  - **corners**: `[Point]`.

    Source image top-left corners

  ##### Return
  - **masks**: `[Evision.Mat]`.

    Source image masks to update

  Python prototype (for reference only):
  ```python3
  find(src, corners, masks) -> masks
  ```
  """
  @spec find(Evision.Detail.SeamFinder.t(), list(Evision.Mat.maybe_mat_in()), list({number(), number()}), list(Evision.Mat.maybe_mat_in())) :: list(Evision.Mat.t()) | {:error, String.t()}
  def find(self, src, corners, masks) when is_list(src) and is_list(corners) and is_list(masks)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      corners: Evision.Internal.Structurise.from_struct(corners),
      masks: Evision.Internal.Structurise.from_struct(masks)
    ]
    :evision_nif.detail_detail_SeamFinder_find(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
