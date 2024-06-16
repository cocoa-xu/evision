defmodule Evision.Detail.VoronoiSeamFinder do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.VoronoiSeamFinder` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.VoronoiSeamFinder, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.VoronoiSeamFinder, ref: ref}) do
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
  find

  ##### Positional Arguments
  - **self**: `Evision.Detail.VoronoiSeamFinder.t()`
  - **src**: `[Evision.Mat]`
  - **corners**: `[Point]`

  ##### Return
  - **masks**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  find(src, corners, masks) -> masks
  ```
  """
  @spec find(Evision.Detail.VoronoiSeamFinder.t(), list(Evision.Mat.maybe_mat_in()), list({number(), number()}), list(Evision.Mat.maybe_mat_in())) :: list(Evision.Mat.t()) | {:error, String.t()}
  def find(self, src, corners, masks) when is_list(src) and is_list(corners) and is_list(masks)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      corners: Evision.Internal.Structurise.from_struct(corners),
      masks: Evision.Internal.Structurise.from_struct(masks)
    ]
    :evision_nif.detail_detail_VoronoiSeamFinder_find(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
