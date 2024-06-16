defmodule Evision.LineMod.DepthNormal do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineMod.DepthNormal` struct.

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
  def to_struct({:ok, %{class: Evision.LineMod.DepthNormal, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineMod.DepthNormal, ref: ref}) do
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
  - **distance_threshold**: `integer()`
  - **difference_threshold**: `integer()`
  - **num_features**: `size_t`
  - **extract_threshold**: `integer()`

  ##### Return
  - **retval**: `DepthNormal`

   \\brief Constructor.
   \\param distance_threshold   Ignore pixels beyond this distance.
   \\param difference_threshold When computing normals, ignore contributions of pixels whose
                               depth difference with the central pixel is above this threshold.
   \\param num_features         How many features a template must contain.
   \\param extract_threshold    Consider as candidate feature only if there are no differing
                               orientations within a distance of extract_threshold.

  Python prototype (for reference only):
  ```python3
  create(distance_threshold, difference_threshold, num_features, extract_threshold) -> retval
  ```
  """
  @spec create(integer(), integer(), integer(), integer()) :: Evision.LineMod.DepthNormal.t() | {:error, String.t()}
  def create(distance_threshold, difference_threshold, num_features, extract_threshold) when is_integer(distance_threshold) and is_integer(difference_threshold) and is_integer(num_features) and is_integer(extract_threshold)
  do
    positional = [
      distance_threshold: Evision.Internal.Structurise.from_struct(distance_threshold),
      difference_threshold: Evision.Internal.Structurise.from_struct(difference_threshold),
      num_features: Evision.Internal.Structurise.from_struct(num_features),
      extract_threshold: Evision.Internal.Structurise.from_struct(extract_threshold)
    ]
    :evision_nif.linemod_linemod_DepthNormal_create_static(positional)
    |> to_struct()
  end
  @spec get_difference_threshold(Evision.LineMod.DepthNormal.t()) :: integer()
  def get_difference_threshold(self) do
    :evision_nif.linemod_DepthNormal_get_difference_threshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_distance_threshold(Evision.LineMod.DepthNormal.t()) :: integer()
  def get_distance_threshold(self) do
    :evision_nif.linemod_DepthNormal_get_distance_threshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_extract_threshold(Evision.LineMod.DepthNormal.t()) :: integer()
  def get_extract_threshold(self) do
    :evision_nif.linemod_DepthNormal_get_extract_threshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_num_features(Evision.LineMod.DepthNormal.t()) :: integer()
  def get_num_features(self) do
    :evision_nif.linemod_DepthNormal_get_num_features(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
end
