defmodule Evision.LineMod.ColorGradient do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineMod.ColorGradient` struct.

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
  def to_struct({:ok, %{class: Evision.LineMod.ColorGradient, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineMod.ColorGradient, ref: ref}) do
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
  - **weak_threshold**: `float`
  - **num_features**: `size_t`
  - **strong_threshold**: `float`

  ##### Return
  - **retval**: `ColorGradient`

   \\brief Constructor.
   \\param weak_threshold   When quantizing, discard gradients with magnitude less than this.
   \\param num_features     How many features a template must contain.
   \\param strong_threshold Consider as candidate features only gradients whose norms are
                           larger than this.

  Python prototype (for reference only):
  ```python3
  create(weak_threshold, num_features, strong_threshold) -> retval
  ```
  """
  @spec create(number(), integer(), number()) :: Evision.LineMod.ColorGradient.t() | {:error, String.t()}
  def create(weak_threshold, num_features, strong_threshold) when is_float(weak_threshold) and is_integer(num_features) and is_float(strong_threshold)
  do
    positional = [
      weak_threshold: Evision.Internal.Structurise.from_struct(weak_threshold),
      num_features: Evision.Internal.Structurise.from_struct(num_features),
      strong_threshold: Evision.Internal.Structurise.from_struct(strong_threshold)
    ]
    :evision_nif.linemod_linemod_ColorGradient_create_static(positional)
    |> to_struct()
  end
  @spec get_num_features(Evision.LineMod.ColorGradient.t()) :: integer()
  def get_num_features(self) do
    :evision_nif.linemod_ColorGradient_get_num_features(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_strong_threshold(Evision.LineMod.ColorGradient.t()) :: number()
  def get_strong_threshold(self) do
    :evision_nif.linemod_ColorGradient_get_strong_threshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_weak_threshold(Evision.LineMod.ColorGradient.t()) :: number()
  def get_weak_threshold(self) do
    :evision_nif.linemod_ColorGradient_get_weak_threshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
end
