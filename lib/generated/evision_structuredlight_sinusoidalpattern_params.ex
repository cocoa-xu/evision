defmodule Evision.StructuredLight.SinusoidalPattern.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `StructuredLight.SinusoidalPattern.Params` struct.

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
  def to_struct({:ok, %{class: Evision.StructuredLight.SinusoidalPattern.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.StructuredLight.SinusoidalPattern.Params, ref: ref}) do
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
  SinusoidalPattern_Params
  ##### Return
  - **self**: `Evision.StructuredLight.SinusoidalPattern.Params.t()`

  Python prototype (for reference only):
  ```python3
  SinusoidalPattern_Params() -> <structured_light_SinusoidalPattern_Params object>
  ```
  """
  @spec structured_light_SinusoidalPattern_Params() :: Evision.StructuredLight.SinusoidalPattern.Params.t() | {:error, String.t()}
  def structured_light_SinusoidalPattern_Params() do
    positional = [
    ]
    :evision_nif.structured_light_structured_light_SinusoidalPattern_Params_SinusoidalPattern_Params(positional)
    |> to_struct()
  end
  @spec get_height(Evision.StructuredLight.SinusoidalPattern.Params.t()) :: integer()
  def get_height(self) do
    :evision_nif.structured_light_SinusoidalPattern_Params_get_height(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_height(Evision.StructuredLight.SinusoidalPattern.Params.t(), integer()) :: Evision.StructuredLight.SinusoidalPattern.Params.t()
  def set_height(self, prop) do
    :evision_nif.structured_light_SinusoidalPattern_Params_set_height(
        Evision.Internal.Structurise.from_struct(self),
        [height: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_horizontal(Evision.StructuredLight.SinusoidalPattern.Params.t()) :: boolean()
  def get_horizontal(self) do
    :evision_nif.structured_light_SinusoidalPattern_Params_get_horizontal(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_horizontal(Evision.StructuredLight.SinusoidalPattern.Params.t(), boolean()) :: Evision.StructuredLight.SinusoidalPattern.Params.t()
  def set_horizontal(self, prop) do
    :evision_nif.structured_light_SinusoidalPattern_Params_set_horizontal(
        Evision.Internal.Structurise.from_struct(self),
        [horizontal: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_methodId(Evision.StructuredLight.SinusoidalPattern.Params.t()) :: integer()
  def get_methodId(self) do
    :evision_nif.structured_light_SinusoidalPattern_Params_get_methodId(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_methodId(Evision.StructuredLight.SinusoidalPattern.Params.t(), integer()) :: Evision.StructuredLight.SinusoidalPattern.Params.t()
  def set_methodId(self, prop) do
    :evision_nif.structured_light_SinusoidalPattern_Params_set_methodId(
        Evision.Internal.Structurise.from_struct(self),
        [methodId: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_nbrOfPeriods(Evision.StructuredLight.SinusoidalPattern.Params.t()) :: integer()
  def get_nbrOfPeriods(self) do
    :evision_nif.structured_light_SinusoidalPattern_Params_get_nbrOfPeriods(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_nbrOfPeriods(Evision.StructuredLight.SinusoidalPattern.Params.t(), integer()) :: Evision.StructuredLight.SinusoidalPattern.Params.t()
  def set_nbrOfPeriods(self, prop) do
    :evision_nif.structured_light_SinusoidalPattern_Params_set_nbrOfPeriods(
        Evision.Internal.Structurise.from_struct(self),
        [nbrOfPeriods: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_nbrOfPixelsBetweenMarkers(Evision.StructuredLight.SinusoidalPattern.Params.t()) :: integer()
  def get_nbrOfPixelsBetweenMarkers(self) do
    :evision_nif.structured_light_SinusoidalPattern_Params_get_nbrOfPixelsBetweenMarkers(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_nbrOfPixelsBetweenMarkers(Evision.StructuredLight.SinusoidalPattern.Params.t(), integer()) :: Evision.StructuredLight.SinusoidalPattern.Params.t()
  def set_nbrOfPixelsBetweenMarkers(self, prop) do
    :evision_nif.structured_light_SinusoidalPattern_Params_set_nbrOfPixelsBetweenMarkers(
        Evision.Internal.Structurise.from_struct(self),
        [nbrOfPixelsBetweenMarkers: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_setMarkers(Evision.StructuredLight.SinusoidalPattern.Params.t()) :: boolean()
  def get_setMarkers(self) do
    :evision_nif.structured_light_SinusoidalPattern_Params_get_setMarkers(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_setMarkers(Evision.StructuredLight.SinusoidalPattern.Params.t(), boolean()) :: Evision.StructuredLight.SinusoidalPattern.Params.t()
  def set_setMarkers(self, prop) do
    :evision_nif.structured_light_SinusoidalPattern_Params_set_setMarkers(
        Evision.Internal.Structurise.from_struct(self),
        [setMarkers: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_shiftValue(Evision.StructuredLight.SinusoidalPattern.Params.t()) :: number()
  def get_shiftValue(self) do
    :evision_nif.structured_light_SinusoidalPattern_Params_get_shiftValue(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_shiftValue(Evision.StructuredLight.SinusoidalPattern.Params.t(), number()) :: Evision.StructuredLight.SinusoidalPattern.Params.t()
  def set_shiftValue(self, prop) do
    :evision_nif.structured_light_SinusoidalPattern_Params_set_shiftValue(
        Evision.Internal.Structurise.from_struct(self),
        [shiftValue: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_width(Evision.StructuredLight.SinusoidalPattern.Params.t()) :: integer()
  def get_width(self) do
    :evision_nif.structured_light_SinusoidalPattern_Params_get_width(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_width(Evision.StructuredLight.SinusoidalPattern.Params.t(), integer()) :: Evision.StructuredLight.SinusoidalPattern.Params.t()
  def set_width(self, prop) do
    :evision_nif.structured_light_SinusoidalPattern_Params_set_width(
        Evision.Internal.Structurise.from_struct(self),
        [width: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
