defmodule Evision.TonemapReinhard do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `TonemapReinhard` struct.

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
  def to_struct({:ok, %{class: Evision.TonemapReinhard, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.TonemapReinhard, ref: ref}) do
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
  getColorAdaptation

  ##### Positional Arguments
  - **self**: `Evision.TonemapReinhard.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getColorAdaptation() -> retval
  ```
  """
  @spec getColorAdaptation(Evision.TonemapReinhard.t()) :: number() | {:error, String.t()}
  def getColorAdaptation(self) do
    positional = [
    ]
    :evision_nif.tonemapReinhard_getColorAdaptation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getIntensity

  ##### Positional Arguments
  - **self**: `Evision.TonemapReinhard.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getIntensity() -> retval
  ```
  """
  @spec getIntensity(Evision.TonemapReinhard.t()) :: number() | {:error, String.t()}
  def getIntensity(self) do
    positional = [
    ]
    :evision_nif.tonemapReinhard_getIntensity(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getLightAdaptation

  ##### Positional Arguments
  - **self**: `Evision.TonemapReinhard.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getLightAdaptation() -> retval
  ```
  """
  @spec getLightAdaptation(Evision.TonemapReinhard.t()) :: number() | {:error, String.t()}
  def getLightAdaptation(self) do
    positional = [
    ]
    :evision_nif.tonemapReinhard_getLightAdaptation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setColorAdaptation

  ##### Positional Arguments
  - **self**: `Evision.TonemapReinhard.t()`
  - **color_adapt**: `float`

  Python prototype (for reference only):
  ```python3
  setColorAdaptation(color_adapt) -> None
  ```
  """
  @spec setColorAdaptation(Evision.TonemapReinhard.t(), number()) :: Evision.TonemapReinhard.t() | {:error, String.t()}
  def setColorAdaptation(self, color_adapt) when is_float(color_adapt)
  do
    positional = [
      color_adapt: Evision.Internal.Structurise.from_struct(color_adapt)
    ]
    :evision_nif.tonemapReinhard_setColorAdaptation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setIntensity

  ##### Positional Arguments
  - **self**: `Evision.TonemapReinhard.t()`
  - **intensity**: `float`

  Python prototype (for reference only):
  ```python3
  setIntensity(intensity) -> None
  ```
  """
  @spec setIntensity(Evision.TonemapReinhard.t(), number()) :: Evision.TonemapReinhard.t() | {:error, String.t()}
  def setIntensity(self, intensity) when is_float(intensity)
  do
    positional = [
      intensity: Evision.Internal.Structurise.from_struct(intensity)
    ]
    :evision_nif.tonemapReinhard_setIntensity(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setLightAdaptation

  ##### Positional Arguments
  - **self**: `Evision.TonemapReinhard.t()`
  - **light_adapt**: `float`

  Python prototype (for reference only):
  ```python3
  setLightAdaptation(light_adapt) -> None
  ```
  """
  @spec setLightAdaptation(Evision.TonemapReinhard.t(), number()) :: Evision.TonemapReinhard.t() | {:error, String.t()}
  def setLightAdaptation(self, light_adapt) when is_float(light_adapt)
  do
    positional = [
      light_adapt: Evision.Internal.Structurise.from_struct(light_adapt)
    ]
    :evision_nif.tonemapReinhard_setLightAdaptation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
