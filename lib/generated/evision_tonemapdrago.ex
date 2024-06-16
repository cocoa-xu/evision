defmodule Evision.TonemapDrago do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `TonemapDrago` struct.

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
  def to_struct({:ok, %{class: Evision.TonemapDrago, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.TonemapDrago, ref: ref}) do
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
  getBias

  ##### Positional Arguments
  - **self**: `Evision.TonemapDrago.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getBias() -> retval
  ```
  """
  @spec getBias(Evision.TonemapDrago.t()) :: number() | {:error, String.t()}
  def getBias(self) do
    positional = [
    ]
    :evision_nif.tonemapDrago_getBias(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSaturation

  ##### Positional Arguments
  - **self**: `Evision.TonemapDrago.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getSaturation() -> retval
  ```
  """
  @spec getSaturation(Evision.TonemapDrago.t()) :: number() | {:error, String.t()}
  def getSaturation(self) do
    positional = [
    ]
    :evision_nif.tonemapDrago_getSaturation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBias

  ##### Positional Arguments
  - **self**: `Evision.TonemapDrago.t()`
  - **bias**: `float`

  Python prototype (for reference only):
  ```python3
  setBias(bias) -> None
  ```
  """
  @spec setBias(Evision.TonemapDrago.t(), number()) :: Evision.TonemapDrago.t() | {:error, String.t()}
  def setBias(self, bias) when is_float(bias)
  do
    positional = [
      bias: Evision.Internal.Structurise.from_struct(bias)
    ]
    :evision_nif.tonemapDrago_setBias(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSaturation

  ##### Positional Arguments
  - **self**: `Evision.TonemapDrago.t()`
  - **saturation**: `float`

  Python prototype (for reference only):
  ```python3
  setSaturation(saturation) -> None
  ```
  """
  @spec setSaturation(Evision.TonemapDrago.t(), number()) :: Evision.TonemapDrago.t() | {:error, String.t()}
  def setSaturation(self, saturation) when is_float(saturation)
  do
    positional = [
      saturation: Evision.Internal.Structurise.from_struct(saturation)
    ]
    :evision_nif.tonemapDrago_setSaturation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
