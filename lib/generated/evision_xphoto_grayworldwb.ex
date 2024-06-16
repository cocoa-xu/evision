defmodule Evision.XPhoto.GrayworldWB do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XPhoto.GrayworldWB` struct.

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
  def to_struct({:ok, %{class: Evision.XPhoto.GrayworldWB, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XPhoto.GrayworldWB, ref: ref}) do
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
  Maximum saturation for a pixel to be included in the
  gray-world assumption

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.GrayworldWB.t()`

  ##### Return
  - **retval**: `float`

  @see `setSaturationThreshold/2`

  Python prototype (for reference only):
  ```python3
  getSaturationThreshold() -> retval
  ```
  """
  @spec getSaturationThreshold(Evision.XPhoto.GrayworldWB.t()) :: number() | {:error, String.t()}
  def getSaturationThreshold(self) do
    positional = [
    ]
    :evision_nif.xphoto_xphoto_GrayworldWB_getSaturationThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSaturationThreshold

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.GrayworldWB.t()`
  - **val**: `float`

  @see `getSaturationThreshold/1`

  Python prototype (for reference only):
  ```python3
  setSaturationThreshold(val) -> None
  ```
  """
  @spec setSaturationThreshold(Evision.XPhoto.GrayworldWB.t(), number()) :: Evision.XPhoto.GrayworldWB.t() | {:error, String.t()}
  def setSaturationThreshold(self, val) when is_float(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.xphoto_xphoto_GrayworldWB_setSaturationThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
