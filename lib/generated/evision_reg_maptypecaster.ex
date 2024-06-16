defmodule Evision.Reg.MapTypeCaster do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Reg.MapTypeCaster` struct.

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
  def to_struct({:ok, %{class: Evision.Reg.MapTypeCaster, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Reg.MapTypeCaster, ref: ref}) do
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
  toAffine

  ##### Positional Arguments
  - **sourceMap**: `Map`

  ##### Return
  - **retval**: `MapAffine`

  Python prototype (for reference only):
  ```python3
  toAffine(sourceMap) -> retval
  ```
  """
  @spec toAffine(Evision.Reg.Map.t()) :: Evision.Reg.MapAffine.t() | {:error, String.t()}
  def toAffine(sourceMap) when is_struct(sourceMap, Evision.Reg.Map)
  do
    positional = [
      sourceMap: Evision.Internal.Structurise.from_struct(sourceMap)
    ]
    :evision_nif.reg_reg_MapTypeCaster_toAffine_static(positional)
    |> to_struct()
  end

  @doc """
  toProjec

  ##### Positional Arguments
  - **sourceMap**: `Map`

  ##### Return
  - **retval**: `MapProjec`

  Python prototype (for reference only):
  ```python3
  toProjec(sourceMap) -> retval
  ```
  """
  @spec toProjec(Evision.Reg.Map.t()) :: Evision.Reg.MapProjec.t() | {:error, String.t()}
  def toProjec(sourceMap) when is_struct(sourceMap, Evision.Reg.Map)
  do
    positional = [
      sourceMap: Evision.Internal.Structurise.from_struct(sourceMap)
    ]
    :evision_nif.reg_reg_MapTypeCaster_toProjec_static(positional)
    |> to_struct()
  end

  @doc """
  toShift

  ##### Positional Arguments
  - **sourceMap**: `Map`

  ##### Return
  - **retval**: `MapShift`

  Python prototype (for reference only):
  ```python3
  toShift(sourceMap) -> retval
  ```
  """
  @spec toShift(Evision.Reg.Map.t()) :: Evision.Reg.MapShift.t() | {:error, String.t()}
  def toShift(sourceMap) when is_struct(sourceMap, Evision.Reg.Map)
  do
    positional = [
      sourceMap: Evision.Internal.Structurise.from_struct(sourceMap)
    ]
    :evision_nif.reg_reg_MapTypeCaster_toShift_static(positional)
    |> to_struct()
  end
end
