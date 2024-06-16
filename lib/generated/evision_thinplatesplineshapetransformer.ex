defmodule Evision.ThinPlateSplineShapeTransformer do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ThinPlateSplineShapeTransformer` struct.

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
  def to_struct({:ok, %{class: Evision.ThinPlateSplineShapeTransformer, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ThinPlateSplineShapeTransformer, ref: ref}) do
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
  getRegularizationParameter

  ##### Positional Arguments
  - **self**: `Evision.ThinPlateSplineShapeTransformer.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getRegularizationParameter() -> retval
  ```
  """
  @spec getRegularizationParameter(Evision.ThinPlateSplineShapeTransformer.t()) :: number() | {:error, String.t()}
  def getRegularizationParameter(self) do
    positional = [
    ]
    :evision_nif.thinPlateSplineShapeTransformer_getRegularizationParameter(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the regularization parameter for relaxing the exact interpolation requirements of the TPS
  algorithm.

  ##### Positional Arguments
  - **self**: `Evision.ThinPlateSplineShapeTransformer.t()`
  - **beta**: `double`.

    value of the regularization parameter.

  Python prototype (for reference only):
  ```python3
  setRegularizationParameter(beta) -> None
  ```
  """
  @spec setRegularizationParameter(Evision.ThinPlateSplineShapeTransformer.t(), number()) :: Evision.ThinPlateSplineShapeTransformer.t() | {:error, String.t()}
  def setRegularizationParameter(self, beta) when is_number(beta)
  do
    positional = [
      beta: Evision.Internal.Structurise.from_struct(beta)
    ]
    :evision_nif.thinPlateSplineShapeTransformer_setRegularizationParameter(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
