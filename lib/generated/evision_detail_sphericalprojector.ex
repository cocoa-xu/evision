defmodule Evision.Detail.SphericalProjector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.SphericalProjector` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.SphericalProjector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.SphericalProjector, ref: ref}) do
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
  mapBackward

  ##### Positional Arguments
  - **self**: `Evision.Detail.SphericalProjector.t()`
  - **u**: `float`
  - **v**: `float`
  - **x**: `float`
  - **y**: `float`

  Python prototype (for reference only):
  ```python3
  mapBackward(u, v, x, y) -> None
  ```
  """
  @spec mapBackward(Evision.Detail.SphericalProjector.t(), number(), number(), number(), number()) :: Evision.Detail.SphericalProjector.t() | {:error, String.t()}
  def mapBackward(self, u, v, x, y) when is_float(u) and is_float(v) and is_float(x) and is_float(y)
  do
    positional = [
      u: Evision.Internal.Structurise.from_struct(u),
      v: Evision.Internal.Structurise.from_struct(v),
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.detail_detail_SphericalProjector_mapBackward(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  mapForward

  ##### Positional Arguments
  - **self**: `Evision.Detail.SphericalProjector.t()`
  - **x**: `float`
  - **y**: `float`
  - **u**: `float`
  - **v**: `float`

  Python prototype (for reference only):
  ```python3
  mapForward(x, y, u, v) -> None
  ```
  """
  @spec mapForward(Evision.Detail.SphericalProjector.t(), number(), number(), number(), number()) :: Evision.Detail.SphericalProjector.t() | {:error, String.t()}
  def mapForward(self, x, y, u, v) when is_float(x) and is_float(y) and is_float(u) and is_float(v)
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y),
      u: Evision.Internal.Structurise.from_struct(u),
      v: Evision.Internal.Structurise.from_struct(v)
    ]
    :evision_nif.detail_detail_SphericalProjector_mapForward(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
