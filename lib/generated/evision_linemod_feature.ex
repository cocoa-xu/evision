defmodule Evision.LineMod.Feature do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineMod.Feature` struct.

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
  def to_struct({:ok, %{class: Evision.LineMod.Feature, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineMod.Feature, ref: ref}) do
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
  Feature

  ##### Positional Arguments
  - **x**: `integer()`
  - **y**: `integer()`
  - **label**: `integer()`

  ##### Return
  - **self**: `Feature`

  Python prototype (for reference only):
  ```python3
  Feature(x, y, label) -> <linemod_Feature object>
  ```
  """
  @spec feature(integer(), integer(), integer()) :: Evision.LineMod.Feature.t() | {:error, String.t()}
  def feature(x, y, label) when is_integer(x) and is_integer(y) and is_integer(label)
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y),
      label: Evision.Internal.Structurise.from_struct(label)
    ]
    :evision_nif.linemod_linemod_Feature_Feature(positional)
    |> to_struct()
  end

  @doc """
  Feature
  ##### Return
  - **self**: `Feature`

  Python prototype (for reference only):
  ```python3
  Feature() -> <linemod_Feature object>
  ```
  """
  @spec feature() :: Evision.LineMod.Feature.t() | {:error, String.t()}
  def feature() do
    positional = [
    ]
    :evision_nif.linemod_linemod_Feature_Feature(positional)
    |> to_struct()
  end
  @spec get_label(Evision.LineMod.Feature.t()) :: integer()
  def get_label(self) do
    :evision_nif.linemod_Feature_get_label(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_label(Evision.LineMod.Feature.t(), integer()) :: Evision.LineMod.Feature.t()
  def set_label(self, prop) do
    :evision_nif.linemod_Feature_set_label(
        Evision.Internal.Structurise.from_struct(self),
        [label: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_x(Evision.LineMod.Feature.t()) :: integer()
  def get_x(self) do
    :evision_nif.linemod_Feature_get_x(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_x(Evision.LineMod.Feature.t(), integer()) :: Evision.LineMod.Feature.t()
  def set_x(self, prop) do
    :evision_nif.linemod_Feature_set_x(
        Evision.Internal.Structurise.from_struct(self),
        [x: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_y(Evision.LineMod.Feature.t()) :: integer()
  def get_y(self) do
    :evision_nif.linemod_Feature_get_y(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_y(Evision.LineMod.Feature.t(), integer()) :: Evision.LineMod.Feature.t()
  def set_y(self, prop) do
    :evision_nif.linemod_Feature_set_y(
        Evision.Internal.Structurise.from_struct(self),
        [y: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
