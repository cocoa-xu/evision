defmodule Evision.LineMod.Match do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineMod.Match` struct.

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
  def to_struct({:ok, %{class: Evision.LineMod.Match, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineMod.Match, ref: ref}) do
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
  Match

  ##### Positional Arguments
  - **x**: `integer()`
  - **y**: `integer()`
  - **similarity**: `float`
  - **class_id**: `String`
  - **template_id**: `integer()`

  ##### Return
  - **self**: `Match`

  Python prototype (for reference only):
  ```python3
  Match(x, y, similarity, class_id, template_id) -> <linemod_Match object>
  ```
  """
  @spec match(integer(), integer(), number(), binary(), integer()) :: Evision.LineMod.Match.t() | {:error, String.t()}
  def match(x, y, similarity, class_id, template_id) when is_integer(x) and is_integer(y) and is_float(similarity) and is_binary(class_id) and is_integer(template_id)
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y),
      similarity: Evision.Internal.Structurise.from_struct(similarity),
      class_id: Evision.Internal.Structurise.from_struct(class_id),
      template_id: Evision.Internal.Structurise.from_struct(template_id)
    ]
    :evision_nif.linemod_linemod_Match_Match(positional)
    |> to_struct()
  end

  @doc """
  Match
  ##### Return
  - **self**: `Match`

  Python prototype (for reference only):
  ```python3
  Match() -> <linemod_Match object>
  ```
  """
  @spec match() :: Evision.LineMod.Match.t() | {:error, String.t()}
  def match() do
    positional = [
    ]
    :evision_nif.linemod_linemod_Match_Match(positional)
    |> to_struct()
  end
  @spec get_class_id(Evision.LineMod.Match.t()) :: binary()
  def get_class_id(self) do
    :evision_nif.linemod_Match_get_class_id(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_class_id(Evision.LineMod.Match.t(), binary()) :: Evision.LineMod.Match.t()
  def set_class_id(self, prop) do
    :evision_nif.linemod_Match_set_class_id(
        Evision.Internal.Structurise.from_struct(self),
        [class_id: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_similarity(Evision.LineMod.Match.t()) :: number()
  def get_similarity(self) do
    :evision_nif.linemod_Match_get_similarity(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_similarity(Evision.LineMod.Match.t(), number()) :: Evision.LineMod.Match.t()
  def set_similarity(self, prop) do
    :evision_nif.linemod_Match_set_similarity(
        Evision.Internal.Structurise.from_struct(self),
        [similarity: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_template_id(Evision.LineMod.Match.t()) :: integer()
  def get_template_id(self) do
    :evision_nif.linemod_Match_get_template_id(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_template_id(Evision.LineMod.Match.t(), integer()) :: Evision.LineMod.Match.t()
  def set_template_id(self, prop) do
    :evision_nif.linemod_Match_set_template_id(
        Evision.Internal.Structurise.from_struct(self),
        [template_id: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_x(Evision.LineMod.Match.t()) :: integer()
  def get_x(self) do
    :evision_nif.linemod_Match_get_x(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_x(Evision.LineMod.Match.t(), integer()) :: Evision.LineMod.Match.t()
  def set_x(self, prop) do
    :evision_nif.linemod_Match_set_x(
        Evision.Internal.Structurise.from_struct(self),
        [x: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_y(Evision.LineMod.Match.t()) :: integer()
  def get_y(self) do
    :evision_nif.linemod_Match_get_y(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_y(Evision.LineMod.Match.t(), integer()) :: Evision.LineMod.Match.t()
  def set_y(self, prop) do
    :evision_nif.linemod_Match_set_y(
        Evision.Internal.Structurise.from_struct(self),
        [y: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
