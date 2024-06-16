defmodule Evision.LineDescriptor.DrawLinesMatchesFlags do
  @typedoc """
  Type that represents an `LineDescriptor.DrawLinesMatchesFlags` struct.

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
  def to_struct({:ok, %{class: "Evision.LineDescriptor.DrawLinesMatchesFlags", ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: "Evision.LineDescriptor.DrawLinesMatchesFlags", ref: ref}) do
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
end
