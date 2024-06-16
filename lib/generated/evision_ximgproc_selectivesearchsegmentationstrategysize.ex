defmodule Evision.XImgProc.SelectiveSearchSegmentationStrategySize do
  @typedoc """
  Type that represents an `XImgProc.SelectiveSearchSegmentationStrategySize` struct.

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
  def to_struct({:ok, %{class: "Evision.XImgProc.SelectiveSearchSegmentationStrategySize", ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: "Evision.XImgProc.SelectiveSearchSegmentationStrategySize", ref: ref}) do
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
