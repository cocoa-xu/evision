defmodule Evision.LineMod.Template do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineMod.Template` struct.

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
  def to_struct({:ok, %{class: Evision.LineMod.Template, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineMod.Template, ref: ref}) do
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
  @spec get_features(Evision.LineMod.Template.t()) :: list(Evision.LineMod.Feature.t())
  def get_features(self) do
    :evision_nif.linemod_Template_get_features(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_height(Evision.LineMod.Template.t()) :: integer()
  def get_height(self) do
    :evision_nif.linemod_Template_get_height(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_pyramid_level(Evision.LineMod.Template.t()) :: integer()
  def get_pyramid_level(self) do
    :evision_nif.linemod_Template_get_pyramid_level(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_width(Evision.LineMod.Template.t()) :: integer()
  def get_width(self) do
    :evision_nif.linemod_Template_get_width(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
end
