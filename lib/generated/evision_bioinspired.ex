defmodule Evision.Bioinspired do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Bioinspired` struct.

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
  def to_struct({:ok, %{class: Evision.Bioinspired, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Bioinspired, ref: ref}) do
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
  @type enum :: integer()
  @doc enum: true
  def cv_RETINA_COLOR_RANDOM, do: 0
  @doc enum: true
  def cv_RETINA_COLOR_DIAGONAL, do: 1
  @doc enum: true
  def cv_RETINA_COLOR_BAYER, do: 2

end
