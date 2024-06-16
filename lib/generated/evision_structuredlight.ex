defmodule Evision.StructuredLight do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `StructuredLight` struct.

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
  def to_struct({:ok, %{class: Evision.StructuredLight, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.StructuredLight, ref: ref}) do
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
  def cv_FTP, do: 0
  @doc enum: true
  def cv_PSP, do: 1
  @doc enum: true
  def cv_FAPS, do: 2
  @doc enum: true
  def cv_DECODE_3D_UNDERWORLD, do: 0

end
