defmodule Evision.ImgHash.ColorMomentHash do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ImgHash.ColorMomentHash` struct.

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
  def to_struct({:ok, %{class: Evision.ImgHash.ColorMomentHash, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ImgHash.ColorMomentHash, ref: ref}) do
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
  create
  ##### Return
  - **retval**: `Evision.ImgHash.ColorMomentHash.t()`

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.ImgHash.ColorMomentHash.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.img_hash_img_hash_ColorMomentHash_create_static(positional)
    |> to_struct()
  end
end
