defmodule Evision.AffineTransformer do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `AffineTransformer` struct.

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
  def to_struct({:ok, %{class: Evision.AffineTransformer, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.AffineTransformer, ref: ref}) do
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
  getFullAffine

  ##### Positional Arguments
  - **self**: `Evision.AffineTransformer.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getFullAffine() -> retval
  ```
  """
  @spec getFullAffine(Evision.AffineTransformer.t()) :: boolean() | {:error, String.t()}
  def getFullAffine(self) do
    positional = [
    ]
    :evision_nif.affineTransformer_getFullAffine(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setFullAffine

  ##### Positional Arguments
  - **self**: `Evision.AffineTransformer.t()`
  - **fullAffine**: `bool`

  Python prototype (for reference only):
  ```python3
  setFullAffine(fullAffine) -> None
  ```
  """
  @spec setFullAffine(Evision.AffineTransformer.t(), boolean()) :: Evision.AffineTransformer.t() | {:error, String.t()}
  def setFullAffine(self, fullAffine) when is_boolean(fullAffine)
  do
    positional = [
      fullAffine: Evision.Internal.Structurise.from_struct(fullAffine)
    ]
    :evision_nif.affineTransformer_setFullAffine(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
