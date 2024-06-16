defmodule Evision.Utils.ClassWithKeywordProperties do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Utils.ClassWithKeywordProperties` struct.

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
  def to_struct({:ok, %{class: Evision.Utils.ClassWithKeywordProperties, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Utils.ClassWithKeywordProperties, ref: ref}) do
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
  ClassWithKeywordProperties
  ##### Keyword Arguments
  - **lambda_arg**: `integer()`.
  - **except_arg**: `integer()`.

  ##### Return
  - **self**: `ClassWithKeywordProperties`

  Python prototype (for reference only):
  ```python3
  ClassWithKeywordProperties([, lambda_arg[, except_arg]]) -> <utils_ClassWithKeywordProperties object>
  ```
  """
  @spec classWithKeywordProperties([{:except_arg, term()} | {:lambda_arg, term()}] | nil) :: Evision.Utils.ClassWithKeywordProperties.t() | {:error, String.t()}
  def classWithKeywordProperties(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:except_arg, :lambda_arg])
    positional = [
    ]
    :evision_nif.utils_utils_ClassWithKeywordProperties_ClassWithKeywordProperties(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  ClassWithKeywordProperties
  ##### Keyword Arguments
  - **lambda_arg**: `integer()`.
  - **except_arg**: `integer()`.

  ##### Return
  - **self**: `ClassWithKeywordProperties`

  Python prototype (for reference only):
  ```python3
  ClassWithKeywordProperties([, lambda_arg[, except_arg]]) -> <utils_ClassWithKeywordProperties object>
  ```
  """
  @spec classWithKeywordProperties() :: Evision.Utils.ClassWithKeywordProperties.t() | {:error, String.t()}
  def classWithKeywordProperties() do
    positional = [
    ]
    :evision_nif.utils_utils_ClassWithKeywordProperties_ClassWithKeywordProperties(positional)
    |> to_struct()
  end
  @spec get_except(Evision.Utils.ClassWithKeywordProperties.t()) :: integer()
  def get_except(self) do
    :evision_nif.utils_ClassWithKeywordProperties_get_except(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_lambda(Evision.Utils.ClassWithKeywordProperties.t()) :: integer()
  def get_lambda(self) do
    :evision_nif.utils_ClassWithKeywordProperties_get_lambda(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_lambda(Evision.Utils.ClassWithKeywordProperties.t(), integer()) :: Evision.Utils.ClassWithKeywordProperties.t()
  def set_lambda(self, prop) do
    :evision_nif.utils_ClassWithKeywordProperties_set_lambda(
        Evision.Internal.Structurise.from_struct(self),
        [lambda: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
