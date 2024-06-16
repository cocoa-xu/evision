defmodule Evision.Utils.Nested.OriginalClassName.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Utils.Nested.OriginalClassName.Params` struct.

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
  def to_struct({:ok, %{class: Evision.Utils.Nested.OriginalClassName.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Utils.Nested.OriginalClassName.Params, ref: ref}) do
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
  OriginalClassName_Params
  ##### Keyword Arguments
  - **int_param**: `integer()`.
  - **float_param**: `float`.

  ##### Return
  - **self**: `Evision.Utils.Nested.OriginalClassName.Params.t()`

  Python prototype (for reference only):
  ```python3
  OriginalClassName_Params([, int_param[, float_param]]) -> <utils_nested_OriginalClassName_Params object>
  ```
  """
  @spec utils_nested_OriginalClassName_Params([{:float_param, term()} | {:int_param, term()}] | nil) :: Evision.Utils.Nested.OriginalClassName.Params.t() | {:error, String.t()}
  def utils_nested_OriginalClassName_Params(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:float_param, :int_param])
    positional = [
    ]
    :evision_nif.utils_nested_utils_nested_OriginalClassName_Params_OriginalClassName_Params(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  OriginalClassName_Params
  ##### Keyword Arguments
  - **int_param**: `integer()`.
  - **float_param**: `float`.

  ##### Return
  - **self**: `Evision.Utils.Nested.OriginalClassName.Params.t()`

  Python prototype (for reference only):
  ```python3
  OriginalClassName_Params([, int_param[, float_param]]) -> <utils_nested_OriginalClassName_Params object>
  ```
  """
  @spec utils_nested_OriginalClassName_Params() :: Evision.Utils.Nested.OriginalClassName.Params.t() | {:error, String.t()}
  def utils_nested_OriginalClassName_Params() do
    positional = [
    ]
    :evision_nif.utils_nested_utils_nested_OriginalClassName_Params_OriginalClassName_Params(positional)
    |> to_struct()
  end
  @spec get_float_value(Evision.Utils.Nested.OriginalClassName.Params.t()) :: number()
  def get_float_value(self) do
    :evision_nif.utils_nested_OriginalClassName_Params_get_float_value(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_float_value(Evision.Utils.Nested.OriginalClassName.Params.t(), number()) :: Evision.Utils.Nested.OriginalClassName.Params.t()
  def set_float_value(self, prop) do
    :evision_nif.utils_nested_OriginalClassName_Params_set_float_value(
        Evision.Internal.Structurise.from_struct(self),
        [float_value: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_int_value(Evision.Utils.Nested.OriginalClassName.Params.t()) :: integer()
  def get_int_value(self) do
    :evision_nif.utils_nested_OriginalClassName_Params_get_int_value(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_int_value(Evision.Utils.Nested.OriginalClassName.Params.t(), integer()) :: Evision.Utils.Nested.OriginalClassName.Params.t()
  def set_int_value(self, prop) do
    :evision_nif.utils_nested_OriginalClassName_Params_set_int_value(
        Evision.Internal.Structurise.from_struct(self),
        [int_value: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
