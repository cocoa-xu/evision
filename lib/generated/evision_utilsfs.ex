defmodule Evision.UtilsFS do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `UtilsFS` struct.

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
  def to_struct({:ok, %{class: Evision.UtilsFS, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.UtilsFS, ref: ref}) do
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
  getCacheDirectoryForDownloads
  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  getCacheDirectoryForDownloads() -> retval
  ```
  """
  @spec getCacheDirectoryForDownloads() :: binary() | {:error, String.t()}
  def getCacheDirectoryForDownloads() do
    positional = [
    ]
    :evision_nif.utils_fs_getCacheDirectoryForDownloads(positional)
    |> to_struct()
  end
end
