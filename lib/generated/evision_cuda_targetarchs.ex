defmodule Evision.CUDA.TargetArchs do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.TargetArchs` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.TargetArchs, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.TargetArchs, ref: ref}) do
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
  There is a set of methods to check whether the module contains intermediate (PTX) or binary CUDA
  code for the given architecture(s):

  ##### Positional Arguments
  - **major**: `integer()`.

    Major compute capability version.

  - **minor**: `integer()`.

    Minor compute capability version.

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  has(major, minor) -> retval
  ```
  """
  @spec has(integer(), integer()) :: boolean() | {:error, String.t()}
  def has(major, minor) when is_integer(major) and is_integer(minor)
  do
    positional = [
      major: Evision.Internal.Structurise.from_struct(major),
      minor: Evision.Internal.Structurise.from_struct(minor)
    ]
    :evision_nif.cuda_cuda_TargetArchs_has_static(positional)
    |> to_struct()
  end

  @doc """
  hasBin

  ##### Positional Arguments
  - **major**: `integer()`
  - **minor**: `integer()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  hasBin(major, minor) -> retval
  ```
  """
  @spec hasBin(integer(), integer()) :: boolean() | {:error, String.t()}
  def hasBin(major, minor) when is_integer(major) and is_integer(minor)
  do
    positional = [
      major: Evision.Internal.Structurise.from_struct(major),
      minor: Evision.Internal.Structurise.from_struct(minor)
    ]
    :evision_nif.cuda_cuda_TargetArchs_hasBin_static(positional)
    |> to_struct()
  end

  @doc """
  hasEqualOrGreater

  ##### Positional Arguments
  - **major**: `integer()`
  - **minor**: `integer()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  hasEqualOrGreater(major, minor) -> retval
  ```
  """
  @spec hasEqualOrGreater(integer(), integer()) :: boolean() | {:error, String.t()}
  def hasEqualOrGreater(major, minor) when is_integer(major) and is_integer(minor)
  do
    positional = [
      major: Evision.Internal.Structurise.from_struct(major),
      minor: Evision.Internal.Structurise.from_struct(minor)
    ]
    :evision_nif.cuda_cuda_TargetArchs_hasEqualOrGreater_static(positional)
    |> to_struct()
  end

  @doc """
  hasEqualOrGreaterBin

  ##### Positional Arguments
  - **major**: `integer()`
  - **minor**: `integer()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  hasEqualOrGreaterBin(major, minor) -> retval
  ```
  """
  @spec hasEqualOrGreaterBin(integer(), integer()) :: boolean() | {:error, String.t()}
  def hasEqualOrGreaterBin(major, minor) when is_integer(major) and is_integer(minor)
  do
    positional = [
      major: Evision.Internal.Structurise.from_struct(major),
      minor: Evision.Internal.Structurise.from_struct(minor)
    ]
    :evision_nif.cuda_cuda_TargetArchs_hasEqualOrGreaterBin_static(positional)
    |> to_struct()
  end

  @doc """
  hasEqualOrGreaterPtx

  ##### Positional Arguments
  - **major**: `integer()`
  - **minor**: `integer()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  hasEqualOrGreaterPtx(major, minor) -> retval
  ```
  """
  @spec hasEqualOrGreaterPtx(integer(), integer()) :: boolean() | {:error, String.t()}
  def hasEqualOrGreaterPtx(major, minor) when is_integer(major) and is_integer(minor)
  do
    positional = [
      major: Evision.Internal.Structurise.from_struct(major),
      minor: Evision.Internal.Structurise.from_struct(minor)
    ]
    :evision_nif.cuda_cuda_TargetArchs_hasEqualOrGreaterPtx_static(positional)
    |> to_struct()
  end

  @doc """
  hasEqualOrLessPtx

  ##### Positional Arguments
  - **major**: `integer()`
  - **minor**: `integer()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  hasEqualOrLessPtx(major, minor) -> retval
  ```
  """
  @spec hasEqualOrLessPtx(integer(), integer()) :: boolean() | {:error, String.t()}
  def hasEqualOrLessPtx(major, minor) when is_integer(major) and is_integer(minor)
  do
    positional = [
      major: Evision.Internal.Structurise.from_struct(major),
      minor: Evision.Internal.Structurise.from_struct(minor)
    ]
    :evision_nif.cuda_cuda_TargetArchs_hasEqualOrLessPtx_static(positional)
    |> to_struct()
  end

  @doc """
  hasPtx

  ##### Positional Arguments
  - **major**: `integer()`
  - **minor**: `integer()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  hasPtx(major, minor) -> retval
  ```
  """
  @spec hasPtx(integer(), integer()) :: boolean() | {:error, String.t()}
  def hasPtx(major, minor) when is_integer(major) and is_integer(minor)
  do
    positional = [
      major: Evision.Internal.Structurise.from_struct(major),
      minor: Evision.Internal.Structurise.from_struct(minor)
    ]
    :evision_nif.cuda_cuda_TargetArchs_hasPtx_static(positional)
    |> to_struct()
  end
end
