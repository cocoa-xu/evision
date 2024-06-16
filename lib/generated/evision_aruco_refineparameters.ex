defmodule Evision.ArUco.RefineParameters do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ArUco.RefineParameters` struct.

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
  def to_struct({:ok, %{class: Evision.ArUco.RefineParameters, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ArUco.RefineParameters, ref: ref}) do
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
  RefineParameters
  ##### Keyword Arguments
  - **minRepDistance**: `float`.
  - **errorCorrectionRate**: `float`.
  - **checkAllOrders**: `bool`.

  ##### Return
  - **self**: `RefineParameters`

  Python prototype (for reference only):
  ```python3
  RefineParameters([, minRepDistance[, errorCorrectionRate[, checkAllOrders]]]) -> <aruco_RefineParameters object>
  ```
  """
  @spec refineParameters([{:checkAllOrders, term()} | {:errorCorrectionRate, term()} | {:minRepDistance, term()}] | nil) :: Evision.ArUco.RefineParameters.t() | {:error, String.t()}
  def refineParameters(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:checkAllOrders, :errorCorrectionRate, :minRepDistance])
    positional = [
    ]
    :evision_nif.aruco_aruco_RefineParameters_RefineParameters(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  RefineParameters
  ##### Keyword Arguments
  - **minRepDistance**: `float`.
  - **errorCorrectionRate**: `float`.
  - **checkAllOrders**: `bool`.

  ##### Return
  - **self**: `RefineParameters`

  Python prototype (for reference only):
  ```python3
  RefineParameters([, minRepDistance[, errorCorrectionRate[, checkAllOrders]]]) -> <aruco_RefineParameters object>
  ```
  """
  @spec refineParameters() :: Evision.ArUco.RefineParameters.t() | {:error, String.t()}
  def refineParameters() do
    positional = [
    ]
    :evision_nif.aruco_aruco_RefineParameters_RefineParameters(positional)
    |> to_struct()
  end

  @doc """
  Read a new set of RefineParameters from FileNode (use FileStorage.root()).

  ##### Positional Arguments
  - **self**: `Evision.ArUco.RefineParameters.t()`
  - **func**: `Evision.FileNode`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  readRefineParameters(fn) -> retval
  ```
  """
  @spec readRefineParameters(Evision.ArUco.RefineParameters.t(), Evision.FileNode.t()) :: boolean() | {:error, String.t()}
  def readRefineParameters(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.aruco_aruco_RefineParameters_readRefineParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Write a set of RefineParameters to FileStorage

  ##### Positional Arguments
  - **self**: `Evision.ArUco.RefineParameters.t()`
  - **fs**: `Evision.FileStorage`

  ##### Keyword Arguments
  - **name**: `String`.

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  writeRefineParameters(fs[, name]) -> retval
  ```
  """
  @spec writeRefineParameters(Evision.ArUco.RefineParameters.t(), Evision.FileStorage.t(), [{:name, term()}] | nil) :: boolean() | {:error, String.t()}
  def writeRefineParameters(self, fs, opts) when is_struct(fs, Evision.FileStorage) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:name])
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.aruco_aruco_RefineParameters_writeRefineParameters(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Write a set of RefineParameters to FileStorage

  ##### Positional Arguments
  - **self**: `Evision.ArUco.RefineParameters.t()`
  - **fs**: `Evision.FileStorage`

  ##### Keyword Arguments
  - **name**: `String`.

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  writeRefineParameters(fs[, name]) -> retval
  ```
  """
  @spec writeRefineParameters(Evision.ArUco.RefineParameters.t(), Evision.FileStorage.t()) :: boolean() | {:error, String.t()}
  def writeRefineParameters(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.aruco_aruco_RefineParameters_writeRefineParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_checkAllOrders(Evision.ArUco.RefineParameters.t()) :: boolean()
  def get_checkAllOrders(self) do
    :evision_nif.aruco_RefineParameters_get_checkAllOrders(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_checkAllOrders(Evision.ArUco.RefineParameters.t(), boolean()) :: Evision.ArUco.RefineParameters.t()
  def set_checkAllOrders(self, prop) do
    :evision_nif.aruco_RefineParameters_set_checkAllOrders(
        Evision.Internal.Structurise.from_struct(self),
        [checkAllOrders: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_errorCorrectionRate(Evision.ArUco.RefineParameters.t()) :: number()
  def get_errorCorrectionRate(self) do
    :evision_nif.aruco_RefineParameters_get_errorCorrectionRate(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_errorCorrectionRate(Evision.ArUco.RefineParameters.t(), number()) :: Evision.ArUco.RefineParameters.t()
  def set_errorCorrectionRate(self, prop) do
    :evision_nif.aruco_RefineParameters_set_errorCorrectionRate(
        Evision.Internal.Structurise.from_struct(self),
        [errorCorrectionRate: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minRepDistance(Evision.ArUco.RefineParameters.t()) :: number()
  def get_minRepDistance(self) do
    :evision_nif.aruco_RefineParameters_get_minRepDistance(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minRepDistance(Evision.ArUco.RefineParameters.t(), number()) :: Evision.ArUco.RefineParameters.t()
  def set_minRepDistance(self, prop) do
    :evision_nif.aruco_RefineParameters_set_minRepDistance(
        Evision.Internal.Structurise.from_struct(self),
        [minRepDistance: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
