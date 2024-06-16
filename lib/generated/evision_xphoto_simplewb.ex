defmodule Evision.XPhoto.SimpleWB do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XPhoto.SimpleWB` struct.

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
  def to_struct({:ok, %{class: Evision.XPhoto.SimpleWB, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XPhoto.SimpleWB, ref: ref}) do
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
  Input image range maximum value

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.SimpleWB.t()`

  ##### Return
  - **retval**: `float`

  @see `setInputMax/2`

  Python prototype (for reference only):
  ```python3
  getInputMax() -> retval
  ```
  """
  @spec getInputMax(Evision.XPhoto.SimpleWB.t()) :: number() | {:error, String.t()}
  def getInputMax(self) do
    positional = [
    ]
    :evision_nif.xphoto_xphoto_SimpleWB_getInputMax(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Input image range minimum value

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.SimpleWB.t()`

  ##### Return
  - **retval**: `float`

  @see `setInputMin/2`

  Python prototype (for reference only):
  ```python3
  getInputMin() -> retval
  ```
  """
  @spec getInputMin(Evision.XPhoto.SimpleWB.t()) :: number() | {:error, String.t()}
  def getInputMin(self) do
    positional = [
    ]
    :evision_nif.xphoto_xphoto_SimpleWB_getInputMin(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Output image range maximum value

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.SimpleWB.t()`

  ##### Return
  - **retval**: `float`

  @see `setOutputMax/2`

  Python prototype (for reference only):
  ```python3
  getOutputMax() -> retval
  ```
  """
  @spec getOutputMax(Evision.XPhoto.SimpleWB.t()) :: number() | {:error, String.t()}
  def getOutputMax(self) do
    positional = [
    ]
    :evision_nif.xphoto_xphoto_SimpleWB_getOutputMax(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Output image range minimum value

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.SimpleWB.t()`

  ##### Return
  - **retval**: `float`

  @see `setOutputMin/2`

  Python prototype (for reference only):
  ```python3
  getOutputMin() -> retval
  ```
  """
  @spec getOutputMin(Evision.XPhoto.SimpleWB.t()) :: number() | {:error, String.t()}
  def getOutputMin(self) do
    positional = [
    ]
    :evision_nif.xphoto_xphoto_SimpleWB_getOutputMin(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Percent of top/bottom values to ignore

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.SimpleWB.t()`

  ##### Return
  - **retval**: `float`

  @see `setP/2`

  Python prototype (for reference only):
  ```python3
  getP() -> retval
  ```
  """
  @spec getP(Evision.XPhoto.SimpleWB.t()) :: number() | {:error, String.t()}
  def getP(self) do
    positional = [
    ]
    :evision_nif.xphoto_xphoto_SimpleWB_getP(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setInputMax

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.SimpleWB.t()`
  - **val**: `float`

  @see `getInputMax/1`

  Python prototype (for reference only):
  ```python3
  setInputMax(val) -> None
  ```
  """
  @spec setInputMax(Evision.XPhoto.SimpleWB.t(), number()) :: Evision.XPhoto.SimpleWB.t() | {:error, String.t()}
  def setInputMax(self, val) when is_float(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.xphoto_xphoto_SimpleWB_setInputMax(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setInputMin

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.SimpleWB.t()`
  - **val**: `float`

  @see `getInputMin/1`

  Python prototype (for reference only):
  ```python3
  setInputMin(val) -> None
  ```
  """
  @spec setInputMin(Evision.XPhoto.SimpleWB.t(), number()) :: Evision.XPhoto.SimpleWB.t() | {:error, String.t()}
  def setInputMin(self, val) when is_float(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.xphoto_xphoto_SimpleWB_setInputMin(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setOutputMax

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.SimpleWB.t()`
  - **val**: `float`

  @see `getOutputMax/1`

  Python prototype (for reference only):
  ```python3
  setOutputMax(val) -> None
  ```
  """
  @spec setOutputMax(Evision.XPhoto.SimpleWB.t(), number()) :: Evision.XPhoto.SimpleWB.t() | {:error, String.t()}
  def setOutputMax(self, val) when is_float(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.xphoto_xphoto_SimpleWB_setOutputMax(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setOutputMin

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.SimpleWB.t()`
  - **val**: `float`

  @see `getOutputMin/1`

  Python prototype (for reference only):
  ```python3
  setOutputMin(val) -> None
  ```
  """
  @spec setOutputMin(Evision.XPhoto.SimpleWB.t(), number()) :: Evision.XPhoto.SimpleWB.t() | {:error, String.t()}
  def setOutputMin(self, val) when is_float(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.xphoto_xphoto_SimpleWB_setOutputMin(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setP

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.SimpleWB.t()`
  - **val**: `float`

  @see `getP/1`

  Python prototype (for reference only):
  ```python3
  setP(val) -> None
  ```
  """
  @spec setP(Evision.XPhoto.SimpleWB.t(), number()) :: Evision.XPhoto.SimpleWB.t() | {:error, String.t()}
  def setP(self, val) when is_float(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.xphoto_xphoto_SimpleWB_setP(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
