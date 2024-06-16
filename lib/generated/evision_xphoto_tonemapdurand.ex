defmodule Evision.XPhoto.TonemapDurand do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XPhoto.TonemapDurand` struct.

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
  def to_struct({:ok, %{class: Evision.XPhoto.TonemapDurand, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XPhoto.TonemapDurand, ref: ref}) do
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
  getContrast

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.TonemapDurand.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getContrast() -> retval
  ```
  """
  @spec getContrast(Evision.XPhoto.TonemapDurand.t()) :: number() | {:error, String.t()}
  def getContrast(self) do
    positional = [
    ]
    :evision_nif.xphoto_xphoto_TonemapDurand_getContrast(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSaturation

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.TonemapDurand.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getSaturation() -> retval
  ```
  """
  @spec getSaturation(Evision.XPhoto.TonemapDurand.t()) :: number() | {:error, String.t()}
  def getSaturation(self) do
    positional = [
    ]
    :evision_nif.xphoto_xphoto_TonemapDurand_getSaturation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSigmaColor

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.TonemapDurand.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getSigmaColor() -> retval
  ```
  """
  @spec getSigmaColor(Evision.XPhoto.TonemapDurand.t()) :: number() | {:error, String.t()}
  def getSigmaColor(self) do
    positional = [
    ]
    :evision_nif.xphoto_xphoto_TonemapDurand_getSigmaColor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSigmaSpace

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.TonemapDurand.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getSigmaSpace() -> retval
  ```
  """
  @spec getSigmaSpace(Evision.XPhoto.TonemapDurand.t()) :: number() | {:error, String.t()}
  def getSigmaSpace(self) do
    positional = [
    ]
    :evision_nif.xphoto_xphoto_TonemapDurand_getSigmaSpace(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setContrast

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.TonemapDurand.t()`
  - **contrast**: `float`

  Python prototype (for reference only):
  ```python3
  setContrast(contrast) -> None
  ```
  """
  @spec setContrast(Evision.XPhoto.TonemapDurand.t(), number()) :: Evision.XPhoto.TonemapDurand.t() | {:error, String.t()}
  def setContrast(self, contrast) when is_float(contrast)
  do
    positional = [
      contrast: Evision.Internal.Structurise.from_struct(contrast)
    ]
    :evision_nif.xphoto_xphoto_TonemapDurand_setContrast(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSaturation

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.TonemapDurand.t()`
  - **saturation**: `float`

  Python prototype (for reference only):
  ```python3
  setSaturation(saturation) -> None
  ```
  """
  @spec setSaturation(Evision.XPhoto.TonemapDurand.t(), number()) :: Evision.XPhoto.TonemapDurand.t() | {:error, String.t()}
  def setSaturation(self, saturation) when is_float(saturation)
  do
    positional = [
      saturation: Evision.Internal.Structurise.from_struct(saturation)
    ]
    :evision_nif.xphoto_xphoto_TonemapDurand_setSaturation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSigmaColor

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.TonemapDurand.t()`
  - **sigma_color**: `float`

  Python prototype (for reference only):
  ```python3
  setSigmaColor(sigma_color) -> None
  ```
  """
  @spec setSigmaColor(Evision.XPhoto.TonemapDurand.t(), number()) :: Evision.XPhoto.TonemapDurand.t() | {:error, String.t()}
  def setSigmaColor(self, sigma_color) when is_float(sigma_color)
  do
    positional = [
      sigma_color: Evision.Internal.Structurise.from_struct(sigma_color)
    ]
    :evision_nif.xphoto_xphoto_TonemapDurand_setSigmaColor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSigmaSpace

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.TonemapDurand.t()`
  - **sigma_space**: `float`

  Python prototype (for reference only):
  ```python3
  setSigmaSpace(sigma_space) -> None
  ```
  """
  @spec setSigmaSpace(Evision.XPhoto.TonemapDurand.t(), number()) :: Evision.XPhoto.TonemapDurand.t() | {:error, String.t()}
  def setSigmaSpace(self, sigma_space) when is_float(sigma_space)
  do
    positional = [
      sigma_space: Evision.Internal.Structurise.from_struct(sigma_space)
    ]
    :evision_nif.xphoto_xphoto_TonemapDurand_setSigmaSpace(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
