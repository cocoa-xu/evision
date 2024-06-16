defmodule Evision.GeneralizedHoughGuil do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `GeneralizedHoughGuil` struct.

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
  def to_struct({:ok, %{class: Evision.GeneralizedHoughGuil, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.GeneralizedHoughGuil, ref: ref}) do
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
  getAngleEpsilon

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getAngleEpsilon() -> retval
  ```
  """
  @spec getAngleEpsilon(Evision.GeneralizedHoughGuil.t()) :: number() | {:error, String.t()}
  def getAngleEpsilon(self) do
    positional = [
    ]
    :evision_nif.generalizedHoughGuil_getAngleEpsilon(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getAngleStep

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getAngleStep() -> retval
  ```
  """
  @spec getAngleStep(Evision.GeneralizedHoughGuil.t()) :: number() | {:error, String.t()}
  def getAngleStep(self) do
    positional = [
    ]
    :evision_nif.generalizedHoughGuil_getAngleStep(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getAngleThresh

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getAngleThresh() -> retval
  ```
  """
  @spec getAngleThresh(Evision.GeneralizedHoughGuil.t()) :: integer() | {:error, String.t()}
  def getAngleThresh(self) do
    positional = [
    ]
    :evision_nif.generalizedHoughGuil_getAngleThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getLevels

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getLevels() -> retval
  ```
  """
  @spec getLevels(Evision.GeneralizedHoughGuil.t()) :: integer() | {:error, String.t()}
  def getLevels(self) do
    positional = [
    ]
    :evision_nif.generalizedHoughGuil_getLevels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxAngle

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxAngle() -> retval
  ```
  """
  @spec getMaxAngle(Evision.GeneralizedHoughGuil.t()) :: number() | {:error, String.t()}
  def getMaxAngle(self) do
    positional = [
    ]
    :evision_nif.generalizedHoughGuil_getMaxAngle(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxScale

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxScale() -> retval
  ```
  """
  @spec getMaxScale(Evision.GeneralizedHoughGuil.t()) :: number() | {:error, String.t()}
  def getMaxScale(self) do
    positional = [
    ]
    :evision_nif.generalizedHoughGuil_getMaxScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinAngle

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMinAngle() -> retval
  ```
  """
  @spec getMinAngle(Evision.GeneralizedHoughGuil.t()) :: number() | {:error, String.t()}
  def getMinAngle(self) do
    positional = [
    ]
    :evision_nif.generalizedHoughGuil_getMinAngle(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinScale

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMinScale() -> retval
  ```
  """
  @spec getMinScale(Evision.GeneralizedHoughGuil.t()) :: number() | {:error, String.t()}
  def getMinScale(self) do
    positional = [
    ]
    :evision_nif.generalizedHoughGuil_getMinScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getPosThresh

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getPosThresh() -> retval
  ```
  """
  @spec getPosThresh(Evision.GeneralizedHoughGuil.t()) :: integer() | {:error, String.t()}
  def getPosThresh(self) do
    positional = [
    ]
    :evision_nif.generalizedHoughGuil_getPosThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getScaleStep

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getScaleStep() -> retval
  ```
  """
  @spec getScaleStep(Evision.GeneralizedHoughGuil.t()) :: number() | {:error, String.t()}
  def getScaleStep(self) do
    positional = [
    ]
    :evision_nif.generalizedHoughGuil_getScaleStep(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getScaleThresh

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getScaleThresh() -> retval
  ```
  """
  @spec getScaleThresh(Evision.GeneralizedHoughGuil.t()) :: integer() | {:error, String.t()}
  def getScaleThresh(self) do
    positional = [
    ]
    :evision_nif.generalizedHoughGuil_getScaleThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getXi

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getXi() -> retval
  ```
  """
  @spec getXi(Evision.GeneralizedHoughGuil.t()) :: number() | {:error, String.t()}
  def getXi(self) do
    positional = [
    ]
    :evision_nif.generalizedHoughGuil_getXi(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setAngleEpsilon

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`
  - **angleEpsilon**: `double`

  Python prototype (for reference only):
  ```python3
  setAngleEpsilon(angleEpsilon) -> None
  ```
  """
  @spec setAngleEpsilon(Evision.GeneralizedHoughGuil.t(), number()) :: Evision.GeneralizedHoughGuil.t() | {:error, String.t()}
  def setAngleEpsilon(self, angleEpsilon) when is_number(angleEpsilon)
  do
    positional = [
      angleEpsilon: Evision.Internal.Structurise.from_struct(angleEpsilon)
    ]
    :evision_nif.generalizedHoughGuil_setAngleEpsilon(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setAngleStep

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`
  - **angleStep**: `double`

  Python prototype (for reference only):
  ```python3
  setAngleStep(angleStep) -> None
  ```
  """
  @spec setAngleStep(Evision.GeneralizedHoughGuil.t(), number()) :: Evision.GeneralizedHoughGuil.t() | {:error, String.t()}
  def setAngleStep(self, angleStep) when is_number(angleStep)
  do
    positional = [
      angleStep: Evision.Internal.Structurise.from_struct(angleStep)
    ]
    :evision_nif.generalizedHoughGuil_setAngleStep(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setAngleThresh

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`
  - **angleThresh**: `integer()`

  Python prototype (for reference only):
  ```python3
  setAngleThresh(angleThresh) -> None
  ```
  """
  @spec setAngleThresh(Evision.GeneralizedHoughGuil.t(), integer()) :: Evision.GeneralizedHoughGuil.t() | {:error, String.t()}
  def setAngleThresh(self, angleThresh) when is_integer(angleThresh)
  do
    positional = [
      angleThresh: Evision.Internal.Structurise.from_struct(angleThresh)
    ]
    :evision_nif.generalizedHoughGuil_setAngleThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setLevels

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`
  - **levels**: `integer()`

  Python prototype (for reference only):
  ```python3
  setLevels(levels) -> None
  ```
  """
  @spec setLevels(Evision.GeneralizedHoughGuil.t(), integer()) :: Evision.GeneralizedHoughGuil.t() | {:error, String.t()}
  def setLevels(self, levels) when is_integer(levels)
  do
    positional = [
      levels: Evision.Internal.Structurise.from_struct(levels)
    ]
    :evision_nif.generalizedHoughGuil_setLevels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxAngle

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`
  - **maxAngle**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxAngle(maxAngle) -> None
  ```
  """
  @spec setMaxAngle(Evision.GeneralizedHoughGuil.t(), number()) :: Evision.GeneralizedHoughGuil.t() | {:error, String.t()}
  def setMaxAngle(self, maxAngle) when is_number(maxAngle)
  do
    positional = [
      maxAngle: Evision.Internal.Structurise.from_struct(maxAngle)
    ]
    :evision_nif.generalizedHoughGuil_setMaxAngle(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxScale

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`
  - **maxScale**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxScale(maxScale) -> None
  ```
  """
  @spec setMaxScale(Evision.GeneralizedHoughGuil.t(), number()) :: Evision.GeneralizedHoughGuil.t() | {:error, String.t()}
  def setMaxScale(self, maxScale) when is_number(maxScale)
  do
    positional = [
      maxScale: Evision.Internal.Structurise.from_struct(maxScale)
    ]
    :evision_nif.generalizedHoughGuil_setMaxScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinAngle

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`
  - **minAngle**: `double`

  Python prototype (for reference only):
  ```python3
  setMinAngle(minAngle) -> None
  ```
  """
  @spec setMinAngle(Evision.GeneralizedHoughGuil.t(), number()) :: Evision.GeneralizedHoughGuil.t() | {:error, String.t()}
  def setMinAngle(self, minAngle) when is_number(minAngle)
  do
    positional = [
      minAngle: Evision.Internal.Structurise.from_struct(minAngle)
    ]
    :evision_nif.generalizedHoughGuil_setMinAngle(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinScale

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`
  - **minScale**: `double`

  Python prototype (for reference only):
  ```python3
  setMinScale(minScale) -> None
  ```
  """
  @spec setMinScale(Evision.GeneralizedHoughGuil.t(), number()) :: Evision.GeneralizedHoughGuil.t() | {:error, String.t()}
  def setMinScale(self, minScale) when is_number(minScale)
  do
    positional = [
      minScale: Evision.Internal.Structurise.from_struct(minScale)
    ]
    :evision_nif.generalizedHoughGuil_setMinScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPosThresh

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`
  - **posThresh**: `integer()`

  Python prototype (for reference only):
  ```python3
  setPosThresh(posThresh) -> None
  ```
  """
  @spec setPosThresh(Evision.GeneralizedHoughGuil.t(), integer()) :: Evision.GeneralizedHoughGuil.t() | {:error, String.t()}
  def setPosThresh(self, posThresh) when is_integer(posThresh)
  do
    positional = [
      posThresh: Evision.Internal.Structurise.from_struct(posThresh)
    ]
    :evision_nif.generalizedHoughGuil_setPosThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setScaleStep

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`
  - **scaleStep**: `double`

  Python prototype (for reference only):
  ```python3
  setScaleStep(scaleStep) -> None
  ```
  """
  @spec setScaleStep(Evision.GeneralizedHoughGuil.t(), number()) :: Evision.GeneralizedHoughGuil.t() | {:error, String.t()}
  def setScaleStep(self, scaleStep) when is_number(scaleStep)
  do
    positional = [
      scaleStep: Evision.Internal.Structurise.from_struct(scaleStep)
    ]
    :evision_nif.generalizedHoughGuil_setScaleStep(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setScaleThresh

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`
  - **scaleThresh**: `integer()`

  Python prototype (for reference only):
  ```python3
  setScaleThresh(scaleThresh) -> None
  ```
  """
  @spec setScaleThresh(Evision.GeneralizedHoughGuil.t(), integer()) :: Evision.GeneralizedHoughGuil.t() | {:error, String.t()}
  def setScaleThresh(self, scaleThresh) when is_integer(scaleThresh)
  do
    positional = [
      scaleThresh: Evision.Internal.Structurise.from_struct(scaleThresh)
    ]
    :evision_nif.generalizedHoughGuil_setScaleThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setXi

  ##### Positional Arguments
  - **self**: `Evision.GeneralizedHoughGuil.t()`
  - **xi**: `double`

  Python prototype (for reference only):
  ```python3
  setXi(xi) -> None
  ```
  """
  @spec setXi(Evision.GeneralizedHoughGuil.t(), number()) :: Evision.GeneralizedHoughGuil.t() | {:error, String.t()}
  def setXi(self, xi) when is_number(xi)
  do
    positional = [
      xi: Evision.Internal.Structurise.from_struct(xi)
    ]
    :evision_nif.generalizedHoughGuil_setXi(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
