defmodule Evision.Face.BasicFaceRecognizer do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Face.BasicFaceRecognizer` struct.

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
  def to_struct({:ok, %{class: Evision.Face.BasicFaceRecognizer, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Face.BasicFaceRecognizer, ref: ref}) do
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
  getEigenValues

  ##### Positional Arguments
  - **self**: `Evision.Face.BasicFaceRecognizer.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getEigenValues() -> retval
  ```
  """
  @spec getEigenValues(Evision.Face.BasicFaceRecognizer.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getEigenValues(self) do
    positional = [
    ]
    :evision_nif.face_face_BasicFaceRecognizer_getEigenValues(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getEigenVectors

  ##### Positional Arguments
  - **self**: `Evision.Face.BasicFaceRecognizer.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getEigenVectors() -> retval
  ```
  """
  @spec getEigenVectors(Evision.Face.BasicFaceRecognizer.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getEigenVectors(self) do
    positional = [
    ]
    :evision_nif.face_face_BasicFaceRecognizer_getEigenVectors(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getLabels

  ##### Positional Arguments
  - **self**: `Evision.Face.BasicFaceRecognizer.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getLabels() -> retval
  ```
  """
  @spec getLabels(Evision.Face.BasicFaceRecognizer.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getLabels(self) do
    positional = [
    ]
    :evision_nif.face_face_BasicFaceRecognizer_getLabels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMean

  ##### Positional Arguments
  - **self**: `Evision.Face.BasicFaceRecognizer.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getMean() -> retval
  ```
  """
  @spec getMean(Evision.Face.BasicFaceRecognizer.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getMean(self) do
    positional = [
    ]
    :evision_nif.face_face_BasicFaceRecognizer_getMean(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNumComponents

  ##### Positional Arguments
  - **self**: `Evision.Face.BasicFaceRecognizer.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setNumComponents/2`

  Python prototype (for reference only):
  ```python3
  getNumComponents() -> retval
  ```
  """
  @spec getNumComponents(Evision.Face.BasicFaceRecognizer.t()) :: integer() | {:error, String.t()}
  def getNumComponents(self) do
    positional = [
    ]
    :evision_nif.face_face_BasicFaceRecognizer_getNumComponents(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getProjections

  ##### Positional Arguments
  - **self**: `Evision.Face.BasicFaceRecognizer.t()`

  ##### Return
  - **retval**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  getProjections() -> retval
  ```
  """
  @spec getProjections(Evision.Face.BasicFaceRecognizer.t()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getProjections(self) do
    positional = [
    ]
    :evision_nif.face_face_BasicFaceRecognizer_getProjections(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getThreshold

  ##### Positional Arguments
  - **self**: `Evision.Face.BasicFaceRecognizer.t()`

  ##### Return
  - **retval**: `double`

  @see `setThreshold/2`

  Python prototype (for reference only):
  ```python3
  getThreshold() -> retval
  ```
  """
  @spec getThreshold(Evision.Face.BasicFaceRecognizer.t()) :: number() | {:error, String.t()}
  def getThreshold(self) do
    positional = [
    ]
    :evision_nif.face_face_BasicFaceRecognizer_getThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNumComponents

  ##### Positional Arguments
  - **self**: `Evision.Face.BasicFaceRecognizer.t()`
  - **val**: `integer()`

  @see `getNumComponents/1`

  Python prototype (for reference only):
  ```python3
  setNumComponents(val) -> None
  ```
  """
  @spec setNumComponents(Evision.Face.BasicFaceRecognizer.t(), integer()) :: Evision.Face.BasicFaceRecognizer.t() | {:error, String.t()}
  def setNumComponents(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.face_face_BasicFaceRecognizer_setNumComponents(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setThreshold

  ##### Positional Arguments
  - **self**: `Evision.Face.BasicFaceRecognizer.t()`
  - **val**: `double`

  @see `getThreshold/1`

  Python prototype (for reference only):
  ```python3
  setThreshold(val) -> None
  ```
  """
  @spec setThreshold(Evision.Face.BasicFaceRecognizer.t(), number()) :: Evision.Face.BasicFaceRecognizer.t() | {:error, String.t()}
  def setThreshold(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.face_face_BasicFaceRecognizer_setThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
