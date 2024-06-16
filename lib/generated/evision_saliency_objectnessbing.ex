defmodule Evision.Saliency.ObjectnessBING do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Saliency.ObjectnessBING` struct.

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
  def to_struct({:ok, %{class: Evision.Saliency.ObjectnessBING, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Saliency.ObjectnessBING, ref: ref}) do
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
  computeSaliency

  ##### Positional Arguments
  - **self**: `Evision.Saliency.ObjectnessBING.t()`
  - **image**: `Evision.Mat`

  ##### Return
  - **retval**: `bool`
  - **saliencyMap**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  computeSaliency(image[, saliencyMap]) -> retval, saliencyMap
  ```
  """
  @spec computeSaliency(Evision.Saliency.ObjectnessBING.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def computeSaliency(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_computeSaliency(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  computeSaliency

  ##### Positional Arguments
  - **self**: `Evision.Saliency.ObjectnessBING.t()`
  - **image**: `Evision.Mat`

  ##### Return
  - **retval**: `bool`
  - **saliencyMap**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  computeSaliency(image[, saliencyMap]) -> retval, saliencyMap
  ```
  """
  @spec computeSaliency(Evision.Saliency.ObjectnessBING.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | false | {:error, String.t()}
  def computeSaliency(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_computeSaliency(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Return
  - **retval**: `ObjectnessBING`

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.Saliency.ObjectnessBING.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_create_static(positional)
    |> to_struct()
  end

  @doc """
  getBase

  ##### Positional Arguments
  - **self**: `Evision.Saliency.ObjectnessBING.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getBase() -> retval
  ```
  """
  @spec getBase(Evision.Saliency.ObjectnessBING.t()) :: number() | {:error, String.t()}
  def getBase(self) do
    positional = [
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_getBase(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNSS

  ##### Positional Arguments
  - **self**: `Evision.Saliency.ObjectnessBING.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNSS() -> retval
  ```
  """
  @spec getNSS(Evision.Saliency.ObjectnessBING.t()) :: integer() | {:error, String.t()}
  def getNSS(self) do
    positional = [
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_getNSS(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getW

  ##### Positional Arguments
  - **self**: `Evision.Saliency.ObjectnessBING.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getW() -> retval
  ```
  """
  @spec getW(Evision.Saliency.ObjectnessBING.t()) :: integer() | {:error, String.t()}
  def getW(self) do
    positional = [
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_getW(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Return the list of the rectangles' objectness value,

  ##### Positional Arguments
  - **self**: `Evision.Saliency.ObjectnessBING.t()`

  ##### Return
  - **retval**: `[float]`

  in the same order as the *vector\\<Vec4i\\> objectnessBoundingBox* returned by the algorithm (in
  computeSaliencyImpl function). The bigger value these scores are, it is more likely to be an
  object window.

  Python prototype (for reference only):
  ```python3
  getobjectnessValues() -> retval
  ```
  """
  @spec getobjectnessValues(Evision.Saliency.ObjectnessBING.t()) :: list(number()) | {:error, String.t()}
  def getobjectnessValues(self) do
    positional = [
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_getobjectnessValues(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  read

  ##### Positional Arguments
  - **self**: `Evision.Saliency.ObjectnessBING.t()`

  Python prototype (for reference only):
  ```python3
  read() -> None
  ```
  """
  @spec read(Evision.Saliency.ObjectnessBING.t()) :: Evision.Saliency.ObjectnessBING.t() | {:error, String.t()}
  def read(self) do
    positional = [
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  This is a utility function that allows to set an arbitrary path in which the algorithm will save the
  optional results

  ##### Positional Arguments
  - **self**: `Evision.Saliency.ObjectnessBING.t()`
  - **resultsDir**: `String`.

    results' folder path

  (ie writing on file the total number and the list of rectangles returned by objectess, one for
  each row).

  Python prototype (for reference only):
  ```python3
  setBBResDir(resultsDir) -> None
  ```
  """
  @spec setBBResDir(Evision.Saliency.ObjectnessBING.t(), binary()) :: Evision.Saliency.ObjectnessBING.t() | {:error, String.t()}
  def setBBResDir(self, resultsDir) when is_binary(resultsDir)
  do
    positional = [
      resultsDir: Evision.Internal.Structurise.from_struct(resultsDir)
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_setBBResDir(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBase

  ##### Positional Arguments
  - **self**: `Evision.Saliency.ObjectnessBING.t()`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  setBase(val) -> None
  ```
  """
  @spec setBase(Evision.Saliency.ObjectnessBING.t(), number()) :: Evision.Saliency.ObjectnessBING.t() | {:error, String.t()}
  def setBase(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_setBase(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNSS

  ##### Positional Arguments
  - **self**: `Evision.Saliency.ObjectnessBING.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNSS(val) -> None
  ```
  """
  @spec setNSS(Evision.Saliency.ObjectnessBING.t(), integer()) :: Evision.Saliency.ObjectnessBING.t() | {:error, String.t()}
  def setNSS(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_setNSS(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  This is a utility function that allows to set the correct path from which the algorithm will load
  the trained model.

  ##### Positional Arguments
  - **self**: `Evision.Saliency.ObjectnessBING.t()`
  - **trainingPath**: `String`.

    trained model path

  Python prototype (for reference only):
  ```python3
  setTrainingPath(trainingPath) -> None
  ```
  """
  @spec setTrainingPath(Evision.Saliency.ObjectnessBING.t(), binary()) :: Evision.Saliency.ObjectnessBING.t() | {:error, String.t()}
  def setTrainingPath(self, trainingPath) when is_binary(trainingPath)
  do
    positional = [
      trainingPath: Evision.Internal.Structurise.from_struct(trainingPath)
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_setTrainingPath(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setW

  ##### Positional Arguments
  - **self**: `Evision.Saliency.ObjectnessBING.t()`
  - **val**: `integer()`

  Python prototype (for reference only):
  ```python3
  setW(val) -> None
  ```
  """
  @spec setW(Evision.Saliency.ObjectnessBING.t(), integer()) :: Evision.Saliency.ObjectnessBING.t() | {:error, String.t()}
  def setW(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_setW(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.Saliency.ObjectnessBING.t()`

  Python prototype (for reference only):
  ```python3
  write() -> None
  ```
  """
  @spec write(Evision.Saliency.ObjectnessBING.t()) :: Evision.Saliency.ObjectnessBING.t() | {:error, String.t()}
  def write(self) do
    positional = [
    ]
    :evision_nif.saliency_saliency_ObjectnessBING_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
