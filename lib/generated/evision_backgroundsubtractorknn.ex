defmodule Evision.BackgroundSubtractorKNN do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `BackgroundSubtractorKNN` struct.

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
  def to_struct({:ok, %{class: Evision.BackgroundSubtractorKNN, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.BackgroundSubtractorKNN, ref: ref}) do
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
  Returns the shadow detection flag

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`

  ##### Return
  - **retval**: `bool`

  If true, the algorithm detects shadows and marks them. See createBackgroundSubtractorKNN for
  details.

  Python prototype (for reference only):
  ```python3
  getDetectShadows() -> retval
  ```
  """
  @spec getDetectShadows(Evision.BackgroundSubtractorKNN.t()) :: boolean() | {:error, String.t()}
  def getDetectShadows(self) do
    positional = [
    ]
    :evision_nif.backgroundSubtractorKNN_getDetectShadows(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the threshold on the squared distance between the pixel and the sample

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`

  ##### Return
  - **retval**: `double`

  The threshold on the squared distance between the pixel and the sample to decide whether a pixel is
  close to a data sample.

  Python prototype (for reference only):
  ```python3
  getDist2Threshold() -> retval
  ```
  """
  @spec getDist2Threshold(Evision.BackgroundSubtractorKNN.t()) :: number() | {:error, String.t()}
  def getDist2Threshold(self) do
    positional = [
    ]
    :evision_nif.backgroundSubtractorKNN_getDist2Threshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the number of last frames that affect the background model

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getHistory() -> retval
  ```
  """
  @spec getHistory(Evision.BackgroundSubtractorKNN.t()) :: integer() | {:error, String.t()}
  def getHistory(self) do
    positional = [
    ]
    :evision_nif.backgroundSubtractorKNN_getHistory(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the number of data samples in the background model

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNSamples() -> retval
  ```
  """
  @spec getNSamples(Evision.BackgroundSubtractorKNN.t()) :: integer() | {:error, String.t()}
  def getNSamples(self) do
    positional = [
    ]
    :evision_nif.backgroundSubtractorKNN_getNSamples(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the shadow threshold

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`

  ##### Return
  - **retval**: `double`

  A shadow is detected if pixel is a darker version of the background. The shadow threshold (Tau in
  the paper) is a threshold defining how much darker the shadow can be. Tau= 0.5 means that if a pixel
  is more than twice darker then it is not shadow. See Prati, Mikic, Trivedi and Cucchiara,
  Detecting Moving Shadows...*, IEEE PAMI,2003.

  Python prototype (for reference only):
  ```python3
  getShadowThreshold() -> retval
  ```
  """
  @spec getShadowThreshold(Evision.BackgroundSubtractorKNN.t()) :: number() | {:error, String.t()}
  def getShadowThreshold(self) do
    positional = [
    ]
    :evision_nif.backgroundSubtractorKNN_getShadowThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the shadow value

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`

  ##### Return
  - **retval**: `integer()`

  Shadow value is the value used to mark shadows in the foreground mask. Default value is 127. Value 0
  in the mask always means background, 255 means foreground.

  Python prototype (for reference only):
  ```python3
  getShadowValue() -> retval
  ```
  """
  @spec getShadowValue(Evision.BackgroundSubtractorKNN.t()) :: integer() | {:error, String.t()}
  def getShadowValue(self) do
    positional = [
    ]
    :evision_nif.backgroundSubtractorKNN_getShadowValue(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the number of neighbours, the k in the kNN.

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`

  ##### Return
  - **retval**: `integer()`

  K is the number of samples that need to be within dist2Threshold in order to decide that that
  pixel is matching the kNN background model.

  Python prototype (for reference only):
  ```python3
  getkNNSamples() -> retval
  ```
  """
  @spec getkNNSamples(Evision.BackgroundSubtractorKNN.t()) :: integer() | {:error, String.t()}
  def getkNNSamples(self) do
    positional = [
    ]
    :evision_nif.backgroundSubtractorKNN_getkNNSamples(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Enables or disables shadow detection

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`
  - **detectShadows**: `bool`

  Python prototype (for reference only):
  ```python3
  setDetectShadows(detectShadows) -> None
  ```
  """
  @spec setDetectShadows(Evision.BackgroundSubtractorKNN.t(), boolean()) :: Evision.BackgroundSubtractorKNN.t() | {:error, String.t()}
  def setDetectShadows(self, detectShadows) when is_boolean(detectShadows)
  do
    positional = [
      detectShadows: Evision.Internal.Structurise.from_struct(detectShadows)
    ]
    :evision_nif.backgroundSubtractorKNN_setDetectShadows(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the threshold on the squared distance

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`
  - **dist2Threshold**: `double`

  Python prototype (for reference only):
  ```python3
  setDist2Threshold(_dist2Threshold) -> None
  ```
  """
  @spec setDist2Threshold(Evision.BackgroundSubtractorKNN.t(), number()) :: Evision.BackgroundSubtractorKNN.t() | {:error, String.t()}
  def setDist2Threshold(self, dist2Threshold) when is_number(dist2Threshold)
  do
    positional = [
      dist2Threshold: Evision.Internal.Structurise.from_struct(dist2Threshold)
    ]
    :evision_nif.backgroundSubtractorKNN_setDist2Threshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the number of last frames that affect the background model

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`
  - **history**: `integer()`

  Python prototype (for reference only):
  ```python3
  setHistory(history) -> None
  ```
  """
  @spec setHistory(Evision.BackgroundSubtractorKNN.t(), integer()) :: Evision.BackgroundSubtractorKNN.t() | {:error, String.t()}
  def setHistory(self, history) when is_integer(history)
  do
    positional = [
      history: Evision.Internal.Structurise.from_struct(history)
    ]
    :evision_nif.backgroundSubtractorKNN_setHistory(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the number of data samples in the background model.

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`
  - **nN**: `integer()`

  The model needs to be reinitalized to reserve memory.

  Python prototype (for reference only):
  ```python3
  setNSamples(_nN) -> None
  ```
  """
  @spec setNSamples(Evision.BackgroundSubtractorKNN.t(), integer()) :: Evision.BackgroundSubtractorKNN.t() | {:error, String.t()}
  def setNSamples(self, nN) when is_integer(nN)
  do
    positional = [
      nN: Evision.Internal.Structurise.from_struct(nN)
    ]
    :evision_nif.backgroundSubtractorKNN_setNSamples(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the shadow threshold

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`
  - **threshold**: `double`

  Python prototype (for reference only):
  ```python3
  setShadowThreshold(threshold) -> None
  ```
  """
  @spec setShadowThreshold(Evision.BackgroundSubtractorKNN.t(), number()) :: Evision.BackgroundSubtractorKNN.t() | {:error, String.t()}
  def setShadowThreshold(self, threshold) when is_number(threshold)
  do
    positional = [
      threshold: Evision.Internal.Structurise.from_struct(threshold)
    ]
    :evision_nif.backgroundSubtractorKNN_setShadowThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the shadow value

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`
  - **value**: `integer()`

  Python prototype (for reference only):
  ```python3
  setShadowValue(value) -> None
  ```
  """
  @spec setShadowValue(Evision.BackgroundSubtractorKNN.t(), integer()) :: Evision.BackgroundSubtractorKNN.t() | {:error, String.t()}
  def setShadowValue(self, value) when is_integer(value)
  do
    positional = [
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.backgroundSubtractorKNN_setShadowValue(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the k in the kNN. How many nearest neighbours need to match.

  ##### Positional Arguments
  - **self**: `Evision.BackgroundSubtractorKNN.t()`
  - **nkNN**: `integer()`

  Python prototype (for reference only):
  ```python3
  setkNNSamples(_nkNN) -> None
  ```
  """
  @spec setkNNSamples(Evision.BackgroundSubtractorKNN.t(), integer()) :: Evision.BackgroundSubtractorKNN.t() | {:error, String.t()}
  def setkNNSamples(self, nkNN) when is_integer(nkNN)
  do
    positional = [
      nkNN: Evision.Internal.Structurise.from_struct(nkNN)
    ]
    :evision_nif.backgroundSubtractorKNN_setkNNSamples(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
