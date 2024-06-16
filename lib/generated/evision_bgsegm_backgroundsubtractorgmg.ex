defmodule Evision.BgSegm.BackgroundSubtractorGMG do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `BgSegm.BackgroundSubtractorGMG` struct.

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
  def to_struct({:ok, %{class: Evision.BgSegm.BackgroundSubtractorGMG, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.BgSegm.BackgroundSubtractorGMG, ref: ref}) do
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
  Returns the prior probability that each individual pixel is a background pixel.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getBackgroundPrior() -> retval
  ```
  """
  @spec getBackgroundPrior(Evision.BgSegm.BackgroundSubtractorGMG.t()) :: number() | {:error, String.t()}
  def getBackgroundPrior(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_getBackgroundPrior(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the value of decision threshold.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`

  ##### Return
  - **retval**: `double`

  Decision value is the value above which pixel is determined to be FG.

  Python prototype (for reference only):
  ```python3
  getDecisionThreshold() -> retval
  ```
  """
  @spec getDecisionThreshold(Evision.BgSegm.BackgroundSubtractorGMG.t()) :: number() | {:error, String.t()}
  def getDecisionThreshold(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_getDecisionThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the learning rate of the algorithm.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`

  ##### Return
  - **retval**: `double`

  It lies between 0.0 and 1.0. It determines how quickly features are "forgotten" from
  histograms.

  Python prototype (for reference only):
  ```python3
  getDefaultLearningRate() -> retval
  ```
  """
  @spec getDefaultLearningRate(Evision.BgSegm.BackgroundSubtractorGMG.t()) :: number() | {:error, String.t()}
  def getDefaultLearningRate(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_getDefaultLearningRate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns total number of distinct colors to maintain in histogram.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxFeatures() -> retval
  ```
  """
  @spec getMaxFeatures(Evision.BgSegm.BackgroundSubtractorGMG.t()) :: integer() | {:error, String.t()}
  def getMaxFeatures(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_getMaxFeatures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the maximum value taken on by pixels in image sequence. e.g. 1.0 or 255.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxVal() -> retval
  ```
  """
  @spec getMaxVal(Evision.BgSegm.BackgroundSubtractorGMG.t()) :: number() | {:error, String.t()}
  def getMaxVal(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_getMaxVal(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the minimum value taken on by pixels in image sequence. Usually 0.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMinVal() -> retval
  ```
  """
  @spec getMinVal(Evision.BgSegm.BackgroundSubtractorGMG.t()) :: number() | {:error, String.t()}
  def getMinVal(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_getMinVal(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the number of frames used to initialize background model.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumFrames() -> retval
  ```
  """
  @spec getNumFrames(Evision.BgSegm.BackgroundSubtractorGMG.t()) :: integer() | {:error, String.t()}
  def getNumFrames(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_getNumFrames(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the parameter used for quantization of color-space.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`

  ##### Return
  - **retval**: `integer()`

  It is the number of discrete levels in each channel to be used in histograms.

  Python prototype (for reference only):
  ```python3
  getQuantizationLevels() -> retval
  ```
  """
  @spec getQuantizationLevels(Evision.BgSegm.BackgroundSubtractorGMG.t()) :: integer() | {:error, String.t()}
  def getQuantizationLevels(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_getQuantizationLevels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the kernel radius used for morphological operations

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getSmoothingRadius() -> retval
  ```
  """
  @spec getSmoothingRadius(Evision.BgSegm.BackgroundSubtractorGMG.t()) :: integer() | {:error, String.t()}
  def getSmoothingRadius(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_getSmoothingRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the status of background model update

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getUpdateBackgroundModel() -> retval
  ```
  """
  @spec getUpdateBackgroundModel(Evision.BgSegm.BackgroundSubtractorGMG.t()) :: boolean() | {:error, String.t()}
  def getUpdateBackgroundModel(self) do
    positional = [
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_getUpdateBackgroundModel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the prior probability that each individual pixel is a background pixel.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`
  - **bgprior**: `double`

  Python prototype (for reference only):
  ```python3
  setBackgroundPrior(bgprior) -> None
  ```
  """
  @spec setBackgroundPrior(Evision.BgSegm.BackgroundSubtractorGMG.t(), number()) :: Evision.BgSegm.BackgroundSubtractorGMG.t() | {:error, String.t()}
  def setBackgroundPrior(self, bgprior) when is_number(bgprior)
  do
    positional = [
      bgprior: Evision.Internal.Structurise.from_struct(bgprior)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_setBackgroundPrior(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the value of decision threshold.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`
  - **thresh**: `double`

  Python prototype (for reference only):
  ```python3
  setDecisionThreshold(thresh) -> None
  ```
  """
  @spec setDecisionThreshold(Evision.BgSegm.BackgroundSubtractorGMG.t(), number()) :: Evision.BgSegm.BackgroundSubtractorGMG.t() | {:error, String.t()}
  def setDecisionThreshold(self, thresh) when is_number(thresh)
  do
    positional = [
      thresh: Evision.Internal.Structurise.from_struct(thresh)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_setDecisionThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the learning rate of the algorithm.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`
  - **lr**: `double`

  Python prototype (for reference only):
  ```python3
  setDefaultLearningRate(lr) -> None
  ```
  """
  @spec setDefaultLearningRate(Evision.BgSegm.BackgroundSubtractorGMG.t(), number()) :: Evision.BgSegm.BackgroundSubtractorGMG.t() | {:error, String.t()}
  def setDefaultLearningRate(self, lr) when is_number(lr)
  do
    positional = [
      lr: Evision.Internal.Structurise.from_struct(lr)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_setDefaultLearningRate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets total number of distinct colors to maintain in histogram.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`
  - **maxFeatures**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxFeatures(maxFeatures) -> None
  ```
  """
  @spec setMaxFeatures(Evision.BgSegm.BackgroundSubtractorGMG.t(), integer()) :: Evision.BgSegm.BackgroundSubtractorGMG.t() | {:error, String.t()}
  def setMaxFeatures(self, maxFeatures) when is_integer(maxFeatures)
  do
    positional = [
      maxFeatures: Evision.Internal.Structurise.from_struct(maxFeatures)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_setMaxFeatures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the maximum value taken on by pixels in image sequence.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxVal(val) -> None
  ```
  """
  @spec setMaxVal(Evision.BgSegm.BackgroundSubtractorGMG.t(), number()) :: Evision.BgSegm.BackgroundSubtractorGMG.t() | {:error, String.t()}
  def setMaxVal(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_setMaxVal(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the minimum value taken on by pixels in image sequence.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`
  - **val**: `double`

  Python prototype (for reference only):
  ```python3
  setMinVal(val) -> None
  ```
  """
  @spec setMinVal(Evision.BgSegm.BackgroundSubtractorGMG.t(), number()) :: Evision.BgSegm.BackgroundSubtractorGMG.t() | {:error, String.t()}
  def setMinVal(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_setMinVal(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the number of frames used to initialize background model.

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`
  - **nframes**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNumFrames(nframes) -> None
  ```
  """
  @spec setNumFrames(Evision.BgSegm.BackgroundSubtractorGMG.t(), integer()) :: Evision.BgSegm.BackgroundSubtractorGMG.t() | {:error, String.t()}
  def setNumFrames(self, nframes) when is_integer(nframes)
  do
    positional = [
      nframes: Evision.Internal.Structurise.from_struct(nframes)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_setNumFrames(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the parameter used for quantization of color-space

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`
  - **nlevels**: `integer()`

  Python prototype (for reference only):
  ```python3
  setQuantizationLevels(nlevels) -> None
  ```
  """
  @spec setQuantizationLevels(Evision.BgSegm.BackgroundSubtractorGMG.t(), integer()) :: Evision.BgSegm.BackgroundSubtractorGMG.t() | {:error, String.t()}
  def setQuantizationLevels(self, nlevels) when is_integer(nlevels)
  do
    positional = [
      nlevels: Evision.Internal.Structurise.from_struct(nlevels)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_setQuantizationLevels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the kernel radius used for morphological operations

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`
  - **radius**: `integer()`

  Python prototype (for reference only):
  ```python3
  setSmoothingRadius(radius) -> None
  ```
  """
  @spec setSmoothingRadius(Evision.BgSegm.BackgroundSubtractorGMG.t(), integer()) :: Evision.BgSegm.BackgroundSubtractorGMG.t() | {:error, String.t()}
  def setSmoothingRadius(self, radius) when is_integer(radius)
  do
    positional = [
      radius: Evision.Internal.Structurise.from_struct(radius)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_setSmoothingRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets the status of background model update

  ##### Positional Arguments
  - **self**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`
  - **update**: `bool`

  Python prototype (for reference only):
  ```python3
  setUpdateBackgroundModel(update) -> None
  ```
  """
  @spec setUpdateBackgroundModel(Evision.BgSegm.BackgroundSubtractorGMG.t(), boolean()) :: Evision.BgSegm.BackgroundSubtractorGMG.t() | {:error, String.t()}
  def setUpdateBackgroundModel(self, update) when is_boolean(update)
  do
    positional = [
      update: Evision.Internal.Structurise.from_struct(update)
    ]
    :evision_nif.bgsegm_bgsegm_BackgroundSubtractorGMG_setUpdateBackgroundModel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
