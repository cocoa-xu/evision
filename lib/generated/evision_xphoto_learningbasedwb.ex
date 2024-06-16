defmodule Evision.XPhoto.LearningBasedWB do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XPhoto.LearningBasedWB` struct.

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
  def to_struct({:ok, %{class: Evision.XPhoto.LearningBasedWB, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XPhoto.LearningBasedWB, ref: ref}) do
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
  Implements the feature extraction part of the algorithm.

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.LearningBasedWB.t()`
  - **src**: `Evision.Mat`.

    Input three-channel image (BGR color space is assumed).

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    An array of four (r,g) chromaticity tuples corresponding to the features listed above.

  In accordance with @cite Cheng2015 , computes the following features for the input image:
  1. Chromaticity of an average (R,G,B) tuple
  2. Chromaticity of the brightest (R,G,B) tuple (while ignoring saturated pixels)
  3. Chromaticity of the dominant (R,G,B) tuple (the one that has the highest value in the RGB histogram)
  4. Mode of the chromaticity palette, that is constructed by taking 300 most common colors according to
  the RGB histogram and projecting them on the chromaticity plane. Mode is the most high-density point
  of the palette, which is computed by a straightforward fixed-bandwidth kernel density estimator with
  a Epanechnikov kernel function.

  Python prototype (for reference only):
  ```python3
  extractSimpleFeatures(src[, dst]) -> dst
  ```
  """
  @spec extractSimpleFeatures(Evision.XPhoto.LearningBasedWB.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def extractSimpleFeatures(self, src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.xphoto_xphoto_LearningBasedWB_extractSimpleFeatures(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Implements the feature extraction part of the algorithm.

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.LearningBasedWB.t()`
  - **src**: `Evision.Mat`.

    Input three-channel image (BGR color space is assumed).

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    An array of four (r,g) chromaticity tuples corresponding to the features listed above.

  In accordance with @cite Cheng2015 , computes the following features for the input image:
  1. Chromaticity of an average (R,G,B) tuple
  2. Chromaticity of the brightest (R,G,B) tuple (while ignoring saturated pixels)
  3. Chromaticity of the dominant (R,G,B) tuple (the one that has the highest value in the RGB histogram)
  4. Mode of the chromaticity palette, that is constructed by taking 300 most common colors according to
  the RGB histogram and projecting them on the chromaticity plane. Mode is the most high-density point
  of the palette, which is computed by a straightforward fixed-bandwidth kernel density estimator with
  a Epanechnikov kernel function.

  Python prototype (for reference only):
  ```python3
  extractSimpleFeatures(src[, dst]) -> dst
  ```
  """
  @spec extractSimpleFeatures(Evision.XPhoto.LearningBasedWB.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def extractSimpleFeatures(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.xphoto_xphoto_LearningBasedWB_extractSimpleFeatures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Defines the size of one dimension of a three-dimensional RGB histogram that is used internally
  by the algorithm. It often makes sense to increase the number of bins for images with higher bit depth
  (e.g. 256 bins for a 12 bit image).

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.LearningBasedWB.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setHistBinNum/2`

  Python prototype (for reference only):
  ```python3
  getHistBinNum() -> retval
  ```
  """
  @spec getHistBinNum(Evision.XPhoto.LearningBasedWB.t()) :: integer() | {:error, String.t()}
  def getHistBinNum(self) do
    positional = [
    ]
    :evision_nif.xphoto_xphoto_LearningBasedWB_getHistBinNum(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Maximum possible value of the input image (e.g. 255 for 8 bit images,
  4095 for 12 bit images)

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.LearningBasedWB.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setRangeMaxVal/2`

  Python prototype (for reference only):
  ```python3
  getRangeMaxVal() -> retval
  ```
  """
  @spec getRangeMaxVal(Evision.XPhoto.LearningBasedWB.t()) :: integer() | {:error, String.t()}
  def getRangeMaxVal(self) do
    positional = [
    ]
    :evision_nif.xphoto_xphoto_LearningBasedWB_getRangeMaxVal(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Threshold that is used to determine saturated pixels, i.e. pixels where at least one of the
  channels exceeds \\f$\\texttt{saturation\\_threshold}\\times\\texttt{range\\_max\\_val}\\f$ are ignored.

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.LearningBasedWB.t()`

  ##### Return
  - **retval**: `float`

  @see `setSaturationThreshold/2`

  Python prototype (for reference only):
  ```python3
  getSaturationThreshold() -> retval
  ```
  """
  @spec getSaturationThreshold(Evision.XPhoto.LearningBasedWB.t()) :: number() | {:error, String.t()}
  def getSaturationThreshold(self) do
    positional = [
    ]
    :evision_nif.xphoto_xphoto_LearningBasedWB_getSaturationThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setHistBinNum

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.LearningBasedWB.t()`
  - **val**: `integer()`

  @see `getHistBinNum/1`

  Python prototype (for reference only):
  ```python3
  setHistBinNum(val) -> None
  ```
  """
  @spec setHistBinNum(Evision.XPhoto.LearningBasedWB.t(), integer()) :: Evision.XPhoto.LearningBasedWB.t() | {:error, String.t()}
  def setHistBinNum(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.xphoto_xphoto_LearningBasedWB_setHistBinNum(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRangeMaxVal

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.LearningBasedWB.t()`
  - **val**: `integer()`

  @see `getRangeMaxVal/1`

  Python prototype (for reference only):
  ```python3
  setRangeMaxVal(val) -> None
  ```
  """
  @spec setRangeMaxVal(Evision.XPhoto.LearningBasedWB.t(), integer()) :: Evision.XPhoto.LearningBasedWB.t() | {:error, String.t()}
  def setRangeMaxVal(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.xphoto_xphoto_LearningBasedWB_setRangeMaxVal(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSaturationThreshold

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.LearningBasedWB.t()`
  - **val**: `float`

  @see `getSaturationThreshold/1`

  Python prototype (for reference only):
  ```python3
  setSaturationThreshold(val) -> None
  ```
  """
  @spec setSaturationThreshold(Evision.XPhoto.LearningBasedWB.t(), number()) :: Evision.XPhoto.LearningBasedWB.t() | {:error, String.t()}
  def setSaturationThreshold(self, val) when is_float(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.xphoto_xphoto_LearningBasedWB_setSaturationThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
