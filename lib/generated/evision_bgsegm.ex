defmodule Evision.BgSegm do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `BgSegm` struct.

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
  def to_struct({:ok, %{class: Evision.BgSegm, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.BgSegm, ref: ref}) do
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
  Creates a CNT Background Subtractor
  ##### Keyword Arguments
  - **minPixelStability**: `integer()`.

    number of frames with same pixel color to consider stable

  - **useHistory**: `bool`.

    determines if we're giving a pixel credit for being stable for a long time

  - **maxPixelStability**: `integer()`.

    maximum allowed credit for a pixel in history

  - **isParallel**: `bool`.

    determines if we're parallelizing the algorithm

  ##### Return
  - **retval**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorCNT([, minPixelStability[, useHistory[, maxPixelStability[, isParallel]]]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorCNT([{:isParallel, term()} | {:maxPixelStability, term()} | {:minPixelStability, term()} | {:useHistory, term()}] | nil) :: Evision.BgSegm.BackgroundSubtractorCNT.t() | {:error, String.t()}
  def createBackgroundSubtractorCNT(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:isParallel, :maxPixelStability, :minPixelStability, :useHistory])
    positional = [
    ]
    :evision_nif.bgsegm_createBackgroundSubtractorCNT(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a CNT Background Subtractor
  ##### Keyword Arguments
  - **minPixelStability**: `integer()`.

    number of frames with same pixel color to consider stable

  - **useHistory**: `bool`.

    determines if we're giving a pixel credit for being stable for a long time

  - **maxPixelStability**: `integer()`.

    maximum allowed credit for a pixel in history

  - **isParallel**: `bool`.

    determines if we're parallelizing the algorithm

  ##### Return
  - **retval**: `Evision.BgSegm.BackgroundSubtractorCNT.t()`

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorCNT([, minPixelStability[, useHistory[, maxPixelStability[, isParallel]]]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorCNT() :: Evision.BgSegm.BackgroundSubtractorCNT.t() | {:error, String.t()}
  def createBackgroundSubtractorCNT() do
    positional = [
    ]
    :evision_nif.bgsegm_createBackgroundSubtractorCNT(positional)
    |> to_struct()
  end

  @doc """
  Creates a GMG Background Subtractor
  ##### Keyword Arguments
  - **initializationFrames**: `integer()`.

    number of frames used to initialize the background models.

  - **decisionThreshold**: `double`.

    Threshold value, above which it is marked foreground, else background.

  ##### Return
  - **retval**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorGMG([, initializationFrames[, decisionThreshold]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorGMG([{:decisionThreshold, term()} | {:initializationFrames, term()}] | nil) :: Evision.BgSegm.BackgroundSubtractorGMG.t() | {:error, String.t()}
  def createBackgroundSubtractorGMG(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:decisionThreshold, :initializationFrames])
    positional = [
    ]
    :evision_nif.bgsegm_createBackgroundSubtractorGMG(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a GMG Background Subtractor
  ##### Keyword Arguments
  - **initializationFrames**: `integer()`.

    number of frames used to initialize the background models.

  - **decisionThreshold**: `double`.

    Threshold value, above which it is marked foreground, else background.

  ##### Return
  - **retval**: `Evision.BgSegm.BackgroundSubtractorGMG.t()`

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorGMG([, initializationFrames[, decisionThreshold]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorGMG() :: Evision.BgSegm.BackgroundSubtractorGMG.t() | {:error, String.t()}
  def createBackgroundSubtractorGMG() do
    positional = [
    ]
    :evision_nif.bgsegm_createBackgroundSubtractorGMG(positional)
    |> to_struct()
  end

  @doc """
  Creates an instance of BackgroundSubtractorGSOC algorithm.
  ##### Keyword Arguments
  - **mc**: `integer()`.

    Whether to use camera motion compensation.

  - **nSamples**: `integer()`.

    Number of samples to maintain at each point of the frame.

  - **replaceRate**: `float`.

    Probability of replacing the old sample - how fast the model will update itself.

  - **propagationRate**: `float`.

    Probability of propagating to neighbors.

  - **hitsThreshold**: `integer()`.

    How many positives the sample must get before it will be considered as a possible replacement.

  - **alpha**: `float`.

    Scale coefficient for threshold.

  - **beta**: `float`.

    Bias coefficient for threshold.

  - **blinkingSupressionDecay**: `float`.

    Blinking supression decay factor.

  - **blinkingSupressionMultiplier**: `float`.

    Blinking supression multiplier.

  - **noiseRemovalThresholdFacBG**: `float`.

    Strength of the noise removal for background points.

  - **noiseRemovalThresholdFacFG**: `float`.

    Strength of the noise removal for foreground points.

  ##### Return
  - **retval**: `Evision.BgSegm.BackgroundSubtractorGSOC.t()`

  Implementation of the different yet better algorithm which is called GSOC, as it was implemented during GSOC and was not originated from any paper.

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorGSOC([, mc[, nSamples[, replaceRate[, propagationRate[, hitsThreshold[, alpha[, beta[, blinkingSupressionDecay[, blinkingSupressionMultiplier[, noiseRemovalThresholdFacBG[, noiseRemovalThresholdFacFG]]]]]]]]]]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorGSOC([{:alpha, term()} | {:beta, term()} | {:blinkingSupressionDecay, term()} | {:blinkingSupressionMultiplier, term()} | {:hitsThreshold, term()} | {:mc, term()} | {:nSamples, term()} | {:noiseRemovalThresholdFacBG, term()} | {:noiseRemovalThresholdFacFG, term()} | {:propagationRate, term()} | {:replaceRate, term()}] | nil) :: Evision.BgSegm.BackgroundSubtractorGSOC.t() | {:error, String.t()}
  def createBackgroundSubtractorGSOC(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:alpha, :beta, :blinkingSupressionDecay, :blinkingSupressionMultiplier, :hitsThreshold, :mc, :nSamples, :noiseRemovalThresholdFacBG, :noiseRemovalThresholdFacFG, :propagationRate, :replaceRate])
    positional = [
    ]
    :evision_nif.bgsegm_createBackgroundSubtractorGSOC(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates an instance of BackgroundSubtractorGSOC algorithm.
  ##### Keyword Arguments
  - **mc**: `integer()`.

    Whether to use camera motion compensation.

  - **nSamples**: `integer()`.

    Number of samples to maintain at each point of the frame.

  - **replaceRate**: `float`.

    Probability of replacing the old sample - how fast the model will update itself.

  - **propagationRate**: `float`.

    Probability of propagating to neighbors.

  - **hitsThreshold**: `integer()`.

    How many positives the sample must get before it will be considered as a possible replacement.

  - **alpha**: `float`.

    Scale coefficient for threshold.

  - **beta**: `float`.

    Bias coefficient for threshold.

  - **blinkingSupressionDecay**: `float`.

    Blinking supression decay factor.

  - **blinkingSupressionMultiplier**: `float`.

    Blinking supression multiplier.

  - **noiseRemovalThresholdFacBG**: `float`.

    Strength of the noise removal for background points.

  - **noiseRemovalThresholdFacFG**: `float`.

    Strength of the noise removal for foreground points.

  ##### Return
  - **retval**: `Evision.BgSegm.BackgroundSubtractorGSOC.t()`

  Implementation of the different yet better algorithm which is called GSOC, as it was implemented during GSOC and was not originated from any paper.

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorGSOC([, mc[, nSamples[, replaceRate[, propagationRate[, hitsThreshold[, alpha[, beta[, blinkingSupressionDecay[, blinkingSupressionMultiplier[, noiseRemovalThresholdFacBG[, noiseRemovalThresholdFacFG]]]]]]]]]]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorGSOC() :: Evision.BgSegm.BackgroundSubtractorGSOC.t() | {:error, String.t()}
  def createBackgroundSubtractorGSOC() do
    positional = [
    ]
    :evision_nif.bgsegm_createBackgroundSubtractorGSOC(positional)
    |> to_struct()
  end

  @doc """
  Creates an instance of BackgroundSubtractorLSBP algorithm.
  ##### Keyword Arguments
  - **mc**: `integer()`.

    Whether to use camera motion compensation.

  - **nSamples**: `integer()`.

    Number of samples to maintain at each point of the frame.

  - **lSBPRadius**: `integer()`.

    LSBP descriptor radius.

  - **tlower**: `float`.

    Lower bound for T-values. See @cite LGuo2016 for details.

  - **tupper**: `float`.

    Upper bound for T-values. See @cite LGuo2016 for details.

  - **tinc**: `float`.

    Increase step for T-values. See @cite LGuo2016 for details.

  - **tdec**: `float`.

    Decrease step for T-values. See @cite LGuo2016 for details.

  - **rscale**: `float`.

    Scale coefficient for threshold values.

  - **rincdec**: `float`.

    Increase/Decrease step for threshold values.

  - **noiseRemovalThresholdFacBG**: `float`.

    Strength of the noise removal for background points.

  - **noiseRemovalThresholdFacFG**: `float`.

    Strength of the noise removal for foreground points.

  - **lSBPthreshold**: `integer()`.

    Threshold for LSBP binary string.

  - **minCount**: `integer()`.

    Minimal number of matches for sample to be considered as foreground.

  ##### Return
  - **retval**: `Evision.BgSegm.BackgroundSubtractorLSBP.t()`

  Background Subtraction using Local SVD Binary Pattern. More details about the algorithm can be found at @cite LGuo2016

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorLSBP([, mc[, nSamples[, LSBPRadius[, Tlower[, Tupper[, Tinc[, Tdec[, Rscale[, Rincdec[, noiseRemovalThresholdFacBG[, noiseRemovalThresholdFacFG[, LSBPthreshold[, minCount]]]]]]]]]]]]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorLSBP([{:lSBPRadius, term()} | {:lSBPthreshold, term()} | {:mc, term()} | {:minCount, term()} | {:nSamples, term()} | {:noiseRemovalThresholdFacBG, term()} | {:noiseRemovalThresholdFacFG, term()} | {:rincdec, term()} | {:rscale, term()} | {:tdec, term()} | {:tinc, term()} | {:tlower, term()} | {:tupper, term()}] | nil) :: Evision.BgSegm.BackgroundSubtractorLSBP.t() | {:error, String.t()}
  def createBackgroundSubtractorLSBP(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:lSBPRadius, :lSBPthreshold, :mc, :minCount, :nSamples, :noiseRemovalThresholdFacBG, :noiseRemovalThresholdFacFG, :rincdec, :rscale, :tdec, :tinc, :tlower, :tupper])
    positional = [
    ]
    :evision_nif.bgsegm_createBackgroundSubtractorLSBP(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates an instance of BackgroundSubtractorLSBP algorithm.
  ##### Keyword Arguments
  - **mc**: `integer()`.

    Whether to use camera motion compensation.

  - **nSamples**: `integer()`.

    Number of samples to maintain at each point of the frame.

  - **lSBPRadius**: `integer()`.

    LSBP descriptor radius.

  - **tlower**: `float`.

    Lower bound for T-values. See @cite LGuo2016 for details.

  - **tupper**: `float`.

    Upper bound for T-values. See @cite LGuo2016 for details.

  - **tinc**: `float`.

    Increase step for T-values. See @cite LGuo2016 for details.

  - **tdec**: `float`.

    Decrease step for T-values. See @cite LGuo2016 for details.

  - **rscale**: `float`.

    Scale coefficient for threshold values.

  - **rincdec**: `float`.

    Increase/Decrease step for threshold values.

  - **noiseRemovalThresholdFacBG**: `float`.

    Strength of the noise removal for background points.

  - **noiseRemovalThresholdFacFG**: `float`.

    Strength of the noise removal for foreground points.

  - **lSBPthreshold**: `integer()`.

    Threshold for LSBP binary string.

  - **minCount**: `integer()`.

    Minimal number of matches for sample to be considered as foreground.

  ##### Return
  - **retval**: `Evision.BgSegm.BackgroundSubtractorLSBP.t()`

  Background Subtraction using Local SVD Binary Pattern. More details about the algorithm can be found at @cite LGuo2016

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorLSBP([, mc[, nSamples[, LSBPRadius[, Tlower[, Tupper[, Tinc[, Tdec[, Rscale[, Rincdec[, noiseRemovalThresholdFacBG[, noiseRemovalThresholdFacFG[, LSBPthreshold[, minCount]]]]]]]]]]]]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorLSBP() :: Evision.BgSegm.BackgroundSubtractorLSBP.t() | {:error, String.t()}
  def createBackgroundSubtractorLSBP() do
    positional = [
    ]
    :evision_nif.bgsegm_createBackgroundSubtractorLSBP(positional)
    |> to_struct()
  end

  @doc """
  Creates mixture-of-gaussian background subtractor
  ##### Keyword Arguments
  - **history**: `integer()`.

    Length of the history.

  - **nmixtures**: `integer()`.

    Number of Gaussian mixtures.

  - **backgroundRatio**: `double`.

    Background ratio.

  - **noiseSigma**: `double`.

    Noise strength (standard deviation of the brightness or each color channel). 0
    means some automatic value.

  ##### Return
  - **retval**: `Evision.BgSegm.BackgroundSubtractorMOG.t()`

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorMOG([, history[, nmixtures[, backgroundRatio[, noiseSigma]]]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorMOG([{:backgroundRatio, term()} | {:history, term()} | {:nmixtures, term()} | {:noiseSigma, term()}] | nil) :: Evision.BgSegm.BackgroundSubtractorMOG.t() | {:error, String.t()}
  def createBackgroundSubtractorMOG(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:backgroundRatio, :history, :nmixtures, :noiseSigma])
    positional = [
    ]
    :evision_nif.bgsegm_createBackgroundSubtractorMOG(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates mixture-of-gaussian background subtractor
  ##### Keyword Arguments
  - **history**: `integer()`.

    Length of the history.

  - **nmixtures**: `integer()`.

    Number of Gaussian mixtures.

  - **backgroundRatio**: `double`.

    Background ratio.

  - **noiseSigma**: `double`.

    Noise strength (standard deviation of the brightness or each color channel). 0
    means some automatic value.

  ##### Return
  - **retval**: `Evision.BgSegm.BackgroundSubtractorMOG.t()`

  Python prototype (for reference only):
  ```python3
  createBackgroundSubtractorMOG([, history[, nmixtures[, backgroundRatio[, noiseSigma]]]]) -> retval
  ```
  """
  @spec createBackgroundSubtractorMOG() :: Evision.BgSegm.BackgroundSubtractorMOG.t() | {:error, String.t()}
  def createBackgroundSubtractorMOG() do
    positional = [
    ]
    :evision_nif.bgsegm_createBackgroundSubtractorMOG(positional)
    |> to_struct()
  end

  @doc """
  Creates an instance of SyntheticSequenceGenerator.

  ##### Positional Arguments
  - **background**: `Evision.Mat`.

    Background image for object.

  - **object**: `Evision.Mat`.

    Object image which will move slowly over the background.

  ##### Keyword Arguments
  - **amplitude**: `double`.

    Amplitude of wave distortion applied to background.

  - **wavelength**: `double`.

    Length of waves in distortion applied to background.

  - **wavespeed**: `double`.

    How fast waves will move.

  - **objspeed**: `double`.

    How fast object will fly over background.

  ##### Return
  - **retval**: `Evision.BgSegm.SyntheticSequenceGenerator.t()`

  Python prototype (for reference only):
  ```python3
  createSyntheticSequenceGenerator(background, object[, amplitude[, wavelength[, wavespeed[, objspeed]]]]) -> retval
  ```
  """
  @spec createSyntheticSequenceGenerator(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:amplitude, term()} | {:objspeed, term()} | {:wavelength, term()} | {:wavespeed, term()}] | nil) :: Evision.BgSegm.SyntheticSequenceGenerator.t() | {:error, String.t()}
  def createSyntheticSequenceGenerator(background, object, opts) when (is_struct(background, Evision.Mat) or is_struct(background, Nx.Tensor) or is_number(background) or is_tuple(background)) and (is_struct(object, Evision.Mat) or is_struct(object, Nx.Tensor) or is_number(object) or is_tuple(object)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:amplitude, :objspeed, :wavelength, :wavespeed])
    positional = [
      background: Evision.Internal.Structurise.from_struct(background),
      object: Evision.Internal.Structurise.from_struct(object)
    ]
    :evision_nif.bgsegm_createSyntheticSequenceGenerator(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates an instance of SyntheticSequenceGenerator.

  ##### Positional Arguments
  - **background**: `Evision.Mat`.

    Background image for object.

  - **object**: `Evision.Mat`.

    Object image which will move slowly over the background.

  ##### Keyword Arguments
  - **amplitude**: `double`.

    Amplitude of wave distortion applied to background.

  - **wavelength**: `double`.

    Length of waves in distortion applied to background.

  - **wavespeed**: `double`.

    How fast waves will move.

  - **objspeed**: `double`.

    How fast object will fly over background.

  ##### Return
  - **retval**: `Evision.BgSegm.SyntheticSequenceGenerator.t()`

  Python prototype (for reference only):
  ```python3
  createSyntheticSequenceGenerator(background, object[, amplitude[, wavelength[, wavespeed[, objspeed]]]]) -> retval
  ```
  """
  @spec createSyntheticSequenceGenerator(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.BgSegm.SyntheticSequenceGenerator.t() | {:error, String.t()}
  def createSyntheticSequenceGenerator(background, object) when (is_struct(background, Evision.Mat) or is_struct(background, Nx.Tensor) or is_number(background) or is_tuple(background)) and (is_struct(object, Evision.Mat) or is_struct(object, Nx.Tensor) or is_number(object) or is_tuple(object))
  do
    positional = [
      background: Evision.Internal.Structurise.from_struct(background),
      object: Evision.Internal.Structurise.from_struct(object)
    ]
    :evision_nif.bgsegm_createSyntheticSequenceGenerator(positional)
    |> to_struct()
  end
end
