defmodule Evision.XImgProc.DisparityWLSFilter do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.DisparityWLSFilter` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.DisparityWLSFilter, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.DisparityWLSFilter, ref: ref}) do
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
  Get the confidence map that was used in the last filter call. It is a CV_32F one-channel image
  with values ranging from 0.0 (totally untrusted regions of the raw disparity map) to 255.0 (regions containing
  correct disparity values with a high degree of confidence).

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityWLSFilter.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getConfidenceMap() -> retval
  ```
  """
  @spec getConfidenceMap(Evision.XImgProc.DisparityWLSFilter.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getConfidenceMap(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_DisparityWLSFilter_getConfidenceMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  DepthDiscontinuityRadius is a parameter used in confidence computation. It defines the size of
  low-confidence regions around depth discontinuities.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityWLSFilter.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getDepthDiscontinuityRadius() -> retval
  ```
  """
  @spec getDepthDiscontinuityRadius(Evision.XImgProc.DisparityWLSFilter.t()) :: integer() | {:error, String.t()}
  def getDepthDiscontinuityRadius(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_DisparityWLSFilter_getDepthDiscontinuityRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  LRCthresh is a threshold of disparity difference used in left-right-consistency check during
  confidence map computation. The default value of 24 (1.5 pixels) is virtually always good enough.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityWLSFilter.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getLRCthresh() -> retval
  ```
  """
  @spec getLRCthresh(Evision.XImgProc.DisparityWLSFilter.t()) :: integer() | {:error, String.t()}
  def getLRCthresh(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_DisparityWLSFilter_getLRCthresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Lambda is a parameter defining the amount of regularization during filtering. Larger values force
  filtered disparity map edges to adhere more to source image edges. Typical value is 8000.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityWLSFilter.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getLambda() -> retval
  ```
  """
  @spec getLambda(Evision.XImgProc.DisparityWLSFilter.t()) :: number() | {:error, String.t()}
  def getLambda(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_DisparityWLSFilter_getLambda(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Get the ROI used in the last filter call

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityWLSFilter.t()`

  ##### Return
  - **retval**: `Rect`

  Python prototype (for reference only):
  ```python3
  getROI() -> retval
  ```
  """
  @spec getROI(Evision.XImgProc.DisparityWLSFilter.t()) :: {number(), number(), number(), number()} | {:error, String.t()}
  def getROI(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_DisparityWLSFilter_getROI(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  SigmaColor is a parameter defining how sensitive the filtering process is to source image edges.
  Large values can lead to disparity leakage through low-contrast edges. Small values can make the filter too
  sensitive to noise and textures in the source image. Typical values range from 0.8 to 2.0.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityWLSFilter.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getSigmaColor() -> retval
  ```
  """
  @spec getSigmaColor(Evision.XImgProc.DisparityWLSFilter.t()) :: number() | {:error, String.t()}
  def getSigmaColor(self) do
    positional = [
    ]
    :evision_nif.ximgproc_ximgproc_DisparityWLSFilter_getSigmaColor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDepthDiscontinuityRadius

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityWLSFilter.t()`
  - **disc_radius**: `integer()`

  @see getDepthDiscontinuityRadius

  Python prototype (for reference only):
  ```python3
  setDepthDiscontinuityRadius(_disc_radius) -> None
  ```
  """
  @spec setDepthDiscontinuityRadius(Evision.XImgProc.DisparityWLSFilter.t(), integer()) :: Evision.XImgProc.DisparityWLSFilter.t() | {:error, String.t()}
  def setDepthDiscontinuityRadius(self, disc_radius) when is_integer(disc_radius)
  do
    positional = [
      disc_radius: Evision.Internal.Structurise.from_struct(disc_radius)
    ]
    :evision_nif.ximgproc_ximgproc_DisparityWLSFilter_setDepthDiscontinuityRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setLRCthresh

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityWLSFilter.t()`
  - **lRC_thresh**: `integer()`

  @see getLRCthresh

  Python prototype (for reference only):
  ```python3
  setLRCthresh(_LRC_thresh) -> None
  ```
  """
  @spec setLRCthresh(Evision.XImgProc.DisparityWLSFilter.t(), integer()) :: Evision.XImgProc.DisparityWLSFilter.t() | {:error, String.t()}
  def setLRCthresh(self, lRC_thresh) when is_integer(lRC_thresh)
  do
    positional = [
      lRC_thresh: Evision.Internal.Structurise.from_struct(lRC_thresh)
    ]
    :evision_nif.ximgproc_ximgproc_DisparityWLSFilter_setLRCthresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setLambda

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityWLSFilter.t()`
  - **lambda**: `double`

  @see getLambda

  Python prototype (for reference only):
  ```python3
  setLambda(_lambda) -> None
  ```
  """
  @spec setLambda(Evision.XImgProc.DisparityWLSFilter.t(), number()) :: Evision.XImgProc.DisparityWLSFilter.t() | {:error, String.t()}
  def setLambda(self, lambda) when is_number(lambda)
  do
    positional = [
      lambda: Evision.Internal.Structurise.from_struct(lambda)
    ]
    :evision_nif.ximgproc_ximgproc_DisparityWLSFilter_setLambda(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSigmaColor

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.DisparityWLSFilter.t()`
  - **sigma_color**: `double`

  @see getSigmaColor

  Python prototype (for reference only):
  ```python3
  setSigmaColor(_sigma_color) -> None
  ```
  """
  @spec setSigmaColor(Evision.XImgProc.DisparityWLSFilter.t(), number()) :: Evision.XImgProc.DisparityWLSFilter.t() | {:error, String.t()}
  def setSigmaColor(self, sigma_color) when is_number(sigma_color)
  do
    positional = [
      sigma_color: Evision.Internal.Structurise.from_struct(sigma_color)
    ]
    :evision_nif.ximgproc_ximgproc_DisparityWLSFilter_setSigmaColor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
