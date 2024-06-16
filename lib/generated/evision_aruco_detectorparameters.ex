defmodule Evision.ArUco.DetectorParameters do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ArUco.DetectorParameters` struct.

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
  def to_struct({:ok, %{class: Evision.ArUco.DetectorParameters, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ArUco.DetectorParameters, ref: ref}) do
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
  DetectorParameters
  ##### Return
  - **self**: `DetectorParameters`

  Python prototype (for reference only):
  ```python3
  DetectorParameters() -> <aruco_DetectorParameters object>
  ```
  """
  @spec detectorParameters() :: Evision.ArUco.DetectorParameters.t() | {:error, String.t()}
  def detectorParameters() do
    positional = [
    ]
    :evision_nif.aruco_aruco_DetectorParameters_DetectorParameters(positional)
    |> to_struct()
  end

  @doc """
  Read a new set of DetectorParameters from FileNode (use FileStorage.root()).

  ##### Positional Arguments
  - **self**: `Evision.ArUco.DetectorParameters.t()`
  - **func**: `Evision.FileNode`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  readDetectorParameters(fn) -> retval
  ```
  """
  @spec readDetectorParameters(Evision.ArUco.DetectorParameters.t(), Evision.FileNode.t()) :: boolean() | {:error, String.t()}
  def readDetectorParameters(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.aruco_aruco_DetectorParameters_readDetectorParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Write a set of DetectorParameters to FileStorage

  ##### Positional Arguments
  - **self**: `Evision.ArUco.DetectorParameters.t()`
  - **fs**: `Evision.FileStorage`

  ##### Keyword Arguments
  - **name**: `String`.

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  writeDetectorParameters(fs[, name]) -> retval
  ```
  """
  @spec writeDetectorParameters(Evision.ArUco.DetectorParameters.t(), Evision.FileStorage.t(), [{:name, term()}] | nil) :: boolean() | {:error, String.t()}
  def writeDetectorParameters(self, fs, opts) when is_struct(fs, Evision.FileStorage) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:name])
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.aruco_aruco_DetectorParameters_writeDetectorParameters(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Write a set of DetectorParameters to FileStorage

  ##### Positional Arguments
  - **self**: `Evision.ArUco.DetectorParameters.t()`
  - **fs**: `Evision.FileStorage`

  ##### Keyword Arguments
  - **name**: `String`.

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  writeDetectorParameters(fs[, name]) -> retval
  ```
  """
  @spec writeDetectorParameters(Evision.ArUco.DetectorParameters.t(), Evision.FileStorage.t()) :: boolean() | {:error, String.t()}
  def writeDetectorParameters(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.aruco_aruco_DetectorParameters_writeDetectorParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_adaptiveThreshConstant(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_adaptiveThreshConstant(self) do
    :evision_nif.aruco_DetectorParameters_get_adaptiveThreshConstant(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_adaptiveThreshConstant(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_adaptiveThreshConstant(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_adaptiveThreshConstant(
        Evision.Internal.Structurise.from_struct(self),
        [adaptiveThreshConstant: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_adaptiveThreshWinSizeMax(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_adaptiveThreshWinSizeMax(self) do
    :evision_nif.aruco_DetectorParameters_get_adaptiveThreshWinSizeMax(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_adaptiveThreshWinSizeMax(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_adaptiveThreshWinSizeMax(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_adaptiveThreshWinSizeMax(
        Evision.Internal.Structurise.from_struct(self),
        [adaptiveThreshWinSizeMax: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_adaptiveThreshWinSizeMin(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_adaptiveThreshWinSizeMin(self) do
    :evision_nif.aruco_DetectorParameters_get_adaptiveThreshWinSizeMin(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_adaptiveThreshWinSizeMin(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_adaptiveThreshWinSizeMin(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_adaptiveThreshWinSizeMin(
        Evision.Internal.Structurise.from_struct(self),
        [adaptiveThreshWinSizeMin: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_adaptiveThreshWinSizeStep(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_adaptiveThreshWinSizeStep(self) do
    :evision_nif.aruco_DetectorParameters_get_adaptiveThreshWinSizeStep(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_adaptiveThreshWinSizeStep(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_adaptiveThreshWinSizeStep(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_adaptiveThreshWinSizeStep(
        Evision.Internal.Structurise.from_struct(self),
        [adaptiveThreshWinSizeStep: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_aprilTagCriticalRad(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_aprilTagCriticalRad(self) do
    :evision_nif.aruco_DetectorParameters_get_aprilTagCriticalRad(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_aprilTagCriticalRad(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_aprilTagCriticalRad(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_aprilTagCriticalRad(
        Evision.Internal.Structurise.from_struct(self),
        [aprilTagCriticalRad: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_aprilTagDeglitch(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_aprilTagDeglitch(self) do
    :evision_nif.aruco_DetectorParameters_get_aprilTagDeglitch(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_aprilTagDeglitch(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_aprilTagDeglitch(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_aprilTagDeglitch(
        Evision.Internal.Structurise.from_struct(self),
        [aprilTagDeglitch: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_aprilTagMaxLineFitMse(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_aprilTagMaxLineFitMse(self) do
    :evision_nif.aruco_DetectorParameters_get_aprilTagMaxLineFitMse(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_aprilTagMaxLineFitMse(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_aprilTagMaxLineFitMse(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_aprilTagMaxLineFitMse(
        Evision.Internal.Structurise.from_struct(self),
        [aprilTagMaxLineFitMse: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_aprilTagMaxNmaxima(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_aprilTagMaxNmaxima(self) do
    :evision_nif.aruco_DetectorParameters_get_aprilTagMaxNmaxima(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_aprilTagMaxNmaxima(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_aprilTagMaxNmaxima(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_aprilTagMaxNmaxima(
        Evision.Internal.Structurise.from_struct(self),
        [aprilTagMaxNmaxima: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_aprilTagMinClusterPixels(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_aprilTagMinClusterPixels(self) do
    :evision_nif.aruco_DetectorParameters_get_aprilTagMinClusterPixels(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_aprilTagMinClusterPixels(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_aprilTagMinClusterPixels(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_aprilTagMinClusterPixels(
        Evision.Internal.Structurise.from_struct(self),
        [aprilTagMinClusterPixels: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_aprilTagMinWhiteBlackDiff(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_aprilTagMinWhiteBlackDiff(self) do
    :evision_nif.aruco_DetectorParameters_get_aprilTagMinWhiteBlackDiff(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_aprilTagMinWhiteBlackDiff(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_aprilTagMinWhiteBlackDiff(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_aprilTagMinWhiteBlackDiff(
        Evision.Internal.Structurise.from_struct(self),
        [aprilTagMinWhiteBlackDiff: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_aprilTagQuadDecimate(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_aprilTagQuadDecimate(self) do
    :evision_nif.aruco_DetectorParameters_get_aprilTagQuadDecimate(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_aprilTagQuadDecimate(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_aprilTagQuadDecimate(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_aprilTagQuadDecimate(
        Evision.Internal.Structurise.from_struct(self),
        [aprilTagQuadDecimate: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_aprilTagQuadSigma(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_aprilTagQuadSigma(self) do
    :evision_nif.aruco_DetectorParameters_get_aprilTagQuadSigma(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_aprilTagQuadSigma(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_aprilTagQuadSigma(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_aprilTagQuadSigma(
        Evision.Internal.Structurise.from_struct(self),
        [aprilTagQuadSigma: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_cornerRefinementMaxIterations(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_cornerRefinementMaxIterations(self) do
    :evision_nif.aruco_DetectorParameters_get_cornerRefinementMaxIterations(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_cornerRefinementMaxIterations(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_cornerRefinementMaxIterations(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_cornerRefinementMaxIterations(
        Evision.Internal.Structurise.from_struct(self),
        [cornerRefinementMaxIterations: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_cornerRefinementMethod(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_cornerRefinementMethod(self) do
    :evision_nif.aruco_DetectorParameters_get_cornerRefinementMethod(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_cornerRefinementMethod(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_cornerRefinementMethod(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_cornerRefinementMethod(
        Evision.Internal.Structurise.from_struct(self),
        [cornerRefinementMethod: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_cornerRefinementMinAccuracy(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_cornerRefinementMinAccuracy(self) do
    :evision_nif.aruco_DetectorParameters_get_cornerRefinementMinAccuracy(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_cornerRefinementMinAccuracy(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_cornerRefinementMinAccuracy(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_cornerRefinementMinAccuracy(
        Evision.Internal.Structurise.from_struct(self),
        [cornerRefinementMinAccuracy: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_cornerRefinementWinSize(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_cornerRefinementWinSize(self) do
    :evision_nif.aruco_DetectorParameters_get_cornerRefinementWinSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_cornerRefinementWinSize(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_cornerRefinementWinSize(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_cornerRefinementWinSize(
        Evision.Internal.Structurise.from_struct(self),
        [cornerRefinementWinSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_detectInvertedMarker(Evision.ArUco.DetectorParameters.t()) :: boolean()
  def get_detectInvertedMarker(self) do
    :evision_nif.aruco_DetectorParameters_get_detectInvertedMarker(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_detectInvertedMarker(Evision.ArUco.DetectorParameters.t(), boolean()) :: Evision.ArUco.DetectorParameters.t()
  def set_detectInvertedMarker(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_detectInvertedMarker(
        Evision.Internal.Structurise.from_struct(self),
        [detectInvertedMarker: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_errorCorrectionRate(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_errorCorrectionRate(self) do
    :evision_nif.aruco_DetectorParameters_get_errorCorrectionRate(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_errorCorrectionRate(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_errorCorrectionRate(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_errorCorrectionRate(
        Evision.Internal.Structurise.from_struct(self),
        [errorCorrectionRate: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_markerBorderBits(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_markerBorderBits(self) do
    :evision_nif.aruco_DetectorParameters_get_markerBorderBits(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_markerBorderBits(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_markerBorderBits(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_markerBorderBits(
        Evision.Internal.Structurise.from_struct(self),
        [markerBorderBits: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxErroneousBitsInBorderRate(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_maxErroneousBitsInBorderRate(self) do
    :evision_nif.aruco_DetectorParameters_get_maxErroneousBitsInBorderRate(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxErroneousBitsInBorderRate(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_maxErroneousBitsInBorderRate(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_maxErroneousBitsInBorderRate(
        Evision.Internal.Structurise.from_struct(self),
        [maxErroneousBitsInBorderRate: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxMarkerPerimeterRate(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_maxMarkerPerimeterRate(self) do
    :evision_nif.aruco_DetectorParameters_get_maxMarkerPerimeterRate(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxMarkerPerimeterRate(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_maxMarkerPerimeterRate(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_maxMarkerPerimeterRate(
        Evision.Internal.Structurise.from_struct(self),
        [maxMarkerPerimeterRate: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minCornerDistanceRate(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_minCornerDistanceRate(self) do
    :evision_nif.aruco_DetectorParameters_get_minCornerDistanceRate(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minCornerDistanceRate(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_minCornerDistanceRate(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_minCornerDistanceRate(
        Evision.Internal.Structurise.from_struct(self),
        [minCornerDistanceRate: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minDistanceToBorder(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_minDistanceToBorder(self) do
    :evision_nif.aruco_DetectorParameters_get_minDistanceToBorder(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minDistanceToBorder(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_minDistanceToBorder(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_minDistanceToBorder(
        Evision.Internal.Structurise.from_struct(self),
        [minDistanceToBorder: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minGroupDistance(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_minGroupDistance(self) do
    :evision_nif.aruco_DetectorParameters_get_minGroupDistance(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minGroupDistance(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_minGroupDistance(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_minGroupDistance(
        Evision.Internal.Structurise.from_struct(self),
        [minGroupDistance: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minMarkerDistanceRate(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_minMarkerDistanceRate(self) do
    :evision_nif.aruco_DetectorParameters_get_minMarkerDistanceRate(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minMarkerDistanceRate(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_minMarkerDistanceRate(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_minMarkerDistanceRate(
        Evision.Internal.Structurise.from_struct(self),
        [minMarkerDistanceRate: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minMarkerLengthRatioOriginalImg(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_minMarkerLengthRatioOriginalImg(self) do
    :evision_nif.aruco_DetectorParameters_get_minMarkerLengthRatioOriginalImg(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minMarkerLengthRatioOriginalImg(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_minMarkerLengthRatioOriginalImg(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_minMarkerLengthRatioOriginalImg(
        Evision.Internal.Structurise.from_struct(self),
        [minMarkerLengthRatioOriginalImg: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minMarkerPerimeterRate(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_minMarkerPerimeterRate(self) do
    :evision_nif.aruco_DetectorParameters_get_minMarkerPerimeterRate(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minMarkerPerimeterRate(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_minMarkerPerimeterRate(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_minMarkerPerimeterRate(
        Evision.Internal.Structurise.from_struct(self),
        [minMarkerPerimeterRate: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minOtsuStdDev(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_minOtsuStdDev(self) do
    :evision_nif.aruco_DetectorParameters_get_minOtsuStdDev(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minOtsuStdDev(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_minOtsuStdDev(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_minOtsuStdDev(
        Evision.Internal.Structurise.from_struct(self),
        [minOtsuStdDev: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minSideLengthCanonicalImg(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_minSideLengthCanonicalImg(self) do
    :evision_nif.aruco_DetectorParameters_get_minSideLengthCanonicalImg(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minSideLengthCanonicalImg(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_minSideLengthCanonicalImg(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_minSideLengthCanonicalImg(
        Evision.Internal.Structurise.from_struct(self),
        [minSideLengthCanonicalImg: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_perspectiveRemoveIgnoredMarginPerCell(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_perspectiveRemoveIgnoredMarginPerCell(self) do
    :evision_nif.aruco_DetectorParameters_get_perspectiveRemoveIgnoredMarginPerCell(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_perspectiveRemoveIgnoredMarginPerCell(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_perspectiveRemoveIgnoredMarginPerCell(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_perspectiveRemoveIgnoredMarginPerCell(
        Evision.Internal.Structurise.from_struct(self),
        [perspectiveRemoveIgnoredMarginPerCell: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_perspectiveRemovePixelPerCell(Evision.ArUco.DetectorParameters.t()) :: integer()
  def get_perspectiveRemovePixelPerCell(self) do
    :evision_nif.aruco_DetectorParameters_get_perspectiveRemovePixelPerCell(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_perspectiveRemovePixelPerCell(Evision.ArUco.DetectorParameters.t(), integer()) :: Evision.ArUco.DetectorParameters.t()
  def set_perspectiveRemovePixelPerCell(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_perspectiveRemovePixelPerCell(
        Evision.Internal.Structurise.from_struct(self),
        [perspectiveRemovePixelPerCell: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_polygonalApproxAccuracyRate(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_polygonalApproxAccuracyRate(self) do
    :evision_nif.aruco_DetectorParameters_get_polygonalApproxAccuracyRate(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_polygonalApproxAccuracyRate(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_polygonalApproxAccuracyRate(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_polygonalApproxAccuracyRate(
        Evision.Internal.Structurise.from_struct(self),
        [polygonalApproxAccuracyRate: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_relativeCornerRefinmentWinSize(Evision.ArUco.DetectorParameters.t()) :: number()
  def get_relativeCornerRefinmentWinSize(self) do
    :evision_nif.aruco_DetectorParameters_get_relativeCornerRefinmentWinSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_relativeCornerRefinmentWinSize(Evision.ArUco.DetectorParameters.t(), number()) :: Evision.ArUco.DetectorParameters.t()
  def set_relativeCornerRefinmentWinSize(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_relativeCornerRefinmentWinSize(
        Evision.Internal.Structurise.from_struct(self),
        [relativeCornerRefinmentWinSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_useAruco3Detection(Evision.ArUco.DetectorParameters.t()) :: boolean()
  def get_useAruco3Detection(self) do
    :evision_nif.aruco_DetectorParameters_get_useAruco3Detection(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_useAruco3Detection(Evision.ArUco.DetectorParameters.t(), boolean()) :: Evision.ArUco.DetectorParameters.t()
  def set_useAruco3Detection(self, prop) do
    :evision_nif.aruco_DetectorParameters_set_useAruco3Detection(
        Evision.Internal.Structurise.from_struct(self),
        [useAruco3Detection: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
