defmodule Evision.StructuredLight.SinusoidalPattern do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `StructuredLight.SinusoidalPattern` struct.

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
  def to_struct({:ok, %{class: Evision.StructuredLight.SinusoidalPattern, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.StructuredLight.SinusoidalPattern, ref: ref}) do
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
  compute the data modulation term.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.SinusoidalPattern.t()`
  - **patternImages**: `[Evision.Mat]`.

    captured images with projected patterns.

  - **shadowMask**: `Evision.Mat`.

    Mask used to discard shadow regions.

  ##### Return
  - **dataModulationTerm**: `Evision.Mat.t()`.

    Mat where the data modulation term is saved.

  Python prototype (for reference only):
  ```python3
  computeDataModulationTerm(patternImages, shadowMask[, dataModulationTerm]) -> dataModulationTerm
  ```
  """
  @spec computeDataModulationTerm(Evision.StructuredLight.SinusoidalPattern.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def computeDataModulationTerm(self, patternImages, shadowMask, opts) when is_list(patternImages) and (is_struct(shadowMask, Evision.Mat) or is_struct(shadowMask, Nx.Tensor) or is_number(shadowMask) or is_tuple(shadowMask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      patternImages: Evision.Internal.Structurise.from_struct(patternImages),
      shadowMask: Evision.Internal.Structurise.from_struct(shadowMask)
    ]
    :evision_nif.structured_light_structured_light_SinusoidalPattern_computeDataModulationTerm(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  compute the data modulation term.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.SinusoidalPattern.t()`
  - **patternImages**: `[Evision.Mat]`.

    captured images with projected patterns.

  - **shadowMask**: `Evision.Mat`.

    Mask used to discard shadow regions.

  ##### Return
  - **dataModulationTerm**: `Evision.Mat.t()`.

    Mat where the data modulation term is saved.

  Python prototype (for reference only):
  ```python3
  computeDataModulationTerm(patternImages, shadowMask[, dataModulationTerm]) -> dataModulationTerm
  ```
  """
  @spec computeDataModulationTerm(Evision.StructuredLight.SinusoidalPattern.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def computeDataModulationTerm(self, patternImages, shadowMask) when is_list(patternImages) and (is_struct(shadowMask, Evision.Mat) or is_struct(shadowMask, Nx.Tensor) or is_number(shadowMask) or is_tuple(shadowMask))
  do
    positional = [
      patternImages: Evision.Internal.Structurise.from_struct(patternImages),
      shadowMask: Evision.Internal.Structurise.from_struct(shadowMask)
    ]
    :evision_nif.structured_light_structured_light_SinusoidalPattern_computeDataModulationTerm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Compute a wrapped phase map from sinusoidal patterns.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.SinusoidalPattern.t()`
  - **patternImages**: `[Evision.Mat]`.

    Input data to compute the wrapped phase map.

  ##### Keyword Arguments
  - **fundamental**: `Evision.Mat`.

    Fundamental matrix used to compute epipolar lines and ease the matching step.

  ##### Return
  - **wrappedPhaseMap**: `Evision.Mat.t()`.

    Wrapped phase map obtained through one of the three methods.

  - **shadowMask**: `Evision.Mat.t()`.

    Mask used to discard shadow regions.

  Python prototype (for reference only):
  ```python3
  computePhaseMap(patternImages[, wrappedPhaseMap[, shadowMask[, fundamental]]]) -> wrappedPhaseMap, shadowMask
  ```
  """
  @spec computePhaseMap(Evision.StructuredLight.SinusoidalPattern.t(), list(Evision.Mat.maybe_mat_in()), [{:fundamental, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def computePhaseMap(self, patternImages, opts) when is_list(patternImages) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:fundamental])
    positional = [
      patternImages: Evision.Internal.Structurise.from_struct(patternImages)
    ]
    :evision_nif.structured_light_structured_light_SinusoidalPattern_computePhaseMap(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Compute a wrapped phase map from sinusoidal patterns.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.SinusoidalPattern.t()`
  - **patternImages**: `[Evision.Mat]`.

    Input data to compute the wrapped phase map.

  ##### Keyword Arguments
  - **fundamental**: `Evision.Mat`.

    Fundamental matrix used to compute epipolar lines and ease the matching step.

  ##### Return
  - **wrappedPhaseMap**: `Evision.Mat.t()`.

    Wrapped phase map obtained through one of the three methods.

  - **shadowMask**: `Evision.Mat.t()`.

    Mask used to discard shadow regions.

  Python prototype (for reference only):
  ```python3
  computePhaseMap(patternImages[, wrappedPhaseMap[, shadowMask[, fundamental]]]) -> wrappedPhaseMap, shadowMask
  ```
  """
  @spec computePhaseMap(Evision.StructuredLight.SinusoidalPattern.t(), list(Evision.Mat.maybe_mat_in())) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def computePhaseMap(self, patternImages) when is_list(patternImages)
  do
    positional = [
      patternImages: Evision.Internal.Structurise.from_struct(patternImages)
    ]
    :evision_nif.structured_light_structured_light_SinusoidalPattern_computePhaseMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Constructor.
  ##### Keyword Arguments
  - **parameters**: `Evision.StructuredLight.SinusoidalPattern.Params`.

    SinusoidalPattern parameters SinusoidalPattern::Params: width, height of the projector and patterns parameters.

  ##### Return
  - **retval**: `Evision.StructuredLight.SinusoidalPattern.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create([{:parameters, term()}] | nil) :: Evision.StructuredLight.SinusoidalPattern.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:parameters])
    positional = [
    ]
    :evision_nif.structured_light_structured_light_SinusoidalPattern_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Constructor.
  ##### Keyword Arguments
  - **parameters**: `Evision.StructuredLight.SinusoidalPattern.Params`.

    SinusoidalPattern parameters SinusoidalPattern::Params: width, height of the projector and patterns parameters.

  ##### Return
  - **retval**: `Evision.StructuredLight.SinusoidalPattern.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create() :: Evision.StructuredLight.SinusoidalPattern.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.structured_light_structured_light_SinusoidalPattern_create_static(positional)
    |> to_struct()
  end

  @doc """
  Find correspondences between the two devices thanks to unwrapped phase maps.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.SinusoidalPattern.t()`
  - **projUnwrappedPhaseMap**: `Evision.Mat`.

    Projector's unwrapped phase map.

  - **camUnwrappedPhaseMap**: `Evision.Mat`.

    Camera's unwrapped phase map.

  ##### Return
  - **matches**: `[Evision.Mat]`.

    Images used to display correspondences map.

  Python prototype (for reference only):
  ```python3
  findProCamMatches(projUnwrappedPhaseMap, camUnwrappedPhaseMap[, matches]) -> matches
  ```
  """
  @spec findProCamMatches(Evision.StructuredLight.SinusoidalPattern.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: list(Evision.Mat.t()) | {:error, String.t()}
  def findProCamMatches(self, projUnwrappedPhaseMap, camUnwrappedPhaseMap, opts) when (is_struct(projUnwrappedPhaseMap, Evision.Mat) or is_struct(projUnwrappedPhaseMap, Nx.Tensor) or is_number(projUnwrappedPhaseMap) or is_tuple(projUnwrappedPhaseMap)) and (is_struct(camUnwrappedPhaseMap, Evision.Mat) or is_struct(camUnwrappedPhaseMap, Nx.Tensor) or is_number(camUnwrappedPhaseMap) or is_tuple(camUnwrappedPhaseMap)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      projUnwrappedPhaseMap: Evision.Internal.Structurise.from_struct(projUnwrappedPhaseMap),
      camUnwrappedPhaseMap: Evision.Internal.Structurise.from_struct(camUnwrappedPhaseMap)
    ]
    :evision_nif.structured_light_structured_light_SinusoidalPattern_findProCamMatches(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Find correspondences between the two devices thanks to unwrapped phase maps.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.SinusoidalPattern.t()`
  - **projUnwrappedPhaseMap**: `Evision.Mat`.

    Projector's unwrapped phase map.

  - **camUnwrappedPhaseMap**: `Evision.Mat`.

    Camera's unwrapped phase map.

  ##### Return
  - **matches**: `[Evision.Mat]`.

    Images used to display correspondences map.

  Python prototype (for reference only):
  ```python3
  findProCamMatches(projUnwrappedPhaseMap, camUnwrappedPhaseMap[, matches]) -> matches
  ```
  """
  @spec findProCamMatches(Evision.StructuredLight.SinusoidalPattern.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def findProCamMatches(self, projUnwrappedPhaseMap, camUnwrappedPhaseMap) when (is_struct(projUnwrappedPhaseMap, Evision.Mat) or is_struct(projUnwrappedPhaseMap, Nx.Tensor) or is_number(projUnwrappedPhaseMap) or is_tuple(projUnwrappedPhaseMap)) and (is_struct(camUnwrappedPhaseMap, Evision.Mat) or is_struct(camUnwrappedPhaseMap, Nx.Tensor) or is_number(camUnwrappedPhaseMap) or is_tuple(camUnwrappedPhaseMap))
  do
    positional = [
      projUnwrappedPhaseMap: Evision.Internal.Structurise.from_struct(projUnwrappedPhaseMap),
      camUnwrappedPhaseMap: Evision.Internal.Structurise.from_struct(camUnwrappedPhaseMap)
    ]
    :evision_nif.structured_light_structured_light_SinusoidalPattern_findProCamMatches(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Unwrap the wrapped phase map to remove phase ambiguities.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.SinusoidalPattern.t()`
  - **wrappedPhaseMap**: `Evision.Mat`.

    The wrapped phase map computed from the pattern.

  - **camSize**: `Size`.

    Resolution of the camera.

  ##### Keyword Arguments
  - **shadowMask**: `Evision.Mat`.

    Mask used to discard shadow regions.

  ##### Return
  - **unwrappedPhaseMap**: `Evision.Mat.t()`.

    The unwrapped phase map used to find correspondences between the two devices.

  Python prototype (for reference only):
  ```python3
  unwrapPhaseMap(wrappedPhaseMap, camSize[, unwrappedPhaseMap[, shadowMask]]) -> unwrappedPhaseMap
  ```
  """
  @spec unwrapPhaseMap(Evision.StructuredLight.SinusoidalPattern.t(), Evision.Mat.maybe_mat_in(), {number(), number()}, [{:shadowMask, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def unwrapPhaseMap(self, wrappedPhaseMap, camSize, opts) when (is_struct(wrappedPhaseMap, Evision.Mat) or is_struct(wrappedPhaseMap, Nx.Tensor) or is_number(wrappedPhaseMap) or is_tuple(wrappedPhaseMap)) and is_tuple(camSize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:shadowMask])
    positional = [
      wrappedPhaseMap: Evision.Internal.Structurise.from_struct(wrappedPhaseMap),
      camSize: Evision.Internal.Structurise.from_struct(camSize)
    ]
    :evision_nif.structured_light_structured_light_SinusoidalPattern_unwrapPhaseMap(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Unwrap the wrapped phase map to remove phase ambiguities.

  ##### Positional Arguments
  - **self**: `Evision.StructuredLight.SinusoidalPattern.t()`
  - **wrappedPhaseMap**: `Evision.Mat`.

    The wrapped phase map computed from the pattern.

  - **camSize**: `Size`.

    Resolution of the camera.

  ##### Keyword Arguments
  - **shadowMask**: `Evision.Mat`.

    Mask used to discard shadow regions.

  ##### Return
  - **unwrappedPhaseMap**: `Evision.Mat.t()`.

    The unwrapped phase map used to find correspondences between the two devices.

  Python prototype (for reference only):
  ```python3
  unwrapPhaseMap(wrappedPhaseMap, camSize[, unwrappedPhaseMap[, shadowMask]]) -> unwrappedPhaseMap
  ```
  """
  @spec unwrapPhaseMap(Evision.StructuredLight.SinusoidalPattern.t(), Evision.Mat.maybe_mat_in(), {number(), number()}) :: Evision.Mat.t() | {:error, String.t()}
  def unwrapPhaseMap(self, wrappedPhaseMap, camSize) when (is_struct(wrappedPhaseMap, Evision.Mat) or is_struct(wrappedPhaseMap, Nx.Tensor) or is_number(wrappedPhaseMap) or is_tuple(wrappedPhaseMap)) and is_tuple(camSize)
  do
    positional = [
      wrappedPhaseMap: Evision.Internal.Structurise.from_struct(wrappedPhaseMap),
      camSize: Evision.Internal.Structurise.from_struct(camSize)
    ]
    :evision_nif.structured_light_structured_light_SinusoidalPattern_unwrapPhaseMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
