defmodule Evision.ShapeContextDistanceExtractor do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ShapeContextDistanceExtractor` struct.

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
  def to_struct({:ok, %{class: Evision.ShapeContextDistanceExtractor, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ShapeContextDistanceExtractor, ref: ref}) do
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
  getAngularBins

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getAngularBins() -> retval
  ```
  """
  @spec getAngularBins(Evision.ShapeContextDistanceExtractor.t()) :: integer() | {:error, String.t()}
  def getAngularBins(self) do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getAngularBins(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBendingEnergyWeight

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getBendingEnergyWeight() -> retval
  ```
  """
  @spec getBendingEnergyWeight(Evision.ShapeContextDistanceExtractor.t()) :: number() | {:error, String.t()}
  def getBendingEnergyWeight(self) do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getBendingEnergyWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCostExtractor

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **retval**: `Evision.HistogramCostExtractor.t()`

  Python prototype (for reference only):
  ```python3
  getCostExtractor() -> retval
  ```
  """
  @spec getCostExtractor(Evision.ShapeContextDistanceExtractor.t()) :: Evision.HistogramCostExtractor.t() | {:error, String.t()}
  def getCostExtractor(self) do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getCostExtractor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getImageAppearanceWeight

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getImageAppearanceWeight() -> retval
  ```
  """
  @spec getImageAppearanceWeight(Evision.ShapeContextDistanceExtractor.t()) :: number() | {:error, String.t()}
  def getImageAppearanceWeight(self) do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getImageAppearanceWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getImages

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **image1**: `Evision.Mat.t()`.
  - **image2**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getImages([, image1[, image2]]) -> image1, image2
  ```
  """
  @spec getImages(Evision.ShapeContextDistanceExtractor.t(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getImages(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getImages(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getImages

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **image1**: `Evision.Mat.t()`.
  - **image2**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  getImages([, image1[, image2]]) -> image1, image2
  ```
  """
  @spec getImages(Evision.ShapeContextDistanceExtractor.t()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def getImages(self) do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getImages(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getInnerRadius

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getInnerRadius() -> retval
  ```
  """
  @spec getInnerRadius(Evision.ShapeContextDistanceExtractor.t()) :: number() | {:error, String.t()}
  def getInnerRadius(self) do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getInnerRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getIterations

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getIterations() -> retval
  ```
  """
  @spec getIterations(Evision.ShapeContextDistanceExtractor.t()) :: integer() | {:error, String.t()}
  def getIterations(self) do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getIterations(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getOuterRadius

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getOuterRadius() -> retval
  ```
  """
  @spec getOuterRadius(Evision.ShapeContextDistanceExtractor.t()) :: number() | {:error, String.t()}
  def getOuterRadius(self) do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getOuterRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRadialBins

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getRadialBins() -> retval
  ```
  """
  @spec getRadialBins(Evision.ShapeContextDistanceExtractor.t()) :: integer() | {:error, String.t()}
  def getRadialBins(self) do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getRadialBins(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRotationInvariant

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getRotationInvariant() -> retval
  ```
  """
  @spec getRotationInvariant(Evision.ShapeContextDistanceExtractor.t()) :: boolean() | {:error, String.t()}
  def getRotationInvariant(self) do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getRotationInvariant(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getShapeContextWeight

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getShapeContextWeight() -> retval
  ```
  """
  @spec getShapeContextWeight(Evision.ShapeContextDistanceExtractor.t()) :: number() | {:error, String.t()}
  def getShapeContextWeight(self) do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getShapeContextWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getStdDev

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getStdDev() -> retval
  ```
  """
  @spec getStdDev(Evision.ShapeContextDistanceExtractor.t()) :: number() | {:error, String.t()}
  def getStdDev(self) do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getStdDev(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTransformAlgorithm

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`

  ##### Return
  - **retval**: `Evision.ShapeTransformer.t()`

  Python prototype (for reference only):
  ```python3
  getTransformAlgorithm() -> retval
  ```
  """
  @spec getTransformAlgorithm(Evision.ShapeContextDistanceExtractor.t()) :: Evision.ShapeTransformer.t() | {:error, String.t()}
  def getTransformAlgorithm(self) do
    positional = [
    ]
    :evision_nif.shapeContextDistanceExtractor_getTransformAlgorithm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Establish the number of angular bins for the Shape Context Descriptor used in the shape matching
  pipeline.

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`
  - **nAngularBins**: `integer()`.

    The number of angular bins in the shape context descriptor.

  Python prototype (for reference only):
  ```python3
  setAngularBins(nAngularBins) -> None
  ```
  """
  @spec setAngularBins(Evision.ShapeContextDistanceExtractor.t(), integer()) :: Evision.ShapeContextDistanceExtractor.t() | {:error, String.t()}
  def setAngularBins(self, nAngularBins) when is_integer(nAngularBins)
  do
    positional = [
      nAngularBins: Evision.Internal.Structurise.from_struct(nAngularBins)
    ]
    :evision_nif.shapeContextDistanceExtractor_setAngularBins(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the weight of the Bending Energy in the final value of the shape distance. The bending energy
  definition depends on what transformation is being used to align the shapes. The final value of the
  shape distance is a user-defined linear combination of the shape context distance, an image
  appearance distance, and a bending energy.

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`
  - **bendingEnergyWeight**: `float`.

    The weight of the Bending Energy in the final distance value.

  Python prototype (for reference only):
  ```python3
  setBendingEnergyWeight(bendingEnergyWeight) -> None
  ```
  """
  @spec setBendingEnergyWeight(Evision.ShapeContextDistanceExtractor.t(), number()) :: Evision.ShapeContextDistanceExtractor.t() | {:error, String.t()}
  def setBendingEnergyWeight(self, bendingEnergyWeight) when is_float(bendingEnergyWeight)
  do
    positional = [
      bendingEnergyWeight: Evision.Internal.Structurise.from_struct(bendingEnergyWeight)
    ]
    :evision_nif.shapeContextDistanceExtractor_setBendingEnergyWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the algorithm used for building the shape context descriptor cost matrix.

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`
  - **comparer**: `Evision.HistogramCostExtractor`.

    Smart pointer to a HistogramCostExtractor, an algorithm that defines the cost
    matrix between descriptors.

  Python prototype (for reference only):
  ```python3
  setCostExtractor(comparer) -> None
  ```
  """
  @spec setCostExtractor(Evision.ShapeContextDistanceExtractor.t(), Evision.HistogramCostExtractor.t()) :: Evision.ShapeContextDistanceExtractor.t() | {:error, String.t()}
  def setCostExtractor(self, comparer) when is_struct(comparer, Evision.HistogramCostExtractor)
  do
    positional = [
      comparer: Evision.Internal.Structurise.from_struct(comparer)
    ]
    :evision_nif.shapeContextDistanceExtractor_setCostExtractor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the weight of the Image Appearance cost in the final value of the shape distance. The image
  appearance cost is defined as the sum of squared brightness differences in Gaussian windows around
  corresponding image points. The final value of the shape distance is a user-defined linear
  combination of the shape context distance, an image appearance distance, and a bending energy. If
  this value is set to a number different from 0, is mandatory to set the images that correspond to
  each shape.

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`
  - **imageAppearanceWeight**: `float`.

    The weight of the appearance cost in the final distance value.

  Python prototype (for reference only):
  ```python3
  setImageAppearanceWeight(imageAppearanceWeight) -> None
  ```
  """
  @spec setImageAppearanceWeight(Evision.ShapeContextDistanceExtractor.t(), number()) :: Evision.ShapeContextDistanceExtractor.t() | {:error, String.t()}
  def setImageAppearanceWeight(self, imageAppearanceWeight) when is_float(imageAppearanceWeight)
  do
    positional = [
      imageAppearanceWeight: Evision.Internal.Structurise.from_struct(imageAppearanceWeight)
    ]
    :evision_nif.shapeContextDistanceExtractor_setImageAppearanceWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the images that correspond to each shape. This images are used in the calculation of the Image
  Appearance cost.

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`
  - **image1**: `Evision.Mat`.

    Image corresponding to the shape defined by contours1.

  - **image2**: `Evision.Mat`.

    Image corresponding to the shape defined by contours2.

  Python prototype (for reference only):
  ```python3
  setImages(image1, image2) -> None
  ```
  """
  @spec setImages(Evision.ShapeContextDistanceExtractor.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.ShapeContextDistanceExtractor.t() | {:error, String.t()}
  def setImages(self, image1, image2) when (is_struct(image1, Evision.Mat) or is_struct(image1, Nx.Tensor) or is_number(image1) or is_tuple(image1)) and (is_struct(image2, Evision.Mat) or is_struct(image2, Nx.Tensor) or is_number(image2) or is_tuple(image2))
  do
    positional = [
      image1: Evision.Internal.Structurise.from_struct(image1),
      image2: Evision.Internal.Structurise.from_struct(image2)
    ]
    :evision_nif.shapeContextDistanceExtractor_setImages(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the inner radius of the shape context descriptor.

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`
  - **innerRadius**: `float`.

    The value of the inner radius.

  Python prototype (for reference only):
  ```python3
  setInnerRadius(innerRadius) -> None
  ```
  """
  @spec setInnerRadius(Evision.ShapeContextDistanceExtractor.t(), number()) :: Evision.ShapeContextDistanceExtractor.t() | {:error, String.t()}
  def setInnerRadius(self, innerRadius) when is_float(innerRadius)
  do
    positional = [
      innerRadius: Evision.Internal.Structurise.from_struct(innerRadius)
    ]
    :evision_nif.shapeContextDistanceExtractor_setInnerRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setIterations

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`
  - **iterations**: `integer()`

  Python prototype (for reference only):
  ```python3
  setIterations(iterations) -> None
  ```
  """
  @spec setIterations(Evision.ShapeContextDistanceExtractor.t(), integer()) :: Evision.ShapeContextDistanceExtractor.t() | {:error, String.t()}
  def setIterations(self, iterations) when is_integer(iterations)
  do
    positional = [
      iterations: Evision.Internal.Structurise.from_struct(iterations)
    ]
    :evision_nif.shapeContextDistanceExtractor_setIterations(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the outer radius of the shape context descriptor.

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`
  - **outerRadius**: `float`.

    The value of the outer radius.

  Python prototype (for reference only):
  ```python3
  setOuterRadius(outerRadius) -> None
  ```
  """
  @spec setOuterRadius(Evision.ShapeContextDistanceExtractor.t(), number()) :: Evision.ShapeContextDistanceExtractor.t() | {:error, String.t()}
  def setOuterRadius(self, outerRadius) when is_float(outerRadius)
  do
    positional = [
      outerRadius: Evision.Internal.Structurise.from_struct(outerRadius)
    ]
    :evision_nif.shapeContextDistanceExtractor_setOuterRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Establish the number of radial bins for the Shape Context Descriptor used in the shape matching
  pipeline.

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`
  - **nRadialBins**: `integer()`.

    The number of radial bins in the shape context descriptor.

  Python prototype (for reference only):
  ```python3
  setRadialBins(nRadialBins) -> None
  ```
  """
  @spec setRadialBins(Evision.ShapeContextDistanceExtractor.t(), integer()) :: Evision.ShapeContextDistanceExtractor.t() | {:error, String.t()}
  def setRadialBins(self, nRadialBins) when is_integer(nRadialBins)
  do
    positional = [
      nRadialBins: Evision.Internal.Structurise.from_struct(nRadialBins)
    ]
    :evision_nif.shapeContextDistanceExtractor_setRadialBins(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRotationInvariant

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`
  - **rotationInvariant**: `bool`

  Python prototype (for reference only):
  ```python3
  setRotationInvariant(rotationInvariant) -> None
  ```
  """
  @spec setRotationInvariant(Evision.ShapeContextDistanceExtractor.t(), boolean()) :: Evision.ShapeContextDistanceExtractor.t() | {:error, String.t()}
  def setRotationInvariant(self, rotationInvariant) when is_boolean(rotationInvariant)
  do
    positional = [
      rotationInvariant: Evision.Internal.Structurise.from_struct(rotationInvariant)
    ]
    :evision_nif.shapeContextDistanceExtractor_setRotationInvariant(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the weight of the shape context distance in the final value of the shape distance. The shape
  context distance between two shapes is defined as the symmetric sum of shape context matching costs
  over best matching points. The final value of the shape distance is a user-defined linear
  combination of the shape context distance, an image appearance distance, and a bending energy.

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`
  - **shapeContextWeight**: `float`.

    The weight of the shape context distance in the final distance value.

  Python prototype (for reference only):
  ```python3
  setShapeContextWeight(shapeContextWeight) -> None
  ```
  """
  @spec setShapeContextWeight(Evision.ShapeContextDistanceExtractor.t(), number()) :: Evision.ShapeContextDistanceExtractor.t() | {:error, String.t()}
  def setShapeContextWeight(self, shapeContextWeight) when is_float(shapeContextWeight)
  do
    positional = [
      shapeContextWeight: Evision.Internal.Structurise.from_struct(shapeContextWeight)
    ]
    :evision_nif.shapeContextDistanceExtractor_setShapeContextWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the value of the standard deviation for the Gaussian window for the image appearance cost.

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`
  - **sigma**: `float`.

    Standard Deviation.

  Python prototype (for reference only):
  ```python3
  setStdDev(sigma) -> None
  ```
  """
  @spec setStdDev(Evision.ShapeContextDistanceExtractor.t(), number()) :: Evision.ShapeContextDistanceExtractor.t() | {:error, String.t()}
  def setStdDev(self, sigma) when is_float(sigma)
  do
    positional = [
      sigma: Evision.Internal.Structurise.from_struct(sigma)
    ]
    :evision_nif.shapeContextDistanceExtractor_setStdDev(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the algorithm used for aligning the shapes.

  ##### Positional Arguments
  - **self**: `Evision.ShapeContextDistanceExtractor.t()`
  - **transformer**: `Evision.ShapeTransformer`.

    Smart pointer to a ShapeTransformer, an algorithm that defines the aligning
    transformation.

  Python prototype (for reference only):
  ```python3
  setTransformAlgorithm(transformer) -> None
  ```
  """
  @spec setTransformAlgorithm(Evision.ShapeContextDistanceExtractor.t(), Evision.ShapeTransformer.t()) :: Evision.ShapeContextDistanceExtractor.t() | {:error, String.t()}
  def setTransformAlgorithm(self, transformer) when is_struct(transformer, Evision.ShapeTransformer)
  do
    positional = [
      transformer: Evision.Internal.Structurise.from_struct(transformer)
    ]
    :evision_nif.shapeContextDistanceExtractor_setTransformAlgorithm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
