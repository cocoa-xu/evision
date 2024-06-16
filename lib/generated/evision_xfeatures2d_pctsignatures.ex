defmodule Evision.XFeatures2D.PCTSignatures do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XFeatures2D.PCTSignatures` struct.

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
  def to_struct({:ok, %{class: Evision.XFeatures2D.PCTSignatures, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XFeatures2D.PCTSignatures, ref: ref}) do
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
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XFeatures2D.PCTSignatures.t()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_PCTSignatures_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Computes signature of given image.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **image**: `Evision.Mat`.

    Input image of CV_8U type.

  ##### Return
  - **signature**: `Evision.Mat.t()`.

    Output computed signature.

  Python prototype (for reference only):
  ```python3
  computeSignature(image[, signature]) -> signature
  ```
  """
  @spec computeSignature(Evision.XFeatures2D.PCTSignatures.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def computeSignature(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_computeSignature(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes signature of given image.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **image**: `Evision.Mat`.

    Input image of CV_8U type.

  ##### Return
  - **signature**: `Evision.Mat.t()`.

    Output computed signature.

  Python prototype (for reference only):
  ```python3
  computeSignature(image[, signature]) -> signature
  ```
  """
  @spec computeSignature(Evision.XFeatures2D.PCTSignatures.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def computeSignature(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_computeSignature(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Computes signatures for multiple images in parallel.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **images**: `[Evision.Mat]`.

    Vector of input images of CV_8U type.

  - **signatures**: `[Evision.Mat]`.

    Vector of computed signatures.

  Python prototype (for reference only):
  ```python3
  computeSignatures(images, signatures) -> None
  ```
  """
  @spec computeSignatures(Evision.XFeatures2D.PCTSignatures.t(), list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in())) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def computeSignatures(self, images, signatures) when is_list(images) and is_list(signatures)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      signatures: Evision.Internal.Structurise.from_struct(signatures)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_computeSignatures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Creates PCTSignatures algorithm using pre-generated sampling points
  and clusterization seeds indexes.

  ##### Positional Arguments
  - **initSamplingPoints**: `[Point2f]`.

    Sampling points used in image sampling.

  - **initClusterSeedIndexes**: `[integer()]`.

    Indexes of initial clusterization seeds.
    Its size must be lower or equal to initSamplingPoints.size().

  ##### Return
  - **retval**: `PCTSignatures`

  @return Created algorithm.

  Python prototype (for reference only):
  ```python3
  create(initSamplingPoints, initClusterSeedIndexes) -> retval
  ```
  #### Variant 2:
  Creates PCTSignatures algorithm using pre-generated sampling points
  and number of clusterization seeds. It uses the provided
  sampling points and generates its own clusterization seed indexes.

  ##### Positional Arguments
  - **initSamplingPoints**: `[Point2f]`.

    Sampling points used in image sampling.

  - **initSeedCount**: `integer()`.

    Number of initial clusterization seeds.
    Must be lower or equal to initSamplingPoints.size().

  ##### Return
  - **retval**: `PCTSignatures`

  @return Created algorithm.

  Python prototype (for reference only):
  ```python3
  create(initSamplingPoints, initSeedCount) -> retval
  ```

  """
  @spec create(list({number(), number()}), list(integer())) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def create(initSamplingPoints, initClusterSeedIndexes) when is_list(initSamplingPoints) and is_list(initClusterSeedIndexes)
  do
    positional = [
      initSamplingPoints: Evision.Internal.Structurise.from_struct(initSamplingPoints),
      initClusterSeedIndexes: Evision.Internal.Structurise.from_struct(initClusterSeedIndexes)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_create_static(positional)
    |> to_struct()
  end
  @spec create(list({number(), number()}), integer()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def create(initSamplingPoints, initSeedCount) when is_list(initSamplingPoints) and is_integer(initSeedCount)
  do
    positional = [
      initSamplingPoints: Evision.Internal.Structurise.from_struct(initSamplingPoints),
      initSeedCount: Evision.Internal.Structurise.from_struct(initSeedCount)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_create_static(positional)
    |> to_struct()
  end

  @doc """
  Creates PCTSignatures algorithm using sample and seed count.
  It generates its own sets of sampling points and clusterization seed indexes.

  ##### Keyword Arguments
  - **initSampleCount**: `integer()`.

    Number of points used for image sampling.

  - **initSeedCount**: `integer()`.

    Number of initial clusterization seeds.
    Must be lower or equal to initSampleCount

  - **pointDistribution**: `integer()`.

    Distribution of generated points. Default: UNIFORM.
    Available: UNIFORM, REGULAR, NORMAL.

  ##### Return
  - **retval**: `PCTSignatures`

  @return Created algorithm.

  Python prototype (for reference only):
  ```python3
  create([, initSampleCount[, initSeedCount[, pointDistribution]]]) -> retval
  ```
  """
  @spec create([{:initSampleCount, term()} | {:initSeedCount, term()} | {:pointDistribution, term()}] | nil) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:initSampleCount, :initSeedCount, :pointDistribution])
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates PCTSignatures algorithm using sample and seed count.
  It generates its own sets of sampling points and clusterization seed indexes.

  ##### Keyword Arguments
  - **initSampleCount**: `integer()`.

    Number of points used for image sampling.

  - **initSeedCount**: `integer()`.

    Number of initial clusterization seeds.
    Must be lower or equal to initSampleCount

  - **pointDistribution**: `integer()`.

    Distribution of generated points. Default: UNIFORM.
    Available: UNIFORM, REGULAR, NORMAL.

  ##### Return
  - **retval**: `PCTSignatures`

  @return Created algorithm.

  Python prototype (for reference only):
  ```python3
  create([, initSampleCount[, initSeedCount[, pointDistribution]]]) -> retval
  ```
  """
  @spec create() :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_create_static(positional)
    |> to_struct()
  end

  @doc """
  Draws signature in the source image and outputs the result.
  Signatures are visualized as a circle
  with radius based on signature weight
  and color based on signature color.
  Contrast and entropy are not visualized.

  ##### Positional Arguments
  - **source**: `Evision.Mat`.

    Source image.

  - **signature**: `Evision.Mat`.

    Image signature.

  ##### Keyword Arguments
  - **radiusToShorterSideRatio**: `float`.

    Determines maximal radius of signature in the output image.

  - **borderThickness**: `integer()`.

    Border thickness of the visualized signature.

  ##### Return
  - **result**: `Evision.Mat.t()`.

    Output result.

  Python prototype (for reference only):
  ```python3
  drawSignature(source, signature[, result[, radiusToShorterSideRatio[, borderThickness]]]) -> result
  ```
  """
  @spec drawSignature(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:borderThickness, term()} | {:radiusToShorterSideRatio, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawSignature(source, signature, opts) when (is_struct(source, Evision.Mat) or is_struct(source, Nx.Tensor) or is_number(source) or is_tuple(source)) and (is_struct(signature, Evision.Mat) or is_struct(signature, Nx.Tensor) or is_number(signature) or is_tuple(signature)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderThickness, :radiusToShorterSideRatio])
    positional = [
      source: Evision.Internal.Structurise.from_struct(source),
      signature: Evision.Internal.Structurise.from_struct(signature)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_drawSignature_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Draws signature in the source image and outputs the result.
  Signatures are visualized as a circle
  with radius based on signature weight
  and color based on signature color.
  Contrast and entropy are not visualized.

  ##### Positional Arguments
  - **source**: `Evision.Mat`.

    Source image.

  - **signature**: `Evision.Mat`.

    Image signature.

  ##### Keyword Arguments
  - **radiusToShorterSideRatio**: `float`.

    Determines maximal radius of signature in the output image.

  - **borderThickness**: `integer()`.

    Border thickness of the visualized signature.

  ##### Return
  - **result**: `Evision.Mat.t()`.

    Output result.

  Python prototype (for reference only):
  ```python3
  drawSignature(source, signature[, result[, radiusToShorterSideRatio[, borderThickness]]]) -> result
  ```
  """
  @spec drawSignature(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def drawSignature(source, signature) when (is_struct(source, Evision.Mat) or is_struct(source, Nx.Tensor) or is_number(source) or is_tuple(source)) and (is_struct(signature, Evision.Mat) or is_struct(signature, Nx.Tensor) or is_number(signature) or is_tuple(signature))
  do
    positional = [
      source: Evision.Internal.Structurise.from_struct(source),
      signature: Evision.Internal.Structurise.from_struct(signature)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_drawSignature_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XFeatures2D.PCTSignatures.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_PCTSignatures_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Generates initial sampling points according to selected point distribution.

  ##### Positional Arguments
  - **initPoints**: `[Point2f]`.

    Output vector where the generated points will be saved.

  - **count**: `integer()`.

    Number of points to generate.

  - **pointDistribution**: `integer()`.

    Point distribution selector.
    Available: UNIFORM, REGULAR, NORMAL.

  **Note**: Generated coordinates are in range [0..1)

  Python prototype (for reference only):
  ```python3
  generateInitPoints(initPoints, count, pointDistribution) -> None
  ```
  """
  @spec generateInitPoints(list({number(), number()}), integer(), integer()) :: :ok | {:error, String.t()}
  def generateInitPoints(initPoints, count, pointDistribution) when is_list(initPoints) and is_integer(count) and is_integer(pointDistribution)
  do
    positional = [
      initPoints: Evision.Internal.Structurise.from_struct(initPoints),
      count: Evision.Internal.Structurise.from_struct(count),
      pointDistribution: Evision.Internal.Structurise.from_struct(pointDistribution)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_generateInitPoints_static(positional)
    |> to_struct()
  end

  @doc """
  This parameter multiplied by the index of iteration gives lower limit for cluster size.
  Clusters containing fewer points than specified by the limit have their centroid dismissed
  and points are reassigned.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getClusterMinSize() -> retval
  ```
  """
  @spec getClusterMinSize(Evision.XFeatures2D.PCTSignatures.t()) :: integer() | {:error, String.t()}
  def getClusterMinSize(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getClusterMinSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XFeatures2D.PCTSignatures.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_PCTSignatures_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Distance function selector used for measuring distance between two points in k-means.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getDistanceFunction() -> retval
  ```
  """
  @spec getDistanceFunction(Evision.XFeatures2D.PCTSignatures.t()) :: integer() | {:error, String.t()}
  def getDistanceFunction(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getDistanceFunction(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Remove centroids in k-means whose weight is lesser or equal to given threshold.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getDropThreshold() -> retval
  ```
  """
  @spec getDropThreshold(Evision.XFeatures2D.PCTSignatures.t()) :: number() | {:error, String.t()}
  def getDropThreshold(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getDropThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Color resolution of the greyscale bitmap represented in allocated bits
  (i.e., value 4 means that 16 shades of grey are used).
  The greyscale bitmap is used for computing contrast and entropy values.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getGrayscaleBits() -> retval
  ```
  """
  @spec getGrayscaleBits(Evision.XFeatures2D.PCTSignatures.t()) :: integer() | {:error, String.t()}
  def getGrayscaleBits(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getGrayscaleBits(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Number of initial seeds (initial number of clusters) for the k-means algorithm.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getInitSeedCount() -> retval
  ```
  """
  @spec getInitSeedCount(Evision.XFeatures2D.PCTSignatures.t()) :: integer() | {:error, String.t()}
  def getInitSeedCount(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getInitSeedCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Initial seeds (initial number of clusters) for the k-means algorithm.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `[integer()]`

  Python prototype (for reference only):
  ```python3
  getInitSeedIndexes() -> retval
  ```
  """
  @spec getInitSeedIndexes(Evision.XFeatures2D.PCTSignatures.t()) :: list(integer()) | {:error, String.t()}
  def getInitSeedIndexes(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getInitSeedIndexes(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Number of iterations of the k-means clustering.
  We use fixed number of iterations, since the modified clustering is pruning clusters
  (not iteratively refining k clusters).

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getIterationCount() -> retval
  ```
  """
  @spec getIterationCount(Evision.XFeatures2D.PCTSignatures.t()) :: integer() | {:error, String.t()}
  def getIterationCount(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getIterationCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Threshold euclidean distance between two centroids.
  If two cluster centers are closer than this distance,
  one of the centroid is dismissed and points are reassigned.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getJoiningDistance() -> retval
  ```
  """
  @spec getJoiningDistance(Evision.XFeatures2D.PCTSignatures.t()) :: number() | {:error, String.t()}
  def getJoiningDistance(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getJoiningDistance(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Maximal number of generated clusters. If the number is exceeded,
  the clusters are sorted by their weights and the smallest clusters are cropped.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxClustersCount() -> retval
  ```
  """
  @spec getMaxClustersCount(Evision.XFeatures2D.PCTSignatures.t()) :: integer() | {:error, String.t()}
  def getMaxClustersCount(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getMaxClustersCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Number of initial samples taken from the image.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getSampleCount() -> retval
  ```
  """
  @spec getSampleCount(Evision.XFeatures2D.PCTSignatures.t()) :: integer() | {:error, String.t()}
  def getSampleCount(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getSampleCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Initial samples taken from the image.
  These sampled features become the input for clustering.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `[Point2f]`

  Python prototype (for reference only):
  ```python3
  getSamplingPoints() -> retval
  ```
  """
  @spec getSamplingPoints(Evision.XFeatures2D.PCTSignatures.t()) :: list({number(), number()}) | {:error, String.t()}
  def getSamplingPoints(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getSamplingPoints(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getWeightA() -> retval
  ```
  """
  @spec getWeightA(Evision.XFeatures2D.PCTSignatures.t()) :: number() | {:error, String.t()}
  def getWeightA(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getWeightA(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getWeightB() -> retval
  ```
  """
  @spec getWeightB(Evision.XFeatures2D.PCTSignatures.t()) :: number() | {:error, String.t()}
  def getWeightB(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getWeightB(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getWeightContrast() -> retval
  ```
  """
  @spec getWeightContrast(Evision.XFeatures2D.PCTSignatures.t()) :: number() | {:error, String.t()}
  def getWeightContrast(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getWeightContrast(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getWeightEntropy() -> retval
  ```
  """
  @spec getWeightEntropy(Evision.XFeatures2D.PCTSignatures.t()) :: number() | {:error, String.t()}
  def getWeightEntropy(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getWeightEntropy(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getWeightL() -> retval
  ```
  """
  @spec getWeightL(Evision.XFeatures2D.PCTSignatures.t()) :: number() | {:error, String.t()}
  def getWeightL(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getWeightL(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getWeightX() -> retval
  ```
  """
  @spec getWeightX(Evision.XFeatures2D.PCTSignatures.t()) :: number() | {:error, String.t()}
  def getWeightX(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getWeightX(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getWeightY() -> retval
  ```
  """
  @spec getWeightY(Evision.XFeatures2D.PCTSignatures.t()) :: number() | {:error, String.t()}
  def getWeightY(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getWeightY(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Size of the texture sampling window used to compute contrast and entropy
  (center of the window is always in the pixel selected by x,y coordinates
  of the corresponding feature sample).

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getWindowRadius() -> retval
  ```
  """
  @spec getWindowRadius(Evision.XFeatures2D.PCTSignatures.t()) :: integer() | {:error, String.t()}
  def getWindowRadius(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_getWindowRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XFeatures2D.PCTSignatures.t(), Evision.FileNode.t()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.xfeatures2d_PCTSignatures_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XFeatures2D.PCTSignatures.t(), binary()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.xfeatures2d_PCTSignatures_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  This parameter multiplied by the index of iteration gives lower limit for cluster size.
  Clusters containing fewer points than specified by the limit have their centroid dismissed
  and points are reassigned.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **clusterMinSize**: `integer()`

  Python prototype (for reference only):
  ```python3
  setClusterMinSize(clusterMinSize) -> None
  ```
  """
  @spec setClusterMinSize(Evision.XFeatures2D.PCTSignatures.t(), integer()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setClusterMinSize(self, clusterMinSize) when is_integer(clusterMinSize)
  do
    positional = [
      clusterMinSize: Evision.Internal.Structurise.from_struct(clusterMinSize)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setClusterMinSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Distance function selector used for measuring distance between two points in k-means.
  Available: L0_25, L0_5, L1, L2, L2SQUARED, L5, L_INFINITY.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **distanceFunction**: `integer()`

  Python prototype (for reference only):
  ```python3
  setDistanceFunction(distanceFunction) -> None
  ```
  """
  @spec setDistanceFunction(Evision.XFeatures2D.PCTSignatures.t(), integer()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setDistanceFunction(self, distanceFunction) when is_integer(distanceFunction)
  do
    positional = [
      distanceFunction: Evision.Internal.Structurise.from_struct(distanceFunction)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setDistanceFunction(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Remove centroids in k-means whose weight is lesser or equal to given threshold.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **dropThreshold**: `float`

  Python prototype (for reference only):
  ```python3
  setDropThreshold(dropThreshold) -> None
  ```
  """
  @spec setDropThreshold(Evision.XFeatures2D.PCTSignatures.t(), number()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setDropThreshold(self, dropThreshold) when is_float(dropThreshold)
  do
    positional = [
      dropThreshold: Evision.Internal.Structurise.from_struct(dropThreshold)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setDropThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Color resolution of the greyscale bitmap represented in allocated bits
  (i.e., value 4 means that 16 shades of grey are used).
  The greyscale bitmap is used for computing contrast and entropy values.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **grayscaleBits**: `integer()`

  Python prototype (for reference only):
  ```python3
  setGrayscaleBits(grayscaleBits) -> None
  ```
  """
  @spec setGrayscaleBits(Evision.XFeatures2D.PCTSignatures.t(), integer()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setGrayscaleBits(self, grayscaleBits) when is_integer(grayscaleBits)
  do
    positional = [
      grayscaleBits: Evision.Internal.Structurise.from_struct(grayscaleBits)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setGrayscaleBits(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Initial seed indexes for the k-means algorithm.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **initSeedIndexes**: `[integer()]`

  Python prototype (for reference only):
  ```python3
  setInitSeedIndexes(initSeedIndexes) -> None
  ```
  """
  @spec setInitSeedIndexes(Evision.XFeatures2D.PCTSignatures.t(), list(integer())) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setInitSeedIndexes(self, initSeedIndexes) when is_list(initSeedIndexes)
  do
    positional = [
      initSeedIndexes: Evision.Internal.Structurise.from_struct(initSeedIndexes)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setInitSeedIndexes(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Number of iterations of the k-means clustering.
  We use fixed number of iterations, since the modified clustering is pruning clusters
  (not iteratively refining k clusters).

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **iterationCount**: `integer()`

  Python prototype (for reference only):
  ```python3
  setIterationCount(iterationCount) -> None
  ```
  """
  @spec setIterationCount(Evision.XFeatures2D.PCTSignatures.t(), integer()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setIterationCount(self, iterationCount) when is_integer(iterationCount)
  do
    positional = [
      iterationCount: Evision.Internal.Structurise.from_struct(iterationCount)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setIterationCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Threshold euclidean distance between two centroids.
  If two cluster centers are closer than this distance,
  one of the centroid is dismissed and points are reassigned.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **joiningDistance**: `float`

  Python prototype (for reference only):
  ```python3
  setJoiningDistance(joiningDistance) -> None
  ```
  """
  @spec setJoiningDistance(Evision.XFeatures2D.PCTSignatures.t(), number()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setJoiningDistance(self, joiningDistance) when is_float(joiningDistance)
  do
    positional = [
      joiningDistance: Evision.Internal.Structurise.from_struct(joiningDistance)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setJoiningDistance(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Maximal number of generated clusters. If the number is exceeded,
  the clusters are sorted by their weights and the smallest clusters are cropped.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **maxClustersCount**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxClustersCount(maxClustersCount) -> None
  ```
  """
  @spec setMaxClustersCount(Evision.XFeatures2D.PCTSignatures.t(), integer()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setMaxClustersCount(self, maxClustersCount) when is_integer(maxClustersCount)
  do
    positional = [
      maxClustersCount: Evision.Internal.Structurise.from_struct(maxClustersCount)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setMaxClustersCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets sampling points used to sample the input image.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **samplingPoints**: `[Point2f]`.

    Vector of sampling points in range [0..1)

  **Note**: Number of sampling points must be greater or equal to clusterization seed count.

  Python prototype (for reference only):
  ```python3
  setSamplingPoints(samplingPoints) -> None
  ```
  """
  @spec setSamplingPoints(Evision.XFeatures2D.PCTSignatures.t(), list({number(), number()})) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setSamplingPoints(self, samplingPoints) when is_list(samplingPoints)
  do
    positional = [
      samplingPoints: Evision.Internal.Structurise.from_struct(samplingPoints)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setSamplingPoints(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Translations of the individual axes of the feature space.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **idx**: `integer()`.

    ID of the translation

  - **value**: `float`.

    Value of the translation

  **Note**: 
         WEIGHT_IDX = 0;
         X_IDX = 1;
         Y_IDX = 2;
         L_IDX = 3;
         A_IDX = 4;
         B_IDX = 5;
         CONTRAST_IDX = 6;
         ENTROPY_IDX = 7;

  Python prototype (for reference only):
  ```python3
  setTranslation(idx, value) -> None
  ```
  """
  @spec setTranslation(Evision.XFeatures2D.PCTSignatures.t(), integer(), number()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setTranslation(self, idx, value) when is_integer(idx) and is_float(value)
  do
    positional = [
      idx: Evision.Internal.Structurise.from_struct(idx),
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setTranslation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Translations of the individual axes of the feature space.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **translations**: `[float]`.

    Values of all translations.

  **Note**: 
         WEIGHT_IDX = 0;
         X_IDX = 1;
         Y_IDX = 2;
         L_IDX = 3;
         A_IDX = 4;
         B_IDX = 5;
         CONTRAST_IDX = 6;
         ENTROPY_IDX = 7;

  Python prototype (for reference only):
  ```python3
  setTranslations(translations) -> None
  ```
  """
  @spec setTranslations(Evision.XFeatures2D.PCTSignatures.t(), list(number())) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setTranslations(self, translations) when is_list(translations)
  do
    positional = [
      translations: Evision.Internal.Structurise.from_struct(translations)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setTranslations(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **idx**: `integer()`.

    ID of the weight

  - **value**: `float`.

    Value of the weight

  **Note**: 
         WEIGHT_IDX = 0;
         X_IDX = 1;
         Y_IDX = 2;
         L_IDX = 3;
         A_IDX = 4;
         B_IDX = 5;
         CONTRAST_IDX = 6;
         ENTROPY_IDX = 7;

  Python prototype (for reference only):
  ```python3
  setWeight(idx, value) -> None
  ```
  """
  @spec setWeight(Evision.XFeatures2D.PCTSignatures.t(), integer(), number()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setWeight(self, idx, value) when is_integer(idx) and is_float(value)
  do
    positional = [
      idx: Evision.Internal.Structurise.from_struct(idx),
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setWeight(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **weight**: `float`

  Python prototype (for reference only):
  ```python3
  setWeightA(weight) -> None
  ```
  """
  @spec setWeightA(Evision.XFeatures2D.PCTSignatures.t(), number()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setWeightA(self, weight) when is_float(weight)
  do
    positional = [
      weight: Evision.Internal.Structurise.from_struct(weight)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setWeightA(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **weight**: `float`

  Python prototype (for reference only):
  ```python3
  setWeightB(weight) -> None
  ```
  """
  @spec setWeightB(Evision.XFeatures2D.PCTSignatures.t(), number()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setWeightB(self, weight) when is_float(weight)
  do
    positional = [
      weight: Evision.Internal.Structurise.from_struct(weight)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setWeightB(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **weight**: `float`

  Python prototype (for reference only):
  ```python3
  setWeightContrast(weight) -> None
  ```
  """
  @spec setWeightContrast(Evision.XFeatures2D.PCTSignatures.t(), number()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setWeightContrast(self, weight) when is_float(weight)
  do
    positional = [
      weight: Evision.Internal.Structurise.from_struct(weight)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setWeightContrast(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **weight**: `float`

  Python prototype (for reference only):
  ```python3
  setWeightEntropy(weight) -> None
  ```
  """
  @spec setWeightEntropy(Evision.XFeatures2D.PCTSignatures.t(), number()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setWeightEntropy(self, weight) when is_float(weight)
  do
    positional = [
      weight: Evision.Internal.Structurise.from_struct(weight)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setWeightEntropy(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **weight**: `float`

  Python prototype (for reference only):
  ```python3
  setWeightL(weight) -> None
  ```
  """
  @spec setWeightL(Evision.XFeatures2D.PCTSignatures.t(), number()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setWeightL(self, weight) when is_float(weight)
  do
    positional = [
      weight: Evision.Internal.Structurise.from_struct(weight)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setWeightL(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **weight**: `float`

  Python prototype (for reference only):
  ```python3
  setWeightX(weight) -> None
  ```
  """
  @spec setWeightX(Evision.XFeatures2D.PCTSignatures.t(), number()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setWeightX(self, weight) when is_float(weight)
  do
    positional = [
      weight: Evision.Internal.Structurise.from_struct(weight)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setWeightX(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space
  (x,y = position; L,a,b = color in CIE Lab space; c = contrast. e = entropy)

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **weight**: `float`

  Python prototype (for reference only):
  ```python3
  setWeightY(weight) -> None
  ```
  """
  @spec setWeightY(Evision.XFeatures2D.PCTSignatures.t(), number()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setWeightY(self, weight) when is_float(weight)
  do
    positional = [
      weight: Evision.Internal.Structurise.from_struct(weight)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setWeightY(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Weights (multiplicative constants) that linearly stretch individual axes of the feature space.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **weights**: `[float]`.

    Values of all weights.

  **Note**: 
         WEIGHT_IDX = 0;
         X_IDX = 1;
         Y_IDX = 2;
         L_IDX = 3;
         A_IDX = 4;
         B_IDX = 5;
         CONTRAST_IDX = 6;
         ENTROPY_IDX = 7;

  Python prototype (for reference only):
  ```python3
  setWeights(weights) -> None
  ```
  """
  @spec setWeights(Evision.XFeatures2D.PCTSignatures.t(), list(number())) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setWeights(self, weights) when is_list(weights)
  do
    positional = [
      weights: Evision.Internal.Structurise.from_struct(weights)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setWeights(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Size of the texture sampling window used to compute contrast and entropy
  (center of the window is always in the pixel selected by x,y coordinates
  of the corresponding feature sample).

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **radius**: `integer()`

  Python prototype (for reference only):
  ```python3
  setWindowRadius(radius) -> None
  ```
  """
  @spec setWindowRadius(Evision.XFeatures2D.PCTSignatures.t(), integer()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def setWindowRadius(self, radius) when is_integer(radius)
  do
    positional = [
      radius: Evision.Internal.Structurise.from_struct(radius)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignatures_setWindowRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XFeatures2D.PCTSignatures.t(), Evision.FileStorage.t(), binary()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.xfeatures2d_PCTSignatures_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignatures.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XFeatures2D.PCTSignatures.t(), Evision.FileStorage.t()) :: Evision.XFeatures2D.PCTSignatures.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.xfeatures2d_PCTSignatures_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
