defmodule Evision.XFeatures2D.HarrisLaplaceFeatureDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XFeatures2D.HarrisLaplaceFeatureDetector` struct.

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
  def to_struct({:ok, %{class: Evision.XFeatures2D.HarrisLaplaceFeatureDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XFeatures2D.HarrisLaplaceFeatureDetector, ref: ref}) do
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
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **images**: `[Evision.Mat]`.

    Image set.

  ##### Return
  - **keypoints**: `[[Evision.KeyPoint]]`.

    Input collection of keypoints. Keypoints for which a descriptor cannot be
    computed are removed. Sometimes new keypoints can be added, for example: SIFT duplicates keypoint
    with several dominant orientations (for each orientation).

  - **descriptors**: `[Evision.Mat]`.

    Computed descriptors. In the second variant of the method descriptors[i] are
    descriptors computed for a keypoints[i]. Row j is the keypoints (or keypoints[i]) is the
    descriptor for keypoint j-th keypoint.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  compute(images, keypoints[, descriptors]) -> keypoints, descriptors
  ```
  #### Variant 2:
  Computes the descriptors for a set of keypoints detected in an image (first variant) or image set
  (second variant).

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **image**: `Evision.Mat`.

    Image.

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`.

    Input collection of keypoints. Keypoints for which a descriptor cannot be
    computed are removed. Sometimes new keypoints can be added, for example: SIFT duplicates keypoint
    with several dominant orientations (for each orientation).

  - **descriptors**: `Evision.Mat.t()`.

    Computed descriptors. In the second variant of the method descriptors[i] are
    descriptors computed for a keypoints[i]. Row j is the keypoints (or keypoints[i]) is the
    descriptor for keypoint j-th keypoint.

  Python prototype (for reference only):
  ```python3
  compute(image, keypoints[, descriptors]) -> keypoints, descriptors
  ```

  """
  @spec compute(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t())), [{atom(), term()},...] | nil) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints, opts) when is_list(images) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t()), [{atom(), term()},...] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **images**: `[Evision.Mat]`.

    Image set.

  ##### Return
  - **keypoints**: `[[Evision.KeyPoint]]`.

    Input collection of keypoints. Keypoints for which a descriptor cannot be
    computed are removed. Sometimes new keypoints can be added, for example: SIFT duplicates keypoint
    with several dominant orientations (for each orientation).

  - **descriptors**: `[Evision.Mat]`.

    Computed descriptors. In the second variant of the method descriptors[i] are
    descriptors computed for a keypoints[i]. Row j is the keypoints (or keypoints[i]) is the
    descriptor for keypoint j-th keypoint.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  compute(images, keypoints[, descriptors]) -> keypoints, descriptors
  ```
  #### Variant 2:
  Computes the descriptors for a set of keypoints detected in an image (first variant) or image set
  (second variant).

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **image**: `Evision.Mat`.

    Image.

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`.

    Input collection of keypoints. Keypoints for which a descriptor cannot be
    computed are removed. Sometimes new keypoints can be added, for example: SIFT duplicates keypoint
    with several dominant orientations (for each orientation).

  - **descriptors**: `Evision.Mat.t()`.

    Computed descriptors. In the second variant of the method descriptors[i] are
    descriptors computed for a keypoints[i]. Row j is the keypoints (or keypoints[i]) is the
    descriptor for keypoint j-th keypoint.

  Python prototype (for reference only):
  ```python3
  compute(image, keypoints[, descriptors]) -> keypoints, descriptors
  ```

  """
  @spec compute(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t()))) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints) when is_list(images) and is_list(keypoints)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t())) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Creates a new implementation instance.
  ##### Keyword Arguments
  - **numOctaves**: `integer()`.

    the number of octaves in the scale-space pyramid

  - **corn_thresh**: `float`.

    the threshold for the Harris cornerness measure

  - **dOG_thresh**: `float`.

    the threshold for the Difference-of-Gaussians scale selection

  - **maxCorners**: `integer()`.

    the maximum number of corners to consider

  - **num_layers**: `integer()`.

    the number of intermediate scales per octave

  ##### Return
  - **retval**: `HarrisLaplaceFeatureDetector`

  Python prototype (for reference only):
  ```python3
  create([, numOctaves[, corn_thresh[, DOG_thresh[, maxCorners[, num_layers]]]]]) -> retval
  ```
  """
  @spec create([{:corn_thresh, term()} | {:dOG_thresh, term()} | {:maxCorners, term()} | {:numOctaves, term()} | {:num_layers, term()}] | nil) :: Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:corn_thresh, :dOG_thresh, :maxCorners, :numOctaves, :num_layers])
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_HarrisLaplaceFeatureDetector_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a new implementation instance.
  ##### Keyword Arguments
  - **numOctaves**: `integer()`.

    the number of octaves in the scale-space pyramid

  - **corn_thresh**: `float`.

    the threshold for the Harris cornerness measure

  - **dOG_thresh**: `float`.

    the threshold for the Difference-of-Gaussians scale selection

  - **maxCorners**: `integer()`.

    the maximum number of corners to consider

  - **num_layers**: `integer()`.

    the number of intermediate scales per octave

  ##### Return
  - **retval**: `HarrisLaplaceFeatureDetector`

  Python prototype (for reference only):
  ```python3
  create([, numOctaves[, corn_thresh[, DOG_thresh[, maxCorners[, num_layers]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_HarrisLaplaceFeatureDetector_create_static(positional)
    |> to_struct()
  end

  @doc """
  defaultNorm

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  defaultNorm() -> retval
  ```
  """
  @spec defaultNorm(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()) :: integer() | {:error, String.t()}
  def defaultNorm(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_defaultNorm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorSize

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorSize() -> retval
  ```
  """
  @spec descriptorSize(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()) :: integer() | {:error, String.t()}
  def descriptorSize(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_descriptorSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorType

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorType() -> retval
  ```
  """
  @spec descriptorType(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()) :: integer() | {:error, String.t()}
  def descriptorType(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_descriptorType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **images**: `[Evision.Mat]`.

    Image set.

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    Masks for each input image specifying where to look for keypoints (optional).
    masks[i] is a mask for images[i].

  ##### Return
  - **keypoints**: `[[Evision.KeyPoint]]`.

    The detected keypoints. In the second variant of the method keypoints[i] is a set
    of keypoints detected in images[i] .

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  detect(images[, masks]) -> keypoints
  ```
  #### Variant 2:
  Detects keypoints in an image (first variant) or image set (second variant).

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **image**: `Evision.Mat`.

    Image.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying where to look for keypoints (optional). It must be a 8-bit integer
    matrix with non-zero values in the region of interest.

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`.

    The detected keypoints. In the second variant of the method keypoints[i] is a set
    of keypoints detected in images[i] .

  Python prototype (for reference only):
  ```python3
  detect(image[, mask]) -> keypoints
  ```

  """
  @spec detect(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), list(Evision.Mat.maybe_mat_in()), [{:masks, term()}] | nil) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images, opts) when is_list(images) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **images**: `[Evision.Mat]`.

    Image set.

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    Masks for each input image specifying where to look for keypoints (optional).
    masks[i] is a mask for images[i].

  ##### Return
  - **keypoints**: `[[Evision.KeyPoint]]`.

    The detected keypoints. In the second variant of the method keypoints[i] is a set
    of keypoints detected in images[i] .

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  detect(images[, masks]) -> keypoints
  ```
  #### Variant 2:
  Detects keypoints in an image (first variant) or image set (second variant).

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **image**: `Evision.Mat`.

    Image.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying where to look for keypoints (optional). It must be a 8-bit integer
    matrix with non-zero values in the region of interest.

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`.

    The detected keypoints. In the second variant of the method keypoints[i] is a set
    of keypoints detected in images[i] .

  Python prototype (for reference only):
  ```python3
  detect(image[, mask]) -> keypoints
  ```

  """
  @spec detect(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), list(Evision.Mat.maybe_mat_in())) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images) when is_list(images)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **image**: `Evision.Mat`
  - **mask**: `Evision.Mat`

  ##### Keyword Arguments
  - **useProvidedKeypoints**: `bool`.

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`
  - **descriptors**: `Evision.Mat.t()`.

  Detects keypoints and computes the descriptors

  Python prototype (for reference only):
  ```python3
  detectAndCompute(image, mask[, descriptors[, useProvidedKeypoints]]) -> keypoints, descriptors
  ```
  """
  @spec detectAndCompute(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:useProvidedKeypoints, term()}] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:useProvidedKeypoints])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **image**: `Evision.Mat`
  - **mask**: `Evision.Mat`

  ##### Keyword Arguments
  - **useProvidedKeypoints**: `bool`.

  ##### Return
  - **keypoints**: `[Evision.KeyPoint]`
  - **descriptors**: `Evision.Mat.t()`.

  Detects keypoints and computes the descriptors

  Python prototype (for reference only):
  ```python3
  detectAndCompute(image, mask[, descriptors[, useProvidedKeypoints]]) -> keypoints, descriptors
  ```
  """
  @spec detectAndCompute(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCornThresh

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getCornThresh() -> retval
  ```
  """
  @spec getCornThresh(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()) :: number() | {:error, String.t()}
  def getCornThresh(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_HarrisLaplaceFeatureDetector_getCornThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDOGThresh

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getDOGThresh() -> retval
  ```
  """
  @spec getDOGThresh(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()) :: number() | {:error, String.t()}
  def getDOGThresh(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_HarrisLaplaceFeatureDetector_getDOGThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_HarrisLaplaceFeatureDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxCorners

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxCorners() -> retval
  ```
  """
  @spec getMaxCorners(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()) :: integer() | {:error, String.t()}
  def getMaxCorners(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_HarrisLaplaceFeatureDetector_getMaxCorners(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNumLayers

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumLayers() -> retval
  ```
  """
  @spec getNumLayers(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()) :: integer() | {:error, String.t()}
  def getNumLayers(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_HarrisLaplaceFeatureDetector_getNumLayers(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNumOctaves

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNumOctaves() -> retval
  ```
  """
  @spec getNumOctaves(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()) :: integer() | {:error, String.t()}
  def getNumOctaves(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_HarrisLaplaceFeatureDetector_getNumOctaves(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  read

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **arg1**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(arg1) -> None
  ```
  #### Variant 2:
  read

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  read(fileName) -> None
  ```

  """
  @spec read(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), Evision.FileNode.t()) :: Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t() | {:error, String.t()}
  def read(self, arg1) when is_struct(arg1, Evision.FileNode)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec read(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), binary()) :: Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t() | {:error, String.t()}
  def read(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCornThresh

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **corn_thresh_**: `float`

  Python prototype (for reference only):
  ```python3
  setCornThresh(corn_thresh_) -> None
  ```
  """
  @spec setCornThresh(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), number()) :: Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t() | {:error, String.t()}
  def setCornThresh(self, corn_thresh_) when is_float(corn_thresh_)
  do
    positional = [
      corn_thresh_: Evision.Internal.Structurise.from_struct(corn_thresh_)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_HarrisLaplaceFeatureDetector_setCornThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDOGThresh

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **dOG_thresh_**: `float`

  Python prototype (for reference only):
  ```python3
  setDOGThresh(DOG_thresh_) -> None
  ```
  """
  @spec setDOGThresh(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), number()) :: Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t() | {:error, String.t()}
  def setDOGThresh(self, dOG_thresh_) when is_float(dOG_thresh_)
  do
    positional = [
      dOG_thresh_: Evision.Internal.Structurise.from_struct(dOG_thresh_)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_HarrisLaplaceFeatureDetector_setDOGThresh(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxCorners

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **maxCorners_**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxCorners(maxCorners_) -> None
  ```
  """
  @spec setMaxCorners(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), integer()) :: Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t() | {:error, String.t()}
  def setMaxCorners(self, maxCorners_) when is_integer(maxCorners_)
  do
    positional = [
      maxCorners_: Evision.Internal.Structurise.from_struct(maxCorners_)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_HarrisLaplaceFeatureDetector_setMaxCorners(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNumLayers

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **num_layers_**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNumLayers(num_layers_) -> None
  ```
  """
  @spec setNumLayers(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), integer()) :: Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t() | {:error, String.t()}
  def setNumLayers(self, num_layers_) when is_integer(num_layers_)
  do
    positional = [
      num_layers_: Evision.Internal.Structurise.from_struct(num_layers_)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_HarrisLaplaceFeatureDetector_setNumLayers(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNumOctaves

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **numOctaves_**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNumOctaves(numOctaves_) -> None
  ```
  """
  @spec setNumOctaves(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), integer()) :: Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t() | {:error, String.t()}
  def setNumOctaves(self, numOctaves_) when is_integer(numOctaves_)
  do
    positional = [
      numOctaves_: Evision.Internal.Structurise.from_struct(numOctaves_)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_HarrisLaplaceFeatureDetector_setNumOctaves(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  write(fileName) -> None
  ```
  """
  @spec write(Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t(), binary()) :: Evision.XFeatures2D.HarrisLaplaceFeatureDetector.t() | {:error, String.t()}
  def write(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.xfeatures2d_HarrisLaplaceFeatureDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
