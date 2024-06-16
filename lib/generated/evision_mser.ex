defmodule Evision.MSER do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `MSER` struct.

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
  def to_struct({:ok, %{class: Evision.MSER, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.MSER, ref: ref}) do
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
  - **self**: `Evision.MSER.t()`
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
  - **self**: `Evision.MSER.t()`
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
  @spec compute(Evision.MSER.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t())), [{atom(), term()},...] | nil) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints, opts) when is_list(images) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.mser_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.MSER.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t()), [{atom(), term()},...] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.mser_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
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
  - **self**: `Evision.MSER.t()`
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
  @spec compute(Evision.MSER.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t()))) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints) when is_list(images) and is_list(keypoints)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.mser_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.MSER.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t())) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.mser_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Full constructor for %MSER detector
  ##### Keyword Arguments
  - **delta**: `integer()`.

    it compares \\f$(size_{i}-size_{i-delta})/size_{i-delta}\\f$

  - **min_area**: `integer()`.

    prune the area which smaller than minArea

  - **max_area**: `integer()`.

    prune the area which bigger than maxArea

  - **max_variation**: `double`.

    prune the area have similar size to its children

  - **min_diversity**: `double`.

    for color image, trace back to cut off mser with diversity less than min_diversity

  - **max_evolution**: `integer()`.

    for color image, the evolution steps

  - **area_threshold**: `double`.

    for color image, the area threshold to cause re-initialize

  - **min_margin**: `double`.

    for color image, ignore too small margin

  - **edge_blur_size**: `integer()`.

    for color image, the aperture size for edge blur

  ##### Return
  - **retval**: `Evision.MSER.t()`

  Python prototype (for reference only):
  ```python3
  create([, delta[, min_area[, max_area[, max_variation[, min_diversity[, max_evolution[, area_threshold[, min_margin[, edge_blur_size]]]]]]]]]) -> retval
  ```
  """
  @spec create([{:area_threshold, term()} | {:delta, term()} | {:edge_blur_size, term()} | {:max_area, term()} | {:max_evolution, term()} | {:max_variation, term()} | {:min_area, term()} | {:min_diversity, term()} | {:min_margin, term()}] | nil) :: Evision.MSER.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:area_threshold, :delta, :edge_blur_size, :max_area, :max_evolution, :max_variation, :min_area, :min_diversity, :min_margin])
    positional = [
    ]
    :evision_nif.mser_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Full constructor for %MSER detector
  ##### Keyword Arguments
  - **delta**: `integer()`.

    it compares \\f$(size_{i}-size_{i-delta})/size_{i-delta}\\f$

  - **min_area**: `integer()`.

    prune the area which smaller than minArea

  - **max_area**: `integer()`.

    prune the area which bigger than maxArea

  - **max_variation**: `double`.

    prune the area have similar size to its children

  - **min_diversity**: `double`.

    for color image, trace back to cut off mser with diversity less than min_diversity

  - **max_evolution**: `integer()`.

    for color image, the evolution steps

  - **area_threshold**: `double`.

    for color image, the area threshold to cause re-initialize

  - **min_margin**: `double`.

    for color image, ignore too small margin

  - **edge_blur_size**: `integer()`.

    for color image, the aperture size for edge blur

  ##### Return
  - **retval**: `Evision.MSER.t()`

  Python prototype (for reference only):
  ```python3
  create([, delta[, min_area[, max_area[, max_variation[, min_diversity[, max_evolution[, area_threshold[, min_margin[, edge_blur_size]]]]]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.MSER.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.mser_create_static(positional)
    |> to_struct()
  end

  @doc """
  defaultNorm

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  defaultNorm() -> retval
  ```
  """
  @spec defaultNorm(Evision.MSER.t()) :: integer() | {:error, String.t()}
  def defaultNorm(self) do
    positional = [
    ]
    :evision_nif.mser_defaultNorm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorSize

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorSize() -> retval
  ```
  """
  @spec descriptorSize(Evision.MSER.t()) :: integer() | {:error, String.t()}
  def descriptorSize(self) do
    positional = [
    ]
    :evision_nif.mser_descriptorSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorType

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorType() -> retval
  ```
  """
  @spec descriptorType(Evision.MSER.t()) :: integer() | {:error, String.t()}
  def descriptorType(self) do
    positional = [
    ]
    :evision_nif.mser_descriptorType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
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
  - **self**: `Evision.MSER.t()`
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
  @spec detect(Evision.MSER.t(), list(Evision.Mat.maybe_mat_in()), [{:masks, term()}] | nil) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images, opts) when is_list(images) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.mser_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.MSER.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.mser_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
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
  - **self**: `Evision.MSER.t()`
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
  @spec detect(Evision.MSER.t(), list(Evision.Mat.maybe_mat_in())) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images) when is_list(images)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.mser_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.MSER.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.mser_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
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
  @spec detectAndCompute(Evision.MSER.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:useProvidedKeypoints, term()}] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:useProvidedKeypoints])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.mser_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
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
  @spec detectAndCompute(Evision.MSER.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.mser_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Detect %MSER regions

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **image**: `Evision.Mat`.

    input image (8UC1, 8UC3 or 8UC4, must be greater or equal than 3x3)

  ##### Return
  - **msers**: `[[Point]]`.

    resulting list of point sets

  - **bboxes**: `[Rect]`.

    resulting bounding boxes

  Python prototype (for reference only):
  ```python3
  detectRegions(image) -> msers, bboxes
  ```
  """
  @spec detectRegions(Evision.MSER.t(), Evision.Mat.maybe_mat_in()) :: {list(list({number(), number()})), list({number(), number(), number(), number()})} | {:error, String.t()}
  def detectRegions(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.mser_detectRegions(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.MSER.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.mser_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getAreaThreshold

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getAreaThreshold() -> retval
  ```
  """
  @spec getAreaThreshold(Evision.MSER.t()) :: number() | {:error, String.t()}
  def getAreaThreshold(self) do
    positional = [
    ]
    :evision_nif.mser_getAreaThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.MSER.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.mser_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDelta

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getDelta() -> retval
  ```
  """
  @spec getDelta(Evision.MSER.t()) :: integer() | {:error, String.t()}
  def getDelta(self) do
    positional = [
    ]
    :evision_nif.mser_getDelta(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getEdgeBlurSize

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getEdgeBlurSize() -> retval
  ```
  """
  @spec getEdgeBlurSize(Evision.MSER.t()) :: integer() | {:error, String.t()}
  def getEdgeBlurSize(self) do
    positional = [
    ]
    :evision_nif.mser_getEdgeBlurSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxArea

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxArea() -> retval
  ```
  """
  @spec getMaxArea(Evision.MSER.t()) :: integer() | {:error, String.t()}
  def getMaxArea(self) do
    positional = [
    ]
    :evision_nif.mser_getMaxArea(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxEvolution

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxEvolution() -> retval
  ```
  """
  @spec getMaxEvolution(Evision.MSER.t()) :: integer() | {:error, String.t()}
  def getMaxEvolution(self) do
    positional = [
    ]
    :evision_nif.mser_getMaxEvolution(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxVariation

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMaxVariation() -> retval
  ```
  """
  @spec getMaxVariation(Evision.MSER.t()) :: number() | {:error, String.t()}
  def getMaxVariation(self) do
    positional = [
    ]
    :evision_nif.mser_getMaxVariation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinArea

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMinArea() -> retval
  ```
  """
  @spec getMinArea(Evision.MSER.t()) :: integer() | {:error, String.t()}
  def getMinArea(self) do
    positional = [
    ]
    :evision_nif.mser_getMinArea(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinDiversity

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMinDiversity() -> retval
  ```
  """
  @spec getMinDiversity(Evision.MSER.t()) :: number() | {:error, String.t()}
  def getMinDiversity(self) do
    positional = [
    ]
    :evision_nif.mser_getMinDiversity(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinMargin

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMinMargin() -> retval
  ```
  """
  @spec getMinMargin(Evision.MSER.t()) :: number() | {:error, String.t()}
  def getMinMargin(self) do
    positional = [
    ]
    :evision_nif.mser_getMinMargin(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getPass2Only

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getPass2Only() -> retval
  ```
  """
  @spec getPass2Only(Evision.MSER.t()) :: boolean() | {:error, String.t()}
  def getPass2Only(self) do
    positional = [
    ]
    :evision_nif.mser_getPass2Only(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  read

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **arg1**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(arg1) -> None
  ```
  #### Variant 2:
  read

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  read(fileName) -> None
  ```

  """
  @spec read(Evision.MSER.t(), Evision.FileNode.t()) :: Evision.MSER.t() | {:error, String.t()}
  def read(self, arg1) when is_struct(arg1, Evision.FileNode)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1)
    ]
    :evision_nif.mser_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec read(Evision.MSER.t(), binary()) :: Evision.MSER.t() | {:error, String.t()}
  def read(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.mser_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setAreaThreshold

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **areaThreshold**: `double`

  Python prototype (for reference only):
  ```python3
  setAreaThreshold(areaThreshold) -> None
  ```
  """
  @spec setAreaThreshold(Evision.MSER.t(), number()) :: Evision.MSER.t() | {:error, String.t()}
  def setAreaThreshold(self, areaThreshold) when is_number(areaThreshold)
  do
    positional = [
      areaThreshold: Evision.Internal.Structurise.from_struct(areaThreshold)
    ]
    :evision_nif.mser_setAreaThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDelta

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **delta**: `integer()`

  Python prototype (for reference only):
  ```python3
  setDelta(delta) -> None
  ```
  """
  @spec setDelta(Evision.MSER.t(), integer()) :: Evision.MSER.t() | {:error, String.t()}
  def setDelta(self, delta) when is_integer(delta)
  do
    positional = [
      delta: Evision.Internal.Structurise.from_struct(delta)
    ]
    :evision_nif.mser_setDelta(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setEdgeBlurSize

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **edge_blur_size**: `integer()`

  Python prototype (for reference only):
  ```python3
  setEdgeBlurSize(edge_blur_size) -> None
  ```
  """
  @spec setEdgeBlurSize(Evision.MSER.t(), integer()) :: Evision.MSER.t() | {:error, String.t()}
  def setEdgeBlurSize(self, edge_blur_size) when is_integer(edge_blur_size)
  do
    positional = [
      edge_blur_size: Evision.Internal.Structurise.from_struct(edge_blur_size)
    ]
    :evision_nif.mser_setEdgeBlurSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxArea

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **maxArea**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxArea(maxArea) -> None
  ```
  """
  @spec setMaxArea(Evision.MSER.t(), integer()) :: Evision.MSER.t() | {:error, String.t()}
  def setMaxArea(self, maxArea) when is_integer(maxArea)
  do
    positional = [
      maxArea: Evision.Internal.Structurise.from_struct(maxArea)
    ]
    :evision_nif.mser_setMaxArea(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxEvolution

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **maxEvolution**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxEvolution(maxEvolution) -> None
  ```
  """
  @spec setMaxEvolution(Evision.MSER.t(), integer()) :: Evision.MSER.t() | {:error, String.t()}
  def setMaxEvolution(self, maxEvolution) when is_integer(maxEvolution)
  do
    positional = [
      maxEvolution: Evision.Internal.Structurise.from_struct(maxEvolution)
    ]
    :evision_nif.mser_setMaxEvolution(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxVariation

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **maxVariation**: `double`

  Python prototype (for reference only):
  ```python3
  setMaxVariation(maxVariation) -> None
  ```
  """
  @spec setMaxVariation(Evision.MSER.t(), number()) :: Evision.MSER.t() | {:error, String.t()}
  def setMaxVariation(self, maxVariation) when is_number(maxVariation)
  do
    positional = [
      maxVariation: Evision.Internal.Structurise.from_struct(maxVariation)
    ]
    :evision_nif.mser_setMaxVariation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinArea

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **minArea**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMinArea(minArea) -> None
  ```
  """
  @spec setMinArea(Evision.MSER.t(), integer()) :: Evision.MSER.t() | {:error, String.t()}
  def setMinArea(self, minArea) when is_integer(minArea)
  do
    positional = [
      minArea: Evision.Internal.Structurise.from_struct(minArea)
    ]
    :evision_nif.mser_setMinArea(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinDiversity

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **minDiversity**: `double`

  Python prototype (for reference only):
  ```python3
  setMinDiversity(minDiversity) -> None
  ```
  """
  @spec setMinDiversity(Evision.MSER.t(), number()) :: Evision.MSER.t() | {:error, String.t()}
  def setMinDiversity(self, minDiversity) when is_number(minDiversity)
  do
    positional = [
      minDiversity: Evision.Internal.Structurise.from_struct(minDiversity)
    ]
    :evision_nif.mser_setMinDiversity(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinMargin

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **min_margin**: `double`

  Python prototype (for reference only):
  ```python3
  setMinMargin(min_margin) -> None
  ```
  """
  @spec setMinMargin(Evision.MSER.t(), number()) :: Evision.MSER.t() | {:error, String.t()}
  def setMinMargin(self, min_margin) when is_number(min_margin)
  do
    positional = [
      min_margin: Evision.Internal.Structurise.from_struct(min_margin)
    ]
    :evision_nif.mser_setMinMargin(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPass2Only

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **f**: `bool`

  Python prototype (for reference only):
  ```python3
  setPass2Only(f) -> None
  ```
  """
  @spec setPass2Only(Evision.MSER.t(), boolean()) :: Evision.MSER.t() | {:error, String.t()}
  def setPass2Only(self, f) when is_boolean(f)
  do
    positional = [
      f: Evision.Internal.Structurise.from_struct(f)
    ]
    :evision_nif.mser_setPass2Only(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.MSER.t(), Evision.FileStorage.t(), binary()) :: Evision.MSER.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.mser_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.MSER.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  write(fileName) -> None
  ```
  """
  @spec write(Evision.MSER.t(), binary()) :: Evision.MSER.t() | {:error, String.t()}
  def write(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.mser_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
