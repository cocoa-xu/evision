defmodule Evision.BRISK do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `BRISK` struct.

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
  def to_struct({:ok, %{class: Evision.BRISK, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.BRISK, ref: ref}) do
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
  - **self**: `Evision.BRISK.t()`
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
  - **self**: `Evision.BRISK.t()`
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
  @spec compute(Evision.BRISK.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t())), [{atom(), term()},...] | nil) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints, opts) when is_list(images) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.brisk_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.BRISK.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t()), [{atom(), term()},...] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.brisk_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`
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
  - **self**: `Evision.BRISK.t()`
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
  @spec compute(Evision.BRISK.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t()))) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints) when is_list(images) and is_list(keypoints)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.brisk_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.BRISK.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t())) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.brisk_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  The BRISK constructor for a custom pattern, detection threshold and octaves

  ##### Positional Arguments
  - **thresh**: `integer()`.

    AGAST detection threshold score.

  - **octaves**: `integer()`.

    detection octaves. Use 0 to do single scale.

  - **radiusList**: `[float]`.

    defines the radii (in pixels) where the samples around a keypoint are taken (for
    keypoint scale 1).

  - **numberList**: `[integer()]`.

    defines the number of sampling points on the sampling circle. Must be the same
    size as radiusList..

  ##### Keyword Arguments
  - **dMax**: `float`.

    threshold for the short pairings used for descriptor formation (in pixels for keypoint
    scale 1).

  - **dMin**: `float`.

    threshold for the long pairings used for orientation determination (in pixels for
    keypoint scale 1).

  - **indexChange**: `[integer()]`.

    index remapping of the bits.

  ##### Return
  - **retval**: `Evision.BRISK.t()`

  Python prototype (for reference only):
  ```python3
  create(thresh, octaves, radiusList, numberList[, dMax[, dMin[, indexChange]]]) -> retval
  ```
  """
  @spec create(integer(), integer(), list(number()), list(integer()), [{:dMax, term()} | {:dMin, term()} | {:indexChange, term()}] | nil) :: Evision.BRISK.t() | {:error, String.t()}
  def create(thresh, octaves, radiusList, numberList, opts) when is_integer(thresh) and is_integer(octaves) and is_list(radiusList) and is_list(numberList) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dMax, :dMin, :indexChange])
    positional = [
      thresh: Evision.Internal.Structurise.from_struct(thresh),
      octaves: Evision.Internal.Structurise.from_struct(octaves),
      radiusList: Evision.Internal.Structurise.from_struct(radiusList),
      numberList: Evision.Internal.Structurise.from_struct(numberList)
    ]
    :evision_nif.brisk_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  The BRISK constructor for a custom pattern, detection threshold and octaves

  ##### Positional Arguments
  - **thresh**: `integer()`.

    AGAST detection threshold score.

  - **octaves**: `integer()`.

    detection octaves. Use 0 to do single scale.

  - **radiusList**: `[float]`.

    defines the radii (in pixels) where the samples around a keypoint are taken (for
    keypoint scale 1).

  - **numberList**: `[integer()]`.

    defines the number of sampling points on the sampling circle. Must be the same
    size as radiusList..

  ##### Keyword Arguments
  - **dMax**: `float`.

    threshold for the short pairings used for descriptor formation (in pixels for keypoint
    scale 1).

  - **dMin**: `float`.

    threshold for the long pairings used for orientation determination (in pixels for
    keypoint scale 1).

  - **indexChange**: `[integer()]`.

    index remapping of the bits.

  ##### Return
  - **retval**: `Evision.BRISK.t()`

  Python prototype (for reference only):
  ```python3
  create(thresh, octaves, radiusList, numberList[, dMax[, dMin[, indexChange]]]) -> retval
  ```
  """
  @spec create(integer(), integer(), list(number()), list(integer())) :: Evision.BRISK.t() | {:error, String.t()}
  def create(thresh, octaves, radiusList, numberList) when is_integer(thresh) and is_integer(octaves) and is_list(radiusList) and is_list(numberList)
  do
    positional = [
      thresh: Evision.Internal.Structurise.from_struct(thresh),
      octaves: Evision.Internal.Structurise.from_struct(octaves),
      radiusList: Evision.Internal.Structurise.from_struct(radiusList),
      numberList: Evision.Internal.Structurise.from_struct(numberList)
    ]
    :evision_nif.brisk_create_static(positional)
    |> to_struct()
  end

  @doc """
  The BRISK constructor for a custom pattern

  ##### Positional Arguments
  - **radiusList**: `[float]`.

    defines the radii (in pixels) where the samples around a keypoint are taken (for
    keypoint scale 1).

  - **numberList**: `[integer()]`.

    defines the number of sampling points on the sampling circle. Must be the same
    size as radiusList..

  ##### Keyword Arguments
  - **dMax**: `float`.

    threshold for the short pairings used for descriptor formation (in pixels for keypoint
    scale 1).

  - **dMin**: `float`.

    threshold for the long pairings used for orientation determination (in pixels for
    keypoint scale 1).

  - **indexChange**: `[integer()]`.

    index remapping of the bits.

  ##### Return
  - **retval**: `Evision.BRISK.t()`

  Python prototype (for reference only):
  ```python3
  create(radiusList, numberList[, dMax[, dMin[, indexChange]]]) -> retval
  ```
  """
  @spec create(list(number()), list(integer()), [{:dMax, term()} | {:dMin, term()} | {:indexChange, term()}] | nil) :: Evision.BRISK.t() | {:error, String.t()}
  def create(radiusList, numberList, opts) when is_list(radiusList) and is_list(numberList) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dMax, :dMin, :indexChange])
    positional = [
      radiusList: Evision.Internal.Structurise.from_struct(radiusList),
      numberList: Evision.Internal.Structurise.from_struct(numberList)
    ]
    :evision_nif.brisk_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  The BRISK constructor for a custom pattern

  ##### Positional Arguments
  - **radiusList**: `[float]`.

    defines the radii (in pixels) where the samples around a keypoint are taken (for
    keypoint scale 1).

  - **numberList**: `[integer()]`.

    defines the number of sampling points on the sampling circle. Must be the same
    size as radiusList..

  ##### Keyword Arguments
  - **dMax**: `float`.

    threshold for the short pairings used for descriptor formation (in pixels for keypoint
    scale 1).

  - **dMin**: `float`.

    threshold for the long pairings used for orientation determination (in pixels for
    keypoint scale 1).

  - **indexChange**: `[integer()]`.

    index remapping of the bits.

  ##### Return
  - **retval**: `Evision.BRISK.t()`

  Python prototype (for reference only):
  ```python3
  create(radiusList, numberList[, dMax[, dMin[, indexChange]]]) -> retval
  ```
  """
  @spec create(list(number()), list(integer())) :: Evision.BRISK.t() | {:error, String.t()}
  def create(radiusList, numberList) when is_list(radiusList) and is_list(numberList)
  do
    positional = [
      radiusList: Evision.Internal.Structurise.from_struct(radiusList),
      numberList: Evision.Internal.Structurise.from_struct(numberList)
    ]
    :evision_nif.brisk_create_static(positional)
    |> to_struct()
  end

  @doc """
  The BRISK constructor
  ##### Keyword Arguments
  - **thresh**: `integer()`.

    AGAST detection threshold score.

  - **octaves**: `integer()`.

    detection octaves. Use 0 to do single scale.

  - **patternScale**: `float`.

    apply this scale to the pattern used for sampling the neighbourhood of a
    keypoint.

  ##### Return
  - **retval**: `Evision.BRISK.t()`

  Python prototype (for reference only):
  ```python3
  create([, thresh[, octaves[, patternScale]]]) -> retval
  ```
  """
  @spec create([{:octaves, term()} | {:patternScale, term()} | {:thresh, term()}] | nil) :: Evision.BRISK.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:octaves, :patternScale, :thresh])
    positional = [
    ]
    :evision_nif.brisk_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  The BRISK constructor
  ##### Keyword Arguments
  - **thresh**: `integer()`.

    AGAST detection threshold score.

  - **octaves**: `integer()`.

    detection octaves. Use 0 to do single scale.

  - **patternScale**: `float`.

    apply this scale to the pattern used for sampling the neighbourhood of a
    keypoint.

  ##### Return
  - **retval**: `Evision.BRISK.t()`

  Python prototype (for reference only):
  ```python3
  create([, thresh[, octaves[, patternScale]]]) -> retval
  ```
  """
  @spec create() :: Evision.BRISK.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.brisk_create_static(positional)
    |> to_struct()
  end

  @doc """
  defaultNorm

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  defaultNorm() -> retval
  ```
  """
  @spec defaultNorm(Evision.BRISK.t()) :: integer() | {:error, String.t()}
  def defaultNorm(self) do
    positional = [
    ]
    :evision_nif.brisk_defaultNorm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorSize

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorSize() -> retval
  ```
  """
  @spec descriptorSize(Evision.BRISK.t()) :: integer() | {:error, String.t()}
  def descriptorSize(self) do
    positional = [
    ]
    :evision_nif.brisk_descriptorSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorType

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorType() -> retval
  ```
  """
  @spec descriptorType(Evision.BRISK.t()) :: integer() | {:error, String.t()}
  def descriptorType(self) do
    positional = [
    ]
    :evision_nif.brisk_descriptorType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`
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
  - **self**: `Evision.BRISK.t()`
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
  @spec detect(Evision.BRISK.t(), list(Evision.Mat.maybe_mat_in()), [{:masks, term()}] | nil) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images, opts) when is_list(images) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.brisk_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.BRISK.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.brisk_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`
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
  - **self**: `Evision.BRISK.t()`
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
  @spec detect(Evision.BRISK.t(), list(Evision.Mat.maybe_mat_in())) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images) when is_list(images)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.brisk_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.BRISK.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.brisk_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`
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
  @spec detectAndCompute(Evision.BRISK.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:useProvidedKeypoints, term()}] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:useProvidedKeypoints])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.brisk_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`
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
  @spec detectAndCompute(Evision.BRISK.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.brisk_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.BRISK.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.brisk_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.BRISK.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.brisk_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getOctaves

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getOctaves() -> retval
  ```
  """
  @spec getOctaves(Evision.BRISK.t()) :: integer() | {:error, String.t()}
  def getOctaves(self) do
    positional = [
    ]
    :evision_nif.brisk_getOctaves(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getPatternScale

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getPatternScale() -> retval
  ```
  """
  @spec getPatternScale(Evision.BRISK.t()) :: number() | {:error, String.t()}
  def getPatternScale(self) do
    positional = [
    ]
    :evision_nif.brisk_getPatternScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getThreshold

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getThreshold() -> retval
  ```
  """
  @spec getThreshold(Evision.BRISK.t()) :: integer() | {:error, String.t()}
  def getThreshold(self) do
    positional = [
    ]
    :evision_nif.brisk_getThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  read

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`
  - **arg1**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(arg1) -> None
  ```
  #### Variant 2:
  read

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  read(fileName) -> None
  ```

  """
  @spec read(Evision.BRISK.t(), Evision.FileNode.t()) :: Evision.BRISK.t() | {:error, String.t()}
  def read(self, arg1) when is_struct(arg1, Evision.FileNode)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1)
    ]
    :evision_nif.brisk_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec read(Evision.BRISK.t(), binary()) :: Evision.BRISK.t() | {:error, String.t()}
  def read(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.brisk_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set detection octaves.

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`
  - **octaves**: `integer()`.

    detection octaves. Use 0 to do single scale.

  Python prototype (for reference only):
  ```python3
  setOctaves(octaves) -> None
  ```
  """
  @spec setOctaves(Evision.BRISK.t(), integer()) :: Evision.BRISK.t() | {:error, String.t()}
  def setOctaves(self, octaves) when is_integer(octaves)
  do
    positional = [
      octaves: Evision.Internal.Structurise.from_struct(octaves)
    ]
    :evision_nif.brisk_setOctaves(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set detection patternScale.

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`
  - **patternScale**: `float`.

    apply this scale to the pattern used for sampling the neighbourhood of a
    keypoint.

  Python prototype (for reference only):
  ```python3
  setPatternScale(patternScale) -> None
  ```
  """
  @spec setPatternScale(Evision.BRISK.t(), number()) :: Evision.BRISK.t() | {:error, String.t()}
  def setPatternScale(self, patternScale) when is_float(patternScale)
  do
    positional = [
      patternScale: Evision.Internal.Structurise.from_struct(patternScale)
    ]
    :evision_nif.brisk_setPatternScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set detection threshold.

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`
  - **threshold**: `integer()`.

    AGAST detection threshold score.

  Python prototype (for reference only):
  ```python3
  setThreshold(threshold) -> None
  ```
  """
  @spec setThreshold(Evision.BRISK.t(), integer()) :: Evision.BRISK.t() | {:error, String.t()}
  def setThreshold(self, threshold) when is_integer(threshold)
  do
    positional = [
      threshold: Evision.Internal.Structurise.from_struct(threshold)
    ]
    :evision_nif.brisk_setThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.BRISK.t(), Evision.FileStorage.t(), binary()) :: Evision.BRISK.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.brisk_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.BRISK.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  write(fileName) -> None
  ```
  """
  @spec write(Evision.BRISK.t(), binary()) :: Evision.BRISK.t() | {:error, String.t()}
  def write(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.brisk_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
