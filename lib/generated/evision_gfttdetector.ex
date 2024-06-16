defmodule Evision.GFTTDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `GFTTDetector` struct.

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
  def to_struct({:ok, %{class: Evision.GFTTDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.GFTTDetector, ref: ref}) do
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
  - **self**: `Evision.GFTTDetector.t()`
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
  - **self**: `Evision.GFTTDetector.t()`
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
  @spec compute(Evision.GFTTDetector.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t())), [{atom(), term()},...] | nil) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints, opts) when is_list(images) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.gfttDetector_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.GFTTDetector.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t()), [{atom(), term()},...] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.gfttDetector_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
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
  - **self**: `Evision.GFTTDetector.t()`
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
  @spec compute(Evision.GFTTDetector.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t()))) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints) when is_list(images) and is_list(keypoints)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.gfttDetector_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.GFTTDetector.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t())) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.gfttDetector_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **maxCorners**: `integer()`
  - **qualityLevel**: `double`
  - **minDistance**: `double`
  - **blockSize**: `integer()`
  - **gradiantSize**: `integer()`

  ##### Keyword Arguments
  - **useHarrisDetector**: `bool`.
  - **k**: `double`.

  ##### Return
  - **retval**: `Evision.GFTTDetector.t()`

  Python prototype (for reference only):
  ```python3
  create(maxCorners, qualityLevel, minDistance, blockSize, gradiantSize[, useHarrisDetector[, k]]) -> retval
  ```
  """
  @spec create(integer(), number(), number(), integer(), integer(), [{:k, term()} | {:useHarrisDetector, term()}] | nil) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def create(maxCorners, qualityLevel, minDistance, blockSize, gradiantSize, opts) when is_integer(maxCorners) and is_number(qualityLevel) and is_number(minDistance) and is_integer(blockSize) and is_integer(gradiantSize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:k, :useHarrisDetector])
    positional = [
      maxCorners: Evision.Internal.Structurise.from_struct(maxCorners),
      qualityLevel: Evision.Internal.Structurise.from_struct(qualityLevel),
      minDistance: Evision.Internal.Structurise.from_struct(minDistance),
      blockSize: Evision.Internal.Structurise.from_struct(blockSize),
      gradiantSize: Evision.Internal.Structurise.from_struct(gradiantSize)
    ]
    :evision_nif.gfttDetector_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **maxCorners**: `integer()`
  - **qualityLevel**: `double`
  - **minDistance**: `double`
  - **blockSize**: `integer()`
  - **gradiantSize**: `integer()`

  ##### Keyword Arguments
  - **useHarrisDetector**: `bool`.
  - **k**: `double`.

  ##### Return
  - **retval**: `Evision.GFTTDetector.t()`

  Python prototype (for reference only):
  ```python3
  create(maxCorners, qualityLevel, minDistance, blockSize, gradiantSize[, useHarrisDetector[, k]]) -> retval
  ```
  """
  @spec create(integer(), number(), number(), integer(), integer()) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def create(maxCorners, qualityLevel, minDistance, blockSize, gradiantSize) when is_integer(maxCorners) and is_number(qualityLevel) and is_number(minDistance) and is_integer(blockSize) and is_integer(gradiantSize)
  do
    positional = [
      maxCorners: Evision.Internal.Structurise.from_struct(maxCorners),
      qualityLevel: Evision.Internal.Structurise.from_struct(qualityLevel),
      minDistance: Evision.Internal.Structurise.from_struct(minDistance),
      blockSize: Evision.Internal.Structurise.from_struct(blockSize),
      gradiantSize: Evision.Internal.Structurise.from_struct(gradiantSize)
    ]
    :evision_nif.gfttDetector_create_static(positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **maxCorners**: `integer()`.
  - **qualityLevel**: `double`.
  - **minDistance**: `double`.
  - **blockSize**: `integer()`.
  - **useHarrisDetector**: `bool`.
  - **k**: `double`.

  ##### Return
  - **retval**: `Evision.GFTTDetector.t()`

  Python prototype (for reference only):
  ```python3
  create([, maxCorners[, qualityLevel[, minDistance[, blockSize[, useHarrisDetector[, k]]]]]]) -> retval
  ```
  """
  @spec create([{:blockSize, term()} | {:k, term()} | {:maxCorners, term()} | {:minDistance, term()} | {:qualityLevel, term()} | {:useHarrisDetector, term()}] | nil) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:blockSize, :k, :maxCorners, :minDistance, :qualityLevel, :useHarrisDetector])
    positional = [
    ]
    :evision_nif.gfttDetector_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **maxCorners**: `integer()`.
  - **qualityLevel**: `double`.
  - **minDistance**: `double`.
  - **blockSize**: `integer()`.
  - **useHarrisDetector**: `bool`.
  - **k**: `double`.

  ##### Return
  - **retval**: `Evision.GFTTDetector.t()`

  Python prototype (for reference only):
  ```python3
  create([, maxCorners[, qualityLevel[, minDistance[, blockSize[, useHarrisDetector[, k]]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.GFTTDetector.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.gfttDetector_create_static(positional)
    |> to_struct()
  end

  @doc """
  defaultNorm

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  defaultNorm() -> retval
  ```
  """
  @spec defaultNorm(Evision.GFTTDetector.t()) :: integer() | {:error, String.t()}
  def defaultNorm(self) do
    positional = [
    ]
    :evision_nif.gfttDetector_defaultNorm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorSize

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorSize() -> retval
  ```
  """
  @spec descriptorSize(Evision.GFTTDetector.t()) :: integer() | {:error, String.t()}
  def descriptorSize(self) do
    positional = [
    ]
    :evision_nif.gfttDetector_descriptorSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorType

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorType() -> retval
  ```
  """
  @spec descriptorType(Evision.GFTTDetector.t()) :: integer() | {:error, String.t()}
  def descriptorType(self) do
    positional = [
    ]
    :evision_nif.gfttDetector_descriptorType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
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
  - **self**: `Evision.GFTTDetector.t()`
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
  @spec detect(Evision.GFTTDetector.t(), list(Evision.Mat.maybe_mat_in()), [{:masks, term()}] | nil) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images, opts) when is_list(images) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.gfttDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.GFTTDetector.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.gfttDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
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
  - **self**: `Evision.GFTTDetector.t()`
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
  @spec detect(Evision.GFTTDetector.t(), list(Evision.Mat.maybe_mat_in())) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images) when is_list(images)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.gfttDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.GFTTDetector.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.gfttDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
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
  @spec detectAndCompute(Evision.GFTTDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:useProvidedKeypoints, term()}] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:useProvidedKeypoints])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.gfttDetector_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
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
  @spec detectAndCompute(Evision.GFTTDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.gfttDetector_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.GFTTDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.gfttDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBlockSize

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getBlockSize() -> retval
  ```
  """
  @spec getBlockSize(Evision.GFTTDetector.t()) :: integer() | {:error, String.t()}
  def getBlockSize(self) do
    positional = [
    ]
    :evision_nif.gfttDetector_getBlockSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.GFTTDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.gfttDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getGradientSize

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getGradientSize() -> retval
  ```
  """
  @spec getGradientSize(Evision.GFTTDetector.t()) :: integer() | {:error, String.t()}
  def getGradientSize(self) do
    positional = [
    ]
    :evision_nif.gfttDetector_getGradientSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getHarrisDetector

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getHarrisDetector() -> retval
  ```
  """
  @spec getHarrisDetector(Evision.GFTTDetector.t()) :: boolean() | {:error, String.t()}
  def getHarrisDetector(self) do
    positional = [
    ]
    :evision_nif.gfttDetector_getHarrisDetector(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getK

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getK() -> retval
  ```
  """
  @spec getK(Evision.GFTTDetector.t()) :: number() | {:error, String.t()}
  def getK(self) do
    positional = [
    ]
    :evision_nif.gfttDetector_getK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxFeatures

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxFeatures() -> retval
  ```
  """
  @spec getMaxFeatures(Evision.GFTTDetector.t()) :: integer() | {:error, String.t()}
  def getMaxFeatures(self) do
    positional = [
    ]
    :evision_nif.gfttDetector_getMaxFeatures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinDistance

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getMinDistance() -> retval
  ```
  """
  @spec getMinDistance(Evision.GFTTDetector.t()) :: number() | {:error, String.t()}
  def getMinDistance(self) do
    positional = [
    ]
    :evision_nif.gfttDetector_getMinDistance(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getQualityLevel

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getQualityLevel() -> retval
  ```
  """
  @spec getQualityLevel(Evision.GFTTDetector.t()) :: number() | {:error, String.t()}
  def getQualityLevel(self) do
    positional = [
    ]
    :evision_nif.gfttDetector_getQualityLevel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  read

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
  - **arg1**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(arg1) -> None
  ```
  #### Variant 2:
  read

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  read(fileName) -> None
  ```

  """
  @spec read(Evision.GFTTDetector.t(), Evision.FileNode.t()) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def read(self, arg1) when is_struct(arg1, Evision.FileNode)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1)
    ]
    :evision_nif.gfttDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec read(Evision.GFTTDetector.t(), binary()) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def read(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.gfttDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBlockSize

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
  - **blockSize**: `integer()`

  Python prototype (for reference only):
  ```python3
  setBlockSize(blockSize) -> None
  ```
  """
  @spec setBlockSize(Evision.GFTTDetector.t(), integer()) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def setBlockSize(self, blockSize) when is_integer(blockSize)
  do
    positional = [
      blockSize: Evision.Internal.Structurise.from_struct(blockSize)
    ]
    :evision_nif.gfttDetector_setBlockSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setGradientSize

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
  - **gradientSize_**: `integer()`

  Python prototype (for reference only):
  ```python3
  setGradientSize(gradientSize_) -> None
  ```
  """
  @spec setGradientSize(Evision.GFTTDetector.t(), integer()) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def setGradientSize(self, gradientSize_) when is_integer(gradientSize_)
  do
    positional = [
      gradientSize_: Evision.Internal.Structurise.from_struct(gradientSize_)
    ]
    :evision_nif.gfttDetector_setGradientSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setHarrisDetector

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
  - **val**: `bool`

  Python prototype (for reference only):
  ```python3
  setHarrisDetector(val) -> None
  ```
  """
  @spec setHarrisDetector(Evision.GFTTDetector.t(), boolean()) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def setHarrisDetector(self, val) when is_boolean(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.gfttDetector_setHarrisDetector(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setK

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
  - **k**: `double`

  Python prototype (for reference only):
  ```python3
  setK(k) -> None
  ```
  """
  @spec setK(Evision.GFTTDetector.t(), number()) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def setK(self, k) when is_number(k)
  do
    positional = [
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.gfttDetector_setK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxFeatures

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
  - **maxFeatures**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxFeatures(maxFeatures) -> None
  ```
  """
  @spec setMaxFeatures(Evision.GFTTDetector.t(), integer()) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def setMaxFeatures(self, maxFeatures) when is_integer(maxFeatures)
  do
    positional = [
      maxFeatures: Evision.Internal.Structurise.from_struct(maxFeatures)
    ]
    :evision_nif.gfttDetector_setMaxFeatures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinDistance

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
  - **minDistance**: `double`

  Python prototype (for reference only):
  ```python3
  setMinDistance(minDistance) -> None
  ```
  """
  @spec setMinDistance(Evision.GFTTDetector.t(), number()) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def setMinDistance(self, minDistance) when is_number(minDistance)
  do
    positional = [
      minDistance: Evision.Internal.Structurise.from_struct(minDistance)
    ]
    :evision_nif.gfttDetector_setMinDistance(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setQualityLevel

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
  - **qlevel**: `double`

  Python prototype (for reference only):
  ```python3
  setQualityLevel(qlevel) -> None
  ```
  """
  @spec setQualityLevel(Evision.GFTTDetector.t(), number()) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def setQualityLevel(self, qlevel) when is_number(qlevel)
  do
    positional = [
      qlevel: Evision.Internal.Structurise.from_struct(qlevel)
    ]
    :evision_nif.gfttDetector_setQualityLevel(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.GFTTDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.gfttDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.GFTTDetector.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  write(fileName) -> None
  ```
  """
  @spec write(Evision.GFTTDetector.t(), binary()) :: Evision.GFTTDetector.t() | {:error, String.t()}
  def write(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.gfttDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
