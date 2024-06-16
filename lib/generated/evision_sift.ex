defmodule Evision.SIFT do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `SIFT` struct.

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
  def to_struct({:ok, %{class: Evision.SIFT, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.SIFT, ref: ref}) do
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
  - **self**: `Evision.SIFT.t()`
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
  - **self**: `Evision.SIFT.t()`
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
  @spec compute(Evision.SIFT.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t())), [{atom(), term()},...] | nil) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints, opts) when is_list(images) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.sift_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.SIFT.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t()), [{atom(), term()},...] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.sift_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
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
  - **self**: `Evision.SIFT.t()`
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
  @spec compute(Evision.SIFT.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t()))) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints) when is_list(images) and is_list(keypoints)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.sift_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.SIFT.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t())) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.sift_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Create SIFT with specified descriptorType.

  ##### Positional Arguments
  - **nfeatures**: `integer()`.

    The number of best features to retain. The features are ranked by their scores
    (measured in SIFT algorithm as the local contrast)

  - **nOctaveLayers**: `integer()`.

    The number of layers in each octave. 3 is the value used in D. Lowe paper. The
    number of octaves is computed automatically from the image resolution.

  - **contrastThreshold**: `double`.

    The contrast threshold used to filter out weak features in semi-uniform
    (low-contrast) regions. The larger the threshold, the less features are produced by the detector.

  - **edgeThreshold**: `double`.

    The threshold used to filter out edge-like features. Note that the its meaning
    is different from the contrastThreshold, i.e. the larger the edgeThreshold, the less features are
    filtered out (more features are retained).

  - **sigma**: `double`.

    The sigma of the Gaussian applied to the input image at the octave \\#0. If your image
    is captured with a weak camera with soft lenses, you might want to reduce the number.

  - **descriptorType**: `integer()`.

    The type of descriptors. Only CV_32F and CV_8U are supported.

  ##### Keyword Arguments
  - **enable_precise_upscale**: `bool`.

    Whether to enable precise upscaling in the scale pyramid, which maps
    index \\f$\\texttt{x}\\f$ to \\f$\\texttt{2x}\\f$. This prevents localization bias. The option
    is disabled by default.

  ##### Return
  - **retval**: `Evision.SIFT.t()`

  **Note**: The contrast threshold will be divided by nOctaveLayers when the filtering is applied. When
  nOctaveLayers is set to default and if you want to use the value used in D. Lowe paper, 0.03, set
  this argument to 0.09.

  Python prototype (for reference only):
  ```python3
  create(nfeatures, nOctaveLayers, contrastThreshold, edgeThreshold, sigma, descriptorType[, enable_precise_upscale]) -> retval
  ```
  """
  @spec create(integer(), integer(), number(), number(), number(), integer(), [{:enable_precise_upscale, term()}] | nil) :: Evision.SIFT.t() | {:error, String.t()}
  def create(nfeatures, nOctaveLayers, contrastThreshold, edgeThreshold, sigma, descriptorType, opts) when is_integer(nfeatures) and is_integer(nOctaveLayers) and is_number(contrastThreshold) and is_number(edgeThreshold) and is_number(sigma) and is_integer(descriptorType) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:enable_precise_upscale])
    positional = [
      nfeatures: Evision.Internal.Structurise.from_struct(nfeatures),
      nOctaveLayers: Evision.Internal.Structurise.from_struct(nOctaveLayers),
      contrastThreshold: Evision.Internal.Structurise.from_struct(contrastThreshold),
      edgeThreshold: Evision.Internal.Structurise.from_struct(edgeThreshold),
      sigma: Evision.Internal.Structurise.from_struct(sigma),
      descriptorType: Evision.Internal.Structurise.from_struct(descriptorType)
    ]
    :evision_nif.sift_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Create SIFT with specified descriptorType.

  ##### Positional Arguments
  - **nfeatures**: `integer()`.

    The number of best features to retain. The features are ranked by their scores
    (measured in SIFT algorithm as the local contrast)

  - **nOctaveLayers**: `integer()`.

    The number of layers in each octave. 3 is the value used in D. Lowe paper. The
    number of octaves is computed automatically from the image resolution.

  - **contrastThreshold**: `double`.

    The contrast threshold used to filter out weak features in semi-uniform
    (low-contrast) regions. The larger the threshold, the less features are produced by the detector.

  - **edgeThreshold**: `double`.

    The threshold used to filter out edge-like features. Note that the its meaning
    is different from the contrastThreshold, i.e. the larger the edgeThreshold, the less features are
    filtered out (more features are retained).

  - **sigma**: `double`.

    The sigma of the Gaussian applied to the input image at the octave \\#0. If your image
    is captured with a weak camera with soft lenses, you might want to reduce the number.

  - **descriptorType**: `integer()`.

    The type of descriptors. Only CV_32F and CV_8U are supported.

  ##### Keyword Arguments
  - **enable_precise_upscale**: `bool`.

    Whether to enable precise upscaling in the scale pyramid, which maps
    index \\f$\\texttt{x}\\f$ to \\f$\\texttt{2x}\\f$. This prevents localization bias. The option
    is disabled by default.

  ##### Return
  - **retval**: `Evision.SIFT.t()`

  **Note**: The contrast threshold will be divided by nOctaveLayers when the filtering is applied. When
  nOctaveLayers is set to default and if you want to use the value used in D. Lowe paper, 0.03, set
  this argument to 0.09.

  Python prototype (for reference only):
  ```python3
  create(nfeatures, nOctaveLayers, contrastThreshold, edgeThreshold, sigma, descriptorType[, enable_precise_upscale]) -> retval
  ```
  """
  @spec create(integer(), integer(), number(), number(), number(), integer()) :: Evision.SIFT.t() | {:error, String.t()}
  def create(nfeatures, nOctaveLayers, contrastThreshold, edgeThreshold, sigma, descriptorType) when is_integer(nfeatures) and is_integer(nOctaveLayers) and is_number(contrastThreshold) and is_number(edgeThreshold) and is_number(sigma) and is_integer(descriptorType)
  do
    positional = [
      nfeatures: Evision.Internal.Structurise.from_struct(nfeatures),
      nOctaveLayers: Evision.Internal.Structurise.from_struct(nOctaveLayers),
      contrastThreshold: Evision.Internal.Structurise.from_struct(contrastThreshold),
      edgeThreshold: Evision.Internal.Structurise.from_struct(edgeThreshold),
      sigma: Evision.Internal.Structurise.from_struct(sigma),
      descriptorType: Evision.Internal.Structurise.from_struct(descriptorType)
    ]
    :evision_nif.sift_create_static(positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **nfeatures**: `integer()`.

    The number of best features to retain. The features are ranked by their scores
    (measured in SIFT algorithm as the local contrast)

  - **nOctaveLayers**: `integer()`.

    The number of layers in each octave. 3 is the value used in D. Lowe paper. The
    number of octaves is computed automatically from the image resolution.

  - **contrastThreshold**: `double`.

    The contrast threshold used to filter out weak features in semi-uniform
    (low-contrast) regions. The larger the threshold, the less features are produced by the detector.

  - **edgeThreshold**: `double`.

    The threshold used to filter out edge-like features. Note that the its meaning
    is different from the contrastThreshold, i.e. the larger the edgeThreshold, the less features are
    filtered out (more features are retained).

  - **sigma**: `double`.

    The sigma of the Gaussian applied to the input image at the octave \\#0. If your image
    is captured with a weak camera with soft lenses, you might want to reduce the number.

  - **enable_precise_upscale**: `bool`.

    Whether to enable precise upscaling in the scale pyramid, which maps
    index \\f$\\texttt{x}\\f$ to \\f$\\texttt{2x}\\f$. This prevents localization bias. The option
    is disabled by default.

  ##### Return
  - **retval**: `Evision.SIFT.t()`

  **Note**: The contrast threshold will be divided by nOctaveLayers when the filtering is applied. When
  nOctaveLayers is set to default and if you want to use the value used in D. Lowe paper, 0.03, set
  this argument to 0.09.

  Python prototype (for reference only):
  ```python3
  create([, nfeatures[, nOctaveLayers[, contrastThreshold[, edgeThreshold[, sigma[, enable_precise_upscale]]]]]]) -> retval
  ```
  """
  @spec create([{:contrastThreshold, term()} | {:edgeThreshold, term()} | {:enable_precise_upscale, term()} | {:nOctaveLayers, term()} | {:nfeatures, term()} | {:sigma, term()}] | nil) :: Evision.SIFT.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:contrastThreshold, :edgeThreshold, :enable_precise_upscale, :nOctaveLayers, :nfeatures, :sigma])
    positional = [
    ]
    :evision_nif.sift_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **nfeatures**: `integer()`.

    The number of best features to retain. The features are ranked by their scores
    (measured in SIFT algorithm as the local contrast)

  - **nOctaveLayers**: `integer()`.

    The number of layers in each octave. 3 is the value used in D. Lowe paper. The
    number of octaves is computed automatically from the image resolution.

  - **contrastThreshold**: `double`.

    The contrast threshold used to filter out weak features in semi-uniform
    (low-contrast) regions. The larger the threshold, the less features are produced by the detector.

  - **edgeThreshold**: `double`.

    The threshold used to filter out edge-like features. Note that the its meaning
    is different from the contrastThreshold, i.e. the larger the edgeThreshold, the less features are
    filtered out (more features are retained).

  - **sigma**: `double`.

    The sigma of the Gaussian applied to the input image at the octave \\#0. If your image
    is captured with a weak camera with soft lenses, you might want to reduce the number.

  - **enable_precise_upscale**: `bool`.

    Whether to enable precise upscaling in the scale pyramid, which maps
    index \\f$\\texttt{x}\\f$ to \\f$\\texttt{2x}\\f$. This prevents localization bias. The option
    is disabled by default.

  ##### Return
  - **retval**: `Evision.SIFT.t()`

  **Note**: The contrast threshold will be divided by nOctaveLayers when the filtering is applied. When
  nOctaveLayers is set to default and if you want to use the value used in D. Lowe paper, 0.03, set
  this argument to 0.09.

  Python prototype (for reference only):
  ```python3
  create([, nfeatures[, nOctaveLayers[, contrastThreshold[, edgeThreshold[, sigma[, enable_precise_upscale]]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.SIFT.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.sift_create_static(positional)
    |> to_struct()
  end

  @doc """
  defaultNorm

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  defaultNorm() -> retval
  ```
  """
  @spec defaultNorm(Evision.SIFT.t()) :: integer() | {:error, String.t()}
  def defaultNorm(self) do
    positional = [
    ]
    :evision_nif.sift_defaultNorm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorSize

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorSize() -> retval
  ```
  """
  @spec descriptorSize(Evision.SIFT.t()) :: integer() | {:error, String.t()}
  def descriptorSize(self) do
    positional = [
    ]
    :evision_nif.sift_descriptorSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorType

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorType() -> retval
  ```
  """
  @spec descriptorType(Evision.SIFT.t()) :: integer() | {:error, String.t()}
  def descriptorType(self) do
    positional = [
    ]
    :evision_nif.sift_descriptorType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
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
  - **self**: `Evision.SIFT.t()`
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
  @spec detect(Evision.SIFT.t(), list(Evision.Mat.maybe_mat_in()), [{:masks, term()}] | nil) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images, opts) when is_list(images) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.sift_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.SIFT.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.sift_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
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
  - **self**: `Evision.SIFT.t()`
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
  @spec detect(Evision.SIFT.t(), list(Evision.Mat.maybe_mat_in())) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images) when is_list(images)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.sift_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.SIFT.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.sift_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
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
  @spec detectAndCompute(Evision.SIFT.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:useProvidedKeypoints, term()}] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:useProvidedKeypoints])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.sift_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
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
  @spec detectAndCompute(Evision.SIFT.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.sift_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.SIFT.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.sift_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getContrastThreshold

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getContrastThreshold() -> retval
  ```
  """
  @spec getContrastThreshold(Evision.SIFT.t()) :: number() | {:error, String.t()}
  def getContrastThreshold(self) do
    positional = [
    ]
    :evision_nif.sift_getContrastThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.SIFT.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.sift_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getEdgeThreshold

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getEdgeThreshold() -> retval
  ```
  """
  @spec getEdgeThreshold(Evision.SIFT.t()) :: number() | {:error, String.t()}
  def getEdgeThreshold(self) do
    positional = [
    ]
    :evision_nif.sift_getEdgeThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNFeatures

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNFeatures() -> retval
  ```
  """
  @spec getNFeatures(Evision.SIFT.t()) :: integer() | {:error, String.t()}
  def getNFeatures(self) do
    positional = [
    ]
    :evision_nif.sift_getNFeatures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNOctaveLayers

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNOctaveLayers() -> retval
  ```
  """
  @spec getNOctaveLayers(Evision.SIFT.t()) :: integer() | {:error, String.t()}
  def getNOctaveLayers(self) do
    positional = [
    ]
    :evision_nif.sift_getNOctaveLayers(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSigma

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getSigma() -> retval
  ```
  """
  @spec getSigma(Evision.SIFT.t()) :: number() | {:error, String.t()}
  def getSigma(self) do
    positional = [
    ]
    :evision_nif.sift_getSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  read

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
  - **arg1**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(arg1) -> None
  ```
  #### Variant 2:
  read

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  read(fileName) -> None
  ```

  """
  @spec read(Evision.SIFT.t(), Evision.FileNode.t()) :: Evision.SIFT.t() | {:error, String.t()}
  def read(self, arg1) when is_struct(arg1, Evision.FileNode)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1)
    ]
    :evision_nif.sift_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec read(Evision.SIFT.t(), binary()) :: Evision.SIFT.t() | {:error, String.t()}
  def read(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.sift_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setContrastThreshold

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
  - **contrastThreshold**: `double`

  Python prototype (for reference only):
  ```python3
  setContrastThreshold(contrastThreshold) -> None
  ```
  """
  @spec setContrastThreshold(Evision.SIFT.t(), number()) :: Evision.SIFT.t() | {:error, String.t()}
  def setContrastThreshold(self, contrastThreshold) when is_number(contrastThreshold)
  do
    positional = [
      contrastThreshold: Evision.Internal.Structurise.from_struct(contrastThreshold)
    ]
    :evision_nif.sift_setContrastThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setEdgeThreshold

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
  - **edgeThreshold**: `double`

  Python prototype (for reference only):
  ```python3
  setEdgeThreshold(edgeThreshold) -> None
  ```
  """
  @spec setEdgeThreshold(Evision.SIFT.t(), number()) :: Evision.SIFT.t() | {:error, String.t()}
  def setEdgeThreshold(self, edgeThreshold) when is_number(edgeThreshold)
  do
    positional = [
      edgeThreshold: Evision.Internal.Structurise.from_struct(edgeThreshold)
    ]
    :evision_nif.sift_setEdgeThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNFeatures

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
  - **maxFeatures**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNFeatures(maxFeatures) -> None
  ```
  """
  @spec setNFeatures(Evision.SIFT.t(), integer()) :: Evision.SIFT.t() | {:error, String.t()}
  def setNFeatures(self, maxFeatures) when is_integer(maxFeatures)
  do
    positional = [
      maxFeatures: Evision.Internal.Structurise.from_struct(maxFeatures)
    ]
    :evision_nif.sift_setNFeatures(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNOctaveLayers

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
  - **nOctaveLayers**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNOctaveLayers(nOctaveLayers) -> None
  ```
  """
  @spec setNOctaveLayers(Evision.SIFT.t(), integer()) :: Evision.SIFT.t() | {:error, String.t()}
  def setNOctaveLayers(self, nOctaveLayers) when is_integer(nOctaveLayers)
  do
    positional = [
      nOctaveLayers: Evision.Internal.Structurise.from_struct(nOctaveLayers)
    ]
    :evision_nif.sift_setNOctaveLayers(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSigma

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
  - **sigma**: `double`

  Python prototype (for reference only):
  ```python3
  setSigma(sigma) -> None
  ```
  """
  @spec setSigma(Evision.SIFT.t(), number()) :: Evision.SIFT.t() | {:error, String.t()}
  def setSigma(self, sigma) when is_number(sigma)
  do
    positional = [
      sigma: Evision.Internal.Structurise.from_struct(sigma)
    ]
    :evision_nif.sift_setSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.SIFT.t(), Evision.FileStorage.t(), binary()) :: Evision.SIFT.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.sift_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.SIFT.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  write(fileName) -> None
  ```
  """
  @spec write(Evision.SIFT.t(), binary()) :: Evision.SIFT.t() | {:error, String.t()}
  def write(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.sift_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
