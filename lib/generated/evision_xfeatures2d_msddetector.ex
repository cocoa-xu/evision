defmodule Evision.XFeatures2D.MSDDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XFeatures2D.MSDDetector` struct.

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
  def to_struct({:ok, %{class: Evision.XFeatures2D.MSDDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XFeatures2D.MSDDetector, ref: ref}) do
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
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
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
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
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
  @spec compute(Evision.XFeatures2D.MSDDetector.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t())), [{atom(), term()},...] | nil) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints, opts) when is_list(images) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_MSDDetector_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.XFeatures2D.MSDDetector.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t()), [{atom(), term()},...] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_MSDDetector_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
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
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
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
  @spec compute(Evision.XFeatures2D.MSDDetector.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t()))) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints) when is_list(images) and is_list(keypoints)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_MSDDetector_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.XFeatures2D.MSDDetector.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t())) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_MSDDetector_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **m_patch_radius**: `integer()`.
  - **m_search_area_radius**: `integer()`.
  - **m_nms_radius**: `integer()`.
  - **m_nms_scale_radius**: `integer()`.
  - **m_th_saliency**: `float`.
  - **m_kNN**: `integer()`.
  - **m_scale_factor**: `float`.
  - **m_n_scales**: `integer()`.
  - **m_compute_orientation**: `bool`.

  ##### Return
  - **retval**: `MSDDetector`

  Python prototype (for reference only):
  ```python3
  create([, m_patch_radius[, m_search_area_radius[, m_nms_radius[, m_nms_scale_radius[, m_th_saliency[, m_kNN[, m_scale_factor[, m_n_scales[, m_compute_orientation]]]]]]]]]) -> retval
  ```
  """
  @spec create([{:m_compute_orientation, term()} | {:m_kNN, term()} | {:m_n_scales, term()} | {:m_nms_radius, term()} | {:m_nms_scale_radius, term()} | {:m_patch_radius, term()} | {:m_scale_factor, term()} | {:m_search_area_radius, term()} | {:m_th_saliency, term()}] | nil) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:m_compute_orientation, :m_kNN, :m_n_scales, :m_nms_radius, :m_nms_scale_radius, :m_patch_radius, :m_scale_factor, :m_search_area_radius, :m_th_saliency])
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **m_patch_radius**: `integer()`.
  - **m_search_area_radius**: `integer()`.
  - **m_nms_radius**: `integer()`.
  - **m_nms_scale_radius**: `integer()`.
  - **m_th_saliency**: `float`.
  - **m_kNN**: `integer()`.
  - **m_scale_factor**: `float`.
  - **m_n_scales**: `integer()`.
  - **m_compute_orientation**: `bool`.

  ##### Return
  - **retval**: `MSDDetector`

  Python prototype (for reference only):
  ```python3
  create([, m_patch_radius[, m_search_area_radius[, m_nms_radius[, m_nms_scale_radius[, m_th_saliency[, m_kNN[, m_scale_factor[, m_n_scales[, m_compute_orientation]]]]]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_create_static(positional)
    |> to_struct()
  end

  @doc """
  defaultNorm

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  defaultNorm() -> retval
  ```
  """
  @spec defaultNorm(Evision.XFeatures2D.MSDDetector.t()) :: integer() | {:error, String.t()}
  def defaultNorm(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_MSDDetector_defaultNorm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorSize

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorSize() -> retval
  ```
  """
  @spec descriptorSize(Evision.XFeatures2D.MSDDetector.t()) :: integer() | {:error, String.t()}
  def descriptorSize(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_MSDDetector_descriptorSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorType

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorType() -> retval
  ```
  """
  @spec descriptorType(Evision.XFeatures2D.MSDDetector.t()) :: integer() | {:error, String.t()}
  def descriptorType(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_MSDDetector_descriptorType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
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
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
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
  @spec detect(Evision.XFeatures2D.MSDDetector.t(), list(Evision.Mat.maybe_mat_in()), [{:masks, term()}] | nil) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images, opts) when is_list(images) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.xfeatures2d_MSDDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.XFeatures2D.MSDDetector.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.xfeatures2d_MSDDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
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
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
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
  @spec detect(Evision.XFeatures2D.MSDDetector.t(), list(Evision.Mat.maybe_mat_in())) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images) when is_list(images)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.xfeatures2d_MSDDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.XFeatures2D.MSDDetector.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.xfeatures2d_MSDDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
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
  @spec detectAndCompute(Evision.XFeatures2D.MSDDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:useProvidedKeypoints, term()}] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:useProvidedKeypoints])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.xfeatures2d_MSDDetector_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
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
  @spec detectAndCompute(Evision.XFeatures2D.MSDDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.xfeatures2d_MSDDetector_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XFeatures2D.MSDDetector.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_MSDDetector_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getComputeOrientation

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getComputeOrientation() -> retval
  ```
  """
  @spec getComputeOrientation(Evision.XFeatures2D.MSDDetector.t()) :: boolean() | {:error, String.t()}
  def getComputeOrientation(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_getComputeOrientation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XFeatures2D.MSDDetector.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getKNN

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getKNN() -> retval
  ```
  """
  @spec getKNN(Evision.XFeatures2D.MSDDetector.t()) :: integer() | {:error, String.t()}
  def getKNN(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_getKNN(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNScales

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNScales() -> retval
  ```
  """
  @spec getNScales(Evision.XFeatures2D.MSDDetector.t()) :: integer() | {:error, String.t()}
  def getNScales(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_getNScales(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNmsRadius

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNmsRadius() -> retval
  ```
  """
  @spec getNmsRadius(Evision.XFeatures2D.MSDDetector.t()) :: integer() | {:error, String.t()}
  def getNmsRadius(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_getNmsRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNmsScaleRadius

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNmsScaleRadius() -> retval
  ```
  """
  @spec getNmsScaleRadius(Evision.XFeatures2D.MSDDetector.t()) :: integer() | {:error, String.t()}
  def getNmsScaleRadius(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_getNmsScaleRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getPatchRadius

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getPatchRadius() -> retval
  ```
  """
  @spec getPatchRadius(Evision.XFeatures2D.MSDDetector.t()) :: integer() | {:error, String.t()}
  def getPatchRadius(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_getPatchRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getScaleFactor

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getScaleFactor() -> retval
  ```
  """
  @spec getScaleFactor(Evision.XFeatures2D.MSDDetector.t()) :: number() | {:error, String.t()}
  def getScaleFactor(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_getScaleFactor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSearchAreaRadius

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getSearchAreaRadius() -> retval
  ```
  """
  @spec getSearchAreaRadius(Evision.XFeatures2D.MSDDetector.t()) :: integer() | {:error, String.t()}
  def getSearchAreaRadius(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_getSearchAreaRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getThSaliency

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getThSaliency() -> retval
  ```
  """
  @spec getThSaliency(Evision.XFeatures2D.MSDDetector.t()) :: number() | {:error, String.t()}
  def getThSaliency(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_getThSaliency(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  read

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
  - **arg1**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(arg1) -> None
  ```
  #### Variant 2:
  read

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  read(fileName) -> None
  ```

  """
  @spec read(Evision.XFeatures2D.MSDDetector.t(), Evision.FileNode.t()) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def read(self, arg1) when is_struct(arg1, Evision.FileNode)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1)
    ]
    :evision_nif.xfeatures2d_MSDDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec read(Evision.XFeatures2D.MSDDetector.t(), binary()) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def read(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.xfeatures2d_MSDDetector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setComputeOrientation

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
  - **compute_orientation**: `bool`

  Python prototype (for reference only):
  ```python3
  setComputeOrientation(compute_orientation) -> None
  ```
  """
  @spec setComputeOrientation(Evision.XFeatures2D.MSDDetector.t(), boolean()) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def setComputeOrientation(self, compute_orientation) when is_boolean(compute_orientation)
  do
    positional = [
      compute_orientation: Evision.Internal.Structurise.from_struct(compute_orientation)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_setComputeOrientation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setKNN

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
  - **kNN**: `integer()`

  Python prototype (for reference only):
  ```python3
  setKNN(kNN) -> None
  ```
  """
  @spec setKNN(Evision.XFeatures2D.MSDDetector.t(), integer()) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def setKNN(self, kNN) when is_integer(kNN)
  do
    positional = [
      kNN: Evision.Internal.Structurise.from_struct(kNN)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_setKNN(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNScales

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
  - **use_orientation**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNScales(use_orientation) -> None
  ```
  """
  @spec setNScales(Evision.XFeatures2D.MSDDetector.t(), integer()) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def setNScales(self, use_orientation) when is_integer(use_orientation)
  do
    positional = [
      use_orientation: Evision.Internal.Structurise.from_struct(use_orientation)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_setNScales(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNmsRadius

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
  - **nms_radius**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNmsRadius(nms_radius) -> None
  ```
  """
  @spec setNmsRadius(Evision.XFeatures2D.MSDDetector.t(), integer()) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def setNmsRadius(self, nms_radius) when is_integer(nms_radius)
  do
    positional = [
      nms_radius: Evision.Internal.Structurise.from_struct(nms_radius)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_setNmsRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNmsScaleRadius

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
  - **nms_scale_radius**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNmsScaleRadius(nms_scale_radius) -> None
  ```
  """
  @spec setNmsScaleRadius(Evision.XFeatures2D.MSDDetector.t(), integer()) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def setNmsScaleRadius(self, nms_scale_radius) when is_integer(nms_scale_radius)
  do
    positional = [
      nms_scale_radius: Evision.Internal.Structurise.from_struct(nms_scale_radius)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_setNmsScaleRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPatchRadius

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
  - **patch_radius**: `integer()`

  Python prototype (for reference only):
  ```python3
  setPatchRadius(patch_radius) -> None
  ```
  """
  @spec setPatchRadius(Evision.XFeatures2D.MSDDetector.t(), integer()) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def setPatchRadius(self, patch_radius) when is_integer(patch_radius)
  do
    positional = [
      patch_radius: Evision.Internal.Structurise.from_struct(patch_radius)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_setPatchRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setScaleFactor

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
  - **scale_factor**: `float`

  Python prototype (for reference only):
  ```python3
  setScaleFactor(scale_factor) -> None
  ```
  """
  @spec setScaleFactor(Evision.XFeatures2D.MSDDetector.t(), number()) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def setScaleFactor(self, scale_factor) when is_float(scale_factor)
  do
    positional = [
      scale_factor: Evision.Internal.Structurise.from_struct(scale_factor)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_setScaleFactor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSearchAreaRadius

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
  - **use_orientation**: `integer()`

  Python prototype (for reference only):
  ```python3
  setSearchAreaRadius(use_orientation) -> None
  ```
  """
  @spec setSearchAreaRadius(Evision.XFeatures2D.MSDDetector.t(), integer()) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def setSearchAreaRadius(self, use_orientation) when is_integer(use_orientation)
  do
    positional = [
      use_orientation: Evision.Internal.Structurise.from_struct(use_orientation)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_setSearchAreaRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setThSaliency

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
  - **th_saliency**: `float`

  Python prototype (for reference only):
  ```python3
  setThSaliency(th_saliency) -> None
  ```
  """
  @spec setThSaliency(Evision.XFeatures2D.MSDDetector.t(), number()) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def setThSaliency(self, th_saliency) when is_float(th_saliency)
  do
    positional = [
      th_saliency: Evision.Internal.Structurise.from_struct(th_saliency)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_MSDDetector_setThSaliency(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XFeatures2D.MSDDetector.t(), Evision.FileStorage.t(), binary()) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.xfeatures2d_MSDDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.MSDDetector.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  write(fileName) -> None
  ```
  """
  @spec write(Evision.XFeatures2D.MSDDetector.t(), binary()) :: Evision.XFeatures2D.MSDDetector.t() | {:error, String.t()}
  def write(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.xfeatures2d_MSDDetector_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
