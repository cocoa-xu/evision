defmodule Evision.XFeatures2D.DAISY do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XFeatures2D.DAISY` struct.

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
  def to_struct({:ok, %{class: Evision.XFeatures2D.DAISY, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XFeatures2D.DAISY, ref: ref}) do
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
  - **self**: `Evision.XFeatures2D.DAISY.t()`
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
  - **self**: `Evision.XFeatures2D.DAISY.t()`
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
  @spec compute(Evision.XFeatures2D.DAISY.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t())), [{atom(), term()},...] | nil) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints, opts) when is_list(images) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_DAISY_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.XFeatures2D.DAISY.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t()), [{atom(), term()},...] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_DAISY_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
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
  - **self**: `Evision.XFeatures2D.DAISY.t()`
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
  @spec compute(Evision.XFeatures2D.DAISY.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t()))) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints) when is_list(images) and is_list(keypoints)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_DAISY_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.XFeatures2D.DAISY.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t())) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_DAISY_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **radius**: `float`.
  - **q_radius**: `integer()`.
  - **q_theta**: `integer()`.
  - **q_hist**: `integer()`.
  - **norm**: `DAISY_NormalizationType`.
  - **h**: `Evision.Mat`.
  - **interpolation**: `bool`.
  - **use_orientation**: `bool`.

  ##### Return
  - **retval**: `DAISY`

  Python prototype (for reference only):
  ```python3
  create([, radius[, q_radius[, q_theta[, q_hist[, norm[, H[, interpolation[, use_orientation]]]]]]]]) -> retval
  ```
  """
  @spec create([{:h, term()} | {:interpolation, term()} | {:norm, term()} | {:q_hist, term()} | {:q_radius, term()} | {:q_theta, term()} | {:radius, term()} | {:use_orientation, term()}] | nil) :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:h, :interpolation, :norm, :q_hist, :q_radius, :q_theta, :radius, :use_orientation])
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **radius**: `float`.
  - **q_radius**: `integer()`.
  - **q_theta**: `integer()`.
  - **q_hist**: `integer()`.
  - **norm**: `DAISY_NormalizationType`.
  - **h**: `Evision.Mat`.
  - **interpolation**: `bool`.
  - **use_orientation**: `bool`.

  ##### Return
  - **retval**: `DAISY`

  Python prototype (for reference only):
  ```python3
  create([, radius[, q_radius[, q_theta[, q_hist[, norm[, H[, interpolation[, use_orientation]]]]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_create_static(positional)
    |> to_struct()
  end

  @doc """
  defaultNorm

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  defaultNorm() -> retval
  ```
  """
  @spec defaultNorm(Evision.XFeatures2D.DAISY.t()) :: integer() | {:error, String.t()}
  def defaultNorm(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_DAISY_defaultNorm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorSize

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorSize() -> retval
  ```
  """
  @spec descriptorSize(Evision.XFeatures2D.DAISY.t()) :: integer() | {:error, String.t()}
  def descriptorSize(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_DAISY_descriptorSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorType

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorType() -> retval
  ```
  """
  @spec descriptorType(Evision.XFeatures2D.DAISY.t()) :: integer() | {:error, String.t()}
  def descriptorType(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_DAISY_descriptorType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
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
  - **self**: `Evision.XFeatures2D.DAISY.t()`
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
  @spec detect(Evision.XFeatures2D.DAISY.t(), list(Evision.Mat.maybe_mat_in()), [{:masks, term()}] | nil) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images, opts) when is_list(images) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.xfeatures2d_DAISY_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.XFeatures2D.DAISY.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.xfeatures2d_DAISY_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
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
  - **self**: `Evision.XFeatures2D.DAISY.t()`
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
  @spec detect(Evision.XFeatures2D.DAISY.t(), list(Evision.Mat.maybe_mat_in())) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images) when is_list(images)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.xfeatures2d_DAISY_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.XFeatures2D.DAISY.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.xfeatures2d_DAISY_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
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
  @spec detectAndCompute(Evision.XFeatures2D.DAISY.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:useProvidedKeypoints, term()}] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:useProvidedKeypoints])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.xfeatures2d_DAISY_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
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
  @spec detectAndCompute(Evision.XFeatures2D.DAISY.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.xfeatures2d_DAISY_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XFeatures2D.DAISY.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_DAISY_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XFeatures2D.DAISY.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getH

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getH() -> retval
  ```
  """
  @spec getH(Evision.XFeatures2D.DAISY.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getH(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_getH(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getInterpolation

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getInterpolation() -> retval
  ```
  """
  @spec getInterpolation(Evision.XFeatures2D.DAISY.t()) :: boolean() | {:error, String.t()}
  def getInterpolation(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_getInterpolation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getNorm

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getNorm() -> retval
  ```
  """
  @spec getNorm(Evision.XFeatures2D.DAISY.t()) :: integer() | {:error, String.t()}
  def getNorm(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_getNorm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getQHist

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getQHist() -> retval
  ```
  """
  @spec getQHist(Evision.XFeatures2D.DAISY.t()) :: integer() | {:error, String.t()}
  def getQHist(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_getQHist(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getQRadius

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getQRadius() -> retval
  ```
  """
  @spec getQRadius(Evision.XFeatures2D.DAISY.t()) :: integer() | {:error, String.t()}
  def getQRadius(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_getQRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getQTheta

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getQTheta() -> retval
  ```
  """
  @spec getQTheta(Evision.XFeatures2D.DAISY.t()) :: integer() | {:error, String.t()}
  def getQTheta(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_getQTheta(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRadius

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getRadius() -> retval
  ```
  """
  @spec getRadius(Evision.XFeatures2D.DAISY.t()) :: number() | {:error, String.t()}
  def getRadius(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_getRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUseOrientation

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getUseOrientation() -> retval
  ```
  """
  @spec getUseOrientation(Evision.XFeatures2D.DAISY.t()) :: boolean() | {:error, String.t()}
  def getUseOrientation(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_getUseOrientation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  read

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
  - **arg1**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(arg1) -> None
  ```
  #### Variant 2:
  read

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  read(fileName) -> None
  ```

  """
  @spec read(Evision.XFeatures2D.DAISY.t(), Evision.FileNode.t()) :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def read(self, arg1) when is_struct(arg1, Evision.FileNode)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1)
    ]
    :evision_nif.xfeatures2d_DAISY_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec read(Evision.XFeatures2D.DAISY.t(), binary()) :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def read(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.xfeatures2d_DAISY_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setH

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
  - **h**: `Evision.Mat`

  Python prototype (for reference only):
  ```python3
  setH(H) -> None
  ```
  """
  @spec setH(Evision.XFeatures2D.DAISY.t(), Evision.Mat.maybe_mat_in()) :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def setH(self, h) when (is_struct(h, Evision.Mat) or is_struct(h, Nx.Tensor) or is_number(h) or is_tuple(h))
  do
    positional = [
      h: Evision.Internal.Structurise.from_struct(h)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_setH(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setInterpolation

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
  - **interpolation**: `bool`

  Python prototype (for reference only):
  ```python3
  setInterpolation(interpolation) -> None
  ```
  """
  @spec setInterpolation(Evision.XFeatures2D.DAISY.t(), boolean()) :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def setInterpolation(self, interpolation) when is_boolean(interpolation)
  do
    positional = [
      interpolation: Evision.Internal.Structurise.from_struct(interpolation)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_setInterpolation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setNorm

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
  - **norm**: `integer()`

  Python prototype (for reference only):
  ```python3
  setNorm(norm) -> None
  ```
  """
  @spec setNorm(Evision.XFeatures2D.DAISY.t(), integer()) :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def setNorm(self, norm) when is_integer(norm)
  do
    positional = [
      norm: Evision.Internal.Structurise.from_struct(norm)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_setNorm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setQHist

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
  - **q_hist**: `integer()`

  Python prototype (for reference only):
  ```python3
  setQHist(q_hist) -> None
  ```
  """
  @spec setQHist(Evision.XFeatures2D.DAISY.t(), integer()) :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def setQHist(self, q_hist) when is_integer(q_hist)
  do
    positional = [
      q_hist: Evision.Internal.Structurise.from_struct(q_hist)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_setQHist(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setQRadius

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
  - **q_radius**: `integer()`

  Python prototype (for reference only):
  ```python3
  setQRadius(q_radius) -> None
  ```
  """
  @spec setQRadius(Evision.XFeatures2D.DAISY.t(), integer()) :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def setQRadius(self, q_radius) when is_integer(q_radius)
  do
    positional = [
      q_radius: Evision.Internal.Structurise.from_struct(q_radius)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_setQRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setQTheta

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
  - **q_theta**: `integer()`

  Python prototype (for reference only):
  ```python3
  setQTheta(q_theta) -> None
  ```
  """
  @spec setQTheta(Evision.XFeatures2D.DAISY.t(), integer()) :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def setQTheta(self, q_theta) when is_integer(q_theta)
  do
    positional = [
      q_theta: Evision.Internal.Structurise.from_struct(q_theta)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_setQTheta(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRadius

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
  - **radius**: `float`

  Python prototype (for reference only):
  ```python3
  setRadius(radius) -> None
  ```
  """
  @spec setRadius(Evision.XFeatures2D.DAISY.t(), number()) :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def setRadius(self, radius) when is_float(radius)
  do
    positional = [
      radius: Evision.Internal.Structurise.from_struct(radius)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_setRadius(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setUseOrientation

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
  - **use_orientation**: `bool`

  Python prototype (for reference only):
  ```python3
  setUseOrientation(use_orientation) -> None
  ```
  """
  @spec setUseOrientation(Evision.XFeatures2D.DAISY.t(), boolean()) :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def setUseOrientation(self, use_orientation) when is_boolean(use_orientation)
  do
    positional = [
      use_orientation: Evision.Internal.Structurise.from_struct(use_orientation)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_DAISY_setUseOrientation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XFeatures2D.DAISY.t(), Evision.FileStorage.t(), binary()) :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.xfeatures2d_DAISY_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.DAISY.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  write(fileName) -> None
  ```
  """
  @spec write(Evision.XFeatures2D.DAISY.t(), binary()) :: Evision.XFeatures2D.DAISY.t() | {:error, String.t()}
  def write(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.xfeatures2d_DAISY_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
