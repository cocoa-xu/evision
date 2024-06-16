defmodule Evision.XFeatures2D.VGG do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XFeatures2D.VGG` struct.

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
  def to_struct({:ok, %{class: Evision.XFeatures2D.VGG, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XFeatures2D.VGG, ref: ref}) do
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
  - **self**: `Evision.XFeatures2D.VGG.t()`
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
  - **self**: `Evision.XFeatures2D.VGG.t()`
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
  @spec compute(Evision.XFeatures2D.VGG.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t())), [{atom(), term()},...] | nil) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints, opts) when is_list(images) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_VGG_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.XFeatures2D.VGG.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t()), [{atom(), term()},...] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_VGG_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
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
  - **self**: `Evision.XFeatures2D.VGG.t()`
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
  @spec compute(Evision.XFeatures2D.VGG.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t()))) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints) when is_list(images) and is_list(keypoints)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_VGG_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.XFeatures2D.VGG.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t())) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.xfeatures2d_VGG_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **desc**: `integer()`.
  - **isigma**: `float`.
  - **img_normalize**: `bool`.
  - **use_scale_orientation**: `bool`.
  - **scale_factor**: `float`.
  - **dsc_normalize**: `bool`.

  ##### Return
  - **retval**: `VGG`

  Python prototype (for reference only):
  ```python3
  create([, desc[, isigma[, img_normalize[, use_scale_orientation[, scale_factor[, dsc_normalize]]]]]]) -> retval
  ```
  """
  @spec create([{:desc, term()} | {:dsc_normalize, term()} | {:img_normalize, term()} | {:isigma, term()} | {:scale_factor, term()} | {:use_scale_orientation, term()}] | nil) :: Evision.XFeatures2D.VGG.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:desc, :dsc_normalize, :img_normalize, :isigma, :scale_factor, :use_scale_orientation])
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_VGG_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create
  ##### Keyword Arguments
  - **desc**: `integer()`.
  - **isigma**: `float`.
  - **img_normalize**: `bool`.
  - **use_scale_orientation**: `bool`.
  - **scale_factor**: `float`.
  - **dsc_normalize**: `bool`.

  ##### Return
  - **retval**: `VGG`

  Python prototype (for reference only):
  ```python3
  create([, desc[, isigma[, img_normalize[, use_scale_orientation[, scale_factor[, dsc_normalize]]]]]]) -> retval
  ```
  """
  @spec create() :: Evision.XFeatures2D.VGG.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_VGG_create_static(positional)
    |> to_struct()
  end

  @doc """
  defaultNorm

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  defaultNorm() -> retval
  ```
  """
  @spec defaultNorm(Evision.XFeatures2D.VGG.t()) :: integer() | {:error, String.t()}
  def defaultNorm(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_VGG_defaultNorm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorSize

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorSize() -> retval
  ```
  """
  @spec descriptorSize(Evision.XFeatures2D.VGG.t()) :: integer() | {:error, String.t()}
  def descriptorSize(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_VGG_descriptorSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorType

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorType() -> retval
  ```
  """
  @spec descriptorType(Evision.XFeatures2D.VGG.t()) :: integer() | {:error, String.t()}
  def descriptorType(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_VGG_descriptorType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
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
  - **self**: `Evision.XFeatures2D.VGG.t()`
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
  @spec detect(Evision.XFeatures2D.VGG.t(), list(Evision.Mat.maybe_mat_in()), [{:masks, term()}] | nil) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images, opts) when is_list(images) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.xfeatures2d_VGG_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.XFeatures2D.VGG.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.xfeatures2d_VGG_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
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
  - **self**: `Evision.XFeatures2D.VGG.t()`
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
  @spec detect(Evision.XFeatures2D.VGG.t(), list(Evision.Mat.maybe_mat_in())) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images) when is_list(images)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.xfeatures2d_VGG_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.XFeatures2D.VGG.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.xfeatures2d_VGG_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
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
  @spec detectAndCompute(Evision.XFeatures2D.VGG.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:useProvidedKeypoints, term()}] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:useProvidedKeypoints])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.xfeatures2d_VGG_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
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
  @spec detectAndCompute(Evision.XFeatures2D.VGG.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.xfeatures2d_VGG_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XFeatures2D.VGG.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_VGG_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XFeatures2D.VGG.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_VGG_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getScaleFactor

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getScaleFactor() -> retval
  ```
  """
  @spec getScaleFactor(Evision.XFeatures2D.VGG.t()) :: number() | {:error, String.t()}
  def getScaleFactor(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_VGG_getScaleFactor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getSigma

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  getSigma() -> retval
  ```
  """
  @spec getSigma(Evision.XFeatures2D.VGG.t()) :: number() | {:error, String.t()}
  def getSigma(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_VGG_getSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUseNormalizeDescriptor

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getUseNormalizeDescriptor() -> retval
  ```
  """
  @spec getUseNormalizeDescriptor(Evision.XFeatures2D.VGG.t()) :: boolean() | {:error, String.t()}
  def getUseNormalizeDescriptor(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_VGG_getUseNormalizeDescriptor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUseNormalizeImage

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getUseNormalizeImage() -> retval
  ```
  """
  @spec getUseNormalizeImage(Evision.XFeatures2D.VGG.t()) :: boolean() | {:error, String.t()}
  def getUseNormalizeImage(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_VGG_getUseNormalizeImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUseScaleOrientation

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getUseScaleOrientation() -> retval
  ```
  """
  @spec getUseScaleOrientation(Evision.XFeatures2D.VGG.t()) :: boolean() | {:error, String.t()}
  def getUseScaleOrientation(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_VGG_getUseScaleOrientation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  read

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
  - **arg1**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(arg1) -> None
  ```
  #### Variant 2:
  read

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  read(fileName) -> None
  ```

  """
  @spec read(Evision.XFeatures2D.VGG.t(), Evision.FileNode.t()) :: Evision.XFeatures2D.VGG.t() | {:error, String.t()}
  def read(self, arg1) when is_struct(arg1, Evision.FileNode)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1)
    ]
    :evision_nif.xfeatures2d_VGG_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec read(Evision.XFeatures2D.VGG.t(), binary()) :: Evision.XFeatures2D.VGG.t() | {:error, String.t()}
  def read(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.xfeatures2d_VGG_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setScaleFactor

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
  - **scale_factor**: `float`

  Python prototype (for reference only):
  ```python3
  setScaleFactor(scale_factor) -> None
  ```
  """
  @spec setScaleFactor(Evision.XFeatures2D.VGG.t(), number()) :: Evision.XFeatures2D.VGG.t() | {:error, String.t()}
  def setScaleFactor(self, scale_factor) when is_float(scale_factor)
  do
    positional = [
      scale_factor: Evision.Internal.Structurise.from_struct(scale_factor)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_VGG_setScaleFactor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setSigma

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
  - **isigma**: `float`

  Python prototype (for reference only):
  ```python3
  setSigma(isigma) -> None
  ```
  """
  @spec setSigma(Evision.XFeatures2D.VGG.t(), number()) :: Evision.XFeatures2D.VGG.t() | {:error, String.t()}
  def setSigma(self, isigma) when is_float(isigma)
  do
    positional = [
      isigma: Evision.Internal.Structurise.from_struct(isigma)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_VGG_setSigma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setUseNormalizeDescriptor

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
  - **dsc_normalize**: `bool`

  Python prototype (for reference only):
  ```python3
  setUseNormalizeDescriptor(dsc_normalize) -> None
  ```
  """
  @spec setUseNormalizeDescriptor(Evision.XFeatures2D.VGG.t(), boolean()) :: Evision.XFeatures2D.VGG.t() | {:error, String.t()}
  def setUseNormalizeDescriptor(self, dsc_normalize) when is_boolean(dsc_normalize)
  do
    positional = [
      dsc_normalize: Evision.Internal.Structurise.from_struct(dsc_normalize)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_VGG_setUseNormalizeDescriptor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setUseNormalizeImage

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
  - **img_normalize**: `bool`

  Python prototype (for reference only):
  ```python3
  setUseNormalizeImage(img_normalize) -> None
  ```
  """
  @spec setUseNormalizeImage(Evision.XFeatures2D.VGG.t(), boolean()) :: Evision.XFeatures2D.VGG.t() | {:error, String.t()}
  def setUseNormalizeImage(self, img_normalize) when is_boolean(img_normalize)
  do
    positional = [
      img_normalize: Evision.Internal.Structurise.from_struct(img_normalize)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_VGG_setUseNormalizeImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setUseScaleOrientation

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
  - **use_scale_orientation**: `bool`

  Python prototype (for reference only):
  ```python3
  setUseScaleOrientation(use_scale_orientation) -> None
  ```
  """
  @spec setUseScaleOrientation(Evision.XFeatures2D.VGG.t(), boolean()) :: Evision.XFeatures2D.VGG.t() | {:error, String.t()}
  def setUseScaleOrientation(self, use_scale_orientation) when is_boolean(use_scale_orientation)
  do
    positional = [
      use_scale_orientation: Evision.Internal.Structurise.from_struct(use_scale_orientation)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_VGG_setUseScaleOrientation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XFeatures2D.VGG.t(), Evision.FileStorage.t(), binary()) :: Evision.XFeatures2D.VGG.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.xfeatures2d_VGG_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.VGG.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  write(fileName) -> None
  ```
  """
  @spec write(Evision.XFeatures2D.VGG.t(), binary()) :: Evision.XFeatures2D.VGG.t() | {:error, String.t()}
  def write(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.xfeatures2d_VGG_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
