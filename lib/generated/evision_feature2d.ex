defmodule Evision.Feature2D do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Feature2D` struct.

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
  def to_struct({:ok, %{class: Evision.Feature2D, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Feature2D, ref: ref}) do
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
  - **self**: `Evision.Feature2D.t()`
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
  - **self**: `Evision.Feature2D.t()`
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
  @spec compute(Evision.Feature2D.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t())), [{atom(), term()},...] | nil) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints, opts) when is_list(images) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.feature2D_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec compute(Evision.Feature2D.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t()), [{atom(), term()},...] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.feature2D_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  compute

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`
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
  - **self**: `Evision.Feature2D.t()`
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
  @spec compute(Evision.Feature2D.t(), list(Evision.Mat.maybe_mat_in()), list(list(Evision.KeyPoint.t()))) :: {list(list(Evision.KeyPoint.t())), list(Evision.Mat.t())} | {:error, String.t()}
  def compute(self, images, keypoints) when is_list(images) and is_list(keypoints)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.feature2D_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec compute(Evision.Feature2D.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t())) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def compute(self, image, keypoints) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.feature2D_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  defaultNorm

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  defaultNorm() -> retval
  ```
  """
  @spec defaultNorm(Evision.Feature2D.t()) :: integer() | {:error, String.t()}
  def defaultNorm(self) do
    positional = [
    ]
    :evision_nif.feature2D_defaultNorm(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorSize

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorSize() -> retval
  ```
  """
  @spec descriptorSize(Evision.Feature2D.t()) :: integer() | {:error, String.t()}
  def descriptorSize(self) do
    positional = [
    ]
    :evision_nif.feature2D_descriptorSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  descriptorType

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorType() -> retval
  ```
  """
  @spec descriptorType(Evision.Feature2D.t()) :: integer() | {:error, String.t()}
  def descriptorType(self) do
    positional = [
    ]
    :evision_nif.feature2D_descriptorType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`
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
  - **self**: `Evision.Feature2D.t()`
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
  @spec detect(Evision.Feature2D.t(), list(Evision.Mat.maybe_mat_in()), [{:masks, term()}] | nil) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images, opts) when is_list(images) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.feature2D_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detect(Evision.Feature2D.t(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.feature2D_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  detect

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`
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
  - **self**: `Evision.Feature2D.t()`
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
  @spec detect(Evision.Feature2D.t(), list(Evision.Mat.maybe_mat_in())) :: list(list(Evision.KeyPoint.t())) | {:error, String.t()}
  def detect(self, images) when is_list(images)
  do
    positional = [
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.feature2D_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detect(Evision.Feature2D.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.KeyPoint.t()) | {:error, String.t()}
  def detect(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.feature2D_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`
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
  @spec detectAndCompute(Evision.Feature2D.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:useProvidedKeypoints, term()}] | nil) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:useProvidedKeypoints])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.feature2D_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detectAndCompute

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`
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
  @spec detectAndCompute(Evision.Feature2D.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(Evision.KeyPoint.t()), Evision.Mat.t()} | {:error, String.t()}
  def detectAndCompute(self, image, mask) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      mask: Evision.Internal.Structurise.from_struct(mask)
    ]
    :evision_nif.feature2D_detectAndCompute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Feature2D.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.feature2D_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.Feature2D.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.feature2D_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  read

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`
  - **arg1**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(arg1) -> None
  ```
  #### Variant 2:
  read

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  read(fileName) -> None
  ```

  """
  @spec read(Evision.Feature2D.t(), Evision.FileNode.t()) :: Evision.Feature2D.t() | {:error, String.t()}
  def read(self, arg1) when is_struct(arg1, Evision.FileNode)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1)
    ]
    :evision_nif.feature2D_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec read(Evision.Feature2D.t(), binary()) :: Evision.Feature2D.t() | {:error, String.t()}
  def read(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.feature2D_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.Feature2D.t(), Evision.FileStorage.t(), binary()) :: Evision.Feature2D.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.feature2D_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.Feature2D.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  write(fileName) -> None
  ```
  """
  @spec write(Evision.Feature2D.t(), binary()) :: Evision.Feature2D.t() | {:error, String.t()}
  def write(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.feature2D_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
