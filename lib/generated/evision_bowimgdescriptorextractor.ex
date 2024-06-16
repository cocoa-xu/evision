defmodule Evision.BOWImgDescriptorExtractor do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `BOWImgDescriptorExtractor` struct.

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
  def to_struct({:ok, %{class: Evision.BOWImgDescriptorExtractor, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.BOWImgDescriptorExtractor, ref: ref}) do
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
  The constructor.

  ##### Positional Arguments
  - **dextractor**: `Evision.Feature2D`.

    Descriptor extractor that is used to compute descriptors for an input image and
    its keypoints.

  - **dmatcher**: `Evision.DescriptorMatcher`.

    Descriptor matcher that is used to find the nearest word of the trained vocabulary
    for each keypoint descriptor of the image.

  ##### Return
  - **self**: `Evision.BOWImgDescriptorExtractor.t()`

  Python prototype (for reference only):
  ```python3
  BOWImgDescriptorExtractor(dextractor, dmatcher) -> <BOWImgDescriptorExtractor object>
  ```
  """
  @spec bowImgDescriptorExtractor(Evision.Feature2D.t(), Evision.DescriptorMatcher.t()) :: Evision.BOWImgDescriptorExtractor.t() | {:error, String.t()}
  def bowImgDescriptorExtractor(dextractor, dmatcher) when is_struct(dmatcher, Evision.DescriptorMatcher)
  do
    positional = [
      dextractor: Evision.Internal.Structurise.from_struct(dextractor),
      dmatcher: Evision.Internal.Structurise.from_struct(dmatcher)
    ]
    :evision_nif.bowImgDescriptorExtractor_BOWImgDescriptorExtractor(positional)
    |> to_struct()
  end

  @doc """
  compute

  ##### Positional Arguments
  - **self**: `Evision.BOWImgDescriptorExtractor.t()`
  - **image**: `Evision.Mat`
  - **keypoints**: `[Evision.KeyPoint]`

  ##### Return
  - **imgDescriptor**: `Evision.Mat.t()`.

    Computed output image descriptor.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  compute(image, keypoints[, imgDescriptor]) -> imgDescriptor
  ```
  """
  @spec compute(Evision.BOWImgDescriptorExtractor.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t()), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, image, keypoints, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.bowImgDescriptorExtractor_compute(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  compute

  ##### Positional Arguments
  - **self**: `Evision.BOWImgDescriptorExtractor.t()`
  - **image**: `Evision.Mat`
  - **keypoints**: `[Evision.KeyPoint]`

  ##### Return
  - **imgDescriptor**: `Evision.Mat.t()`.

    Computed output image descriptor.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  compute(image, keypoints[, imgDescriptor]) -> imgDescriptor
  ```
  """
  @spec compute(Evision.BOWImgDescriptorExtractor.t(), Evision.Mat.maybe_mat_in(), list(Evision.KeyPoint.t())) :: Evision.Mat.t() | {:error, String.t()}
  def compute(self, image, keypoints) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keypoints)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keypoints: Evision.Internal.Structurise.from_struct(keypoints)
    ]
    :evision_nif.bowImgDescriptorExtractor_compute(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns an image descriptor size if the vocabulary is set. Otherwise, it returns 0.

  ##### Positional Arguments
  - **self**: `Evision.BOWImgDescriptorExtractor.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorSize() -> retval
  ```
  """
  @spec descriptorSize(Evision.BOWImgDescriptorExtractor.t()) :: integer() | {:error, String.t()}
  def descriptorSize(self) do
    positional = [
    ]
    :evision_nif.bowImgDescriptorExtractor_descriptorSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns an image descriptor type.

  ##### Positional Arguments
  - **self**: `Evision.BOWImgDescriptorExtractor.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  descriptorType() -> retval
  ```
  """
  @spec descriptorType(Evision.BOWImgDescriptorExtractor.t()) :: integer() | {:error, String.t()}
  def descriptorType(self) do
    positional = [
    ]
    :evision_nif.bowImgDescriptorExtractor_descriptorType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the set vocabulary.

  ##### Positional Arguments
  - **self**: `Evision.BOWImgDescriptorExtractor.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getVocabulary() -> retval
  ```
  """
  @spec getVocabulary(Evision.BOWImgDescriptorExtractor.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getVocabulary(self) do
    positional = [
    ]
    :evision_nif.bowImgDescriptorExtractor_getVocabulary(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets a visual vocabulary.

  ##### Positional Arguments
  - **self**: `Evision.BOWImgDescriptorExtractor.t()`
  - **vocabulary**: `Evision.Mat`.

    Vocabulary (can be trained using the inheritor of BOWTrainer ). Each row of the
    vocabulary is a visual word (cluster center).

  Python prototype (for reference only):
  ```python3
  setVocabulary(vocabulary) -> None
  ```
  """
  @spec setVocabulary(Evision.BOWImgDescriptorExtractor.t(), Evision.Mat.maybe_mat_in()) :: Evision.BOWImgDescriptorExtractor.t() | {:error, String.t()}
  def setVocabulary(self, vocabulary) when (is_struct(vocabulary, Evision.Mat) or is_struct(vocabulary, Nx.Tensor) or is_number(vocabulary) or is_tuple(vocabulary))
  do
    positional = [
      vocabulary: Evision.Internal.Structurise.from_struct(vocabulary)
    ]
    :evision_nif.bowImgDescriptorExtractor_setVocabulary(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
