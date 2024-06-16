defmodule Evision.CascadeClassifier do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CascadeClassifier` struct.

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
  def to_struct({:ok, %{class: Evision.CascadeClassifier, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CascadeClassifier, ref: ref}) do
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
  Loads a classifier from a file.

  ##### Positional Arguments
  - **filename**: `String`.

    Name of the file from which the classifier is loaded.

  ##### Return
  - **self**: `Evision.CascadeClassifier.t()`

  Python prototype (for reference only):
  ```python3
  CascadeClassifier(filename) -> <CascadeClassifier object>
  ```
  """
  @spec cascadeClassifier(binary()) :: Evision.CascadeClassifier.t() | {:error, String.t()}
  def cascadeClassifier(filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cascadeClassifier_CascadeClassifier(positional)
    |> to_struct()
  end

  @doc """
  CascadeClassifier
  ##### Return
  - **self**: `Evision.CascadeClassifier.t()`

  Python prototype (for reference only):
  ```python3
  CascadeClassifier() -> <CascadeClassifier object>
  ```
  """
  @spec cascadeClassifier() :: Evision.CascadeClassifier.t() | {:error, String.t()}
  def cascadeClassifier() do
    positional = [
    ]
    :evision_nif.cascadeClassifier_CascadeClassifier(positional)
    |> to_struct()
  end

  @doc """
  convert

  ##### Positional Arguments
  - **oldcascade**: `String`
  - **newcascade**: `String`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  convert(oldcascade, newcascade) -> retval
  ```
  """
  @spec convert(binary(), binary()) :: boolean() | {:error, String.t()}
  def convert(oldcascade, newcascade) when is_binary(oldcascade) and is_binary(newcascade)
  do
    positional = [
      oldcascade: Evision.Internal.Structurise.from_struct(oldcascade),
      newcascade: Evision.Internal.Structurise.from_struct(newcascade)
    ]
    :evision_nif.cascadeClassifier_convert_static(positional)
    |> to_struct()
  end

  @doc """
  Detects objects of different sizes in the input image. The detected objects are returned as a list
  of rectangles.

  ##### Positional Arguments
  - **self**: `Evision.CascadeClassifier.t()`
  - **image**: `Evision.Mat`.

    Matrix of the type CV_8U containing an image where objects are detected.

  ##### Keyword Arguments
  - **scaleFactor**: `double`.

    Parameter specifying how much the image size is reduced at each image scale.

  - **minNeighbors**: `integer()`.

    Parameter specifying how many neighbors each candidate rectangle should have
    to retain it.

  - **flags**: `integer()`.

    Parameter with the same meaning for an old cascade as in the function
    cvHaarDetectObjects. It is not used for a new cascade.

  - **minSize**: `Size`.

    Minimum possible object size. Objects smaller than that are ignored.

  - **maxSize**: `Size`.

    Maximum possible object size. Objects larger than that are ignored. If `maxSize == minSize` model is evaluated on single scale.

  ##### Return
  - **objects**: `[Rect]`.

    Vector of rectangles where each rectangle contains the detected object, the
    rectangles may be partially outside the original image.

  Python prototype (for reference only):
  ```python3
  detectMultiScale(image[, scaleFactor[, minNeighbors[, flags[, minSize[, maxSize]]]]]) -> objects
  ```
  """
  @spec detectMultiScale(Evision.CascadeClassifier.t(), Evision.Mat.maybe_mat_in(), [{:flags, term()} | {:maxSize, term()} | {:minNeighbors, term()} | {:minSize, term()} | {:scaleFactor, term()}] | nil) :: list({number(), number(), number(), number()}) | {:error, String.t()}
  def detectMultiScale(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags, :maxSize, :minNeighbors, :minSize, :scaleFactor])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cascadeClassifier_detectMultiScale(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Detects objects of different sizes in the input image. The detected objects are returned as a list
  of rectangles.

  ##### Positional Arguments
  - **self**: `Evision.CascadeClassifier.t()`
  - **image**: `Evision.Mat`.

    Matrix of the type CV_8U containing an image where objects are detected.

  ##### Keyword Arguments
  - **scaleFactor**: `double`.

    Parameter specifying how much the image size is reduced at each image scale.

  - **minNeighbors**: `integer()`.

    Parameter specifying how many neighbors each candidate rectangle should have
    to retain it.

  - **flags**: `integer()`.

    Parameter with the same meaning for an old cascade as in the function
    cvHaarDetectObjects. It is not used for a new cascade.

  - **minSize**: `Size`.

    Minimum possible object size. Objects smaller than that are ignored.

  - **maxSize**: `Size`.

    Maximum possible object size. Objects larger than that are ignored. If `maxSize == minSize` model is evaluated on single scale.

  ##### Return
  - **objects**: `[Rect]`.

    Vector of rectangles where each rectangle contains the detected object, the
    rectangles may be partially outside the original image.

  Python prototype (for reference only):
  ```python3
  detectMultiScale(image[, scaleFactor[, minNeighbors[, flags[, minSize[, maxSize]]]]]) -> objects
  ```
  """
  @spec detectMultiScale(Evision.CascadeClassifier.t(), Evision.Mat.maybe_mat_in()) :: list({number(), number(), number(), number()}) | {:error, String.t()}
  def detectMultiScale(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cascadeClassifier_detectMultiScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detectMultiScale2

  ##### Positional Arguments
  - **self**: `Evision.CascadeClassifier.t()`
  - **image**: `Evision.Mat`.

    Matrix of the type CV_8U containing an image where objects are detected.

  ##### Keyword Arguments
  - **scaleFactor**: `double`.

    Parameter specifying how much the image size is reduced at each image scale.

  - **minNeighbors**: `integer()`.

    Parameter specifying how many neighbors each candidate rectangle should have
    to retain it.

  - **flags**: `integer()`.

    Parameter with the same meaning for an old cascade as in the function
    cvHaarDetectObjects. It is not used for a new cascade.

  - **minSize**: `Size`.

    Minimum possible object size. Objects smaller than that are ignored.

  - **maxSize**: `Size`.

    Maximum possible object size. Objects larger than that are ignored. If `maxSize == minSize` model is evaluated on single scale.

  ##### Return
  - **objects**: `[Rect]`.

    Vector of rectangles where each rectangle contains the detected object, the
    rectangles may be partially outside the original image.

  - **numDetections**: `[integer()]`.

    Vector of detection numbers for the corresponding objects. An object's number
    of detections is the number of neighboring positively classified rectangles that were joined
    together to form the object.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  detectMultiScale2(image[, scaleFactor[, minNeighbors[, flags[, minSize[, maxSize]]]]]) -> objects, numDetections
  ```
  """
  @spec detectMultiScale2(Evision.CascadeClassifier.t(), Evision.Mat.maybe_mat_in(), [{:flags, term()} | {:maxSize, term()} | {:minNeighbors, term()} | {:minSize, term()} | {:scaleFactor, term()}] | nil) :: {list({number(), number(), number(), number()}), list(integer())} | {:error, String.t()}
  def detectMultiScale2(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags, :maxSize, :minNeighbors, :minSize, :scaleFactor])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cascadeClassifier_detectMultiScale2(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detectMultiScale2

  ##### Positional Arguments
  - **self**: `Evision.CascadeClassifier.t()`
  - **image**: `Evision.Mat`.

    Matrix of the type CV_8U containing an image where objects are detected.

  ##### Keyword Arguments
  - **scaleFactor**: `double`.

    Parameter specifying how much the image size is reduced at each image scale.

  - **minNeighbors**: `integer()`.

    Parameter specifying how many neighbors each candidate rectangle should have
    to retain it.

  - **flags**: `integer()`.

    Parameter with the same meaning for an old cascade as in the function
    cvHaarDetectObjects. It is not used for a new cascade.

  - **minSize**: `Size`.

    Minimum possible object size. Objects smaller than that are ignored.

  - **maxSize**: `Size`.

    Maximum possible object size. Objects larger than that are ignored. If `maxSize == minSize` model is evaluated on single scale.

  ##### Return
  - **objects**: `[Rect]`.

    Vector of rectangles where each rectangle contains the detected object, the
    rectangles may be partially outside the original image.

  - **numDetections**: `[integer()]`.

    Vector of detection numbers for the corresponding objects. An object's number
    of detections is the number of neighboring positively classified rectangles that were joined
    together to form the object.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  detectMultiScale2(image[, scaleFactor[, minNeighbors[, flags[, minSize[, maxSize]]]]]) -> objects, numDetections
  ```
  """
  @spec detectMultiScale2(Evision.CascadeClassifier.t(), Evision.Mat.maybe_mat_in()) :: {list({number(), number(), number(), number()}), list(integer())} | {:error, String.t()}
  def detectMultiScale2(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cascadeClassifier_detectMultiScale2(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  detectMultiScale3

  ##### Positional Arguments
  - **self**: `Evision.CascadeClassifier.t()`
  - **image**: `Evision.Mat`

  ##### Keyword Arguments
  - **scaleFactor**: `double`.
  - **minNeighbors**: `integer()`.
  - **flags**: `integer()`.
  - **minSize**: `Size`.
  - **maxSize**: `Size`.
  - **outputRejectLevels**: `bool`.

  ##### Return
  - **objects**: `[Rect]`
  - **rejectLevels**: `[integer()]`
  - **levelWeights**: `[double]`

  Has overloading in C++

  This function allows you to retrieve the final stage decision certainty of classification.
  For this, one needs to set `outputRejectLevels` on true and provide the `rejectLevels` and `levelWeights` parameter.
  For each resulting detection, `levelWeights` will then contain the certainty of classification at the final stage.
  This value can then be used to separate strong from weaker classifications.
  A code sample on how to use it efficiently can be found below:
  ```
  Mat img;
  vector<double> weights;
  vector<int> levels;
  vector<Rect> detections;
  CascadeClassifier model("/path/to/your/model.xml");
  model.detectMultiScale(img, detections, levels, weights, 1.1, 3, 0, Size(), Size(), true);
  cerr << "Detection " << detections[0] << " with weight " << weights[0] << endl;
  ```

  Python prototype (for reference only):
  ```python3
  detectMultiScale3(image[, scaleFactor[, minNeighbors[, flags[, minSize[, maxSize[, outputRejectLevels]]]]]]) -> objects, rejectLevels, levelWeights
  ```
  """
  @spec detectMultiScale3(Evision.CascadeClassifier.t(), Evision.Mat.maybe_mat_in(), [{:flags, term()} | {:maxSize, term()} | {:minNeighbors, term()} | {:minSize, term()} | {:outputRejectLevels, term()} | {:scaleFactor, term()}] | nil) :: {list({number(), number(), number(), number()}), list(integer()), list(number())} | {:error, String.t()}
  def detectMultiScale3(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags, :maxSize, :minNeighbors, :minSize, :outputRejectLevels, :scaleFactor])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cascadeClassifier_detectMultiScale3(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  detectMultiScale3

  ##### Positional Arguments
  - **self**: `Evision.CascadeClassifier.t()`
  - **image**: `Evision.Mat`

  ##### Keyword Arguments
  - **scaleFactor**: `double`.
  - **minNeighbors**: `integer()`.
  - **flags**: `integer()`.
  - **minSize**: `Size`.
  - **maxSize**: `Size`.
  - **outputRejectLevels**: `bool`.

  ##### Return
  - **objects**: `[Rect]`
  - **rejectLevels**: `[integer()]`
  - **levelWeights**: `[double]`

  Has overloading in C++

  This function allows you to retrieve the final stage decision certainty of classification.
  For this, one needs to set `outputRejectLevels` on true and provide the `rejectLevels` and `levelWeights` parameter.
  For each resulting detection, `levelWeights` will then contain the certainty of classification at the final stage.
  This value can then be used to separate strong from weaker classifications.
  A code sample on how to use it efficiently can be found below:
  ```
  Mat img;
  vector<double> weights;
  vector<int> levels;
  vector<Rect> detections;
  CascadeClassifier model("/path/to/your/model.xml");
  model.detectMultiScale(img, detections, levels, weights, 1.1, 3, 0, Size(), Size(), true);
  cerr << "Detection " << detections[0] << " with weight " << weights[0] << endl;
  ```

  Python prototype (for reference only):
  ```python3
  detectMultiScale3(image[, scaleFactor[, minNeighbors[, flags[, minSize[, maxSize[, outputRejectLevels]]]]]]) -> objects, rejectLevels, levelWeights
  ```
  """
  @spec detectMultiScale3(Evision.CascadeClassifier.t(), Evision.Mat.maybe_mat_in()) :: {list({number(), number(), number(), number()}), list(integer()), list(number())} | {:error, String.t()}
  def detectMultiScale3(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cascadeClassifier_detectMultiScale3(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Checks whether the classifier has been loaded.

  ##### Positional Arguments
  - **self**: `Evision.CascadeClassifier.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CascadeClassifier.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cascadeClassifier_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getFeatureType

  ##### Positional Arguments
  - **self**: `Evision.CascadeClassifier.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getFeatureType() -> retval
  ```
  """
  @spec getFeatureType(Evision.CascadeClassifier.t()) :: integer() | {:error, String.t()}
  def getFeatureType(self) do
    positional = [
    ]
    :evision_nif.cascadeClassifier_getFeatureType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getOriginalWindowSize

  ##### Positional Arguments
  - **self**: `Evision.CascadeClassifier.t()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  getOriginalWindowSize() -> retval
  ```
  """
  @spec getOriginalWindowSize(Evision.CascadeClassifier.t()) :: {number(), number()} | {:error, String.t()}
  def getOriginalWindowSize(self) do
    positional = [
    ]
    :evision_nif.cascadeClassifier_getOriginalWindowSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  isOldFormatCascade

  ##### Positional Arguments
  - **self**: `Evision.CascadeClassifier.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isOldFormatCascade() -> retval
  ```
  """
  @spec isOldFormatCascade(Evision.CascadeClassifier.t()) :: boolean() | {:error, String.t()}
  def isOldFormatCascade(self) do
    positional = [
    ]
    :evision_nif.cascadeClassifier_isOldFormatCascade(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Loads a classifier from a file.

  ##### Positional Arguments
  - **self**: `Evision.CascadeClassifier.t()`
  - **filename**: `String`.

    Name of the file from which the classifier is loaded. The file may contain an old
    HAAR classifier trained by the haartraining application or a new cascade classifier trained by the
    traincascade application.

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  load(filename) -> retval
  ```
  """
  @spec load(Evision.CascadeClassifier.t(), binary()) :: boolean() | {:error, String.t()}
  def load(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cascadeClassifier_load(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads a classifier from a FileStorage node.

  ##### Positional Arguments
  - **self**: `Evision.CascadeClassifier.t()`
  - **node**: `Evision.FileNode`

  ##### Return
  - **retval**: `bool`

  **Note**: The file may contain a new cascade classifier (trained by the traincascade application) only.

  Python prototype (for reference only):
  ```python3
  read(node) -> retval
  ```
  """
  @spec read(Evision.CascadeClassifier.t(), Evision.FileNode.t()) :: boolean() | {:error, String.t()}
  def read(self, node) when is_struct(node, Evision.FileNode)
  do
    positional = [
      node: Evision.Internal.Structurise.from_struct(node)
    ]
    :evision_nif.cascadeClassifier_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
