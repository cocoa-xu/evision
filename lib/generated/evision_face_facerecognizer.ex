defmodule Evision.Face.FaceRecognizer do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Face.FaceRecognizer` struct.

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
  def to_struct({:ok, %{class: Evision.Face.FaceRecognizer, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Face.FaceRecognizer, ref: ref}) do
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
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.Face.FaceRecognizer.t()) :: Evision.Face.FaceRecognizer.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.face_FaceRecognizer_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.Face.FaceRecognizer.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.face_FaceRecognizer_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.Face.FaceRecognizer.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.face_FaceRecognizer_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Gets string information by label.

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`
  - **label**: `integer()`

  ##### Return
  - **retval**: `String`

  If an unknown label id is provided or there is no label information associated with the specified
  label id the method returns an empty string.

  Python prototype (for reference only):
  ```python3
  getLabelInfo(label) -> retval
  ```
  """
  @spec getLabelInfo(Evision.Face.FaceRecognizer.t(), integer()) :: binary() | {:error, String.t()}
  def getLabelInfo(self, label) when is_integer(label)
  do
    positional = [
      label: Evision.Internal.Structurise.from_struct(label)
    ]
    :evision_nif.face_face_FaceRecognizer_getLabelInfo(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Gets vector of labels by string.

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`
  - **str**: `String`

  ##### Return
  - **retval**: `[integer()]`

  The function searches for the labels containing the specified sub-string in the associated string
  info.

  Python prototype (for reference only):
  ```python3
  getLabelsByString(str) -> retval
  ```
  """
  @spec getLabelsByString(Evision.Face.FaceRecognizer.t(), binary()) :: list(integer()) | {:error, String.t()}
  def getLabelsByString(self, str) when is_binary(str)
  do
    positional = [
      str: Evision.Internal.Structurise.from_struct(str)
    ]
    :evision_nif.face_face_FaceRecognizer_getLabelsByString(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Predicts a label and associated confidence (e.g. distance) for a given input image.

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`
  - **src**: `Evision.Mat`.

    Sample image to get a prediction from.

  ##### Return
  - **label**: `integer()`.

    The predicted label for the given image.

  - **confidence**: `double`.

    Associated confidence (e.g. distance) for the predicted label.

  The suffix const means that prediction does not affect the internal model state, so the method can
  be safely called from within different threads.
  The following example shows how to get a prediction from a trained model:
  ```
  using namespace cv;
  // Do your initialization here (create the cv::FaceRecognizer model) ...
  // ...
  // Read in a sample image:
  Mat img = imread("person1/3.jpg", IMREAD_GRAYSCALE);
  // And get a prediction from the cv::FaceRecognizer:
  int predicted = model->predict(img);
  ```
  Or to get a prediction and the associated confidence (e.g. distance):
  ```
  using namespace cv;
  // Do your initialization here (create the cv::FaceRecognizer model) ...
  // ...
  Mat img = imread("person1/3.jpg", IMREAD_GRAYSCALE);
  // Some variables for the predicted label and associated confidence (e.g. distance):
  int predicted_label = -1;
  double predicted_confidence = 0.0;
  // Get the prediction and associated confidence from the model
  model->predict(img, predicted_label, predicted_confidence);
  ```

  Python prototype (for reference only):
  ```python3
  predict(src) -> label, confidence
  ```
  """
  @spec predict(Evision.Face.FaceRecognizer.t(), Evision.Mat.maybe_mat_in()) :: {integer(), number()} | {:error, String.t()}
  def predict(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.face_face_FaceRecognizer_predict(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  - if implemented - send all result of prediction to collector that can be used for somehow custom result handling

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`
  - **src**: `Evision.Mat`.

    Sample image to get a prediction from.

  - **collector**: `PredictCollector`.

    User-defined collector object that accepts all results

  To implement this method u just have to do same internal cycle as in predict(InputArray src, CV_OUT int &label, CV_OUT double &confidence) but
  not try to get "best@ result, just resend it to caller side with given collector

  Python prototype (for reference only):
  ```python3
  predict_collect(src, collector) -> None
  ```
  """
  @spec predict_collect(Evision.Face.FaceRecognizer.t(), Evision.Mat.maybe_mat_in(), Evision.Face.PredictCollector.t()) :: Evision.Face.FaceRecognizer.t() | {:error, String.t()}
  def predict_collect(self, src, collector) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_struct(collector, Evision.Face.PredictCollector)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      collector: Evision.Internal.Structurise.from_struct(collector)
    ]
    :evision_nif.face_face_FaceRecognizer_predict_collect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  predict_label

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`
  - **src**: `Evision.Mat`

  ##### Return
  - **retval**: `integer()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  predict_label(src) -> retval
  ```
  """
  @spec predict_label(Evision.Face.FaceRecognizer.t(), Evision.Mat.maybe_mat_in()) :: integer() | {:error, String.t()}
  def predict_label(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.face_face_FaceRecognizer_predict_label(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Loads a FaceRecognizer and its model state.

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`
  - **filename**: `String`

  Loads a persisted model and state from a given XML or YAML file . Every FaceRecognizer has to
  overwrite FaceRecognizer::load(FileStorage& fs) to enable loading the model state.
  FaceRecognizer::load(FileStorage& fs) in turn gets called by
  FaceRecognizer::load(const String& filename), to ease saving a model.

  Python prototype (for reference only):
  ```python3
  read(filename) -> None
  ```
  """
  @spec read(Evision.Face.FaceRecognizer.t(), binary()) :: Evision.Face.FaceRecognizer.t() | {:error, String.t()}
  def read(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.face_face_FaceRecognizer_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.Face.FaceRecognizer.t(), binary()) :: Evision.Face.FaceRecognizer.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.face_FaceRecognizer_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets string info for the specified model's label.

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`
  - **label**: `integer()`
  - **strInfo**: `String`

  The string info is replaced by the provided value if it was set before for the specified label.

  Python prototype (for reference only):
  ```python3
  setLabelInfo(label, strInfo) -> None
  ```
  """
  @spec setLabelInfo(Evision.Face.FaceRecognizer.t(), integer(), binary()) :: Evision.Face.FaceRecognizer.t() | {:error, String.t()}
  def setLabelInfo(self, label, strInfo) when is_integer(label) and is_binary(strInfo)
  do
    positional = [
      label: Evision.Internal.Structurise.from_struct(label),
      strInfo: Evision.Internal.Structurise.from_struct(strInfo)
    ]
    :evision_nif.face_face_FaceRecognizer_setLabelInfo(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains a FaceRecognizer with given data and associated labels.

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`
  - **src**: `[Evision.Mat]`.

    The training images, that means the faces you want to learn. The data has to be
    given as a vector\\<Mat\\>.

  - **labels**: `Evision.Mat`.

    The labels corresponding to the images have to be given either as a vector\\<int\\>
    or a Mat of type CV_32SC1.

  The following source code snippet shows you how to learn a Fisherfaces model on a given set of
  images. The images are read with imread and pushed into a std::vector\\<Mat\\>. The labels of each
  image are stored within a std::vector\\<int\\> (you could also use a Mat of type CV_32SC1). Think of
  the label as the subject (the person) this image belongs to, so same subjects (persons) should have
  the same label. For the available FaceRecognizer you don't have to pay any attention to the order of
  the labels, just make sure same persons have the same label:
  ```
  // holds images and labels
  vector<Mat> images;
  vector<int> labels;
  // using Mat of type CV_32SC1
  // Mat labels(number_of_samples, 1, CV_32SC1);
  // images for first person
  images.push_back(imread("person0/0.jpg", IMREAD_GRAYSCALE)); labels.push_back(0);
  images.push_back(imread("person0/1.jpg", IMREAD_GRAYSCALE)); labels.push_back(0);
  images.push_back(imread("person0/2.jpg", IMREAD_GRAYSCALE)); labels.push_back(0);
  // images for second person
  images.push_back(imread("person1/0.jpg", IMREAD_GRAYSCALE)); labels.push_back(1);
  images.push_back(imread("person1/1.jpg", IMREAD_GRAYSCALE)); labels.push_back(1);
  images.push_back(imread("person1/2.jpg", IMREAD_GRAYSCALE)); labels.push_back(1);
  ```
  Now that you have read some images, we can create a new FaceRecognizer. In this example I'll create
  a Fisherfaces model and decide to keep all of the possible Fisherfaces:
  ```
  // Create a new Fisherfaces model and retain all available Fisherfaces,
  // this is the most common usage of this specific FaceRecognizer:
  //
  Ptr<FaceRecognizer> model =  FisherFaceRecognizer::create();
  ```
  And finally train it on the given dataset (the face images and labels):
  ```
  // This is the common interface to train all of the available cv::FaceRecognizer
  // implementations:
  //
  model->train(images, labels);
  ```

  Python prototype (for reference only):
  ```python3
  train(src, labels) -> None
  ```
  """
  @spec train(Evision.Face.FaceRecognizer.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in()) :: Evision.Face.FaceRecognizer.t() | {:error, String.t()}
  def train(self, src, labels) when is_list(src) and (is_struct(labels, Evision.Mat) or is_struct(labels, Nx.Tensor) or is_number(labels) or is_tuple(labels))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      labels: Evision.Internal.Structurise.from_struct(labels)
    ]
    :evision_nif.face_face_FaceRecognizer_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Updates a FaceRecognizer with given data and associated labels.

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`
  - **src**: `[Evision.Mat]`.

    The training images, that means the faces you want to learn. The data has to be given
    as a vector\\<Mat\\>.

  - **labels**: `Evision.Mat`.

    The labels corresponding to the images have to be given either as a vector\\<int\\> or
    a Mat of type CV_32SC1.

  This method updates a (probably trained) FaceRecognizer, but only if the algorithm supports it. The
  Local Binary Patterns Histograms (LBPH) recognizer (see createLBPHFaceRecognizer) can be updated.
  For the Eigenfaces and Fisherfaces method, this is algorithmically not possible and you have to
  re-estimate the model with FaceRecognizer::train. In any case, a call to train empties the existing
  model and learns a new model, while update does not delete any model data.
  ```
  // Create a new LBPH model (it can be updated) and use the default parameters,
  // this is the most common usage of this specific FaceRecognizer:
  //
  Ptr<FaceRecognizer> model =  LBPHFaceRecognizer::create();
  // This is the common interface to train all of the available cv::FaceRecognizer
  // implementations:
  //
  model->train(images, labels);
  // Some containers to hold new image:
  vector<Mat> newImages;
  vector<int> newLabels;
  // You should add some images to the containers:
  //
  // ...
  //
  // Now updating the model is as easy as calling:
  model->update(newImages,newLabels);
  // This will preserve the old model data and extend the existing model
  // with the new features extracted from newImages!
  ```
  Calling update on an Eigenfaces model (see EigenFaceRecognizer::create), which doesn't support
  updating, will throw an error similar to:
  ```
  OpenCV Error: The function/feature is not implemented (This FaceRecognizer (FaceRecognizer.Eigenfaces) does not support updating, you have to use FaceRecognizer::train to update it.) in update, file /home/philipp/git/opencv/modules/contrib/src/facerec.cpp, line 305
  terminate called after throwing an instance of 'cv::Exception'
  ```
  **Note**: The FaceRecognizer does not store your training images, because this would be very
  memory intense and it's not the responsibility of te FaceRecognizer to do so. The caller is
  responsible for maintaining the dataset, he want to work with.

  Python prototype (for reference only):
  ```python3
  update(src, labels) -> None
  ```
  """
  @spec update(Evision.Face.FaceRecognizer.t(), list(Evision.Mat.maybe_mat_in()), Evision.Mat.maybe_mat_in()) :: Evision.Face.FaceRecognizer.t() | {:error, String.t()}
  def update(self, src, labels) when is_list(src) and (is_struct(labels, Evision.Mat) or is_struct(labels, Nx.Tensor) or is_number(labels) or is_tuple(labels))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      labels: Evision.Internal.Structurise.from_struct(labels)
    ]
    :evision_nif.face_face_FaceRecognizer_update(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Saves a FaceRecognizer and its model state.

  ##### Positional Arguments
  - **self**: `Evision.Face.FaceRecognizer.t()`
  - **filename**: `String`.

    The filename to store this FaceRecognizer to (either XML/YAML).

  Saves this model to a given filename, either as XML or YAML.

  Every FaceRecognizer overwrites FaceRecognizer::save(FileStorage& fs) to save the internal model
  state. FaceRecognizer::save(const String& filename) saves the state of a model to the given
  filename.
  The suffix const means that prediction does not affect the internal model state, so the method can
  be safely called from within different threads.

  Python prototype (for reference only):
  ```python3
  write(filename) -> None
  ```
  """
  @spec write(Evision.Face.FaceRecognizer.t(), binary()) :: Evision.Face.FaceRecognizer.t() | {:error, String.t()}
  def write(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.face_face_FaceRecognizer_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
