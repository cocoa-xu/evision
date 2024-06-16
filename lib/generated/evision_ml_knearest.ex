defmodule Evision.ML.KNearest do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ML.KNearest` struct.

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
  def to_struct({:ok, %{class: Evision.ML.KNearest, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ML.KNearest, ref: ref}) do
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
  Computes error on the training or test dataset

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **data**: `Evision.ML.TrainData.t()`.

    the training data

  - **test**: `bool`.

    if true, the error is computed over the test subset of the data, otherwise it's
    computed over the training subset of the data. Please note that if you loaded a completely
    different dataset to evaluate already trained classifier, you will probably want not to set
    the test subset at all with TrainData::setTrainTestSplitRatio and specify test=false, so
    that the error is computed for the whole new set. Yes, this sounds a bit confusing.

  ##### Return
  - **retval**: `float`
  - **resp**: `Evision.Mat.t()`.

    the optional output responses.

  The method uses StatModel::predict to compute the error. For regression models the error is
  computed as RMS, for classifiers - as a percent of missclassified samples (0%-100%).

  Python prototype (for reference only):
  ```python3
  calcError(data, test[, resp]) -> retval, resp
  ```
  """
  @spec calcError(Evision.ML.KNearest.t(), Evision.ML.TrainData.t(), boolean(), [{atom(), term()},...] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test, opts) when is_struct(data, Evision.ML.TrainData) and is_boolean(test) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_KNearest_calcError(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes error on the training or test dataset

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **data**: `Evision.ML.TrainData.t()`.

    the training data

  - **test**: `bool`.

    if true, the error is computed over the test subset of the data, otherwise it's
    computed over the training subset of the data. Please note that if you loaded a completely
    different dataset to evaluate already trained classifier, you will probably want not to set
    the test subset at all with TrainData::setTrainTestSplitRatio and specify test=false, so
    that the error is computed for the whole new set. Yes, this sounds a bit confusing.

  ##### Return
  - **retval**: `float`
  - **resp**: `Evision.Mat.t()`.

    the optional output responses.

  The method uses StatModel::predict to compute the error. For regression models the error is
  computed as RMS, for classifiers - as a percent of missclassified samples (0%-100%).

  Python prototype (for reference only):
  ```python3
  calcError(data, test[, resp]) -> retval, resp
  ```
  """
  @spec calcError(Evision.ML.KNearest.t(), Evision.ML.TrainData.t(), boolean()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test) when is_struct(data, Evision.ML.TrainData) and is_boolean(test)
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_KNearest_calcError(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.ML.KNearest.t()) :: Evision.ML.KNearest.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ml_KNearest_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Creates the empty model
  ##### Return
  - **retval**: `Evision.ML.KNearest.t()`

  The static method creates empty %KNearest classifier. It should be then trained using StatModel::train method.

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.ML.KNearest.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.ml_ml_KNearest_create_static(positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.ML.KNearest.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ml_ml_KNearest_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Finds the neighbors and predicts responses for input vectors.

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **samples**: `Evision.Mat`.

    Input samples stored by rows. It is a single-precision floating-point matrix of
    `<number_of_samples> * k` size.

  - **k**: `integer()`.

    Number of used nearest neighbors. Should be greater than 1.

  ##### Return
  - **retval**: `float`
  - **results**: `Evision.Mat.t()`.

    Vector with results of prediction (regression or classification) for each input
    sample. It is a single-precision floating-point vector with `<number_of_samples>` elements.

  - **neighborResponses**: `Evision.Mat.t()`.

    Optional output values for corresponding neighbors. It is a single-
    precision floating-point matrix of `<number_of_samples> * k` size.

  - **dist**: `Evision.Mat.t()`.

    Optional output distances from the input vectors to the corresponding neighbors. It
    is a single-precision floating-point matrix of `<number_of_samples> * k` size.

  For each input vector (a row of the matrix samples), the method finds the k nearest neighbors.
  In case of regression, the predicted result is a mean value of the particular vector's neighbor
  responses. In case of classification, the class is determined by voting.
  For each input vector, the neighbors are sorted by their distances to the vector.
  In case of C++ interface you can use output pointers to empty matrices and the function will
  allocate memory itself.
  If only a single input vector is passed, all output matrices are optional and the predicted
  value is returned by the method.
  The function is parallelized with the TBB library.

  Python prototype (for reference only):
  ```python3
  findNearest(samples, k[, results[, neighborResponses[, dist]]]) -> retval, results, neighborResponses, dist
  ```
  """
  @spec findNearest(Evision.ML.KNearest.t(), Evision.Mat.maybe_mat_in(), integer(), [{atom(), term()},...] | nil) :: {number(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def findNearest(self, samples, k, opts) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and is_integer(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.ml_ml_KNearest_findNearest(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Finds the neighbors and predicts responses for input vectors.

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **samples**: `Evision.Mat`.

    Input samples stored by rows. It is a single-precision floating-point matrix of
    `<number_of_samples> * k` size.

  - **k**: `integer()`.

    Number of used nearest neighbors. Should be greater than 1.

  ##### Return
  - **retval**: `float`
  - **results**: `Evision.Mat.t()`.

    Vector with results of prediction (regression or classification) for each input
    sample. It is a single-precision floating-point vector with `<number_of_samples>` elements.

  - **neighborResponses**: `Evision.Mat.t()`.

    Optional output values for corresponding neighbors. It is a single-
    precision floating-point matrix of `<number_of_samples> * k` size.

  - **dist**: `Evision.Mat.t()`.

    Optional output distances from the input vectors to the corresponding neighbors. It
    is a single-precision floating-point matrix of `<number_of_samples> * k` size.

  For each input vector (a row of the matrix samples), the method finds the k nearest neighbors.
  In case of regression, the predicted result is a mean value of the particular vector's neighbor
  responses. In case of classification, the class is determined by voting.
  For each input vector, the neighbors are sorted by their distances to the vector.
  In case of C++ interface you can use output pointers to empty matrices and the function will
  allocate memory itself.
  If only a single input vector is passed, all output matrices are optional and the predicted
  value is returned by the method.
  The function is parallelized with the TBB library.

  Python prototype (for reference only):
  ```python3
  findNearest(samples, k[, results[, neighborResponses[, dist]]]) -> retval, results, neighborResponses, dist
  ```
  """
  @spec findNearest(Evision.ML.KNearest.t(), Evision.Mat.maybe_mat_in(), integer()) :: {number(), Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def findNearest(self, samples, k) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and is_integer(k)
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.ml_ml_KNearest_findNearest(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getAlgorithmType

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setAlgorithmType/2`

  Python prototype (for reference only):
  ```python3
  getAlgorithmType() -> retval
  ```
  """
  @spec getAlgorithmType(Evision.ML.KNearest.t()) :: integer() | {:error, String.t()}
  def getAlgorithmType(self) do
    positional = [
    ]
    :evision_nif.ml_ml_KNearest_getAlgorithmType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultK

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setDefaultK/2`

  Python prototype (for reference only):
  ```python3
  getDefaultK() -> retval
  ```
  """
  @spec getDefaultK(Evision.ML.KNearest.t()) :: integer() | {:error, String.t()}
  def getDefaultK(self) do
    positional = [
    ]
    :evision_nif.ml_ml_KNearest_getDefaultK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.ML.KNearest.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ml_KNearest_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getEmax

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setEmax/2`

  Python prototype (for reference only):
  ```python3
  getEmax() -> retval
  ```
  """
  @spec getEmax(Evision.ML.KNearest.t()) :: integer() | {:error, String.t()}
  def getEmax(self) do
    positional = [
    ]
    :evision_nif.ml_ml_KNearest_getEmax(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getIsClassifier

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`

  ##### Return
  - **retval**: `bool`

  @see `setIsClassifier/2`

  Python prototype (for reference only):
  ```python3
  getIsClassifier() -> retval
  ```
  """
  @spec getIsClassifier(Evision.ML.KNearest.t()) :: boolean() | {:error, String.t()}
  def getIsClassifier(self) do
    positional = [
    ]
    :evision_nif.ml_ml_KNearest_getIsClassifier(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the number of variables in training samples

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getVarCount() -> retval
  ```
  """
  @spec getVarCount(Evision.ML.KNearest.t()) :: integer() | {:error, String.t()}
  def getVarCount(self) do
    positional = [
    ]
    :evision_nif.ml_ml_KNearest_getVarCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is classifier

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isClassifier() -> retval
  ```
  """
  @spec isClassifier(Evision.ML.KNearest.t()) :: boolean() | {:error, String.t()}
  def isClassifier(self) do
    positional = [
    ]
    :evision_nif.ml_ml_KNearest_isClassifier(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is trained

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isTrained() -> retval
  ```
  """
  @spec isTrained(Evision.ML.KNearest.t()) :: boolean() | {:error, String.t()}
  def isTrained(self) do
    positional = [
    ]
    :evision_nif.ml_ml_KNearest_isTrained(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Loads and creates a serialized knearest from a file

  ##### Positional Arguments
  - **filepath**: `String`.

    path to serialized KNearest

  ##### Return
  - **retval**: `Evision.ML.KNearest.t()`

   Use KNearest::save to serialize and store an KNearest to disk.
   Load the KNearest from this file again, by calling this function with the path to the file.

  Python prototype (for reference only):
  ```python3
  load(filepath) -> retval
  ```
  """
  @spec load(binary()) :: Evision.ML.KNearest.t() | {:error, String.t()}
  def load(filepath) when is_binary(filepath)
  do
    positional = [
      filepath: Evision.Internal.Structurise.from_struct(filepath)
    ]
    :evision_nif.ml_ml_KNearest_load_static(positional)
    |> to_struct()
  end

  @doc """
  Predicts response(s) for the provided sample(s)

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **samples**: `Evision.Mat`.

    The input samples, floating-point matrix

  ##### Keyword Arguments
  - **flags**: `integer()`.

    The optional flags, model-dependent. See cv::ml::StatModel::Flags.

  ##### Return
  - **retval**: `float`
  - **results**: `Evision.Mat.t()`.

    The optional output matrix of results.

  Python prototype (for reference only):
  ```python3
  predict(samples[, results[, flags]]) -> retval, results
  ```
  """
  @spec predict(Evision.ML.KNearest.t(), Evision.Mat.maybe_mat_in(), [{:flags, term()}] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples, opts) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_KNearest_predict(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Predicts response(s) for the provided sample(s)

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **samples**: `Evision.Mat`.

    The input samples, floating-point matrix

  ##### Keyword Arguments
  - **flags**: `integer()`.

    The optional flags, model-dependent. See cv::ml::StatModel::Flags.

  ##### Return
  - **retval**: `float`
  - **results**: `Evision.Mat.t()`.

    The optional output matrix of results.

  Python prototype (for reference only):
  ```python3
  predict(samples[, results[, flags]]) -> retval, results
  ```
  """
  @spec predict(Evision.ML.KNearest.t(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_KNearest_predict(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.ML.KNearest.t(), Evision.FileNode.t()) :: Evision.ML.KNearest.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ml_KNearest_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.ML.KNearest.t(), binary()) :: Evision.ML.KNearest.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ml_KNearest_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setAlgorithmType

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **val**: `integer()`

  @see `getAlgorithmType/1`

  Python prototype (for reference only):
  ```python3
  setAlgorithmType(val) -> None
  ```
  """
  @spec setAlgorithmType(Evision.ML.KNearest.t(), integer()) :: Evision.ML.KNearest.t() | {:error, String.t()}
  def setAlgorithmType(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_KNearest_setAlgorithmType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setDefaultK

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **val**: `integer()`

  @see `getDefaultK/1`

  Python prototype (for reference only):
  ```python3
  setDefaultK(val) -> None
  ```
  """
  @spec setDefaultK(Evision.ML.KNearest.t(), integer()) :: Evision.ML.KNearest.t() | {:error, String.t()}
  def setDefaultK(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_KNearest_setDefaultK(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setEmax

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **val**: `integer()`

  @see `getEmax/1`

  Python prototype (for reference only):
  ```python3
  setEmax(val) -> None
  ```
  """
  @spec setEmax(Evision.ML.KNearest.t(), integer()) :: Evision.ML.KNearest.t() | {:error, String.t()}
  def setEmax(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_KNearest_setEmax(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setIsClassifier

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **val**: `bool`

  @see `getIsClassifier/1`

  Python prototype (for reference only):
  ```python3
  setIsClassifier(val) -> None
  ```
  """
  @spec setIsClassifier(Evision.ML.KNearest.t(), boolean()) :: Evision.ML.KNearest.t() | {:error, String.t()}
  def setIsClassifier(self, val) when is_boolean(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_KNearest_setIsClassifier(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **samples**: `Evision.Mat`.

    training samples

  - **layout**: `integer()`.

    See ml::SampleTypes.

  - **responses**: `Evision.Mat`.

    vector of responses associated with the training samples.

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  train(samples, layout, responses) -> retval
  ```
  """
  @spec train(Evision.ML.KNearest.t(), Evision.Mat.maybe_mat_in(), integer(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def train(self, samples, layout, responses) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and is_integer(layout) and (is_struct(responses, Evision.Mat) or is_struct(responses, Nx.Tensor) or is_number(responses) or is_tuple(responses))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      layout: Evision.Internal.Structurise.from_struct(layout),
      responses: Evision.Internal.Structurise.from_struct(responses)
    ]
    :evision_nif.ml_ml_KNearest_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **trainData**: `Evision.ML.TrainData.t()`.

    training data that can be loaded from file using TrainData::loadFromCSV or
    created with TrainData::create.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    optional flags, depending on the model. Some of the models can be updated with the
    new training samples, not completely overwritten (such as NormalBayesClassifier or ANN_MLP).

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  train(trainData[, flags]) -> retval
  ```
  """
  @spec train(Evision.ML.KNearest.t(), Evision.ML.TrainData.t(), [{:flags, term()}] | nil) :: boolean() | {:error, String.t()}
  def train(self, trainData, opts) when is_struct(trainData, Evision.ML.TrainData) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_KNearest_train(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **trainData**: `Evision.ML.TrainData.t()`.

    training data that can be loaded from file using TrainData::loadFromCSV or
    created with TrainData::create.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    optional flags, depending on the model. Some of the models can be updated with the
    new training samples, not completely overwritten (such as NormalBayesClassifier or ANN_MLP).

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  train(trainData[, flags]) -> retval
  ```
  """
  @spec train(Evision.ML.KNearest.t(), Evision.ML.TrainData.t()) :: boolean() | {:error, String.t()}
  def train(self, trainData) when is_struct(trainData, Evision.ML.TrainData)
  do
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_KNearest_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.ML.KNearest.t(), Evision.FileStorage.t(), binary()) :: Evision.ML.KNearest.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ml_KNearest_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.KNearest.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.ML.KNearest.t(), Evision.FileStorage.t()) :: Evision.ML.KNearest.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ml_KNearest_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
