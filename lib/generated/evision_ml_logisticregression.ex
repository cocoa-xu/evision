defmodule Evision.ML.LogisticRegression do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ML.LogisticRegression` struct.

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
  def to_struct({:ok, %{class: Evision.ML.LogisticRegression, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ML.LogisticRegression, ref: ref}) do
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
  - **self**: `Evision.ML.LogisticRegression.t()`
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
  @spec calcError(Evision.ML.LogisticRegression.t(), Evision.ML.TrainData.t(), boolean(), [{atom(), term()},...] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test, opts) when is_struct(data, Evision.ML.TrainData) and is_boolean(test) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_LogisticRegression_calcError(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes error on the training or test dataset

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
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
  @spec calcError(Evision.ML.LogisticRegression.t(), Evision.ML.TrainData.t(), boolean()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test) when is_struct(data, Evision.ML.TrainData) and is_boolean(test)
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_LogisticRegression_calcError(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.ML.LogisticRegression.t()) :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ml_LogisticRegression_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Creates empty model.
  ##### Return
  - **retval**: `Evision.ML.LogisticRegression.t()`

  Creates Logistic Regression model with parameters given.

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.ml_ml_LogisticRegression_create_static(positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.ML.LogisticRegression.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ml_ml_LogisticRegression_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.ML.LogisticRegression.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ml_LogisticRegression_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getIterations

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setIterations/2`

  Python prototype (for reference only):
  ```python3
  getIterations() -> retval
  ```
  """
  @spec getIterations(Evision.ML.LogisticRegression.t()) :: integer() | {:error, String.t()}
  def getIterations(self) do
    positional = [
    ]
    :evision_nif.ml_ml_LogisticRegression_getIterations(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getLearningRate

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`

  ##### Return
  - **retval**: `double`

  @see `setLearningRate/2`

  Python prototype (for reference only):
  ```python3
  getLearningRate() -> retval
  ```
  """
  @spec getLearningRate(Evision.ML.LogisticRegression.t()) :: number() | {:error, String.t()}
  def getLearningRate(self) do
    positional = [
    ]
    :evision_nif.ml_ml_LogisticRegression_getLearningRate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMiniBatchSize

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setMiniBatchSize/2`

  Python prototype (for reference only):
  ```python3
  getMiniBatchSize() -> retval
  ```
  """
  @spec getMiniBatchSize(Evision.ML.LogisticRegression.t()) :: integer() | {:error, String.t()}
  def getMiniBatchSize(self) do
    positional = [
    ]
    :evision_nif.ml_ml_LogisticRegression_getMiniBatchSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRegularization

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setRegularization/2`

  Python prototype (for reference only):
  ```python3
  getRegularization() -> retval
  ```
  """
  @spec getRegularization(Evision.ML.LogisticRegression.t()) :: integer() | {:error, String.t()}
  def getRegularization(self) do
    positional = [
    ]
    :evision_nif.ml_ml_LogisticRegression_getRegularization(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTermCriteria

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`

  ##### Return
  - **retval**: `TermCriteria`

  @see `setTermCriteria/2`

  Python prototype (for reference only):
  ```python3
  getTermCriteria() -> retval
  ```
  """
  @spec getTermCriteria(Evision.ML.LogisticRegression.t()) :: {integer(), integer(), number()} | {:error, String.t()}
  def getTermCriteria(self) do
    positional = [
    ]
    :evision_nif.ml_ml_LogisticRegression_getTermCriteria(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTrainMethod

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setTrainMethod/2`

  Python prototype (for reference only):
  ```python3
  getTrainMethod() -> retval
  ```
  """
  @spec getTrainMethod(Evision.ML.LogisticRegression.t()) :: integer() | {:error, String.t()}
  def getTrainMethod(self) do
    positional = [
    ]
    :evision_nif.ml_ml_LogisticRegression_getTrainMethod(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the number of variables in training samples

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getVarCount() -> retval
  ```
  """
  @spec getVarCount(Evision.ML.LogisticRegression.t()) :: integer() | {:error, String.t()}
  def getVarCount(self) do
    positional = [
    ]
    :evision_nif.ml_ml_LogisticRegression_getVarCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  This function returns the trained parameters arranged across rows.

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  For a two class classification problem, it returns a row matrix. It returns learnt parameters of
  the Logistic Regression as a matrix of type CV_32F.

  Python prototype (for reference only):
  ```python3
  get_learnt_thetas() -> retval
  ```
  """
  @spec get_learnt_thetas(Evision.ML.LogisticRegression.t()) :: Evision.Mat.t() | {:error, String.t()}
  def get_learnt_thetas(self) do
    positional = [
    ]
    :evision_nif.ml_ml_LogisticRegression_get_learnt_thetas(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is classifier

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isClassifier() -> retval
  ```
  """
  @spec isClassifier(Evision.ML.LogisticRegression.t()) :: boolean() | {:error, String.t()}
  def isClassifier(self) do
    positional = [
    ]
    :evision_nif.ml_ml_LogisticRegression_isClassifier(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is trained

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isTrained() -> retval
  ```
  """
  @spec isTrained(Evision.ML.LogisticRegression.t()) :: boolean() | {:error, String.t()}
  def isTrained(self) do
    positional = [
    ]
    :evision_nif.ml_ml_LogisticRegression_isTrained(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Loads and creates a serialized LogisticRegression from a file

  ##### Positional Arguments
  - **filepath**: `String`.

    path to serialized LogisticRegression

  ##### Keyword Arguments
  - **nodeName**: `String`.

    name of node containing the classifier

  ##### Return
  - **retval**: `Evision.ML.LogisticRegression.t()`

   Use LogisticRegression::save to serialize and store an LogisticRegression to disk.
   Load the LogisticRegression from this file again, by calling this function with the path to the file.
   Optionally specify the node for the file containing the classifier

  Python prototype (for reference only):
  ```python3
  load(filepath[, nodeName]) -> retval
  ```
  """
  @spec load(binary(), [{:nodeName, term()}] | nil) :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def load(filepath, opts) when is_binary(filepath) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:nodeName])
    positional = [
      filepath: Evision.Internal.Structurise.from_struct(filepath)
    ]
    :evision_nif.ml_ml_LogisticRegression_load_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Loads and creates a serialized LogisticRegression from a file

  ##### Positional Arguments
  - **filepath**: `String`.

    path to serialized LogisticRegression

  ##### Keyword Arguments
  - **nodeName**: `String`.

    name of node containing the classifier

  ##### Return
  - **retval**: `Evision.ML.LogisticRegression.t()`

   Use LogisticRegression::save to serialize and store an LogisticRegression to disk.
   Load the LogisticRegression from this file again, by calling this function with the path to the file.
   Optionally specify the node for the file containing the classifier

  Python prototype (for reference only):
  ```python3
  load(filepath[, nodeName]) -> retval
  ```
  """
  @spec load(binary()) :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def load(filepath) when is_binary(filepath)
  do
    positional = [
      filepath: Evision.Internal.Structurise.from_struct(filepath)
    ]
    :evision_nif.ml_ml_LogisticRegression_load_static(positional)
    |> to_struct()
  end

  @doc """
  Predicts responses for input samples and returns a float type.

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
  - **samples**: `Evision.Mat`.

    The input data for the prediction algorithm. Matrix [m x n], where each row
    contains variables (features) of one object being classified. Should have data type CV_32F.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Not used.

  ##### Return
  - **retval**: `float`
  - **results**: `Evision.Mat.t()`.

    Predicted labels as a column matrix of type CV_32S.

  Python prototype (for reference only):
  ```python3
  predict(samples[, results[, flags]]) -> retval, results
  ```
  """
  @spec predict(Evision.ML.LogisticRegression.t(), Evision.Mat.maybe_mat_in(), [{:flags, term()}] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples, opts) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_LogisticRegression_predict(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Predicts responses for input samples and returns a float type.

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
  - **samples**: `Evision.Mat`.

    The input data for the prediction algorithm. Matrix [m x n], where each row
    contains variables (features) of one object being classified. Should have data type CV_32F.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Not used.

  ##### Return
  - **retval**: `float`
  - **results**: `Evision.Mat.t()`.

    Predicted labels as a column matrix of type CV_32S.

  Python prototype (for reference only):
  ```python3
  predict(samples[, results[, flags]]) -> retval, results
  ```
  """
  @spec predict(Evision.ML.LogisticRegression.t(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_LogisticRegression_predict(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.ML.LogisticRegression.t(), Evision.FileNode.t()) :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ml_LogisticRegression_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.ML.LogisticRegression.t(), binary()) :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ml_LogisticRegression_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setIterations

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
  - **val**: `integer()`

  @see `getIterations/1`

  Python prototype (for reference only):
  ```python3
  setIterations(val) -> None
  ```
  """
  @spec setIterations(Evision.ML.LogisticRegression.t(), integer()) :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def setIterations(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_LogisticRegression_setIterations(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setLearningRate

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
  - **val**: `double`

  @see `getLearningRate/1`

  Python prototype (for reference only):
  ```python3
  setLearningRate(val) -> None
  ```
  """
  @spec setLearningRate(Evision.ML.LogisticRegression.t(), number()) :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def setLearningRate(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_LogisticRegression_setLearningRate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMiniBatchSize

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
  - **val**: `integer()`

  @see `getMiniBatchSize/1`

  Python prototype (for reference only):
  ```python3
  setMiniBatchSize(val) -> None
  ```
  """
  @spec setMiniBatchSize(Evision.ML.LogisticRegression.t(), integer()) :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def setMiniBatchSize(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_LogisticRegression_setMiniBatchSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRegularization

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
  - **val**: `integer()`

  @see `getRegularization/1`

  Python prototype (for reference only):
  ```python3
  setRegularization(val) -> None
  ```
  """
  @spec setRegularization(Evision.ML.LogisticRegression.t(), integer()) :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def setRegularization(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_LogisticRegression_setRegularization(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTermCriteria

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
  - **val**: `TermCriteria`

  @see `getTermCriteria/1`

  Python prototype (for reference only):
  ```python3
  setTermCriteria(val) -> None
  ```
  """
  @spec setTermCriteria(Evision.ML.LogisticRegression.t(), {integer(), integer(), number()}) :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def setTermCriteria(self, val) when is_tuple(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_LogisticRegression_setTermCriteria(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTrainMethod

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
  - **val**: `integer()`

  @see `getTrainMethod/1`

  Python prototype (for reference only):
  ```python3
  setTrainMethod(val) -> None
  ```
  """
  @spec setTrainMethod(Evision.ML.LogisticRegression.t(), integer()) :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def setTrainMethod(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_LogisticRegression_setTrainMethod(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
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
  @spec train(Evision.ML.LogisticRegression.t(), Evision.Mat.maybe_mat_in(), integer(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def train(self, samples, layout, responses) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and is_integer(layout) and (is_struct(responses, Evision.Mat) or is_struct(responses, Nx.Tensor) or is_number(responses) or is_tuple(responses))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      layout: Evision.Internal.Structurise.from_struct(layout),
      responses: Evision.Internal.Structurise.from_struct(responses)
    ]
    :evision_nif.ml_ml_LogisticRegression_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
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
  @spec train(Evision.ML.LogisticRegression.t(), Evision.ML.TrainData.t(), [{:flags, term()}] | nil) :: boolean() | {:error, String.t()}
  def train(self, trainData, opts) when is_struct(trainData, Evision.ML.TrainData) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_LogisticRegression_train(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
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
  @spec train(Evision.ML.LogisticRegression.t(), Evision.ML.TrainData.t()) :: boolean() | {:error, String.t()}
  def train(self, trainData) when is_struct(trainData, Evision.ML.TrainData)
  do
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_LogisticRegression_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.ML.LogisticRegression.t(), Evision.FileStorage.t(), binary()) :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ml_LogisticRegression_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.LogisticRegression.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.ML.LogisticRegression.t(), Evision.FileStorage.t()) :: Evision.ML.LogisticRegression.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ml_LogisticRegression_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
