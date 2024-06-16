defmodule Evision.ML.StatModel do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ML.StatModel` struct.

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
  def to_struct({:ok, %{class: Evision.ML.StatModel, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ML.StatModel, ref: ref}) do
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
  - **self**: `Evision.ML.StatModel.t()`
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
  @spec calcError(Evision.ML.StatModel.t(), Evision.ML.TrainData.t(), boolean(), [{atom(), term()},...] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test, opts) when is_struct(data, Evision.ML.TrainData) and is_boolean(test) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_StatModel_calcError(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes error on the training or test dataset

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`
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
  @spec calcError(Evision.ML.StatModel.t(), Evision.ML.TrainData.t(), boolean()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test) when is_struct(data, Evision.ML.TrainData) and is_boolean(test)
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_StatModel_calcError(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.ML.StatModel.t()) :: Evision.ML.StatModel.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ml_StatModel_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.ML.StatModel.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ml_ml_StatModel_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.ML.StatModel.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ml_StatModel_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the number of variables in training samples

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getVarCount() -> retval
  ```
  """
  @spec getVarCount(Evision.ML.StatModel.t()) :: integer() | {:error, String.t()}
  def getVarCount(self) do
    positional = [
    ]
    :evision_nif.ml_ml_StatModel_getVarCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is classifier

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isClassifier() -> retval
  ```
  """
  @spec isClassifier(Evision.ML.StatModel.t()) :: boolean() | {:error, String.t()}
  def isClassifier(self) do
    positional = [
    ]
    :evision_nif.ml_ml_StatModel_isClassifier(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is trained

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isTrained() -> retval
  ```
  """
  @spec isTrained(Evision.ML.StatModel.t()) :: boolean() | {:error, String.t()}
  def isTrained(self) do
    positional = [
    ]
    :evision_nif.ml_ml_StatModel_isTrained(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Predicts response(s) for the provided sample(s)

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`
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
  @spec predict(Evision.ML.StatModel.t(), Evision.Mat.maybe_mat_in(), [{:flags, term()}] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples, opts) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_StatModel_predict(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Predicts response(s) for the provided sample(s)

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`
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
  @spec predict(Evision.ML.StatModel.t(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_StatModel_predict(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.ML.StatModel.t(), Evision.FileNode.t()) :: Evision.ML.StatModel.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ml_StatModel_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.ML.StatModel.t(), binary()) :: Evision.ML.StatModel.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ml_StatModel_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`
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
  @spec train(Evision.ML.StatModel.t(), Evision.Mat.maybe_mat_in(), integer(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def train(self, samples, layout, responses) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and is_integer(layout) and (is_struct(responses, Evision.Mat) or is_struct(responses, Nx.Tensor) or is_number(responses) or is_tuple(responses))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      layout: Evision.Internal.Structurise.from_struct(layout),
      responses: Evision.Internal.Structurise.from_struct(responses)
    ]
    :evision_nif.ml_ml_StatModel_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`
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
  @spec train(Evision.ML.StatModel.t(), Evision.ML.TrainData.t(), [{:flags, term()}] | nil) :: boolean() | {:error, String.t()}
  def train(self, trainData, opts) when is_struct(trainData, Evision.ML.TrainData) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_StatModel_train(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`
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
  @spec train(Evision.ML.StatModel.t(), Evision.ML.TrainData.t()) :: boolean() | {:error, String.t()}
  def train(self, trainData) when is_struct(trainData, Evision.ML.TrainData)
  do
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_StatModel_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.ML.StatModel.t(), Evision.FileStorage.t(), binary()) :: Evision.ML.StatModel.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ml_StatModel_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.StatModel.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.ML.StatModel.t(), Evision.FileStorage.t()) :: Evision.ML.StatModel.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ml_StatModel_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
