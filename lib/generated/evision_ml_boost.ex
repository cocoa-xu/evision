defmodule Evision.ML.Boost do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ML.Boost` struct.

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
  def to_struct({:ok, %{class: Evision.ML.Boost, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ML.Boost, ref: ref}) do
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
  - **self**: `Evision.ML.Boost.t()`
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
  @spec calcError(Evision.ML.Boost.t(), Evision.ML.TrainData.t(), boolean(), [{atom(), term()},...] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test, opts) when is_struct(data, Evision.ML.TrainData) and is_boolean(test) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_Boost_calcError(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes error on the training or test dataset

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
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
  @spec calcError(Evision.ML.Boost.t(), Evision.ML.TrainData.t(), boolean()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test) when is_struct(data, Evision.ML.TrainData) and is_boolean(test)
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_Boost_calcError(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.ML.Boost.t()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ml_Boost_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Return
  - **retval**: `Evision.ML.Boost.t()`

  Creates the empty model.
  Use StatModel::train to train the model, Algorithm::load\\<Boost\\>(filename) to load the pre-trained model.

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.ML.Boost.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_create_static(positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.ML.Boost.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBoostType

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setBoostType/2`

  Python prototype (for reference only):
  ```python3
  getBoostType() -> retval
  ```
  """
  @spec getBoostType(Evision.ML.Boost.t()) :: integer() | {:error, String.t()}
  def getBoostType(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_getBoostType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCVFolds

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setCVFolds/2`

  Python prototype (for reference only):
  ```python3
  getCVFolds() -> retval
  ```
  """
  @spec getCVFolds(Evision.ML.Boost.t()) :: integer() | {:error, String.t()}
  def getCVFolds(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_getCVFolds(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.ML.Boost.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ml_Boost_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxCategories

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setMaxCategories/2`

  Python prototype (for reference only):
  ```python3
  getMaxCategories() -> retval
  ```
  """
  @spec getMaxCategories(Evision.ML.Boost.t()) :: integer() | {:error, String.t()}
  def getMaxCategories(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_getMaxCategories(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxDepth

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setMaxDepth/2`

  Python prototype (for reference only):
  ```python3
  getMaxDepth() -> retval
  ```
  """
  @spec getMaxDepth(Evision.ML.Boost.t()) :: integer() | {:error, String.t()}
  def getMaxDepth(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_getMaxDepth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinSampleCount

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setMinSampleCount/2`

  Python prototype (for reference only):
  ```python3
  getMinSampleCount() -> retval
  ```
  """
  @spec getMinSampleCount(Evision.ML.Boost.t()) :: integer() | {:error, String.t()}
  def getMinSampleCount(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_getMinSampleCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getPriors

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  @see `setPriors/2`

  Python prototype (for reference only):
  ```python3
  getPriors() -> retval
  ```
  """
  @spec getPriors(Evision.ML.Boost.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getPriors(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_getPriors(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRegressionAccuracy

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `float`

  @see `setRegressionAccuracy/2`

  Python prototype (for reference only):
  ```python3
  getRegressionAccuracy() -> retval
  ```
  """
  @spec getRegressionAccuracy(Evision.ML.Boost.t()) :: number() | {:error, String.t()}
  def getRegressionAccuracy(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_getRegressionAccuracy(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTruncatePrunedTree

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `bool`

  @see `setTruncatePrunedTree/2`

  Python prototype (for reference only):
  ```python3
  getTruncatePrunedTree() -> retval
  ```
  """
  @spec getTruncatePrunedTree(Evision.ML.Boost.t()) :: boolean() | {:error, String.t()}
  def getTruncatePrunedTree(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_getTruncatePrunedTree(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUse1SERule

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `bool`

  @see `setUse1SERule/2`

  Python prototype (for reference only):
  ```python3
  getUse1SERule() -> retval
  ```
  """
  @spec getUse1SERule(Evision.ML.Boost.t()) :: boolean() | {:error, String.t()}
  def getUse1SERule(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_getUse1SERule(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUseSurrogates

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `bool`

  @see `setUseSurrogates/2`

  Python prototype (for reference only):
  ```python3
  getUseSurrogates() -> retval
  ```
  """
  @spec getUseSurrogates(Evision.ML.Boost.t()) :: boolean() | {:error, String.t()}
  def getUseSurrogates(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_getUseSurrogates(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the number of variables in training samples

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getVarCount() -> retval
  ```
  """
  @spec getVarCount(Evision.ML.Boost.t()) :: integer() | {:error, String.t()}
  def getVarCount(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_getVarCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getWeakCount

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setWeakCount/2`

  Python prototype (for reference only):
  ```python3
  getWeakCount() -> retval
  ```
  """
  @spec getWeakCount(Evision.ML.Boost.t()) :: integer() | {:error, String.t()}
  def getWeakCount(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_getWeakCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getWeightTrimRate

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `double`

  @see `setWeightTrimRate/2`

  Python prototype (for reference only):
  ```python3
  getWeightTrimRate() -> retval
  ```
  """
  @spec getWeightTrimRate(Evision.ML.Boost.t()) :: number() | {:error, String.t()}
  def getWeightTrimRate(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_getWeightTrimRate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is classifier

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isClassifier() -> retval
  ```
  """
  @spec isClassifier(Evision.ML.Boost.t()) :: boolean() | {:error, String.t()}
  def isClassifier(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_isClassifier(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is trained

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isTrained() -> retval
  ```
  """
  @spec isTrained(Evision.ML.Boost.t()) :: boolean() | {:error, String.t()}
  def isTrained(self) do
    positional = [
    ]
    :evision_nif.ml_ml_Boost_isTrained(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Loads and creates a serialized Boost from a file

  ##### Positional Arguments
  - **filepath**: `String`.

    path to serialized Boost

  ##### Keyword Arguments
  - **nodeName**: `String`.

    name of node containing the classifier

  ##### Return
  - **retval**: `Evision.ML.Boost.t()`

   Use Boost::save to serialize and store an RTree to disk.
   Load the Boost from this file again, by calling this function with the path to the file.
   Optionally specify the node for the file containing the classifier

  Python prototype (for reference only):
  ```python3
  load(filepath[, nodeName]) -> retval
  ```
  """
  @spec load(binary(), [{:nodeName, term()}] | nil) :: Evision.ML.Boost.t() | {:error, String.t()}
  def load(filepath, opts) when is_binary(filepath) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:nodeName])
    positional = [
      filepath: Evision.Internal.Structurise.from_struct(filepath)
    ]
    :evision_nif.ml_ml_Boost_load_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Loads and creates a serialized Boost from a file

  ##### Positional Arguments
  - **filepath**: `String`.

    path to serialized Boost

  ##### Keyword Arguments
  - **nodeName**: `String`.

    name of node containing the classifier

  ##### Return
  - **retval**: `Evision.ML.Boost.t()`

   Use Boost::save to serialize and store an RTree to disk.
   Load the Boost from this file again, by calling this function with the path to the file.
   Optionally specify the node for the file containing the classifier

  Python prototype (for reference only):
  ```python3
  load(filepath[, nodeName]) -> retval
  ```
  """
  @spec load(binary()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def load(filepath) when is_binary(filepath)
  do
    positional = [
      filepath: Evision.Internal.Structurise.from_struct(filepath)
    ]
    :evision_nif.ml_ml_Boost_load_static(positional)
    |> to_struct()
  end

  @doc """
  Predicts response(s) for the provided sample(s)

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
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
  @spec predict(Evision.ML.Boost.t(), Evision.Mat.maybe_mat_in(), [{:flags, term()}] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples, opts) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_Boost_predict(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Predicts response(s) for the provided sample(s)

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
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
  @spec predict(Evision.ML.Boost.t(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_Boost_predict(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.ML.Boost.t(), Evision.FileNode.t()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ml_Boost_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.ML.Boost.t(), binary()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ml_Boost_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBoostType

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **val**: `integer()`

  @see `getBoostType/1`

  Python prototype (for reference only):
  ```python3
  setBoostType(val) -> None
  ```
  """
  @spec setBoostType(Evision.ML.Boost.t(), integer()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def setBoostType(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_Boost_setBoostType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCVFolds

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **val**: `integer()`

  @see `getCVFolds/1`

  Python prototype (for reference only):
  ```python3
  setCVFolds(val) -> None
  ```
  """
  @spec setCVFolds(Evision.ML.Boost.t(), integer()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def setCVFolds(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_Boost_setCVFolds(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxCategories

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **val**: `integer()`

  @see `getMaxCategories/1`

  Python prototype (for reference only):
  ```python3
  setMaxCategories(val) -> None
  ```
  """
  @spec setMaxCategories(Evision.ML.Boost.t(), integer()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def setMaxCategories(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_Boost_setMaxCategories(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxDepth

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **val**: `integer()`

  @see `getMaxDepth/1`

  Python prototype (for reference only):
  ```python3
  setMaxDepth(val) -> None
  ```
  """
  @spec setMaxDepth(Evision.ML.Boost.t(), integer()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def setMaxDepth(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_Boost_setMaxDepth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinSampleCount

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **val**: `integer()`

  @see `getMinSampleCount/1`

  Python prototype (for reference only):
  ```python3
  setMinSampleCount(val) -> None
  ```
  """
  @spec setMinSampleCount(Evision.ML.Boost.t(), integer()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def setMinSampleCount(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_Boost_setMinSampleCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPriors

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **val**: `Evision.Mat`

  @see `getPriors/1`

  Python prototype (for reference only):
  ```python3
  setPriors(val) -> None
  ```
  """
  @spec setPriors(Evision.ML.Boost.t(), Evision.Mat.maybe_mat_in()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def setPriors(self, val) when (is_struct(val, Evision.Mat) or is_struct(val, Nx.Tensor) or is_number(val) or is_tuple(val))
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_Boost_setPriors(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRegressionAccuracy

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **val**: `float`

  @see `getRegressionAccuracy/1`

  Python prototype (for reference only):
  ```python3
  setRegressionAccuracy(val) -> None
  ```
  """
  @spec setRegressionAccuracy(Evision.ML.Boost.t(), number()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def setRegressionAccuracy(self, val) when is_float(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_Boost_setRegressionAccuracy(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTruncatePrunedTree

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **val**: `bool`

  @see `getTruncatePrunedTree/1`

  Python prototype (for reference only):
  ```python3
  setTruncatePrunedTree(val) -> None
  ```
  """
  @spec setTruncatePrunedTree(Evision.ML.Boost.t(), boolean()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def setTruncatePrunedTree(self, val) when is_boolean(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_Boost_setTruncatePrunedTree(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setUse1SERule

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **val**: `bool`

  @see `getUse1SERule/1`

  Python prototype (for reference only):
  ```python3
  setUse1SERule(val) -> None
  ```
  """
  @spec setUse1SERule(Evision.ML.Boost.t(), boolean()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def setUse1SERule(self, val) when is_boolean(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_Boost_setUse1SERule(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setUseSurrogates

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **val**: `bool`

  @see `getUseSurrogates/1`

  Python prototype (for reference only):
  ```python3
  setUseSurrogates(val) -> None
  ```
  """
  @spec setUseSurrogates(Evision.ML.Boost.t(), boolean()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def setUseSurrogates(self, val) when is_boolean(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_Boost_setUseSurrogates(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setWeakCount

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **val**: `integer()`

  @see `getWeakCount/1`

  Python prototype (for reference only):
  ```python3
  setWeakCount(val) -> None
  ```
  """
  @spec setWeakCount(Evision.ML.Boost.t(), integer()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def setWeakCount(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_Boost_setWeakCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setWeightTrimRate

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **val**: `double`

  @see `getWeightTrimRate/1`

  Python prototype (for reference only):
  ```python3
  setWeightTrimRate(val) -> None
  ```
  """
  @spec setWeightTrimRate(Evision.ML.Boost.t(), number()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def setWeightTrimRate(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_Boost_setWeightTrimRate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
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
  @spec train(Evision.ML.Boost.t(), Evision.Mat.maybe_mat_in(), integer(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def train(self, samples, layout, responses) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and is_integer(layout) and (is_struct(responses, Evision.Mat) or is_struct(responses, Nx.Tensor) or is_number(responses) or is_tuple(responses))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      layout: Evision.Internal.Structurise.from_struct(layout),
      responses: Evision.Internal.Structurise.from_struct(responses)
    ]
    :evision_nif.ml_ml_Boost_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
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
  @spec train(Evision.ML.Boost.t(), Evision.ML.TrainData.t(), [{:flags, term()}] | nil) :: boolean() | {:error, String.t()}
  def train(self, trainData, opts) when is_struct(trainData, Evision.ML.TrainData) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_Boost_train(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
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
  @spec train(Evision.ML.Boost.t(), Evision.ML.TrainData.t()) :: boolean() | {:error, String.t()}
  def train(self, trainData) when is_struct(trainData, Evision.ML.TrainData)
  do
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_Boost_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.ML.Boost.t(), Evision.FileStorage.t(), binary()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ml_Boost_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.Boost.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.ML.Boost.t(), Evision.FileStorage.t()) :: Evision.ML.Boost.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ml_Boost_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
