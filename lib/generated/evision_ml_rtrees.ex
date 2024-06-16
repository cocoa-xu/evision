defmodule Evision.ML.RTrees do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ML.RTrees` struct.

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
  def to_struct({:ok, %{class: Evision.ML.RTrees, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ML.RTrees, ref: ref}) do
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
  - **self**: `Evision.ML.RTrees.t()`
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
  @spec calcError(Evision.ML.RTrees.t(), Evision.ML.TrainData.t(), boolean(), [{atom(), term()},...] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test, opts) when is_struct(data, Evision.ML.TrainData) and is_boolean(test) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_RTrees_calcError(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes error on the training or test dataset

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
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
  @spec calcError(Evision.ML.RTrees.t(), Evision.ML.TrainData.t(), boolean()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test) when is_struct(data, Evision.ML.TrainData) and is_boolean(test)
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_RTrees_calcError(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.ML.RTrees.t()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ml_RTrees_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Return
  - **retval**: `Evision.ML.RTrees.t()`

  Creates the empty model.
  Use StatModel::train to train the model, StatModel::train to create and train the model,
  Algorithm::load to load the pre-trained model.

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.ML.RTrees.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_create_static(positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.ML.RTrees.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getActiveVarCount

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setActiveVarCount/2`

  Python prototype (for reference only):
  ```python3
  getActiveVarCount() -> retval
  ```
  """
  @spec getActiveVarCount(Evision.ML.RTrees.t()) :: integer() | {:error, String.t()}
  def getActiveVarCount(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getActiveVarCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCVFolds

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setCVFolds/2`

  Python prototype (for reference only):
  ```python3
  getCVFolds() -> retval
  ```
  """
  @spec getCVFolds(Evision.ML.RTrees.t()) :: integer() | {:error, String.t()}
  def getCVFolds(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getCVFolds(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCalculateVarImportance

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `bool`

  @see `setCalculateVarImportance/2`

  Python prototype (for reference only):
  ```python3
  getCalculateVarImportance() -> retval
  ```
  """
  @spec getCalculateVarImportance(Evision.ML.RTrees.t()) :: boolean() | {:error, String.t()}
  def getCalculateVarImportance(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getCalculateVarImportance(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.ML.RTrees.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ml_RTrees_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxCategories

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setMaxCategories/2`

  Python prototype (for reference only):
  ```python3
  getMaxCategories() -> retval
  ```
  """
  @spec getMaxCategories(Evision.ML.RTrees.t()) :: integer() | {:error, String.t()}
  def getMaxCategories(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getMaxCategories(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxDepth

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setMaxDepth/2`

  Python prototype (for reference only):
  ```python3
  getMaxDepth() -> retval
  ```
  """
  @spec getMaxDepth(Evision.ML.RTrees.t()) :: integer() | {:error, String.t()}
  def getMaxDepth(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getMaxDepth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinSampleCount

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setMinSampleCount/2`

  Python prototype (for reference only):
  ```python3
  getMinSampleCount() -> retval
  ```
  """
  @spec getMinSampleCount(Evision.ML.RTrees.t()) :: integer() | {:error, String.t()}
  def getMinSampleCount(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getMinSampleCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getOOBError

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `double`

  Returns the OOB error value, computed at the training stage when calcOOBError is set to true.
   If this flag was set to false, 0 is returned. The OOB error is also scaled by sample weighting.

  Python prototype (for reference only):
  ```python3
  getOOBError() -> retval
  ```
  """
  @spec getOOBError(Evision.ML.RTrees.t()) :: number() | {:error, String.t()}
  def getOOBError(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getOOBError(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getPriors

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  @see `setPriors/2`

  Python prototype (for reference only):
  ```python3
  getPriors() -> retval
  ```
  """
  @spec getPriors(Evision.ML.RTrees.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getPriors(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getPriors(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRegressionAccuracy

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `float`

  @see `setRegressionAccuracy/2`

  Python prototype (for reference only):
  ```python3
  getRegressionAccuracy() -> retval
  ```
  """
  @spec getRegressionAccuracy(Evision.ML.RTrees.t()) :: number() | {:error, String.t()}
  def getRegressionAccuracy(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getRegressionAccuracy(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTermCriteria

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `TermCriteria`

  @see `setTermCriteria/2`

  Python prototype (for reference only):
  ```python3
  getTermCriteria() -> retval
  ```
  """
  @spec getTermCriteria(Evision.ML.RTrees.t()) :: {integer(), integer(), number()} | {:error, String.t()}
  def getTermCriteria(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getTermCriteria(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTruncatePrunedTree

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `bool`

  @see `setTruncatePrunedTree/2`

  Python prototype (for reference only):
  ```python3
  getTruncatePrunedTree() -> retval
  ```
  """
  @spec getTruncatePrunedTree(Evision.ML.RTrees.t()) :: boolean() | {:error, String.t()}
  def getTruncatePrunedTree(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getTruncatePrunedTree(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUse1SERule

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `bool`

  @see `setUse1SERule/2`

  Python prototype (for reference only):
  ```python3
  getUse1SERule() -> retval
  ```
  """
  @spec getUse1SERule(Evision.ML.RTrees.t()) :: boolean() | {:error, String.t()}
  def getUse1SERule(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getUse1SERule(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getUseSurrogates

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `bool`

  @see `setUseSurrogates/2`

  Python prototype (for reference only):
  ```python3
  getUseSurrogates() -> retval
  ```
  """
  @spec getUseSurrogates(Evision.ML.RTrees.t()) :: boolean() | {:error, String.t()}
  def getUseSurrogates(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getUseSurrogates(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the number of variables in training samples

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getVarCount() -> retval
  ```
  """
  @spec getVarCount(Evision.ML.RTrees.t()) :: integer() | {:error, String.t()}
  def getVarCount(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getVarCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getVarImportance

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Returns the variable importance array.
  The method returns the variable importance vector, computed at the training stage when
  CalculateVarImportance is set to true. If this flag was set to false, the empty matrix is
  returned.

  Python prototype (for reference only):
  ```python3
  getVarImportance() -> retval
  ```
  """
  @spec getVarImportance(Evision.ML.RTrees.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getVarImportance(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_getVarImportance(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getVotes

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **samples**: `Evision.Mat`.

    Array containing the samples for which votes will be calculated.

  - **flags**: `integer()`.

    Flags for defining the type of RTrees.

  ##### Return
  - **results**: `Evision.Mat.t()`.

    Array where the result of the calculation will be written.

  Returns the result of each individual tree in the forest.
  In case the model is a regression problem, the method will return each of the trees'
  results for each of the sample cases. If the model is a classifier, it will return
  a Mat with samples + 1 rows, where the first row gives the class number and the
  following rows return the votes each class had for each sample.

  Python prototype (for reference only):
  ```python3
  getVotes(samples, flags[, results]) -> results
  ```
  """
  @spec getVotes(Evision.ML.RTrees.t(), Evision.Mat.maybe_mat_in(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getVotes(self, samples, flags, opts) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and is_integer(flags) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.ml_ml_RTrees_getVotes(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  getVotes

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **samples**: `Evision.Mat`.

    Array containing the samples for which votes will be calculated.

  - **flags**: `integer()`.

    Flags for defining the type of RTrees.

  ##### Return
  - **results**: `Evision.Mat.t()`.

    Array where the result of the calculation will be written.

  Returns the result of each individual tree in the forest.
  In case the model is a regression problem, the method will return each of the trees'
  results for each of the sample cases. If the model is a classifier, it will return
  a Mat with samples + 1 rows, where the first row gives the class number and the
  following rows return the votes each class had for each sample.

  Python prototype (for reference only):
  ```python3
  getVotes(samples, flags[, results]) -> results
  ```
  """
  @spec getVotes(Evision.ML.RTrees.t(), Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def getVotes(self, samples, flags) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and is_integer(flags)
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      flags: Evision.Internal.Structurise.from_struct(flags)
    ]
    :evision_nif.ml_ml_RTrees_getVotes(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is classifier

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isClassifier() -> retval
  ```
  """
  @spec isClassifier(Evision.ML.RTrees.t()) :: boolean() | {:error, String.t()}
  def isClassifier(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_isClassifier(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is trained

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isTrained() -> retval
  ```
  """
  @spec isTrained(Evision.ML.RTrees.t()) :: boolean() | {:error, String.t()}
  def isTrained(self) do
    positional = [
    ]
    :evision_nif.ml_ml_RTrees_isTrained(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Loads and creates a serialized RTree from a file

  ##### Positional Arguments
  - **filepath**: `String`.

    path to serialized RTree

  ##### Keyword Arguments
  - **nodeName**: `String`.

    name of node containing the classifier

  ##### Return
  - **retval**: `Evision.ML.RTrees.t()`

   Use RTree::save to serialize and store an RTree to disk.
   Load the RTree from this file again, by calling this function with the path to the file.
   Optionally specify the node for the file containing the classifier

  Python prototype (for reference only):
  ```python3
  load(filepath[, nodeName]) -> retval
  ```
  """
  @spec load(binary(), [{:nodeName, term()}] | nil) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def load(filepath, opts) when is_binary(filepath) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:nodeName])
    positional = [
      filepath: Evision.Internal.Structurise.from_struct(filepath)
    ]
    :evision_nif.ml_ml_RTrees_load_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Loads and creates a serialized RTree from a file

  ##### Positional Arguments
  - **filepath**: `String`.

    path to serialized RTree

  ##### Keyword Arguments
  - **nodeName**: `String`.

    name of node containing the classifier

  ##### Return
  - **retval**: `Evision.ML.RTrees.t()`

   Use RTree::save to serialize and store an RTree to disk.
   Load the RTree from this file again, by calling this function with the path to the file.
   Optionally specify the node for the file containing the classifier

  Python prototype (for reference only):
  ```python3
  load(filepath[, nodeName]) -> retval
  ```
  """
  @spec load(binary()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def load(filepath) when is_binary(filepath)
  do
    positional = [
      filepath: Evision.Internal.Structurise.from_struct(filepath)
    ]
    :evision_nif.ml_ml_RTrees_load_static(positional)
    |> to_struct()
  end

  @doc """
  Predicts response(s) for the provided sample(s)

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
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
  @spec predict(Evision.ML.RTrees.t(), Evision.Mat.maybe_mat_in(), [{:flags, term()}] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples, opts) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_RTrees_predict(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Predicts response(s) for the provided sample(s)

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
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
  @spec predict(Evision.ML.RTrees.t(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_RTrees_predict(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.ML.RTrees.t(), Evision.FileNode.t()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ml_RTrees_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.ML.RTrees.t(), binary()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ml_RTrees_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setActiveVarCount

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **val**: `integer()`

  @see `getActiveVarCount/1`

  Python prototype (for reference only):
  ```python3
  setActiveVarCount(val) -> None
  ```
  """
  @spec setActiveVarCount(Evision.ML.RTrees.t(), integer()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def setActiveVarCount(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_RTrees_setActiveVarCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCVFolds

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **val**: `integer()`

  @see `getCVFolds/1`

  Python prototype (for reference only):
  ```python3
  setCVFolds(val) -> None
  ```
  """
  @spec setCVFolds(Evision.ML.RTrees.t(), integer()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def setCVFolds(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_RTrees_setCVFolds(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCalculateVarImportance

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **val**: `bool`

  @see `getCalculateVarImportance/1`

  Python prototype (for reference only):
  ```python3
  setCalculateVarImportance(val) -> None
  ```
  """
  @spec setCalculateVarImportance(Evision.ML.RTrees.t(), boolean()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def setCalculateVarImportance(self, val) when is_boolean(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_RTrees_setCalculateVarImportance(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxCategories

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **val**: `integer()`

  @see `getMaxCategories/1`

  Python prototype (for reference only):
  ```python3
  setMaxCategories(val) -> None
  ```
  """
  @spec setMaxCategories(Evision.ML.RTrees.t(), integer()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def setMaxCategories(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_RTrees_setMaxCategories(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxDepth

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **val**: `integer()`

  @see `getMaxDepth/1`

  Python prototype (for reference only):
  ```python3
  setMaxDepth(val) -> None
  ```
  """
  @spec setMaxDepth(Evision.ML.RTrees.t(), integer()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def setMaxDepth(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_RTrees_setMaxDepth(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinSampleCount

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **val**: `integer()`

  @see `getMinSampleCount/1`

  Python prototype (for reference only):
  ```python3
  setMinSampleCount(val) -> None
  ```
  """
  @spec setMinSampleCount(Evision.ML.RTrees.t(), integer()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def setMinSampleCount(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_RTrees_setMinSampleCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setPriors

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **val**: `Evision.Mat`

  @see `getPriors/1`

  Python prototype (for reference only):
  ```python3
  setPriors(val) -> None
  ```
  """
  @spec setPriors(Evision.ML.RTrees.t(), Evision.Mat.maybe_mat_in()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def setPriors(self, val) when (is_struct(val, Evision.Mat) or is_struct(val, Nx.Tensor) or is_number(val) or is_tuple(val))
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_RTrees_setPriors(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRegressionAccuracy

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **val**: `float`

  @see `getRegressionAccuracy/1`

  Python prototype (for reference only):
  ```python3
  setRegressionAccuracy(val) -> None
  ```
  """
  @spec setRegressionAccuracy(Evision.ML.RTrees.t(), number()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def setRegressionAccuracy(self, val) when is_float(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_RTrees_setRegressionAccuracy(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTermCriteria

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **val**: `TermCriteria`

  @see `getTermCriteria/1`

  Python prototype (for reference only):
  ```python3
  setTermCriteria(val) -> None
  ```
  """
  @spec setTermCriteria(Evision.ML.RTrees.t(), {integer(), integer(), number()}) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def setTermCriteria(self, val) when is_tuple(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_RTrees_setTermCriteria(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTruncatePrunedTree

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **val**: `bool`

  @see `getTruncatePrunedTree/1`

  Python prototype (for reference only):
  ```python3
  setTruncatePrunedTree(val) -> None
  ```
  """
  @spec setTruncatePrunedTree(Evision.ML.RTrees.t(), boolean()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def setTruncatePrunedTree(self, val) when is_boolean(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_RTrees_setTruncatePrunedTree(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setUse1SERule

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **val**: `bool`

  @see `getUse1SERule/1`

  Python prototype (for reference only):
  ```python3
  setUse1SERule(val) -> None
  ```
  """
  @spec setUse1SERule(Evision.ML.RTrees.t(), boolean()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def setUse1SERule(self, val) when is_boolean(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_RTrees_setUse1SERule(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setUseSurrogates

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **val**: `bool`

  @see `getUseSurrogates/1`

  Python prototype (for reference only):
  ```python3
  setUseSurrogates(val) -> None
  ```
  """
  @spec setUseSurrogates(Evision.ML.RTrees.t(), boolean()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def setUseSurrogates(self, val) when is_boolean(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_RTrees_setUseSurrogates(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
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
  @spec train(Evision.ML.RTrees.t(), Evision.Mat.maybe_mat_in(), integer(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def train(self, samples, layout, responses) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and is_integer(layout) and (is_struct(responses, Evision.Mat) or is_struct(responses, Nx.Tensor) or is_number(responses) or is_tuple(responses))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      layout: Evision.Internal.Structurise.from_struct(layout),
      responses: Evision.Internal.Structurise.from_struct(responses)
    ]
    :evision_nif.ml_ml_RTrees_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
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
  @spec train(Evision.ML.RTrees.t(), Evision.ML.TrainData.t(), [{:flags, term()}] | nil) :: boolean() | {:error, String.t()}
  def train(self, trainData, opts) when is_struct(trainData, Evision.ML.TrainData) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_RTrees_train(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
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
  @spec train(Evision.ML.RTrees.t(), Evision.ML.TrainData.t()) :: boolean() | {:error, String.t()}
  def train(self, trainData) when is_struct(trainData, Evision.ML.TrainData)
  do
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_RTrees_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.ML.RTrees.t(), Evision.FileStorage.t(), binary()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ml_RTrees_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.RTrees.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.ML.RTrees.t(), Evision.FileStorage.t()) :: Evision.ML.RTrees.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ml_RTrees_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
