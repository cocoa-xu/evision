defmodule Evision.ML.EM do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ML.EM` struct.

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
  def to_struct({:ok, %{class: Evision.ML.EM, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ML.EM, ref: ref}) do
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
  @type enum :: integer()
  @doc enum: true
  def cv_DEFAULT_NCLUSTERS, do: 5
  @doc enum: true
  def cv_DEFAULT_MAX_ITERS, do: 100
  @doc enum: true
  def cv_START_E_STEP, do: 1
  @doc enum: true
  def cv_START_M_STEP, do: 2
  @doc enum: true
  def cv_START_AUTO_STEP, do: 0


  @doc """
  Computes error on the training or test dataset

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
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
  @spec calcError(Evision.ML.EM.t(), Evision.ML.TrainData.t(), boolean(), [{atom(), term()},...] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test, opts) when is_struct(data, Evision.ML.TrainData) and is_boolean(test) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_EM_calcError(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes error on the training or test dataset

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
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
  @spec calcError(Evision.ML.EM.t(), Evision.ML.TrainData.t(), boolean()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test) when is_struct(data, Evision.ML.TrainData) and is_boolean(test)
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_EM_calcError(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.ML.EM.t()) :: Evision.ML.EM.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ml_EM_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  create
  ##### Return
  - **retval**: `Evision.ML.EM.t()`

  Creates empty %EM model.
  The model should be trained then using StatModel::train(traindata, flags) method. Alternatively, you
  can use one of the EM::train\\* methods or load it from file using Algorithm::load\\<EM\\>(filename).

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.ML.EM.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.ml_ml_EM_create_static(positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.ML.EM.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ml_ml_EM_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getClustersNumber

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setClustersNumber/2`

  Python prototype (for reference only):
  ```python3
  getClustersNumber() -> retval
  ```
  """
  @spec getClustersNumber(Evision.ML.EM.t()) :: integer() | {:error, String.t()}
  def getClustersNumber(self) do
    positional = [
    ]
    :evision_nif.ml_ml_EM_getClustersNumber(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getCovarianceMatrixType

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setCovarianceMatrixType/2`

  Python prototype (for reference only):
  ```python3
  getCovarianceMatrixType() -> retval
  ```
  """
  @spec getCovarianceMatrixType(Evision.ML.EM.t()) :: integer() | {:error, String.t()}
  def getCovarianceMatrixType(self) do
    positional = [
    ]
    :evision_nif.ml_ml_EM_getCovarianceMatrixType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns covariation matrices

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`

  ##### Return
  - **covs**: `[Evision.Mat]`.

  Returns vector of covariation matrices. Number of matrices is the number of gaussian mixtures,
  each matrix is a square floating-point matrix NxN, where N is the space dimensionality.

  Python prototype (for reference only):
  ```python3
  getCovs([, covs]) -> covs
  ```
  """
  @spec getCovs(Evision.ML.EM.t(), [{atom(), term()},...] | nil) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getCovs(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.ml_ml_EM_getCovs(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Returns covariation matrices

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`

  ##### Return
  - **covs**: `[Evision.Mat]`.

  Returns vector of covariation matrices. Number of matrices is the number of gaussian mixtures,
  each matrix is a square floating-point matrix NxN, where N is the space dimensionality.

  Python prototype (for reference only):
  ```python3
  getCovs([, covs]) -> covs
  ```
  """
  @spec getCovs(Evision.ML.EM.t()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getCovs(self) do
    positional = [
    ]
    :evision_nif.ml_ml_EM_getCovs(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.ML.EM.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ml_EM_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the cluster centers (means of the Gaussian mixture)

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Returns matrix with the number of rows equal to the number of mixtures and number of columns
  equal to the space dimensionality.

  Python prototype (for reference only):
  ```python3
  getMeans() -> retval
  ```
  """
  @spec getMeans(Evision.ML.EM.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getMeans(self) do
    positional = [
    ]
    :evision_nif.ml_ml_EM_getMeans(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTermCriteria

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`

  ##### Return
  - **retval**: `TermCriteria`

  @see `setTermCriteria/2`

  Python prototype (for reference only):
  ```python3
  getTermCriteria() -> retval
  ```
  """
  @spec getTermCriteria(Evision.ML.EM.t()) :: {integer(), integer(), number()} | {:error, String.t()}
  def getTermCriteria(self) do
    positional = [
    ]
    :evision_nif.ml_ml_EM_getTermCriteria(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the number of variables in training samples

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getVarCount() -> retval
  ```
  """
  @spec getVarCount(Evision.ML.EM.t()) :: integer() | {:error, String.t()}
  def getVarCount(self) do
    positional = [
    ]
    :evision_nif.ml_ml_EM_getVarCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns weights of the mixtures

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Returns vector with the number of elements equal to the number of mixtures.

  Python prototype (for reference only):
  ```python3
  getWeights() -> retval
  ```
  """
  @spec getWeights(Evision.ML.EM.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getWeights(self) do
    positional = [
    ]
    :evision_nif.ml_ml_EM_getWeights(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is classifier

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isClassifier() -> retval
  ```
  """
  @spec isClassifier(Evision.ML.EM.t()) :: boolean() | {:error, String.t()}
  def isClassifier(self) do
    positional = [
    ]
    :evision_nif.ml_ml_EM_isClassifier(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is trained

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isTrained() -> retval
  ```
  """
  @spec isTrained(Evision.ML.EM.t()) :: boolean() | {:error, String.t()}
  def isTrained(self) do
    positional = [
    ]
    :evision_nif.ml_ml_EM_isTrained(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Loads and creates a serialized EM from a file

  ##### Positional Arguments
  - **filepath**: `String`.

    path to serialized EM

  ##### Keyword Arguments
  - **nodeName**: `String`.

    name of node containing the classifier

  ##### Return
  - **retval**: `Evision.ML.EM.t()`

   Use EM::save to serialize and store an EM to disk.
   Load the EM from this file again, by calling this function with the path to the file.
   Optionally specify the node for the file containing the classifier

  Python prototype (for reference only):
  ```python3
  load(filepath[, nodeName]) -> retval
  ```
  """
  @spec load(binary(), [{:nodeName, term()}] | nil) :: Evision.ML.EM.t() | {:error, String.t()}
  def load(filepath, opts) when is_binary(filepath) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:nodeName])
    positional = [
      filepath: Evision.Internal.Structurise.from_struct(filepath)
    ]
    :evision_nif.ml_ml_EM_load_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Loads and creates a serialized EM from a file

  ##### Positional Arguments
  - **filepath**: `String`.

    path to serialized EM

  ##### Keyword Arguments
  - **nodeName**: `String`.

    name of node containing the classifier

  ##### Return
  - **retval**: `Evision.ML.EM.t()`

   Use EM::save to serialize and store an EM to disk.
   Load the EM from this file again, by calling this function with the path to the file.
   Optionally specify the node for the file containing the classifier

  Python prototype (for reference only):
  ```python3
  load(filepath[, nodeName]) -> retval
  ```
  """
  @spec load(binary()) :: Evision.ML.EM.t() | {:error, String.t()}
  def load(filepath) when is_binary(filepath)
  do
    positional = [
      filepath: Evision.Internal.Structurise.from_struct(filepath)
    ]
    :evision_nif.ml_ml_EM_load_static(positional)
    |> to_struct()
  end

  @doc """
  Returns posterior probabilities for the provided samples

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **samples**: `Evision.Mat`.

    The input samples, floating-point matrix

  ##### Keyword Arguments
  - **flags**: `integer()`.

    This parameter will be ignored

  ##### Return
  - **retval**: `float`
  - **results**: `Evision.Mat.t()`.

    The optional output \\f$ nSamples \\times nClusters\\f$ matrix of results. It contains
    posterior probabilities for each sample from the input

  Python prototype (for reference only):
  ```python3
  predict(samples[, results[, flags]]) -> retval, results
  ```
  """
  @spec predict(Evision.ML.EM.t(), Evision.Mat.maybe_mat_in(), [{:flags, term()}] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples, opts) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_EM_predict(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Returns posterior probabilities for the provided samples

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **samples**: `Evision.Mat`.

    The input samples, floating-point matrix

  ##### Keyword Arguments
  - **flags**: `integer()`.

    This parameter will be ignored

  ##### Return
  - **retval**: `float`
  - **results**: `Evision.Mat.t()`.

    The optional output \\f$ nSamples \\times nClusters\\f$ matrix of results. It contains
    posterior probabilities for each sample from the input

  Python prototype (for reference only):
  ```python3
  predict(samples[, results[, flags]]) -> retval, results
  ```
  """
  @spec predict(Evision.ML.EM.t(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_EM_predict(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns a likelihood logarithm value and an index of the most probable mixture component
  for the given sample.

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **sample**: `Evision.Mat`.

    A sample for classification. It should be a one-channel matrix of
    \\f$1 \\times dims\\f$ or \\f$dims \\times 1\\f$ size.

  ##### Return
  - **retval**: `Vec2d`
  - **probs**: `Evision.Mat.t()`.

    Optional output matrix that contains posterior probabilities of each component
    given the sample. It has \\f$1 \\times nclusters\\f$ size and CV_64FC1 type.

  The method returns a two-element double vector. Zero element is a likelihood logarithm value for
  the sample. First element is an index of the most probable mixture component for the given
  sample.

  Python prototype (for reference only):
  ```python3
  predict2(sample[, probs]) -> retval, probs
  ```
  """
  @spec predict2(Evision.ML.EM.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {{number(), number()}, Evision.Mat.t()} | {:error, String.t()}
  def predict2(self, sample, opts) when (is_struct(sample, Evision.Mat) or is_struct(sample, Nx.Tensor) or is_number(sample) or is_tuple(sample)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      sample: Evision.Internal.Structurise.from_struct(sample)
    ]
    :evision_nif.ml_ml_EM_predict2(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Returns a likelihood logarithm value and an index of the most probable mixture component
  for the given sample.

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **sample**: `Evision.Mat`.

    A sample for classification. It should be a one-channel matrix of
    \\f$1 \\times dims\\f$ or \\f$dims \\times 1\\f$ size.

  ##### Return
  - **retval**: `Vec2d`
  - **probs**: `Evision.Mat.t()`.

    Optional output matrix that contains posterior probabilities of each component
    given the sample. It has \\f$1 \\times nclusters\\f$ size and CV_64FC1 type.

  The method returns a two-element double vector. Zero element is a likelihood logarithm value for
  the sample. First element is an index of the most probable mixture component for the given
  sample.

  Python prototype (for reference only):
  ```python3
  predict2(sample[, probs]) -> retval, probs
  ```
  """
  @spec predict2(Evision.ML.EM.t(), Evision.Mat.maybe_mat_in()) :: {{number(), number()}, Evision.Mat.t()} | {:error, String.t()}
  def predict2(self, sample) when (is_struct(sample, Evision.Mat) or is_struct(sample, Nx.Tensor) or is_number(sample) or is_tuple(sample))
  do
    positional = [
      sample: Evision.Internal.Structurise.from_struct(sample)
    ]
    :evision_nif.ml_ml_EM_predict2(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.ML.EM.t(), Evision.FileNode.t()) :: Evision.ML.EM.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ml_EM_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.ML.EM.t(), binary()) :: Evision.ML.EM.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ml_EM_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setClustersNumber

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **val**: `integer()`

  @see `getClustersNumber/1`

  Python prototype (for reference only):
  ```python3
  setClustersNumber(val) -> None
  ```
  """
  @spec setClustersNumber(Evision.ML.EM.t(), integer()) :: Evision.ML.EM.t() | {:error, String.t()}
  def setClustersNumber(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_EM_setClustersNumber(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setCovarianceMatrixType

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **val**: `integer()`

  @see `getCovarianceMatrixType/1`

  Python prototype (for reference only):
  ```python3
  setCovarianceMatrixType(val) -> None
  ```
  """
  @spec setCovarianceMatrixType(Evision.ML.EM.t(), integer()) :: Evision.ML.EM.t() | {:error, String.t()}
  def setCovarianceMatrixType(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_EM_setCovarianceMatrixType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTermCriteria

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **val**: `TermCriteria`

  @see `getTermCriteria/1`

  Python prototype (for reference only):
  ```python3
  setTermCriteria(val) -> None
  ```
  """
  @spec setTermCriteria(Evision.ML.EM.t(), {integer(), integer(), number()}) :: Evision.ML.EM.t() | {:error, String.t()}
  def setTermCriteria(self, val) when is_tuple(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_EM_setTermCriteria(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
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
  @spec train(Evision.ML.EM.t(), Evision.Mat.maybe_mat_in(), integer(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def train(self, samples, layout, responses) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and is_integer(layout) and (is_struct(responses, Evision.Mat) or is_struct(responses, Nx.Tensor) or is_number(responses) or is_tuple(responses))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      layout: Evision.Internal.Structurise.from_struct(layout),
      responses: Evision.Internal.Structurise.from_struct(responses)
    ]
    :evision_nif.ml_ml_EM_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
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
  @spec train(Evision.ML.EM.t(), Evision.ML.TrainData.t(), [{:flags, term()}] | nil) :: boolean() | {:error, String.t()}
  def train(self, trainData, opts) when is_struct(trainData, Evision.ML.TrainData) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_EM_train(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
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
  @spec train(Evision.ML.EM.t(), Evision.ML.TrainData.t()) :: boolean() | {:error, String.t()}
  def train(self, trainData) when is_struct(trainData, Evision.ML.TrainData)
  do
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_EM_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Estimate the Gaussian mixture parameters from a samples set.

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **samples**: `Evision.Mat`.

    Samples from which the Gaussian mixture model will be estimated. It should be a
    one-channel matrix, each row of which is a sample. If the matrix does not have CV_64F type
    it will be converted to the inner matrix of such type for the further computing.

  - **means0**: `Evision.Mat`.

    Initial means \\f$a_k\\f$ of mixture components. It is a one-channel matrix of
    \\f$nclusters \\times dims\\f$ size. If the matrix does not have CV_64F type it will be
    converted to the inner matrix of such type for the further computing.

  ##### Keyword Arguments
  - **covs0**: `Evision.Mat`.

    The vector of initial covariance matrices \\f$S_k\\f$ of mixture components. Each of
    covariance matrices is a one-channel matrix of \\f$dims \\times dims\\f$ size. If the matrices
    do not have CV_64F type they will be converted to the inner matrices of such type for the
    further computing.

  - **weights0**: `Evision.Mat`.

    Initial weights \\f$\\pi_k\\f$ of mixture components. It should be a one-channel
    floating-point matrix with \\f$1 \\times nclusters\\f$ or \\f$nclusters \\times 1\\f$ size.

  ##### Return
  - **retval**: `bool`
  - **logLikelihoods**: `Evision.Mat.t()`.

    The optional output matrix that contains a likelihood logarithm value for
    each sample. It has \\f$nsamples \\times 1\\f$ size and CV_64FC1 type.

  - **labels**: `Evision.Mat.t()`.

    The optional output "class label" for each sample:
    \\f$\\texttt{labels}_i=\\texttt{arg max}_k(p_{i,k}), i=1..N\\f$ (indices of the most probable
    mixture component for each sample). It has \\f$nsamples \\times 1\\f$ size and CV_32SC1 type.

  - **probs**: `Evision.Mat.t()`.

    The optional output matrix that contains posterior probabilities of each Gaussian
    mixture component given the each sample. It has \\f$nsamples \\times nclusters\\f$ size and
    CV_64FC1 type.

  This variation starts with Expectation step. You need to provide initial means \\f$a\\_k\\f$ of
  mixture components. Optionally you can pass initial weights \\f$\\pi\\_k\\f$ and covariance matrices
  \\f$S\\_k\\f$ of mixture components.

  Python prototype (for reference only):
  ```python3
  trainE(samples, means0[, covs0[, weights0[, logLikelihoods[, labels[, probs]]]]]) -> retval, logLikelihoods, labels, probs
  ```
  """
  @spec trainE(Evision.ML.EM.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:covs0, term()} | {:weights0, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | false | {:error, String.t()}
  def trainE(self, samples, means0, opts) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and (is_struct(means0, Evision.Mat) or is_struct(means0, Nx.Tensor) or is_number(means0) or is_tuple(means0)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:covs0, :weights0])
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      means0: Evision.Internal.Structurise.from_struct(means0)
    ]
    :evision_nif.ml_ml_EM_trainE(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Estimate the Gaussian mixture parameters from a samples set.

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **samples**: `Evision.Mat`.

    Samples from which the Gaussian mixture model will be estimated. It should be a
    one-channel matrix, each row of which is a sample. If the matrix does not have CV_64F type
    it will be converted to the inner matrix of such type for the further computing.

  - **means0**: `Evision.Mat`.

    Initial means \\f$a_k\\f$ of mixture components. It is a one-channel matrix of
    \\f$nclusters \\times dims\\f$ size. If the matrix does not have CV_64F type it will be
    converted to the inner matrix of such type for the further computing.

  ##### Keyword Arguments
  - **covs0**: `Evision.Mat`.

    The vector of initial covariance matrices \\f$S_k\\f$ of mixture components. Each of
    covariance matrices is a one-channel matrix of \\f$dims \\times dims\\f$ size. If the matrices
    do not have CV_64F type they will be converted to the inner matrices of such type for the
    further computing.

  - **weights0**: `Evision.Mat`.

    Initial weights \\f$\\pi_k\\f$ of mixture components. It should be a one-channel
    floating-point matrix with \\f$1 \\times nclusters\\f$ or \\f$nclusters \\times 1\\f$ size.

  ##### Return
  - **retval**: `bool`
  - **logLikelihoods**: `Evision.Mat.t()`.

    The optional output matrix that contains a likelihood logarithm value for
    each sample. It has \\f$nsamples \\times 1\\f$ size and CV_64FC1 type.

  - **labels**: `Evision.Mat.t()`.

    The optional output "class label" for each sample:
    \\f$\\texttt{labels}_i=\\texttt{arg max}_k(p_{i,k}), i=1..N\\f$ (indices of the most probable
    mixture component for each sample). It has \\f$nsamples \\times 1\\f$ size and CV_32SC1 type.

  - **probs**: `Evision.Mat.t()`.

    The optional output matrix that contains posterior probabilities of each Gaussian
    mixture component given the each sample. It has \\f$nsamples \\times nclusters\\f$ size and
    CV_64FC1 type.

  This variation starts with Expectation step. You need to provide initial means \\f$a\\_k\\f$ of
  mixture components. Optionally you can pass initial weights \\f$\\pi\\_k\\f$ and covariance matrices
  \\f$S\\_k\\f$ of mixture components.

  Python prototype (for reference only):
  ```python3
  trainE(samples, means0[, covs0[, weights0[, logLikelihoods[, labels[, probs]]]]]) -> retval, logLikelihoods, labels, probs
  ```
  """
  @spec trainE(Evision.ML.EM.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | false | {:error, String.t()}
  def trainE(self, samples, means0) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and (is_struct(means0, Evision.Mat) or is_struct(means0, Nx.Tensor) or is_number(means0) or is_tuple(means0))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      means0: Evision.Internal.Structurise.from_struct(means0)
    ]
    :evision_nif.ml_ml_EM_trainE(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Estimate the Gaussian mixture parameters from a samples set.

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **samples**: `Evision.Mat`.

    Samples from which the Gaussian mixture model will be estimated. It should be a
    one-channel matrix, each row of which is a sample. If the matrix does not have CV_64F type
    it will be converted to the inner matrix of such type for the further computing.

  ##### Return
  - **retval**: `bool`
  - **logLikelihoods**: `Evision.Mat.t()`.

    The optional output matrix that contains a likelihood logarithm value for
    each sample. It has \\f$nsamples \\times 1\\f$ size and CV_64FC1 type.

  - **labels**: `Evision.Mat.t()`.

    The optional output "class label" for each sample:
    \\f$\\texttt{labels}_i=\\texttt{arg max}_k(p_{i,k}), i=1..N\\f$ (indices of the most probable
    mixture component for each sample). It has \\f$nsamples \\times 1\\f$ size and CV_32SC1 type.

  - **probs**: `Evision.Mat.t()`.

    The optional output matrix that contains posterior probabilities of each Gaussian
    mixture component given the each sample. It has \\f$nsamples \\times nclusters\\f$ size and
    CV_64FC1 type.

  This variation starts with Expectation step. Initial values of the model parameters will be
  estimated by the k-means algorithm.
  Unlike many of the ML models, %EM is an unsupervised learning algorithm and it does not take
  responses (class labels or function values) as input. Instead, it computes the *Maximum
  Likelihood Estimate* of the Gaussian mixture parameters from an input sample set, stores all the
  parameters inside the structure: \\f$p\\_{i,k}\\f$ in probs, \\f$a\\_k\\f$ in means , \\f$S\\_k\\f$ in
  covs[k], \\f$\\pi\\_k\\f$ in weights , and optionally computes the output "class label" for each
  sample: \\f$\\texttt{labels}\\_i=\\texttt{arg max}\\_k(p\\_{i,k}), i=1..N\\f$ (indices of the most
  probable mixture component for each sample).
  The trained model can be used further for prediction, just like any other classifier. The
  trained model is similar to the NormalBayesClassifier.

  Python prototype (for reference only):
  ```python3
  trainEM(samples[, logLikelihoods[, labels[, probs]]]) -> retval, logLikelihoods, labels, probs
  ```
  """
  @spec trainEM(Evision.ML.EM.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | false | {:error, String.t()}
  def trainEM(self, samples, opts) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_EM_trainEM(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Estimate the Gaussian mixture parameters from a samples set.

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **samples**: `Evision.Mat`.

    Samples from which the Gaussian mixture model will be estimated. It should be a
    one-channel matrix, each row of which is a sample. If the matrix does not have CV_64F type
    it will be converted to the inner matrix of such type for the further computing.

  ##### Return
  - **retval**: `bool`
  - **logLikelihoods**: `Evision.Mat.t()`.

    The optional output matrix that contains a likelihood logarithm value for
    each sample. It has \\f$nsamples \\times 1\\f$ size and CV_64FC1 type.

  - **labels**: `Evision.Mat.t()`.

    The optional output "class label" for each sample:
    \\f$\\texttt{labels}_i=\\texttt{arg max}_k(p_{i,k}), i=1..N\\f$ (indices of the most probable
    mixture component for each sample). It has \\f$nsamples \\times 1\\f$ size and CV_32SC1 type.

  - **probs**: `Evision.Mat.t()`.

    The optional output matrix that contains posterior probabilities of each Gaussian
    mixture component given the each sample. It has \\f$nsamples \\times nclusters\\f$ size and
    CV_64FC1 type.

  This variation starts with Expectation step. Initial values of the model parameters will be
  estimated by the k-means algorithm.
  Unlike many of the ML models, %EM is an unsupervised learning algorithm and it does not take
  responses (class labels or function values) as input. Instead, it computes the *Maximum
  Likelihood Estimate* of the Gaussian mixture parameters from an input sample set, stores all the
  parameters inside the structure: \\f$p\\_{i,k}\\f$ in probs, \\f$a\\_k\\f$ in means , \\f$S\\_k\\f$ in
  covs[k], \\f$\\pi\\_k\\f$ in weights , and optionally computes the output "class label" for each
  sample: \\f$\\texttt{labels}\\_i=\\texttt{arg max}\\_k(p\\_{i,k}), i=1..N\\f$ (indices of the most
  probable mixture component for each sample).
  The trained model can be used further for prediction, just like any other classifier. The
  trained model is similar to the NormalBayesClassifier.

  Python prototype (for reference only):
  ```python3
  trainEM(samples[, logLikelihoods[, labels[, probs]]]) -> retval, logLikelihoods, labels, probs
  ```
  """
  @spec trainEM(Evision.ML.EM.t(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | false | {:error, String.t()}
  def trainEM(self, samples) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_EM_trainEM(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Estimate the Gaussian mixture parameters from a samples set.

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **samples**: `Evision.Mat`.

    Samples from which the Gaussian mixture model will be estimated. It should be a
    one-channel matrix, each row of which is a sample. If the matrix does not have CV_64F type
    it will be converted to the inner matrix of such type for the further computing.

  - **probs0**: `Evision.Mat`.

    the probabilities

  ##### Return
  - **retval**: `bool`
  - **logLikelihoods**: `Evision.Mat.t()`.

    The optional output matrix that contains a likelihood logarithm value for
    each sample. It has \\f$nsamples \\times 1\\f$ size and CV_64FC1 type.

  - **labels**: `Evision.Mat.t()`.

    The optional output "class label" for each sample:
    \\f$\\texttt{labels}_i=\\texttt{arg max}_k(p_{i,k}), i=1..N\\f$ (indices of the most probable
    mixture component for each sample). It has \\f$nsamples \\times 1\\f$ size and CV_32SC1 type.

  - **probs**: `Evision.Mat.t()`.

    The optional output matrix that contains posterior probabilities of each Gaussian
    mixture component given the each sample. It has \\f$nsamples \\times nclusters\\f$ size and
    CV_64FC1 type.

  This variation starts with Maximization step. You need to provide initial probabilities
  \\f$p\\_{i,k}\\f$ to use this option.

  Python prototype (for reference only):
  ```python3
  trainM(samples, probs0[, logLikelihoods[, labels[, probs]]]) -> retval, logLikelihoods, labels, probs
  ```
  """
  @spec trainM(Evision.ML.EM.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | false | {:error, String.t()}
  def trainM(self, samples, probs0, opts) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and (is_struct(probs0, Evision.Mat) or is_struct(probs0, Nx.Tensor) or is_number(probs0) or is_tuple(probs0)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      probs0: Evision.Internal.Structurise.from_struct(probs0)
    ]
    :evision_nif.ml_ml_EM_trainM(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Estimate the Gaussian mixture parameters from a samples set.

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **samples**: `Evision.Mat`.

    Samples from which the Gaussian mixture model will be estimated. It should be a
    one-channel matrix, each row of which is a sample. If the matrix does not have CV_64F type
    it will be converted to the inner matrix of such type for the further computing.

  - **probs0**: `Evision.Mat`.

    the probabilities

  ##### Return
  - **retval**: `bool`
  - **logLikelihoods**: `Evision.Mat.t()`.

    The optional output matrix that contains a likelihood logarithm value for
    each sample. It has \\f$nsamples \\times 1\\f$ size and CV_64FC1 type.

  - **labels**: `Evision.Mat.t()`.

    The optional output "class label" for each sample:
    \\f$\\texttt{labels}_i=\\texttt{arg max}_k(p_{i,k}), i=1..N\\f$ (indices of the most probable
    mixture component for each sample). It has \\f$nsamples \\times 1\\f$ size and CV_32SC1 type.

  - **probs**: `Evision.Mat.t()`.

    The optional output matrix that contains posterior probabilities of each Gaussian
    mixture component given the each sample. It has \\f$nsamples \\times nclusters\\f$ size and
    CV_64FC1 type.

  This variation starts with Maximization step. You need to provide initial probabilities
  \\f$p\\_{i,k}\\f$ to use this option.

  Python prototype (for reference only):
  ```python3
  trainM(samples, probs0[, logLikelihoods[, labels[, probs]]]) -> retval, logLikelihoods, labels, probs
  ```
  """
  @spec trainM(Evision.ML.EM.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t(), Evision.Mat.t()} | false | {:error, String.t()}
  def trainM(self, samples, probs0) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and (is_struct(probs0, Evision.Mat) or is_struct(probs0, Nx.Tensor) or is_number(probs0) or is_tuple(probs0))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      probs0: Evision.Internal.Structurise.from_struct(probs0)
    ]
    :evision_nif.ml_ml_EM_trainM(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.ML.EM.t(), Evision.FileStorage.t(), binary()) :: Evision.ML.EM.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ml_EM_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.EM.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.ML.EM.t(), Evision.FileStorage.t()) :: Evision.ML.EM.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ml_EM_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
