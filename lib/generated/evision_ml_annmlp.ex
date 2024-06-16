defmodule Evision.ML.ANNMLP do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ML.ANNMLP` struct.

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
  def to_struct({:ok, %{class: Evision.ML.ANNMLP, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ML.ANNMLP, ref: ref}) do
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
  - **self**: `Evision.ML.ANNMLP.t()`
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
  @spec calcError(Evision.ML.ANNMLP.t(), Evision.ML.TrainData.t(), boolean(), [{atom(), term()},...] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test, opts) when is_struct(data, Evision.ML.TrainData) and is_boolean(test) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_ANN_MLP_calcError(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes error on the training or test dataset

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
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
  @spec calcError(Evision.ML.ANNMLP.t(), Evision.ML.TrainData.t(), boolean()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def calcError(self, data, test) when is_struct(data, Evision.ML.TrainData) and is_boolean(test)
  do
    positional = [
      data: Evision.Internal.Structurise.from_struct(data),
      test: Evision.Internal.Structurise.from_struct(test)
    ]
    :evision_nif.ml_ml_ANN_MLP_calcError(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.ML.ANNMLP.t()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ml_ANN_MLP_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Creates empty model
  ##### Return
  - **retval**: `Evision.ML.ANNMLP.t()`

  Use StatModel::train to train the model, Algorithm::load\\<ANN_MLP\\>(filename) to load the pre-trained model.
  Note that the train method has optional flags: ANN_MLP::TrainFlags.

  Python prototype (for reference only):
  ```python3
  create() -> retval
  ```
  """
  @spec create() :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_create_static(positional)
    |> to_struct()
  end

  @doc """
  empty

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.ML.ANNMLP.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getAnnealCoolingRatio

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `double`

  @see `setAnnealCoolingRatio/2`

  Python prototype (for reference only):
  ```python3
  getAnnealCoolingRatio() -> retval
  ```
  """
  @spec getAnnealCoolingRatio(Evision.ML.ANNMLP.t()) :: number() | {:error, String.t()}
  def getAnnealCoolingRatio(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getAnnealCoolingRatio(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getAnnealFinalT

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `double`

  @see `setAnnealFinalT/2`

  Python prototype (for reference only):
  ```python3
  getAnnealFinalT() -> retval
  ```
  """
  @spec getAnnealFinalT(Evision.ML.ANNMLP.t()) :: number() | {:error, String.t()}
  def getAnnealFinalT(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getAnnealFinalT(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getAnnealInitialT

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `double`

  @see `setAnnealInitialT/2`

  Python prototype (for reference only):
  ```python3
  getAnnealInitialT() -> retval
  ```
  """
  @spec getAnnealInitialT(Evision.ML.ANNMLP.t()) :: number() | {:error, String.t()}
  def getAnnealInitialT(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getAnnealInitialT(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getAnnealItePerStep

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `integer()`

  @see `setAnnealItePerStep/2`

  Python prototype (for reference only):
  ```python3
  getAnnealItePerStep() -> retval
  ```
  """
  @spec getAnnealItePerStep(Evision.ML.ANNMLP.t()) :: integer() | {:error, String.t()}
  def getAnnealItePerStep(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getAnnealItePerStep(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBackpropMomentumScale

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `double`

  @see `setBackpropMomentumScale/2`

  Python prototype (for reference only):
  ```python3
  getBackpropMomentumScale() -> retval
  ```
  """
  @spec getBackpropMomentumScale(Evision.ML.ANNMLP.t()) :: number() | {:error, String.t()}
  def getBackpropMomentumScale(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getBackpropMomentumScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getBackpropWeightScale

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `double`

  @see `setBackpropWeightScale/2`

  Python prototype (for reference only):
  ```python3
  getBackpropWeightScale() -> retval
  ```
  """
  @spec getBackpropWeightScale(Evision.ML.ANNMLP.t()) :: number() | {:error, String.t()}
  def getBackpropWeightScale(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getBackpropWeightScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.ML.ANNMLP.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ml_ANN_MLP_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getLayerSizes

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Integer vector specifying the number of neurons in each layer including the input and output layers.
  The very first element specifies the number of elements in the input layer.
  The last element - number of elements in the output layer.
  @sa setLayerSizes

  Python prototype (for reference only):
  ```python3
  getLayerSizes() -> retval
  ```
  """
  @spec getLayerSizes(Evision.ML.ANNMLP.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getLayerSizes(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getLayerSizes(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRpropDW0

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `double`

  @see `setRpropDW0/2`

  Python prototype (for reference only):
  ```python3
  getRpropDW0() -> retval
  ```
  """
  @spec getRpropDW0(Evision.ML.ANNMLP.t()) :: number() | {:error, String.t()}
  def getRpropDW0(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getRpropDW0(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRpropDWMax

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `double`

  @see `setRpropDWMax/2`

  Python prototype (for reference only):
  ```python3
  getRpropDWMax() -> retval
  ```
  """
  @spec getRpropDWMax(Evision.ML.ANNMLP.t()) :: number() | {:error, String.t()}
  def getRpropDWMax(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getRpropDWMax(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRpropDWMin

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `double`

  @see `setRpropDWMin/2`

  Python prototype (for reference only):
  ```python3
  getRpropDWMin() -> retval
  ```
  """
  @spec getRpropDWMin(Evision.ML.ANNMLP.t()) :: number() | {:error, String.t()}
  def getRpropDWMin(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getRpropDWMin(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRpropDWMinus

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `double`

  @see `setRpropDWMinus/2`

  Python prototype (for reference only):
  ```python3
  getRpropDWMinus() -> retval
  ```
  """
  @spec getRpropDWMinus(Evision.ML.ANNMLP.t()) :: number() | {:error, String.t()}
  def getRpropDWMinus(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getRpropDWMinus(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getRpropDWPlus

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `double`

  @see `setRpropDWPlus/2`

  Python prototype (for reference only):
  ```python3
  getRpropDWPlus() -> retval
  ```
  """
  @spec getRpropDWPlus(Evision.ML.ANNMLP.t()) :: number() | {:error, String.t()}
  def getRpropDWPlus(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getRpropDWPlus(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTermCriteria

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `TermCriteria`

  @see `setTermCriteria/2`

  Python prototype (for reference only):
  ```python3
  getTermCriteria() -> retval
  ```
  """
  @spec getTermCriteria(Evision.ML.ANNMLP.t()) :: {integer(), integer(), number()} | {:error, String.t()}
  def getTermCriteria(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getTermCriteria(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTrainMethod

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `integer()`

  Returns current training method

  Python prototype (for reference only):
  ```python3
  getTrainMethod() -> retval
  ```
  """
  @spec getTrainMethod(Evision.ML.ANNMLP.t()) :: integer() | {:error, String.t()}
  def getTrainMethod(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getTrainMethod(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the number of variables in training samples

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getVarCount() -> retval
  ```
  """
  @spec getVarCount(Evision.ML.ANNMLP.t()) :: integer() | {:error, String.t()}
  def getVarCount(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_getVarCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getWeights

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **layerIdx**: `integer()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getWeights(layerIdx) -> retval
  ```
  """
  @spec getWeights(Evision.ML.ANNMLP.t(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def getWeights(self, layerIdx) when is_integer(layerIdx)
  do
    positional = [
      layerIdx: Evision.Internal.Structurise.from_struct(layerIdx)
    ]
    :evision_nif.ml_ml_ANN_MLP_getWeights(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is classifier

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isClassifier() -> retval
  ```
  """
  @spec isClassifier(Evision.ML.ANNMLP.t()) :: boolean() | {:error, String.t()}
  def isClassifier(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_isClassifier(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the model is trained

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isTrained() -> retval
  ```
  """
  @spec isTrained(Evision.ML.ANNMLP.t()) :: boolean() | {:error, String.t()}
  def isTrained(self) do
    positional = [
    ]
    :evision_nif.ml_ml_ANN_MLP_isTrained(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Loads and creates a serialized ANN from a file

  ##### Positional Arguments
  - **filepath**: `String`.

    path to serialized ANN

  ##### Return
  - **retval**: `Evision.ML.ANNMLP.t()`

   Use ANN::save to serialize and store an ANN to disk.
   Load the ANN from this file again, by calling this function with the path to the file.

  Python prototype (for reference only):
  ```python3
  load(filepath) -> retval
  ```
  """
  @spec load(binary()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def load(filepath) when is_binary(filepath)
  do
    positional = [
      filepath: Evision.Internal.Structurise.from_struct(filepath)
    ]
    :evision_nif.ml_ml_ANN_MLP_load_static(positional)
    |> to_struct()
  end

  @doc """
  Predicts response(s) for the provided sample(s)

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
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
  @spec predict(Evision.ML.ANNMLP.t(), Evision.Mat.maybe_mat_in(), [{:flags, term()}] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples, opts) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_ANN_MLP_predict(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Predicts response(s) for the provided sample(s)

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
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
  @spec predict(Evision.ML.ANNMLP.t(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def predict(self, samples) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples)
    ]
    :evision_nif.ml_ml_ANN_MLP_predict(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.ML.ANNMLP.t(), Evision.FileNode.t()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ml_ANN_MLP_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.ML.ANNMLP.t(), binary()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ml_ANN_MLP_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setActivationFunction

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **type**: `integer()`.

    The type of activation function. See ANN_MLP::ActivationFunctions.

  ##### Keyword Arguments
  - **param1**: `double`.

    The first parameter of the activation function, \\f$\\alpha\\f$. Default value is 0.

  - **param2**: `double`.

    The second parameter of the activation function, \\f$\\beta\\f$. Default value is 0.

  Initialize the activation function for each neuron.
  Currently the default and the only fully supported activation function is ANN_MLP::SIGMOID_SYM.

  Python prototype (for reference only):
  ```python3
  setActivationFunction(type[, param1[, param2]]) -> None
  ```
  """
  @spec setActivationFunction(Evision.ML.ANNMLP.t(), integer(), [{:param1, term()} | {:param2, term()}] | nil) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setActivationFunction(self, type, opts) when is_integer(type) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:param1, :param2])
    positional = [
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.ml_ml_ANN_MLP_setActivationFunction(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  setActivationFunction

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **type**: `integer()`.

    The type of activation function. See ANN_MLP::ActivationFunctions.

  ##### Keyword Arguments
  - **param1**: `double`.

    The first parameter of the activation function, \\f$\\alpha\\f$. Default value is 0.

  - **param2**: `double`.

    The second parameter of the activation function, \\f$\\beta\\f$. Default value is 0.

  Initialize the activation function for each neuron.
  Currently the default and the only fully supported activation function is ANN_MLP::SIGMOID_SYM.

  Python prototype (for reference only):
  ```python3
  setActivationFunction(type[, param1[, param2]]) -> None
  ```
  """
  @spec setActivationFunction(Evision.ML.ANNMLP.t(), integer()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setActivationFunction(self, type) when is_integer(type)
  do
    positional = [
      type: Evision.Internal.Structurise.from_struct(type)
    ]
    :evision_nif.ml_ml_ANN_MLP_setActivationFunction(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setAnnealCoolingRatio

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **val**: `double`

  @see `getAnnealCoolingRatio/1`

  Python prototype (for reference only):
  ```python3
  setAnnealCoolingRatio(val) -> None
  ```
  """
  @spec setAnnealCoolingRatio(Evision.ML.ANNMLP.t(), number()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setAnnealCoolingRatio(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_ANN_MLP_setAnnealCoolingRatio(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setAnnealFinalT

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **val**: `double`

  @see `getAnnealFinalT/1`

  Python prototype (for reference only):
  ```python3
  setAnnealFinalT(val) -> None
  ```
  """
  @spec setAnnealFinalT(Evision.ML.ANNMLP.t(), number()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setAnnealFinalT(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_ANN_MLP_setAnnealFinalT(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setAnnealInitialT

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **val**: `double`

  @see `getAnnealInitialT/1`

  Python prototype (for reference only):
  ```python3
  setAnnealInitialT(val) -> None
  ```
  """
  @spec setAnnealInitialT(Evision.ML.ANNMLP.t(), number()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setAnnealInitialT(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_ANN_MLP_setAnnealInitialT(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setAnnealItePerStep

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **val**: `integer()`

  @see `getAnnealItePerStep/1`

  Python prototype (for reference only):
  ```python3
  setAnnealItePerStep(val) -> None
  ```
  """
  @spec setAnnealItePerStep(Evision.ML.ANNMLP.t(), integer()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setAnnealItePerStep(self, val) when is_integer(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_ANN_MLP_setAnnealItePerStep(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBackpropMomentumScale

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **val**: `double`

  @see `getBackpropMomentumScale/1`

  Python prototype (for reference only):
  ```python3
  setBackpropMomentumScale(val) -> None
  ```
  """
  @spec setBackpropMomentumScale(Evision.ML.ANNMLP.t(), number()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setBackpropMomentumScale(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_ANN_MLP_setBackpropMomentumScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setBackpropWeightScale

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **val**: `double`

  @see `getBackpropWeightScale/1`

  Python prototype (for reference only):
  ```python3
  setBackpropWeightScale(val) -> None
  ```
  """
  @spec setBackpropWeightScale(Evision.ML.ANNMLP.t(), number()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setBackpropWeightScale(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_ANN_MLP_setBackpropWeightScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setLayerSizes

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **layer_sizes**: `Evision.Mat`

  Integer vector specifying the number of neurons in each layer including the input and output layers.
  The very first element specifies the number of elements in the input layer.
  The last element - number of elements in the output layer. Default value is empty Mat.
  @sa getLayerSizes

  Python prototype (for reference only):
  ```python3
  setLayerSizes(_layer_sizes) -> None
  ```
  """
  @spec setLayerSizes(Evision.ML.ANNMLP.t(), Evision.Mat.maybe_mat_in()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setLayerSizes(self, layer_sizes) when (is_struct(layer_sizes, Evision.Mat) or is_struct(layer_sizes, Nx.Tensor) or is_number(layer_sizes) or is_tuple(layer_sizes))
  do
    positional = [
      layer_sizes: Evision.Internal.Structurise.from_struct(layer_sizes)
    ]
    :evision_nif.ml_ml_ANN_MLP_setLayerSizes(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRpropDW0

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **val**: `double`

  @see `getRpropDW0/1`

  Python prototype (for reference only):
  ```python3
  setRpropDW0(val) -> None
  ```
  """
  @spec setRpropDW0(Evision.ML.ANNMLP.t(), number()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setRpropDW0(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_ANN_MLP_setRpropDW0(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRpropDWMax

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **val**: `double`

  @see `getRpropDWMax/1`

  Python prototype (for reference only):
  ```python3
  setRpropDWMax(val) -> None
  ```
  """
  @spec setRpropDWMax(Evision.ML.ANNMLP.t(), number()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setRpropDWMax(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_ANN_MLP_setRpropDWMax(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRpropDWMin

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **val**: `double`

  @see `getRpropDWMin/1`

  Python prototype (for reference only):
  ```python3
  setRpropDWMin(val) -> None
  ```
  """
  @spec setRpropDWMin(Evision.ML.ANNMLP.t(), number()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setRpropDWMin(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_ANN_MLP_setRpropDWMin(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRpropDWMinus

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **val**: `double`

  @see `getRpropDWMinus/1`

  Python prototype (for reference only):
  ```python3
  setRpropDWMinus(val) -> None
  ```
  """
  @spec setRpropDWMinus(Evision.ML.ANNMLP.t(), number()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setRpropDWMinus(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_ANN_MLP_setRpropDWMinus(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setRpropDWPlus

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **val**: `double`

  @see `getRpropDWPlus/1`

  Python prototype (for reference only):
  ```python3
  setRpropDWPlus(val) -> None
  ```
  """
  @spec setRpropDWPlus(Evision.ML.ANNMLP.t(), number()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setRpropDWPlus(self, val) when is_number(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_ANN_MLP_setRpropDWPlus(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTermCriteria

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **val**: `TermCriteria`

  @see `getTermCriteria/1`

  Python prototype (for reference only):
  ```python3
  setTermCriteria(val) -> None
  ```
  """
  @spec setTermCriteria(Evision.ML.ANNMLP.t(), {integer(), integer(), number()}) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setTermCriteria(self, val) when is_tuple(val)
  do
    positional = [
      val: Evision.Internal.Structurise.from_struct(val)
    ]
    :evision_nif.ml_ml_ANN_MLP_setTermCriteria(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setTrainMethod

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **method**: `integer()`.

    Default value is ANN_MLP::RPROP. See ANN_MLP::TrainingMethods.

  ##### Keyword Arguments
  - **param1**: `double`.

    passed to setRpropDW0 for ANN_MLP::RPROP and to setBackpropWeightScale for ANN_MLP::BACKPROP and to initialT for ANN_MLP::ANNEAL.

  - **param2**: `double`.

    passed to setRpropDWMin for ANN_MLP::RPROP and to setBackpropMomentumScale for ANN_MLP::BACKPROP and to finalT for ANN_MLP::ANNEAL.

  Sets training method and common parameters.

  Python prototype (for reference only):
  ```python3
  setTrainMethod(method[, param1[, param2]]) -> None
  ```
  """
  @spec setTrainMethod(Evision.ML.ANNMLP.t(), integer(), [{:param1, term()} | {:param2, term()}] | nil) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setTrainMethod(self, method, opts) when is_integer(method) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:param1, :param2])
    positional = [
      method: Evision.Internal.Structurise.from_struct(method)
    ]
    :evision_nif.ml_ml_ANN_MLP_setTrainMethod(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  setTrainMethod

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **method**: `integer()`.

    Default value is ANN_MLP::RPROP. See ANN_MLP::TrainingMethods.

  ##### Keyword Arguments
  - **param1**: `double`.

    passed to setRpropDW0 for ANN_MLP::RPROP and to setBackpropWeightScale for ANN_MLP::BACKPROP and to initialT for ANN_MLP::ANNEAL.

  - **param2**: `double`.

    passed to setRpropDWMin for ANN_MLP::RPROP and to setBackpropMomentumScale for ANN_MLP::BACKPROP and to finalT for ANN_MLP::ANNEAL.

  Sets training method and common parameters.

  Python prototype (for reference only):
  ```python3
  setTrainMethod(method[, param1[, param2]]) -> None
  ```
  """
  @spec setTrainMethod(Evision.ML.ANNMLP.t(), integer()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def setTrainMethod(self, method) when is_integer(method)
  do
    positional = [
      method: Evision.Internal.Structurise.from_struct(method)
    ]
    :evision_nif.ml_ml_ANN_MLP_setTrainMethod(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
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
  @spec train(Evision.ML.ANNMLP.t(), Evision.Mat.maybe_mat_in(), integer(), Evision.Mat.maybe_mat_in()) :: boolean() | {:error, String.t()}
  def train(self, samples, layout, responses) when (is_struct(samples, Evision.Mat) or is_struct(samples, Nx.Tensor) or is_number(samples) or is_tuple(samples)) and is_integer(layout) and (is_struct(responses, Evision.Mat) or is_struct(responses, Nx.Tensor) or is_number(responses) or is_tuple(responses))
  do
    positional = [
      samples: Evision.Internal.Structurise.from_struct(samples),
      layout: Evision.Internal.Structurise.from_struct(layout),
      responses: Evision.Internal.Structurise.from_struct(responses)
    ]
    :evision_nif.ml_ml_ANN_MLP_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
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
  @spec train(Evision.ML.ANNMLP.t(), Evision.ML.TrainData.t(), [{:flags, term()}] | nil) :: boolean() | {:error, String.t()}
  def train(self, trainData, opts) when is_struct(trainData, Evision.ML.TrainData) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags])
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_ANN_MLP_train(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Trains the statistical model

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
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
  @spec train(Evision.ML.ANNMLP.t(), Evision.ML.TrainData.t()) :: boolean() | {:error, String.t()}
  def train(self, trainData) when is_struct(trainData, Evision.ML.TrainData)
  do
    positional = [
      trainData: Evision.Internal.Structurise.from_struct(trainData)
    ]
    :evision_nif.ml_ml_ANN_MLP_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.ML.ANNMLP.t(), Evision.FileStorage.t(), binary()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ml_ANN_MLP_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.ML.ANNMLP.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.ML.ANNMLP.t(), Evision.FileStorage.t()) :: Evision.ML.ANNMLP.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ml_ANN_MLP_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
