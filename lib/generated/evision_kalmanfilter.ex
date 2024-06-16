defmodule Evision.KalmanFilter do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `KalmanFilter` struct.

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
  def to_struct({:ok, %{class: Evision.KalmanFilter, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.KalmanFilter, ref: ref}) do
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
  KalmanFilter

  ##### Positional Arguments
  - **dynamParams**: `integer()`.

    Dimensionality of the state.

  - **measureParams**: `integer()`.

    Dimensionality of the measurement.

  ##### Keyword Arguments
  - **controlParams**: `integer()`.

    Dimensionality of the control vector.

  - **type**: `integer()`.

    Type of the created matrices that should be CV_32F or CV_64F.

  ##### Return
  - **self**: `Evision.KalmanFilter.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  KalmanFilter(dynamParams, measureParams[, controlParams[, type]]) -> <KalmanFilter object>
  ```
  """
  @spec kalmanFilter(integer(), integer(), [{:controlParams, term()} | {:type, term()}] | nil) :: Evision.KalmanFilter.t() | {:error, String.t()}
  def kalmanFilter(dynamParams, measureParams, opts) when is_integer(dynamParams) and is_integer(measureParams) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:controlParams, :type])
    positional = [
      dynamParams: Evision.Internal.Structurise.from_struct(dynamParams),
      measureParams: Evision.Internal.Structurise.from_struct(measureParams)
    ]
    :evision_nif.kalmanFilter_KalmanFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  KalmanFilter

  ##### Positional Arguments
  - **dynamParams**: `integer()`.

    Dimensionality of the state.

  - **measureParams**: `integer()`.

    Dimensionality of the measurement.

  ##### Keyword Arguments
  - **controlParams**: `integer()`.

    Dimensionality of the control vector.

  - **type**: `integer()`.

    Type of the created matrices that should be CV_32F or CV_64F.

  ##### Return
  - **self**: `Evision.KalmanFilter.t()`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  KalmanFilter(dynamParams, measureParams[, controlParams[, type]]) -> <KalmanFilter object>
  ```
  """
  @spec kalmanFilter(integer(), integer()) :: Evision.KalmanFilter.t() | {:error, String.t()}
  def kalmanFilter(dynamParams, measureParams) when is_integer(dynamParams) and is_integer(measureParams)
  do
    positional = [
      dynamParams: Evision.Internal.Structurise.from_struct(dynamParams),
      measureParams: Evision.Internal.Structurise.from_struct(measureParams)
    ]
    :evision_nif.kalmanFilter_KalmanFilter(positional)
    |> to_struct()
  end

  @doc """
  KalmanFilter
  ##### Return
  - **self**: `Evision.KalmanFilter.t()`

  Python prototype (for reference only):
  ```python3
  KalmanFilter() -> <KalmanFilter object>
  ```
  """
  @spec kalmanFilter() :: Evision.KalmanFilter.t() | {:error, String.t()}
  def kalmanFilter() do
    positional = [
    ]
    :evision_nif.kalmanFilter_KalmanFilter(positional)
    |> to_struct()
  end

  @doc """
  Updates the predicted state from the measurement.

  ##### Positional Arguments
  - **self**: `Evision.KalmanFilter.t()`
  - **measurement**: `Evision.Mat`.

    The measured system parameters

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  correct(measurement) -> retval
  ```
  """
  @spec correct(Evision.KalmanFilter.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def correct(self, measurement) when (is_struct(measurement, Evision.Mat) or is_struct(measurement, Nx.Tensor) or is_number(measurement) or is_tuple(measurement))
  do
    positional = [
      measurement: Evision.Internal.Structurise.from_struct(measurement)
    ]
    :evision_nif.kalmanFilter_correct(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Computes a predicted state.

  ##### Positional Arguments
  - **self**: `Evision.KalmanFilter.t()`

  ##### Keyword Arguments
  - **control**: `Evision.Mat`.

    The optional input control

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  predict([, control]) -> retval
  ```
  """
  @spec predict(Evision.KalmanFilter.t(), [{:control, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def predict(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:control])
    positional = [
    ]
    :evision_nif.kalmanFilter_predict(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes a predicted state.

  ##### Positional Arguments
  - **self**: `Evision.KalmanFilter.t()`

  ##### Keyword Arguments
  - **control**: `Evision.Mat`.

    The optional input control

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  predict([, control]) -> retval
  ```
  """
  @spec predict(Evision.KalmanFilter.t()) :: Evision.Mat.t() | {:error, String.t()}
  def predict(self) do
    positional = [
    ]
    :evision_nif.kalmanFilter_predict(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_controlMatrix(Evision.KalmanFilter.t()) :: Evision.Mat.t()
  def get_controlMatrix(self) do
    :evision_nif.kalmanFilter_get_controlMatrix(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_controlMatrix(Evision.KalmanFilter.t(), Evision.Mat.maybe_mat_in()) :: Evision.KalmanFilter.t()
  def set_controlMatrix(self, prop) do
    :evision_nif.kalmanFilter_set_controlMatrix(
        Evision.Internal.Structurise.from_struct(self),
        [controlMatrix: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_errorCovPost(Evision.KalmanFilter.t()) :: Evision.Mat.t()
  def get_errorCovPost(self) do
    :evision_nif.kalmanFilter_get_errorCovPost(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_errorCovPost(Evision.KalmanFilter.t(), Evision.Mat.maybe_mat_in()) :: Evision.KalmanFilter.t()
  def set_errorCovPost(self, prop) do
    :evision_nif.kalmanFilter_set_errorCovPost(
        Evision.Internal.Structurise.from_struct(self),
        [errorCovPost: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_errorCovPre(Evision.KalmanFilter.t()) :: Evision.Mat.t()
  def get_errorCovPre(self) do
    :evision_nif.kalmanFilter_get_errorCovPre(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_errorCovPre(Evision.KalmanFilter.t(), Evision.Mat.maybe_mat_in()) :: Evision.KalmanFilter.t()
  def set_errorCovPre(self, prop) do
    :evision_nif.kalmanFilter_set_errorCovPre(
        Evision.Internal.Structurise.from_struct(self),
        [errorCovPre: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_gain(Evision.KalmanFilter.t()) :: Evision.Mat.t()
  def get_gain(self) do
    :evision_nif.kalmanFilter_get_gain(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_gain(Evision.KalmanFilter.t(), Evision.Mat.maybe_mat_in()) :: Evision.KalmanFilter.t()
  def set_gain(self, prop) do
    :evision_nif.kalmanFilter_set_gain(
        Evision.Internal.Structurise.from_struct(self),
        [gain: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_measurementMatrix(Evision.KalmanFilter.t()) :: Evision.Mat.t()
  def get_measurementMatrix(self) do
    :evision_nif.kalmanFilter_get_measurementMatrix(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_measurementMatrix(Evision.KalmanFilter.t(), Evision.Mat.maybe_mat_in()) :: Evision.KalmanFilter.t()
  def set_measurementMatrix(self, prop) do
    :evision_nif.kalmanFilter_set_measurementMatrix(
        Evision.Internal.Structurise.from_struct(self),
        [measurementMatrix: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_measurementNoiseCov(Evision.KalmanFilter.t()) :: Evision.Mat.t()
  def get_measurementNoiseCov(self) do
    :evision_nif.kalmanFilter_get_measurementNoiseCov(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_measurementNoiseCov(Evision.KalmanFilter.t(), Evision.Mat.maybe_mat_in()) :: Evision.KalmanFilter.t()
  def set_measurementNoiseCov(self, prop) do
    :evision_nif.kalmanFilter_set_measurementNoiseCov(
        Evision.Internal.Structurise.from_struct(self),
        [measurementNoiseCov: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_processNoiseCov(Evision.KalmanFilter.t()) :: Evision.Mat.t()
  def get_processNoiseCov(self) do
    :evision_nif.kalmanFilter_get_processNoiseCov(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_processNoiseCov(Evision.KalmanFilter.t(), Evision.Mat.maybe_mat_in()) :: Evision.KalmanFilter.t()
  def set_processNoiseCov(self, prop) do
    :evision_nif.kalmanFilter_set_processNoiseCov(
        Evision.Internal.Structurise.from_struct(self),
        [processNoiseCov: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_statePost(Evision.KalmanFilter.t()) :: Evision.Mat.t()
  def get_statePost(self) do
    :evision_nif.kalmanFilter_get_statePost(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_statePost(Evision.KalmanFilter.t(), Evision.Mat.maybe_mat_in()) :: Evision.KalmanFilter.t()
  def set_statePost(self, prop) do
    :evision_nif.kalmanFilter_set_statePost(
        Evision.Internal.Structurise.from_struct(self),
        [statePost: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_statePre(Evision.KalmanFilter.t()) :: Evision.Mat.t()
  def get_statePre(self) do
    :evision_nif.kalmanFilter_get_statePre(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_statePre(Evision.KalmanFilter.t(), Evision.Mat.maybe_mat_in()) :: Evision.KalmanFilter.t()
  def set_statePre(self, prop) do
    :evision_nif.kalmanFilter_set_statePre(
        Evision.Internal.Structurise.from_struct(self),
        [statePre: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_transitionMatrix(Evision.KalmanFilter.t()) :: Evision.Mat.t()
  def get_transitionMatrix(self) do
    :evision_nif.kalmanFilter_get_transitionMatrix(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_transitionMatrix(Evision.KalmanFilter.t(), Evision.Mat.maybe_mat_in()) :: Evision.KalmanFilter.t()
  def set_transitionMatrix(self, prop) do
    :evision_nif.kalmanFilter_set_transitionMatrix(
        Evision.Internal.Structurise.from_struct(self),
        [transitionMatrix: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
