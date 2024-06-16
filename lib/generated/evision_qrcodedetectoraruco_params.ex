defmodule Evision.QRCodeDetectorAruco.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `QRCodeDetectorAruco.Params` struct.

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
  def to_struct({:ok, %{class: Evision.QRCodeDetectorAruco.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.QRCodeDetectorAruco.Params, ref: ref}) do
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
  QRCodeDetectorAruco_Params
  ##### Return
  - **self**: `QRCodeDetectorAruco_Params`

  Python prototype (for reference only):
  ```python3
  QRCodeDetectorAruco_Params() -> <QRCodeDetectorAruco_Params object>
  ```
  """
  @spec qrcodedetectoraruco_params() :: Evision.QRCodeDetectorAruco.Params.t() | {:error, String.t()}
  def qrcodedetectoraruco_params() do
    positional = [
    ]
    :evision_nif.qrCodeDetectorAruco_Params_QRCodeDetectorAruco_Params(positional)
    |> to_struct()
  end
  @spec get_maxColorsMismatch(Evision.QRCodeDetectorAruco.Params.t()) :: number()
  def get_maxColorsMismatch(self) do
    :evision_nif.qrCodeDetectorAruco_Params_get_maxColorsMismatch(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxColorsMismatch(Evision.QRCodeDetectorAruco.Params.t(), number()) :: Evision.QRCodeDetectorAruco.Params.t()
  def set_maxColorsMismatch(self, prop) do
    :evision_nif.qrCodeDetectorAruco_Params_set_maxColorsMismatch(
        Evision.Internal.Structurise.from_struct(self),
        [maxColorsMismatch: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxModuleSizeMismatch(Evision.QRCodeDetectorAruco.Params.t()) :: number()
  def get_maxModuleSizeMismatch(self) do
    :evision_nif.qrCodeDetectorAruco_Params_get_maxModuleSizeMismatch(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxModuleSizeMismatch(Evision.QRCodeDetectorAruco.Params.t(), number()) :: Evision.QRCodeDetectorAruco.Params.t()
  def set_maxModuleSizeMismatch(self, prop) do
    :evision_nif.qrCodeDetectorAruco_Params_set_maxModuleSizeMismatch(
        Evision.Internal.Structurise.from_struct(self),
        [maxModuleSizeMismatch: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxPenalties(Evision.QRCodeDetectorAruco.Params.t()) :: number()
  def get_maxPenalties(self) do
    :evision_nif.qrCodeDetectorAruco_Params_get_maxPenalties(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxPenalties(Evision.QRCodeDetectorAruco.Params.t(), number()) :: Evision.QRCodeDetectorAruco.Params.t()
  def set_maxPenalties(self, prop) do
    :evision_nif.qrCodeDetectorAruco_Params_set_maxPenalties(
        Evision.Internal.Structurise.from_struct(self),
        [maxPenalties: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxRotation(Evision.QRCodeDetectorAruco.Params.t()) :: number()
  def get_maxRotation(self) do
    :evision_nif.qrCodeDetectorAruco_Params_get_maxRotation(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxRotation(Evision.QRCodeDetectorAruco.Params.t(), number()) :: Evision.QRCodeDetectorAruco.Params.t()
  def set_maxRotation(self, prop) do
    :evision_nif.qrCodeDetectorAruco_Params_set_maxRotation(
        Evision.Internal.Structurise.from_struct(self),
        [maxRotation: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_maxTimingPatternMismatch(Evision.QRCodeDetectorAruco.Params.t()) :: number()
  def get_maxTimingPatternMismatch(self) do
    :evision_nif.qrCodeDetectorAruco_Params_get_maxTimingPatternMismatch(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_maxTimingPatternMismatch(Evision.QRCodeDetectorAruco.Params.t(), number()) :: Evision.QRCodeDetectorAruco.Params.t()
  def set_maxTimingPatternMismatch(self, prop) do
    :evision_nif.qrCodeDetectorAruco_Params_set_maxTimingPatternMismatch(
        Evision.Internal.Structurise.from_struct(self),
        [maxTimingPatternMismatch: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_minModuleSizeInPyramid(Evision.QRCodeDetectorAruco.Params.t()) :: number()
  def get_minModuleSizeInPyramid(self) do
    :evision_nif.qrCodeDetectorAruco_Params_get_minModuleSizeInPyramid(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_minModuleSizeInPyramid(Evision.QRCodeDetectorAruco.Params.t(), number()) :: Evision.QRCodeDetectorAruco.Params.t()
  def set_minModuleSizeInPyramid(self, prop) do
    :evision_nif.qrCodeDetectorAruco_Params_set_minModuleSizeInPyramid(
        Evision.Internal.Structurise.from_struct(self),
        [minModuleSizeInPyramid: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_scaleTimingPatternScore(Evision.QRCodeDetectorAruco.Params.t()) :: number()
  def get_scaleTimingPatternScore(self) do
    :evision_nif.qrCodeDetectorAruco_Params_get_scaleTimingPatternScore(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_scaleTimingPatternScore(Evision.QRCodeDetectorAruco.Params.t(), number()) :: Evision.QRCodeDetectorAruco.Params.t()
  def set_scaleTimingPatternScore(self, prop) do
    :evision_nif.qrCodeDetectorAruco_Params_set_scaleTimingPatternScore(
        Evision.Internal.Structurise.from_struct(self),
        [scaleTimingPatternScore: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
