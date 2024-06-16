defmodule Evision.QRCodeEncoder.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `QRCodeEncoder.Params` struct.

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
  def to_struct({:ok, %{class: Evision.QRCodeEncoder.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.QRCodeEncoder.Params, ref: ref}) do
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
  QRCodeEncoder_Params
  ##### Return
  - **self**: `Evision.QRCodeEncoder.Params.t()`

  Python prototype (for reference only):
  ```python3
  QRCodeEncoder_Params() -> <QRCodeEncoder_Params object>
  ```
  """
  @spec params() :: Evision.QRCodeEncoder.Params.t() | {:error, String.t()}
  def params() do
    positional = [
    ]
    :evision_nif.qrCodeEncoder_Params_QRCodeEncoder_Params(positional)
    |> to_struct()
  end
  @spec get_correction_level(Evision.QRCodeEncoder.Params.t()) :: Evision.QRCodeEncoder.CorrectionLevel.enum()
  def get_correction_level(self) do
    :evision_nif.qrCodeEncoder_Params_get_correction_level(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_correction_level(Evision.QRCodeEncoder.Params.t(), Evision.QRCodeEncoder.CorrectionLevel.enum()) :: Evision.QRCodeEncoder.Params.t()
  def set_correction_level(self, prop) do
    :evision_nif.qrCodeEncoder_Params_set_correction_level(
        Evision.Internal.Structurise.from_struct(self),
        [correction_level: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_mode(Evision.QRCodeEncoder.Params.t()) :: Evision.QRCodeEncoder.EncodeMode.enum()
  def get_mode(self) do
    :evision_nif.qrCodeEncoder_Params_get_mode(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_mode(Evision.QRCodeEncoder.Params.t(), Evision.QRCodeEncoder.EncodeMode.enum()) :: Evision.QRCodeEncoder.Params.t()
  def set_mode(self, prop) do
    :evision_nif.qrCodeEncoder_Params_set_mode(
        Evision.Internal.Structurise.from_struct(self),
        [mode: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_structure_number(Evision.QRCodeEncoder.Params.t()) :: integer()
  def get_structure_number(self) do
    :evision_nif.qrCodeEncoder_Params_get_structure_number(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_structure_number(Evision.QRCodeEncoder.Params.t(), integer()) :: Evision.QRCodeEncoder.Params.t()
  def set_structure_number(self, prop) do
    :evision_nif.qrCodeEncoder_Params_set_structure_number(
        Evision.Internal.Structurise.from_struct(self),
        [structure_number: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_version(Evision.QRCodeEncoder.Params.t()) :: integer()
  def get_version(self) do
    :evision_nif.qrCodeEncoder_Params_get_version(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_version(Evision.QRCodeEncoder.Params.t(), integer()) :: Evision.QRCodeEncoder.Params.t()
  def set_version(self, prop) do
    :evision_nif.qrCodeEncoder_Params_set_version(
        Evision.Internal.Structurise.from_struct(self),
        [version: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
