defmodule Evision.QRCodeEncoder do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `QRCodeEncoder` struct.

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
  def to_struct({:ok, %{class: Evision.QRCodeEncoder, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.QRCodeEncoder, ref: ref}) do
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
  Constructor
  ##### Keyword Arguments
  - **parameters**: `Evision.QRCodeEncoder.Params.t()`.

    QR code encoder parameters QRCodeEncoder::Params

  ##### Return
  - **retval**: `Evision.QRCodeEncoder.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create([{:parameters, term()}] | nil) :: Evision.QRCodeEncoder.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:parameters])
    positional = [
    ]
    :evision_nif.qrCodeEncoder_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Constructor
  ##### Keyword Arguments
  - **parameters**: `Evision.QRCodeEncoder.Params.t()`.

    QR code encoder parameters QRCodeEncoder::Params

  ##### Return
  - **retval**: `Evision.QRCodeEncoder.t()`

  Python prototype (for reference only):
  ```python3
  create([, parameters]) -> retval
  ```
  """
  @spec create() :: Evision.QRCodeEncoder.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.qrCodeEncoder_create_static(positional)
    |> to_struct()
  end

  @doc """
  Generates QR code from input string.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeEncoder.t()`
  - **encoded_info**: `String`.

    Input string to encode.

  ##### Return
  - **qrcode**: `Evision.Mat.t()`.

    Generated QR code.

  Python prototype (for reference only):
  ```python3
  encode(encoded_info[, qrcode]) -> qrcode
  ```
  """
  @spec encode(Evision.QRCodeEncoder.t(), binary(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def encode(self, encoded_info, opts) when is_binary(encoded_info) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      encoded_info: Evision.Internal.Structurise.from_struct(encoded_info)
    ]
    :evision_nif.qrCodeEncoder_encode(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Generates QR code from input string.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeEncoder.t()`
  - **encoded_info**: `String`.

    Input string to encode.

  ##### Return
  - **qrcode**: `Evision.Mat.t()`.

    Generated QR code.

  Python prototype (for reference only):
  ```python3
  encode(encoded_info[, qrcode]) -> qrcode
  ```
  """
  @spec encode(Evision.QRCodeEncoder.t(), binary()) :: Evision.Mat.t() | {:error, String.t()}
  def encode(self, encoded_info) when is_binary(encoded_info)
  do
    positional = [
      encoded_info: Evision.Internal.Structurise.from_struct(encoded_info)
    ]
    :evision_nif.qrCodeEncoder_encode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Generates QR code from input string in Structured Append mode. The encoded message is splitting over a number of QR codes.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeEncoder.t()`
  - **encoded_info**: `String`.

    Input string to encode.

  ##### Return
  - **qrcodes**: `[Evision.Mat]`.

    Vector of generated QR codes.

  Python prototype (for reference only):
  ```python3
  encodeStructuredAppend(encoded_info[, qrcodes]) -> qrcodes
  ```
  """
  @spec encodeStructuredAppend(Evision.QRCodeEncoder.t(), binary(), [{atom(), term()},...] | nil) :: list(Evision.Mat.t()) | {:error, String.t()}
  def encodeStructuredAppend(self, encoded_info, opts) when is_binary(encoded_info) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      encoded_info: Evision.Internal.Structurise.from_struct(encoded_info)
    ]
    :evision_nif.qrCodeEncoder_encodeStructuredAppend(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Generates QR code from input string in Structured Append mode. The encoded message is splitting over a number of QR codes.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeEncoder.t()`
  - **encoded_info**: `String`.

    Input string to encode.

  ##### Return
  - **qrcodes**: `[Evision.Mat]`.

    Vector of generated QR codes.

  Python prototype (for reference only):
  ```python3
  encodeStructuredAppend(encoded_info[, qrcodes]) -> qrcodes
  ```
  """
  @spec encodeStructuredAppend(Evision.QRCodeEncoder.t(), binary()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def encodeStructuredAppend(self, encoded_info) when is_binary(encoded_info)
  do
    positional = [
      encoded_info: Evision.Internal.Structurise.from_struct(encoded_info)
    ]
    :evision_nif.qrCodeEncoder_encodeStructuredAppend(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
