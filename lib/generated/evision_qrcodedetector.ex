defmodule Evision.QRCodeDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `QRCodeDetector` struct.

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
  def to_struct({:ok, %{class: Evision.QRCodeDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.QRCodeDetector, ref: ref}) do
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
  QRCodeDetector
  ##### Return
  - **self**: `Evision.QRCodeDetector.t()`

  Python prototype (for reference only):
  ```python3
  QRCodeDetector() -> <QRCodeDetector object>
  ```
  """
  @spec qrCodeDetector() :: Evision.QRCodeDetector.t() | {:error, String.t()}
  def qrCodeDetector() do
    positional = [
    ]
    :evision_nif.qrCodeDetector_QRCodeDetector(positional)
    |> to_struct()
  end

  @doc """
  Decodes graphical code in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing graphical code.

  - **points**: `Evision.Mat`.

    Quadrangle vertices found by detect() method (or some other algorithm).

  ##### Return
  - **retval**: `string`
  - **straight_code**: `Evision.Mat.t()`.

    The optional output image containing binarized code, will be empty if not found.

  Returns UTF8-encoded output string or empty string if the code cannot be decoded.

  Python prototype (for reference only):
  ```python3
  decode(img, points[, straight_code]) -> retval, straight_code
  ```
  """
  @spec decode(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {binary(), Evision.Mat.t()} | {:error, String.t()}
  def decode(self, img, points, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.qrCodeDetector_decode(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Decodes graphical code in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing graphical code.

  - **points**: `Evision.Mat`.

    Quadrangle vertices found by detect() method (or some other algorithm).

  ##### Return
  - **retval**: `string`
  - **straight_code**: `Evision.Mat.t()`.

    The optional output image containing binarized code, will be empty if not found.

  Returns UTF8-encoded output string or empty string if the code cannot be decoded.

  Python prototype (for reference only):
  ```python3
  decode(img, points[, straight_code]) -> retval, straight_code
  ```
  """
  @spec decode(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {binary(), Evision.Mat.t()} | {:error, String.t()}
  def decode(self, img, points) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.qrCodeDetector_decode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Decodes QR code on a curved surface in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing QR code.

  - **points**: `Evision.Mat`.

    Quadrangle vertices found by detect() method (or some other algorithm).

  ##### Return
  - **retval**: `String`
  - **straight_qrcode**: `Evision.Mat.t()`.

    The optional output image containing rectified and binarized QR code

  Returns UTF8-encoded output string or empty string if the code cannot be decoded.

  Python prototype (for reference only):
  ```python3
  decodeCurved(img, points[, straight_qrcode]) -> retval, straight_qrcode
  ```
  """
  @spec decodeCurved(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {binary(), Evision.Mat.t()} | {:error, String.t()}
  def decodeCurved(self, img, points, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.qrCodeDetector_decodeCurved(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Decodes QR code on a curved surface in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing QR code.

  - **points**: `Evision.Mat`.

    Quadrangle vertices found by detect() method (or some other algorithm).

  ##### Return
  - **retval**: `String`
  - **straight_qrcode**: `Evision.Mat.t()`.

    The optional output image containing rectified and binarized QR code

  Returns UTF8-encoded output string or empty string if the code cannot be decoded.

  Python prototype (for reference only):
  ```python3
  decodeCurved(img, points[, straight_qrcode]) -> retval, straight_qrcode
  ```
  """
  @spec decodeCurved(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {binary(), Evision.Mat.t()} | {:error, String.t()}
  def decodeCurved(self, img, points) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.qrCodeDetector_decodeCurved(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Decodes graphical codes in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing graphical codes.

  - **points**: `Evision.Mat`.

    vector of Quadrangle vertices found by detect() method (or some other algorithm).

  ##### Return
  - **retval**: `bool`
  - **decoded_info**: `[string]`.

    UTF8-encoded output vector of string or empty vector of string if the codes cannot be decoded.

  - **straight_code**: `[Evision.Mat]`.

    The optional output vector of images containing binarized codes

  Python prototype (for reference only):
  ```python3
  decodeMulti(img, points[, straight_code]) -> retval, decoded_info, straight_code
  ```
  """
  @spec decodeMulti(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {list(binary()), list(Evision.Mat.t())} | false | {:error, String.t()}
  def decodeMulti(self, img, points, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.qrCodeDetector_decodeMulti(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Decodes graphical codes in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing graphical codes.

  - **points**: `Evision.Mat`.

    vector of Quadrangle vertices found by detect() method (or some other algorithm).

  ##### Return
  - **retval**: `bool`
  - **decoded_info**: `[string]`.

    UTF8-encoded output vector of string or empty vector of string if the codes cannot be decoded.

  - **straight_code**: `[Evision.Mat]`.

    The optional output vector of images containing binarized codes

  Python prototype (for reference only):
  ```python3
  decodeMulti(img, points[, straight_code]) -> retval, decoded_info, straight_code
  ```
  """
  @spec decodeMulti(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(binary()), list(Evision.Mat.t())} | false | {:error, String.t()}
  def decodeMulti(self, img, points) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.qrCodeDetector_decodeMulti(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Detects graphical code in image and returns the quadrangle containing the code.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing (or not) graphical code.

  ##### Return
  - **retval**: `bool`
  - **points**: `Evision.Mat.t()`.

    Output vector of vertices of the minimum-area quadrangle containing the code.

  Python prototype (for reference only):
  ```python3
  detect(img[, points]) -> retval, points
  ```
  """
  @spec detect(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def detect(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.qrCodeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Detects graphical code in image and returns the quadrangle containing the code.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing (or not) graphical code.

  ##### Return
  - **retval**: `bool`
  - **points**: `Evision.Mat.t()`.

    Output vector of vertices of the minimum-area quadrangle containing the code.

  Python prototype (for reference only):
  ```python3
  detect(img[, points]) -> retval, points
  ```
  """
  @spec detect(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | false | {:error, String.t()}
  def detect(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.qrCodeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Both detects and decodes graphical code

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing graphical code.

  ##### Return
  - **retval**: `string`
  - **points**: `Evision.Mat.t()`.

    optional output array of vertices of the found graphical code quadrangle, will be empty if not found.

  - **straight_code**: `Evision.Mat.t()`.

    The optional output image containing binarized code

  Python prototype (for reference only):
  ```python3
  detectAndDecode(img[, points[, straight_code]]) -> retval, points, straight_code
  ```
  """
  @spec detectAndDecode(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {binary(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detectAndDecode(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.qrCodeDetector_detectAndDecode(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Both detects and decodes graphical code

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing graphical code.

  ##### Return
  - **retval**: `string`
  - **points**: `Evision.Mat.t()`.

    optional output array of vertices of the found graphical code quadrangle, will be empty if not found.

  - **straight_code**: `Evision.Mat.t()`.

    The optional output image containing binarized code

  Python prototype (for reference only):
  ```python3
  detectAndDecode(img[, points[, straight_code]]) -> retval, points, straight_code
  ```
  """
  @spec detectAndDecode(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in()) :: {binary(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detectAndDecode(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.qrCodeDetector_detectAndDecode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Both detects and decodes QR code on a curved surface

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing QR code.

  ##### Return
  - **retval**: `string`
  - **points**: `Evision.Mat.t()`.

    optional output array of vertices of the found QR code quadrangle. Will be empty if not found.

  - **straight_qrcode**: `Evision.Mat.t()`.

    The optional output image containing rectified and binarized QR code

  Python prototype (for reference only):
  ```python3
  detectAndDecodeCurved(img[, points[, straight_qrcode]]) -> retval, points, straight_qrcode
  ```
  """
  @spec detectAndDecodeCurved(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {binary(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detectAndDecodeCurved(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.qrCodeDetector_detectAndDecodeCurved(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Both detects and decodes QR code on a curved surface

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing QR code.

  ##### Return
  - **retval**: `string`
  - **points**: `Evision.Mat.t()`.

    optional output array of vertices of the found QR code quadrangle. Will be empty if not found.

  - **straight_qrcode**: `Evision.Mat.t()`.

    The optional output image containing rectified and binarized QR code

  Python prototype (for reference only):
  ```python3
  detectAndDecodeCurved(img[, points[, straight_qrcode]]) -> retval, points, straight_qrcode
  ```
  """
  @spec detectAndDecodeCurved(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in()) :: {binary(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detectAndDecodeCurved(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.qrCodeDetector_detectAndDecodeCurved(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Both detects and decodes graphical codes

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing graphical codes.

  ##### Return
  - **retval**: `bool`
  - **decoded_info**: `[string]`.

    UTF8-encoded output vector of string or empty vector of string if the codes cannot be decoded.

  - **points**: `Evision.Mat.t()`.

    optional output vector of vertices of the found graphical code quadrangles. Will be empty if not found.

  - **straight_code**: `[Evision.Mat]`.

    The optional vector of images containing binarized codes

  - If there are QR codes encoded with a Structured Append mode on the image and all of them detected and decoded correctly,
    method writes a full message to position corresponds to 0-th code in a sequence. The rest of QR codes from the same sequence
    have empty string.

  Python prototype (for reference only):
  ```python3
  detectAndDecodeMulti(img[, points[, straight_code]]) -> retval, decoded_info, points, straight_code
  ```
  """
  @spec detectAndDecodeMulti(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {list(binary()), Evision.Mat.t(), list(Evision.Mat.t())} | false | {:error, String.t()}
  def detectAndDecodeMulti(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.qrCodeDetector_detectAndDecodeMulti(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Both detects and decodes graphical codes

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing graphical codes.

  ##### Return
  - **retval**: `bool`
  - **decoded_info**: `[string]`.

    UTF8-encoded output vector of string or empty vector of string if the codes cannot be decoded.

  - **points**: `Evision.Mat.t()`.

    optional output vector of vertices of the found graphical code quadrangles. Will be empty if not found.

  - **straight_code**: `[Evision.Mat]`.

    The optional vector of images containing binarized codes

  - If there are QR codes encoded with a Structured Append mode on the image and all of them detected and decoded correctly,
    method writes a full message to position corresponds to 0-th code in a sequence. The rest of QR codes from the same sequence
    have empty string.

  Python prototype (for reference only):
  ```python3
  detectAndDecodeMulti(img[, points[, straight_code]]) -> retval, decoded_info, points, straight_code
  ```
  """
  @spec detectAndDecodeMulti(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in()) :: {list(binary()), Evision.Mat.t(), list(Evision.Mat.t())} | false | {:error, String.t()}
  def detectAndDecodeMulti(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.qrCodeDetector_detectAndDecodeMulti(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Detects graphical codes in image and returns the vector of the quadrangles containing the codes.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing (or not) graphical codes.

  ##### Return
  - **retval**: `bool`
  - **points**: `Evision.Mat.t()`.

    Output vector of vector of vertices of the minimum-area quadrangle containing the codes.

  Python prototype (for reference only):
  ```python3
  detectMulti(img[, points]) -> retval, points
  ```
  """
  @spec detectMulti(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def detectMulti(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.qrCodeDetector_detectMulti(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Detects graphical codes in image and returns the vector of the quadrangles containing the codes.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing (or not) graphical codes.

  ##### Return
  - **retval**: `bool`
  - **points**: `Evision.Mat.t()`.

    Output vector of vector of vertices of the minimum-area quadrangle containing the codes.

  Python prototype (for reference only):
  ```python3
  detectMulti(img[, points]) -> retval, points
  ```
  """
  @spec detectMulti(Evision.QRCodeDetector.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | false | {:error, String.t()}
  def detectMulti(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.qrCodeDetector_detectMulti(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  sets the epsilon used during the horizontal scan of QR code stop marker detection.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **epsX**: `double`.

    Epsilon neighborhood, which allows you to determine the horizontal pattern
    of the scheme 1:1:3:1:1 according to QR code standard.

  ##### Return
  - **retval**: `Evision.QRCodeDetector.t()`

  Python prototype (for reference only):
  ```python3
  setEpsX(epsX) -> retval
  ```
  """
  @spec setEpsX(Evision.QRCodeDetector.t(), number()) :: Evision.QRCodeDetector.t() | {:error, String.t()}
  def setEpsX(self, epsX) when is_number(epsX)
  do
    positional = [
      epsX: Evision.Internal.Structurise.from_struct(epsX)
    ]
    :evision_nif.qrCodeDetector_setEpsX(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  sets the epsilon used during the vertical scan of QR code stop marker detection.

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **epsY**: `double`.

    Epsilon neighborhood, which allows you to determine the vertical pattern
    of the scheme 1:1:3:1:1 according to QR code standard.

  ##### Return
  - **retval**: `Evision.QRCodeDetector.t()`

  Python prototype (for reference only):
  ```python3
  setEpsY(epsY) -> retval
  ```
  """
  @spec setEpsY(Evision.QRCodeDetector.t(), number()) :: Evision.QRCodeDetector.t() | {:error, String.t()}
  def setEpsY(self, epsY) when is_number(epsY)
  do
    positional = [
      epsY: Evision.Internal.Structurise.from_struct(epsY)
    ]
    :evision_nif.qrCodeDetector_setEpsY(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  use markers to improve the position of the corners of the QR code

  ##### Positional Arguments
  - **self**: `Evision.QRCodeDetector.t()`
  - **useAlignmentMarkers**: `bool`

  ##### Return
  - **retval**: `Evision.QRCodeDetector.t()`

   alignmentMarkers using by default

  Python prototype (for reference only):
  ```python3
  setUseAlignmentMarkers(useAlignmentMarkers) -> retval
  ```
  """
  @spec setUseAlignmentMarkers(Evision.QRCodeDetector.t(), boolean()) :: Evision.QRCodeDetector.t() | {:error, String.t()}
  def setUseAlignmentMarkers(self, useAlignmentMarkers) when is_boolean(useAlignmentMarkers)
  do
    positional = [
      useAlignmentMarkers: Evision.Internal.Structurise.from_struct(useAlignmentMarkers)
    ]
    :evision_nif.qrCodeDetector_setUseAlignmentMarkers(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
