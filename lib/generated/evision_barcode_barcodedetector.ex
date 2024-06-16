defmodule Evision.Barcode.BarcodeDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Barcode.BarcodeDetector` struct.

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
  def to_struct({:ok, %{class: Evision.Barcode.BarcodeDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Barcode.BarcodeDetector, ref: ref}) do
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
  Initialize the BarcodeDetector.

  ##### Positional Arguments
  - **prototxt_path**: `string`.

    prototxt file path for the super resolution model

  - **model_path**: `string`.

    model file path for the super resolution model

  ##### Return
  - **self**: `BarcodeDetector`

   Parameters allow to load _optional_ Super Resolution DNN model for better quality.

  Python prototype (for reference only):
  ```python3
  BarcodeDetector(prototxt_path, model_path) -> <barcode_BarcodeDetector object>
  ```
  """
  @spec barcodeDetector(binary(), binary()) :: Evision.Barcode.BarcodeDetector.t() | {:error, String.t()}
  def barcodeDetector(prototxt_path, model_path) when is_binary(prototxt_path) and is_binary(model_path)
  do
    positional = [
      prototxt_path: Evision.Internal.Structurise.from_struct(prototxt_path),
      model_path: Evision.Internal.Structurise.from_struct(model_path)
    ]
    :evision_nif.barcode_barcode_BarcodeDetector_BarcodeDetector(positional)
    |> to_struct()
  end

  @doc """
  Initialize the BarcodeDetector.
  ##### Return
  - **self**: `BarcodeDetector`

  Python prototype (for reference only):
  ```python3
  BarcodeDetector() -> <barcode_BarcodeDetector object>
  ```
  """
  @spec barcodeDetector() :: Evision.Barcode.BarcodeDetector.t() | {:error, String.t()}
  def barcodeDetector() do
    positional = [
    ]
    :evision_nif.barcode_barcode_BarcodeDetector_BarcodeDetector(positional)
    |> to_struct()
  end

  @doc """
  Decodes graphical code in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
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
  @spec decode(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {binary(), Evision.Mat.t()} | {:error, String.t()}
  def decode(self, img, points, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.barcode_BarcodeDetector_decode(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Decodes graphical code in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
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
  @spec decode(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {binary(), Evision.Mat.t()} | {:error, String.t()}
  def decode(self, img, points) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.barcode_BarcodeDetector_decode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Decodes graphical codes in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
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
  @spec decodeMulti(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {list(binary()), list(Evision.Mat.t())} | false | {:error, String.t()}
  def decodeMulti(self, img, points, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.barcode_BarcodeDetector_decodeMulti(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Decodes graphical codes in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
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
  @spec decodeMulti(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(binary()), list(Evision.Mat.t())} | false | {:error, String.t()}
  def decodeMulti(self, img, points) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.barcode_BarcodeDetector_decodeMulti(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Decodes barcode in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing bar code.

  - **points**: `Evision.Mat`.

    vector of rotated rectangle vertices found by detect() method (or some other algorithm).
    For N detected barcodes, the dimensions of this array should be [N][4].
    Order of four points in vector<Point2f> is bottomLeft, topLeft, topRight, bottomRight.

  ##### Return
  - **retval**: `bool`
  - **decoded_info**: `[string]`.

    UTF8-encoded output vector of string or empty vector of string if the codes cannot be decoded.

  - **decoded_type**: `[string]`.

    vector strings, specifies the type of these barcodes

  @return true if at least one valid barcode have been found

  Python prototype (for reference only):
  ```python3
  decodeWithType(img, points) -> retval, decoded_info, decoded_type
  ```
  """
  @spec decodeWithType(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(binary()), list(binary())} | false | {:error, String.t()}
  def decodeWithType(self, img, points) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.barcode_barcode_BarcodeDetector_decodeWithType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Detects graphical code in image and returns the quadrangle containing the code.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
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
  @spec detect(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def detect(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.barcode_BarcodeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Detects graphical code in image and returns the quadrangle containing the code.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
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
  @spec detect(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | false | {:error, String.t()}
  def detect(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.barcode_BarcodeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Both detects and decodes graphical code

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
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
  @spec detectAndDecode(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {binary(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detectAndDecode(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.barcode_BarcodeDetector_detectAndDecode(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Both detects and decodes graphical code

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
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
  @spec detectAndDecode(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in()) :: {binary(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detectAndDecode(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.barcode_BarcodeDetector_detectAndDecode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Both detects and decodes graphical codes

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
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
  @spec detectAndDecodeMulti(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {list(binary()), Evision.Mat.t(), list(Evision.Mat.t())} | false | {:error, String.t()}
  def detectAndDecodeMulti(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.barcode_BarcodeDetector_detectAndDecodeMulti(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Both detects and decodes graphical codes

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
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
  @spec detectAndDecodeMulti(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in()) :: {list(binary()), Evision.Mat.t(), list(Evision.Mat.t())} | false | {:error, String.t()}
  def detectAndDecodeMulti(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.barcode_BarcodeDetector_detectAndDecodeMulti(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Both detects and decodes barcode

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing barcode.

  ##### Return
  - **retval**: `bool`
  - **decoded_info**: `[string]`.

    UTF8-encoded output vector of string(s) or empty vector of string if the codes cannot be decoded.

  - **decoded_type**: `[string]`.

    vector of strings, specifies the type of these barcodes

  - **points**: `Evision.Mat.t()`.

    optional output vector of vertices of the found  barcode rectangle. Will be empty if not found.

  @return true if at least one valid barcode have been found

  Python prototype (for reference only):
  ```python3
  detectAndDecodeWithType(img[, points]) -> retval, decoded_info, decoded_type, points
  ```
  """
  @spec detectAndDecodeWithType(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {list(binary()), list(binary()), Evision.Mat.t()} | false | {:error, String.t()}
  def detectAndDecodeWithType(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.barcode_barcode_BarcodeDetector_detectAndDecodeWithType(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Both detects and decodes barcode

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
  - **img**: `Evision.Mat`.

    grayscale or color (BGR) image containing barcode.

  ##### Return
  - **retval**: `bool`
  - **decoded_info**: `[string]`.

    UTF8-encoded output vector of string(s) or empty vector of string if the codes cannot be decoded.

  - **decoded_type**: `[string]`.

    vector of strings, specifies the type of these barcodes

  - **points**: `Evision.Mat.t()`.

    optional output vector of vertices of the found  barcode rectangle. Will be empty if not found.

  @return true if at least one valid barcode have been found

  Python prototype (for reference only):
  ```python3
  detectAndDecodeWithType(img[, points]) -> retval, decoded_info, decoded_type, points
  ```
  """
  @spec detectAndDecodeWithType(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in()) :: {list(binary()), list(binary()), Evision.Mat.t()} | false | {:error, String.t()}
  def detectAndDecodeWithType(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.barcode_barcode_BarcodeDetector_detectAndDecodeWithType(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Detects graphical codes in image and returns the vector of the quadrangles containing the codes.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
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
  @spec detectMulti(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def detectMulti(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.barcode_BarcodeDetector_detectMulti(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Detects graphical codes in image and returns the vector of the quadrangles containing the codes.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
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
  @spec detectMulti(Evision.Barcode.BarcodeDetector.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | false | {:error, String.t()}
  def detectMulti(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.barcode_BarcodeDetector_detectMulti(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns detector box filter sizes.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`

  ##### Return
  - **sizes**: `[float]`.

    output parameter for returning the sizes.

  Python prototype (for reference only):
  ```python3
  getDetectorScales() -> sizes
  ```
  """
  @spec getDetectorScales(Evision.Barcode.BarcodeDetector.t()) :: list(number()) | {:error, String.t()}
  def getDetectorScales(self) do
    positional = [
    ]
    :evision_nif.barcode_barcode_BarcodeDetector_getDetectorScales(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Get detector downsampling threshold.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`

  ##### Return
  - **retval**: `double`

  @return detector downsampling threshold

  Python prototype (for reference only):
  ```python3
  getDownsamplingThreshold() -> retval
  ```
  """
  @spec getDownsamplingThreshold(Evision.Barcode.BarcodeDetector.t()) :: number() | {:error, String.t()}
  def getDownsamplingThreshold(self) do
    positional = [
    ]
    :evision_nif.barcode_barcode_BarcodeDetector_getDownsamplingThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Get detector gradient magnitude threshold.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`

  ##### Return
  - **retval**: `double`

  @return detector gradient magnitude threshold.

  Python prototype (for reference only):
  ```python3
  getGradientThreshold() -> retval
  ```
  """
  @spec getGradientThreshold(Evision.Barcode.BarcodeDetector.t()) :: number() | {:error, String.t()}
  def getGradientThreshold(self) do
    positional = [
    ]
    :evision_nif.barcode_barcode_BarcodeDetector_getGradientThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set detector box filter sizes.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
  - **sizes**: `[float]`.

    box filter sizes, relative to minimum dimension of the image (default [0.01, 0.03, 0.06, 0.08])

  ##### Return
  - **retval**: `BarcodeDetector`

   Adjusts the value and the number of box filters used in the detect step.
   The filter sizes directly correlate with the expected line widths for a barcode. Corresponds to expected barcode distance.
   If the downsampling limit is increased, filter sizes need to be adjusted in an inversely proportional way.

  Python prototype (for reference only):
  ```python3
  setDetectorScales(sizes) -> retval
  ```
  """
  @spec setDetectorScales(Evision.Barcode.BarcodeDetector.t(), list(number())) :: Evision.Barcode.BarcodeDetector.t() | {:error, String.t()}
  def setDetectorScales(self, sizes) when is_list(sizes)
  do
    positional = [
      sizes: Evision.Internal.Structurise.from_struct(sizes)
    ]
    :evision_nif.barcode_barcode_BarcodeDetector_setDetectorScales(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set detector downsampling threshold.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
  - **thresh**: `double`.

    downsampling limit to apply (default 512)

  ##### Return
  - **retval**: `BarcodeDetector`

   By default, the detect method resizes the input image to this limit if the smallest image size is is greater than the threshold.
   Increasing this value can improve detection accuracy and the number of results at the expense of performance.
   Correlates with detector scales. Setting this to a large value will disable downsampling.
  @see `setDetectorScales/2`

  Python prototype (for reference only):
  ```python3
  setDownsamplingThreshold(thresh) -> retval
  ```
  """
  @spec setDownsamplingThreshold(Evision.Barcode.BarcodeDetector.t(), number()) :: Evision.Barcode.BarcodeDetector.t() | {:error, String.t()}
  def setDownsamplingThreshold(self, thresh) when is_number(thresh)
  do
    positional = [
      thresh: Evision.Internal.Structurise.from_struct(thresh)
    ]
    :evision_nif.barcode_barcode_BarcodeDetector_setDownsamplingThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set detector gradient magnitude threshold.

  ##### Positional Arguments
  - **self**: `Evision.Barcode.BarcodeDetector.t()`
  - **thresh**: `double`.

    gradient magnitude threshold (default 64).

  ##### Return
  - **retval**: `BarcodeDetector`

   Sets the coherence threshold for detected bounding boxes.
   Increasing this value will generate a closer fitted bounding box width and can reduce false-positives.
   Values between 16 and 1024 generally work, while too high of a value will remove valid detections.

  Python prototype (for reference only):
  ```python3
  setGradientThreshold(thresh) -> retval
  ```
  """
  @spec setGradientThreshold(Evision.Barcode.BarcodeDetector.t(), number()) :: Evision.Barcode.BarcodeDetector.t() | {:error, String.t()}
  def setGradientThreshold(self, thresh) when is_number(thresh)
  do
    positional = [
      thresh: Evision.Internal.Structurise.from_struct(thresh)
    ]
    :evision_nif.barcode_barcode_BarcodeDetector_setGradientThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
