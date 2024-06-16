defmodule Evision.GraphicalCodeDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `GraphicalCodeDetector` struct.

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
  def to_struct({:ok, %{class: Evision.GraphicalCodeDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.GraphicalCodeDetector, ref: ref}) do
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
  Decodes graphical code in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.GraphicalCodeDetector.t()`
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
  @spec decode(Evision.GraphicalCodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {binary(), Evision.Mat.t()} | {:error, String.t()}
  def decode(self, img, points, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.graphicalCodeDetector_decode(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Decodes graphical code in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.GraphicalCodeDetector.t()`
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
  @spec decode(Evision.GraphicalCodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {binary(), Evision.Mat.t()} | {:error, String.t()}
  def decode(self, img, points) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.graphicalCodeDetector_decode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Decodes graphical codes in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.GraphicalCodeDetector.t()`
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
  @spec decodeMulti(Evision.GraphicalCodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {list(binary()), list(Evision.Mat.t())} | false | {:error, String.t()}
  def decodeMulti(self, img, points, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.graphicalCodeDetector_decodeMulti(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Decodes graphical codes in image once it's found by the detect() method.

  ##### Positional Arguments
  - **self**: `Evision.GraphicalCodeDetector.t()`
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
  @spec decodeMulti(Evision.GraphicalCodeDetector.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {list(binary()), list(Evision.Mat.t())} | false | {:error, String.t()}
  def decodeMulti(self, img, points) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.graphicalCodeDetector_decodeMulti(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Detects graphical code in image and returns the quadrangle containing the code.

  ##### Positional Arguments
  - **self**: `Evision.GraphicalCodeDetector.t()`
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
  @spec detect(Evision.GraphicalCodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def detect(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.graphicalCodeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Detects graphical code in image and returns the quadrangle containing the code.

  ##### Positional Arguments
  - **self**: `Evision.GraphicalCodeDetector.t()`
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
  @spec detect(Evision.GraphicalCodeDetector.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | false | {:error, String.t()}
  def detect(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.graphicalCodeDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Both detects and decodes graphical code

  ##### Positional Arguments
  - **self**: `Evision.GraphicalCodeDetector.t()`
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
  @spec detectAndDecode(Evision.GraphicalCodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {binary(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detectAndDecode(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.graphicalCodeDetector_detectAndDecode(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Both detects and decodes graphical code

  ##### Positional Arguments
  - **self**: `Evision.GraphicalCodeDetector.t()`
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
  @spec detectAndDecode(Evision.GraphicalCodeDetector.t(), Evision.Mat.maybe_mat_in()) :: {binary(), Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def detectAndDecode(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.graphicalCodeDetector_detectAndDecode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Both detects and decodes graphical codes

  ##### Positional Arguments
  - **self**: `Evision.GraphicalCodeDetector.t()`
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
  @spec detectAndDecodeMulti(Evision.GraphicalCodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {list(binary()), Evision.Mat.t(), list(Evision.Mat.t())} | false | {:error, String.t()}
  def detectAndDecodeMulti(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.graphicalCodeDetector_detectAndDecodeMulti(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Both detects and decodes graphical codes

  ##### Positional Arguments
  - **self**: `Evision.GraphicalCodeDetector.t()`
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
  @spec detectAndDecodeMulti(Evision.GraphicalCodeDetector.t(), Evision.Mat.maybe_mat_in()) :: {list(binary()), Evision.Mat.t(), list(Evision.Mat.t())} | false | {:error, String.t()}
  def detectAndDecodeMulti(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.graphicalCodeDetector_detectAndDecodeMulti(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Detects graphical codes in image and returns the vector of the quadrangles containing the codes.

  ##### Positional Arguments
  - **self**: `Evision.GraphicalCodeDetector.t()`
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
  @spec detectMulti(Evision.GraphicalCodeDetector.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def detectMulti(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.graphicalCodeDetector_detectMulti(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Detects graphical codes in image and returns the vector of the quadrangles containing the codes.

  ##### Positional Arguments
  - **self**: `Evision.GraphicalCodeDetector.t()`
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
  @spec detectMulti(Evision.GraphicalCodeDetector.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | false | {:error, String.t()}
  def detectMulti(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.graphicalCodeDetector_detectMulti(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
