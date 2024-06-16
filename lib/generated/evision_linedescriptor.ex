defmodule Evision.LineDescriptor do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineDescriptor` struct.

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
  def to_struct({:ok, %{class: Evision.LineDescriptor, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineDescriptor, ref: ref}) do
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
  Draws keylines.

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    input image

  - **keylines**: `[Evision.LineDescriptor.KeyLine]`.

    keylines to be drawn

  ##### Keyword Arguments
  - **color**: `Evision.scalar()`.

    color of lines to be drawn (if set to defaul value, color is chosen randomly)

  - **flags**: `integer()`.

    drawing flags

  ##### Return
  - **outImage**: `Evision.Mat.t()`.

    output image to draw on

  Python prototype (for reference only):
  ```python3
  drawKeylines(image, keylines[, outImage[, color[, flags]]]) -> outImage
  ```
  """
  @spec drawKeylines(Evision.Mat.maybe_mat_in(), list(Evision.LineDescriptor.KeyLine.t()), [{:color, term()} | {:flags, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawKeylines(image, keylines, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keylines) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:color, :flags])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keylines: Evision.Internal.Structurise.from_struct(keylines)
    ]
    :evision_nif.line_descriptor_drawKeylines(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Draws keylines.

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    input image

  - **keylines**: `[Evision.LineDescriptor.KeyLine]`.

    keylines to be drawn

  ##### Keyword Arguments
  - **color**: `Evision.scalar()`.

    color of lines to be drawn (if set to defaul value, color is chosen randomly)

  - **flags**: `integer()`.

    drawing flags

  ##### Return
  - **outImage**: `Evision.Mat.t()`.

    output image to draw on

  Python prototype (for reference only):
  ```python3
  drawKeylines(image, keylines[, outImage[, color[, flags]]]) -> outImage
  ```
  """
  @spec drawKeylines(Evision.Mat.maybe_mat_in(), list(Evision.LineDescriptor.KeyLine.t())) :: Evision.Mat.t() | {:error, String.t()}
  def drawKeylines(image, keylines) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_list(keylines)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      keylines: Evision.Internal.Structurise.from_struct(keylines)
    ]
    :evision_nif.line_descriptor_drawKeylines(positional)
    |> to_struct()
  end

  @doc """
  Draws the found matches of keylines from two images.

  ##### Positional Arguments
  - **img1**: `Evision.Mat`.

    first image

  - **keylines1**: `[Evision.LineDescriptor.KeyLine]`.

    keylines extracted from first image

  - **img2**: `Evision.Mat`.

    second image

  - **keylines2**: `[Evision.LineDescriptor.KeyLine]`.

    keylines extracted from second image

  - **matches1to2**: `[Evision.DMatch]`.

    vector of matches

  ##### Keyword Arguments
  - **matchColor**: `Evision.scalar()`.

    drawing color for matches (chosen randomly in case of default value)

  - **singleLineColor**: `Evision.scalar()`.

    drawing color for keylines (chosen randomly in case of default value)

  - **matchesMask**: `[char]`.

    mask to indicate which matches must be drawn

  - **flags**: `integer()`.

    drawing flags, see DrawLinesMatchesFlags

  ##### Return
  - **outImg**: `Evision.Mat.t()`.

    output matrix to draw on

  **Note**: If both *matchColor* and *singleLineColor* are set to their default values, function draws
  matched lines and line connecting them with same color

  Python prototype (for reference only):
  ```python3
  drawLineMatches(img1, keylines1, img2, keylines2, matches1to2[, outImg[, matchColor[, singleLineColor[, matchesMask[, flags]]]]]) -> outImg
  ```
  """
  @spec drawLineMatches(Evision.Mat.maybe_mat_in(), list(Evision.LineDescriptor.KeyLine.t()), Evision.Mat.maybe_mat_in(), list(Evision.LineDescriptor.KeyLine.t()), list(Evision.DMatch.t()), [{:flags, term()} | {:matchColor, term()} | {:matchesMask, term()} | {:singleLineColor, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawLineMatches(img1, keylines1, img2, keylines2, matches1to2, opts) when (is_struct(img1, Evision.Mat) or is_struct(img1, Nx.Tensor) or is_number(img1) or is_tuple(img1)) and is_list(keylines1) and (is_struct(img2, Evision.Mat) or is_struct(img2, Nx.Tensor) or is_number(img2) or is_tuple(img2)) and is_list(keylines2) and is_list(matches1to2) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:flags, :matchColor, :matchesMask, :singleLineColor])
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1),
      keylines1: Evision.Internal.Structurise.from_struct(keylines1),
      img2: Evision.Internal.Structurise.from_struct(img2),
      keylines2: Evision.Internal.Structurise.from_struct(keylines2),
      matches1to2: Evision.Internal.Structurise.from_struct(matches1to2)
    ]
    :evision_nif.line_descriptor_drawLineMatches(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Draws the found matches of keylines from two images.

  ##### Positional Arguments
  - **img1**: `Evision.Mat`.

    first image

  - **keylines1**: `[Evision.LineDescriptor.KeyLine]`.

    keylines extracted from first image

  - **img2**: `Evision.Mat`.

    second image

  - **keylines2**: `[Evision.LineDescriptor.KeyLine]`.

    keylines extracted from second image

  - **matches1to2**: `[Evision.DMatch]`.

    vector of matches

  ##### Keyword Arguments
  - **matchColor**: `Evision.scalar()`.

    drawing color for matches (chosen randomly in case of default value)

  - **singleLineColor**: `Evision.scalar()`.

    drawing color for keylines (chosen randomly in case of default value)

  - **matchesMask**: `[char]`.

    mask to indicate which matches must be drawn

  - **flags**: `integer()`.

    drawing flags, see DrawLinesMatchesFlags

  ##### Return
  - **outImg**: `Evision.Mat.t()`.

    output matrix to draw on

  **Note**: If both *matchColor* and *singleLineColor* are set to their default values, function draws
  matched lines and line connecting them with same color

  Python prototype (for reference only):
  ```python3
  drawLineMatches(img1, keylines1, img2, keylines2, matches1to2[, outImg[, matchColor[, singleLineColor[, matchesMask[, flags]]]]]) -> outImg
  ```
  """
  @spec drawLineMatches(Evision.Mat.maybe_mat_in(), list(Evision.LineDescriptor.KeyLine.t()), Evision.Mat.maybe_mat_in(), list(Evision.LineDescriptor.KeyLine.t()), list(Evision.DMatch.t())) :: Evision.Mat.t() | {:error, String.t()}
  def drawLineMatches(img1, keylines1, img2, keylines2, matches1to2) when (is_struct(img1, Evision.Mat) or is_struct(img1, Nx.Tensor) or is_number(img1) or is_tuple(img1)) and is_list(keylines1) and (is_struct(img2, Evision.Mat) or is_struct(img2, Nx.Tensor) or is_number(img2) or is_tuple(img2)) and is_list(keylines2) and is_list(matches1to2)
  do
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1),
      keylines1: Evision.Internal.Structurise.from_struct(keylines1),
      img2: Evision.Internal.Structurise.from_struct(img2),
      keylines2: Evision.Internal.Structurise.from_struct(keylines2),
      matches1to2: Evision.Internal.Structurise.from_struct(matches1to2)
    ]
    :evision_nif.line_descriptor_drawLineMatches(positional)
    |> to_struct()
  end
end
