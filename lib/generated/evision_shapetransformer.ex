defmodule Evision.ShapeTransformer do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `ShapeTransformer` struct.

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
  def to_struct({:ok, %{class: Evision.ShapeTransformer, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.ShapeTransformer, ref: ref}) do
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
  Apply a transformation, given a pre-estimated transformation parameters.

  ##### Positional Arguments
  - **self**: `Evision.ShapeTransformer.t()`
  - **input**: `Evision.Mat`.

    Contour (set of points) to apply the transformation.

  ##### Return
  - **retval**: `float`
  - **output**: `Evision.Mat.t()`.

    Output contour.

  Python prototype (for reference only):
  ```python3
  applyTransformation(input[, output]) -> retval, output
  ```
  """
  @spec applyTransformation(Evision.ShapeTransformer.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def applyTransformation(self, input, opts) when (is_struct(input, Evision.Mat) or is_struct(input, Nx.Tensor) or is_number(input) or is_tuple(input)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      input: Evision.Internal.Structurise.from_struct(input)
    ]
    :evision_nif.shapeTransformer_applyTransformation(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Apply a transformation, given a pre-estimated transformation parameters.

  ##### Positional Arguments
  - **self**: `Evision.ShapeTransformer.t()`
  - **input**: `Evision.Mat`.

    Contour (set of points) to apply the transformation.

  ##### Return
  - **retval**: `float`
  - **output**: `Evision.Mat.t()`.

    Output contour.

  Python prototype (for reference only):
  ```python3
  applyTransformation(input[, output]) -> retval, output
  ```
  """
  @spec applyTransformation(Evision.ShapeTransformer.t(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t()} | {:error, String.t()}
  def applyTransformation(self, input) when (is_struct(input, Evision.Mat) or is_struct(input, Nx.Tensor) or is_number(input) or is_tuple(input))
  do
    positional = [
      input: Evision.Internal.Structurise.from_struct(input)
    ]
    :evision_nif.shapeTransformer_applyTransformation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.ShapeTransformer.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.ShapeTransformer.t()) :: Evision.ShapeTransformer.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.shapeTransformer_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.ShapeTransformer.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.ShapeTransformer.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.shapeTransformer_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Estimate the transformation parameters of the current transformer algorithm, based on point matches.

  ##### Positional Arguments
  - **self**: `Evision.ShapeTransformer.t()`
  - **transformingShape**: `Evision.Mat`.

    Contour defining first shape.

  - **targetShape**: `Evision.Mat`.

    Contour defining second shape (Target).

  - **matches**: `[Evision.DMatch]`.

    Standard vector of Matches between points.

  Python prototype (for reference only):
  ```python3
  estimateTransformation(transformingShape, targetShape, matches) -> None
  ```
  """
  @spec estimateTransformation(Evision.ShapeTransformer.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), list(Evision.DMatch.t())) :: Evision.ShapeTransformer.t() | {:error, String.t()}
  def estimateTransformation(self, transformingShape, targetShape, matches) when (is_struct(transformingShape, Evision.Mat) or is_struct(transformingShape, Nx.Tensor) or is_number(transformingShape) or is_tuple(transformingShape)) and (is_struct(targetShape, Evision.Mat) or is_struct(targetShape, Nx.Tensor) or is_number(targetShape) or is_tuple(targetShape)) and is_list(matches)
  do
    positional = [
      transformingShape: Evision.Internal.Structurise.from_struct(transformingShape),
      targetShape: Evision.Internal.Structurise.from_struct(targetShape),
      matches: Evision.Internal.Structurise.from_struct(matches)
    ]
    :evision_nif.shapeTransformer_estimateTransformation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.ShapeTransformer.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.ShapeTransformer.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.shapeTransformer_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.ShapeTransformer.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.ShapeTransformer.t(), Evision.FileNode.t()) :: Evision.ShapeTransformer.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.shapeTransformer_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.ShapeTransformer.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.ShapeTransformer.t(), binary()) :: Evision.ShapeTransformer.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.shapeTransformer_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Apply a transformation, given a pre-estimated transformation parameters, to an Image.

  ##### Positional Arguments
  - **self**: `Evision.ShapeTransformer.t()`
  - **transformingImage**: `Evision.Mat`.

    Input image.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Image interpolation method.

  - **borderMode**: `integer()`.

    border style.

  - **borderValue**: `Evision.scalar()`.

    border value.

  ##### Return
  - **output**: `Evision.Mat.t()`.

    Output image.

  Python prototype (for reference only):
  ```python3
  warpImage(transformingImage[, output[, flags[, borderMode[, borderValue]]]]) -> output
  ```
  """
  @spec warpImage(Evision.ShapeTransformer.t(), Evision.Mat.maybe_mat_in(), [{:borderMode, term()} | {:borderValue, term()} | {:flags, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def warpImage(self, transformingImage, opts) when (is_struct(transformingImage, Evision.Mat) or is_struct(transformingImage, Nx.Tensor) or is_number(transformingImage) or is_tuple(transformingImage)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderMode, :borderValue, :flags])
    positional = [
      transformingImage: Evision.Internal.Structurise.from_struct(transformingImage)
    ]
    :evision_nif.shapeTransformer_warpImage(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Apply a transformation, given a pre-estimated transformation parameters, to an Image.

  ##### Positional Arguments
  - **self**: `Evision.ShapeTransformer.t()`
  - **transformingImage**: `Evision.Mat`.

    Input image.

  ##### Keyword Arguments
  - **flags**: `integer()`.

    Image interpolation method.

  - **borderMode**: `integer()`.

    border style.

  - **borderValue**: `Evision.scalar()`.

    border value.

  ##### Return
  - **output**: `Evision.Mat.t()`.

    Output image.

  Python prototype (for reference only):
  ```python3
  warpImage(transformingImage[, output[, flags[, borderMode[, borderValue]]]]) -> output
  ```
  """
  @spec warpImage(Evision.ShapeTransformer.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def warpImage(self, transformingImage) when (is_struct(transformingImage, Evision.Mat) or is_struct(transformingImage, Nx.Tensor) or is_number(transformingImage) or is_tuple(transformingImage))
  do
    positional = [
      transformingImage: Evision.Internal.Structurise.from_struct(transformingImage)
    ]
    :evision_nif.shapeTransformer_warpImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.ShapeTransformer.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.ShapeTransformer.t(), Evision.FileStorage.t(), binary()) :: Evision.ShapeTransformer.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.shapeTransformer_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.ShapeTransformer.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.ShapeTransformer.t(), Evision.FileStorage.t()) :: Evision.ShapeTransformer.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.shapeTransformer_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
