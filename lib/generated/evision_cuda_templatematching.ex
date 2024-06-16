defmodule Evision.CUDA.TemplateMatching do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.TemplateMatching` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.TemplateMatching, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.TemplateMatching, ref: ref}) do
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
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.CUDA.TemplateMatching.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CUDA.TemplateMatching.t()) :: Evision.CUDA.TemplateMatching.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.cuda_TemplateMatching_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CUDA.TemplateMatching.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.TemplateMatching.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_TemplateMatching_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CUDA.TemplateMatching.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CUDA.TemplateMatching.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.cuda_TemplateMatching_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a proximity map for a raster template and an image where the template is searched for.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.TemplateMatching.t()`
  - **image**: `Evision.Mat`.

    Source image.

  - **templ**: `Evision.Mat`.

    Template image with the size and type the same as image .

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **result**: `Evision.Mat.t()`.

    Map containing comparison results ( CV_32FC1 ). If image is *W x H* and templ is *w
    x h*, then result must be *W-w+1 x H-h+1*.

  Python prototype (for reference only):
  ```python3
  match(image, templ[, result[, stream]]) -> result
  ```
  #### Variant 2:
  Computes a proximity map for a raster template and an image where the template is searched for.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.TemplateMatching.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`.

    Source image.

  - **templ**: `Evision.CUDA.GpuMat.t()`.

    Template image with the size and type the same as image .

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **result**: `Evision.CUDA.GpuMat.t()`.

    Map containing comparison results ( CV_32FC1 ). If image is *W x H* and templ is *w
    x h*, then result must be *W-w+1 x H-h+1*.

  Python prototype (for reference only):
  ```python3
  match(image, templ[, result[, stream]]) -> result
  ```

  """
  @spec match(Evision.CUDA.TemplateMatching.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def match(self, image, templ, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(templ, Evision.Mat) or is_struct(templ, Nx.Tensor) or is_number(templ) or is_tuple(templ)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      templ: Evision.Internal.Structurise.from_struct(templ)
    ]
    :evision_nif.cuda_cuda_TemplateMatching_match(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec match(Evision.CUDA.TemplateMatching.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def match(self, image, templ, opts) when is_struct(image, Evision.CUDA.GpuMat) and is_struct(templ, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      templ: Evision.Internal.Structurise.from_struct(templ)
    ]
    :evision_nif.cuda_cuda_TemplateMatching_match(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Computes a proximity map for a raster template and an image where the template is searched for.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.TemplateMatching.t()`
  - **image**: `Evision.Mat`.

    Source image.

  - **templ**: `Evision.Mat`.

    Template image with the size and type the same as image .

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **result**: `Evision.Mat.t()`.

    Map containing comparison results ( CV_32FC1 ). If image is *W x H* and templ is *w
    x h*, then result must be *W-w+1 x H-h+1*.

  Python prototype (for reference only):
  ```python3
  match(image, templ[, result[, stream]]) -> result
  ```
  #### Variant 2:
  Computes a proximity map for a raster template and an image where the template is searched for.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.TemplateMatching.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`.

    Source image.

  - **templ**: `Evision.CUDA.GpuMat.t()`.

    Template image with the size and type the same as image .

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    Stream for the asynchronous version.

  ##### Return
  - **result**: `Evision.CUDA.GpuMat.t()`.

    Map containing comparison results ( CV_32FC1 ). If image is *W x H* and templ is *w
    x h*, then result must be *W-w+1 x H-h+1*.

  Python prototype (for reference only):
  ```python3
  match(image, templ[, result[, stream]]) -> result
  ```

  """
  @spec match(Evision.CUDA.TemplateMatching.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def match(self, image, templ) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(templ, Evision.Mat) or is_struct(templ, Nx.Tensor) or is_number(templ) or is_tuple(templ))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      templ: Evision.Internal.Structurise.from_struct(templ)
    ]
    :evision_nif.cuda_cuda_TemplateMatching_match(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec match(Evision.CUDA.TemplateMatching.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def match(self, image, templ) when is_struct(image, Evision.CUDA.GpuMat) and is_struct(templ, Evision.CUDA.GpuMat)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      templ: Evision.Internal.Structurise.from_struct(templ)
    ]
    :evision_nif.cuda_cuda_TemplateMatching_match(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.TemplateMatching.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CUDA.TemplateMatching.t(), Evision.FileNode.t()) :: Evision.CUDA.TemplateMatching.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.cuda_TemplateMatching_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CUDA.TemplateMatching.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CUDA.TemplateMatching.t(), binary()) :: Evision.CUDA.TemplateMatching.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cuda_TemplateMatching_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.TemplateMatching.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CUDA.TemplateMatching.t(), Evision.FileStorage.t(), binary()) :: Evision.CUDA.TemplateMatching.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.cuda_TemplateMatching_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.TemplateMatching.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CUDA.TemplateMatching.t(), Evision.FileStorage.t()) :: Evision.CUDA.TemplateMatching.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.cuda_TemplateMatching_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
