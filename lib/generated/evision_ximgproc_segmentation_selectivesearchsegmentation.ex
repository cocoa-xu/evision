defmodule Evision.XImgProc.Segmentation.SelectiveSearchSegmentation do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc.Segmentation.SelectiveSearchSegmentation` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation, ref: ref}) do
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
  Add a new graph segmentation in the list of graph segementations to process.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`
  - **g**: `Evision.XImgProc.GraphSegmentation.t()`.

    The graph segmentation

  Python prototype (for reference only):
  ```python3
  addGraphSegmentation(g) -> None
  ```
  """
  @spec addGraphSegmentation(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t(), Evision.XImgProc.GraphSegmentation.t()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def addGraphSegmentation(self, g) when is_struct(g, Evision.XImgProc.GraphSegmentation)
  do
    positional = [
      g: Evision.Internal.Structurise.from_struct(g)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_addGraphSegmentation(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Add a new image in the list of images to process.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`
  - **img**: `Evision.Mat`.

    The image

  Python prototype (for reference only):
  ```python3
  addImage(img) -> None
  ```
  """
  @spec addImage(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t(), Evision.Mat.maybe_mat_in()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def addImage(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_addImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Add a new strategy in the list of strategy to process.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`
  - **s**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`.

    The strategy

  Python prototype (for reference only):
  ```python3
  addStrategy(s) -> None
  ```
  """
  @spec addStrategy(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t(), Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def addStrategy(self, s) when is_struct(s, Evision.XImgProc.SelectiveSearchSegmentationStrategy)
  do
    positional = [
      s: Evision.Internal.Structurise.from_struct(s)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_addStrategy(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentation_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clear the list of graph segmentations to process;

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`

  Python prototype (for reference only):
  ```python3
  clearGraphSegmentations() -> None
  ```
  """
  @spec clearGraphSegmentations(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def clearGraphSegmentations(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_clearGraphSegmentations(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clear the list of images to process

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`

  Python prototype (for reference only):
  ```python3
  clearImages() -> None
  ```
  """
  @spec clearImages(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def clearImages(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_clearImages(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clear the list of strategy to process;

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`

  Python prototype (for reference only):
  ```python3
  clearStrategies() -> None
  ```
  """
  @spec clearStrategies(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def clearStrategies(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_clearStrategies(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentation_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentation_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Based on all images, graph segmentations and stragies, computes all possible rects and return them

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`

  ##### Return
  - **rects**: `[Rect]`.

    The list of rects. The first ones are more relevents than the lasts ones.

  Python prototype (for reference only):
  ```python3
  process() -> rects
  ```
  """
  @spec process(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()) :: list({number(), number(), number(), number()}) | {:error, String.t()}
  def process(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t(), Evision.FileNode.t()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentation_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t(), binary()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentation_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set a image used by switch* functions to initialize the class

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`
  - **img**: `Evision.Mat`.

    The image

  Python prototype (for reference only):
  ```python3
  setBaseImage(img) -> None
  ```
  """
  @spec setBaseImage(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t(), Evision.Mat.maybe_mat_in()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def setBaseImage(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_setBaseImage(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Initialize the class with the 'Selective search fast' parameters describled in @cite uijlings2013selective.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`

  ##### Keyword Arguments
  - **base_k**: `integer()`.

    The k parameter for the first graph segmentation

  - **inc_k**: `integer()`.

    The increment of the k parameter for all graph segmentations

  - **sigma**: `float`.

    The sigma parameter for the graph segmentation

  Python prototype (for reference only):
  ```python3
  switchToSelectiveSearchFast([, base_k[, inc_k[, sigma]]]) -> None
  ```
  """
  @spec switchToSelectiveSearchFast(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t(), [{:base_k, term()} | {:inc_k, term()} | {:sigma, term()}] | nil) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def switchToSelectiveSearchFast(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:base_k, :inc_k, :sigma])
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_switchToSelectiveSearchFast(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Initialize the class with the 'Selective search fast' parameters describled in @cite uijlings2013selective.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`

  ##### Keyword Arguments
  - **base_k**: `integer()`.

    The k parameter for the first graph segmentation

  - **inc_k**: `integer()`.

    The increment of the k parameter for all graph segmentations

  - **sigma**: `float`.

    The sigma parameter for the graph segmentation

  Python prototype (for reference only):
  ```python3
  switchToSelectiveSearchFast([, base_k[, inc_k[, sigma]]]) -> None
  ```
  """
  @spec switchToSelectiveSearchFast(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def switchToSelectiveSearchFast(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_switchToSelectiveSearchFast(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Initialize the class with the 'Selective search fast' parameters describled in @cite uijlings2013selective.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`

  ##### Keyword Arguments
  - **base_k**: `integer()`.

    The k parameter for the first graph segmentation

  - **inc_k**: `integer()`.

    The increment of the k parameter for all graph segmentations

  - **sigma**: `float`.

    The sigma parameter for the graph segmentation

  Python prototype (for reference only):
  ```python3
  switchToSelectiveSearchQuality([, base_k[, inc_k[, sigma]]]) -> None
  ```
  """
  @spec switchToSelectiveSearchQuality(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t(), [{:base_k, term()} | {:inc_k, term()} | {:sigma, term()}] | nil) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def switchToSelectiveSearchQuality(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:base_k, :inc_k, :sigma])
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_switchToSelectiveSearchQuality(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Initialize the class with the 'Selective search fast' parameters describled in @cite uijlings2013selective.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`

  ##### Keyword Arguments
  - **base_k**: `integer()`.

    The k parameter for the first graph segmentation

  - **inc_k**: `integer()`.

    The increment of the k parameter for all graph segmentations

  - **sigma**: `float`.

    The sigma parameter for the graph segmentation

  Python prototype (for reference only):
  ```python3
  switchToSelectiveSearchQuality([, base_k[, inc_k[, sigma]]]) -> None
  ```
  """
  @spec switchToSelectiveSearchQuality(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def switchToSelectiveSearchQuality(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_switchToSelectiveSearchQuality(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Initialize the class with the 'Single stragegy' parameters describled in @cite uijlings2013selective.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`

  ##### Keyword Arguments
  - **k**: `integer()`.

    The k parameter for the graph segmentation

  - **sigma**: `float`.

    The sigma parameter for the graph segmentation

  Python prototype (for reference only):
  ```python3
  switchToSingleStrategy([, k[, sigma]]) -> None
  ```
  """
  @spec switchToSingleStrategy(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t(), [{:k, term()} | {:sigma, term()}] | nil) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def switchToSingleStrategy(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:k, :sigma])
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_switchToSingleStrategy(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Initialize the class with the 'Single stragegy' parameters describled in @cite uijlings2013selective.

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`

  ##### Keyword Arguments
  - **k**: `integer()`.

    The k parameter for the graph segmentation

  - **sigma**: `float`.

    The sigma parameter for the graph segmentation

  Python prototype (for reference only):
  ```python3
  switchToSingleStrategy([, k[, sigma]]) -> None
  ```
  """
  @spec switchToSingleStrategy(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def switchToSingleStrategy(self) do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_ximgproc_segmentation_SelectiveSearchSegmentation_switchToSingleStrategy(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t(), Evision.FileStorage.t(), binary()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentation_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t(), Evision.FileStorage.t()) :: Evision.XImgProc.Segmentation.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.ximgproc_segmentation_SelectiveSearchSegmentation_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
