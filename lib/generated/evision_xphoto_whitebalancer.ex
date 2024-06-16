defmodule Evision.XPhoto.WhiteBalancer do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XPhoto.WhiteBalancer` struct.

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
  def to_struct({:ok, %{class: Evision.XPhoto.WhiteBalancer, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XPhoto.WhiteBalancer, ref: ref}) do
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
  Applies white balancing to the input image

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.WhiteBalancer.t()`
  - **src**: `Evision.Mat`.

    Input image

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    White balancing result

  @sa cvtColor, equalizeHist

  Python prototype (for reference only):
  ```python3
  balanceWhite(src[, dst]) -> dst
  ```
  """
  @spec balanceWhite(Evision.XPhoto.WhiteBalancer.t(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def balanceWhite(self, src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.xphoto_xphoto_WhiteBalancer_balanceWhite(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Applies white balancing to the input image

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.WhiteBalancer.t()`
  - **src**: `Evision.Mat`.

    Input image

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    White balancing result

  @sa cvtColor, equalizeHist

  Python prototype (for reference only):
  ```python3
  balanceWhite(src[, dst]) -> dst
  ```
  """
  @spec balanceWhite(Evision.XPhoto.WhiteBalancer.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def balanceWhite(self, src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.xphoto_xphoto_WhiteBalancer_balanceWhite(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.WhiteBalancer.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XPhoto.WhiteBalancer.t()) :: Evision.XPhoto.WhiteBalancer.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.xphoto_WhiteBalancer_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.WhiteBalancer.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XPhoto.WhiteBalancer.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.xphoto_WhiteBalancer_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.WhiteBalancer.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XPhoto.WhiteBalancer.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.xphoto_WhiteBalancer_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.WhiteBalancer.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XPhoto.WhiteBalancer.t(), Evision.FileNode.t()) :: Evision.XPhoto.WhiteBalancer.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.xphoto_WhiteBalancer_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.WhiteBalancer.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XPhoto.WhiteBalancer.t(), binary()) :: Evision.XPhoto.WhiteBalancer.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.xphoto_WhiteBalancer_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.WhiteBalancer.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XPhoto.WhiteBalancer.t(), Evision.FileStorage.t(), binary()) :: Evision.XPhoto.WhiteBalancer.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.xphoto_WhiteBalancer_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XPhoto.WhiteBalancer.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XPhoto.WhiteBalancer.t(), Evision.FileStorage.t()) :: Evision.XPhoto.WhiteBalancer.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.xphoto_WhiteBalancer_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
