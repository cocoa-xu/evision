defmodule Evision.DNN.Layer do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `DNN.Layer` struct.

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
  def to_struct({:ok, %{class: Evision.DNN.Layer, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.DNN.Layer, ref: ref}) do
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
  - **self**: `Evision.DNN.Layer.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.DNN.Layer.t()) :: Evision.DNN.Layer.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.dnn_Layer_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.DNN.Layer.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.DNN.Layer.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.dnn_Layer_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Computes and sets internal parameters according to inputs, outputs and blobs.

  ##### Positional Arguments
  - **self**: `Evision.DNN.Layer.t()`
  - **inputs**: `[Evision.Mat]`

  ##### Return
  - **outputs**: `[Evision.Mat]`.

    vector of already allocated output blobs

   This method is called after network has allocated all memory for input and output blobs
   and before inferencing.

  Python prototype (for reference only):
  ```python3
  finalize(inputs[, outputs]) -> outputs
  ```
  """
  @spec finalize(Evision.DNN.Layer.t(), list(Evision.Mat.maybe_mat_in()), [{atom(), term()},...] | nil) :: list(Evision.Mat.t()) | {:error, String.t()}
  def finalize(self, inputs, opts) when is_list(inputs) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      inputs: Evision.Internal.Structurise.from_struct(inputs)
    ]
    :evision_nif.dnn_dnn_Layer_finalize(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes and sets internal parameters according to inputs, outputs and blobs.

  ##### Positional Arguments
  - **self**: `Evision.DNN.Layer.t()`
  - **inputs**: `[Evision.Mat]`

  ##### Return
  - **outputs**: `[Evision.Mat]`.

    vector of already allocated output blobs

   This method is called after network has allocated all memory for input and output blobs
   and before inferencing.

  Python prototype (for reference only):
  ```python3
  finalize(inputs[, outputs]) -> outputs
  ```
  """
  @spec finalize(Evision.DNN.Layer.t(), list(Evision.Mat.maybe_mat_in())) :: list(Evision.Mat.t()) | {:error, String.t()}
  def finalize(self, inputs) when is_list(inputs)
  do
    positional = [
      inputs: Evision.Internal.Structurise.from_struct(inputs)
    ]
    :evision_nif.dnn_dnn_Layer_finalize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.DNN.Layer.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.DNN.Layer.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.dnn_Layer_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns index of output blob in output array.

  ##### Positional Arguments
  - **self**: `Evision.DNN.Layer.t()`
  - **outputName**: `String`

  ##### Return
  - **retval**: `integer()`

  @see inputNameToIndex()

  Python prototype (for reference only):
  ```python3
  outputNameToIndex(outputName) -> retval
  ```
  """
  @spec outputNameToIndex(Evision.DNN.Layer.t(), binary()) :: integer() | {:error, String.t()}
  def outputNameToIndex(self, outputName) when is_binary(outputName)
  do
    positional = [
      outputName: Evision.Internal.Structurise.from_struct(outputName)
    ]
    :evision_nif.dnn_dnn_Layer_outputNameToIndex(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.DNN.Layer.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.DNN.Layer.t(), Evision.FileNode.t()) :: Evision.DNN.Layer.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.dnn_Layer_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Allocates layer and computes output.

  ##### Positional Arguments
  - **self**: `Evision.DNN.Layer.t()`
  - **inputs**: `[Evision.Mat]`

  ##### Return
  - **outputs**: `[Evision.Mat]`.
  - **internals**: `[Evision.Mat]`

  @deprecated This method will be removed in the future release.

  Python prototype (for reference only):
  ```python3
  run(inputs, internals[, outputs]) -> outputs, internals
  ```
  """
  @spec run(Evision.DNN.Layer.t(), list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in()), [{atom(), term()},...] | nil) :: {list(Evision.Mat.t()), list(Evision.Mat.t())} | {:error, String.t()}
  def run(self, inputs, internals, opts) when is_list(inputs) and is_list(internals) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      inputs: Evision.Internal.Structurise.from_struct(inputs),
      internals: Evision.Internal.Structurise.from_struct(internals)
    ]
    :evision_nif.dnn_dnn_Layer_run(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Allocates layer and computes output.

  ##### Positional Arguments
  - **self**: `Evision.DNN.Layer.t()`
  - **inputs**: `[Evision.Mat]`

  ##### Return
  - **outputs**: `[Evision.Mat]`.
  - **internals**: `[Evision.Mat]`

  @deprecated This method will be removed in the future release.

  Python prototype (for reference only):
  ```python3
  run(inputs, internals[, outputs]) -> outputs, internals
  ```
  """
  @spec run(Evision.DNN.Layer.t(), list(Evision.Mat.maybe_mat_in()), list(Evision.Mat.maybe_mat_in())) :: {list(Evision.Mat.t()), list(Evision.Mat.t())} | {:error, String.t()}
  def run(self, inputs, internals) when is_list(inputs) and is_list(internals)
  do
    positional = [
      inputs: Evision.Internal.Structurise.from_struct(inputs),
      internals: Evision.Internal.Structurise.from_struct(internals)
    ]
    :evision_nif.dnn_dnn_Layer_run(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.DNN.Layer.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.DNN.Layer.t(), binary()) :: Evision.DNN.Layer.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.dnn_Layer_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.DNN.Layer.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.DNN.Layer.t(), Evision.FileStorage.t(), binary()) :: Evision.DNN.Layer.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.dnn_Layer_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.DNN.Layer.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.DNN.Layer.t(), Evision.FileStorage.t()) :: Evision.DNN.Layer.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.dnn_Layer_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_blobs(Evision.DNN.Layer.t()) :: list(Evision.Mat.t())
  def get_blobs(self) do
    :evision_nif.dnn_Layer_get_blobs(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_blobs(Evision.DNN.Layer.t(), list(Evision.Mat.maybe_mat_in())) :: Evision.DNN.Layer.t()
  def set_blobs(self, prop) do
    :evision_nif.dnn_Layer_set_blobs(
        Evision.Internal.Structurise.from_struct(self),
        [blobs: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_name(Evision.DNN.Layer.t()) :: binary()
  def get_name(self) do
    :evision_nif.dnn_Layer_get_name(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_preferableTarget(Evision.DNN.Layer.t()) :: integer()
  def get_preferableTarget(self) do
    :evision_nif.dnn_Layer_get_preferableTarget(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec get_type(Evision.DNN.Layer.t()) :: binary()
  def get_type(self) do
    :evision_nif.dnn_Layer_get_type(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
end
