defmodule Evision.LineDescriptor.BinaryDescriptorMatcher do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineDescriptor.BinaryDescriptorMatcher` struct.

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
  def to_struct({:ok, %{class: Evision.LineDescriptor.BinaryDescriptorMatcher, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineDescriptor.BinaryDescriptorMatcher, ref: ref}) do
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
  Constructor.
  ##### Return
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`

  The BinaryDescriptorMatcher constructed is able to store and manage 256-bits long entries.

  Python prototype (for reference only):
  ```python3
  BinaryDescriptorMatcher() -> <line_descriptor_BinaryDescriptorMatcher object>
  ```
  """
  @spec binaryDescriptorMatcher() :: Evision.LineDescriptor.BinaryDescriptorMatcher.t() | {:error, String.t()}
  def binaryDescriptorMatcher() do
    positional = [
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptorMatcher_BinaryDescriptorMatcher(positional)
    |> to_struct()
  end

  @doc """
  Clears the algorithm state

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.LineDescriptor.BinaryDescriptorMatcher.t()) :: Evision.LineDescriptor.BinaryDescriptorMatcher.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_BinaryDescriptorMatcher_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.LineDescriptor.BinaryDescriptorMatcher.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_BinaryDescriptorMatcher_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.LineDescriptor.BinaryDescriptorMatcher.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.line_descriptor_BinaryDescriptorMatcher_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  For every input query descriptor, retrieve the best *k* matching ones from a dataset provided from
  user or from the one internal to class

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    query descriptors

  - **trainDescriptors**: `Evision.Mat`.

    dataset of descriptors furnished by user

  - **k**: `integer()`.

    number of the closest descriptors to be returned for every input query

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    mask to select which input descriptors must be matched to ones in dataset

  - **compactResult**: `bool`.

    flag to obtain a compact result (if true, a vector that doesn't contain any
    matches for a given query is not inserted in final result)

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    vector to host retrieved matches

  Python prototype (for reference only):
  ```python3
  knnMatch(queryDescriptors, trainDescriptors, k[, mask[, compactResult]]) -> matches
  ```
  """
  @spec knnMatch(Evision.LineDescriptor.BinaryDescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{:compactResult, term()} | {:mask, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, trainDescriptors, k, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_integer(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :mask])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptorMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  For every input query descriptor, retrieve the best *k* matching ones from a dataset provided from
  user or from the one internal to class

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    query descriptors

  - **trainDescriptors**: `Evision.Mat`.

    dataset of descriptors furnished by user

  - **k**: `integer()`.

    number of the closest descriptors to be returned for every input query

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    mask to select which input descriptors must be matched to ones in dataset

  - **compactResult**: `bool`.

    flag to obtain a compact result (if true, a vector that doesn't contain any
    matches for a given query is not inserted in final result)

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    vector to host retrieved matches

  Python prototype (for reference only):
  ```python3
  knnMatch(queryDescriptors, trainDescriptors, k[, mask[, compactResult]]) -> matches
  ```
  """
  @spec knnMatch(Evision.LineDescriptor.BinaryDescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, trainDescriptors, k) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_integer(k)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptorMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  knnMatchQuery

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    query descriptors

  - **matches**: `[[Evision.DMatch]]`.

    vector to host retrieved matches

  - **k**: `integer()`.

    number of the closest descriptors to be returned for every input query

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    vector of masks to select which input descriptors must be matched to ones in dataset
    (the *i*-th mask in vector indicates whether each input query can be matched with descriptors in
    dataset relative to *i*-th image)

  - **compactResult**: `bool`.

    flag to obtain a compact result (if true, a vector that doesn't contain any
    matches for a given query is not inserted in final result)

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  knnMatchQuery(queryDescriptors, matches, k[, masks[, compactResult]]) -> None
  ```
  """
  @spec knnMatchQuery(Evision.LineDescriptor.BinaryDescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), list(list(Evision.DMatch.t())), integer(), [{:compactResult, term()} | {:masks, term()}] | nil) :: Evision.LineDescriptor.BinaryDescriptorMatcher.t() | {:error, String.t()}
  def knnMatchQuery(self, queryDescriptors, matches, k, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_list(matches) and is_integer(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :masks])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      matches: Evision.Internal.Structurise.from_struct(matches),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptorMatcher_knnMatchQuery(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  knnMatchQuery

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    query descriptors

  - **matches**: `[[Evision.DMatch]]`.

    vector to host retrieved matches

  - **k**: `integer()`.

    number of the closest descriptors to be returned for every input query

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    vector of masks to select which input descriptors must be matched to ones in dataset
    (the *i*-th mask in vector indicates whether each input query can be matched with descriptors in
    dataset relative to *i*-th image)

  - **compactResult**: `bool`.

    flag to obtain a compact result (if true, a vector that doesn't contain any
    matches for a given query is not inserted in final result)

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  knnMatchQuery(queryDescriptors, matches, k[, masks[, compactResult]]) -> None
  ```
  """
  @spec knnMatchQuery(Evision.LineDescriptor.BinaryDescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), list(list(Evision.DMatch.t())), integer()) :: Evision.LineDescriptor.BinaryDescriptorMatcher.t() | {:error, String.t()}
  def knnMatchQuery(self, queryDescriptors, matches, k) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_list(matches) and is_integer(k)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      matches: Evision.Internal.Structurise.from_struct(matches),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptorMatcher_knnMatchQuery(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  For every input query descriptor, retrieve the best matching one from a dataset provided from user
  or from the one internal to class

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    query descriptors

  - **trainDescriptors**: `Evision.Mat`.

    dataset of descriptors furnished by user

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    mask to select which input descriptors must be matched to one in dataset

  ##### Return
  - **matches**: `[Evision.DMatch]`.

    vector to host retrieved matches

  Python prototype (for reference only):
  ```python3
  match(queryDescriptors, trainDescriptors[, mask]) -> matches
  ```
  """
  @spec match(Evision.LineDescriptor.BinaryDescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors, trainDescriptors, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptorMatcher_match(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  For every input query descriptor, retrieve the best matching one from a dataset provided from user
  or from the one internal to class

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    query descriptors

  - **trainDescriptors**: `Evision.Mat`.

    dataset of descriptors furnished by user

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    mask to select which input descriptors must be matched to one in dataset

  ##### Return
  - **matches**: `[Evision.DMatch]`.

    vector to host retrieved matches

  Python prototype (for reference only):
  ```python3
  match(queryDescriptors, trainDescriptors[, mask]) -> matches
  ```
  """
  @spec match(Evision.LineDescriptor.BinaryDescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors, trainDescriptors) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors))
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptorMatcher_match(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  matchQuery

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    query descriptors

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    vector of masks to select which input descriptors must be matched to one in dataset
    (the *i*-th mask in vector indicates whether each input query can be matched with descriptors in
    dataset relative to *i*-th image)

  ##### Return
  - **matches**: `[Evision.DMatch]`.

    vector to host retrieved matches

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  matchQuery(queryDescriptors[, masks]) -> matches
  ```
  """
  @spec matchQuery(Evision.LineDescriptor.BinaryDescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), [{:masks, term()}] | nil) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def matchQuery(self, queryDescriptors, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptorMatcher_matchQuery(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  matchQuery

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    query descriptors

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    vector of masks to select which input descriptors must be matched to one in dataset
    (the *i*-th mask in vector indicates whether each input query can be matched with descriptors in
    dataset relative to *i*-th image)

  ##### Return
  - **matches**: `[Evision.DMatch]`.

    vector to host retrieved matches

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  matchQuery(queryDescriptors[, masks]) -> matches
  ```
  """
  @spec matchQuery(Evision.LineDescriptor.BinaryDescriptorMatcher.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def matchQuery(self, queryDescriptors) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors))
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors)
    ]
    :evision_nif.line_descriptor_line_descriptor_BinaryDescriptorMatcher_matchQuery(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.LineDescriptor.BinaryDescriptorMatcher.t(), Evision.FileNode.t()) :: Evision.LineDescriptor.BinaryDescriptorMatcher.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.line_descriptor_BinaryDescriptorMatcher_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.LineDescriptor.BinaryDescriptorMatcher.t(), binary()) :: Evision.LineDescriptor.BinaryDescriptorMatcher.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.line_descriptor_BinaryDescriptorMatcher_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.LineDescriptor.BinaryDescriptorMatcher.t(), Evision.FileStorage.t(), binary()) :: Evision.LineDescriptor.BinaryDescriptorMatcher.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.line_descriptor_BinaryDescriptorMatcher_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.LineDescriptor.BinaryDescriptorMatcher.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.LineDescriptor.BinaryDescriptorMatcher.t(), Evision.FileStorage.t()) :: Evision.LineDescriptor.BinaryDescriptorMatcher.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.line_descriptor_BinaryDescriptorMatcher_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
