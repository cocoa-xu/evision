defmodule Evision.XFeatures2D.PCTSignaturesSQFD do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XFeatures2D.PCTSignaturesSQFD` struct.

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
  def to_struct({:ok, %{class: Evision.XFeatures2D.PCTSignaturesSQFD, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XFeatures2D.PCTSignaturesSQFD, ref: ref}) do
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
  - **self**: `Evision.XFeatures2D.PCTSignaturesSQFD.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.XFeatures2D.PCTSignaturesSQFD.t()) :: Evision.XFeatures2D.PCTSignaturesSQFD.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_PCTSignaturesSQFD_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Computes Signature Quadratic Form Distance of two signatures.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignaturesSQFD.t()`
  - **signature0**: `Evision.Mat`.

    The first signature.

  - **signature1**: `Evision.Mat`.

    The second signature.

  ##### Return
  - **retval**: `float`

  Python prototype (for reference only):
  ```python3
  computeQuadraticFormDistance(_signature0, _signature1) -> retval
  ```
  """
  @spec computeQuadraticFormDistance(Evision.XFeatures2D.PCTSignaturesSQFD.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: number() | {:error, String.t()}
  def computeQuadraticFormDistance(self, signature0, signature1) when (is_struct(signature0, Evision.Mat) or is_struct(signature0, Nx.Tensor) or is_number(signature0) or is_tuple(signature0)) and (is_struct(signature1, Evision.Mat) or is_struct(signature1, Nx.Tensor) or is_number(signature1) or is_tuple(signature1))
  do
    positional = [
      signature0: Evision.Internal.Structurise.from_struct(signature0),
      signature1: Evision.Internal.Structurise.from_struct(signature1)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignaturesSQFD_computeQuadraticFormDistance(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Computes Signature Quadratic Form Distance between the reference signature
  and each of the other image signatures.

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignaturesSQFD.t()`
  - **sourceSignature**: `Evision.Mat`.

    The signature to measure distance of other signatures from.

  - **imageSignatures**: `[Evision.Mat]`.

    Vector of signatures to measure distance from the source signature.

  - **distances**: `[float]`.

    Output vector of measured distances.

  Python prototype (for reference only):
  ```python3
  computeQuadraticFormDistances(sourceSignature, imageSignatures, distances) -> None
  ```
  """
  @spec computeQuadraticFormDistances(Evision.XFeatures2D.PCTSignaturesSQFD.t(), Evision.Mat.maybe_mat_in(), list(Evision.Mat.maybe_mat_in()), list(number())) :: Evision.XFeatures2D.PCTSignaturesSQFD.t() | {:error, String.t()}
  def computeQuadraticFormDistances(self, sourceSignature, imageSignatures, distances) when (is_struct(sourceSignature, Evision.Mat) or is_struct(sourceSignature, Nx.Tensor) or is_number(sourceSignature) or is_tuple(sourceSignature)) and is_list(imageSignatures) and is_list(distances)
  do
    positional = [
      sourceSignature: Evision.Internal.Structurise.from_struct(sourceSignature),
      imageSignatures: Evision.Internal.Structurise.from_struct(imageSignatures),
      distances: Evision.Internal.Structurise.from_struct(distances)
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignaturesSQFD_computeQuadraticFormDistances(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Creates the algorithm instance using selected distance function,
  similarity function and similarity function parameter.

  ##### Keyword Arguments
  - **distanceFunction**: `integer()`.

    Distance function selector. Default: L2
    Available: L0_25, L0_5, L1, L2, L2SQUARED, L5, L_INFINITY

  - **similarityFunction**: `integer()`.

    Similarity function selector. Default: HEURISTIC
    Available: MINUS, GAUSSIAN, HEURISTIC

  - **similarityParameter**: `float`.

    Parameter of the similarity function.

  ##### Return
  - **retval**: `PCTSignaturesSQFD`

  Python prototype (for reference only):
  ```python3
  create([, distanceFunction[, similarityFunction[, similarityParameter]]]) -> retval
  ```
  """
  @spec create([{:distanceFunction, term()} | {:similarityFunction, term()} | {:similarityParameter, term()}] | nil) :: Evision.XFeatures2D.PCTSignaturesSQFD.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:distanceFunction, :similarityFunction, :similarityParameter])
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignaturesSQFD_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates the algorithm instance using selected distance function,
  similarity function and similarity function parameter.

  ##### Keyword Arguments
  - **distanceFunction**: `integer()`.

    Distance function selector. Default: L2
    Available: L0_25, L0_5, L1, L2, L2SQUARED, L5, L_INFINITY

  - **similarityFunction**: `integer()`.

    Similarity function selector. Default: HEURISTIC
    Available: MINUS, GAUSSIAN, HEURISTIC

  - **similarityParameter**: `float`.

    Parameter of the similarity function.

  ##### Return
  - **retval**: `PCTSignaturesSQFD`

  Python prototype (for reference only):
  ```python3
  create([, distanceFunction[, similarityFunction[, similarityParameter]]]) -> retval
  ```
  """
  @spec create() :: Evision.XFeatures2D.PCTSignaturesSQFD.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.xfeatures2d_xfeatures2d_PCTSignaturesSQFD_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignaturesSQFD.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.XFeatures2D.PCTSignaturesSQFD.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_PCTSignaturesSQFD_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignaturesSQFD.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.XFeatures2D.PCTSignaturesSQFD.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.xfeatures2d_PCTSignaturesSQFD_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignaturesSQFD.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.XFeatures2D.PCTSignaturesSQFD.t(), Evision.FileNode.t()) :: Evision.XFeatures2D.PCTSignaturesSQFD.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.xfeatures2d_PCTSignaturesSQFD_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignaturesSQFD.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.XFeatures2D.PCTSignaturesSQFD.t(), binary()) :: Evision.XFeatures2D.PCTSignaturesSQFD.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.xfeatures2d_PCTSignaturesSQFD_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignaturesSQFD.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.XFeatures2D.PCTSignaturesSQFD.t(), Evision.FileStorage.t(), binary()) :: Evision.XFeatures2D.PCTSignaturesSQFD.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.xfeatures2d_PCTSignaturesSQFD_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.XFeatures2D.PCTSignaturesSQFD.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.XFeatures2D.PCTSignaturesSQFD.t(), Evision.FileStorage.t()) :: Evision.XFeatures2D.PCTSignaturesSQFD.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.xfeatures2d_PCTSignaturesSQFD_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
