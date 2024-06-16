defmodule Evision.CUDA.DescriptorMatcher do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.DescriptorMatcher` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.DescriptorMatcher, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.DescriptorMatcher, ref: ref}) do
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
  Adds descriptors to train a descriptor collection.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **descriptors**: `[Evision.CUDA.GpuMat]`.

    Descriptors to add. Each descriptors[i] is a set of descriptors from the same
    train image.

  If the collection is not empty, the new descriptors are added to existing train descriptors.

  Python prototype (for reference only):
  ```python3
  add(descriptors) -> None
  ```
  """
  @spec add(Evision.CUDA.DescriptorMatcher.t(), list(Evision.CUDA.GpuMat.t())) :: Evision.CUDA.DescriptorMatcher.t() | {:error, String.t()}
  def add(self, descriptors) when is_list(descriptors)
  do
    positional = [
      descriptors: Evision.Internal.Structurise.from_struct(descriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_add(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the train descriptor collection.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CUDA.DescriptorMatcher.t()) :: Evision.CUDA.DescriptorMatcher.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Brute-force descriptor matcher.
  ##### Keyword Arguments
  - **normType**: `integer()`.

    One of NORM_L1, NORM_L2, NORM_HAMMING. L1 and L2 norms are
    preferable choices for SIFT and SURF descriptors, NORM_HAMMING should be used with ORB, BRISK and
    BRIEF).

  ##### Return
  - **retval**: `Evision.CUDA.DescriptorMatcher.t()`

  For each descriptor in the first set, this matcher finds the closest descriptor in the second set
  by trying each one. This descriptor matcher supports masking permissible matches of descriptor
  sets.

  Python prototype (for reference only):
  ```python3
  createBFMatcher([, normType]) -> retval
  ```
  """
  @spec createBFMatcher([{:normType, term()}] | nil) :: Evision.CUDA.DescriptorMatcher.t() | {:error, String.t()}
  def createBFMatcher(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:normType])
    positional = [
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_createBFMatcher_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Brute-force descriptor matcher.
  ##### Keyword Arguments
  - **normType**: `integer()`.

    One of NORM_L1, NORM_L2, NORM_HAMMING. L1 and L2 norms are
    preferable choices for SIFT and SURF descriptors, NORM_HAMMING should be used with ORB, BRISK and
    BRIEF).

  ##### Return
  - **retval**: `Evision.CUDA.DescriptorMatcher.t()`

  For each descriptor in the first set, this matcher finds the closest descriptor in the second set
  by trying each one. This descriptor matcher supports masking permissible matches of descriptor
  sets.

  Python prototype (for reference only):
  ```python3
  createBFMatcher([, normType]) -> retval
  ```
  """
  @spec createBFMatcher() :: Evision.CUDA.DescriptorMatcher.t() | {:error, String.t()}
  def createBFMatcher() do
    positional = [
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_createBFMatcher_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if there are no train descriptors in the collection.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.DescriptorMatcher.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CUDA.DescriptorMatcher.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.cuda_DescriptorMatcher_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns a constant link to the train descriptor collection.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`

  ##### Return
  - **retval**: `[Evision.CUDA.GpuMat]`

  Python prototype (for reference only):
  ```python3
  getTrainDescriptors() -> retval
  ```
  """
  @spec getTrainDescriptors(Evision.CUDA.DescriptorMatcher.t()) :: list(Evision.CUDA.GpuMat.t()) | {:error, String.t()}
  def getTrainDescriptors(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_getTrainDescriptors(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the descriptor matcher supports masking permissible matches.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isMaskSupported() -> retval
  ```
  """
  @spec isMaskSupported(Evision.CUDA.DescriptorMatcher.t()) :: boolean() | {:error, String.t()}
  def isMaskSupported(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_isMaskSupported(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds the k best matches for each descriptor from a query set (blocking version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.Mat`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **k**: `integer()`.

    Count of best matches found per each query descriptor or less if a query descriptor has
    less than k possible matches in total.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Matches. Each matches[i] is k or less matches for the same query descriptor.

  These extended variants of DescriptorMatcher::match methods find several best matches for each query
  descriptor. The matches are returned in the distance increasing order. See DescriptorMatcher::match
  for the details about query and train descriptors.

  Python prototype (for reference only):
  ```python3
  knnMatch(queryDescriptors, trainDescriptors, k[, mask[, compactResult]]) -> matches
  ```
  #### Variant 2:
  Finds the k best matches for each descriptor from a query set (blocking version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **k**: `integer()`.

    Count of best matches found per each query descriptor or less if a query descriptor has
    less than k possible matches in total.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Matches. Each matches[i] is k or less matches for the same query descriptor.

  These extended variants of DescriptorMatcher::match methods find several best matches for each query
  descriptor. The matches are returned in the distance increasing order. See DescriptorMatcher::match
  for the details about query and train descriptors.

  Python prototype (for reference only):
  ```python3
  knnMatch(queryDescriptors, trainDescriptors, k[, mask[, compactResult]]) -> matches
  ```

  """
  @spec knnMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{:compactResult, term()} | {:mask, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, trainDescriptors, k, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_integer(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :mask])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec knnMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer(), [{:compactResult, term()} | {:mask, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, trainDescriptors, k, opts) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_struct(trainDescriptors, Evision.CUDA.GpuMat) and is_integer(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :mask])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds the k best matches for each descriptor from a query set (blocking version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.Mat`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **k**: `integer()`.

    Count of best matches found per each query descriptor or less if a query descriptor has
    less than k possible matches in total.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Matches. Each matches[i] is k or less matches for the same query descriptor.

  These extended variants of DescriptorMatcher::match methods find several best matches for each query
  descriptor. The matches are returned in the distance increasing order. See DescriptorMatcher::match
  for the details about query and train descriptors.

  Python prototype (for reference only):
  ```python3
  knnMatch(queryDescriptors, trainDescriptors, k[, mask[, compactResult]]) -> matches
  ```
  #### Variant 2:
  Finds the k best matches for each descriptor from a query set (blocking version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **k**: `integer()`.

    Count of best matches found per each query descriptor or less if a query descriptor has
    less than k possible matches in total.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Matches. Each matches[i] is k or less matches for the same query descriptor.

  These extended variants of DescriptorMatcher::match methods find several best matches for each query
  descriptor. The matches are returned in the distance increasing order. See DescriptorMatcher::match
  for the details about query and train descriptors.

  Python prototype (for reference only):
  ```python3
  knnMatch(queryDescriptors, trainDescriptors, k[, mask[, compactResult]]) -> matches
  ```
  #### Variant 3:
  knnMatch

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`
  - **k**: `integer()`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **compactResult**: `bool`.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  knnMatch(queryDescriptors, k[, masks[, compactResult]]) -> matches
  ```
  #### Variant 4:
  knnMatch

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`
  - **k**: `integer()`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **compactResult**: `bool`.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  knnMatch(queryDescriptors, k[, masks[, compactResult]]) -> matches
  ```

  """
  @spec knnMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), integer(), [{:compactResult, term()} | {:masks, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, k, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_integer(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :masks])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec knnMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), integer(), [{:compactResult, term()} | {:masks, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, k, opts) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_integer(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :masks])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec knnMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, trainDescriptors, k) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_integer(k)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec knnMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, trainDescriptors, k) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_struct(trainDescriptors, Evision.CUDA.GpuMat) and is_integer(k)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  knnMatch

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`
  - **k**: `integer()`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **compactResult**: `bool`.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  knnMatch(queryDescriptors, k[, masks[, compactResult]]) -> matches
  ```
  #### Variant 2:
  knnMatch

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`
  - **k**: `integer()`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **compactResult**: `bool`.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  knnMatch(queryDescriptors, k[, masks[, compactResult]]) -> matches
  ```

  """
  @spec knnMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), integer()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, k) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_integer(k)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec knnMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), integer()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, k) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_integer(k)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds the k best matches for each descriptor from a query set (asynchronous version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.Mat`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **k**: `integer()`.

    Count of best matches found per each query descriptor or less if a query descriptor has
    less than k possible matches in total.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **matches**: `Evision.Mat.t()`.

    Matches array stored in GPU memory. Internal representation is not defined.
    Use DescriptorMatcher::knnMatchConvert method to retrieve results in standard representation.

  These extended variants of DescriptorMatcher::matchAsync methods find several best matches for each query
  descriptor. The matches are returned in the distance increasing order. See DescriptorMatcher::matchAsync
  for the details about query and train descriptors.

  Python prototype (for reference only):
  ```python3
  knnMatchAsync(queryDescriptors, trainDescriptors, k[, matches[, mask[, stream]]]) -> matches
  ```
  #### Variant 2:
  Finds the k best matches for each descriptor from a query set (asynchronous version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **k**: `integer()`.

    Count of best matches found per each query descriptor or less if a query descriptor has
    less than k possible matches in total.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **matches**: `Evision.CUDA.GpuMat.t()`.

    Matches array stored in GPU memory. Internal representation is not defined.
    Use DescriptorMatcher::knnMatchConvert method to retrieve results in standard representation.

  These extended variants of DescriptorMatcher::matchAsync methods find several best matches for each query
  descriptor. The matches are returned in the distance increasing order. See DescriptorMatcher::matchAsync
  for the details about query and train descriptors.

  Python prototype (for reference only):
  ```python3
  knnMatchAsync(queryDescriptors, trainDescriptors, k[, matches[, mask[, stream]]]) -> matches
  ```

  """
  @spec knnMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def knnMatchAsync(self, queryDescriptors, trainDescriptors, k, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_integer(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatchAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec knnMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def knnMatchAsync(self, queryDescriptors, trainDescriptors, k, opts) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_struct(trainDescriptors, Evision.CUDA.GpuMat) and is_integer(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatchAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds the k best matches for each descriptor from a query set (asynchronous version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.Mat`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **k**: `integer()`.

    Count of best matches found per each query descriptor or less if a query descriptor has
    less than k possible matches in total.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **matches**: `Evision.Mat.t()`.

    Matches array stored in GPU memory. Internal representation is not defined.
    Use DescriptorMatcher::knnMatchConvert method to retrieve results in standard representation.

  These extended variants of DescriptorMatcher::matchAsync methods find several best matches for each query
  descriptor. The matches are returned in the distance increasing order. See DescriptorMatcher::matchAsync
  for the details about query and train descriptors.

  Python prototype (for reference only):
  ```python3
  knnMatchAsync(queryDescriptors, trainDescriptors, k[, matches[, mask[, stream]]]) -> matches
  ```
  #### Variant 2:
  Finds the k best matches for each descriptor from a query set (asynchronous version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **k**: `integer()`.

    Count of best matches found per each query descriptor or less if a query descriptor has
    less than k possible matches in total.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **matches**: `Evision.CUDA.GpuMat.t()`.

    Matches array stored in GPU memory. Internal representation is not defined.
    Use DescriptorMatcher::knnMatchConvert method to retrieve results in standard representation.

  These extended variants of DescriptorMatcher::matchAsync methods find several best matches for each query
  descriptor. The matches are returned in the distance increasing order. See DescriptorMatcher::matchAsync
  for the details about query and train descriptors.

  Python prototype (for reference only):
  ```python3
  knnMatchAsync(queryDescriptors, trainDescriptors, k[, matches[, mask[, stream]]]) -> matches
  ```
  #### Variant 3:
  knnMatchAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`
  - **k**: `integer()`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **matches**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  knnMatchAsync(queryDescriptors, k[, matches[, masks[, stream]]]) -> matches
  ```
  #### Variant 4:
  knnMatchAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`
  - **k**: `integer()`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **matches**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  knnMatchAsync(queryDescriptors, k[, matches[, masks[, stream]]]) -> matches
  ```

  """
  @spec knnMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), integer(), [{:masks, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def knnMatchAsync(self, queryDescriptors, k, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_integer(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks, :stream])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatchAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec knnMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), integer(), [{:masks, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def knnMatchAsync(self, queryDescriptors, k, opts) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_integer(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks, :stream])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatchAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec knnMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def knnMatchAsync(self, queryDescriptors, trainDescriptors, k) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_integer(k)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatchAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec knnMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def knnMatchAsync(self, queryDescriptors, trainDescriptors, k) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_struct(trainDescriptors, Evision.CUDA.GpuMat) and is_integer(k)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatchAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  knnMatchAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`
  - **k**: `integer()`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **matches**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  knnMatchAsync(queryDescriptors, k[, matches[, masks[, stream]]]) -> matches
  ```
  #### Variant 2:
  knnMatchAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`
  - **k**: `integer()`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **matches**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  knnMatchAsync(queryDescriptors, k[, matches[, masks[, stream]]]) -> matches
  ```

  """
  @spec knnMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def knnMatchAsync(self, queryDescriptors, k) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_integer(k)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatchAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec knnMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), integer()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def knnMatchAsync(self, queryDescriptors, k) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_integer(k)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatchAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Converts matches array from internal representation to standard matches vector.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **gpu_matches**: `Evision.Mat`.

    Matches, returned from DescriptorMatcher::knnMatchAsync.

  ##### Keyword Arguments
  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Vector of DMatch objects.

  The method is supposed to be used with DescriptorMatcher::knnMatchAsync to get final result.
  Call this method only after DescriptorMatcher::knnMatchAsync is completed (ie. after synchronization).

  Python prototype (for reference only):
  ```python3
  knnMatchConvert(gpu_matches[, compactResult]) -> matches
  ```
  #### Variant 2:
  Converts matches array from internal representation to standard matches vector.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **gpu_matches**: `Evision.CUDA.GpuMat.t()`.

    Matches, returned from DescriptorMatcher::knnMatchAsync.

  ##### Keyword Arguments
  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Vector of DMatch objects.

  The method is supposed to be used with DescriptorMatcher::knnMatchAsync to get final result.
  Call this method only after DescriptorMatcher::knnMatchAsync is completed (ie. after synchronization).

  Python prototype (for reference only):
  ```python3
  knnMatchConvert(gpu_matches[, compactResult]) -> matches
  ```

  """
  @spec knnMatchConvert(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), [{:compactResult, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatchConvert(self, gpu_matches, opts) when (is_struct(gpu_matches, Evision.Mat) or is_struct(gpu_matches, Nx.Tensor) or is_number(gpu_matches) or is_tuple(gpu_matches)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult])
    positional = [
      gpu_matches: Evision.Internal.Structurise.from_struct(gpu_matches)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatchConvert(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec knnMatchConvert(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), [{:compactResult, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatchConvert(self, gpu_matches, opts) when is_struct(gpu_matches, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult])
    positional = [
      gpu_matches: Evision.Internal.Structurise.from_struct(gpu_matches)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatchConvert(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Converts matches array from internal representation to standard matches vector.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **gpu_matches**: `Evision.Mat`.

    Matches, returned from DescriptorMatcher::knnMatchAsync.

  ##### Keyword Arguments
  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Vector of DMatch objects.

  The method is supposed to be used with DescriptorMatcher::knnMatchAsync to get final result.
  Call this method only after DescriptorMatcher::knnMatchAsync is completed (ie. after synchronization).

  Python prototype (for reference only):
  ```python3
  knnMatchConvert(gpu_matches[, compactResult]) -> matches
  ```
  #### Variant 2:
  Converts matches array from internal representation to standard matches vector.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **gpu_matches**: `Evision.CUDA.GpuMat.t()`.

    Matches, returned from DescriptorMatcher::knnMatchAsync.

  ##### Keyword Arguments
  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Vector of DMatch objects.

  The method is supposed to be used with DescriptorMatcher::knnMatchAsync to get final result.
  Call this method only after DescriptorMatcher::knnMatchAsync is completed (ie. after synchronization).

  Python prototype (for reference only):
  ```python3
  knnMatchConvert(gpu_matches[, compactResult]) -> matches
  ```

  """
  @spec knnMatchConvert(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatchConvert(self, gpu_matches) when (is_struct(gpu_matches, Evision.Mat) or is_struct(gpu_matches, Nx.Tensor) or is_number(gpu_matches) or is_tuple(gpu_matches))
  do
    positional = [
      gpu_matches: Evision.Internal.Structurise.from_struct(gpu_matches)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatchConvert(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec knnMatchConvert(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatchConvert(self, gpu_matches) when is_struct(gpu_matches, Evision.CUDA.GpuMat)
  do
    positional = [
      gpu_matches: Evision.Internal.Structurise.from_struct(gpu_matches)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_knnMatchConvert(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds the best match for each descriptor from a query set (blocking version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.Mat`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  ##### Return
  - **matches**: `[Evision.DMatch]`.

    Matches. If a query descriptor is masked out in mask , no match is added for this
    descriptor. So, matches size may be smaller than the query descriptors count.

  In the first variant of this method, the train descriptors are passed as an input argument. In the
  second variant of the method, train descriptors collection that was set by DescriptorMatcher::add is
  used. Optional mask (or masks) can be passed to specify which query and training descriptors can be
  matched. Namely, queryDescriptors[i] can be matched with trainDescriptors[j] only if
  mask.at\\<uchar\\>(i,j) is non-zero.

  Python prototype (for reference only):
  ```python3
  match(queryDescriptors, trainDescriptors[, mask]) -> matches
  ```
  #### Variant 2:
  Finds the best match for each descriptor from a query set (blocking version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  ##### Return
  - **matches**: `[Evision.DMatch]`.

    Matches. If a query descriptor is masked out in mask , no match is added for this
    descriptor. So, matches size may be smaller than the query descriptors count.

  In the first variant of this method, the train descriptors are passed as an input argument. In the
  second variant of the method, train descriptors collection that was set by DescriptorMatcher::add is
  used. Optional mask (or masks) can be passed to specify which query and training descriptors can be
  matched. Namely, queryDescriptors[i] can be matched with trainDescriptors[j] only if
  mask.at\\<uchar\\>(i,j) is non-zero.

  Python prototype (for reference only):
  ```python3
  match(queryDescriptors, trainDescriptors[, mask]) -> matches
  ```

  """
  @spec match(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors, trainDescriptors, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_match(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec match(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:mask, term()}] | nil) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors, trainDescriptors, opts) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_struct(trainDescriptors, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_match(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds the best match for each descriptor from a query set (blocking version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.Mat`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  ##### Return
  - **matches**: `[Evision.DMatch]`.

    Matches. If a query descriptor is masked out in mask , no match is added for this
    descriptor. So, matches size may be smaller than the query descriptors count.

  In the first variant of this method, the train descriptors are passed as an input argument. In the
  second variant of the method, train descriptors collection that was set by DescriptorMatcher::add is
  used. Optional mask (or masks) can be passed to specify which query and training descriptors can be
  matched. Namely, queryDescriptors[i] can be matched with trainDescriptors[j] only if
  mask.at\\<uchar\\>(i,j) is non-zero.

  Python prototype (for reference only):
  ```python3
  match(queryDescriptors, trainDescriptors[, mask]) -> matches
  ```
  #### Variant 2:
  Finds the best match for each descriptor from a query set (blocking version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  ##### Return
  - **matches**: `[Evision.DMatch]`.

    Matches. If a query descriptor is masked out in mask , no match is added for this
    descriptor. So, matches size may be smaller than the query descriptors count.

  In the first variant of this method, the train descriptors are passed as an input argument. In the
  second variant of the method, train descriptors collection that was set by DescriptorMatcher::add is
  used. Optional mask (or masks) can be passed to specify which query and training descriptors can be
  matched. Namely, queryDescriptors[i] can be matched with trainDescriptors[j] only if
  mask.at\\<uchar\\>(i,j) is non-zero.

  Python prototype (for reference only):
  ```python3
  match(queryDescriptors, trainDescriptors[, mask]) -> matches
  ```
  #### Variant 3:
  match

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.

  ##### Return
  - **matches**: `[Evision.DMatch]`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  match(queryDescriptors[, masks]) -> matches
  ```
  #### Variant 4:
  match

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.

  ##### Return
  - **matches**: `[Evision.DMatch]`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  match(queryDescriptors[, masks]) -> matches
  ```

  """
  @spec match(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), [{:masks, term()}] | nil) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_match(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec match(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), [{:masks, term()}] | nil) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors, opts) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_match(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec match(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors, trainDescriptors) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors))
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_match(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec match(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors, trainDescriptors) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_struct(trainDescriptors, Evision.CUDA.GpuMat)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_match(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  match

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.

  ##### Return
  - **matches**: `[Evision.DMatch]`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  match(queryDescriptors[, masks]) -> matches
  ```
  #### Variant 2:
  match

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.

  ##### Return
  - **matches**: `[Evision.DMatch]`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  match(queryDescriptors[, masks]) -> matches
  ```

  """
  @spec match(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors))
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_match(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec match(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t()) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors) when is_struct(queryDescriptors, Evision.CUDA.GpuMat)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_match(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds the best match for each descriptor from a query set (asynchronous version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.Mat`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **matches**: `Evision.Mat.t()`.

    Matches array stored in GPU memory. Internal representation is not defined.
    Use DescriptorMatcher::matchConvert method to retrieve results in standard representation.

  In the first variant of this method, the train descriptors are passed as an input argument. In the
  second variant of the method, train descriptors collection that was set by DescriptorMatcher::add is
  used. Optional mask (or masks) can be passed to specify which query and training descriptors can be
  matched. Namely, queryDescriptors[i] can be matched with trainDescriptors[j] only if
  mask.at\\<uchar\\>(i,j) is non-zero.

  Python prototype (for reference only):
  ```python3
  matchAsync(queryDescriptors, trainDescriptors[, matches[, mask[, stream]]]) -> matches
  ```
  #### Variant 2:
  Finds the best match for each descriptor from a query set (asynchronous version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **matches**: `Evision.CUDA.GpuMat.t()`.

    Matches array stored in GPU memory. Internal representation is not defined.
    Use DescriptorMatcher::matchConvert method to retrieve results in standard representation.

  In the first variant of this method, the train descriptors are passed as an input argument. In the
  second variant of the method, train descriptors collection that was set by DescriptorMatcher::add is
  used. Optional mask (or masks) can be passed to specify which query and training descriptors can be
  matched. Namely, queryDescriptors[i] can be matched with trainDescriptors[j] only if
  mask.at\\<uchar\\>(i,j) is non-zero.

  Python prototype (for reference only):
  ```python3
  matchAsync(queryDescriptors, trainDescriptors[, matches[, mask[, stream]]]) -> matches
  ```

  """
  @spec matchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def matchAsync(self, queryDescriptors, trainDescriptors, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_matchAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec matchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def matchAsync(self, queryDescriptors, trainDescriptors, opts) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_struct(trainDescriptors, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_matchAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds the best match for each descriptor from a query set (asynchronous version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.Mat`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **matches**: `Evision.Mat.t()`.

    Matches array stored in GPU memory. Internal representation is not defined.
    Use DescriptorMatcher::matchConvert method to retrieve results in standard representation.

  In the first variant of this method, the train descriptors are passed as an input argument. In the
  second variant of the method, train descriptors collection that was set by DescriptorMatcher::add is
  used. Optional mask (or masks) can be passed to specify which query and training descriptors can be
  matched. Namely, queryDescriptors[i] can be matched with trainDescriptors[j] only if
  mask.at\\<uchar\\>(i,j) is non-zero.

  Python prototype (for reference only):
  ```python3
  matchAsync(queryDescriptors, trainDescriptors[, matches[, mask[, stream]]]) -> matches
  ```
  #### Variant 2:
  Finds the best match for each descriptor from a query set (asynchronous version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **matches**: `Evision.CUDA.GpuMat.t()`.

    Matches array stored in GPU memory. Internal representation is not defined.
    Use DescriptorMatcher::matchConvert method to retrieve results in standard representation.

  In the first variant of this method, the train descriptors are passed as an input argument. In the
  second variant of the method, train descriptors collection that was set by DescriptorMatcher::add is
  used. Optional mask (or masks) can be passed to specify which query and training descriptors can be
  matched. Namely, queryDescriptors[i] can be matched with trainDescriptors[j] only if
  mask.at\\<uchar\\>(i,j) is non-zero.

  Python prototype (for reference only):
  ```python3
  matchAsync(queryDescriptors, trainDescriptors[, matches[, mask[, stream]]]) -> matches
  ```
  #### Variant 3:
  matchAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **matches**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  matchAsync(queryDescriptors[, matches[, masks[, stream]]]) -> matches
  ```
  #### Variant 4:
  matchAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **matches**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  matchAsync(queryDescriptors[, matches[, masks[, stream]]]) -> matches
  ```

  """
  @spec matchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), [{:masks, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def matchAsync(self, queryDescriptors, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks, :stream])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_matchAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec matchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), [{:masks, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def matchAsync(self, queryDescriptors, opts) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks, :stream])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_matchAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec matchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def matchAsync(self, queryDescriptors, trainDescriptors) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors))
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_matchAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec matchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def matchAsync(self, queryDescriptors, trainDescriptors) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_struct(trainDescriptors, Evision.CUDA.GpuMat)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_matchAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  matchAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **matches**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  matchAsync(queryDescriptors[, matches[, masks[, stream]]]) -> matches
  ```
  #### Variant 2:
  matchAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **matches**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  matchAsync(queryDescriptors[, matches[, masks[, stream]]]) -> matches
  ```

  """
  @spec matchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def matchAsync(self, queryDescriptors) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors))
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_matchAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec matchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def matchAsync(self, queryDescriptors) when is_struct(queryDescriptors, Evision.CUDA.GpuMat)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_matchAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Converts matches array from internal representation to standard matches vector.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **gpu_matches**: `Evision.Mat`.

    Matches, returned from DescriptorMatcher::matchAsync.

  ##### Return
  - **matches**: `[Evision.DMatch]`.

    Vector of DMatch objects.

  The method is supposed to be used with DescriptorMatcher::matchAsync to get final result.
  Call this method only after DescriptorMatcher::matchAsync is completed (ie. after synchronization).

  Python prototype (for reference only):
  ```python3
  matchConvert(gpu_matches) -> matches
  ```
  #### Variant 2:
  Converts matches array from internal representation to standard matches vector.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **gpu_matches**: `Evision.CUDA.GpuMat.t()`.

    Matches, returned from DescriptorMatcher::matchAsync.

  ##### Return
  - **matches**: `[Evision.DMatch]`.

    Vector of DMatch objects.

  The method is supposed to be used with DescriptorMatcher::matchAsync to get final result.
  Call this method only after DescriptorMatcher::matchAsync is completed (ie. after synchronization).

  Python prototype (for reference only):
  ```python3
  matchConvert(gpu_matches) -> matches
  ```

  """
  @spec matchConvert(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def matchConvert(self, gpu_matches) when (is_struct(gpu_matches, Evision.Mat) or is_struct(gpu_matches, Nx.Tensor) or is_number(gpu_matches) or is_tuple(gpu_matches))
  do
    positional = [
      gpu_matches: Evision.Internal.Structurise.from_struct(gpu_matches)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_matchConvert(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec matchConvert(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t()) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def matchConvert(self, gpu_matches) when is_struct(gpu_matches, Evision.CUDA.GpuMat)
  do
    positional = [
      gpu_matches: Evision.Internal.Structurise.from_struct(gpu_matches)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_matchConvert(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  For each query descriptor, finds the training descriptors not farther than the specified distance (blocking version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.Mat`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **maxDistance**: `float`.

    Threshold for the distance between matched descriptors. Distance means here
    metric distance (e.g. Hamming distance), not the distance between coordinates (which is measured
    in Pixels)!

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Found matches.

  For each query descriptor, the methods find such training descriptors that the distance between the
  query descriptor and the training descriptor is equal or smaller than maxDistance. Found matches are
  returned in the distance increasing order.

  Python prototype (for reference only):
  ```python3
  radiusMatch(queryDescriptors, trainDescriptors, maxDistance[, mask[, compactResult]]) -> matches
  ```
  #### Variant 2:
  For each query descriptor, finds the training descriptors not farther than the specified distance (blocking version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **maxDistance**: `float`.

    Threshold for the distance between matched descriptors. Distance means here
    metric distance (e.g. Hamming distance), not the distance between coordinates (which is measured
    in Pixels)!

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Found matches.

  For each query descriptor, the methods find such training descriptors that the distance between the
  query descriptor and the training descriptor is equal or smaller than maxDistance. Found matches are
  returned in the distance increasing order.

  Python prototype (for reference only):
  ```python3
  radiusMatch(queryDescriptors, trainDescriptors, maxDistance[, mask[, compactResult]]) -> matches
  ```

  """
  @spec radiusMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), [{:compactResult, term()} | {:mask, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatch(self, queryDescriptors, trainDescriptors, maxDistance, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_float(maxDistance) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :mask])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec radiusMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), number(), [{:compactResult, term()} | {:mask, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatch(self, queryDescriptors, trainDescriptors, maxDistance, opts) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_struct(trainDescriptors, Evision.CUDA.GpuMat) and is_float(maxDistance) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :mask])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  For each query descriptor, finds the training descriptors not farther than the specified distance (blocking version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.Mat`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **maxDistance**: `float`.

    Threshold for the distance between matched descriptors. Distance means here
    metric distance (e.g. Hamming distance), not the distance between coordinates (which is measured
    in Pixels)!

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Found matches.

  For each query descriptor, the methods find such training descriptors that the distance between the
  query descriptor and the training descriptor is equal or smaller than maxDistance. Found matches are
  returned in the distance increasing order.

  Python prototype (for reference only):
  ```python3
  radiusMatch(queryDescriptors, trainDescriptors, maxDistance[, mask[, compactResult]]) -> matches
  ```
  #### Variant 2:
  For each query descriptor, finds the training descriptors not farther than the specified distance (blocking version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **maxDistance**: `float`.

    Threshold for the distance between matched descriptors. Distance means here
    metric distance (e.g. Hamming distance), not the distance between coordinates (which is measured
    in Pixels)!

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Found matches.

  For each query descriptor, the methods find such training descriptors that the distance between the
  query descriptor and the training descriptor is equal or smaller than maxDistance. Found matches are
  returned in the distance increasing order.

  Python prototype (for reference only):
  ```python3
  radiusMatch(queryDescriptors, trainDescriptors, maxDistance[, mask[, compactResult]]) -> matches
  ```
  #### Variant 3:
  radiusMatch

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`
  - **maxDistance**: `float`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **compactResult**: `bool`.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  radiusMatch(queryDescriptors, maxDistance[, masks[, compactResult]]) -> matches
  ```
  #### Variant 4:
  radiusMatch

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`
  - **maxDistance**: `float`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **compactResult**: `bool`.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  radiusMatch(queryDescriptors, maxDistance[, masks[, compactResult]]) -> matches
  ```

  """
  @spec radiusMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), number(), [{:compactResult, term()} | {:masks, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatch(self, queryDescriptors, maxDistance, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_float(maxDistance) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :masks])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec radiusMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), number(), [{:compactResult, term()} | {:masks, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatch(self, queryDescriptors, maxDistance, opts) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_float(maxDistance) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :masks])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec radiusMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatch(self, queryDescriptors, trainDescriptors, maxDistance) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_float(maxDistance)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec radiusMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), number()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatch(self, queryDescriptors, trainDescriptors, maxDistance) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_struct(trainDescriptors, Evision.CUDA.GpuMat) and is_float(maxDistance)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  radiusMatch

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`
  - **maxDistance**: `float`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **compactResult**: `bool`.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  radiusMatch(queryDescriptors, maxDistance[, masks[, compactResult]]) -> matches
  ```
  #### Variant 2:
  radiusMatch

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`
  - **maxDistance**: `float`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **compactResult**: `bool`.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  radiusMatch(queryDescriptors, maxDistance[, masks[, compactResult]]) -> matches
  ```

  """
  @spec radiusMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), number()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatch(self, queryDescriptors, maxDistance) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_float(maxDistance)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec radiusMatch(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), number()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatch(self, queryDescriptors, maxDistance) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_float(maxDistance)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  For each query descriptor, finds the training descriptors not farther than the specified distance (asynchronous version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.Mat`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **maxDistance**: `float`.

    Threshold for the distance between matched descriptors. Distance means here
    metric distance (e.g. Hamming distance), not the distance between coordinates (which is measured
    in Pixels)!

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **matches**: `Evision.Mat.t()`.

    Matches array stored in GPU memory. Internal representation is not defined.
    Use DescriptorMatcher::radiusMatchConvert method to retrieve results in standard representation.

  For each query descriptor, the methods find such training descriptors that the distance between the
  query descriptor and the training descriptor is equal or smaller than maxDistance. Found matches are
  returned in the distance increasing order.

  Python prototype (for reference only):
  ```python3
  radiusMatchAsync(queryDescriptors, trainDescriptors, maxDistance[, matches[, mask[, stream]]]) -> matches
  ```
  #### Variant 2:
  For each query descriptor, finds the training descriptors not farther than the specified distance (asynchronous version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **maxDistance**: `float`.

    Threshold for the distance between matched descriptors. Distance means here
    metric distance (e.g. Hamming distance), not the distance between coordinates (which is measured
    in Pixels)!

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **matches**: `Evision.CUDA.GpuMat.t()`.

    Matches array stored in GPU memory. Internal representation is not defined.
    Use DescriptorMatcher::radiusMatchConvert method to retrieve results in standard representation.

  For each query descriptor, the methods find such training descriptors that the distance between the
  query descriptor and the training descriptor is equal or smaller than maxDistance. Found matches are
  returned in the distance increasing order.

  Python prototype (for reference only):
  ```python3
  radiusMatchAsync(queryDescriptors, trainDescriptors, maxDistance[, matches[, mask[, stream]]]) -> matches
  ```

  """
  @spec radiusMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def radiusMatchAsync(self, queryDescriptors, trainDescriptors, maxDistance, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_float(maxDistance) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatchAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec radiusMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), number(), [{:mask, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def radiusMatchAsync(self, queryDescriptors, trainDescriptors, maxDistance, opts) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_struct(trainDescriptors, Evision.CUDA.GpuMat) and is_float(maxDistance) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :stream])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatchAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  For each query descriptor, finds the training descriptors not farther than the specified distance (asynchronous version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.Mat`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **maxDistance**: `float`.

    Threshold for the distance between matched descriptors. Distance means here
    metric distance (e.g. Hamming distance), not the distance between coordinates (which is measured
    in Pixels)!

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **matches**: `Evision.Mat.t()`.

    Matches array stored in GPU memory. Internal representation is not defined.
    Use DescriptorMatcher::radiusMatchConvert method to retrieve results in standard representation.

  For each query descriptor, the methods find such training descriptors that the distance between the
  query descriptor and the training descriptor is equal or smaller than maxDistance. Found matches are
  returned in the distance increasing order.

  Python prototype (for reference only):
  ```python3
  radiusMatchAsync(queryDescriptors, trainDescriptors, maxDistance[, matches[, mask[, stream]]]) -> matches
  ```
  #### Variant 2:
  For each query descriptor, finds the training descriptors not farther than the specified distance (asynchronous version).

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Query set of descriptors.

  - **trainDescriptors**: `Evision.CUDA.GpuMat.t()`.

    Train set of descriptors. This set is not added to the train descriptors
    collection stored in the class object.

  - **maxDistance**: `float`.

    Threshold for the distance between matched descriptors. Distance means here
    metric distance (e.g. Hamming distance), not the distance between coordinates (which is measured
    in Pixels)!

  ##### Keyword Arguments
  - **mask**: `Evision.CUDA.GpuMat.t()`.

    Mask specifying permissible matches between an input query and train matrices of
    descriptors.

  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **matches**: `Evision.CUDA.GpuMat.t()`.

    Matches array stored in GPU memory. Internal representation is not defined.
    Use DescriptorMatcher::radiusMatchConvert method to retrieve results in standard representation.

  For each query descriptor, the methods find such training descriptors that the distance between the
  query descriptor and the training descriptor is equal or smaller than maxDistance. Found matches are
  returned in the distance increasing order.

  Python prototype (for reference only):
  ```python3
  radiusMatchAsync(queryDescriptors, trainDescriptors, maxDistance[, matches[, mask[, stream]]]) -> matches
  ```
  #### Variant 3:
  radiusMatchAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`
  - **maxDistance**: `float`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **matches**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  radiusMatchAsync(queryDescriptors, maxDistance[, matches[, masks[, stream]]]) -> matches
  ```
  #### Variant 4:
  radiusMatchAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`
  - **maxDistance**: `float`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **matches**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  radiusMatchAsync(queryDescriptors, maxDistance[, matches[, masks[, stream]]]) -> matches
  ```

  """
  @spec radiusMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), number(), [{:masks, term()} | {:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def radiusMatchAsync(self, queryDescriptors, maxDistance, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_float(maxDistance) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks, :stream])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatchAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec radiusMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), number(), [{:masks, term()} | {:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def radiusMatchAsync(self, queryDescriptors, maxDistance, opts) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_float(maxDistance) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks, :stream])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatchAsync(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec radiusMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def radiusMatchAsync(self, queryDescriptors, trainDescriptors, maxDistance) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_float(maxDistance)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatchAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec radiusMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t(), number()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def radiusMatchAsync(self, queryDescriptors, trainDescriptors, maxDistance) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_struct(trainDescriptors, Evision.CUDA.GpuMat) and is_float(maxDistance)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatchAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  radiusMatchAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`
  - **maxDistance**: `float`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **matches**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  radiusMatchAsync(queryDescriptors, maxDistance[, matches[, masks[, stream]]]) -> matches
  ```
  #### Variant 2:
  radiusMatchAsync

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **queryDescriptors**: `Evision.CUDA.GpuMat.t()`
  - **maxDistance**: `float`

  ##### Keyword Arguments
  - **masks**: `[Evision.CUDA.GpuMat]`.
  - **stream**: `Evision.CUDA.Stream.t()`.

  ##### Return
  - **matches**: `Evision.CUDA.GpuMat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  radiusMatchAsync(queryDescriptors, maxDistance[, matches[, masks[, stream]]]) -> matches
  ```

  """
  @spec radiusMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def radiusMatchAsync(self, queryDescriptors, maxDistance) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_float(maxDistance)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatchAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec radiusMatchAsync(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), number()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def radiusMatchAsync(self, queryDescriptors, maxDistance) when is_struct(queryDescriptors, Evision.CUDA.GpuMat) and is_float(maxDistance)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatchAsync(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Converts matches array from internal representation to standard matches vector.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **gpu_matches**: `Evision.Mat`.

    Matches, returned from DescriptorMatcher::radiusMatchAsync.

  ##### Keyword Arguments
  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Vector of DMatch objects.

  The method is supposed to be used with DescriptorMatcher::radiusMatchAsync to get final result.
  Call this method only after DescriptorMatcher::radiusMatchAsync is completed (ie. after synchronization).

  Python prototype (for reference only):
  ```python3
  radiusMatchConvert(gpu_matches[, compactResult]) -> matches
  ```
  #### Variant 2:
  Converts matches array from internal representation to standard matches vector.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **gpu_matches**: `Evision.CUDA.GpuMat.t()`.

    Matches, returned from DescriptorMatcher::radiusMatchAsync.

  ##### Keyword Arguments
  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Vector of DMatch objects.

  The method is supposed to be used with DescriptorMatcher::radiusMatchAsync to get final result.
  Call this method only after DescriptorMatcher::radiusMatchAsync is completed (ie. after synchronization).

  Python prototype (for reference only):
  ```python3
  radiusMatchConvert(gpu_matches[, compactResult]) -> matches
  ```

  """
  @spec radiusMatchConvert(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in(), [{:compactResult, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatchConvert(self, gpu_matches, opts) when (is_struct(gpu_matches, Evision.Mat) or is_struct(gpu_matches, Nx.Tensor) or is_number(gpu_matches) or is_tuple(gpu_matches)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult])
    positional = [
      gpu_matches: Evision.Internal.Structurise.from_struct(gpu_matches)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatchConvert(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec radiusMatchConvert(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t(), [{:compactResult, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatchConvert(self, gpu_matches, opts) when is_struct(gpu_matches, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult])
    positional = [
      gpu_matches: Evision.Internal.Structurise.from_struct(gpu_matches)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatchConvert(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Converts matches array from internal representation to standard matches vector.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **gpu_matches**: `Evision.Mat`.

    Matches, returned from DescriptorMatcher::radiusMatchAsync.

  ##### Keyword Arguments
  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Vector of DMatch objects.

  The method is supposed to be used with DescriptorMatcher::radiusMatchAsync to get final result.
  Call this method only after DescriptorMatcher::radiusMatchAsync is completed (ie. after synchronization).

  Python prototype (for reference only):
  ```python3
  radiusMatchConvert(gpu_matches[, compactResult]) -> matches
  ```
  #### Variant 2:
  Converts matches array from internal representation to standard matches vector.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **gpu_matches**: `Evision.CUDA.GpuMat.t()`.

    Matches, returned from DescriptorMatcher::radiusMatchAsync.

  ##### Keyword Arguments
  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Vector of DMatch objects.

  The method is supposed to be used with DescriptorMatcher::radiusMatchAsync to get final result.
  Call this method only after DescriptorMatcher::radiusMatchAsync is completed (ie. after synchronization).

  Python prototype (for reference only):
  ```python3
  radiusMatchConvert(gpu_matches[, compactResult]) -> matches
  ```

  """
  @spec radiusMatchConvert(Evision.CUDA.DescriptorMatcher.t(), Evision.Mat.maybe_mat_in()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatchConvert(self, gpu_matches) when (is_struct(gpu_matches, Evision.Mat) or is_struct(gpu_matches, Nx.Tensor) or is_number(gpu_matches) or is_tuple(gpu_matches))
  do
    positional = [
      gpu_matches: Evision.Internal.Structurise.from_struct(gpu_matches)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatchConvert(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec radiusMatchConvert(Evision.CUDA.DescriptorMatcher.t(), Evision.CUDA.GpuMat.t()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatchConvert(self, gpu_matches) when is_struct(gpu_matches, Evision.CUDA.GpuMat)
  do
    positional = [
      gpu_matches: Evision.Internal.Structurise.from_struct(gpu_matches)
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_radiusMatchConvert(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CUDA.DescriptorMatcher.t(), Evision.FileNode.t()) :: Evision.CUDA.DescriptorMatcher.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.cuda_DescriptorMatcher_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CUDA.DescriptorMatcher.t(), binary()) :: Evision.CUDA.DescriptorMatcher.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cuda_DescriptorMatcher_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains a descriptor matcher.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`

  Trains a descriptor matcher (for example, the flann index). In all methods to match, the method
  train() is run every time before matching.

  Python prototype (for reference only):
  ```python3
  train() -> None
  ```
  """
  @spec train(Evision.CUDA.DescriptorMatcher.t()) :: Evision.CUDA.DescriptorMatcher.t() | {:error, String.t()}
  def train(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_DescriptorMatcher_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CUDA.DescriptorMatcher.t(), Evision.FileStorage.t(), binary()) :: Evision.CUDA.DescriptorMatcher.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.cuda_DescriptorMatcher_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.DescriptorMatcher.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CUDA.DescriptorMatcher.t(), Evision.FileStorage.t()) :: Evision.CUDA.DescriptorMatcher.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.cuda_DescriptorMatcher_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
