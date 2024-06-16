defmodule Evision.BFMatcher do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `BFMatcher` struct.

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
  def to_struct({:ok, %{class: Evision.BFMatcher, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.BFMatcher, ref: ref}) do
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
  Brute-force matcher constructor (obsolete). Please use BFMatcher.create()
  ##### Keyword Arguments
  - **normType**: `integer()`.
  - **crossCheck**: `bool`.

  ##### Return
  - **self**: `Evision.BFMatcher.t()`

  Python prototype (for reference only):
  ```python3
  BFMatcher([, normType[, crossCheck]]) -> <BFMatcher object>
  ```
  """
  @spec bfMatcher([{:crossCheck, term()} | {:normType, term()}] | nil) :: Evision.BFMatcher.t() | {:error, String.t()}
  def bfMatcher(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:crossCheck, :normType])
    positional = [
    ]
    :evision_nif.bfMatcher_BFMatcher(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Brute-force matcher constructor (obsolete). Please use BFMatcher.create()
  ##### Keyword Arguments
  - **normType**: `integer()`.
  - **crossCheck**: `bool`.

  ##### Return
  - **self**: `Evision.BFMatcher.t()`

  Python prototype (for reference only):
  ```python3
  BFMatcher([, normType[, crossCheck]]) -> <BFMatcher object>
  ```
  """
  @spec bfMatcher() :: Evision.BFMatcher.t() | {:error, String.t()}
  def bfMatcher() do
    positional = [
    ]
    :evision_nif.bfMatcher_BFMatcher(positional)
    |> to_struct()
  end

  @doc """
  Adds descriptors to train a CPU(trainDescCollectionis) or GPU(utrainDescCollectionis) descriptor
  collection.

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
  - **descriptors**: `[Evision.Mat]`.

    Descriptors to add. Each descriptors[i] is a set of descriptors from the same
    train image.

  If the collection is not empty, the new descriptors are added to existing train descriptors.

  Python prototype (for reference only):
  ```python3
  add(descriptors) -> None
  ```
  """
  @spec add(Evision.BFMatcher.t(), list(Evision.Mat.maybe_mat_in())) :: Evision.BFMatcher.t() | {:error, String.t()}
  def add(self, descriptors) when is_list(descriptors)
  do
    positional = [
      descriptors: Evision.Internal.Structurise.from_struct(descriptors)
    ]
    :evision_nif.bfMatcher_add(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clears the train descriptor collections.

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.BFMatcher.t()) :: Evision.BFMatcher.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.bfMatcher_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Clones the matcher.

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`

  ##### Keyword Arguments
  - **emptyTrainData**: `bool`.

    If emptyTrainData is false, the method creates a deep copy of the object,
    that is, copies both parameters and train data. If emptyTrainData is true, the method creates an
    object copy with the current parameters but with empty train data.

  ##### Return
  - **retval**: `Evision.DescriptorMatcher.t()`

  Python prototype (for reference only):
  ```python3
  clone([, emptyTrainData]) -> retval
  ```
  """
  @spec clone(Evision.BFMatcher.t(), [{:emptyTrainData, term()}] | nil) :: Evision.DescriptorMatcher.t() | {:error, String.t()}
  def clone(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:emptyTrainData])
    positional = [
    ]
    :evision_nif.bfMatcher_clone(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Clones the matcher.

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`

  ##### Keyword Arguments
  - **emptyTrainData**: `bool`.

    If emptyTrainData is false, the method creates a deep copy of the object,
    that is, copies both parameters and train data. If emptyTrainData is true, the method creates an
    object copy with the current parameters but with empty train data.

  ##### Return
  - **retval**: `Evision.DescriptorMatcher.t()`

  Python prototype (for reference only):
  ```python3
  clone([, emptyTrainData]) -> retval
  ```
  """
  @spec clone(Evision.BFMatcher.t()) :: Evision.DescriptorMatcher.t() | {:error, String.t()}
  def clone(self) do
    positional = [
    ]
    :evision_nif.bfMatcher_clone(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Brute-force matcher create method.
  ##### Keyword Arguments
  - **normType**: `integer()`.

    One of NORM_L1, NORM_L2, NORM_HAMMING, NORM_HAMMING2. L1 and L2 norms are
    preferable choices for SIFT and SURF descriptors, NORM_HAMMING should be used with ORB, BRISK and
    BRIEF, NORM_HAMMING2 should be used with ORB when WTA_K==3 or 4 (see ORB::ORB constructor
    description).

  - **crossCheck**: `bool`.

    If it is false, this is will be default BFMatcher behaviour when it finds the k
    nearest neighbors for each query descriptor. If crossCheck==true, then the knnMatch() method with
    k=1 will only return pairs (i,j) such that for i-th query descriptor the j-th descriptor in the
    matcher's collection is the nearest and vice versa, i.e. the BFMatcher will only return consistent
    pairs. Such technique usually produces best results with minimal number of outliers when there are
    enough matches. This is alternative to the ratio test, used by D. Lowe in SIFT paper.

  ##### Return
  - **retval**: `Evision.BFMatcher.t()`

  Python prototype (for reference only):
  ```python3
  create([, normType[, crossCheck]]) -> retval
  ```
  """
  @spec create([{:crossCheck, term()} | {:normType, term()}] | nil) :: Evision.BFMatcher.t() | {:error, String.t()}
  def create(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:crossCheck, :normType])
    positional = [
    ]
    :evision_nif.bfMatcher_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Brute-force matcher create method.
  ##### Keyword Arguments
  - **normType**: `integer()`.

    One of NORM_L1, NORM_L2, NORM_HAMMING, NORM_HAMMING2. L1 and L2 norms are
    preferable choices for SIFT and SURF descriptors, NORM_HAMMING should be used with ORB, BRISK and
    BRIEF, NORM_HAMMING2 should be used with ORB when WTA_K==3 or 4 (see ORB::ORB constructor
    description).

  - **crossCheck**: `bool`.

    If it is false, this is will be default BFMatcher behaviour when it finds the k
    nearest neighbors for each query descriptor. If crossCheck==true, then the knnMatch() method with
    k=1 will only return pairs (i,j) such that for i-th query descriptor the j-th descriptor in the
    matcher's collection is the nearest and vice versa, i.e. the BFMatcher will only return consistent
    pairs. Such technique usually produces best results with minimal number of outliers when there are
    enough matches. This is alternative to the ratio test, used by D. Lowe in SIFT paper.

  ##### Return
  - **retval**: `Evision.BFMatcher.t()`

  Python prototype (for reference only):
  ```python3
  create([, normType[, crossCheck]]) -> retval
  ```
  """
  @spec create() :: Evision.BFMatcher.t() | {:error, String.t()}
  def create() do
    positional = [
    ]
    :evision_nif.bfMatcher_create_static(positional)
    |> to_struct()
  end

  @doc """
  Returns true if there are no train descriptors in the both collections.

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.BFMatcher.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.bfMatcher_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.BFMatcher.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.bfMatcher_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns a constant link to the train descriptor collection trainDescCollection .

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`

  ##### Return
  - **retval**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  getTrainDescriptors() -> retval
  ```
  """
  @spec getTrainDescriptors(Evision.BFMatcher.t()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def getTrainDescriptors(self) do
    positional = [
    ]
    :evision_nif.bfMatcher_getTrainDescriptors(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the descriptor matcher supports masking permissible matches.

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  isMaskSupported() -> retval
  ```
  """
  @spec isMaskSupported(Evision.BFMatcher.t()) :: boolean() | {:error, String.t()}
  def isMaskSupported(self) do
    positional = [
    ]
    :evision_nif.bfMatcher_isMaskSupported(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Finds the k best matches for each descriptor from a query set.

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
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
  """
  @spec knnMatch(Evision.BFMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{:compactResult, term()} | {:mask, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, trainDescriptors, k, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_integer(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :mask])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.bfMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds the k best matches for each descriptor from a query set.

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
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
  knnMatch

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **k**: `integer()`.

    Count of best matches found per each query descriptor or less if a query descriptor has
    less than k possible matches in total.

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    Set of masks. Each masks[i] specifies permissible matches between the input query
    descriptors and stored train descriptors from the i-th image trainDescCollection[i].

  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Matches. Each matches[i] is k or less matches for the same query descriptor.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  knnMatch(queryDescriptors, k[, masks[, compactResult]]) -> matches
  ```

  """
  @spec knnMatch(Evision.BFMatcher.t(), Evision.Mat.maybe_mat_in(), integer(), [{:compactResult, term()} | {:masks, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, k, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_integer(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :masks])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.bfMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec knnMatch(Evision.BFMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, trainDescriptors, k) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_integer(k)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.bfMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  knnMatch

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **k**: `integer()`.

    Count of best matches found per each query descriptor or less if a query descriptor has
    less than k possible matches in total.

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    Set of masks. Each masks[i] specifies permissible matches between the input query
    descriptors and stored train descriptors from the i-th image trainDescCollection[i].

  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Matches. Each matches[i] is k or less matches for the same query descriptor.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  knnMatch(queryDescriptors, k[, masks[, compactResult]]) -> matches
  ```
  """
  @spec knnMatch(Evision.BFMatcher.t(), Evision.Mat.maybe_mat_in(), integer()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def knnMatch(self, queryDescriptors, k) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_integer(k)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.bfMatcher_knnMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Finds the best match for each descriptor from a query set.

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
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
  """
  @spec match(Evision.BFMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors, trainDescriptors, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors)
    ]
    :evision_nif.bfMatcher_match(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Finds the best match for each descriptor from a query set.

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
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
  match

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    Set of masks. Each masks[i] specifies permissible matches between the input query
    descriptors and stored train descriptors from the i-th image trainDescCollection[i].

  ##### Return
  - **matches**: `[Evision.DMatch]`.

    Matches. If a query descriptor is masked out in mask , no match is added for this
    descriptor. So, matches size may be smaller than the query descriptors count.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  match(queryDescriptors[, masks]) -> matches
  ```

  """
  @spec match(Evision.BFMatcher.t(), Evision.Mat.maybe_mat_in(), [{:masks, term()}] | nil) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:masks])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors)
    ]
    :evision_nif.bfMatcher_match(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec match(Evision.BFMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors, trainDescriptors) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors))
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors)
    ]
    :evision_nif.bfMatcher_match(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  match

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    Set of masks. Each masks[i] specifies permissible matches between the input query
    descriptors and stored train descriptors from the i-th image trainDescCollection[i].

  ##### Return
  - **matches**: `[Evision.DMatch]`.

    Matches. If a query descriptor is masked out in mask , no match is added for this
    descriptor. So, matches size may be smaller than the query descriptors count.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  match(queryDescriptors[, masks]) -> matches
  ```
  """
  @spec match(Evision.BFMatcher.t(), Evision.Mat.maybe_mat_in()) :: list(Evision.DMatch.t()) | {:error, String.t()}
  def match(self, queryDescriptors) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors))
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors)
    ]
    :evision_nif.bfMatcher_match(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  For each query descriptor, finds the training descriptors not farther than the specified distance.

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
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
  """
  @spec radiusMatch(Evision.BFMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), [{:compactResult, term()} | {:mask, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatch(self, queryDescriptors, trainDescriptors, maxDistance, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_float(maxDistance) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :mask])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.bfMatcher_radiusMatch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  For each query descriptor, finds the training descriptors not farther than the specified distance.

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
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
  radiusMatch

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **maxDistance**: `float`.

    Threshold for the distance between matched descriptors. Distance means here
    metric distance (e.g. Hamming distance), not the distance between coordinates (which is measured
    in Pixels)!

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    Set of masks. Each masks[i] specifies permissible matches between the input query
    descriptors and stored train descriptors from the i-th image trainDescCollection[i].

  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Found matches.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  radiusMatch(queryDescriptors, maxDistance[, masks[, compactResult]]) -> matches
  ```

  """
  @spec radiusMatch(Evision.BFMatcher.t(), Evision.Mat.maybe_mat_in(), number(), [{:compactResult, term()} | {:masks, term()}] | nil) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatch(self, queryDescriptors, maxDistance, opts) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_float(maxDistance) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:compactResult, :masks])
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.bfMatcher_radiusMatch(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec radiusMatch(Evision.BFMatcher.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatch(self, queryDescriptors, trainDescriptors, maxDistance) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and (is_struct(trainDescriptors, Evision.Mat) or is_struct(trainDescriptors, Nx.Tensor) or is_number(trainDescriptors) or is_tuple(trainDescriptors)) and is_float(maxDistance)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      trainDescriptors: Evision.Internal.Structurise.from_struct(trainDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.bfMatcher_radiusMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  radiusMatch

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
  - **queryDescriptors**: `Evision.Mat`.

    Query set of descriptors.

  - **maxDistance**: `float`.

    Threshold for the distance between matched descriptors. Distance means here
    metric distance (e.g. Hamming distance), not the distance between coordinates (which is measured
    in Pixels)!

  ##### Keyword Arguments
  - **masks**: `[Evision.Mat]`.

    Set of masks. Each masks[i] specifies permissible matches between the input query
    descriptors and stored train descriptors from the i-th image trainDescCollection[i].

  - **compactResult**: `bool`.

    Parameter used when the mask (or masks) is not empty. If compactResult is
    false, the matches vector has the same size as queryDescriptors rows. If compactResult is true,
    the matches vector does not contain matches for fully masked-out query descriptors.

  ##### Return
  - **matches**: `[[Evision.DMatch]]`.

    Found matches.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  radiusMatch(queryDescriptors, maxDistance[, masks[, compactResult]]) -> matches
  ```
  """
  @spec radiusMatch(Evision.BFMatcher.t(), Evision.Mat.maybe_mat_in(), number()) :: list(list(Evision.DMatch.t())) | {:error, String.t()}
  def radiusMatch(self, queryDescriptors, maxDistance) when (is_struct(queryDescriptors, Evision.Mat) or is_struct(queryDescriptors, Nx.Tensor) or is_number(queryDescriptors) or is_tuple(queryDescriptors)) and is_float(maxDistance)
  do
    positional = [
      queryDescriptors: Evision.Internal.Structurise.from_struct(queryDescriptors),
      maxDistance: Evision.Internal.Structurise.from_struct(maxDistance)
    ]
    :evision_nif.bfMatcher_radiusMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  read

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
  - **arg1**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(arg1) -> None
  ```
  #### Variant 2:
  read

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  read(fileName) -> None
  ```

  """
  @spec read(Evision.BFMatcher.t(), Evision.FileNode.t()) :: Evision.BFMatcher.t() | {:error, String.t()}
  def read(self, arg1) when is_struct(arg1, Evision.FileNode)
  do
    positional = [
      arg1: Evision.Internal.Structurise.from_struct(arg1)
    ]
    :evision_nif.bfMatcher_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec read(Evision.BFMatcher.t(), binary()) :: Evision.BFMatcher.t() | {:error, String.t()}
  def read(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.bfMatcher_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.BFMatcher.t(), binary()) :: Evision.BFMatcher.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.bfMatcher_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Trains a descriptor matcher

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`

  Trains a descriptor matcher (for example, the flann index). In all methods to match, the method
  train() is run every time before matching. Some descriptor matchers (for example, BruteForceMatcher)
  have an empty implementation of this method. Other matchers really train their inner structures (for
  example, FlannBasedMatcher trains flann::Index ).

  Python prototype (for reference only):
  ```python3
  train() -> None
  ```
  """
  @spec train(Evision.BFMatcher.t()) :: Evision.BFMatcher.t() | {:error, String.t()}
  def train(self) do
    positional = [
    ]
    :evision_nif.bfMatcher_train(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.BFMatcher.t(), Evision.FileStorage.t(), binary()) :: Evision.BFMatcher.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.bfMatcher_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.BFMatcher.t()`
  - **fileName**: `String`

  Python prototype (for reference only):
  ```python3
  write(fileName) -> None
  ```
  """
  @spec write(Evision.BFMatcher.t(), binary()) :: Evision.BFMatcher.t() | {:error, String.t()}
  def write(self, fileName) when is_binary(fileName)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName)
    ]
    :evision_nif.bfMatcher_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
