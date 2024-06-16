defmodule Evision.Utils do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Utils` struct.

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
  def to_struct({:ok, %{class: Evision.Utils, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Utils, ref: ref}) do
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
  copyMatAndDumpNamedArguments

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **params**: `FunctionParams`.

  ##### Return
  - **retval**: `String`
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  copyMatAndDumpNamedArguments(src[, dst[, params]]) -> retval, dst
  ```
  """
  @spec copyMatAndDumpNamedArguments(Evision.Mat.maybe_mat_in(), [{:params, term()}] | nil) :: {binary(), Evision.Mat.t()} | {:error, String.t()}
  def copyMatAndDumpNamedArguments(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:params])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.utils_copyMatAndDumpNamedArguments(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  copyMatAndDumpNamedArguments

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **params**: `FunctionParams`.

  ##### Return
  - **retval**: `String`
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  copyMatAndDumpNamedArguments(src[, dst[, params]]) -> retval, dst
  ```
  """
  @spec copyMatAndDumpNamedArguments(Evision.Mat.maybe_mat_in()) :: {binary(), Evision.Mat.t()} | {:error, String.t()}
  def copyMatAndDumpNamedArguments(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.utils_copyMatAndDumpNamedArguments(positional)
    |> to_struct()
  end

  @doc """
  dumpBool

  ##### Positional Arguments
  - **argument**: `bool`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpBool(argument) -> retval
  ```
  """
  @spec dumpBool(boolean()) :: binary() | {:error, String.t()}
  def dumpBool(argument) when is_boolean(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpBool(positional)
    |> to_struct()
  end

  @doc """
  dumpCString

  ##### Positional Arguments
  - **argument**: `c_string`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpCString(argument) -> retval
  ```
  """
  @spec dumpCString(binary()) :: binary() | {:error, String.t()}
  def dumpCString(argument) when is_binary(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpCString(positional)
    |> to_struct()
  end

  @doc """
  dumpDouble

  ##### Positional Arguments
  - **argument**: `double`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpDouble(argument) -> retval
  ```
  """
  @spec dumpDouble(number()) :: binary() | {:error, String.t()}
  def dumpDouble(argument) when is_number(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpDouble(positional)
    |> to_struct()
  end

  @doc """
  dumpFloat

  ##### Positional Arguments
  - **argument**: `float`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpFloat(argument) -> retval
  ```
  """
  @spec dumpFloat(number()) :: binary() | {:error, String.t()}
  def dumpFloat(argument) when is_float(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpFloat(positional)
    |> to_struct()
  end

  @doc """
  dumpInputArray

  ##### Positional Arguments
  - **argument**: `Evision.Mat`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpInputArray(argument) -> retval
  ```
  """
  @spec dumpInputArray(Evision.Mat.maybe_mat_in()) :: binary() | {:error, String.t()}
  def dumpInputArray(argument) when (is_struct(argument, Evision.Mat) or is_struct(argument, Nx.Tensor) or is_number(argument) or is_tuple(argument))
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpInputArray(positional)
    |> to_struct()
  end

  @doc """
  dumpInputArrayOfArrays

  ##### Positional Arguments
  - **argument**: `[Evision.Mat]`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpInputArrayOfArrays(argument) -> retval
  ```
  """
  @spec dumpInputArrayOfArrays(list(Evision.Mat.maybe_mat_in())) :: binary() | {:error, String.t()}
  def dumpInputArrayOfArrays(argument) when is_list(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpInputArrayOfArrays(positional)
    |> to_struct()
  end

  @doc """
  dumpInputOutputArray
  ##### Return
  - **retval**: `String`
  - **argument**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  dumpInputOutputArray(argument) -> retval, argument
  ```
  """
  @spec dumpInputOutputArray(Evision.Mat.maybe_mat_in()) :: {binary(), Evision.Mat.t()} | {:error, String.t()}
  def dumpInputOutputArray(argument) when (is_struct(argument, Evision.Mat) or is_struct(argument, Nx.Tensor) or is_number(argument) or is_tuple(argument))
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpInputOutputArray(positional)
    |> to_struct()
  end

  @doc """
  dumpInputOutputArrayOfArrays
  ##### Return
  - **retval**: `String`
  - **argument**: `[Evision.Mat]`

  Python prototype (for reference only):
  ```python3
  dumpInputOutputArrayOfArrays(argument) -> retval, argument
  ```
  """
  @spec dumpInputOutputArrayOfArrays(list(Evision.Mat.maybe_mat_in())) :: {binary(), list(Evision.Mat.t())} | {:error, String.t()}
  def dumpInputOutputArrayOfArrays(argument) when is_list(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpInputOutputArrayOfArrays(positional)
    |> to_struct()
  end

  @doc """
  dumpInt

  ##### Positional Arguments
  - **argument**: `integer()`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpInt(argument) -> retval
  ```
  """
  @spec dumpInt(integer()) :: binary() | {:error, String.t()}
  def dumpInt(argument) when is_integer(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpInt(positional)
    |> to_struct()
  end

  @doc """
  dumpInt64

  ##### Positional Arguments
  - **argument**: `int64`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpInt64(argument) -> retval
  ```
  """
  @spec dumpInt64(integer()) :: binary() | {:error, String.t()}
  def dumpInt64(argument) when is_integer(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpInt64(positional)
    |> to_struct()
  end

  @doc """
  dumpRange

  ##### Positional Arguments
  - **argument**: `Range`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpRange(argument) -> retval
  ```
  """
  @spec dumpRange({integer(), integer()} | :all) :: binary() | {:error, String.t()}
  def dumpRange(argument) when (is_tuple(argument) or argument == :all)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpRange(positional)
    |> to_struct()
  end

  @doc """
  dumpRect

  ##### Positional Arguments
  - **argument**: `Rect`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpRect(argument) -> retval
  ```
  """
  @spec dumpRect({number(), number(), number(), number()}) :: binary() | {:error, String.t()}
  def dumpRect(argument) when is_tuple(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpRect(positional)
    |> to_struct()
  end

  @doc """
  dumpRotatedRect

  ##### Positional Arguments
  - **argument**: `{centre={x, y}, size={s1, s2}, angle}`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpRotatedRect(argument) -> retval
  ```
  """
  @spec dumpRotatedRect({{number(), number()}, {number(), number()}, number()}) :: binary() | {:error, String.t()}
  def dumpRotatedRect(argument) when is_tuple(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpRotatedRect(positional)
    |> to_struct()
  end

  @doc """
  dumpSizeT

  ##### Positional Arguments
  - **argument**: `size_t`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpSizeT(argument) -> retval
  ```
  """
  @spec dumpSizeT(integer()) :: binary() | {:error, String.t()}
  def dumpSizeT(argument) when is_integer(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpSizeT(positional)
    |> to_struct()
  end

  @doc """
  dumpString

  ##### Positional Arguments
  - **argument**: `String`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpString(argument) -> retval
  ```
  """
  @spec dumpString(binary()) :: binary() | {:error, String.t()}
  def dumpString(argument) when is_binary(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpString(positional)
    |> to_struct()
  end

  @doc """
  dumpTermCriteria

  ##### Positional Arguments
  - **argument**: `TermCriteria`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpTermCriteria(argument) -> retval
  ```
  """
  @spec dumpTermCriteria({integer(), integer(), number()}) :: binary() | {:error, String.t()}
  def dumpTermCriteria(argument) when is_tuple(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_dumpTermCriteria(positional)
    |> to_struct()
  end

  @doc """
  dumpVec2i
  ##### Keyword Arguments
  - **value**: `Vec2i`.

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpVec2i([, value]) -> retval
  ```
  """
  @spec dumpVec2i([{:value, term()}] | nil) :: binary() | {:error, String.t()}
  def dumpVec2i(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:value])
    positional = [
    ]
    :evision_nif.utils_dumpVec2i(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  dumpVec2i
  ##### Keyword Arguments
  - **value**: `Vec2i`.

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpVec2i([, value]) -> retval
  ```
  """
  @spec dumpVec2i() :: binary() | {:error, String.t()}
  def dumpVec2i() do
    positional = [
    ]
    :evision_nif.utils_dumpVec2i(positional)
    |> to_struct()
  end

  @doc """
  dumpVectorOfDouble

  ##### Positional Arguments
  - **vec**: `[double]`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpVectorOfDouble(vec) -> retval
  ```
  """
  @spec dumpVectorOfDouble(list(number())) :: binary() | {:error, String.t()}
  def dumpVectorOfDouble(vec) when is_list(vec)
  do
    positional = [
      vec: Evision.Internal.Structurise.from_struct(vec)
    ]
    :evision_nif.utils_dumpVectorOfDouble(positional)
    |> to_struct()
  end

  @doc """
  dumpVectorOfInt

  ##### Positional Arguments
  - **vec**: `[integer()]`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpVectorOfInt(vec) -> retval
  ```
  """
  @spec dumpVectorOfInt(list(integer())) :: binary() | {:error, String.t()}
  def dumpVectorOfInt(vec) when is_list(vec)
  do
    positional = [
      vec: Evision.Internal.Structurise.from_struct(vec)
    ]
    :evision_nif.utils_dumpVectorOfInt(positional)
    |> to_struct()
  end

  @doc """
  dumpVectorOfRect

  ##### Positional Arguments
  - **vec**: `[Rect]`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  dumpVectorOfRect(vec) -> retval
  ```
  """
  @spec dumpVectorOfRect(list({number(), number(), number(), number()})) :: binary() | {:error, String.t()}
  def dumpVectorOfRect(vec) when is_list(vec)
  do
    positional = [
      vec: Evision.Internal.Structurise.from_struct(vec)
    ]
    :evision_nif.utils_dumpVectorOfRect(positional)
    |> to_struct()
  end

  @doc """
  generateVectorOfInt

  ##### Positional Arguments
  - **len**: `size_t`

  ##### Return
  - **vec**: `[integer()]`

  Python prototype (for reference only):
  ```python3
  generateVectorOfInt(len) -> vec
  ```
  """
  @spec generateVectorOfInt(integer()) :: list(integer()) | {:error, String.t()}
  def generateVectorOfInt(len) when is_integer(len)
  do
    positional = [
      len: Evision.Internal.Structurise.from_struct(len)
    ]
    :evision_nif.utils_generateVectorOfInt(positional)
    |> to_struct()
  end

  @doc """
  generateVectorOfMat

  ##### Positional Arguments
  - **len**: `size_t`
  - **rows**: `integer()`
  - **cols**: `integer()`
  - **dtype**: `integer()`

  ##### Return
  - **vec**: `[Evision.Mat]`.

  Python prototype (for reference only):
  ```python3
  generateVectorOfMat(len, rows, cols, dtype[, vec]) -> vec
  ```
  """
  @spec generateVectorOfMat(integer(), integer(), integer(), integer(), [{atom(), term()},...] | nil) :: list(Evision.Mat.t()) | {:error, String.t()}
  def generateVectorOfMat(len, rows, cols, dtype, opts) when is_integer(len) and is_integer(rows) and is_integer(cols) and is_integer(dtype) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      len: Evision.Internal.Structurise.from_struct(len),
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      dtype: Evision.Internal.Structurise.from_struct(dtype)
    ]
    :evision_nif.utils_generateVectorOfMat(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  generateVectorOfMat

  ##### Positional Arguments
  - **len**: `size_t`
  - **rows**: `integer()`
  - **cols**: `integer()`
  - **dtype**: `integer()`

  ##### Return
  - **vec**: `[Evision.Mat]`.

  Python prototype (for reference only):
  ```python3
  generateVectorOfMat(len, rows, cols, dtype[, vec]) -> vec
  ```
  """
  @spec generateVectorOfMat(integer(), integer(), integer(), integer()) :: list(Evision.Mat.t()) | {:error, String.t()}
  def generateVectorOfMat(len, rows, cols, dtype) when is_integer(len) and is_integer(rows) and is_integer(cols) and is_integer(dtype)
  do
    positional = [
      len: Evision.Internal.Structurise.from_struct(len),
      rows: Evision.Internal.Structurise.from_struct(rows),
      cols: Evision.Internal.Structurise.from_struct(cols),
      dtype: Evision.Internal.Structurise.from_struct(dtype)
    ]
    :evision_nif.utils_generateVectorOfMat(positional)
    |> to_struct()
  end

  @doc """
  generateVectorOfRect

  ##### Positional Arguments
  - **len**: `size_t`

  ##### Return
  - **vec**: `[Rect]`

  Python prototype (for reference only):
  ```python3
  generateVectorOfRect(len) -> vec
  ```
  """
  @spec generateVectorOfRect(integer()) :: list({number(), number(), number(), number()}) | {:error, String.t()}
  def generateVectorOfRect(len) when is_integer(len)
  do
    positional = [
      len: Evision.Internal.Structurise.from_struct(len)
    ]
    :evision_nif.utils_generateVectorOfRect(positional)
    |> to_struct()
  end

  @doc """
  testAsyncArray

  ##### Positional Arguments
  - **argument**: `Evision.Mat`

  ##### Return
  - **retval**: `Evision.AsyncArray.t()`

  Python prototype (for reference only):
  ```python3
  testAsyncArray(argument) -> retval
  ```
  """
  @spec testAsyncArray(Evision.Mat.maybe_mat_in()) :: Evision.AsyncArray.t() | {:error, String.t()}
  def testAsyncArray(argument) when (is_struct(argument, Evision.Mat) or is_struct(argument, Nx.Tensor) or is_number(argument) or is_tuple(argument))
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_testAsyncArray(positional)
    |> to_struct()
  end

  @doc """
  testAsyncException
  ##### Return
  - **retval**: `Evision.AsyncArray.t()`

  Python prototype (for reference only):
  ```python3
  testAsyncException() -> retval
  ```
  """
  @spec testAsyncException() :: Evision.AsyncArray.t() | {:error, String.t()}
  def testAsyncException() do
    positional = [
    ]
    :evision_nif.utils_testAsyncException(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  testOverloadResolution

  ##### Positional Arguments
  - **rect**: `Rect`

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  testOverloadResolution(rect) -> retval
  ```
  #### Variant 2:
  testOverloadResolution

  ##### Positional Arguments
  - **value**: `integer()`

  ##### Keyword Arguments
  - **point**: `Point`.

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  testOverloadResolution(value[, point]) -> retval
  ```

  """
  @spec testOverloadResolution({number(), number(), number(), number()}) :: binary() | {:error, String.t()}
  def testOverloadResolution(rect) when is_tuple(rect)
  do
    positional = [
      rect: Evision.Internal.Structurise.from_struct(rect)
    ]
    :evision_nif.utils_testOverloadResolution(positional)
    |> to_struct()
  end
  @spec testOverloadResolution(integer()) :: binary() | {:error, String.t()}
  def testOverloadResolution(value) when is_integer(value)
  do
    positional = [
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.utils_testOverloadResolution(positional)
    |> to_struct()
  end

  @doc """
  testOverloadResolution

  ##### Positional Arguments
  - **value**: `integer()`

  ##### Keyword Arguments
  - **point**: `Point`.

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  testOverloadResolution(value[, point]) -> retval
  ```
  """
  @spec testOverloadResolution(integer(), [{:point, term()}] | nil) :: binary() | {:error, String.t()}
  def testOverloadResolution(value, opts) when is_integer(value) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:point])
    positional = [
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.utils_testOverloadResolution(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  testOverwriteNativeMethod

  ##### Positional Arguments
  - **argument**: `integer()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  testOverwriteNativeMethod(argument) -> retval
  ```
  """
  @spec testOverwriteNativeMethod(integer()) :: integer() | {:error, String.t()}
  def testOverwriteNativeMethod(argument) when is_integer(argument)
  do
    positional = [
      argument: Evision.Internal.Structurise.from_struct(argument)
    ]
    :evision_nif.utils_testOverwriteNativeMethod(positional)
    |> to_struct()
  end

  @doc """
  testRaiseGeneralException

  Python prototype (for reference only):
  ```python3
  testRaiseGeneralException() -> None
  ```
  """
  @spec testRaiseGeneralException() :: :ok | {:error, String.t()}
  def testRaiseGeneralException() do
    positional = [
    ]
    :evision_nif.utils_testRaiseGeneralException(positional)
    |> to_struct()
  end

  @doc """
  testReservedKeywordConversion

  ##### Positional Arguments
  - **positional_argument**: `integer()`

  ##### Keyword Arguments
  - **lambda**: `integer()`.
  - **from**: `integer()`.

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  testReservedKeywordConversion(positional_argument[, lambda[, from]]) -> retval
  ```
  """
  @spec testReservedKeywordConversion(integer(), [{:from, term()} | {:lambda, term()}] | nil) :: binary() | {:error, String.t()}
  def testReservedKeywordConversion(positional_argument, opts) when is_integer(positional_argument) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:from, :lambda])
    positional = [
      positional_argument: Evision.Internal.Structurise.from_struct(positional_argument)
    ]
    :evision_nif.utils_testReservedKeywordConversion(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  testReservedKeywordConversion

  ##### Positional Arguments
  - **positional_argument**: `integer()`

  ##### Keyword Arguments
  - **lambda**: `integer()`.
  - **from**: `integer()`.

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  testReservedKeywordConversion(positional_argument[, lambda[, from]]) -> retval
  ```
  """
  @spec testReservedKeywordConversion(integer()) :: binary() | {:error, String.t()}
  def testReservedKeywordConversion(positional_argument) when is_integer(positional_argument)
  do
    positional = [
      positional_argument: Evision.Internal.Structurise.from_struct(positional_argument)
    ]
    :evision_nif.utils_testReservedKeywordConversion(positional)
    |> to_struct()
  end

  @doc """
  testRotatedRect

  ##### Positional Arguments
  - **x**: `float`
  - **y**: `float`
  - **w**: `float`
  - **h**: `float`
  - **angle**: `float`

  ##### Return
  - **retval**: `{centre={x, y}, size={s1, s2}, angle}`

  Python prototype (for reference only):
  ```python3
  testRotatedRect(x, y, w, h, angle) -> retval
  ```
  """
  @spec testRotatedRect(number(), number(), number(), number(), number()) :: {{number(), number()}, {number(), number()}, number()} | {:error, String.t()}
  def testRotatedRect(x, y, w, h, angle) when is_float(x) and is_float(y) and is_float(w) and is_float(h) and is_float(angle)
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y),
      w: Evision.Internal.Structurise.from_struct(w),
      h: Evision.Internal.Structurise.from_struct(h),
      angle: Evision.Internal.Structurise.from_struct(angle)
    ]
    :evision_nif.utils_testRotatedRect(positional)
    |> to_struct()
  end

  @doc """
  testRotatedRectVector

  ##### Positional Arguments
  - **x**: `float`
  - **y**: `float`
  - **w**: `float`
  - **h**: `float`
  - **angle**: `float`

  ##### Return
  - **retval**: `[{centre={x, y}, size={s1, s2}, angle}]`

  Python prototype (for reference only):
  ```python3
  testRotatedRectVector(x, y, w, h, angle) -> retval
  ```
  """
  @spec testRotatedRectVector(number(), number(), number(), number(), number()) :: list({{number(), number()}, {number(), number()}, number()}) | {:error, String.t()}
  def testRotatedRectVector(x, y, w, h, angle) when is_float(x) and is_float(y) and is_float(w) and is_float(h) and is_float(angle)
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y),
      w: Evision.Internal.Structurise.from_struct(w),
      h: Evision.Internal.Structurise.from_struct(h),
      angle: Evision.Internal.Structurise.from_struct(angle)
    ]
    :evision_nif.utils_testRotatedRectVector(positional)
    |> to_struct()
  end
end
