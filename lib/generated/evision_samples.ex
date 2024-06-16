defmodule Evision.Samples do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Samples` struct.

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
  def to_struct({:ok, %{class: Evision.Samples, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Samples, ref: ref}) do
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
  Override search data path by adding new search location

  ##### Positional Arguments
  - **path**: `String`.

    Path to used samples data

  Use this only to override default behavior
  Passed paths are used in LIFO order.

  Python prototype (for reference only):
  ```python3
  addSamplesDataSearchPath(path) -> None
  ```
  """
  @spec addSamplesDataSearchPath(binary()) :: :ok | {:error, String.t()}
  def addSamplesDataSearchPath(path) when is_binary(path)
  do
    positional = [
      path: Evision.Internal.Structurise.from_struct(path)
    ]
    :evision_nif.samples_addSamplesDataSearchPath(positional)
    |> to_struct()
  end

  @doc """
  Append samples search data sub directory

  ##### Positional Arguments
  - **subdir**: `String`.

    samples data sub directory

  General usage is to add OpenCV modules name (`<opencv_contrib>/modules/<name>/samples/data` -> `<name>/samples/data` + `modules/<name>/samples/data`).
  Passed subdirectories are used in LIFO order.

  Python prototype (for reference only):
  ```python3
  addSamplesDataSearchSubDirectory(subdir) -> None
  ```
  """
  @spec addSamplesDataSearchSubDirectory(binary()) :: :ok | {:error, String.t()}
  def addSamplesDataSearchSubDirectory(subdir) when is_binary(subdir)
  do
    positional = [
      subdir: Evision.Internal.Structurise.from_struct(subdir)
    ]
    :evision_nif.samples_addSamplesDataSearchSubDirectory(positional)
    |> to_struct()
  end

  @doc """
  Try to find requested data file

  ##### Positional Arguments
  - **relative_path**: `String`.

    Relative path to data file

  ##### Keyword Arguments
  - **required**: `bool`.

    Specify "file not found" handling.
    If true, function prints information message and raises cv::Exception.
    If false, function returns empty result

  - **silentMode**: `bool`.

    Disables messages

  ##### Return
  - **retval**: `String`

  Search directories:
  1. Directories passed via `addSamplesDataSearchPath()`
  2. OPENCV_SAMPLES_DATA_PATH_HINT environment variable
  3. OPENCV_SAMPLES_DATA_PATH environment variable
  If parameter value is not empty and nothing is found then stop searching.
  4. Detects build/install path based on:
  a. current working directory (CWD)
  b. and/or binary module location (opencv_core/opencv_world, doesn't work with static linkage)
  5. Scan `<source>/{,data,samples/data}` directories if build directory is detected or the current directory is in source tree.
  6. Scan `<install>/share/OpenCV` directory if install directory is detected.
  @see cv::utils::findDataFile
  @return Returns path (absolute or relative to the current directory) or empty string if file is not found

  Python prototype (for reference only):
  ```python3
  findFile(relative_path[, required[, silentMode]]) -> retval
  ```
  """
  @spec findFile(binary(), [{:required, term()} | {:silentMode, term()}] | nil) :: binary() | {:error, String.t()}
  def findFile(relative_path, opts) when is_binary(relative_path) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:required, :silentMode])
    positional = [
      relative_path: Evision.Internal.Structurise.from_struct(relative_path)
    ]
    :evision_nif.samples_findFile(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Try to find requested data file

  ##### Positional Arguments
  - **relative_path**: `String`.

    Relative path to data file

  ##### Keyword Arguments
  - **required**: `bool`.

    Specify "file not found" handling.
    If true, function prints information message and raises cv::Exception.
    If false, function returns empty result

  - **silentMode**: `bool`.

    Disables messages

  ##### Return
  - **retval**: `String`

  Search directories:
  1. Directories passed via `addSamplesDataSearchPath()`
  2. OPENCV_SAMPLES_DATA_PATH_HINT environment variable
  3. OPENCV_SAMPLES_DATA_PATH environment variable
  If parameter value is not empty and nothing is found then stop searching.
  4. Detects build/install path based on:
  a. current working directory (CWD)
  b. and/or binary module location (opencv_core/opencv_world, doesn't work with static linkage)
  5. Scan `<source>/{,data,samples/data}` directories if build directory is detected or the current directory is in source tree.
  6. Scan `<install>/share/OpenCV` directory if install directory is detected.
  @see cv::utils::findDataFile
  @return Returns path (absolute or relative to the current directory) or empty string if file is not found

  Python prototype (for reference only):
  ```python3
  findFile(relative_path[, required[, silentMode]]) -> retval
  ```
  """
  @spec findFile(binary()) :: binary() | {:error, String.t()}
  def findFile(relative_path) when is_binary(relative_path)
  do
    positional = [
      relative_path: Evision.Internal.Structurise.from_struct(relative_path)
    ]
    :evision_nif.samples_findFile(positional)
    |> to_struct()
  end

  @doc """
  findFileOrKeep

  ##### Positional Arguments
  - **relative_path**: `String`

  ##### Keyword Arguments
  - **silentMode**: `bool`.

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  findFileOrKeep(relative_path[, silentMode]) -> retval
  ```
  """
  @spec findFileOrKeep(binary(), [{:silentMode, term()}] | nil) :: binary() | {:error, String.t()}
  def findFileOrKeep(relative_path, opts) when is_binary(relative_path) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:silentMode])
    positional = [
      relative_path: Evision.Internal.Structurise.from_struct(relative_path)
    ]
    :evision_nif.samples_findFileOrKeep(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  findFileOrKeep

  ##### Positional Arguments
  - **relative_path**: `String`

  ##### Keyword Arguments
  - **silentMode**: `bool`.

  ##### Return
  - **retval**: `String`

  Python prototype (for reference only):
  ```python3
  findFileOrKeep(relative_path[, silentMode]) -> retval
  ```
  """
  @spec findFileOrKeep(binary()) :: binary() | {:error, String.t()}
  def findFileOrKeep(relative_path) when is_binary(relative_path)
  do
    positional = [
      relative_path: Evision.Internal.Structurise.from_struct(relative_path)
    ]
    :evision_nif.samples_findFileOrKeep(positional)
    |> to_struct()
  end
end
