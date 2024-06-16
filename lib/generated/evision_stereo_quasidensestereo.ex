defmodule Evision.Stereo.QuasiDenseStereo do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Stereo.QuasiDenseStereo` struct.

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
  def to_struct({:ok, %{class: Evision.Stereo.QuasiDenseStereo, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Stereo.QuasiDenseStereo, ref: ref}) do
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
  create

  ##### Positional Arguments
  - **monoImgSize**: `Size`

  ##### Keyword Arguments
  - **paramFilepath**: `String`.

  ##### Return
  - **retval**: `cv::Ptr<QuasiDenseStereo>`

  Python prototype (for reference only):
  ```python3
  create(monoImgSize[, paramFilepath]) -> retval
  ```
  """
  @spec create({number(), number()}, [{:paramFilepath, term()}] | nil) :: Evision.Stereo.QuasiDenseStereo.t() | {:error, String.t()}
  def create(monoImgSize, opts) when is_tuple(monoImgSize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:paramFilepath])
    positional = [
      monoImgSize: Evision.Internal.Structurise.from_struct(monoImgSize)
    ]
    :evision_nif.stereo_stereo_QuasiDenseStereo_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **monoImgSize**: `Size`

  ##### Keyword Arguments
  - **paramFilepath**: `String`.

  ##### Return
  - **retval**: `cv::Ptr<QuasiDenseStereo>`

  Python prototype (for reference only):
  ```python3
  create(monoImgSize[, paramFilepath]) -> retval
  ```
  """
  @spec create({number(), number()}) :: Evision.Stereo.QuasiDenseStereo.t() | {:error, String.t()}
  def create(monoImgSize) when is_tuple(monoImgSize)
  do
    positional = [
      monoImgSize: Evision.Internal.Structurise.from_struct(monoImgSize)
    ]
    :evision_nif.stereo_stereo_QuasiDenseStereo_create_static(positional)
    |> to_struct()
  end

  @doc """
  Get The dense corresponding points.

  ##### Positional Arguments
  - **self**: `Evision.Stereo.QuasiDenseStereo.t()`

  ##### Return
  - **denseMatches**: `[MatchQuasiDense]`.

    A vector containing all dense matches.

  **Note**: The method clears the denseMatches vector.
  **Note**: The returned Match elements inside the sMatches vector, do not use corr member.

  Python prototype (for reference only):
  ```python3
  getDenseMatches() -> denseMatches
  ```
  """
  @spec getDenseMatches(Evision.Stereo.QuasiDenseStereo.t()) :: list(Evision.Stereo.MatchQuasiDense.t()) | {:error, String.t()}
  def getDenseMatches(self) do
    positional = [
    ]
    :evision_nif.stereo_stereo_QuasiDenseStereo_getDenseMatches(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Compute and return the disparity map based on the correspondences found in the "process" method.

  ##### Positional Arguments
  - **self**: `Evision.Stereo.QuasiDenseStereo.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  **Note**: Default level is 50
  @return cv::Mat containing a the disparity image in grayscale.
  @sa computeDisparity
  @sa quantizeDisparity

  Python prototype (for reference only):
  ```python3
  getDisparity() -> retval
  ```
  """
  @spec getDisparity(Evision.Stereo.QuasiDenseStereo.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getDisparity(self) do
    positional = [
    ]
    :evision_nif.stereo_stereo_QuasiDenseStereo_getDisparity(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Specify pixel coordinates in the left image and get its corresponding location in the right image.

  ##### Positional Arguments
  - **self**: `Evision.Stereo.QuasiDenseStereo.t()`
  - **x**: `integer()`.

    The x pixel coordinate in the left image channel.

  - **y**: `integer()`.

    The y pixel coordinate in the left image channel.

  ##### Return
  - **retval**: `cv::Point2f`

  @retval cv::Point(x, y) The location of the corresponding pixel in the right image.
  @retval cv::Point(0, 0) (NO_MATCH)  if no match is found in the right image for the specified pixel location in the left image.
  **Note**: This method should be always called after process, otherwise the matches will not be correct.

  Python prototype (for reference only):
  ```python3
  getMatch(x, y) -> retval
  ```
  """
  @spec getMatch(Evision.Stereo.QuasiDenseStereo.t(), integer(), integer()) :: {number(), number()} | {:error, String.t()}
  def getMatch(self, x, y) when is_integer(x) and is_integer(y)
  do
    positional = [
      x: Evision.Internal.Structurise.from_struct(x),
      y: Evision.Internal.Structurise.from_struct(y)
    ]
    :evision_nif.stereo_stereo_QuasiDenseStereo_getMatch(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Get The sparse corresponding points.

  ##### Positional Arguments
  - **self**: `Evision.Stereo.QuasiDenseStereo.t()`

  ##### Return
  - **sMatches**: `[MatchQuasiDense]`.

    A vector containing all sparse correspondences.

  **Note**: The method clears the sMatches vector.
  **Note**: The returned Match elements inside the sMatches vector, do not use corr member.

  Python prototype (for reference only):
  ```python3
  getSparseMatches() -> sMatches
  ```
  """
  @spec getSparseMatches(Evision.Stereo.QuasiDenseStereo.t()) :: list(Evision.Stereo.MatchQuasiDense.t()) | {:error, String.t()}
  def getSparseMatches(self) do
    positional = [
    ]
    :evision_nif.stereo_stereo_QuasiDenseStereo_getSparseMatches(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Load a file containing the configuration parameters of the class.

  ##### Positional Arguments
  - **self**: `Evision.Stereo.QuasiDenseStereo.t()`
  - **filepath**: `String`.

    The location of the .YAML file containing the configuration parameters.

  ##### Return
  - **retval**: `integer()`

  **Note**: default value is an empty string in which case the default parameters will be loaded.
  @retval 1: If the path is not empty and the program loaded the parameters successfully.
  @retval 0: If the path is empty and the program loaded default parameters.
  @retval -1: If the file location is not valid or the program could not open the file and
   loaded default parameters from defaults.hpp.
  **Note**: The method is automatically called in the constructor and configures the class.
  **Note**: Loading different parameters will have an effect on the output. This is useful for tuning
   in case of video processing.
  @sa loadParameters

  Python prototype (for reference only):
  ```python3
  loadParameters(filepath) -> retval
  ```
  """
  @spec loadParameters(Evision.Stereo.QuasiDenseStereo.t(), binary()) :: integer() | {:error, String.t()}
  def loadParameters(self, filepath) when is_binary(filepath)
  do
    positional = [
      filepath: Evision.Internal.Structurise.from_struct(filepath)
    ]
    :evision_nif.stereo_stereo_QuasiDenseStereo_loadParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Main process of the algorithm. This method computes the sparse seeds and then densifies them.

  ##### Positional Arguments
  - **self**: `Evision.Stereo.QuasiDenseStereo.t()`
  - **imgLeft**: `Evision.Mat`.

    The left Channel of a stereo image pair.

  - **imgRight**: `Evision.Mat`.

    The right Channel of a stereo image pair.

   Initially input images are converted to gray-scale and then the sparseMatching method
   is called to obtain the sparse stereo. Finally quasiDenseMatching is called to densify the corresponding
   points.
  **Note**: If input images are in color, the method assumes that are BGR and converts them to grayscale.
  @sa sparseMatching
  @sa quasiDenseMatching

  Python prototype (for reference only):
  ```python3
  process(imgLeft, imgRight) -> None
  ```
  """
  @spec process(Evision.Stereo.QuasiDenseStereo.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Stereo.QuasiDenseStereo.t() | {:error, String.t()}
  def process(self, imgLeft, imgRight) when (is_struct(imgLeft, Evision.Mat) or is_struct(imgLeft, Nx.Tensor) or is_number(imgLeft) or is_tuple(imgLeft)) and (is_struct(imgRight, Evision.Mat) or is_struct(imgRight, Nx.Tensor) or is_number(imgRight) or is_tuple(imgRight))
  do
    positional = [
      imgLeft: Evision.Internal.Structurise.from_struct(imgLeft),
      imgRight: Evision.Internal.Structurise.from_struct(imgRight)
    ]
    :evision_nif.stereo_stereo_QuasiDenseStereo_process(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Save a file containing all the configuration parameters the class is currently set to.

  ##### Positional Arguments
  - **self**: `Evision.Stereo.QuasiDenseStereo.t()`
  - **filepath**: `String`.

    The location to store the parameters file.

  ##### Return
  - **retval**: `integer()`

  **Note**: Calling this method with no arguments will result in storing class parameters to a file
   names "qds_parameters.yaml" in the root project folder.
  **Note**: This method can be used to generate a template file for tuning the class.
  @sa loadParameters

  Python prototype (for reference only):
  ```python3
  saveParameters(filepath) -> retval
  ```
  """
  @spec saveParameters(Evision.Stereo.QuasiDenseStereo.t(), binary()) :: integer() | {:error, String.t()}
  def saveParameters(self, filepath) when is_binary(filepath)
  do
    positional = [
      filepath: Evision.Internal.Structurise.from_struct(filepath)
    ]
    :evision_nif.stereo_stereo_QuasiDenseStereo_saveParameters(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_Param(Evision.Stereo.QuasiDenseStereo.t()) :: Evision.Stereo.PropagationParameters.t()
  def get_Param(self) do
    :evision_nif.stereo_QuasiDenseStereo_get_Param(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_Param(Evision.Stereo.QuasiDenseStereo.t(), Evision.Stereo.PropagationParameters.t()) :: Evision.Stereo.QuasiDenseStereo.t()
  def set_Param(self, prop) do
    :evision_nif.stereo_QuasiDenseStereo_set_Param(
        Evision.Internal.Structurise.from_struct(self),
        [Param: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
