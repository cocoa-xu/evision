defmodule Evision.CUDACodec do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDACodec` struct.

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
  def to_struct({:ok, %{class: Evision.CUDACodec, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDACodec, ref: ref}) do
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
  Utility function demonstrating how to map the luma histogram when FormatInfo::videoFullRangeFlag == false

  ##### Positional Arguments
  - **hist**: `Evision.CUDA.GpuMat.t()`.

    Luma histogram \\a hist returned from VideoReader::nextFrame(GpuMat& frame, GpuMat& hist, Stream& stream).

  ##### Return
  - **histFull**: `Evision.Mat.t()`.

    Host histogram equivelent to downloading \\a hist after calling cuda::calcHist(InputArray frame, OutputArray hist, Stream& stream).

  **Note**: 
  - This function demonstrates how to map the luma histogram back so that it is equivalent to the result obtained from cuda::calcHist()
    if the returned frame was colorFormat::GRAY.

  Python prototype (for reference only):
  ```python3
  MapHist(hist[, histFull]) -> histFull
  ```
  """
  @spec mapHist(Evision.CUDA.GpuMat.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def mapHist(hist, opts) when is_struct(hist, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      hist: Evision.Internal.Structurise.from_struct(hist)
    ]
    :evision_nif.cudacodec_MapHist(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Utility function demonstrating how to map the luma histogram when FormatInfo::videoFullRangeFlag == false

  ##### Positional Arguments
  - **hist**: `Evision.CUDA.GpuMat.t()`.

    Luma histogram \\a hist returned from VideoReader::nextFrame(GpuMat& frame, GpuMat& hist, Stream& stream).

  ##### Return
  - **histFull**: `Evision.Mat.t()`.

    Host histogram equivelent to downloading \\a hist after calling cuda::calcHist(InputArray frame, OutputArray hist, Stream& stream).

  **Note**: 
  - This function demonstrates how to map the luma histogram back so that it is equivalent to the result obtained from cuda::calcHist()
    if the returned frame was colorFormat::GRAY.

  Python prototype (for reference only):
  ```python3
  MapHist(hist[, histFull]) -> histFull
  ```
  """
  @spec mapHist(Evision.CUDA.GpuMat.t()) :: Evision.Mat.t() | {:error, String.t()}
  def mapHist(hist) when is_struct(hist, Evision.CUDA.GpuMat)
  do
    positional = [
      hist: Evision.Internal.Structurise.from_struct(hist)
    ]
    :evision_nif.cudacodec_MapHist(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  createVideoReader

  ##### Positional Arguments
  - **source**: `RawVideoSource`.

    RAW video source implemented by user.

  ##### Keyword Arguments
  - **params**: `VideoReaderInitParams`.

    Initializaton parameters. See cv::cudacodec::VideoReaderInitParams.

  ##### Return
  - **retval**: `VideoReader`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  createVideoReader(source[, params]) -> retval
  ```
  #### Variant 2:
  Creates video reader.

  ##### Positional Arguments
  - **filename**: `String`.

    Name of the input video file.

  ##### Keyword Arguments
  - **sourceParams**: `[integer()]`.

    Pass through parameters for VideoCapure.  VideoCapture with the FFMpeg back end (CAP_FFMPEG) is used to parse the video input.
    The `sourceParams` parameter allows to specify extra parameters encoded as pairs `(paramId_1, paramValue_1, paramId_2, paramValue_2, ...)`.
    See cv::VideoCaptureProperties
    e.g. when streaming from an RTSP source CAP_PROP_OPEN_TIMEOUT_MSEC may need to be set.

  - **params**: `VideoReaderInitParams`.

    Initializaton parameters. See cv::cudacodec::VideoReaderInitParams.

  ##### Return
  - **retval**: `VideoReader`

  FFMPEG is used to read videos. User can implement own demultiplexing with cudacodec::RawVideoSource

  Python prototype (for reference only):
  ```python3
  createVideoReader(filename[, sourceParams[, params]]) -> retval
  ```

  """
  @spec createVideoReader(Evision.CUDACodec.RawVideoSource.t(), [{:params, term()}] | nil) :: Evision.CUDACodec.VideoReader.t() | {:error, String.t()}
  def createVideoReader(source, opts) when (is_reference(source) or is_struct(source)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:params])
    positional = [
      source: Evision.Internal.Structurise.from_struct(source)
    ]
    :evision_nif.cudacodec_createVideoReader(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec createVideoReader(binary(), [{:params, term()} | {:sourceParams, term()}] | nil) :: Evision.CUDACodec.VideoReader.t() | {:error, String.t()}
  def createVideoReader(filename, opts) when is_binary(filename) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:params, :sourceParams])
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cudacodec_createVideoReader(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  createVideoReader

  ##### Positional Arguments
  - **source**: `RawVideoSource`.

    RAW video source implemented by user.

  ##### Keyword Arguments
  - **params**: `VideoReaderInitParams`.

    Initializaton parameters. See cv::cudacodec::VideoReaderInitParams.

  ##### Return
  - **retval**: `VideoReader`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  createVideoReader(source[, params]) -> retval
  ```
  #### Variant 2:
  Creates video reader.

  ##### Positional Arguments
  - **filename**: `String`.

    Name of the input video file.

  ##### Keyword Arguments
  - **sourceParams**: `[integer()]`.

    Pass through parameters for VideoCapure.  VideoCapture with the FFMpeg back end (CAP_FFMPEG) is used to parse the video input.
    The `sourceParams` parameter allows to specify extra parameters encoded as pairs `(paramId_1, paramValue_1, paramId_2, paramValue_2, ...)`.
    See cv::VideoCaptureProperties
    e.g. when streaming from an RTSP source CAP_PROP_OPEN_TIMEOUT_MSEC may need to be set.

  - **params**: `VideoReaderInitParams`.

    Initializaton parameters. See cv::cudacodec::VideoReaderInitParams.

  ##### Return
  - **retval**: `VideoReader`

  FFMPEG is used to read videos. User can implement own demultiplexing with cudacodec::RawVideoSource

  Python prototype (for reference only):
  ```python3
  createVideoReader(filename[, sourceParams[, params]]) -> retval
  ```

  """
  @spec createVideoReader(Evision.CUDACodec.RawVideoSource.t()) :: Evision.CUDACodec.VideoReader.t() | {:error, String.t()}
  def createVideoReader(source) when (is_reference(source) or is_struct(source))
  do
    positional = [
      source: Evision.Internal.Structurise.from_struct(source)
    ]
    :evision_nif.cudacodec_createVideoReader(positional)
    |> to_struct()
  end
  @spec createVideoReader(binary()) :: Evision.CUDACodec.VideoReader.t() | {:error, String.t()}
  def createVideoReader(filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cudacodec_createVideoReader(positional)
    |> to_struct()
  end

  @doc """
  Creates video writer.

  ##### Positional Arguments
  - **fileName**: `String`.

    Name of the output video file.

  - **frameSize**: `Size`.

    Size of the input video frames.

  - **codec**: `Codec`.

    Supports Codec::H264 and Codec::HEVC.

  - **fps**: `double`.

    Framerate of the created video stream.

  - **colorFormat**: `ColorFormat`.

    OpenCv color format of the frames to be encoded.

  - **params**: `EncoderParams`.

    Additional encoding parameters.

  ##### Keyword Arguments
  - **encoderCallback**: `EncoderCallback`.

    Callbacks for video encoder. See cudacodec::EncoderCallback. Required for working with the encoded video stream.

  - **stream**: `cuda_Stream`.

    Stream for frame pre-processing.

  ##### Return
  - **retval**: `Evision.CUDACodec.VideoWriter.t()`

  Python prototype (for reference only):
  ```python3
  createVideoWriter(fileName, frameSize, codec, fps, colorFormat, params[, encoderCallback[, stream]]) -> retval
  ```
  """
  @spec createVideoWriter(binary(), {number(), number()}, Evision.CUDACodec.Codec.t(), number(), Evision.CUDACodec.ColorFormat.t(), Evision.CUDACodec.EncoderParams.t(), [{:encoderCallback, term()} | {:stream, term()}] | nil) :: Evision.CUDACodec.VideoWriter.t() | {:error, String.t()}
  def createVideoWriter(fileName, frameSize, codec, fps, colorFormat, params, opts) when is_binary(fileName) and is_tuple(frameSize) and is_number(fps) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:encoderCallback, :stream])
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName),
      frameSize: Evision.Internal.Structurise.from_struct(frameSize),
      codec: Evision.Internal.Structurise.from_struct(codec),
      fps: Evision.Internal.Structurise.from_struct(fps),
      colorFormat: Evision.Internal.Structurise.from_struct(colorFormat),
      params: Evision.Internal.Structurise.from_struct(params)
    ]
    :evision_nif.cudacodec_createVideoWriter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates video writer.

  ##### Positional Arguments
  - **fileName**: `String`.

    Name of the output video file.

  - **frameSize**: `Size`.

    Size of the input video frames.

  - **codec**: `Codec`.

    Supports Codec::H264 and Codec::HEVC.

  - **fps**: `double`.

    Framerate of the created video stream.

  - **colorFormat**: `ColorFormat`.

    OpenCv color format of the frames to be encoded.

  - **params**: `EncoderParams`.

    Additional encoding parameters.

  ##### Keyword Arguments
  - **encoderCallback**: `EncoderCallback`.

    Callbacks for video encoder. See cudacodec::EncoderCallback. Required for working with the encoded video stream.

  - **stream**: `cuda_Stream`.

    Stream for frame pre-processing.

  ##### Return
  - **retval**: `Evision.CUDACodec.VideoWriter.t()`

  Python prototype (for reference only):
  ```python3
  createVideoWriter(fileName, frameSize, codec, fps, colorFormat, params[, encoderCallback[, stream]]) -> retval
  ```
  """
  @spec createVideoWriter(binary(), {number(), number()}, Evision.CUDACodec.Codec.t(), number(), Evision.CUDACodec.ColorFormat.t(), Evision.CUDACodec.EncoderParams.t()) :: Evision.CUDACodec.VideoWriter.t() | {:error, String.t()}
  def createVideoWriter(fileName, frameSize, codec, fps, colorFormat, params) when is_binary(fileName) and is_tuple(frameSize) and is_number(fps)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName),
      frameSize: Evision.Internal.Structurise.from_struct(frameSize),
      codec: Evision.Internal.Structurise.from_struct(codec),
      fps: Evision.Internal.Structurise.from_struct(fps),
      colorFormat: Evision.Internal.Structurise.from_struct(colorFormat),
      params: Evision.Internal.Structurise.from_struct(params)
    ]
    :evision_nif.cudacodec_createVideoWriter(positional)
    |> to_struct()
  end

  @doc """
  Creates video writer.

  ##### Positional Arguments
  - **fileName**: `String`.

    Name of the output video file.

  - **frameSize**: `Size`.

    Size of the input video frames.

  ##### Keyword Arguments
  - **codec**: `Codec`.

    Supports Codec::H264 and Codec::HEVC.

  - **fps**: `double`.

    Framerate of the created video stream.

  - **colorFormat**: `ColorFormat`.

    OpenCv color format of the frames to be encoded.

  - **encoderCallback**: `EncoderCallback`.

    Callbacks for video encoder. See cudacodec::EncoderCallback. Required for working with the encoded video stream.

  - **stream**: `cuda_Stream`.

    Stream for frame pre-processing.

  ##### Return
  - **retval**: `Evision.CUDACodec.VideoWriter.t()`

  Python prototype (for reference only):
  ```python3
  createVideoWriter(fileName, frameSize[, codec[, fps[, colorFormat[, encoderCallback[, stream]]]]]) -> retval
  ```
  """
  @spec createVideoWriter(binary(), {number(), number()}, [{:codec, term()} | {:colorFormat, term()} | {:encoderCallback, term()} | {:fps, term()} | {:stream, term()}] | nil) :: Evision.CUDACodec.VideoWriter.t() | {:error, String.t()}
  def createVideoWriter(fileName, frameSize, opts) when is_binary(fileName) and is_tuple(frameSize) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:codec, :colorFormat, :encoderCallback, :fps, :stream])
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName),
      frameSize: Evision.Internal.Structurise.from_struct(frameSize)
    ]
    :evision_nif.cudacodec_createVideoWriter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates video writer.

  ##### Positional Arguments
  - **fileName**: `String`.

    Name of the output video file.

  - **frameSize**: `Size`.

    Size of the input video frames.

  ##### Keyword Arguments
  - **codec**: `Codec`.

    Supports Codec::H264 and Codec::HEVC.

  - **fps**: `double`.

    Framerate of the created video stream.

  - **colorFormat**: `ColorFormat`.

    OpenCv color format of the frames to be encoded.

  - **encoderCallback**: `EncoderCallback`.

    Callbacks for video encoder. See cudacodec::EncoderCallback. Required for working with the encoded video stream.

  - **stream**: `cuda_Stream`.

    Stream for frame pre-processing.

  ##### Return
  - **retval**: `Evision.CUDACodec.VideoWriter.t()`

  Python prototype (for reference only):
  ```python3
  createVideoWriter(fileName, frameSize[, codec[, fps[, colorFormat[, encoderCallback[, stream]]]]]) -> retval
  ```
  """
  @spec createVideoWriter(binary(), {number(), number()}) :: Evision.CUDACodec.VideoWriter.t() | {:error, String.t()}
  def createVideoWriter(fileName, frameSize) when is_binary(fileName) and is_tuple(frameSize)
  do
    positional = [
      fileName: Evision.Internal.Structurise.from_struct(fileName),
      frameSize: Evision.Internal.Structurise.from_struct(frameSize)
    ]
    :evision_nif.cudacodec_createVideoWriter(positional)
    |> to_struct()
  end
end
