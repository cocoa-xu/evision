defmodule Evision.CUDACodec.VideoReader do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDACodec.VideoReader` struct.

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
  def to_struct({:ok, %{class: Evision.CUDACodec.VideoReader, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDACodec.VideoReader, ref: ref}) do
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
  Returns information about video file format.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`

  ##### Return
  - **retval**: `FormatInfo`

  Python prototype (for reference only):
  ```python3
  format() -> retval
  ```
  """
  @spec format(Evision.CUDACodec.CUDACodec.VideoReader.t()) :: Evision.CUDACodec.FormatInfo.t() | {:error, String.t()}
  def format(self) do
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_format(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Retrieves the specified property used by the VideoSource.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`
  - **propertyId**: `integer()`.

    Property identifier from cv::VideoCaptureProperties (eg. cv::CAP_PROP_POS_MSEC, cv::CAP_PROP_POS_FRAMES, ...)
    or one from @ref videoio_flags_others.

  ##### Return
  - **retval**: `bool`
  - **propertyVal**: `double`.

    Value for the specified property.

  @return `true` unless the property is unset set or not supported.

  Python prototype (for reference only):
  ```python3
  get(propertyId) -> retval, propertyVal
  ```
  """
  @spec get(Evision.CUDACodec.CUDACodec.VideoReader.t(), integer()) :: number() | false | {:error, String.t()}
  def get(self, propertyId) when is_integer(propertyId)
  do
    positional = [
      propertyId: Evision.Internal.Structurise.from_struct(propertyId)
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_get(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the specified VideoReader property

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`
  - **propertyId**: `VideoReaderProps`.

    Property identifier from cv::cudacodec::VideoReaderProps (eg. cv::cudacodec::VideoReaderProps::PROP_DECODED_FRAME_IDX,
    cv::cudacodec::VideoReaderProps::PROP_EXTRA_DATA_INDEX, ...).

  ##### Keyword Arguments
  - **propertyValIn**: `double`.

  ##### Return
  - **retval**: `bool`
  - **propertyValOut**: `double`

  @return `true` unless the property is not supported.

  Python prototype (for reference only):
  ```python3
  getVideoReaderProps(propertyId[, propertyValIn]) -> retval, propertyValOut
  ```
  """
  @spec getVideoReaderProps(Evision.CUDACodec.CUDACodec.VideoReader.t(), Evision.CUDACodec.VideoReaderProps.t(), [{:propertyValIn, term()}] | nil) :: number() | false | {:error, String.t()}
  def getVideoReaderProps(self, propertyId, opts) when is_struct(propertyId, Evision.CUDACodec.VideoReaderProps) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:propertyValIn])
    positional = [
      propertyId: Evision.Internal.Structurise.from_struct(propertyId)
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_getVideoReaderProps(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Returns the specified VideoReader property

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`
  - **propertyId**: `VideoReaderProps`.

    Property identifier from cv::cudacodec::VideoReaderProps (eg. cv::cudacodec::VideoReaderProps::PROP_DECODED_FRAME_IDX,
    cv::cudacodec::VideoReaderProps::PROP_EXTRA_DATA_INDEX, ...).

  ##### Keyword Arguments
  - **propertyValIn**: `double`.

  ##### Return
  - **retval**: `bool`
  - **propertyValOut**: `double`

  @return `true` unless the property is not supported.

  Python prototype (for reference only):
  ```python3
  getVideoReaderProps(propertyId[, propertyValIn]) -> retval, propertyValOut
  ```
  """
  @spec getVideoReaderProps(Evision.CUDACodec.CUDACodec.VideoReader.t(), Evision.CUDACodec.VideoReaderProps.t()) :: number() | false | {:error, String.t()}
  def getVideoReaderProps(self, propertyId) when is_struct(propertyId, Evision.CUDACodec.VideoReaderProps)
  do
    positional = [
      propertyId: Evision.Internal.Structurise.from_struct(propertyId)
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_getVideoReaderProps(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Grabs the next frame from the video source.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`

  ##### Keyword Arguments
  - **stream**: `cuda_Stream`.

    Stream for the asynchronous version.

  ##### Return
  - **retval**: `bool`

  @return `true` (non-zero) in the case of success.
  The method/function grabs the next frame from video file or camera and returns true (non-zero) in
  the case of success.
  The primary use of the function is for reading both the encoded and decoded video data when rawMode is enabled.  With rawMode enabled
  retrieve() can be called following grab() to retrieve all the data associated with the current video source since the last call to grab() or the creation of the VideoReader.

  Python prototype (for reference only):
  ```python3
  grab([, stream]) -> retval
  ```
  """
  @spec grab(Evision.CUDACodec.CUDACodec.VideoReader.t(), [{:stream, term()}] | nil) :: boolean() | {:error, String.t()}
  def grab(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_grab(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Grabs the next frame from the video source.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`

  ##### Keyword Arguments
  - **stream**: `cuda_Stream`.

    Stream for the asynchronous version.

  ##### Return
  - **retval**: `bool`

  @return `true` (non-zero) in the case of success.
  The method/function grabs the next frame from video file or camera and returns true (non-zero) in
  the case of success.
  The primary use of the function is for reading both the encoded and decoded video data when rawMode is enabled.  With rawMode enabled
  retrieve() can be called following grab() to retrieve all the data associated with the current video source since the last call to grab() or the creation of the VideoReader.

  Python prototype (for reference only):
  ```python3
  grab([, stream]) -> retval
  ```
  """
  @spec grab(Evision.CUDACodec.CUDACodec.VideoReader.t()) :: boolean() | {:error, String.t()}
  def grab(self) do
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_grab(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Grabs, decodes and returns the next video frame.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`

  ##### Keyword Arguments
  - **stream**: `cuda_Stream`.

    Stream for the asynchronous version.

  ##### Return
  - **retval**: `bool`
  - **frame**: `Evision.CUDA.GpuMat.t()`.

  @return `false` if no frames have been grabbed.
  If no frames have been grabbed (there are no more frames in video file), the methods return false.
  The method throws an Exception if error occurs.

  Python prototype (for reference only):
  ```python3
  nextFrame([, frame[, stream]]) -> retval, frame
  ```
  """
  @spec nextFrame(Evision.CUDACodec.CUDACodec.VideoReader.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | false | {:error, String.t()}
  def nextFrame(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_nextFrame(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Grabs, decodes and returns the next video frame.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`

  ##### Keyword Arguments
  - **stream**: `cuda_Stream`.

    Stream for the asynchronous version.

  ##### Return
  - **retval**: `bool`
  - **frame**: `Evision.CUDA.GpuMat.t()`.

  @return `false` if no frames have been grabbed.
  If no frames have been grabbed (there are no more frames in video file), the methods return false.
  The method throws an Exception if error occurs.

  Python prototype (for reference only):
  ```python3
  nextFrame([, frame[, stream]]) -> retval, frame
  ```
  """
  @spec nextFrame(Evision.CUDACodec.CUDACodec.VideoReader.t()) :: Evision.CUDA.GpuMat.t() | false | {:error, String.t()}
  def nextFrame(self) do
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_nextFrame(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Grabs, decodes and returns the next video frame and frame luma histogram.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`

  ##### Keyword Arguments
  - **stream**: `cuda_Stream`.

    Stream for the asynchronous version.

  ##### Return
  - **retval**: `bool`
  - **frame**: `Evision.CUDA.GpuMat.t()`.
  - **histogram**: `Evision.CUDA.GpuMat.t()`.

  @return `false` if no frames have been grabbed.
  If no frames have been grabbed (there are no more frames in video file), the methods return false.
  The method throws an Exception if error occurs.
  **Note**: Histogram data is collected by NVDEC during the decoding process resulting in zero performance penalty. NVDEC computes the histogram data for only the luma component of decoded output, not on post-processed frame(i.e. when scaling, cropping, etc. applied).  If the source is encoded using a limited range of luma values (FormatInfo::videoFullRangeFlag == false) then the histogram bin values will correspond to to this limited range of values and will need to be mapped to contain the same output as cuda::calcHist().  The MapHist() utility function can be used to perform this mapping on the host if required.

  Python prototype (for reference only):
  ```python3
  nextFrameWithHist([, frame[, histogram[, stream]]]) -> retval, frame, histogram
  ```
  """
  @spec nextFrameWithHist(Evision.CUDACodec.CUDACodec.VideoReader.t(), [{:stream, term()}] | nil) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | false | {:error, String.t()}
  def nextFrameWithHist(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_nextFrameWithHist(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Grabs, decodes and returns the next video frame and frame luma histogram.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`

  ##### Keyword Arguments
  - **stream**: `cuda_Stream`.

    Stream for the asynchronous version.

  ##### Return
  - **retval**: `bool`
  - **frame**: `Evision.CUDA.GpuMat.t()`.
  - **histogram**: `Evision.CUDA.GpuMat.t()`.

  @return `false` if no frames have been grabbed.
  If no frames have been grabbed (there are no more frames in video file), the methods return false.
  The method throws an Exception if error occurs.
  **Note**: Histogram data is collected by NVDEC during the decoding process resulting in zero performance penalty. NVDEC computes the histogram data for only the luma component of decoded output, not on post-processed frame(i.e. when scaling, cropping, etc. applied).  If the source is encoded using a limited range of luma values (FormatInfo::videoFullRangeFlag == false) then the histogram bin values will correspond to to this limited range of values and will need to be mapped to contain the same output as cuda::calcHist().  The MapHist() utility function can be used to perform this mapping on the host if required.

  Python prototype (for reference only):
  ```python3
  nextFrameWithHist([, frame[, histogram[, stream]]]) -> retval, frame, histogram
  ```
  """
  @spec nextFrameWithHist(Evision.CUDACodec.CUDACodec.VideoReader.t()) :: {Evision.CUDA.GpuMat.t(), Evision.CUDA.GpuMat.t()} | false | {:error, String.t()}
  def nextFrameWithHist(self) do
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_nextFrameWithHist(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns previously grabbed encoded video data.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`
  - **idx**: `size_t`.

    Determines the returned data inside image. The returned data can be the:
    - Extra data if available, idx = get(PROP_EXTRA_DATA_INDEX).
    - Raw encoded data package.  To retrieve package i,  idx = get(PROP_RAW_PACKAGES_BASE_INDEX) + i with i < get(PROP_NUMBER_OF_RAW_PACKAGES_SINCE_LAST_GRAB)

  ##### Return
  - **retval**: `bool`
  - **frame**: `Evision.Mat.t()`.

  @return `false` if no frames have been grabbed
  The method returns data associated with the current video source since the last call to grab() or the creation of the VideoReader. If no data is present
  the method returns false and the function returns an empty image.

  Python prototype (for reference only):
  ```python3
  retrieve(idx[, frame]) -> retval, frame
  ```
  """
  @spec retrieve(Evision.CUDACodec.CUDACodec.VideoReader.t(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def retrieve(self, idx, opts) when is_integer(idx) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      idx: Evision.Internal.Structurise.from_struct(idx)
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_retrieve(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Returns previously grabbed encoded video data.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`
  - **idx**: `size_t`.

    Determines the returned data inside image. The returned data can be the:
    - Extra data if available, idx = get(PROP_EXTRA_DATA_INDEX).
    - Raw encoded data package.  To retrieve package i,  idx = get(PROP_RAW_PACKAGES_BASE_INDEX) + i with i < get(PROP_NUMBER_OF_RAW_PACKAGES_SINCE_LAST_GRAB)

  ##### Return
  - **retval**: `bool`
  - **frame**: `Evision.Mat.t()`.

  @return `false` if no frames have been grabbed
  The method returns data associated with the current video source since the last call to grab() or the creation of the VideoReader. If no data is present
  the method returns false and the function returns an empty image.

  Python prototype (for reference only):
  ```python3
  retrieve(idx[, frame]) -> retval, frame
  ```
  #### Variant 2:
  Returns the next video frame.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`

  ##### Return
  - **retval**: `bool`
  - **frame**: `Evision.CUDA.GpuMat.t()`.

  @return `false` if no frames have been grabbed
  The method returns data associated with the current video source since the last call to grab(). If no data is present
  the method returns false and the function returns an empty image.

  Python prototype (for reference only):
  ```python3
  retrieve([, frame]) -> retval, frame
  ```

  """
  @spec retrieve(Evision.CUDACodec.CUDACodec.VideoReader.t(), [{atom(), term()},...] | nil) :: Evision.CUDA.GpuMat.t() | false | {:error, String.t()}
  def retrieve(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_retrieve(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec retrieve(Evision.CUDACodec.CUDACodec.VideoReader.t(), integer()) :: Evision.Mat.t() | false | {:error, String.t()}
  def retrieve(self, idx) when is_integer(idx)
  do
    positional = [
      idx: Evision.Internal.Structurise.from_struct(idx)
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_retrieve(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns the next video frame.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`

  ##### Return
  - **retval**: `bool`
  - **frame**: `Evision.CUDA.GpuMat.t()`.

  @return `false` if no frames have been grabbed
  The method returns data associated with the current video source since the last call to grab(). If no data is present
  the method returns false and the function returns an empty image.

  Python prototype (for reference only):
  ```python3
  retrieve([, frame]) -> retval, frame
  ```
  """
  @spec retrieve(Evision.CUDACodec.CUDACodec.VideoReader.t()) :: Evision.CUDA.GpuMat.t() | false | {:error, String.t()}
  def retrieve(self) do
    positional = [
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_retrieve(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Set the desired ColorFormat for the frame returned by nextFrame()/retrieve().

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`
  - **colorFormat**: `ColorFormat`.

    Value of the ColorFormat.

  ##### Return
  - **retval**: `bool`

  @return `true` unless the colorFormat is not supported.

  Python prototype (for reference only):
  ```python3
  set(colorFormat) -> retval
  ```
  """
  @spec set(Evision.CUDACodec.CUDACodec.VideoReader.t(), Evision.CUDACodec.ColorFormat.t()) :: boolean() | {:error, String.t()}
  def set(self, colorFormat) when is_struct(colorFormat, Evision.CUDACodec.ColorFormat)
  do
    positional = [
      colorFormat: Evision.Internal.Structurise.from_struct(colorFormat)
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_set(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets a property in the VideoReader.

  ##### Positional Arguments
  - **self**: `Evision.CUDACodec.VideoReader.t()`
  - **propertyId**: `VideoReaderProps`.

    Property identifier from cv::cudacodec::VideoReaderProps (eg. cv::cudacodec::VideoReaderProps::PROP_DECODED_FRAME_IDX,
    cv::cudacodec::VideoReaderProps::PROP_EXTRA_DATA_INDEX, ...).

  - **propertyVal**: `double`.

    Value of the property.

  ##### Return
  - **retval**: `bool`

  @return `true` if the property has been set.

  Python prototype (for reference only):
  ```python3
  setVideoReaderProps(propertyId, propertyVal) -> retval
  ```
  """
  @spec setVideoReaderProps(Evision.CUDACodec.CUDACodec.VideoReader.t(), Evision.CUDACodec.VideoReaderProps.t(), number()) :: boolean() | {:error, String.t()}
  def setVideoReaderProps(self, propertyId, propertyVal) when is_struct(propertyId, Evision.CUDACodec.VideoReaderProps) and is_number(propertyVal)
  do
    positional = [
      propertyId: Evision.Internal.Structurise.from_struct(propertyId),
      propertyVal: Evision.Internal.Structurise.from_struct(propertyVal)
    ]
    :evision_nif.cudacodec_cudacodec_VideoReader_setVideoReaderProps(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
