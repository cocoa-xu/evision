defmodule Evision.VideoCapture do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Evision.VideoCapture` struct.

  - **fps**. `double`.

    Frames per second.

  - **frame_count**. `double`.

    Total number of frames.
  
  - **frame_width**. `double`.

    Width of each frame.
  
  - **frame_height**. `double`.

    Height of each frame.
  
  - **isOpened**. `boolean`.

    Is successfully opened the video source.
  
  - **ref**. `reference`.

    The underlying erlang resource variable.

  """
  @type t :: %__MODULE__{
    fps: number(),
    frame_count: number(),
    frame_width: number(),
    frame_height: number(),
    isOpened: boolean(),
    ref: reference()
  }
  @enforce_keys [:fps, :frame_count, :frame_width, :frame_height, :isOpened, :ref]
  defstruct [:fps, :frame_count, :frame_width, :frame_height, :isOpened, :ref]
  alias __MODULE__, as: T

  @doc false
  def to_struct(cap = %{:class => Evision.VideoCapture, :ref => ref}) do
    %T{
      fps: cap.fps,
      frame_count: cap.frame_count,
      frame_width: cap.frame_width,
      frame_height: cap.frame_height,
      isOpened: cap.isOpened,
      ref: ref
    }
  end

  @doc false
  def to_struct({:ok, cap = %{:class => Evision.VideoCapture}}) do
    {:ok, to_struct(cap)}
  end

  @doc false
  def to_struct(pass_through) do
    Evision.Internal.Structurise.to_struct(pass_through)
  end


  @doc """
  Wait for ready frames from VideoCapture.

  ##### Positional Arguments
  - **streams**: `[Evision.VideoCapture]`.

    input video streams

  ##### Keyword Arguments
  - **timeoutNs**: `int64`.

    number of nanoseconds (0 - infinite)

  ##### Return
  - **retval**: `bool`
  - **readyIndex**: `[int]`.

    stream indexes with grabbed frames (ready to use .retrieve() to fetch actual frame)

  @return `true` if streamReady is not empty
  @throws Exception %Exception on stream errors (check .isOpened() to filter out malformed streams) or VideoCapture type is not supported
  The primary use of the function is in multi-camera environments.
  The method fills the ready state vector, grabs video frame, if camera is ready.
  After this call use VideoCapture::retrieve() to decode and fetch frame data.

  Python prototype (for reference only):
  ```python3
  waitAny(streams[, timeoutNs]) -> retval, readyIndex
  ```
  """
  @spec waitAny(list(Evision.VideoCapture.t()), [{:timeoutNs, term()}] | nil) :: list(integer()) | false | {:error, String.t()}
  def waitAny(streams, opts) when is_list(streams) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    opts = Keyword.validate!(opts || [], [:timeoutNs])
    positional = [
      streams: Evision.Internal.Structurise.from_struct(streams)
    ]
    :evision_nif.videoCapture_waitAny(positional ++ Evision.Internal.Structurise.from_struct(opts))
     |> to_struct()
  end

  @doc """
  Wait for ready frames from VideoCapture.

  ##### Positional Arguments
  - **streams**: `[Evision.VideoCapture]`.

    input video streams

  ##### Keyword Arguments
  - **timeoutNs**: `int64`.

    number of nanoseconds (0 - infinite)

  ##### Return
  - **retval**: `bool`
  - **readyIndex**: `[int]`.

    stream indexes with grabbed frames (ready to use .retrieve() to fetch actual frame)

  @return `true` if streamReady is not empty
  @throws Exception %Exception on stream errors (check .isOpened() to filter out malformed streams) or VideoCapture type is not supported
  The primary use of the function is in multi-camera environments.
  The method fills the ready state vector, grabs video frame, if camera is ready.
  After this call use VideoCapture::retrieve() to decode and fetch frame data.

  Python prototype (for reference only):
  ```python3
  waitAny(streams[, timeoutNs]) -> retval, readyIndex
  ```
  """
  @spec waitAny(list(Evision.VideoCapture.t())) :: list(integer()) | false | {:error, String.t()}
  def waitAny(streams) when is_list(streams)
  do
    positional = [
      streams: Evision.Internal.Structurise.from_struct(streams)
    ]
    :evision_nif.videoCapture_waitAny(positional)
    |> to_struct()
  end
  
  @doc """
  #### Variant 1:
  Opens a camera for video capturing with API Preference and parameters

  ##### Positional Arguments
  - **index**: `integer()`
  - **apiPreference**: `integer()`
  - **params**: `[integer()]`

  ##### Return
  - **self**: `Evision.VideoCapture.t()`

  Has overloading in C++

  The `params` parameter allows to specify extra parameters encoded as pairs `(paramId_1, paramValue_1, paramId_2, paramValue_2, ...)`.
  See cv::VideoCaptureProperties

  Python prototype (for reference only):
  ```python3
  VideoCapture(index, apiPreference, params) -> <VideoCapture object>
  ```
  #### Variant 2:
  Opens a video file or a capturing device or an IP video stream for video capturing with API Preference and parameters

  ##### Positional Arguments
  - **filename**: `String`
  - **apiPreference**: `integer()`
  - **params**: `[integer()]`

  ##### Return
  - **self**: `Evision.VideoCapture.t()`

  Has overloading in C++

  The `params` parameter allows to specify extra parameters encoded as pairs `(paramId_1, paramValue_1, paramId_2, paramValue_2, ...)`.
  See cv::VideoCaptureProperties

  Python prototype (for reference only):
  ```python3
  VideoCapture(filename, apiPreference, params) -> <VideoCapture object>
  ```

  """
  @spec videoCapture(integer(), integer(), list(integer())) :: Evision.VideoCapture.t() | {:error, String.t()}
  def videoCapture(index, apiPreference, params) when is_integer(index) and is_integer(apiPreference) and is_list(params)
  do
    positional = [
      index: Evision.Internal.Structurise.from_struct(index),
      apiPreference: Evision.Internal.Structurise.from_struct(apiPreference),
      params: Evision.Internal.Structurise.from_struct(params)
    ]
    :evision_nif.videoCapture_VideoCapture(positional)
    |> to_struct()
  end
  @spec videoCapture(binary(), integer(), list(integer())) :: Evision.VideoCapture.t() | {:error, String.t()}
  def videoCapture(filename, apiPreference, params) when is_binary(filename) and is_integer(apiPreference) and is_list(params)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename),
      apiPreference: Evision.Internal.Structurise.from_struct(apiPreference),
      params: Evision.Internal.Structurise.from_struct(params)
    ]
    :evision_nif.videoCapture_VideoCapture(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Opens a camera for video capturing

  ##### Positional Arguments
  - **index**: `integer()`.

    id of the video capturing device to open. To open default camera using default backend just pass 0.
    (to backward compatibility usage of camera_id + domain_offset (CAP_*) is valid when apiPreference is CAP_ANY)

  ##### Keyword Arguments
  - **apiPreference**: `integer()`.

    preferred Capture API backends to use. Can be used to enforce a specific reader
    implementation if multiple are available: e.g. cv::CAP_DSHOW or cv::CAP_MSMF or cv::CAP_V4L.

  ##### Return
  - **self**: `Evision.VideoCapture.t()`

  Has overloading in C++

  @sa cv::VideoCaptureAPIs

  Python prototype (for reference only):
  ```python3
  VideoCapture(index[, apiPreference]) -> <VideoCapture object>
  ```
  #### Variant 2:
  Opens a video file or a capturing device or an IP video stream for video capturing with API Preference

  ##### Positional Arguments
  - **filename**: `String`.

    it can be:
    - name of video file (eg. `video.avi`)
    - or image sequence (eg. `img_%02d.jpg`, which will read samples like `img_00.jpg, img_01.jpg, img_02.jpg, ...`)
    - or URL of video stream (eg. `protocol://host:port/script_name?script_params|auth`)
    - or GStreamer pipeline string in gst-launch tool format in case if GStreamer is used as backend
      Note that each video stream or IP camera feed has its own URL scheme. Please refer to the
      documentation of source stream to know the right URL.

  ##### Keyword Arguments
  - **apiPreference**: `integer()`.

    preferred Capture API backends to use. Can be used to enforce a specific reader
    implementation if multiple are available: e.g. cv::CAP_FFMPEG or cv::CAP_IMAGES or cv::CAP_DSHOW.

  ##### Return
  - **self**: `Evision.VideoCapture.t()`

  Has overloading in C++

  @sa cv::VideoCaptureAPIs

  Python prototype (for reference only):
  ```python3
  VideoCapture(filename[, apiPreference]) -> <VideoCapture object>
  ```

  """
  @spec videoCapture(integer(), [{:apiPreference, term()}] | nil) :: Evision.VideoCapture.t() | {:error, String.t()}
  def videoCapture(index, opts) when is_integer(index) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:apiPreference])
    positional = [
      index: Evision.Internal.Structurise.from_struct(index)
    ]
    :evision_nif.videoCapture_VideoCapture(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec videoCapture(binary(), [{:apiPreference, term()}] | nil) :: Evision.VideoCapture.t() | {:error, String.t()}
  def videoCapture(filename, opts) when is_binary(filename) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:apiPreference])
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.videoCapture_VideoCapture(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Opens a camera for video capturing

  ##### Positional Arguments
  - **index**: `integer()`.

    id of the video capturing device to open. To open default camera using default backend just pass 0.
    (to backward compatibility usage of camera_id + domain_offset (CAP_*) is valid when apiPreference is CAP_ANY)

  ##### Keyword Arguments
  - **apiPreference**: `integer()`.

    preferred Capture API backends to use. Can be used to enforce a specific reader
    implementation if multiple are available: e.g. cv::CAP_DSHOW or cv::CAP_MSMF or cv::CAP_V4L.

  ##### Return
  - **self**: `Evision.VideoCapture.t()`

  Has overloading in C++

  @sa cv::VideoCaptureAPIs

  Python prototype (for reference only):
  ```python3
  VideoCapture(index[, apiPreference]) -> <VideoCapture object>
  ```
  #### Variant 2:
  Opens a video file or a capturing device or an IP video stream for video capturing with API Preference

  ##### Positional Arguments
  - **filename**: `String`.

    it can be:
    - name of video file (eg. `video.avi`)
    - or image sequence (eg. `img_%02d.jpg`, which will read samples like `img_00.jpg, img_01.jpg, img_02.jpg, ...`)
    - or URL of video stream (eg. `protocol://host:port/script_name?script_params|auth`)
    - or GStreamer pipeline string in gst-launch tool format in case if GStreamer is used as backend
      Note that each video stream or IP camera feed has its own URL scheme. Please refer to the
      documentation of source stream to know the right URL.

  ##### Keyword Arguments
  - **apiPreference**: `integer()`.

    preferred Capture API backends to use. Can be used to enforce a specific reader
    implementation if multiple are available: e.g. cv::CAP_FFMPEG or cv::CAP_IMAGES or cv::CAP_DSHOW.

  ##### Return
  - **self**: `Evision.VideoCapture.t()`

  Has overloading in C++

  @sa cv::VideoCaptureAPIs

  Python prototype (for reference only):
  ```python3
  VideoCapture(filename[, apiPreference]) -> <VideoCapture object>
  ```

  """
  @spec videoCapture(integer()) :: Evision.VideoCapture.t() | {:error, String.t()}
  def videoCapture(index) when is_integer(index)
  do
    positional = [
      index: Evision.Internal.Structurise.from_struct(index)
    ]
    :evision_nif.videoCapture_VideoCapture(positional)
    |> to_struct()
  end
  @spec videoCapture(binary()) :: Evision.VideoCapture.t() | {:error, String.t()}
  def videoCapture(filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.videoCapture_VideoCapture(positional)
    |> to_struct()
  end

  @doc """
  Default constructor
  ##### Return
  - **self**: `Evision.VideoCapture.t()`

  **Note**: In @ref videoio_c "C API", when you finished working with video, release CvCapture structure with
  cvReleaseCapture(), or use Ptr\\<CvCapture\\> that calls cvReleaseCapture() automatically in the
  destructor.

  Python prototype (for reference only):
  ```python3
  VideoCapture() -> <VideoCapture object>
  ```
  """
  @spec videoCapture() :: Evision.VideoCapture.t() | {:error, String.t()}
  def videoCapture() do
    positional = [
    ]
    :evision_nif.videoCapture_VideoCapture(positional)
    |> to_struct()
  end

  @doc """
  Returns the specified VideoCapture property

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`
  - **propId**: `integer()`.

    Property identifier from cv::VideoCaptureProperties (eg. cv::CAP_PROP_POS_MSEC, cv::CAP_PROP_POS_FRAMES, ...)
    or one from @ref videoio_flags_others

  ##### Return
  - **retval**: `double`

  @return Value for the specified property. Value 0 is returned when querying a property that is
  not supported by the backend used by the VideoCapture instance.
  **Note**: Reading / writing properties involves many layers. Some unexpected result might happens
  along this chain.
  ```txt
  VideoCapture -> API Backend -> Operating System -> Device Driver -> Device Hardware
  ```
  The returned value might be different from what really used by the device or it could be encoded
  using device dependent rules (eg. steps or percentage). Effective behaviour depends from device
  driver and API Backend

  Python prototype (for reference only):
  ```python3
  get(propId) -> retval
  ```
  """
  @spec get(Evision.VideoCapture.t(), integer()) :: number() | {:error, String.t()}
  def get(self, propId) when is_integer(propId)
  do
    positional = [
      propId: Evision.Internal.Structurise.from_struct(propId)
    ]
    :evision_nif.videoCapture_get(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns used backend API name

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`

  ##### Return
  - **retval**: `String`

  **Note**: Stream should be opened.

  Python prototype (for reference only):
  ```python3
  getBackendName() -> retval
  ```
  """
  @spec getBackendName(Evision.VideoCapture.t()) :: binary() | {:error, String.t()}
  def getBackendName(self) do
    positional = [
    ]
    :evision_nif.videoCapture_getBackendName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getExceptionMode

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getExceptionMode() -> retval
  ```
  """
  @spec getExceptionMode(Evision.VideoCapture.t()) :: boolean() | {:error, String.t()}
  def getExceptionMode(self) do
    positional = [
    ]
    :evision_nif.videoCapture_getExceptionMode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Grabs the next frame from video file or capturing device.

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`

  ##### Return
  - **retval**: `bool`

  @return `true` (non-zero) in the case of success.
  The method/function grabs the next frame from video file or camera and returns true (non-zero) in
  the case of success.
  The primary use of the function is in multi-camera environments, especially when the cameras do not
  have hardware synchronization. That is, you call VideoCapture::grab() for each camera and after that
  call the slower method VideoCapture::retrieve() to decode and get frame from each camera. This way
  the overhead on demosaicing or motion jpeg decompression etc. is eliminated and the retrieved frames
  from different cameras will be closer in time.
  Also, when a connected camera is multi-head (for example, a stereo camera or a Kinect device), the
  correct way of retrieving data from it is to call VideoCapture::grab() first and then call
  VideoCapture::retrieve() one or more times with different values of the channel parameter.
  @ref tutorial_kinect_openni

  Python prototype (for reference only):
  ```python3
  grab() -> retval
  ```
  """
  @spec grab(Evision.VideoCapture.t()) :: boolean() | {:error, String.t()}
  def grab(self) do
    positional = [
    ]
    :evision_nif.videoCapture_grab(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if video capturing has been initialized already.

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`

  ##### Return
  - **retval**: `bool`

  If the previous call to VideoCapture constructor or VideoCapture::open() succeeded, the method returns
  true.

  Python prototype (for reference only):
  ```python3
  isOpened() -> retval
  ```
  """
  @spec isOpened(Evision.VideoCapture.t()) :: boolean() | {:error, String.t()}
  def isOpened(self) do
    positional = [
    ]
    :evision_nif.videoCapture_isOpened(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Opens a camera for video capturing with API Preference and parameters

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`
  - **index**: `integer()`
  - **apiPreference**: `integer()`
  - **params**: `[integer()]`

  ##### Return
  - **retval**: `bool`

  Has overloading in C++

  The `params` parameter allows to specify extra parameters encoded as pairs `(paramId_1, paramValue_1, paramId_2, paramValue_2, ...)`.
  See cv::VideoCaptureProperties
  @return `true` if the camera has been successfully opened.
  The method first calls VideoCapture::release to close the already opened file or camera.

  Python prototype (for reference only):
  ```python3
  open(index, apiPreference, params) -> retval
  ```
  #### Variant 2:
  Opens a video file or a capturing device or an IP video stream for video capturing with API Preference and parameters

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`
  - **filename**: `String`
  - **apiPreference**: `integer()`
  - **params**: `[integer()]`

  ##### Return
  - **retval**: `bool`

  Has overloading in C++

  The `params` parameter allows to specify extra parameters encoded as pairs `(paramId_1, paramValue_1, paramId_2, paramValue_2, ...)`.
  See cv::VideoCaptureProperties
  @return `true` if the file has been successfully opened
  The method first calls VideoCapture::release to close the already opened file or camera.

  Python prototype (for reference only):
  ```python3
  open(filename, apiPreference, params) -> retval
  ```

  """
  @spec open(Evision.VideoCapture.t(), integer(), integer(), list(integer())) :: boolean() | {:error, String.t()}
  def open(self, index, apiPreference, params) when is_integer(index) and is_integer(apiPreference) and is_list(params)
  do
    positional = [
      index: Evision.Internal.Structurise.from_struct(index),
      apiPreference: Evision.Internal.Structurise.from_struct(apiPreference),
      params: Evision.Internal.Structurise.from_struct(params)
    ]
    :evision_nif.videoCapture_open(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec open(Evision.VideoCapture.t(), binary(), integer(), list(integer())) :: boolean() | {:error, String.t()}
  def open(self, filename, apiPreference, params) when is_binary(filename) and is_integer(apiPreference) and is_list(params)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename),
      apiPreference: Evision.Internal.Structurise.from_struct(apiPreference),
      params: Evision.Internal.Structurise.from_struct(params)
    ]
    :evision_nif.videoCapture_open(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Opens a camera for video capturing

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`
  - **index**: `integer()`

  ##### Keyword Arguments
  - **apiPreference**: `integer()`.

  ##### Return
  - **retval**: `bool`

  Has overloading in C++

  Parameters are same as the constructor VideoCapture(int index, int apiPreference = CAP_ANY)
  @return `true` if the camera has been successfully opened.
  The method first calls VideoCapture::release to close the already opened file or camera.

  Python prototype (for reference only):
  ```python3
  open(index[, apiPreference]) -> retval
  ```
  #### Variant 2:
  Opens a video file or a capturing device or an IP video stream for video capturing.

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`
  - **filename**: `String`

  ##### Keyword Arguments
  - **apiPreference**: `integer()`.

  ##### Return
  - **retval**: `bool`

  Has overloading in C++

  Parameters are same as the constructor VideoCapture(const String& filename, int apiPreference = CAP_ANY)
  @return `true` if the file has been successfully opened
  The method first calls VideoCapture::release to close the already opened file or camera.

  Python prototype (for reference only):
  ```python3
  open(filename[, apiPreference]) -> retval
  ```

  """
  @spec open(Evision.VideoCapture.t(), integer(), [{:apiPreference, term()}] | nil) :: boolean() | {:error, String.t()}
  def open(self, index, opts) when is_integer(index) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:apiPreference])
    positional = [
      index: Evision.Internal.Structurise.from_struct(index)
    ]
    :evision_nif.videoCapture_open(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec open(Evision.VideoCapture.t(), binary(), [{:apiPreference, term()}] | nil) :: boolean() | {:error, String.t()}
  def open(self, filename, opts) when is_binary(filename) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:apiPreference])
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.videoCapture_open(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Opens a camera for video capturing

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`
  - **index**: `integer()`

  ##### Keyword Arguments
  - **apiPreference**: `integer()`.

  ##### Return
  - **retval**: `bool`

  Has overloading in C++

  Parameters are same as the constructor VideoCapture(int index, int apiPreference = CAP_ANY)
  @return `true` if the camera has been successfully opened.
  The method first calls VideoCapture::release to close the already opened file or camera.

  Python prototype (for reference only):
  ```python3
  open(index[, apiPreference]) -> retval
  ```
  #### Variant 2:
  Opens a video file or a capturing device or an IP video stream for video capturing.

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`
  - **filename**: `String`

  ##### Keyword Arguments
  - **apiPreference**: `integer()`.

  ##### Return
  - **retval**: `bool`

  Has overloading in C++

  Parameters are same as the constructor VideoCapture(const String& filename, int apiPreference = CAP_ANY)
  @return `true` if the file has been successfully opened
  The method first calls VideoCapture::release to close the already opened file or camera.

  Python prototype (for reference only):
  ```python3
  open(filename[, apiPreference]) -> retval
  ```

  """
  @spec open(Evision.VideoCapture.t(), integer()) :: boolean() | {:error, String.t()}
  def open(self, index) when is_integer(index)
  do
    positional = [
      index: Evision.Internal.Structurise.from_struct(index)
    ]
    :evision_nif.videoCapture_open(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec open(Evision.VideoCapture.t(), binary()) :: boolean() | {:error, String.t()}
  def open(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.videoCapture_open(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Grabs, decodes and returns the next video frame.

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`

  ##### Return
  - **retval**: `bool`
  - **image**: `Evision.Mat.t()`.

  @return `false` if no frames has been grabbed
  The method/function combines VideoCapture::grab() and VideoCapture::retrieve() in one call. This is the
  most convenient method for reading video files or capturing data from decode and returns the just
  grabbed frame. If no frames has been grabbed (camera has been disconnected, or there are no more
  frames in video file), the method returns false and the function returns empty image (with %cv::Mat, test it with Mat::empty()).
  **Note**: In @ref videoio_c "C API", functions cvRetrieveFrame() and cv.RetrieveFrame() return image stored inside the video
  capturing structure. It is not allowed to modify or release the image! You can copy the frame using
  cvCloneImage and then do whatever you want with the copy.

  Python prototype (for reference only):
  ```python3
  read([, image]) -> retval, image
  ```
  """
  @spec read(Evision.VideoCapture.t(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def read(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    positional = [
    ]
    :evision_nif.videoCapture_read(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Grabs, decodes and returns the next video frame.

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`

  ##### Return
  - **retval**: `bool`
  - **image**: `Evision.Mat.t()`.

  @return `false` if no frames has been grabbed
  The method/function combines VideoCapture::grab() and VideoCapture::retrieve() in one call. This is the
  most convenient method for reading video files or capturing data from decode and returns the just
  grabbed frame. If no frames has been grabbed (camera has been disconnected, or there are no more
  frames in video file), the method returns false and the function returns empty image (with %cv::Mat, test it with Mat::empty()).
  **Note**: In @ref videoio_c "C API", functions cvRetrieveFrame() and cv.RetrieveFrame() return image stored inside the video
  capturing structure. It is not allowed to modify or release the image! You can copy the frame using
  cvCloneImage and then do whatever you want with the copy.

  Python prototype (for reference only):
  ```python3
  read([, image]) -> retval, image
  ```
  """
  @spec read(Evision.VideoCapture.t()) :: Evision.Mat.t() | false | {:error, String.t()}
  def read(self) do
    positional = [
    ]
    :evision_nif.videoCapture_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Closes video file or capturing device.

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`

  The method is automatically called by subsequent VideoCapture::open and by VideoCapture
  destructor.
  The C function also deallocates memory and clears \\*capture pointer.

  Python prototype (for reference only):
  ```python3
  release() -> None
  ```
  """
  @spec release(Evision.VideoCapture.t()) :: Evision.VideoCapture.t() | {:error, String.t()}
  def release(self) do
    positional = [
    ]
    :evision_nif.videoCapture_release(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Decodes and returns the grabbed video frame.

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`

  ##### Keyword Arguments
  - **flag**: `integer()`.

    it could be a frame index or a driver specific flag

  ##### Return
  - **retval**: `bool`
  - **image**: `Evision.Mat.t()`.

  @return `false` if no frames has been grabbed
  The method decodes and returns the just grabbed frame. If no frames has been grabbed
  (camera has been disconnected, or there are no more frames in video file), the method returns false
  and the function returns an empty image (with %cv::Mat, test it with Mat::empty()).
  @sa read()
  **Note**: In @ref videoio_c "C API", functions cvRetrieveFrame() and cv.RetrieveFrame() return image stored inside the video
  capturing structure. It is not allowed to modify or release the image! You can copy the frame using
  cvCloneImage and then do whatever you want with the copy.

  Python prototype (for reference only):
  ```python3
  retrieve([, image[, flag]]) -> retval, image
  ```
  """
  @spec retrieve(Evision.VideoCapture.t(), [{:flag, term()}] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def retrieve(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:flag])
    positional = [
    ]
    :evision_nif.videoCapture_retrieve(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Decodes and returns the grabbed video frame.

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`

  ##### Keyword Arguments
  - **flag**: `integer()`.

    it could be a frame index or a driver specific flag

  ##### Return
  - **retval**: `bool`
  - **image**: `Evision.Mat.t()`.

  @return `false` if no frames has been grabbed
  The method decodes and returns the just grabbed frame. If no frames has been grabbed
  (camera has been disconnected, or there are no more frames in video file), the method returns false
  and the function returns an empty image (with %cv::Mat, test it with Mat::empty()).
  @sa read()
  **Note**: In @ref videoio_c "C API", functions cvRetrieveFrame() and cv.RetrieveFrame() return image stored inside the video
  capturing structure. It is not allowed to modify or release the image! You can copy the frame using
  cvCloneImage and then do whatever you want with the copy.

  Python prototype (for reference only):
  ```python3
  retrieve([, image[, flag]]) -> retval, image
  ```
  """
  @spec retrieve(Evision.VideoCapture.t()) :: Evision.Mat.t() | false | {:error, String.t()}
  def retrieve(self) do
    positional = [
    ]
    :evision_nif.videoCapture_retrieve(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Sets a property in the VideoCapture.

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`
  - **propId**: `integer()`.

    Property identifier from cv::VideoCaptureProperties (eg. cv::CAP_PROP_POS_MSEC, cv::CAP_PROP_POS_FRAMES, ...)
    or one from @ref videoio_flags_others

  - **value**: `double`.

    Value of the property.

  ##### Return
  - **retval**: `bool`

  @return `true` if the property is supported by backend used by the VideoCapture instance.
  **Note**: Even if it returns `true` this doesn't ensure that the property
  value has been accepted by the capture device. See note in VideoCapture::get()

  Python prototype (for reference only):
  ```python3
  set(propId, value) -> retval
  ```
  """
  @spec set(Evision.VideoCapture.t(), integer(), number()) :: boolean() | {:error, String.t()}
  def set(self, propId, value) when is_integer(propId) and is_number(value)
  do
    positional = [
      propId: Evision.Internal.Structurise.from_struct(propId),
      value: Evision.Internal.Structurise.from_struct(value)
    ]
    :evision_nif.videoCapture_set(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setExceptionMode

  ##### Positional Arguments
  - **self**: `Evision.VideoCapture.t()`
  - **enable**: `bool`

  Switches exceptions mode
   methods raise exceptions if not successful instead of returning an error code

  Python prototype (for reference only):
  ```python3
  setExceptionMode(enable) -> None
  ```
  """
  @spec setExceptionMode(Evision.VideoCapture.t(), boolean()) :: Evision.VideoCapture.t() | {:error, String.t()}
  def setExceptionMode(self, enable) when is_boolean(enable)
  do
    positional = [
      enable: Evision.Internal.Structurise.from_struct(enable)
    ]
    :evision_nif.videoCapture_setExceptionMode(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
