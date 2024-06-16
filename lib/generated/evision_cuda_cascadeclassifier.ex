defmodule Evision.CUDA.CascadeClassifier do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDA.CascadeClassifier` struct.

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
  def to_struct({:ok, %{class: Evision.CUDA.CascadeClassifier, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDA.CascadeClassifier, ref: ref}) do
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
  - **self**: `Evision.CUDA.CascadeClassifier.t()`

  Python prototype (for reference only):
  ```python3
  clear() -> None
  ```
  """
  @spec clear(Evision.CUDA.CascadeClassifier.t()) :: Evision.CUDA.CascadeClassifier.t() | {:error, String.t()}
  def clear(self) do
    positional = [
    ]
    :evision_nif.cuda_CascadeClassifier_clear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Converts objects array from internal representation to standard vector.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **objects**: `[Rect]`.

    Resulting array.

  ##### Return
  - **gpu_objects**: `Evision.Mat.t()`.

    Objects array in internal representation.

  Python prototype (for reference only):
  ```python3
  convert(objects[, gpu_objects]) -> gpu_objects
  ```
  """
  @spec convert(Evision.CUDA.CascadeClassifier.t(), list({number(), number(), number(), number()}), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def convert(self, objects, opts) when is_list(objects) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      objects: Evision.Internal.Structurise.from_struct(objects)
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_convert(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Converts objects array from internal representation to standard vector.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **objects**: `[Rect]`.

    Resulting array.

  ##### Return
  - **gpu_objects**: `Evision.Mat.t()`.

    Objects array in internal representation.

  Python prototype (for reference only):
  ```python3
  convert(objects[, gpu_objects]) -> gpu_objects
  ```
  """
  @spec convert(Evision.CUDA.CascadeClassifier.t(), list({number(), number(), number(), number()})) :: Evision.Mat.t() | {:error, String.t()}
  def convert(self, objects) when is_list(objects)
  do
    positional = [
      objects: Evision.Internal.Structurise.from_struct(objects)
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_convert(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Loads the classifier from a file. Cascade type is detected automatically by constructor parameter.

  ##### Positional Arguments
  - **filename**: `String`.

    Name of the file from which the classifier is loaded. Only the old haar classifier
    (trained by the haar training application) and NVIDIA's nvbin are supported for HAAR and only new
    type of OpenCV XML cascade supported for LBP. The working haar models can be found at opencv_folder/data/haarcascades_cuda/

  ##### Return
  - **retval**: `Evision.CUDA.CascadeClassifier.t()`

  Python prototype (for reference only):
  ```python3
  create(filename) -> retval
  ```
  """
  @spec create(binary()) :: Evision.CUDA.CascadeClassifier.t() | {:error, String.t()}
  def create(filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_create_static(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  Detects objects of different sizes in the input image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **image**: `Evision.Mat`.

    Matrix of type CV_8U containing an image where objects should be detected.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **objects**: `Evision.Mat.t()`.

    Buffer to store detected objects (rectangles).

  To get final array of detected objects use CascadeClassifier::convert method.
  ```
  Ptr<cuda::CascadeClassifier> cascade_gpu = cuda::CascadeClassifier::create(...);
  Mat image_cpu = imread(...)
  GpuMat image_gpu(image_cpu);
  GpuMat objbuf;
  cascade_gpu->detectMultiScale(image_gpu, objbuf);
  std::vector<Rect> faces;
  cascade_gpu->convert(objbuf, faces);
  for(int i = 0; i < detections_num; ++i)
  cv::rectangle(image_cpu, faces[i], Scalar(255));
  imshow("Faces", image_cpu);
  ```
  @sa CascadeClassifier::detectMultiScale

  Python prototype (for reference only):
  ```python3
  detectMultiScale(image[, objects[, stream]]) -> objects
  ```
  #### Variant 2:
  Detects objects of different sizes in the input image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`.

    Matrix of type CV_8U containing an image where objects should be detected.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **objects**: `Evision.CUDA.GpuMat.t()`.

    Buffer to store detected objects (rectangles).

  To get final array of detected objects use CascadeClassifier::convert method.
  ```
  Ptr<cuda::CascadeClassifier> cascade_gpu = cuda::CascadeClassifier::create(...);
  Mat image_cpu = imread(...)
  GpuMat image_gpu(image_cpu);
  GpuMat objbuf;
  cascade_gpu->detectMultiScale(image_gpu, objbuf);
  std::vector<Rect> faces;
  cascade_gpu->convert(objbuf, faces);
  for(int i = 0; i < detections_num; ++i)
  cv::rectangle(image_cpu, faces[i], Scalar(255));
  imshow("Faces", image_cpu);
  ```
  @sa CascadeClassifier::detectMultiScale

  Python prototype (for reference only):
  ```python3
  detectMultiScale(image[, objects[, stream]]) -> objects
  ```

  """
  @spec detectMultiScale(Evision.CUDA.CascadeClassifier.t(), Evision.Mat.maybe_mat_in(), [{:stream, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def detectMultiScale(self, image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_detectMultiScale(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec detectMultiScale(Evision.CUDA.CascadeClassifier.t(), Evision.CUDA.GpuMat.t(), [{:stream, term()}] | nil) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detectMultiScale(self, image, opts) when is_struct(image, Evision.CUDA.GpuMat) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:stream])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_detectMultiScale(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Detects objects of different sizes in the input image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **image**: `Evision.Mat`.

    Matrix of type CV_8U containing an image where objects should be detected.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **objects**: `Evision.Mat.t()`.

    Buffer to store detected objects (rectangles).

  To get final array of detected objects use CascadeClassifier::convert method.
  ```
  Ptr<cuda::CascadeClassifier> cascade_gpu = cuda::CascadeClassifier::create(...);
  Mat image_cpu = imread(...)
  GpuMat image_gpu(image_cpu);
  GpuMat objbuf;
  cascade_gpu->detectMultiScale(image_gpu, objbuf);
  std::vector<Rect> faces;
  cascade_gpu->convert(objbuf, faces);
  for(int i = 0; i < detections_num; ++i)
  cv::rectangle(image_cpu, faces[i], Scalar(255));
  imshow("Faces", image_cpu);
  ```
  @sa CascadeClassifier::detectMultiScale

  Python prototype (for reference only):
  ```python3
  detectMultiScale(image[, objects[, stream]]) -> objects
  ```
  #### Variant 2:
  Detects objects of different sizes in the input image.

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **image**: `Evision.CUDA.GpuMat.t()`.

    Matrix of type CV_8U containing an image where objects should be detected.

  ##### Keyword Arguments
  - **stream**: `Evision.CUDA.Stream.t()`.

    CUDA stream.

  ##### Return
  - **objects**: `Evision.CUDA.GpuMat.t()`.

    Buffer to store detected objects (rectangles).

  To get final array of detected objects use CascadeClassifier::convert method.
  ```
  Ptr<cuda::CascadeClassifier> cascade_gpu = cuda::CascadeClassifier::create(...);
  Mat image_cpu = imread(...)
  GpuMat image_gpu(image_cpu);
  GpuMat objbuf;
  cascade_gpu->detectMultiScale(image_gpu, objbuf);
  std::vector<Rect> faces;
  cascade_gpu->convert(objbuf, faces);
  for(int i = 0; i < detections_num; ++i)
  cv::rectangle(image_cpu, faces[i], Scalar(255));
  imshow("Faces", image_cpu);
  ```
  @sa CascadeClassifier::detectMultiScale

  Python prototype (for reference only):
  ```python3
  detectMultiScale(image[, objects[, stream]]) -> objects
  ```

  """
  @spec detectMultiScale(Evision.CUDA.CascadeClassifier.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def detectMultiScale(self, image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_detectMultiScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec detectMultiScale(Evision.CUDA.CascadeClassifier.t(), Evision.CUDA.GpuMat.t()) :: Evision.CUDA.GpuMat.t() | {:error, String.t()}
  def detectMultiScale(self, image) when is_struct(image, Evision.CUDA.GpuMat)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_detectMultiScale(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Returns true if the Algorithm is empty (e.g. in the very beginning or after unsuccessful read

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  empty() -> retval
  ```
  """
  @spec empty(Evision.CUDA.CascadeClassifier.t()) :: boolean() | {:error, String.t()}
  def empty(self) do
    positional = [
    ]
    :evision_nif.cuda_CascadeClassifier_empty(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getClassifierSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  getClassifierSize() -> retval
  ```
  """
  @spec getClassifierSize(Evision.CUDA.CascadeClassifier.t()) :: {number(), number()} | {:error, String.t()}
  def getClassifierSize(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_getClassifierSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getDefaultName

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`

  ##### Return
  - **retval**: `String`

  Returns the algorithm string identifier.
  This string is used as top level xml/yml node tag when the object is saved to a file or string.

  Python prototype (for reference only):
  ```python3
  getDefaultName() -> retval
  ```
  """
  @spec getDefaultName(Evision.CUDA.CascadeClassifier.t()) :: binary() | {:error, String.t()}
  def getDefaultName(self) do
    positional = [
    ]
    :evision_nif.cuda_CascadeClassifier_getDefaultName(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getFindLargestObject

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`

  ##### Return
  - **retval**: `bool`

  Python prototype (for reference only):
  ```python3
  getFindLargestObject() -> retval
  ```
  """
  @spec getFindLargestObject(Evision.CUDA.CascadeClassifier.t()) :: boolean() | {:error, String.t()}
  def getFindLargestObject(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_getFindLargestObject(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxNumObjects

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMaxNumObjects() -> retval
  ```
  """
  @spec getMaxNumObjects(Evision.CUDA.CascadeClassifier.t()) :: integer() | {:error, String.t()}
  def getMaxNumObjects(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_getMaxNumObjects(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMaxObjectSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  getMaxObjectSize() -> retval
  ```
  """
  @spec getMaxObjectSize(Evision.CUDA.CascadeClassifier.t()) :: {number(), number()} | {:error, String.t()}
  def getMaxObjectSize(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_getMaxObjectSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinNeighbors

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  getMinNeighbors() -> retval
  ```
  """
  @spec getMinNeighbors(Evision.CUDA.CascadeClassifier.t()) :: integer() | {:error, String.t()}
  def getMinNeighbors(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_getMinNeighbors(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMinObjectSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`

  ##### Return
  - **retval**: `Size`

  Python prototype (for reference only):
  ```python3
  getMinObjectSize() -> retval
  ```
  """
  @spec getMinObjectSize(Evision.CUDA.CascadeClassifier.t()) :: {number(), number()} | {:error, String.t()}
  def getMinObjectSize(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_getMinObjectSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getScaleFactor

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getScaleFactor() -> retval
  ```
  """
  @spec getScaleFactor(Evision.CUDA.CascadeClassifier.t()) :: number() | {:error, String.t()}
  def getScaleFactor(self) do
    positional = [
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_getScaleFactor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Reads algorithm parameters from a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.CUDA.CascadeClassifier.t(), Evision.FileNode.t()) :: Evision.CUDA.CascadeClassifier.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.cuda_CascadeClassifier_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  save

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **filename**: `String`

  Saves the algorithm to a file.
  In order to make this method work, the derived class must implement Algorithm::write(FileStorage& fs).

  Python prototype (for reference only):
  ```python3
  save(filename) -> None
  ```
  """
  @spec save(Evision.CUDA.CascadeClassifier.t(), binary()) :: Evision.CUDA.CascadeClassifier.t() | {:error, String.t()}
  def save(self, filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.cuda_CascadeClassifier_save(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setFindLargestObject

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **findLargestObject**: `bool`

  Python prototype (for reference only):
  ```python3
  setFindLargestObject(findLargestObject) -> None
  ```
  """
  @spec setFindLargestObject(Evision.CUDA.CascadeClassifier.t(), boolean()) :: Evision.CUDA.CascadeClassifier.t() | {:error, String.t()}
  def setFindLargestObject(self, findLargestObject) when is_boolean(findLargestObject)
  do
    positional = [
      findLargestObject: Evision.Internal.Structurise.from_struct(findLargestObject)
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_setFindLargestObject(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxNumObjects

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **maxNumObjects**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMaxNumObjects(maxNumObjects) -> None
  ```
  """
  @spec setMaxNumObjects(Evision.CUDA.CascadeClassifier.t(), integer()) :: Evision.CUDA.CascadeClassifier.t() | {:error, String.t()}
  def setMaxNumObjects(self, maxNumObjects) when is_integer(maxNumObjects)
  do
    positional = [
      maxNumObjects: Evision.Internal.Structurise.from_struct(maxNumObjects)
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_setMaxNumObjects(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMaxObjectSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **maxObjectSize**: `Size`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  setMaxObjectSize(maxObjectSize) -> None
  ```
  """
  @spec setMaxObjectSize(Evision.CUDA.CascadeClassifier.t(), {number(), number()}) :: Evision.CUDA.CascadeClassifier.t() | {:error, String.t()}
  def setMaxObjectSize(self, maxObjectSize) when is_tuple(maxObjectSize)
  do
    positional = [
      maxObjectSize: Evision.Internal.Structurise.from_struct(maxObjectSize)
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_setMaxObjectSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinNeighbors

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **minNeighbors**: `integer()`

  Python prototype (for reference only):
  ```python3
  setMinNeighbors(minNeighbors) -> None
  ```
  """
  @spec setMinNeighbors(Evision.CUDA.CascadeClassifier.t(), integer()) :: Evision.CUDA.CascadeClassifier.t() | {:error, String.t()}
  def setMinNeighbors(self, minNeighbors) when is_integer(minNeighbors)
  do
    positional = [
      minNeighbors: Evision.Internal.Structurise.from_struct(minNeighbors)
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_setMinNeighbors(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setMinObjectSize

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **minSize**: `Size`

  Python prototype (for reference only):
  ```python3
  setMinObjectSize(minSize) -> None
  ```
  """
  @spec setMinObjectSize(Evision.CUDA.CascadeClassifier.t(), {number(), number()}) :: Evision.CUDA.CascadeClassifier.t() | {:error, String.t()}
  def setMinObjectSize(self, minSize) when is_tuple(minSize)
  do
    positional = [
      minSize: Evision.Internal.Structurise.from_struct(minSize)
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_setMinObjectSize(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  setScaleFactor

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **scaleFactor**: `double`

  Python prototype (for reference only):
  ```python3
  setScaleFactor(scaleFactor) -> None
  ```
  """
  @spec setScaleFactor(Evision.CUDA.CascadeClassifier.t(), number()) :: Evision.CUDA.CascadeClassifier.t() | {:error, String.t()}
  def setScaleFactor(self, scaleFactor) when is_number(scaleFactor)
  do
    positional = [
      scaleFactor: Evision.Internal.Structurise.from_struct(scaleFactor)
    ]
    :evision_nif.cuda_cuda_CascadeClassifier_setScaleFactor(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  write

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **fs**: `Evision.FileStorage`
  - **name**: `String`

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  write(fs, name) -> None
  ```
  """
  @spec write(Evision.CUDA.CascadeClassifier.t(), Evision.FileStorage.t(), binary()) :: Evision.CUDA.CascadeClassifier.t() | {:error, String.t()}
  def write(self, fs, name) when is_struct(fs, Evision.FileStorage) and is_binary(name)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs),
      name: Evision.Internal.Structurise.from_struct(name)
    ]
    :evision_nif.cuda_CascadeClassifier_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Stores algorithm parameters in a file storage

  ##### Positional Arguments
  - **self**: `Evision.CUDA.CascadeClassifier.t()`
  - **fs**: `Evision.FileStorage`

  Python prototype (for reference only):
  ```python3
  write(fs) -> None
  ```
  """
  @spec write(Evision.CUDA.CascadeClassifier.t(), Evision.FileStorage.t()) :: Evision.CUDA.CascadeClassifier.t() | {:error, String.t()}
  def write(self, fs) when is_struct(fs, Evision.FileStorage)
  do
    positional = [
      fs: Evision.Internal.Structurise.from_struct(fs)
    ]
    :evision_nif.cuda_CascadeClassifier_write(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
