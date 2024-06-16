defmodule Evision.Face do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Face` struct.

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
  def to_struct({:ok, %{class: Evision.Face, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Face, ref: ref}) do
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
  createFacemarkAAM
  ##### Return
  - **retval**: `Evision.Face.Facemark.t()`

  Python prototype (for reference only):
  ```python3
  createFacemarkAAM() -> retval
  ```
  """
  @spec createFacemarkAAM() :: Evision.Face.Facemark.t() | {:error, String.t()}
  def createFacemarkAAM() do
    positional = [
    ]
    :evision_nif.face_createFacemarkAAM(positional)
    |> to_struct()
  end

  @doc """
  createFacemarkKazemi
  ##### Return
  - **retval**: `Evision.Face.Facemark.t()`

  Python prototype (for reference only):
  ```python3
  createFacemarkKazemi() -> retval
  ```
  """
  @spec createFacemarkKazemi() :: Evision.Face.Facemark.t() | {:error, String.t()}
  def createFacemarkKazemi() do
    positional = [
    ]
    :evision_nif.face_createFacemarkKazemi(positional)
    |> to_struct()
  end

  @doc """
  createFacemarkLBF
  ##### Return
  - **retval**: `Evision.Face.Facemark.t()`

  Python prototype (for reference only):
  ```python3
  createFacemarkLBF() -> retval
  ```
  """
  @spec createFacemarkLBF() :: Evision.Face.Facemark.t() | {:error, String.t()}
  def createFacemarkLBF() do
    positional = [
    ]
    :evision_nif.face_createFacemarkLBF(positional)
    |> to_struct()
  end

  @doc """
  Utility to draw the detected facial landmark points

  ##### Positional Arguments
  - **points**: `Evision.Mat`.

    Contains the data of points which will be drawn.

  ##### Keyword Arguments
  - **color**: `Evision.scalar()`.

    The color of points in BGR format represented by cv::Scalar.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    The input image to be processed.

  <B>Example of usage</B>
  ```
  std::vector<Rect> faces;
  std::vector<std::vector<Point2f> > landmarks;
  facemark->getFaces(img, faces);
  facemark->fit(img, faces, landmarks);
  for(int j=0;j<rects.size();j++){
  face::drawFacemarks(frame, landmarks[j], Scalar(0,0,255));
  }
  ```

  Python prototype (for reference only):
  ```python3
  drawFacemarks(image, points[, color]) -> image
  ```
  """
  @spec drawFacemarks(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:color, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawFacemarks(image, points, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:color])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.face_drawFacemarks(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Utility to draw the detected facial landmark points

  ##### Positional Arguments
  - **points**: `Evision.Mat`.

    Contains the data of points which will be drawn.

  ##### Keyword Arguments
  - **color**: `Evision.scalar()`.

    The color of points in BGR format represented by cv::Scalar.

  ##### Return
  - **image**: `Evision.Mat.t()`.

    The input image to be processed.

  <B>Example of usage</B>
  ```
  std::vector<Rect> faces;
  std::vector<std::vector<Point2f> > landmarks;
  facemark->getFaces(img, faces);
  facemark->fit(img, faces, landmarks);
  for(int j=0;j<rects.size();j++){
  face::drawFacemarks(frame, landmarks[j], Scalar(0,0,255));
  }
  ```

  Python prototype (for reference only):
  ```python3
  drawFacemarks(image, points[, color]) -> image
  ```
  """
  @spec drawFacemarks(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def drawFacemarks(image, points) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (is_struct(points, Evision.Mat) or is_struct(points, Nx.Tensor) or is_number(points) or is_tuple(points))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      points: Evision.Internal.Structurise.from_struct(points)
    ]
    :evision_nif.face_drawFacemarks(positional)
    |> to_struct()
  end

  @doc """
  Default face detector
  This function is mainly utilized by the implementation of a Facemark Algorithm.
  End users are advised to use function Facemark::getFaces which can be manually defined
  and circumvented to the algorithm by Facemark::setFaceDetector.

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    The input image to be processed.

  - **face_cascade_name**: `String`

  ##### Return
  - **retval**: `bool`
  - **faces**: `Evision.Mat.t()`.

    Output of the function which represent region of interest of the detected faces.
    Each face is stored in cv::Rect container.

  <B>Example of usage</B>
  ```
  std::vector<cv::Rect> faces;
  CParams params("haarcascade_frontalface_alt.xml");
  cv::face::getFaces(frame, faces, &params);
  for(int j=0;j<faces.size();j++){
  cv::rectangle(frame, faces[j], cv::Scalar(255,0,255));
  }
  cv::imshow("detection", frame);
  ```

  Python prototype (for reference only):
  ```python3
  getFacesHAAR(image, face_cascade_name[, faces]) -> retval, faces
  ```
  """
  @spec getFacesHAAR(Evision.Mat.maybe_mat_in(), binary(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def getFacesHAAR(image, face_cascade_name, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_binary(face_cascade_name) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      face_cascade_name: Evision.Internal.Structurise.from_struct(face_cascade_name)
    ]
    :evision_nif.face_getFacesHAAR(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Default face detector
  This function is mainly utilized by the implementation of a Facemark Algorithm.
  End users are advised to use function Facemark::getFaces which can be manually defined
  and circumvented to the algorithm by Facemark::setFaceDetector.

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    The input image to be processed.

  - **face_cascade_name**: `String`

  ##### Return
  - **retval**: `bool`
  - **faces**: `Evision.Mat.t()`.

    Output of the function which represent region of interest of the detected faces.
    Each face is stored in cv::Rect container.

  <B>Example of usage</B>
  ```
  std::vector<cv::Rect> faces;
  CParams params("haarcascade_frontalface_alt.xml");
  cv::face::getFaces(frame, faces, &params);
  for(int j=0;j<faces.size();j++){
  cv::rectangle(frame, faces[j], cv::Scalar(255,0,255));
  }
  cv::imshow("detection", frame);
  ```

  Python prototype (for reference only):
  ```python3
  getFacesHAAR(image, face_cascade_name[, faces]) -> retval, faces
  ```
  """
  @spec getFacesHAAR(Evision.Mat.maybe_mat_in(), binary()) :: Evision.Mat.t() | false | {:error, String.t()}
  def getFacesHAAR(image, face_cascade_name) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and is_binary(face_cascade_name)
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image),
      face_cascade_name: Evision.Internal.Structurise.from_struct(face_cascade_name)
    ]
    :evision_nif.face_getFacesHAAR(positional)
    |> to_struct()
  end

  @doc """
  A utility to load list of paths to training image and annotation file.

  ##### Positional Arguments
  - **imageList**: `String`.

    The specified file contains paths to the training images.

  - **annotationList**: `String`.

    The specified file contains paths to the training annotations.

  - **images**: `[String]`.

    The loaded paths of training images.

  - **annotations**: `[String]`.

    The loaded paths of annotation files.

  ##### Return
  - **retval**: `bool`

  Example of usage:
  ```
  String imageFiles = "images_path.txt";
  String ptsFiles = "annotations_path.txt";
  std::vector<String> images_train;
  std::vector<String> landmarks_train;
  loadDatasetList(imageFiles,ptsFiles,images_train,landmarks_train);
  ```

  Python prototype (for reference only):
  ```python3
  loadDatasetList(imageList, annotationList, images, annotations) -> retval
  ```
  """
  @spec loadDatasetList(binary(), binary(), list(binary()), list(binary())) :: boolean() | {:error, String.t()}
  def loadDatasetList(imageList, annotationList, images, annotations) when is_binary(imageList) and is_binary(annotationList) and is_list(images) and is_list(annotations)
  do
    positional = [
      imageList: Evision.Internal.Structurise.from_struct(imageList),
      annotationList: Evision.Internal.Structurise.from_struct(annotationList),
      images: Evision.Internal.Structurise.from_struct(images),
      annotations: Evision.Internal.Structurise.from_struct(annotations)
    ]
    :evision_nif.face_loadDatasetList(positional)
    |> to_struct()
  end

  @doc """
  A utility to load facial landmark information from a given file.

  ##### Positional Arguments
  - **filename**: `String`.

    The filename of file contains the facial landmarks data.

  ##### Keyword Arguments
  - **offset**: `float`.

    An offset value to adjust the loaded points.

  ##### Return
  - **retval**: `bool`
  - **points**: `Evision.Mat.t()`.

    The loaded facial landmark points.

  <B>Example of usage</B>
  ```
  std::vector<Point2f> points;
  face::loadFacePoints("filename.txt", points, 0.0f);
  ```
  The annotation file should follow the default format which is
  ```
  version: 1
  n_points:  68
  {
  212.716603 499.771793
  230.232816 566.290071
  ...
  }
  ```
  where n_points is the number of points considered
  and each point is represented as its position in x and y.

  Python prototype (for reference only):
  ```python3
  loadFacePoints(filename[, points[, offset]]) -> retval, points
  ```
  """
  @spec loadFacePoints(binary(), [{:offset, term()}] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def loadFacePoints(filename, opts) when is_binary(filename) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:offset])
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.face_loadFacePoints(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  A utility to load facial landmark information from a given file.

  ##### Positional Arguments
  - **filename**: `String`.

    The filename of file contains the facial landmarks data.

  ##### Keyword Arguments
  - **offset**: `float`.

    An offset value to adjust the loaded points.

  ##### Return
  - **retval**: `bool`
  - **points**: `Evision.Mat.t()`.

    The loaded facial landmark points.

  <B>Example of usage</B>
  ```
  std::vector<Point2f> points;
  face::loadFacePoints("filename.txt", points, 0.0f);
  ```
  The annotation file should follow the default format which is
  ```
  version: 1
  n_points:  68
  {
  212.716603 499.771793
  230.232816 566.290071
  ...
  }
  ```
  where n_points is the number of points considered
  and each point is represented as its position in x and y.

  Python prototype (for reference only):
  ```python3
  loadFacePoints(filename[, points[, offset]]) -> retval, points
  ```
  """
  @spec loadFacePoints(binary()) :: Evision.Mat.t() | false | {:error, String.t()}
  def loadFacePoints(filename) when is_binary(filename)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename)
    ]
    :evision_nif.face_loadFacePoints(positional)
    |> to_struct()
  end

  @doc """
  #### Variant 1:
  This function extracts the data for training from .txt files which contains the corresponding image name and landmarks.
  The first file in each file should give the path of the image whose
  landmarks are being described in the file. Then in the subsequent
  lines there should be coordinates of the landmarks in the image
  i.e each line should be of the form x,y
  where x represents the x coordinate of the landmark and y represents
  the y coordinate of the landmark.

  ##### Positional Arguments
  - **filename**: `[String]`.

    A vector of type cv::String containing name of the .txt files.

  - **trainlandmarks**: `[[Point2f]]`.

    A vector of type cv::Point2f that would store shape or landmarks of all images.

  - **trainimages**: `[String]`.

    A vector of type cv::String which stores the name of images whose landmarks are tracked

  ##### Return
  - **retval**: `bool`

  For reference you can see the files as provided in the
  <a href="http://www.ifp.illinois.edu/~vuongle2/helen/">HELEN dataset</a>
  @returns A boolean value. It returns true when it reads the data successfully and false otherwise

  Python prototype (for reference only):
  ```python3
  loadTrainingData(filename, trainlandmarks, trainimages) -> retval
  ```
  #### Variant 2:
  A utility to load facial landmark information from the dataset.

  ##### Positional Arguments
  - **imageList**: `String`.

    A file contains the list of image filenames in the training dataset.

  - **groundTruth**: `String`.

    A file contains the list of filenames
    where the landmarks points information are stored.
    The content in each file should follow the standard format (see face::loadFacePoints).

  - **images**: `[String]`.

    A vector where each element represent the filename of image in the dataset.
    Images are not loaded by default to save the memory.

  ##### Keyword Arguments
  - **offset**: `float`.

    An offset value to adjust the loaded points.

  ##### Return
  - **retval**: `bool`
  - **facePoints**: `Evision.Mat.t()`.

    The loaded landmark points for all training data.

  <B>Example of usage</B>
  ```
  cv::String imageFiles = "../data/images_train.txt";
  cv::String ptsFiles = "../data/points_train.txt";
  std::vector<String> images;
  std::vector<std::vector<Point2f> > facePoints;
  loadTrainingData(imageFiles, ptsFiles, images, facePoints, 0.0f);
  ```
  example of content in the images_train.txt
  ```
  /home/user/ibug/image_003_1.jpg
  /home/user/ibug/image_004_1.jpg
  /home/user/ibug/image_005_1.jpg
  /home/user/ibug/image_006.jpg
  ```
  example of content in the points_train.txt
  ```
  /home/user/ibug/image_003_1.pts
  /home/user/ibug/image_004_1.pts
  /home/user/ibug/image_005_1.pts
  /home/user/ibug/image_006.pts
  ```

  Python prototype (for reference only):
  ```python3
  loadTrainingData(imageList, groundTruth, images[, facePoints[, offset]]) -> retval, facePoints
  ```
  #### Variant 3:
  A utility to load facial landmark dataset from a single file.

  ##### Positional Arguments
  - **filename**: `String`.

    The filename of a file that contains the dataset information.
    Each line contains the filename of an image followed by
    pairs of x and y values of facial landmarks points separated by a space.
    Example

  - **images**: `[String]`.

    A vector where each element represent the filename of image in the dataset.
    Images are not loaded by default to save the memory.

  ##### Keyword Arguments
  - **delim**: `char`.

    Delimiter between each element, the default value is a whitespace.

  - **offset**: `float`.

    An offset value to adjust the loaded points.

  ##### Return
  - **retval**: `bool`
  - **facePoints**: `Evision.Mat.t()`.

    The loaded landmark points for all training data.

  ```
  /home/user/ibug/image_003_1.jpg 336.820955 240.864510 334.238298 260.922709 335.266918 ...
  /home/user/ibug/image_005_1.jpg 376.158428 230.845712 376.736984 254.924635 383.265403 ...
  ```

  <B>Example of usage</B>
  ```
  cv::String imageFiles = "../data/images_train.txt";
  cv::String ptsFiles = "../data/points_train.txt";
  std::vector<String> images;
  std::vector<std::vector<Point2f> > facePoints;
  loadTrainingData(imageFiles, ptsFiles, images, facePoints, 0.0f);
  ```

  Python prototype (for reference only):
  ```python3
  loadTrainingData(filename, images[, facePoints[, delim[, offset]]]) -> retval, facePoints
  ```

  """
  @spec loadTrainingData(binary(), list(binary()), [{:delim, term()} | {:offset, term()}] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def loadTrainingData(filename, images, opts) when is_binary(filename) and is_list(images) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:delim, :offset])
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename),
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.face_loadTrainingData(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec loadTrainingData(list(binary()), list(list({number(), number()})), list(binary())) :: boolean() | {:error, String.t()}
  def loadTrainingData(filename, trainlandmarks, trainimages) when is_list(filename) and is_list(trainlandmarks) and is_list(trainimages)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename),
      trainlandmarks: Evision.Internal.Structurise.from_struct(trainlandmarks),
      trainimages: Evision.Internal.Structurise.from_struct(trainimages)
    ]
    :evision_nif.face_loadTrainingData(positional)
    |> to_struct()
  end
  @spec loadTrainingData(binary(), binary(), list(binary())) :: Evision.Mat.t() | false | {:error, String.t()}
  def loadTrainingData(imageList, groundTruth, images) when is_binary(imageList) and is_binary(groundTruth) and is_list(images)
  do
    positional = [
      imageList: Evision.Internal.Structurise.from_struct(imageList),
      groundTruth: Evision.Internal.Structurise.from_struct(groundTruth),
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.face_loadTrainingData(positional)
    |> to_struct()
  end

  @doc """
  A utility to load facial landmark information from the dataset.

  ##### Positional Arguments
  - **imageList**: `String`.

    A file contains the list of image filenames in the training dataset.

  - **groundTruth**: `String`.

    A file contains the list of filenames
    where the landmarks points information are stored.
    The content in each file should follow the standard format (see face::loadFacePoints).

  - **images**: `[String]`.

    A vector where each element represent the filename of image in the dataset.
    Images are not loaded by default to save the memory.

  ##### Keyword Arguments
  - **offset**: `float`.

    An offset value to adjust the loaded points.

  ##### Return
  - **retval**: `bool`
  - **facePoints**: `Evision.Mat.t()`.

    The loaded landmark points for all training data.

  <B>Example of usage</B>
  ```
  cv::String imageFiles = "../data/images_train.txt";
  cv::String ptsFiles = "../data/points_train.txt";
  std::vector<String> images;
  std::vector<std::vector<Point2f> > facePoints;
  loadTrainingData(imageFiles, ptsFiles, images, facePoints, 0.0f);
  ```
  example of content in the images_train.txt
  ```
  /home/user/ibug/image_003_1.jpg
  /home/user/ibug/image_004_1.jpg
  /home/user/ibug/image_005_1.jpg
  /home/user/ibug/image_006.jpg
  ```
  example of content in the points_train.txt
  ```
  /home/user/ibug/image_003_1.pts
  /home/user/ibug/image_004_1.pts
  /home/user/ibug/image_005_1.pts
  /home/user/ibug/image_006.pts
  ```

  Python prototype (for reference only):
  ```python3
  loadTrainingData(imageList, groundTruth, images[, facePoints[, offset]]) -> retval, facePoints
  ```
  """
  @spec loadTrainingData(binary(), binary(), list(binary()), [{:offset, term()}] | nil) :: Evision.Mat.t() | false | {:error, String.t()}
  def loadTrainingData(imageList, groundTruth, images, opts) when is_binary(imageList) and is_binary(groundTruth) and is_list(images) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:offset])
    positional = [
      imageList: Evision.Internal.Structurise.from_struct(imageList),
      groundTruth: Evision.Internal.Structurise.from_struct(groundTruth),
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.face_loadTrainingData(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  A utility to load facial landmark dataset from a single file.

  ##### Positional Arguments
  - **filename**: `String`.

    The filename of a file that contains the dataset information.
    Each line contains the filename of an image followed by
    pairs of x and y values of facial landmarks points separated by a space.
    Example

  - **images**: `[String]`.

    A vector where each element represent the filename of image in the dataset.
    Images are not loaded by default to save the memory.

  ##### Keyword Arguments
  - **delim**: `char`.

    Delimiter between each element, the default value is a whitespace.

  - **offset**: `float`.

    An offset value to adjust the loaded points.

  ##### Return
  - **retval**: `bool`
  - **facePoints**: `Evision.Mat.t()`.

    The loaded landmark points for all training data.

  ```
  /home/user/ibug/image_003_1.jpg 336.820955 240.864510 334.238298 260.922709 335.266918 ...
  /home/user/ibug/image_005_1.jpg 376.158428 230.845712 376.736984 254.924635 383.265403 ...
  ```

  <B>Example of usage</B>
  ```
  cv::String imageFiles = "../data/images_train.txt";
  cv::String ptsFiles = "../data/points_train.txt";
  std::vector<String> images;
  std::vector<std::vector<Point2f> > facePoints;
  loadTrainingData(imageFiles, ptsFiles, images, facePoints, 0.0f);
  ```

  Python prototype (for reference only):
  ```python3
  loadTrainingData(filename, images[, facePoints[, delim[, offset]]]) -> retval, facePoints
  ```
  """
  @spec loadTrainingData(binary(), list(binary())) :: Evision.Mat.t() | false | {:error, String.t()}
  def loadTrainingData(filename, images) when is_binary(filename) and is_list(images)
  do
    positional = [
      filename: Evision.Internal.Structurise.from_struct(filename),
      images: Evision.Internal.Structurise.from_struct(images)
    ]
    :evision_nif.face_loadTrainingData(positional)
    |> to_struct()
  end
end
