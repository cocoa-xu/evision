defmodule Evision.Rapid do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Rapid` struct.

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
  def to_struct({:ok, %{class: Evision.Rapid, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Rapid, ref: ref}) do
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
  convertCorrespondencies

  ##### Positional Arguments
  - **cols**: `Evision.Mat`.

    correspondence-position per line in line-bundle-space

  - **srcLocations**: `Evision.Mat`.

    the source image location

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    mask containing non-zero values for the elements to be retained

  ##### Return
  - **pts2d**: `Evision.Mat.t()`.

    2d points

  - **pts3d**: `Evision.Mat.t()`.

    3d points

   Collect corresponding 2d and 3d points based on correspondencies and mask

  Python prototype (for reference only):
  ```python3
  convertCorrespondencies(cols, srcLocations[, pts2d[, pts3d[, mask]]]) -> pts2d, pts3d
  ```
  """
  @spec convertCorrespondencies(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:mask, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def convertCorrespondencies(cols, srcLocations, opts) when (is_struct(cols, Evision.Mat) or is_struct(cols, Nx.Tensor) or is_number(cols) or is_tuple(cols)) and (is_struct(srcLocations, Evision.Mat) or is_struct(srcLocations, Nx.Tensor) or is_number(srcLocations) or is_tuple(srcLocations)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask])
    positional = [
      cols: Evision.Internal.Structurise.from_struct(cols),
      srcLocations: Evision.Internal.Structurise.from_struct(srcLocations)
    ]
    :evision_nif.rapid_convertCorrespondencies(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  convertCorrespondencies

  ##### Positional Arguments
  - **cols**: `Evision.Mat`.

    correspondence-position per line in line-bundle-space

  - **srcLocations**: `Evision.Mat`.

    the source image location

  ##### Keyword Arguments
  - **mask**: `Evision.Mat`.

    mask containing non-zero values for the elements to be retained

  ##### Return
  - **pts2d**: `Evision.Mat.t()`.

    2d points

  - **pts3d**: `Evision.Mat.t()`.

    3d points

   Collect corresponding 2d and 3d points based on correspondencies and mask

  Python prototype (for reference only):
  ```python3
  convertCorrespondencies(cols, srcLocations[, pts2d[, pts3d[, mask]]]) -> pts2d, pts3d
  ```
  """
  @spec convertCorrespondencies(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def convertCorrespondencies(cols, srcLocations) when (is_struct(cols, Evision.Mat) or is_struct(cols, Nx.Tensor) or is_number(cols) or is_tuple(cols)) and (is_struct(srcLocations, Evision.Mat) or is_struct(srcLocations, Nx.Tensor) or is_number(srcLocations) or is_tuple(srcLocations))
  do
    positional = [
      cols: Evision.Internal.Structurise.from_struct(cols),
      srcLocations: Evision.Internal.Structurise.from_struct(srcLocations)
    ]
    :evision_nif.rapid_convertCorrespondencies(positional)
    |> to_struct()
  end

  @doc """
  drawCorrespondencies

  ##### Positional Arguments
  - **cols**: `Evision.Mat`.

    column coordinates in the line bundle

  ##### Keyword Arguments
  - **colors**: `Evision.Mat`.

    colors for the markers. Defaults to white.

  ##### Return
  - **bundle**: `Evision.Mat.t()`.

    the lineBundle

   Debug draw markers of matched correspondences onto a lineBundle

  Python prototype (for reference only):
  ```python3
  drawCorrespondencies(bundle, cols[, colors]) -> bundle
  ```
  """
  @spec drawCorrespondencies(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:colors, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawCorrespondencies(bundle, cols, opts) when (is_struct(bundle, Evision.Mat) or is_struct(bundle, Nx.Tensor) or is_number(bundle) or is_tuple(bundle)) and (is_struct(cols, Evision.Mat) or is_struct(cols, Nx.Tensor) or is_number(cols) or is_tuple(cols)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:colors])
    positional = [
      bundle: Evision.Internal.Structurise.from_struct(bundle),
      cols: Evision.Internal.Structurise.from_struct(cols)
    ]
    :evision_nif.rapid_drawCorrespondencies(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  drawCorrespondencies

  ##### Positional Arguments
  - **cols**: `Evision.Mat`.

    column coordinates in the line bundle

  ##### Keyword Arguments
  - **colors**: `Evision.Mat`.

    colors for the markers. Defaults to white.

  ##### Return
  - **bundle**: `Evision.Mat.t()`.

    the lineBundle

   Debug draw markers of matched correspondences onto a lineBundle

  Python prototype (for reference only):
  ```python3
  drawCorrespondencies(bundle, cols[, colors]) -> bundle
  ```
  """
  @spec drawCorrespondencies(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def drawCorrespondencies(bundle, cols) when (is_struct(bundle, Evision.Mat) or is_struct(bundle, Nx.Tensor) or is_number(bundle) or is_tuple(bundle)) and (is_struct(cols, Evision.Mat) or is_struct(cols, Nx.Tensor) or is_number(cols) or is_tuple(cols))
  do
    positional = [
      bundle: Evision.Internal.Structurise.from_struct(bundle),
      cols: Evision.Internal.Structurise.from_struct(cols)
    ]
    :evision_nif.rapid_drawCorrespondencies(positional)
    |> to_struct()
  end

  @doc """
  drawSearchLines

  ##### Positional Arguments
  - **locations**: `Evision.Mat`.

    the source locations of a line bundle

  - **color**: `Evision.scalar()`.

    the line color

  ##### Return
  - **img**: `Evision.Mat.t()`.

    the output image

   Debug draw search lines onto an image

  Python prototype (for reference only):
  ```python3
  drawSearchLines(img, locations, color) -> img
  ```
  """
  @spec drawSearchLines(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.scalar()) :: Evision.Mat.t() | {:error, String.t()}
  def drawSearchLines(img, locations, color) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(locations, Evision.Mat) or is_struct(locations, Nx.Tensor) or is_number(locations) or is_tuple(locations)) and (is_number(color) or is_tuple(color))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      locations: Evision.Internal.Structurise.from_struct(locations),
      color: Evision.Internal.Structurise.from_struct(color)
    ]
    :evision_nif.rapid_drawSearchLines(positional)
    |> to_struct()
  end

  @doc """
  drawWireframe

  ##### Positional Arguments
  - **pts2d**: `Evision.Mat`.

    the 2d points obtained by @ref projectPoints

  - **tris**: `Evision.Mat`.

    triangle face connectivity

  - **color**: `Evision.scalar()`.

    line color

  ##### Keyword Arguments
  - **type**: `integer()`.

    line type. See @ref LineTypes.

  - **cullBackface**: `bool`.

    enable back-face culling based on CCW order

  ##### Return
  - **img**: `Evision.Mat.t()`.

    the output image

   Draw a wireframe of a triangle mesh

  Python prototype (for reference only):
  ```python3
  drawWireframe(img, pts2d, tris, color[, type[, cullBackface]]) -> img
  ```
  """
  @spec drawWireframe(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.scalar(), [{:cullBackface, term()} | {:type, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def drawWireframe(img, pts2d, tris, color, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(pts2d, Evision.Mat) or is_struct(pts2d, Nx.Tensor) or is_number(pts2d) or is_tuple(pts2d)) and (is_struct(tris, Evision.Mat) or is_struct(tris, Nx.Tensor) or is_number(tris) or is_tuple(tris)) and (is_number(color) or is_tuple(color)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:cullBackface, :type])
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      pts2d: Evision.Internal.Structurise.from_struct(pts2d),
      tris: Evision.Internal.Structurise.from_struct(tris),
      color: Evision.Internal.Structurise.from_struct(color)
    ]
    :evision_nif.rapid_drawWireframe(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  drawWireframe

  ##### Positional Arguments
  - **pts2d**: `Evision.Mat`.

    the 2d points obtained by @ref projectPoints

  - **tris**: `Evision.Mat`.

    triangle face connectivity

  - **color**: `Evision.scalar()`.

    line color

  ##### Keyword Arguments
  - **type**: `integer()`.

    line type. See @ref LineTypes.

  - **cullBackface**: `bool`.

    enable back-face culling based on CCW order

  ##### Return
  - **img**: `Evision.Mat.t()`.

    the output image

   Draw a wireframe of a triangle mesh

  Python prototype (for reference only):
  ```python3
  drawWireframe(img, pts2d, tris, color[, type[, cullBackface]]) -> img
  ```
  """
  @spec drawWireframe(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.scalar()) :: Evision.Mat.t() | {:error, String.t()}
  def drawWireframe(img, pts2d, tris, color) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(pts2d, Evision.Mat) or is_struct(pts2d, Nx.Tensor) or is_number(pts2d) or is_tuple(pts2d)) and (is_struct(tris, Evision.Mat) or is_struct(tris, Nx.Tensor) or is_number(tris) or is_tuple(tris)) and (is_number(color) or is_tuple(color))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      pts2d: Evision.Internal.Structurise.from_struct(pts2d),
      tris: Evision.Internal.Structurise.from_struct(tris),
      color: Evision.Internal.Structurise.from_struct(color)
    ]
    :evision_nif.rapid_drawWireframe(positional)
    |> to_struct()
  end

  @doc """
  extractControlPoints

  ##### Positional Arguments
  - **num**: `integer()`.

    number of control points

  - **len**: `integer()`.

    search radius (used to restrict the ROI)

  - **pts3d**: `Evision.Mat`.

    the 3D points of the mesh

  - **rvec**: `Evision.Mat`.

    rotation between mesh and camera

  - **tvec**: `Evision.Mat`.

    translation between mesh and camera

  - **k**: `Evision.Mat`.

    camera intrinsic

  - **imsize**: `Size`.

    size of the video frame

  - **tris**: `Evision.Mat`.

    triangle face connectivity

  ##### Return
  - **ctl2d**: `Evision.Mat.t()`.

    the 2D locations of the control points

  - **ctl3d**: `Evision.Mat.t()`.

    matching 3D points of the mesh

   Extract control points from the projected silhouette of a mesh
   see @cite drummond2002real Sec 2.1, Step b

  Python prototype (for reference only):
  ```python3
  extractControlPoints(num, len, pts3d, rvec, tvec, K, imsize, tris[, ctl2d[, ctl3d]]) -> ctl2d, ctl3d
  ```
  """
  @spec extractControlPoints(integer(), integer(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def extractControlPoints(num, len, pts3d, rvec, tvec, k, imsize, tris, opts) when is_integer(num) and is_integer(len) and (is_struct(pts3d, Evision.Mat) or is_struct(pts3d, Nx.Tensor) or is_number(pts3d) or is_tuple(pts3d)) and (is_struct(rvec, Evision.Mat) or is_struct(rvec, Nx.Tensor) or is_number(rvec) or is_tuple(rvec)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and is_tuple(imsize) and (is_struct(tris, Evision.Mat) or is_struct(tris, Nx.Tensor) or is_number(tris) or is_tuple(tris)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      num: Evision.Internal.Structurise.from_struct(num),
      len: Evision.Internal.Structurise.from_struct(len),
      pts3d: Evision.Internal.Structurise.from_struct(pts3d),
      rvec: Evision.Internal.Structurise.from_struct(rvec),
      tvec: Evision.Internal.Structurise.from_struct(tvec),
      k: Evision.Internal.Structurise.from_struct(k),
      imsize: Evision.Internal.Structurise.from_struct(imsize),
      tris: Evision.Internal.Structurise.from_struct(tris)
    ]
    :evision_nif.rapid_extractControlPoints(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  extractControlPoints

  ##### Positional Arguments
  - **num**: `integer()`.

    number of control points

  - **len**: `integer()`.

    search radius (used to restrict the ROI)

  - **pts3d**: `Evision.Mat`.

    the 3D points of the mesh

  - **rvec**: `Evision.Mat`.

    rotation between mesh and camera

  - **tvec**: `Evision.Mat`.

    translation between mesh and camera

  - **k**: `Evision.Mat`.

    camera intrinsic

  - **imsize**: `Size`.

    size of the video frame

  - **tris**: `Evision.Mat`.

    triangle face connectivity

  ##### Return
  - **ctl2d**: `Evision.Mat.t()`.

    the 2D locations of the control points

  - **ctl3d**: `Evision.Mat.t()`.

    matching 3D points of the mesh

   Extract control points from the projected silhouette of a mesh
   see @cite drummond2002real Sec 2.1, Step b

  Python prototype (for reference only):
  ```python3
  extractControlPoints(num, len, pts3d, rvec, tvec, K, imsize, tris[, ctl2d[, ctl3d]]) -> ctl2d, ctl3d
  ```
  """
  @spec extractControlPoints(integer(), integer(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number()}, Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def extractControlPoints(num, len, pts3d, rvec, tvec, k, imsize, tris) when is_integer(num) and is_integer(len) and (is_struct(pts3d, Evision.Mat) or is_struct(pts3d, Nx.Tensor) or is_number(pts3d) or is_tuple(pts3d)) and (is_struct(rvec, Evision.Mat) or is_struct(rvec, Nx.Tensor) or is_number(rvec) or is_tuple(rvec)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and is_tuple(imsize) and (is_struct(tris, Evision.Mat) or is_struct(tris, Nx.Tensor) or is_number(tris) or is_tuple(tris))
  do
    positional = [
      num: Evision.Internal.Structurise.from_struct(num),
      len: Evision.Internal.Structurise.from_struct(len),
      pts3d: Evision.Internal.Structurise.from_struct(pts3d),
      rvec: Evision.Internal.Structurise.from_struct(rvec),
      tvec: Evision.Internal.Structurise.from_struct(tvec),
      k: Evision.Internal.Structurise.from_struct(k),
      imsize: Evision.Internal.Structurise.from_struct(imsize),
      tris: Evision.Internal.Structurise.from_struct(tris)
    ]
    :evision_nif.rapid_extractControlPoints(positional)
    |> to_struct()
  end

  @doc """
  extractLineBundle

  ##### Positional Arguments
  - **len**: `integer()`.

    the search radius. The bundle will have `2*len + 1` columns.

  - **ctl2d**: `Evision.Mat`.

    the search lines will be centered at this points and orthogonal to the contour defined by
    them. The bundle will have as many rows.

  - **img**: `Evision.Mat`.

    the image to read the pixel intensities values from

  ##### Return
  - **bundle**: `Evision.Mat.t()`.

    line bundle image with size `ctl2d.rows() x (2 * len + 1)` and the same type as @p img

  - **srcLocations**: `Evision.Mat.t()`.

    the source pixel locations of @p bundle in @p img as CV_16SC2

   Extract the line bundle from an image

  Python prototype (for reference only):
  ```python3
  extractLineBundle(len, ctl2d, img[, bundle[, srcLocations]]) -> bundle, srcLocations
  ```
  """
  @spec extractLineBundle(integer(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def extractLineBundle(len, ctl2d, img, opts) when is_integer(len) and (is_struct(ctl2d, Evision.Mat) or is_struct(ctl2d, Nx.Tensor) or is_number(ctl2d) or is_tuple(ctl2d)) and (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      len: Evision.Internal.Structurise.from_struct(len),
      ctl2d: Evision.Internal.Structurise.from_struct(ctl2d),
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.rapid_extractLineBundle(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  extractLineBundle

  ##### Positional Arguments
  - **len**: `integer()`.

    the search radius. The bundle will have `2*len + 1` columns.

  - **ctl2d**: `Evision.Mat`.

    the search lines will be centered at this points and orthogonal to the contour defined by
    them. The bundle will have as many rows.

  - **img**: `Evision.Mat`.

    the image to read the pixel intensities values from

  ##### Return
  - **bundle**: `Evision.Mat.t()`.

    line bundle image with size `ctl2d.rows() x (2 * len + 1)` and the same type as @p img

  - **srcLocations**: `Evision.Mat.t()`.

    the source pixel locations of @p bundle in @p img as CV_16SC2

   Extract the line bundle from an image

  Python prototype (for reference only):
  ```python3
  extractLineBundle(len, ctl2d, img[, bundle[, srcLocations]]) -> bundle, srcLocations
  ```
  """
  @spec extractLineBundle(integer(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def extractLineBundle(len, ctl2d, img) when is_integer(len) and (is_struct(ctl2d, Evision.Mat) or is_struct(ctl2d, Nx.Tensor) or is_number(ctl2d) or is_tuple(ctl2d)) and (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      len: Evision.Internal.Structurise.from_struct(len),
      ctl2d: Evision.Internal.Structurise.from_struct(ctl2d),
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.rapid_extractLineBundle(positional)
    |> to_struct()
  end

  @doc """
  findCorrespondencies

  ##### Positional Arguments
  - **bundle**: `Evision.Mat`.

    the line bundle

  ##### Return
  - **cols**: `Evision.Mat.t()`.

    correspondence-position per line in line-bundle-space

  - **response**: `Evision.Mat.t()`.

    the sobel response for the selected point

   Find corresponding image locations by searching for a maximal sobel edge along the search line (a single
   row in the bundle)

  Python prototype (for reference only):
  ```python3
  findCorrespondencies(bundle[, cols[, response]]) -> cols, response
  ```
  """
  @spec findCorrespondencies(Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def findCorrespondencies(bundle, opts) when (is_struct(bundle, Evision.Mat) or is_struct(bundle, Nx.Tensor) or is_number(bundle) or is_tuple(bundle)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      bundle: Evision.Internal.Structurise.from_struct(bundle)
    ]
    :evision_nif.rapid_findCorrespondencies(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  findCorrespondencies

  ##### Positional Arguments
  - **bundle**: `Evision.Mat`.

    the line bundle

  ##### Return
  - **cols**: `Evision.Mat.t()`.

    correspondence-position per line in line-bundle-space

  - **response**: `Evision.Mat.t()`.

    the sobel response for the selected point

   Find corresponding image locations by searching for a maximal sobel edge along the search line (a single
   row in the bundle)

  Python prototype (for reference only):
  ```python3
  findCorrespondencies(bundle[, cols[, response]]) -> cols, response
  ```
  """
  @spec findCorrespondencies(Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def findCorrespondencies(bundle) when (is_struct(bundle, Evision.Mat) or is_struct(bundle, Nx.Tensor) or is_number(bundle) or is_tuple(bundle))
  do
    positional = [
      bundle: Evision.Internal.Structurise.from_struct(bundle)
    ]
    :evision_nif.rapid_findCorrespondencies(positional)
    |> to_struct()
  end

  @doc """
  rapid

  ##### Positional Arguments
  - **img**: `Evision.Mat`.

    the video frame

  - **num**: `integer()`.

    number of search lines

  - **len**: `integer()`.

    search line radius

  - **pts3d**: `Evision.Mat`.

    the 3D points of the mesh

  - **tris**: `Evision.Mat`.

    triangle face connectivity

  - **k**: `Evision.Mat`.

    camera matrix

  ##### Return
  - **retval**: `float`
  - **rvec**: `Evision.Mat.t()`.

    rotation between mesh and camera. Input values are used as an initial solution.

  - **tvec**: `Evision.Mat.t()`.

    translation between mesh and camera. Input values are used as an initial solution.

  - **rmsd**: `double*`.

    the 2d reprojection difference

   High level function to execute a single rapid @cite harris1990rapid iteration
   1. @ref extractControlPoints
   2. @ref extractLineBundle
   3. @ref findCorrespondencies
   4. @ref convertCorrespondencies
   5. @ref solvePnPRefineLM
  @return ratio of search lines that could be extracted and matched

  Python prototype (for reference only):
  ```python3
  rapid(img, num, len, pts3d, tris, K, rvec, tvec) -> retval, rvec, tvec, rmsd
  ```
  """
  @spec rapid(Evision.Mat.maybe_mat_in(), integer(), integer(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {number(), Evision.Mat.t(), Evision.Mat.t(), number()} | {:error, String.t()}
  def rapid(img, num, len, pts3d, tris, k, rvec, tvec) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and is_integer(num) and is_integer(len) and (is_struct(pts3d, Evision.Mat) or is_struct(pts3d, Nx.Tensor) or is_number(pts3d) or is_tuple(pts3d)) and (is_struct(tris, Evision.Mat) or is_struct(tris, Nx.Tensor) or is_number(tris) or is_tuple(tris)) and (is_struct(k, Evision.Mat) or is_struct(k, Nx.Tensor) or is_number(k) or is_tuple(k)) and (is_struct(rvec, Evision.Mat) or is_struct(rvec, Nx.Tensor) or is_number(rvec) or is_tuple(rvec)) and (is_struct(tvec, Evision.Mat) or is_struct(tvec, Nx.Tensor) or is_number(tvec) or is_tuple(tvec))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      num: Evision.Internal.Structurise.from_struct(num),
      len: Evision.Internal.Structurise.from_struct(len),
      pts3d: Evision.Internal.Structurise.from_struct(pts3d),
      tris: Evision.Internal.Structurise.from_struct(tris),
      k: Evision.Internal.Structurise.from_struct(k),
      rvec: Evision.Internal.Structurise.from_struct(rvec),
      tvec: Evision.Internal.Structurise.from_struct(tvec)
    ]
    :evision_nif.rapid_rapid(positional)
    |> to_struct()
  end
end
