defmodule Evision.XImgProc do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XImgProc` struct.

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
  def to_struct({:ok, %{class: Evision.XImgProc, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XImgProc, ref: ref}) do
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
  Calculates 2D Fast Hough transform of an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **dstMatDepth**: `integer()`

  ##### Keyword Arguments
  - **angleRange**: `integer()`.
  - **op**: `integer()`.
  - **makeSkew**: `integer()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   The function calculates the fast Hough transform for full, half or quarter
   range of angles.

  Python prototype (for reference only):
  ```python3
  FastHoughTransform(src, dstMatDepth[, dst[, angleRange[, op[, makeSkew]]]]) -> dst
  ```
  """
  @spec fastHoughTransform(Evision.Mat.maybe_mat_in(), integer(), [{:angleRange, term()} | {:makeSkew, term()} | {:op, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def fastHoughTransform(src, dstMatDepth, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(dstMatDepth) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:angleRange, :makeSkew, :op])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dstMatDepth: Evision.Internal.Structurise.from_struct(dstMatDepth)
    ]
    :evision_nif.ximgproc_FastHoughTransform(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Calculates 2D Fast Hough transform of an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **dstMatDepth**: `integer()`

  ##### Keyword Arguments
  - **angleRange**: `integer()`.
  - **op**: `integer()`.
  - **makeSkew**: `integer()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   The function calculates the fast Hough transform for full, half or quarter
   range of angles.

  Python prototype (for reference only):
  ```python3
  FastHoughTransform(src, dstMatDepth[, dst[, angleRange[, op[, makeSkew]]]]) -> dst
  ```
  """
  @spec fastHoughTransform(Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def fastHoughTransform(src, dstMatDepth) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(dstMatDepth)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dstMatDepth: Evision.Internal.Structurise.from_struct(dstMatDepth)
    ]
    :evision_nif.ximgproc_FastHoughTransform(positional)
    |> to_struct()
  end

  @doc """
  Applies X Deriche filter to an image.

  ##### Positional Arguments
  - **op**: `Evision.Mat`
  - **alpha**: `double`
  - **omega**: `double`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   For more details about this implementation, please see http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.476.5736&rep=rep1&type=pdf

  Python prototype (for reference only):
  ```python3
  GradientDericheX(op, alpha, omega[, dst]) -> dst
  ```
  """
  @spec gradientDericheX(Evision.Mat.maybe_mat_in(), number(), number(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def gradientDericheX(op, alpha, omega, opts) when (is_struct(op, Evision.Mat) or is_struct(op, Nx.Tensor) or is_number(op) or is_tuple(op)) and is_number(alpha) and is_number(omega) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      op: Evision.Internal.Structurise.from_struct(op),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      omega: Evision.Internal.Structurise.from_struct(omega)
    ]
    :evision_nif.ximgproc_GradientDericheX(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Applies X Deriche filter to an image.

  ##### Positional Arguments
  - **op**: `Evision.Mat`
  - **alpha**: `double`
  - **omega**: `double`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   For more details about this implementation, please see http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.476.5736&rep=rep1&type=pdf

  Python prototype (for reference only):
  ```python3
  GradientDericheX(op, alpha, omega[, dst]) -> dst
  ```
  """
  @spec gradientDericheX(Evision.Mat.maybe_mat_in(), number(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def gradientDericheX(op, alpha, omega) when (is_struct(op, Evision.Mat) or is_struct(op, Nx.Tensor) or is_number(op) or is_tuple(op)) and is_number(alpha) and is_number(omega)
  do
    positional = [
      op: Evision.Internal.Structurise.from_struct(op),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      omega: Evision.Internal.Structurise.from_struct(omega)
    ]
    :evision_nif.ximgproc_GradientDericheX(positional)
    |> to_struct()
  end

  @doc """
  Applies Y Deriche filter to an image.

  ##### Positional Arguments
  - **op**: `Evision.Mat`
  - **alpha**: `double`
  - **omega**: `double`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   For more details about this implementation, please see http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.476.5736&rep=rep1&type=pdf

  Python prototype (for reference only):
  ```python3
  GradientDericheY(op, alpha, omega[, dst]) -> dst
  ```
  """
  @spec gradientDericheY(Evision.Mat.maybe_mat_in(), number(), number(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def gradientDericheY(op, alpha, omega, opts) when (is_struct(op, Evision.Mat) or is_struct(op, Nx.Tensor) or is_number(op) or is_tuple(op)) and is_number(alpha) and is_number(omega) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      op: Evision.Internal.Structurise.from_struct(op),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      omega: Evision.Internal.Structurise.from_struct(omega)
    ]
    :evision_nif.ximgproc_GradientDericheY(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Applies Y Deriche filter to an image.

  ##### Positional Arguments
  - **op**: `Evision.Mat`
  - **alpha**: `double`
  - **omega**: `double`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   For more details about this implementation, please see http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.476.5736&rep=rep1&type=pdf

  Python prototype (for reference only):
  ```python3
  GradientDericheY(op, alpha, omega[, dst]) -> dst
  ```
  """
  @spec gradientDericheY(Evision.Mat.maybe_mat_in(), number(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def gradientDericheY(op, alpha, omega) when (is_struct(op, Evision.Mat) or is_struct(op, Nx.Tensor) or is_number(op) or is_tuple(op)) and is_number(alpha) and is_number(omega)
  do
    positional = [
      op: Evision.Internal.Structurise.from_struct(op),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      omega: Evision.Internal.Structurise.from_struct(omega)
    ]
    :evision_nif.ximgproc_GradientDericheY(positional)
    |> to_struct()
  end

  @doc """
  Calculates coordinates of line segment corresponded by point in Hough space.

  ##### Positional Arguments
  - **houghPoint**: `Point`
  - **srcImgInfo**: `Evision.Mat`

  ##### Keyword Arguments
  - **angleRange**: `integer()`.
  - **makeSkew**: `integer()`.
  - **rules**: `integer()`.

  ##### Return
  - **retval**: `Vec4i`

  @retval  [Vec4i]     Coordinates of line segment corresponded by point in Hough space.
  @remarks If rules parameter set to RO_STRICT
  then returned line cut along the border of source image.
  @remarks If rules parameter set to RO_WEAK then in case of point, which belongs
  the incorrect part of Hough image, returned line will not intersect source image.
   The function calculates coordinates of line segment corresponded by point in Hough space.

  Python prototype (for reference only):
  ```python3
  HoughPoint2Line(houghPoint, srcImgInfo[, angleRange[, makeSkew[, rules]]]) -> retval
  ```
  """
  @spec houghPoint2Line({number(), number()}, Evision.Mat.maybe_mat_in(), [{:angleRange, term()} | {:makeSkew, term()} | {:rules, term()}] | nil) :: {integer(), integer(), integer(), integer()} | {:error, String.t()}
  def houghPoint2Line(houghPoint, srcImgInfo, opts) when is_tuple(houghPoint) and (is_struct(srcImgInfo, Evision.Mat) or is_struct(srcImgInfo, Nx.Tensor) or is_number(srcImgInfo) or is_tuple(srcImgInfo)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:angleRange, :makeSkew, :rules])
    positional = [
      houghPoint: Evision.Internal.Structurise.from_struct(houghPoint),
      srcImgInfo: Evision.Internal.Structurise.from_struct(srcImgInfo)
    ]
    :evision_nif.ximgproc_HoughPoint2Line(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Calculates coordinates of line segment corresponded by point in Hough space.

  ##### Positional Arguments
  - **houghPoint**: `Point`
  - **srcImgInfo**: `Evision.Mat`

  ##### Keyword Arguments
  - **angleRange**: `integer()`.
  - **makeSkew**: `integer()`.
  - **rules**: `integer()`.

  ##### Return
  - **retval**: `Vec4i`

  @retval  [Vec4i]     Coordinates of line segment corresponded by point in Hough space.
  @remarks If rules parameter set to RO_STRICT
  then returned line cut along the border of source image.
  @remarks If rules parameter set to RO_WEAK then in case of point, which belongs
  the incorrect part of Hough image, returned line will not intersect source image.
   The function calculates coordinates of line segment corresponded by point in Hough space.

  Python prototype (for reference only):
  ```python3
  HoughPoint2Line(houghPoint, srcImgInfo[, angleRange[, makeSkew[, rules]]]) -> retval
  ```
  """
  @spec houghPoint2Line({number(), number()}, Evision.Mat.maybe_mat_in()) :: {integer(), integer(), integer(), integer()} | {:error, String.t()}
  def houghPoint2Line(houghPoint, srcImgInfo) when is_tuple(houghPoint) and (is_struct(srcImgInfo, Evision.Mat) or is_struct(srcImgInfo, Nx.Tensor) or is_number(srcImgInfo) or is_tuple(srcImgInfo))
  do
    positional = [
      houghPoint: Evision.Internal.Structurise.from_struct(houghPoint),
      srcImgInfo: Evision.Internal.Structurise.from_struct(srcImgInfo)
    ]
    :evision_nif.ximgproc_HoughPoint2Line(positional)
    |> to_struct()
  end

  @doc """
  PeiLinNormalization

  ##### Positional Arguments
  - **i**: `Evision.Mat`

  ##### Return
  - **t**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  PeiLinNormalization(I[, T]) -> T
  ```
  """
  @spec peiLinNormalization(Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def peiLinNormalization(i, opts) when (is_struct(i, Evision.Mat) or is_struct(i, Nx.Tensor) or is_number(i) or is_tuple(i)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      i: Evision.Internal.Structurise.from_struct(i)
    ]
    :evision_nif.ximgproc_PeiLinNormalization(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  PeiLinNormalization

  ##### Positional Arguments
  - **i**: `Evision.Mat`

  ##### Return
  - **t**: `Evision.Mat.t()`.

  Has overloading in C++

  Python prototype (for reference only):
  ```python3
  PeiLinNormalization(I[, T]) -> T
  ```
  """
  @spec peiLinNormalization(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def peiLinNormalization(i) when (is_struct(i, Evision.Mat) or is_struct(i, Nx.Tensor) or is_number(i) or is_tuple(i))
  do
    positional = [
      i: Evision.Internal.Structurise.from_struct(i)
    ]
    :evision_nif.ximgproc_PeiLinNormalization(positional)
    |> to_struct()
  end

  @doc """
  Calculate Radon Transform of an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **theta**: `double`.
  - **start_angle**: `double`.
  - **end_angle**: `double`.
  - **crop**: `bool`.
  - **norm**: `bool`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   This function calculates the Radon Transform of a given image in any range.
   See https://engineering.purdue.edu/~malcolm/pct/CTI_Ch03.pdf for detail.
   If the input type is CV_8U, the output will be CV_32S.
   If the input type is CV_32F or CV_64F, the output will be CV_64F
   The output size will be num_of_integral x src_diagonal_length.
   If crop is selected, the input image will be crop into square then circle,
   and output size will be num_of_integral x min_edge.

  Python prototype (for reference only):
  ```python3
  RadonTransform(src[, dst[, theta[, start_angle[, end_angle[, crop[, norm]]]]]]) -> dst
  ```
  """
  @spec radonTransform(Evision.Mat.maybe_mat_in(), [{:crop, term()} | {:end_angle, term()} | {:norm, term()} | {:start_angle, term()} | {:theta, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def radonTransform(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:crop, :end_angle, :norm, :start_angle, :theta])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_RadonTransform(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Calculate Radon Transform of an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **theta**: `double`.
  - **start_angle**: `double`.
  - **end_angle**: `double`.
  - **crop**: `bool`.
  - **norm**: `bool`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   This function calculates the Radon Transform of a given image in any range.
   See https://engineering.purdue.edu/~malcolm/pct/CTI_Ch03.pdf for detail.
   If the input type is CV_8U, the output will be CV_32S.
   If the input type is CV_32F or CV_64F, the output will be CV_64F
   The output size will be num_of_integral x src_diagonal_length.
   If crop is selected, the input image will be crop into square then circle,
   and output size will be num_of_integral x min_edge.

  Python prototype (for reference only):
  ```python3
  RadonTransform(src[, dst[, theta[, start_angle[, end_angle[, crop[, norm]]]]]]) -> dst
  ```
  """
  @spec radonTransform(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def radonTransform(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_RadonTransform(positional)
    |> to_struct()
  end

  @doc """
  Simple one-line Adaptive Manifold Filter call.

  ##### Positional Arguments
  - **joint**: `Evision.Mat`.

    joint (also called as guided) image or array of images with any numbers of channels.

  - **src**: `Evision.Mat`.

    filtering image with any numbers of channels.

  - **sigma_s**: `double`.

    spatial standard deviation.

  - **sigma_r**: `double`.

    color space standard deviation, it is similar to the sigma in the color space into
    bilateralFilter.

  ##### Keyword Arguments
  - **adjust_outliers**: `bool`.

    optional, specify perform outliers adjust operation or not, (Eq. 9) in the
    original paper.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    output image.

  **Note**: Joint images with CV_8U and CV_16U depth converted to images with CV_32F depth and [0; 1]
  color range before processing. Hence color space sigma sigma_r must be in [0; 1] range, unlike same
  sigmas in bilateralFilter and dtFilter functions. @sa bilateralFilter, dtFilter, guidedFilter

  Python prototype (for reference only):
  ```python3
  amFilter(joint, src, sigma_s, sigma_r[, dst[, adjust_outliers]]) -> dst
  ```
  """
  @spec amFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), number(), [{:adjust_outliers, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def amFilter(joint, src, sigma_s, sigma_r, opts) when (is_struct(joint, Evision.Mat) or is_struct(joint, Nx.Tensor) or is_number(joint) or is_tuple(joint)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(sigma_s) and is_number(sigma_r) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:adjust_outliers])
    positional = [
      joint: Evision.Internal.Structurise.from_struct(joint),
      src: Evision.Internal.Structurise.from_struct(src),
      sigma_s: Evision.Internal.Structurise.from_struct(sigma_s),
      sigma_r: Evision.Internal.Structurise.from_struct(sigma_r)
    ]
    :evision_nif.ximgproc_amFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Simple one-line Adaptive Manifold Filter call.

  ##### Positional Arguments
  - **joint**: `Evision.Mat`.

    joint (also called as guided) image or array of images with any numbers of channels.

  - **src**: `Evision.Mat`.

    filtering image with any numbers of channels.

  - **sigma_s**: `double`.

    spatial standard deviation.

  - **sigma_r**: `double`.

    color space standard deviation, it is similar to the sigma in the color space into
    bilateralFilter.

  ##### Keyword Arguments
  - **adjust_outliers**: `bool`.

    optional, specify perform outliers adjust operation or not, (Eq. 9) in the
    original paper.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    output image.

  **Note**: Joint images with CV_8U and CV_16U depth converted to images with CV_32F depth and [0; 1]
  color range before processing. Hence color space sigma sigma_r must be in [0; 1] range, unlike same
  sigmas in bilateralFilter and dtFilter functions. @sa bilateralFilter, dtFilter, guidedFilter

  Python prototype (for reference only):
  ```python3
  amFilter(joint, src, sigma_s, sigma_r[, dst[, adjust_outliers]]) -> dst
  ```
  """
  @spec amFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def amFilter(joint, src, sigma_s, sigma_r) when (is_struct(joint, Evision.Mat) or is_struct(joint, Nx.Tensor) or is_number(joint) or is_tuple(joint)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(sigma_s) and is_number(sigma_r)
  do
    positional = [
      joint: Evision.Internal.Structurise.from_struct(joint),
      src: Evision.Internal.Structurise.from_struct(src),
      sigma_s: Evision.Internal.Structurise.from_struct(sigma_s),
      sigma_r: Evision.Internal.Structurise.from_struct(sigma_r)
    ]
    :evision_nif.ximgproc_amFilter(positional)
    |> to_struct()
  end

  @doc """
  Performs anisotropic diffusion on an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image with 3 channels.

  - **alpha**: `float`.

    The amount of time to step forward by on each iteration (normally, it's between 0 and 1).

  - **k**: `float`.

    sensitivity to the edges

  - **niters**: `integer()`.

    The number of iterations

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and the same number of channels as src .

  The function applies Perona-Malik anisotropic diffusion to an image. This is the solution to the partial differential equation:
  \\f[{\\frac  {\\partial I}{\\partial t}}={\\mathrm  {div}}\\left(c(x,y,t)\\nabla I\\right)=\\nabla c\\cdot \\nabla I+c(x,y,t)\\Delta I\\f]
  Suggested functions for c(x,y,t) are:
  \\f[c\\left(\\|\\nabla I\\|\\right)=e^{{-\\left(\\|\\nabla I\\|/K\\right)^{2}}}\\f]
  or
  \\f[ c\\left(\\|\\nabla I\\|\\right)={\\frac {1}{1+\\left({\\frac  {\\|\\nabla I\\|}{K}}\\right)^{2}}} \\f]

  Python prototype (for reference only):
  ```python3
  anisotropicDiffusion(src, alpha, K, niters[, dst]) -> dst
  ```
  """
  @spec anisotropicDiffusion(Evision.Mat.maybe_mat_in(), number(), number(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def anisotropicDiffusion(src, alpha, k, niters, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_float(alpha) and is_float(k) and is_integer(niters) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      k: Evision.Internal.Structurise.from_struct(k),
      niters: Evision.Internal.Structurise.from_struct(niters)
    ]
    :evision_nif.ximgproc_anisotropicDiffusion(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Performs anisotropic diffusion on an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image with 3 channels.

  - **alpha**: `float`.

    The amount of time to step forward by on each iteration (normally, it's between 0 and 1).

  - **k**: `float`.

    sensitivity to the edges

  - **niters**: `integer()`.

    The number of iterations

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and the same number of channels as src .

  The function applies Perona-Malik anisotropic diffusion to an image. This is the solution to the partial differential equation:
  \\f[{\\frac  {\\partial I}{\\partial t}}={\\mathrm  {div}}\\left(c(x,y,t)\\nabla I\\right)=\\nabla c\\cdot \\nabla I+c(x,y,t)\\Delta I\\f]
  Suggested functions for c(x,y,t) are:
  \\f[c\\left(\\|\\nabla I\\|\\right)=e^{{-\\left(\\|\\nabla I\\|/K\\right)^{2}}}\\f]
  or
  \\f[ c\\left(\\|\\nabla I\\|\\right)={\\frac {1}{1+\\left({\\frac  {\\|\\nabla I\\|}{K}}\\right)^{2}}} \\f]

  Python prototype (for reference only):
  ```python3
  anisotropicDiffusion(src, alpha, K, niters[, dst]) -> dst
  ```
  """
  @spec anisotropicDiffusion(Evision.Mat.maybe_mat_in(), number(), number(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def anisotropicDiffusion(src, alpha, k, niters) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_float(alpha) and is_float(k) and is_integer(niters)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      alpha: Evision.Internal.Structurise.from_struct(alpha),
      k: Evision.Internal.Structurise.from_struct(k),
      niters: Evision.Internal.Structurise.from_struct(niters)
    ]
    :evision_nif.ximgproc_anisotropicDiffusion(positional)
    |> to_struct()
  end

  @doc """
  Applies the bilateral texture filter to an image. It performs structure-preserving texture filter.
  For more details about this filter see @cite Cho2014.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image whose depth is 8-bit UINT or 32-bit FLOAT

  ##### Keyword Arguments
  - **fr**: `integer()`.

    Radius of kernel to be used for filtering. It should be positive integer

  - **numIter**: `integer()`.

    Number of iterations of algorithm, It should be positive integer

  - **sigmaAlpha**: `double`.

    Controls the sharpness of the weight transition from edges to smooth/texture regions, where
    a bigger value means sharper transition. When the value is negative, it is automatically calculated.

  - **sigmaAvg**: `double`.

    Range blur parameter for texture blurring. Larger value makes result to be more blurred. When the
    value is negative, it is automatically calculated as described in the paper.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and type as src.

  @sa rollingGuidanceFilter, bilateralFilter

  Python prototype (for reference only):
  ```python3
  bilateralTextureFilter(src[, dst[, fr[, numIter[, sigmaAlpha[, sigmaAvg]]]]]) -> dst
  ```
  """
  @spec bilateralTextureFilter(Evision.Mat.maybe_mat_in(), [{:fr, term()} | {:numIter, term()} | {:sigmaAlpha, term()} | {:sigmaAvg, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def bilateralTextureFilter(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:fr, :numIter, :sigmaAlpha, :sigmaAvg])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_bilateralTextureFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Applies the bilateral texture filter to an image. It performs structure-preserving texture filter.
  For more details about this filter see @cite Cho2014.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source image whose depth is 8-bit UINT or 32-bit FLOAT

  ##### Keyword Arguments
  - **fr**: `integer()`.

    Radius of kernel to be used for filtering. It should be positive integer

  - **numIter**: `integer()`.

    Number of iterations of algorithm, It should be positive integer

  - **sigmaAlpha**: `double`.

    Controls the sharpness of the weight transition from edges to smooth/texture regions, where
    a bigger value means sharper transition. When the value is negative, it is automatically calculated.

  - **sigmaAvg**: `double`.

    Range blur parameter for texture blurring. Larger value makes result to be more blurred. When the
    value is negative, it is automatically calculated as described in the paper.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and type as src.

  @sa rollingGuidanceFilter, bilateralFilter

  Python prototype (for reference only):
  ```python3
  bilateralTextureFilter(src[, dst[, fr[, numIter[, sigmaAlpha[, sigmaAvg]]]]]) -> dst
  ```
  """
  @spec bilateralTextureFilter(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def bilateralTextureFilter(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_bilateralTextureFilter(positional)
    |> to_struct()
  end

  @doc """
  Compares a color template against overlapped color image regions.

  ##### Positional Arguments
  - **img**: `Evision.Mat`
  - **templ**: `Evision.Mat`

  ##### Return
  - **result**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  colorMatchTemplate(img, templ[, result]) -> result
  ```
  """
  @spec colorMatchTemplate(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def colorMatchTemplate(img, templ, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(templ, Evision.Mat) or is_struct(templ, Nx.Tensor) or is_number(templ) or is_tuple(templ)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      templ: Evision.Internal.Structurise.from_struct(templ)
    ]
    :evision_nif.ximgproc_colorMatchTemplate(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Compares a color template against overlapped color image regions.

  ##### Positional Arguments
  - **img**: `Evision.Mat`
  - **templ**: `Evision.Mat`

  ##### Return
  - **result**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  colorMatchTemplate(img, templ[, result]) -> result
  ```
  """
  @spec colorMatchTemplate(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def colorMatchTemplate(img, templ) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (is_struct(templ, Evision.Mat) or is_struct(templ, Nx.Tensor) or is_number(templ) or is_tuple(templ))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      templ: Evision.Internal.Structurise.from_struct(templ)
    ]
    :evision_nif.ximgproc_colorMatchTemplate(positional)
    |> to_struct()
  end

  @doc """
  Function for computing the percent of "bad" pixels in the disparity map
  (pixels where error is higher than a specified threshold)

  ##### Positional Arguments
  - **gT**: `Evision.Mat`.

    ground truth disparity map

  - **src**: `Evision.Mat`.

    disparity map to evaluate

  - **rOI**: `Rect`.

    region of interest

  ##### Keyword Arguments
  - **thresh**: `integer()`.

    threshold used to determine "bad" pixels

  ##### Return
  - **retval**: `double`

  @result returns mean square error between GT and src

  Python prototype (for reference only):
  ```python3
  computeBadPixelPercent(GT, src, ROI[, thresh]) -> retval
  ```
  """
  @spec computeBadPixelPercent(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number(), number(), number()}, [{:thresh, term()}] | nil) :: number() | {:error, String.t()}
  def computeBadPixelPercent(gT, src, rOI, opts) when (is_struct(gT, Evision.Mat) or is_struct(gT, Nx.Tensor) or is_number(gT) or is_tuple(gT)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_tuple(rOI) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:thresh])
    positional = [
      gT: Evision.Internal.Structurise.from_struct(gT),
      src: Evision.Internal.Structurise.from_struct(src),
      rOI: Evision.Internal.Structurise.from_struct(rOI)
    ]
    :evision_nif.ximgproc_computeBadPixelPercent(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Function for computing the percent of "bad" pixels in the disparity map
  (pixels where error is higher than a specified threshold)

  ##### Positional Arguments
  - **gT**: `Evision.Mat`.

    ground truth disparity map

  - **src**: `Evision.Mat`.

    disparity map to evaluate

  - **rOI**: `Rect`.

    region of interest

  ##### Keyword Arguments
  - **thresh**: `integer()`.

    threshold used to determine "bad" pixels

  ##### Return
  - **retval**: `double`

  @result returns mean square error between GT and src

  Python prototype (for reference only):
  ```python3
  computeBadPixelPercent(GT, src, ROI[, thresh]) -> retval
  ```
  """
  @spec computeBadPixelPercent(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number(), number(), number()}) :: number() | {:error, String.t()}
  def computeBadPixelPercent(gT, src, rOI) when (is_struct(gT, Evision.Mat) or is_struct(gT, Nx.Tensor) or is_number(gT) or is_tuple(gT)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_tuple(rOI)
  do
    positional = [
      gT: Evision.Internal.Structurise.from_struct(gT),
      src: Evision.Internal.Structurise.from_struct(src),
      rOI: Evision.Internal.Structurise.from_struct(rOI)
    ]
    :evision_nif.ximgproc_computeBadPixelPercent(positional)
    |> to_struct()
  end

  @doc """
  Function for computing mean square error for disparity maps

  ##### Positional Arguments
  - **gT**: `Evision.Mat`.

    ground truth disparity map

  - **src**: `Evision.Mat`.

    disparity map to evaluate

  - **rOI**: `Rect`.

    region of interest

  ##### Return
  - **retval**: `double`

  @result returns mean square error between GT and src

  Python prototype (for reference only):
  ```python3
  computeMSE(GT, src, ROI) -> retval
  ```
  """
  @spec computeMSE(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), {number(), number(), number(), number()}) :: number() | {:error, String.t()}
  def computeMSE(gT, src, rOI) when (is_struct(gT, Evision.Mat) or is_struct(gT, Nx.Tensor) or is_number(gT) or is_tuple(gT)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_tuple(rOI)
  do
    positional = [
      gT: Evision.Internal.Structurise.from_struct(gT),
      src: Evision.Internal.Structurise.from_struct(src),
      rOI: Evision.Internal.Structurise.from_struct(rOI)
    ]
    :evision_nif.ximgproc_computeMSE(positional)
    |> to_struct()
  end

  @doc """
  Contour sampling .

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **nbElt**: `integer()`

  ##### Return
  - **out**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  contourSampling(src, nbElt[, out]) -> out
  ```
  """
  @spec contourSampling(Evision.Mat.maybe_mat_in(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def contourSampling(src, nbElt, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(nbElt) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      nbElt: Evision.Internal.Structurise.from_struct(nbElt)
    ]
    :evision_nif.ximgproc_contourSampling(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Contour sampling .

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **nbElt**: `integer()`

  ##### Return
  - **out**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  contourSampling(src, nbElt[, out]) -> out
  ```
  """
  @spec contourSampling(Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def contourSampling(src, nbElt) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(nbElt)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      nbElt: Evision.Internal.Structurise.from_struct(nbElt)
    ]
    :evision_nif.ximgproc_contourSampling(positional)
    |> to_struct()
  end

  @doc """
  Computes the estimated covariance matrix of an image using the sliding
  window forumlation.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    The source image. Input image must be of a complex type.

  - **windowRows**: `integer()`.

    The number of rows in the window.

  - **windowCols**: `integer()`.

    The number of cols in the window.
    The window size parameters control the accuracy of the estimation.
    The sliding window moves over the entire image from the top-left corner
    to the bottom right corner. Each location of the window represents a sample.
    If the window is the size of the image, then this gives the exact covariance matrix.
    For all other cases, the sizes of the window will impact the number of samples
    and the number of elements in the estimated covariance matrix.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    The destination estimated covariance matrix. Output matrix will be size (windowRows*windowCols, windowRows*windowCols).

  Python prototype (for reference only):
  ```python3
  covarianceEstimation(src, windowRows, windowCols[, dst]) -> dst
  ```
  """
  @spec covarianceEstimation(Evision.Mat.maybe_mat_in(), integer(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def covarianceEstimation(src, windowRows, windowCols, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(windowRows) and is_integer(windowCols) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      windowRows: Evision.Internal.Structurise.from_struct(windowRows),
      windowCols: Evision.Internal.Structurise.from_struct(windowCols)
    ]
    :evision_nif.ximgproc_covarianceEstimation(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Computes the estimated covariance matrix of an image using the sliding
  window forumlation.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    The source image. Input image must be of a complex type.

  - **windowRows**: `integer()`.

    The number of rows in the window.

  - **windowCols**: `integer()`.

    The number of cols in the window.
    The window size parameters control the accuracy of the estimation.
    The sliding window moves over the entire image from the top-left corner
    to the bottom right corner. Each location of the window represents a sample.
    If the window is the size of the image, then this gives the exact covariance matrix.
    For all other cases, the sizes of the window will impact the number of samples
    and the number of elements in the estimated covariance matrix.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    The destination estimated covariance matrix. Output matrix will be size (windowRows*windowCols, windowRows*windowCols).

  Python prototype (for reference only):
  ```python3
  covarianceEstimation(src, windowRows, windowCols[, dst]) -> dst
  ```
  """
  @spec covarianceEstimation(Evision.Mat.maybe_mat_in(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def covarianceEstimation(src, windowRows, windowCols) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(windowRows) and is_integer(windowCols)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      windowRows: Evision.Internal.Structurise.from_struct(windowRows),
      windowCols: Evision.Internal.Structurise.from_struct(windowCols)
    ]
    :evision_nif.ximgproc_covarianceEstimation(positional)
    |> to_struct()
  end

  @doc """
  Factory method, create instance of AdaptiveManifoldFilter and produce some initialization routines.

  ##### Positional Arguments
  - **sigma_s**: `double`.

    spatial standard deviation.

  - **sigma_r**: `double`.

    color space standard deviation, it is similar to the sigma in the color space into
    bilateralFilter.

  ##### Keyword Arguments
  - **adjust_outliers**: `bool`.

    optional, specify perform outliers adjust operation or not, (Eq. 9) in the
    original paper.

  ##### Return
  - **retval**: `Evision.XImgProc.AdaptiveManifoldFilter.t()`

  For more details about Adaptive Manifold Filter parameters, see the original article @cite Gastal12 .
  **Note**: Joint images with CV_8U and CV_16U depth converted to images with CV_32F depth and [0; 1]
  color range before processing. Hence color space sigma sigma_r must be in [0; 1] range, unlike same
  sigmas in bilateralFilter and dtFilter functions.

  Python prototype (for reference only):
  ```python3
  createAMFilter(sigma_s, sigma_r[, adjust_outliers]) -> retval
  ```
  """
  @spec createAMFilter(number(), number(), [{:adjust_outliers, term()}] | nil) :: Evision.XImgProc.AdaptiveManifoldFilter.t() | {:error, String.t()}
  def createAMFilter(sigma_s, sigma_r, opts) when is_number(sigma_s) and is_number(sigma_r) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:adjust_outliers])
    positional = [
      sigma_s: Evision.Internal.Structurise.from_struct(sigma_s),
      sigma_r: Evision.Internal.Structurise.from_struct(sigma_r)
    ]
    :evision_nif.ximgproc_createAMFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Factory method, create instance of AdaptiveManifoldFilter and produce some initialization routines.

  ##### Positional Arguments
  - **sigma_s**: `double`.

    spatial standard deviation.

  - **sigma_r**: `double`.

    color space standard deviation, it is similar to the sigma in the color space into
    bilateralFilter.

  ##### Keyword Arguments
  - **adjust_outliers**: `bool`.

    optional, specify perform outliers adjust operation or not, (Eq. 9) in the
    original paper.

  ##### Return
  - **retval**: `Evision.XImgProc.AdaptiveManifoldFilter.t()`

  For more details about Adaptive Manifold Filter parameters, see the original article @cite Gastal12 .
  **Note**: Joint images with CV_8U and CV_16U depth converted to images with CV_32F depth and [0; 1]
  color range before processing. Hence color space sigma sigma_r must be in [0; 1] range, unlike same
  sigmas in bilateralFilter and dtFilter functions.

  Python prototype (for reference only):
  ```python3
  createAMFilter(sigma_s, sigma_r[, adjust_outliers]) -> retval
  ```
  """
  @spec createAMFilter(number(), number()) :: Evision.XImgProc.AdaptiveManifoldFilter.t() | {:error, String.t()}
  def createAMFilter(sigma_s, sigma_r) when is_number(sigma_s) and is_number(sigma_r)
  do
    positional = [
      sigma_s: Evision.Internal.Structurise.from_struct(sigma_s),
      sigma_r: Evision.Internal.Structurise.from_struct(sigma_r)
    ]
    :evision_nif.ximgproc_createAMFilter(positional)
    |> to_struct()
  end

  @doc """
  create ContourFitting algorithm object
  ##### Keyword Arguments
  - **ctr**: `integer()`.

    number of Fourier descriptors equal to number of contour points after resampling.

  - **fd**: `integer()`.

    Contour defining second shape (Target).

  ##### Return
  - **retval**: `Evision.XImgProc.ContourFitting.t()`

  Python prototype (for reference only):
  ```python3
  createContourFitting([, ctr[, fd]]) -> retval
  ```
  """
  @spec createContourFitting([{:ctr, term()} | {:fd, term()}] | nil) :: Evision.XImgProc.ContourFitting.t() | {:error, String.t()}
  def createContourFitting(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:ctr, :fd])
    positional = [
    ]
    :evision_nif.ximgproc_createContourFitting(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create ContourFitting algorithm object
  ##### Keyword Arguments
  - **ctr**: `integer()`.

    number of Fourier descriptors equal to number of contour points after resampling.

  - **fd**: `integer()`.

    Contour defining second shape (Target).

  ##### Return
  - **retval**: `Evision.XImgProc.ContourFitting.t()`

  Python prototype (for reference only):
  ```python3
  createContourFitting([, ctr[, fd]]) -> retval
  ```
  """
  @spec createContourFitting() :: Evision.XImgProc.ContourFitting.t() | {:error, String.t()}
  def createContourFitting() do
    positional = [
    ]
    :evision_nif.ximgproc_createContourFitting(positional)
    |> to_struct()
  end

  @doc """
  Factory method, create instance of DTFilter and produce initialization routines.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    guided image (used to build transformed distance, which describes edge structure of
    guided image).

  - **sigmaSpatial**: `double`.

    \\f${\\sigma}_H\\f$ parameter in the original article, it's similar to the sigma in the
    coordinate space into bilateralFilter.

  - **sigmaColor**: `double`.

    \\f${\\sigma}_r\\f$ parameter in the original article, it's similar to the sigma in the
    color space into bilateralFilter.

  ##### Keyword Arguments
  - **mode**: `integer()`.

    one form three modes DTF_NC, DTF_RF and DTF_IC which corresponds to three modes for
    filtering 2D signals in the article.

  - **numIters**: `integer()`.

    optional number of iterations used for filtering, 3 is quite enough.

  ##### Return
  - **retval**: `Evision.XImgProc.DTFilter.t()`

  For more details about Domain Transform filter parameters, see the original article @cite Gastal11 and
  [Domain Transform filter homepage](http://www.inf.ufrgs.br/~eslgastal/DomainTransform/).

  Python prototype (for reference only):
  ```python3
  createDTFilter(guide, sigmaSpatial, sigmaColor[, mode[, numIters]]) -> retval
  ```
  """
  @spec createDTFilter(Evision.Mat.maybe_mat_in(), number(), number(), [{:mode, term()} | {:numIters, term()}] | nil) :: Evision.XImgProc.DTFilter.t() | {:error, String.t()}
  def createDTFilter(guide, sigmaSpatial, sigmaColor, opts) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and is_number(sigmaSpatial) and is_number(sigmaColor) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mode, :numIters])
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      sigmaSpatial: Evision.Internal.Structurise.from_struct(sigmaSpatial),
      sigmaColor: Evision.Internal.Structurise.from_struct(sigmaColor)
    ]
    :evision_nif.ximgproc_createDTFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Factory method, create instance of DTFilter and produce initialization routines.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    guided image (used to build transformed distance, which describes edge structure of
    guided image).

  - **sigmaSpatial**: `double`.

    \\f${\\sigma}_H\\f$ parameter in the original article, it's similar to the sigma in the
    coordinate space into bilateralFilter.

  - **sigmaColor**: `double`.

    \\f${\\sigma}_r\\f$ parameter in the original article, it's similar to the sigma in the
    color space into bilateralFilter.

  ##### Keyword Arguments
  - **mode**: `integer()`.

    one form three modes DTF_NC, DTF_RF and DTF_IC which corresponds to three modes for
    filtering 2D signals in the article.

  - **numIters**: `integer()`.

    optional number of iterations used for filtering, 3 is quite enough.

  ##### Return
  - **retval**: `Evision.XImgProc.DTFilter.t()`

  For more details about Domain Transform filter parameters, see the original article @cite Gastal11 and
  [Domain Transform filter homepage](http://www.inf.ufrgs.br/~eslgastal/DomainTransform/).

  Python prototype (for reference only):
  ```python3
  createDTFilter(guide, sigmaSpatial, sigmaColor[, mode[, numIters]]) -> retval
  ```
  """
  @spec createDTFilter(Evision.Mat.maybe_mat_in(), number(), number()) :: Evision.XImgProc.DTFilter.t() | {:error, String.t()}
  def createDTFilter(guide, sigmaSpatial, sigmaColor) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and is_number(sigmaSpatial) and is_number(sigmaColor)
  do
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      sigmaSpatial: Evision.Internal.Structurise.from_struct(sigmaSpatial),
      sigmaColor: Evision.Internal.Structurise.from_struct(sigmaColor)
    ]
    :evision_nif.ximgproc_createDTFilter(positional)
    |> to_struct()
  end

  @doc """
  Convenience factory method that creates an instance of DisparityWLSFilter and sets up all the relevant
  filter parameters automatically based on the matcher instance. Currently supports only StereoBM and StereoSGBM.

  ##### Positional Arguments
  - **matcher_left**: `Evision.StereoMatcher`.

    stereo matcher instance that will be used with the filter

  ##### Return
  - **retval**: `Evision.XImgProc.DisparityWLSFilter.t()`

  Python prototype (for reference only):
  ```python3
  createDisparityWLSFilter(matcher_left) -> retval
  ```
  """
  @spec createDisparityWLSFilter(Evision.StereoMatcher.t()) :: Evision.XImgProc.DisparityWLSFilter.t() | {:error, String.t()}
  def createDisparityWLSFilter(matcher_left) when is_struct(matcher_left, Evision.StereoMatcher)
  do
    positional = [
      matcher_left: Evision.Internal.Structurise.from_struct(matcher_left)
    ]
    :evision_nif.ximgproc_createDisparityWLSFilter(positional)
    |> to_struct()
  end

  @doc """
  More generic factory method, create instance of DisparityWLSFilter and execute basic
  initialization routines. When using this method you will need to set-up the ROI, matchers and
  other parameters by yourself.

  ##### Positional Arguments
  - **use_confidence**: `bool`.

    filtering with confidence requires two disparity maps (for the left and right views) and is
    approximately two times slower. However, quality is typically significantly better.

  ##### Return
  - **retval**: `Evision.XImgProc.DisparityWLSFilter.t()`

  Python prototype (for reference only):
  ```python3
  createDisparityWLSFilterGeneric(use_confidence) -> retval
  ```
  """
  @spec createDisparityWLSFilterGeneric(boolean()) :: Evision.XImgProc.DisparityWLSFilter.t() | {:error, String.t()}
  def createDisparityWLSFilterGeneric(use_confidence) when is_boolean(use_confidence)
  do
    positional = [
      use_confidence: Evision.Internal.Structurise.from_struct(use_confidence)
    ]
    :evision_nif.ximgproc_createDisparityWLSFilterGeneric(positional)
    |> to_struct()
  end

  @doc """
  Factory method that creates an instance of the
  EdgeAwareInterpolator.

  ##### Return
  - **retval**: `Evision.XImgProc.EdgeAwareInterpolator.t()`

  Python prototype (for reference only):
  ```python3
  createEdgeAwareInterpolator() -> retval
  ```
  """
  @spec createEdgeAwareInterpolator() :: Evision.XImgProc.EdgeAwareInterpolator.t() | {:error, String.t()}
  def createEdgeAwareInterpolator() do
    positional = [
    ]
    :evision_nif.ximgproc_createEdgeAwareInterpolator(positional)
    |> to_struct()
  end

  @doc """
  Creates a Edgeboxes
  ##### Keyword Arguments
  - **alpha**: `float`.

    step size of sliding window search.

  - **beta**: `float`.

    nms threshold for object proposals.

  - **eta**: `float`.

    adaptation rate for nms threshold.

  - **minScore**: `float`.

    min score of boxes to detect.

  - **maxBoxes**: `integer()`.

    max number of boxes to detect.

  - **edgeMinMag**: `float`.

    edge min magnitude. Increase to trade off accuracy for speed.

  - **edgeMergeThr**: `float`.

    edge merge threshold. Increase to trade off accuracy for speed.

  - **clusterMinMag**: `float`.

    cluster min magnitude. Increase to trade off accuracy for speed.

  - **maxAspectRatio**: `float`.

    max aspect ratio of boxes.

  - **minBoxArea**: `float`.

    minimum area of boxes.

  - **gamma**: `float`.

    affinity sensitivity.

  - **kappa**: `float`.

    scale sensitivity.

  ##### Return
  - **retval**: `Evision.XImgProc.EdgeBoxes.t()`

  Python prototype (for reference only):
  ```python3
  createEdgeBoxes([, alpha[, beta[, eta[, minScore[, maxBoxes[, edgeMinMag[, edgeMergeThr[, clusterMinMag[, maxAspectRatio[, minBoxArea[, gamma[, kappa]]]]]]]]]]]]) -> retval
  ```
  """
  @spec createEdgeBoxes([{:alpha, term()} | {:beta, term()} | {:clusterMinMag, term()} | {:edgeMergeThr, term()} | {:edgeMinMag, term()} | {:eta, term()} | {:gamma, term()} | {:kappa, term()} | {:maxAspectRatio, term()} | {:maxBoxes, term()} | {:minBoxArea, term()} | {:minScore, term()}] | nil) :: Evision.XImgProc.EdgeBoxes.t() | {:error, String.t()}
  def createEdgeBoxes(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:alpha, :beta, :clusterMinMag, :edgeMergeThr, :edgeMinMag, :eta, :gamma, :kappa, :maxAspectRatio, :maxBoxes, :minBoxArea, :minScore])
    positional = [
    ]
    :evision_nif.ximgproc_createEdgeBoxes(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a Edgeboxes
  ##### Keyword Arguments
  - **alpha**: `float`.

    step size of sliding window search.

  - **beta**: `float`.

    nms threshold for object proposals.

  - **eta**: `float`.

    adaptation rate for nms threshold.

  - **minScore**: `float`.

    min score of boxes to detect.

  - **maxBoxes**: `integer()`.

    max number of boxes to detect.

  - **edgeMinMag**: `float`.

    edge min magnitude. Increase to trade off accuracy for speed.

  - **edgeMergeThr**: `float`.

    edge merge threshold. Increase to trade off accuracy for speed.

  - **clusterMinMag**: `float`.

    cluster min magnitude. Increase to trade off accuracy for speed.

  - **maxAspectRatio**: `float`.

    max aspect ratio of boxes.

  - **minBoxArea**: `float`.

    minimum area of boxes.

  - **gamma**: `float`.

    affinity sensitivity.

  - **kappa**: `float`.

    scale sensitivity.

  ##### Return
  - **retval**: `Evision.XImgProc.EdgeBoxes.t()`

  Python prototype (for reference only):
  ```python3
  createEdgeBoxes([, alpha[, beta[, eta[, minScore[, maxBoxes[, edgeMinMag[, edgeMergeThr[, clusterMinMag[, maxAspectRatio[, minBoxArea[, gamma[, kappa]]]]]]]]]]]]) -> retval
  ```
  """
  @spec createEdgeBoxes() :: Evision.XImgProc.EdgeBoxes.t() | {:error, String.t()}
  def createEdgeBoxes() do
    positional = [
    ]
    :evision_nif.ximgproc_createEdgeBoxes(positional)
    |> to_struct()
  end

  @doc """
  Creates a smart pointer to a EdgeDrawing object and initializes it
  ##### Return
  - **retval**: `Evision.XImgProc.EdgeDrawing.t()`

  Python prototype (for reference only):
  ```python3
  createEdgeDrawing() -> retval
  ```
  """
  @spec createEdgeDrawing() :: Evision.XImgProc.EdgeDrawing.t() | {:error, String.t()}
  def createEdgeDrawing() do
    positional = [
    ]
    :evision_nif.ximgproc_createEdgeDrawing(positional)
    |> to_struct()
  end

  @doc """
  Factory method, create instance of FastBilateralSolverFilter and execute the initialization routines.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    image serving as guide for filtering. It should have 8-bit depth and either 1 or 3 channels.

  - **sigma_spatial**: `double`.

    parameter, that is similar to spatial space sigma (bandwidth) in bilateralFilter.

  - **sigma_luma**: `double`.

    parameter, that is similar to luma space sigma (bandwidth) in bilateralFilter.

  - **sigma_chroma**: `double`.

    parameter, that is similar to chroma space sigma (bandwidth) in bilateralFilter.

  ##### Keyword Arguments
  - **lambda**: `double`.

    smoothness strength parameter for solver.

  - **num_iter**: `integer()`.

    number of iterations used for solver, 25 is usually enough.

  - **max_tol**: `double`.

    convergence tolerance used for solver.

  ##### Return
  - **retval**: `Evision.XImgProc.FastBilateralSolverFilter.t()`

  For more details about the Fast Bilateral Solver parameters, see the original paper @cite BarronPoole2016.

  Python prototype (for reference only):
  ```python3
  createFastBilateralSolverFilter(guide, sigma_spatial, sigma_luma, sigma_chroma[, lambda[, num_iter[, max_tol]]]) -> retval
  ```
  """
  @spec createFastBilateralSolverFilter(Evision.Mat.maybe_mat_in(), number(), number(), number(), [{:lambda, term()} | {:max_tol, term()} | {:num_iter, term()}] | nil) :: Evision.XImgProc.FastBilateralSolverFilter.t() | {:error, String.t()}
  def createFastBilateralSolverFilter(guide, sigma_spatial, sigma_luma, sigma_chroma, opts) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and is_number(sigma_spatial) and is_number(sigma_luma) and is_number(sigma_chroma) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:lambda, :max_tol, :num_iter])
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      sigma_spatial: Evision.Internal.Structurise.from_struct(sigma_spatial),
      sigma_luma: Evision.Internal.Structurise.from_struct(sigma_luma),
      sigma_chroma: Evision.Internal.Structurise.from_struct(sigma_chroma)
    ]
    :evision_nif.ximgproc_createFastBilateralSolverFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Factory method, create instance of FastBilateralSolverFilter and execute the initialization routines.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    image serving as guide for filtering. It should have 8-bit depth and either 1 or 3 channels.

  - **sigma_spatial**: `double`.

    parameter, that is similar to spatial space sigma (bandwidth) in bilateralFilter.

  - **sigma_luma**: `double`.

    parameter, that is similar to luma space sigma (bandwidth) in bilateralFilter.

  - **sigma_chroma**: `double`.

    parameter, that is similar to chroma space sigma (bandwidth) in bilateralFilter.

  ##### Keyword Arguments
  - **lambda**: `double`.

    smoothness strength parameter for solver.

  - **num_iter**: `integer()`.

    number of iterations used for solver, 25 is usually enough.

  - **max_tol**: `double`.

    convergence tolerance used for solver.

  ##### Return
  - **retval**: `Evision.XImgProc.FastBilateralSolverFilter.t()`

  For more details about the Fast Bilateral Solver parameters, see the original paper @cite BarronPoole2016.

  Python prototype (for reference only):
  ```python3
  createFastBilateralSolverFilter(guide, sigma_spatial, sigma_luma, sigma_chroma[, lambda[, num_iter[, max_tol]]]) -> retval
  ```
  """
  @spec createFastBilateralSolverFilter(Evision.Mat.maybe_mat_in(), number(), number(), number()) :: Evision.XImgProc.FastBilateralSolverFilter.t() | {:error, String.t()}
  def createFastBilateralSolverFilter(guide, sigma_spatial, sigma_luma, sigma_chroma) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and is_number(sigma_spatial) and is_number(sigma_luma) and is_number(sigma_chroma)
  do
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      sigma_spatial: Evision.Internal.Structurise.from_struct(sigma_spatial),
      sigma_luma: Evision.Internal.Structurise.from_struct(sigma_luma),
      sigma_chroma: Evision.Internal.Structurise.from_struct(sigma_chroma)
    ]
    :evision_nif.ximgproc_createFastBilateralSolverFilter(positional)
    |> to_struct()
  end

  @doc """
  Factory method, create instance of FastGlobalSmootherFilter and execute the initialization routines.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    image serving as guide for filtering. It should have 8-bit depth and either 1 or 3 channels.

  - **lambda**: `double`.

    parameter defining the amount of regularization

  - **sigma_color**: `double`.

    parameter, that is similar to color space sigma in bilateralFilter.

  ##### Keyword Arguments
  - **lambda_attenuation**: `double`.

    internal parameter, defining how much lambda decreases after each iteration. Normally,
    it should be 0.25. Setting it to 1.0 may lead to streaking artifacts.

  - **num_iter**: `integer()`.

    number of iterations used for filtering, 3 is usually enough.

  ##### Return
  - **retval**: `Evision.XImgProc.FastGlobalSmootherFilter.t()`

  For more details about Fast Global Smoother parameters, see the original paper @cite Min2014. However, please note that
  there are several differences. Lambda attenuation described in the paper is implemented a bit differently so do not
  expect the results to be identical to those from the paper; sigma_color values from the paper should be multiplied by 255.0 to
  achieve the same effect. Also, in case of image filtering where source and guide image are the same, authors
  propose to dynamically update the guide image after each iteration. To maximize the performance this feature
  was not implemented here.

  Python prototype (for reference only):
  ```python3
  createFastGlobalSmootherFilter(guide, lambda, sigma_color[, lambda_attenuation[, num_iter]]) -> retval
  ```
  """
  @spec createFastGlobalSmootherFilter(Evision.Mat.maybe_mat_in(), number(), number(), [{:lambda_attenuation, term()} | {:num_iter, term()}] | nil) :: Evision.XImgProc.FastGlobalSmootherFilter.t() | {:error, String.t()}
  def createFastGlobalSmootherFilter(guide, lambda, sigma_color, opts) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and is_number(lambda) and is_number(sigma_color) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:lambda_attenuation, :num_iter])
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      lambda: Evision.Internal.Structurise.from_struct(lambda),
      sigma_color: Evision.Internal.Structurise.from_struct(sigma_color)
    ]
    :evision_nif.ximgproc_createFastGlobalSmootherFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Factory method, create instance of FastGlobalSmootherFilter and execute the initialization routines.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    image serving as guide for filtering. It should have 8-bit depth and either 1 or 3 channels.

  - **lambda**: `double`.

    parameter defining the amount of regularization

  - **sigma_color**: `double`.

    parameter, that is similar to color space sigma in bilateralFilter.

  ##### Keyword Arguments
  - **lambda_attenuation**: `double`.

    internal parameter, defining how much lambda decreases after each iteration. Normally,
    it should be 0.25. Setting it to 1.0 may lead to streaking artifacts.

  - **num_iter**: `integer()`.

    number of iterations used for filtering, 3 is usually enough.

  ##### Return
  - **retval**: `Evision.XImgProc.FastGlobalSmootherFilter.t()`

  For more details about Fast Global Smoother parameters, see the original paper @cite Min2014. However, please note that
  there are several differences. Lambda attenuation described in the paper is implemented a bit differently so do not
  expect the results to be identical to those from the paper; sigma_color values from the paper should be multiplied by 255.0 to
  achieve the same effect. Also, in case of image filtering where source and guide image are the same, authors
  propose to dynamically update the guide image after each iteration. To maximize the performance this feature
  was not implemented here.

  Python prototype (for reference only):
  ```python3
  createFastGlobalSmootherFilter(guide, lambda, sigma_color[, lambda_attenuation[, num_iter]]) -> retval
  ```
  """
  @spec createFastGlobalSmootherFilter(Evision.Mat.maybe_mat_in(), number(), number()) :: Evision.XImgProc.FastGlobalSmootherFilter.t() | {:error, String.t()}
  def createFastGlobalSmootherFilter(guide, lambda, sigma_color) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and is_number(lambda) and is_number(sigma_color)
  do
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      lambda: Evision.Internal.Structurise.from_struct(lambda),
      sigma_color: Evision.Internal.Structurise.from_struct(sigma_color)
    ]
    :evision_nif.ximgproc_createFastGlobalSmootherFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates a smart pointer to a FastLineDetector object and initializes it
  ##### Keyword Arguments
  - **length_threshold**: `integer()`.

    Segment shorter than this will be discarded

  - **distance_threshold**: `float`.

    A point placed from a hypothesis line
    segment farther than this will be regarded as an outlier

  - **canny_th1**: `double`.

    First threshold for hysteresis procedure in Canny()

  - **canny_th2**: `double`.

    Second threshold for hysteresis procedure in Canny()

  - **canny_aperture_size**: `integer()`.

    Aperturesize for the sobel operator in Canny().
    If zero, Canny() is not applied and the input image is taken as an edge image.

  - **do_merge**: `bool`.

    If true, incremental merging of segments will be performed

  ##### Return
  - **retval**: `Evision.XImgProc.FastLineDetector.t()`

  Python prototype (for reference only):
  ```python3
  createFastLineDetector([, length_threshold[, distance_threshold[, canny_th1[, canny_th2[, canny_aperture_size[, do_merge]]]]]]) -> retval
  ```
  """
  @spec createFastLineDetector([{:canny_aperture_size, term()} | {:canny_th1, term()} | {:canny_th2, term()} | {:distance_threshold, term()} | {:do_merge, term()} | {:length_threshold, term()}] | nil) :: Evision.XImgProc.FastLineDetector.t() | {:error, String.t()}
  def createFastLineDetector(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:canny_aperture_size, :canny_th1, :canny_th2, :distance_threshold, :do_merge, :length_threshold])
    positional = [
    ]
    :evision_nif.ximgproc_createFastLineDetector(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a smart pointer to a FastLineDetector object and initializes it
  ##### Keyword Arguments
  - **length_threshold**: `integer()`.

    Segment shorter than this will be discarded

  - **distance_threshold**: `float`.

    A point placed from a hypothesis line
    segment farther than this will be regarded as an outlier

  - **canny_th1**: `double`.

    First threshold for hysteresis procedure in Canny()

  - **canny_th2**: `double`.

    Second threshold for hysteresis procedure in Canny()

  - **canny_aperture_size**: `integer()`.

    Aperturesize for the sobel operator in Canny().
    If zero, Canny() is not applied and the input image is taken as an edge image.

  - **do_merge**: `bool`.

    If true, incremental merging of segments will be performed

  ##### Return
  - **retval**: `Evision.XImgProc.FastLineDetector.t()`

  Python prototype (for reference only):
  ```python3
  createFastLineDetector([, length_threshold[, distance_threshold[, canny_th1[, canny_th2[, canny_aperture_size[, do_merge]]]]]]) -> retval
  ```
  """
  @spec createFastLineDetector() :: Evision.XImgProc.FastLineDetector.t() | {:error, String.t()}
  def createFastLineDetector() do
    positional = [
    ]
    :evision_nif.ximgproc_createFastLineDetector(positional)
    |> to_struct()
  end

  @doc """
  Factory method, create instance of GuidedFilter and produce initialization routines.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    guided image (or array of images) with up to 3 channels, if it have more then 3
    channels then only first 3 channels will be used.

  - **radius**: `integer()`.

    radius of Guided Filter.

  - **eps**: `double`.

    regularization term of Guided Filter. \\f${eps}^2\\f$ is similar to the sigma in the color
    space into bilateralFilter.

  ##### Keyword Arguments
  - **scale**: `double`.

    subsample factor of Fast Guided Filter, use a scale less than 1 to speeds up computation
    with almost no visible degradation. (e.g. scale==0.5 shrinks the image by 2x inside the filter)

  ##### Return
  - **retval**: `Evision.XImgProc.GuidedFilter.t()`

  For more details about (Fast) Guided Filter parameters, see the original articles @cite Kaiming10 @cite Kaiming15 .

  Python prototype (for reference only):
  ```python3
  createGuidedFilter(guide, radius, eps[, scale]) -> retval
  ```
  """
  @spec createGuidedFilter(Evision.Mat.maybe_mat_in(), integer(), number(), [{:scale, term()}] | nil) :: Evision.XImgProc.GuidedFilter.t() | {:error, String.t()}
  def createGuidedFilter(guide, radius, eps, opts) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and is_integer(radius) and is_number(eps) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:scale])
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      radius: Evision.Internal.Structurise.from_struct(radius),
      eps: Evision.Internal.Structurise.from_struct(eps)
    ]
    :evision_nif.ximgproc_createGuidedFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Factory method, create instance of GuidedFilter and produce initialization routines.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    guided image (or array of images) with up to 3 channels, if it have more then 3
    channels then only first 3 channels will be used.

  - **radius**: `integer()`.

    radius of Guided Filter.

  - **eps**: `double`.

    regularization term of Guided Filter. \\f${eps}^2\\f$ is similar to the sigma in the color
    space into bilateralFilter.

  ##### Keyword Arguments
  - **scale**: `double`.

    subsample factor of Fast Guided Filter, use a scale less than 1 to speeds up computation
    with almost no visible degradation. (e.g. scale==0.5 shrinks the image by 2x inside the filter)

  ##### Return
  - **retval**: `Evision.XImgProc.GuidedFilter.t()`

  For more details about (Fast) Guided Filter parameters, see the original articles @cite Kaiming10 @cite Kaiming15 .

  Python prototype (for reference only):
  ```python3
  createGuidedFilter(guide, radius, eps[, scale]) -> retval
  ```
  """
  @spec createGuidedFilter(Evision.Mat.maybe_mat_in(), integer(), number()) :: Evision.XImgProc.GuidedFilter.t() | {:error, String.t()}
  def createGuidedFilter(guide, radius, eps) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and is_integer(radius) and is_number(eps)
  do
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      radius: Evision.Internal.Structurise.from_struct(radius),
      eps: Evision.Internal.Structurise.from_struct(eps)
    ]
    :evision_nif.ximgproc_createGuidedFilter(positional)
    |> to_struct()
  end

  @doc """
  creates a quaternion image.

  ##### Positional Arguments
  - **img**: `Evision.Mat`

  ##### Return
  - **qimg**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  createQuaternionImage(img[, qimg]) -> qimg
  ```
  """
  @spec createQuaternionImage(Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def createQuaternionImage(img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.ximgproc_createQuaternionImage(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  creates a quaternion image.

  ##### Positional Arguments
  - **img**: `Evision.Mat`

  ##### Return
  - **qimg**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  createQuaternionImage(img[, qimg]) -> qimg
  ```
  """
  @spec createQuaternionImage(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def createQuaternionImage(img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.ximgproc_createQuaternionImage(positional)
    |> to_struct()
  end

  @doc """
  createRFFeatureGetter
  ##### Return
  - **retval**: `Evision.XImgProc.RFFeatureGetter.t()`

  Python prototype (for reference only):
  ```python3
  createRFFeatureGetter() -> retval
  ```
  """
  @spec createRFFeatureGetter() :: Evision.XImgProc.RFFeatureGetter.t() | {:error, String.t()}
  def createRFFeatureGetter() do
    positional = [
    ]
    :evision_nif.ximgproc_createRFFeatureGetter(positional)
    |> to_struct()
  end

  @doc """
  Factory method that creates an instance of the
  RICInterpolator.

  ##### Return
  - **retval**: `Evision.XImgProc.RICInterpolator.t()`

  Python prototype (for reference only):
  ```python3
  createRICInterpolator() -> retval
  ```
  """
  @spec createRICInterpolator() :: Evision.XImgProc.RICInterpolator.t() | {:error, String.t()}
  def createRICInterpolator() do
    positional = [
    ]
    :evision_nif.ximgproc_createRICInterpolator(positional)
    |> to_struct()
  end

  @doc """
  Convenience method to set up the matcher for computing the right-view disparity map
  that is required in case of filtering with confidence.

  ##### Positional Arguments
  - **matcher_left**: `Evision.StereoMatcher`.

    main stereo matcher instance that will be used with the filter

  ##### Return
  - **retval**: `Evision.StereoMatcher.t()`

  Python prototype (for reference only):
  ```python3
  createRightMatcher(matcher_left) -> retval
  ```
  """
  @spec createRightMatcher(Evision.StereoMatcher.t()) :: Evision.StereoMatcher.t() | {:error, String.t()}
  def createRightMatcher(matcher_left) when is_struct(matcher_left, Evision.StereoMatcher)
  do
    positional = [
      matcher_left: Evision.Internal.Structurise.from_struct(matcher_left)
    ]
    :evision_nif.ximgproc_createRightMatcher(positional)
    |> to_struct()
  end

  @doc """
  Initializes a ScanSegment object.

  ##### Positional Arguments
  - **image_width**: `integer()`.

    Image width.

  - **image_height**: `integer()`.

    Image height.

  - **num_superpixels**: `integer()`.

    Desired number of superpixels. Note that the actual number may be smaller
    due to restrictions (depending on the image size). Use getNumberOfSuperpixels() to
    get the actual number.

  ##### Keyword Arguments
  - **slices**: `integer()`.

    Number of processing threads for parallelisation. Setting -1 uses the maximum number
    of threads. In practice, four threads is enough for smaller images and eight threads for larger ones.

  - **merge_small**: `bool`.

    merge small segments to give the desired number of superpixels. Processing is
    much faster without merging, but many small segments will be left in the image.

  ##### Return
  - **retval**: `Evision.XImgProc.ScanSegment.t()`

  The function initializes a ScanSegment object for the input image. It stores the parameters of
  the image: image_width and image_height. It also sets the parameters of the F-DBSCAN superpixel
  algorithm, which are: num_superpixels, threads, and merge_small.

  Python prototype (for reference only):
  ```python3
  createScanSegment(image_width, image_height, num_superpixels[, slices[, merge_small]]) -> retval
  ```
  """
  @spec createScanSegment(integer(), integer(), integer(), [{:merge_small, term()} | {:slices, term()}] | nil) :: Evision.XImgProc.ScanSegment.t() | {:error, String.t()}
  def createScanSegment(image_width, image_height, num_superpixels, opts) when is_integer(image_width) and is_integer(image_height) and is_integer(num_superpixels) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:merge_small, :slices])
    positional = [
      image_width: Evision.Internal.Structurise.from_struct(image_width),
      image_height: Evision.Internal.Structurise.from_struct(image_height),
      num_superpixels: Evision.Internal.Structurise.from_struct(num_superpixels)
    ]
    :evision_nif.ximgproc_createScanSegment(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Initializes a ScanSegment object.

  ##### Positional Arguments
  - **image_width**: `integer()`.

    Image width.

  - **image_height**: `integer()`.

    Image height.

  - **num_superpixels**: `integer()`.

    Desired number of superpixels. Note that the actual number may be smaller
    due to restrictions (depending on the image size). Use getNumberOfSuperpixels() to
    get the actual number.

  ##### Keyword Arguments
  - **slices**: `integer()`.

    Number of processing threads for parallelisation. Setting -1 uses the maximum number
    of threads. In practice, four threads is enough for smaller images and eight threads for larger ones.

  - **merge_small**: `bool`.

    merge small segments to give the desired number of superpixels. Processing is
    much faster without merging, but many small segments will be left in the image.

  ##### Return
  - **retval**: `Evision.XImgProc.ScanSegment.t()`

  The function initializes a ScanSegment object for the input image. It stores the parameters of
  the image: image_width and image_height. It also sets the parameters of the F-DBSCAN superpixel
  algorithm, which are: num_superpixels, threads, and merge_small.

  Python prototype (for reference only):
  ```python3
  createScanSegment(image_width, image_height, num_superpixels[, slices[, merge_small]]) -> retval
  ```
  """
  @spec createScanSegment(integer(), integer(), integer()) :: Evision.XImgProc.ScanSegment.t() | {:error, String.t()}
  def createScanSegment(image_width, image_height, num_superpixels) when is_integer(image_width) and is_integer(image_height) and is_integer(num_superpixels)
  do
    positional = [
      image_width: Evision.Internal.Structurise.from_struct(image_width),
      image_height: Evision.Internal.Structurise.from_struct(image_height),
      num_superpixels: Evision.Internal.Structurise.from_struct(num_superpixels)
    ]
    :evision_nif.ximgproc_createScanSegment(positional)
    |> to_struct()
  end

  @doc """
  createStructuredEdgeDetection

  ##### Positional Arguments
  - **model**: `String`

  ##### Keyword Arguments
  - **howToGetFeatures**: `Evision.XImgProc.RFFeatureGetter.t()`.

  ##### Return
  - **retval**: `Evision.XImgProc.StructuredEdgeDetection.t()`

  Python prototype (for reference only):
  ```python3
  createStructuredEdgeDetection(model[, howToGetFeatures]) -> retval
  ```
  """
  @spec createStructuredEdgeDetection(binary(), [{:howToGetFeatures, term()}] | nil) :: Evision.XImgProc.StructuredEdgeDetection.t() | {:error, String.t()}
  def createStructuredEdgeDetection(model, opts) when is_binary(model) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:howToGetFeatures])
    positional = [
      model: Evision.Internal.Structurise.from_struct(model)
    ]
    :evision_nif.ximgproc_createStructuredEdgeDetection(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  createStructuredEdgeDetection

  ##### Positional Arguments
  - **model**: `String`

  ##### Keyword Arguments
  - **howToGetFeatures**: `Evision.XImgProc.RFFeatureGetter.t()`.

  ##### Return
  - **retval**: `Evision.XImgProc.StructuredEdgeDetection.t()`

  Python prototype (for reference only):
  ```python3
  createStructuredEdgeDetection(model[, howToGetFeatures]) -> retval
  ```
  """
  @spec createStructuredEdgeDetection(binary()) :: Evision.XImgProc.StructuredEdgeDetection.t() | {:error, String.t()}
  def createStructuredEdgeDetection(model) when is_binary(model)
  do
    positional = [
      model: Evision.Internal.Structurise.from_struct(model)
    ]
    :evision_nif.ximgproc_createStructuredEdgeDetection(positional)
    |> to_struct()
  end

  @doc """
  Class implementing the LSC (Linear Spectral Clustering) superpixels

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    Image to segment

  ##### Keyword Arguments
  - **region_size**: `integer()`.

    Chooses an average superpixel size measured in pixels

  - **ratio**: `float`.

    Chooses the enforcement of superpixel compactness factor of superpixel

  ##### Return
  - **retval**: `Evision.XImgProc.SuperpixelLSC.t()`

  The function initializes a SuperpixelLSC object for the input image. It sets the parameters of
  superpixel algorithm, which are: region_size and ruler. It preallocate some buffers for future
  computing iterations over the given image. An example of LSC is ilustrated in the following picture.
  For enanched results it is recommended for color images to preprocess image with little gaussian blur
  with a small 3 x 3 kernel and additional conversion into CieLAB color space.
  ![image](pics/superpixels_lsc.png)

  Python prototype (for reference only):
  ```python3
  createSuperpixelLSC(image[, region_size[, ratio]]) -> retval
  ```
  """
  @spec createSuperpixelLSC(Evision.Mat.maybe_mat_in(), [{:ratio, term()} | {:region_size, term()}] | nil) :: Evision.XImgProc.SuperpixelLSC.t() | {:error, String.t()}
  def createSuperpixelLSC(image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:ratio, :region_size])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.ximgproc_createSuperpixelLSC(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Class implementing the LSC (Linear Spectral Clustering) superpixels

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    Image to segment

  ##### Keyword Arguments
  - **region_size**: `integer()`.

    Chooses an average superpixel size measured in pixels

  - **ratio**: `float`.

    Chooses the enforcement of superpixel compactness factor of superpixel

  ##### Return
  - **retval**: `Evision.XImgProc.SuperpixelLSC.t()`

  The function initializes a SuperpixelLSC object for the input image. It sets the parameters of
  superpixel algorithm, which are: region_size and ruler. It preallocate some buffers for future
  computing iterations over the given image. An example of LSC is ilustrated in the following picture.
  For enanched results it is recommended for color images to preprocess image with little gaussian blur
  with a small 3 x 3 kernel and additional conversion into CieLAB color space.
  ![image](pics/superpixels_lsc.png)

  Python prototype (for reference only):
  ```python3
  createSuperpixelLSC(image[, region_size[, ratio]]) -> retval
  ```
  """
  @spec createSuperpixelLSC(Evision.Mat.maybe_mat_in()) :: Evision.XImgProc.SuperpixelLSC.t() | {:error, String.t()}
  def createSuperpixelLSC(image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.ximgproc_createSuperpixelLSC(positional)
    |> to_struct()
  end

  @doc """
  Initializes a SuperpixelSEEDS object.

  ##### Positional Arguments
  - **image_width**: `integer()`.

    Image width.

  - **image_height**: `integer()`.

    Image height.

  - **image_channels**: `integer()`.

    Number of channels of the image.

  - **num_superpixels**: `integer()`.

    Desired number of superpixels. Note that the actual number may be smaller
    due to restrictions (depending on the image size and num_levels). Use getNumberOfSuperpixels() to
    get the actual number.

  - **num_levels**: `integer()`.

    Number of block levels. The more levels, the more accurate is the segmentation,
    but needs more memory and CPU time.

  ##### Keyword Arguments
  - **prior**: `integer()`.

    enable 3x3 shape smoothing term if \\>0. A larger value leads to smoother shapes. prior
    must be in the range [0, 5].

  - **histogram_bins**: `integer()`.

    Number of histogram bins.

  - **double_step**: `bool`.

    If true, iterate each block level twice for higher accuracy.

  ##### Return
  - **retval**: `Evision.XImgProc.SuperpixelSEEDS.t()`

  The function initializes a SuperpixelSEEDS object for the input image. It stores the parameters of
  the image: image_width, image_height and image_channels. It also sets the parameters of the SEEDS
  superpixel algorithm, which are: num_superpixels, num_levels, use_prior, histogram_bins and
  double_step.
  The number of levels in num_levels defines the amount of block levels that the algorithm use in the
  optimization. The initialization is a grid, in which the superpixels are equally distributed through
  the width and the height of the image. The larger blocks correspond to the superpixel size, and the
  levels with smaller blocks are formed by dividing the larger blocks into 2 x 2 blocks of pixels,
  recursively until the smaller block level. An example of initialization of 4 block levels is
  illustrated in the following figure.
  ![image](pics/superpixels_blocks.png)

  Python prototype (for reference only):
  ```python3
  createSuperpixelSEEDS(image_width, image_height, image_channels, num_superpixels, num_levels[, prior[, histogram_bins[, double_step]]]) -> retval
  ```
  """
  @spec createSuperpixelSEEDS(integer(), integer(), integer(), integer(), integer(), [{:double_step, term()} | {:histogram_bins, term()} | {:prior, term()}] | nil) :: Evision.XImgProc.SuperpixelSEEDS.t() | {:error, String.t()}
  def createSuperpixelSEEDS(image_width, image_height, image_channels, num_superpixels, num_levels, opts) when is_integer(image_width) and is_integer(image_height) and is_integer(image_channels) and is_integer(num_superpixels) and is_integer(num_levels) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:double_step, :histogram_bins, :prior])
    positional = [
      image_width: Evision.Internal.Structurise.from_struct(image_width),
      image_height: Evision.Internal.Structurise.from_struct(image_height),
      image_channels: Evision.Internal.Structurise.from_struct(image_channels),
      num_superpixels: Evision.Internal.Structurise.from_struct(num_superpixels),
      num_levels: Evision.Internal.Structurise.from_struct(num_levels)
    ]
    :evision_nif.ximgproc_createSuperpixelSEEDS(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Initializes a SuperpixelSEEDS object.

  ##### Positional Arguments
  - **image_width**: `integer()`.

    Image width.

  - **image_height**: `integer()`.

    Image height.

  - **image_channels**: `integer()`.

    Number of channels of the image.

  - **num_superpixels**: `integer()`.

    Desired number of superpixels. Note that the actual number may be smaller
    due to restrictions (depending on the image size and num_levels). Use getNumberOfSuperpixels() to
    get the actual number.

  - **num_levels**: `integer()`.

    Number of block levels. The more levels, the more accurate is the segmentation,
    but needs more memory and CPU time.

  ##### Keyword Arguments
  - **prior**: `integer()`.

    enable 3x3 shape smoothing term if \\>0. A larger value leads to smoother shapes. prior
    must be in the range [0, 5].

  - **histogram_bins**: `integer()`.

    Number of histogram bins.

  - **double_step**: `bool`.

    If true, iterate each block level twice for higher accuracy.

  ##### Return
  - **retval**: `Evision.XImgProc.SuperpixelSEEDS.t()`

  The function initializes a SuperpixelSEEDS object for the input image. It stores the parameters of
  the image: image_width, image_height and image_channels. It also sets the parameters of the SEEDS
  superpixel algorithm, which are: num_superpixels, num_levels, use_prior, histogram_bins and
  double_step.
  The number of levels in num_levels defines the amount of block levels that the algorithm use in the
  optimization. The initialization is a grid, in which the superpixels are equally distributed through
  the width and the height of the image. The larger blocks correspond to the superpixel size, and the
  levels with smaller blocks are formed by dividing the larger blocks into 2 x 2 blocks of pixels,
  recursively until the smaller block level. An example of initialization of 4 block levels is
  illustrated in the following figure.
  ![image](pics/superpixels_blocks.png)

  Python prototype (for reference only):
  ```python3
  createSuperpixelSEEDS(image_width, image_height, image_channels, num_superpixels, num_levels[, prior[, histogram_bins[, double_step]]]) -> retval
  ```
  """
  @spec createSuperpixelSEEDS(integer(), integer(), integer(), integer(), integer()) :: Evision.XImgProc.SuperpixelSEEDS.t() | {:error, String.t()}
  def createSuperpixelSEEDS(image_width, image_height, image_channels, num_superpixels, num_levels) when is_integer(image_width) and is_integer(image_height) and is_integer(image_channels) and is_integer(num_superpixels) and is_integer(num_levels)
  do
    positional = [
      image_width: Evision.Internal.Structurise.from_struct(image_width),
      image_height: Evision.Internal.Structurise.from_struct(image_height),
      image_channels: Evision.Internal.Structurise.from_struct(image_channels),
      num_superpixels: Evision.Internal.Structurise.from_struct(num_superpixels),
      num_levels: Evision.Internal.Structurise.from_struct(num_levels)
    ]
    :evision_nif.ximgproc_createSuperpixelSEEDS(positional)
    |> to_struct()
  end

  @doc """
  Initialize a SuperpixelSLIC object

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    Image to segment

  ##### Keyword Arguments
  - **algorithm**: `integer()`.

    Chooses the algorithm variant to use:
    SLIC segments image using a desired region_size, and in addition SLICO will optimize using adaptive compactness factor,
    while MSLIC will optimize using manifold methods resulting in more content-sensitive superpixels.

  - **region_size**: `integer()`.

    Chooses an average superpixel size measured in pixels

  - **ruler**: `float`.

    Chooses the enforcement of superpixel smoothness factor of superpixel

  ##### Return
  - **retval**: `Evision.XImgProc.SuperpixelSLIC.t()`

  The function initializes a SuperpixelSLIC object for the input image. It sets the parameters of choosed
  superpixel algorithm, which are: region_size and ruler. It preallocate some buffers for future
  computing iterations over the given image. For enanched results it is recommended for color images to
  preprocess image with little gaussian blur using a small 3 x 3 kernel and additional conversion into
  CieLAB color space. An example of SLIC versus SLICO and MSLIC is ilustrated in the following picture.
  ![image](pics/superpixels_slic.png)

  Python prototype (for reference only):
  ```python3
  createSuperpixelSLIC(image[, algorithm[, region_size[, ruler]]]) -> retval
  ```
  """
  @spec createSuperpixelSLIC(Evision.Mat.maybe_mat_in(), [{:algorithm, term()} | {:region_size, term()} | {:ruler, term()}] | nil) :: Evision.XImgProc.SuperpixelSLIC.t() | {:error, String.t()}
  def createSuperpixelSLIC(image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:algorithm, :region_size, :ruler])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.ximgproc_createSuperpixelSLIC(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Initialize a SuperpixelSLIC object

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    Image to segment

  ##### Keyword Arguments
  - **algorithm**: `integer()`.

    Chooses the algorithm variant to use:
    SLIC segments image using a desired region_size, and in addition SLICO will optimize using adaptive compactness factor,
    while MSLIC will optimize using manifold methods resulting in more content-sensitive superpixels.

  - **region_size**: `integer()`.

    Chooses an average superpixel size measured in pixels

  - **ruler**: `float`.

    Chooses the enforcement of superpixel smoothness factor of superpixel

  ##### Return
  - **retval**: `Evision.XImgProc.SuperpixelSLIC.t()`

  The function initializes a SuperpixelSLIC object for the input image. It sets the parameters of choosed
  superpixel algorithm, which are: region_size and ruler. It preallocate some buffers for future
  computing iterations over the given image. For enanched results it is recommended for color images to
  preprocess image with little gaussian blur using a small 3 x 3 kernel and additional conversion into
  CieLAB color space. An example of SLIC versus SLICO and MSLIC is ilustrated in the following picture.
  ![image](pics/superpixels_slic.png)

  Python prototype (for reference only):
  ```python3
  createSuperpixelSLIC(image[, algorithm[, region_size[, ruler]]]) -> retval
  ```
  """
  @spec createSuperpixelSLIC(Evision.Mat.maybe_mat_in()) :: Evision.XImgProc.SuperpixelSLIC.t() | {:error, String.t()}
  def createSuperpixelSLIC(image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.ximgproc_createSuperpixelSLIC(positional)
    |> to_struct()
  end

  @doc """
  Simple one-line Domain Transform filter call. If you have multiple images to filter with the same
  guided image then use DTFilter interface to avoid extra computations on initialization stage.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    guided image (also called as joint image) with unsigned 8-bit or floating-point 32-bit
    depth and up to 4 channels.

  - **src**: `Evision.Mat`.

    filtering image with unsigned 8-bit or floating-point 32-bit depth and up to 4 channels.

  - **sigmaSpatial**: `double`.

    \\f${\\sigma}_H\\f$ parameter in the original article, it's similar to the sigma in the
    coordinate space into bilateralFilter.

  - **sigmaColor**: `double`.

    \\f${\\sigma}_r\\f$ parameter in the original article, it's similar to the sigma in the
    color space into bilateralFilter.

  ##### Keyword Arguments
  - **mode**: `integer()`.

    one form three modes DTF_NC, DTF_RF and DTF_IC which corresponds to three modes for
    filtering 2D signals in the article.

  - **numIters**: `integer()`.

    optional number of iterations used for filtering, 3 is quite enough.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image

  @sa bilateralFilter, guidedFilter, amFilter

  Python prototype (for reference only):
  ```python3
  dtFilter(guide, src, sigmaSpatial, sigmaColor[, dst[, mode[, numIters]]]) -> dst
  ```
  """
  @spec dtFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), number(), [{:mode, term()} | {:numIters, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def dtFilter(guide, src, sigmaSpatial, sigmaColor, opts) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(sigmaSpatial) and is_number(sigmaColor) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mode, :numIters])
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      src: Evision.Internal.Structurise.from_struct(src),
      sigmaSpatial: Evision.Internal.Structurise.from_struct(sigmaSpatial),
      sigmaColor: Evision.Internal.Structurise.from_struct(sigmaColor)
    ]
    :evision_nif.ximgproc_dtFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Simple one-line Domain Transform filter call. If you have multiple images to filter with the same
  guided image then use DTFilter interface to avoid extra computations on initialization stage.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    guided image (also called as joint image) with unsigned 8-bit or floating-point 32-bit
    depth and up to 4 channels.

  - **src**: `Evision.Mat`.

    filtering image with unsigned 8-bit or floating-point 32-bit depth and up to 4 channels.

  - **sigmaSpatial**: `double`.

    \\f${\\sigma}_H\\f$ parameter in the original article, it's similar to the sigma in the
    coordinate space into bilateralFilter.

  - **sigmaColor**: `double`.

    \\f${\\sigma}_r\\f$ parameter in the original article, it's similar to the sigma in the
    color space into bilateralFilter.

  ##### Keyword Arguments
  - **mode**: `integer()`.

    one form three modes DTF_NC, DTF_RF and DTF_IC which corresponds to three modes for
    filtering 2D signals in the article.

  - **numIters**: `integer()`.

    optional number of iterations used for filtering, 3 is quite enough.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image

  @sa bilateralFilter, guidedFilter, amFilter

  Python prototype (for reference only):
  ```python3
  dtFilter(guide, src, sigmaSpatial, sigmaColor[, dst[, mode[, numIters]]]) -> dst
  ```
  """
  @spec dtFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def dtFilter(guide, src, sigmaSpatial, sigmaColor) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(sigmaSpatial) and is_number(sigmaColor)
  do
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      src: Evision.Internal.Structurise.from_struct(src),
      sigmaSpatial: Evision.Internal.Structurise.from_struct(sigmaSpatial),
      sigmaColor: Evision.Internal.Structurise.from_struct(sigmaColor)
    ]
    :evision_nif.ximgproc_dtFilter(positional)
    |> to_struct()
  end

  @doc """
  Smoothes an image using the Edge-Preserving filter.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source 8-bit 3-channel image.

  - **d**: `integer()`.

    Diameter of each pixel neighborhood that is used during filtering. Must be greater or equal 3.

  - **threshold**: `double`.

    Threshold, which distinguishes between noise, outliers, and data.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and type as src.

   The function smoothes Gaussian noise as well as salt & pepper noise.
   For more details about this implementation, please see
   [ReiWoe18]  Reich, S. and Wörgötter, F. and Dellen, B. (2018). A Real-Time Edge-Preserving Denoising Filter. Proceedings of the 13th International Joint Conference on Computer Vision, Imaging and Computer Graphics Theory and Applications (VISIGRAPP): Visapp, 85-94, 4. DOI: 10.5220/0006509000850094.

  Python prototype (for reference only):
  ```python3
  edgePreservingFilter(src, d, threshold[, dst]) -> dst
  ```
  """
  @spec edgePreservingFilter(Evision.Mat.maybe_mat_in(), integer(), number(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def edgePreservingFilter(src, d, threshold, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(d) and is_number(threshold) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      d: Evision.Internal.Structurise.from_struct(d),
      threshold: Evision.Internal.Structurise.from_struct(threshold)
    ]
    :evision_nif.ximgproc_edgePreservingFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Smoothes an image using the Edge-Preserving filter.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source 8-bit 3-channel image.

  - **d**: `integer()`.

    Diameter of each pixel neighborhood that is used during filtering. Must be greater or equal 3.

  - **threshold**: `double`.

    Threshold, which distinguishes between noise, outliers, and data.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and type as src.

   The function smoothes Gaussian noise as well as salt & pepper noise.
   For more details about this implementation, please see
   [ReiWoe18]  Reich, S. and Wörgötter, F. and Dellen, B. (2018). A Real-Time Edge-Preserving Denoising Filter. Proceedings of the 13th International Joint Conference on Computer Vision, Imaging and Computer Graphics Theory and Applications (VISIGRAPP): Visapp, 85-94, 4. DOI: 10.5220/0006509000850094.

  Python prototype (for reference only):
  ```python3
  edgePreservingFilter(src, d, threshold[, dst]) -> dst
  ```
  """
  @spec edgePreservingFilter(Evision.Mat.maybe_mat_in(), integer(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def edgePreservingFilter(src, d, threshold) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(d) and is_number(threshold)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      d: Evision.Internal.Structurise.from_struct(d),
      threshold: Evision.Internal.Structurise.from_struct(threshold)
    ]
    :evision_nif.ximgproc_edgePreservingFilter(positional)
    |> to_struct()
  end

  @doc """
  Simple one-line Fast Bilateral Solver filter call. If you have multiple images to filter with the same
  guide then use FastBilateralSolverFilter interface to avoid extra computations.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    image serving as guide for filtering. It should have 8-bit depth and either 1 or 3 channels.

  - **src**: `Evision.Mat`.

    source image for filtering with unsigned 8-bit or signed 16-bit or floating-point 32-bit depth and up to 4 channels.

  - **confidence**: `Evision.Mat`.

    confidence image with unsigned 8-bit or floating-point 32-bit confidence and 1 channel.

  ##### Keyword Arguments
  - **sigma_spatial**: `double`.

    parameter, that is similar to spatial space sigma (bandwidth) in bilateralFilter.

  - **sigma_luma**: `double`.

    parameter, that is similar to luma space sigma (bandwidth) in bilateralFilter.

  - **sigma_chroma**: `double`.

    parameter, that is similar to chroma space sigma (bandwidth) in bilateralFilter.

  - **lambda**: `double`.

    smoothness strength parameter for solver.

  - **num_iter**: `integer()`.

    number of iterations used for solver, 25 is usually enough.

  - **max_tol**: `double`.

    convergence tolerance used for solver.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image.

  For more details about the Fast Bilateral Solver parameters, see the original paper @cite BarronPoole2016.
  **Note**: Confidence images with CV_8U depth are expected to in [0, 255] and CV_32F in [0, 1] range.

  Python prototype (for reference only):
  ```python3
  fastBilateralSolverFilter(guide, src, confidence[, dst[, sigma_spatial[, sigma_luma[, sigma_chroma[, lambda[, num_iter[, max_tol]]]]]]]) -> dst
  ```
  """
  @spec fastBilateralSolverFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:lambda, term()} | {:max_tol, term()} | {:num_iter, term()} | {:sigma_chroma, term()} | {:sigma_luma, term()} | {:sigma_spatial, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def fastBilateralSolverFilter(guide, src, confidence, opts) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(confidence, Evision.Mat) or is_struct(confidence, Nx.Tensor) or is_number(confidence) or is_tuple(confidence)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:lambda, :max_tol, :num_iter, :sigma_chroma, :sigma_luma, :sigma_spatial])
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      src: Evision.Internal.Structurise.from_struct(src),
      confidence: Evision.Internal.Structurise.from_struct(confidence)
    ]
    :evision_nif.ximgproc_fastBilateralSolverFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Simple one-line Fast Bilateral Solver filter call. If you have multiple images to filter with the same
  guide then use FastBilateralSolverFilter interface to avoid extra computations.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    image serving as guide for filtering. It should have 8-bit depth and either 1 or 3 channels.

  - **src**: `Evision.Mat`.

    source image for filtering with unsigned 8-bit or signed 16-bit or floating-point 32-bit depth and up to 4 channels.

  - **confidence**: `Evision.Mat`.

    confidence image with unsigned 8-bit or floating-point 32-bit confidence and 1 channel.

  ##### Keyword Arguments
  - **sigma_spatial**: `double`.

    parameter, that is similar to spatial space sigma (bandwidth) in bilateralFilter.

  - **sigma_luma**: `double`.

    parameter, that is similar to luma space sigma (bandwidth) in bilateralFilter.

  - **sigma_chroma**: `double`.

    parameter, that is similar to chroma space sigma (bandwidth) in bilateralFilter.

  - **lambda**: `double`.

    smoothness strength parameter for solver.

  - **num_iter**: `integer()`.

    number of iterations used for solver, 25 is usually enough.

  - **max_tol**: `double`.

    convergence tolerance used for solver.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image.

  For more details about the Fast Bilateral Solver parameters, see the original paper @cite BarronPoole2016.
  **Note**: Confidence images with CV_8U depth are expected to in [0, 255] and CV_32F in [0, 1] range.

  Python prototype (for reference only):
  ```python3
  fastBilateralSolverFilter(guide, src, confidence[, dst[, sigma_spatial[, sigma_luma[, sigma_chroma[, lambda[, num_iter[, max_tol]]]]]]]) -> dst
  ```
  """
  @spec fastBilateralSolverFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def fastBilateralSolverFilter(guide, src, confidence) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(confidence, Evision.Mat) or is_struct(confidence, Nx.Tensor) or is_number(confidence) or is_tuple(confidence))
  do
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      src: Evision.Internal.Structurise.from_struct(src),
      confidence: Evision.Internal.Structurise.from_struct(confidence)
    ]
    :evision_nif.ximgproc_fastBilateralSolverFilter(positional)
    |> to_struct()
  end

  @doc """
  Simple one-line Fast Global Smoother filter call. If you have multiple images to filter with the same
  guide then use FastGlobalSmootherFilter interface to avoid extra computations.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    image serving as guide for filtering. It should have 8-bit depth and either 1 or 3 channels.

  - **src**: `Evision.Mat`.

    source image for filtering with unsigned 8-bit or signed 16-bit or floating-point 32-bit depth and up to 4 channels.

  - **lambda**: `double`.

    parameter defining the amount of regularization

  - **sigma_color**: `double`.

    parameter, that is similar to color space sigma in bilateralFilter.

  ##### Keyword Arguments
  - **lambda_attenuation**: `double`.

    internal parameter, defining how much lambda decreases after each iteration. Normally,
    it should be 0.25. Setting it to 1.0 may lead to streaking artifacts.

  - **num_iter**: `integer()`.

    number of iterations used for filtering, 3 is usually enough.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image.

  Python prototype (for reference only):
  ```python3
  fastGlobalSmootherFilter(guide, src, lambda, sigma_color[, dst[, lambda_attenuation[, num_iter]]]) -> dst
  ```
  """
  @spec fastGlobalSmootherFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), number(), [{:lambda_attenuation, term()} | {:num_iter, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def fastGlobalSmootherFilter(guide, src, lambda, sigma_color, opts) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(lambda) and is_number(sigma_color) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:lambda_attenuation, :num_iter])
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      src: Evision.Internal.Structurise.from_struct(src),
      lambda: Evision.Internal.Structurise.from_struct(lambda),
      sigma_color: Evision.Internal.Structurise.from_struct(sigma_color)
    ]
    :evision_nif.ximgproc_fastGlobalSmootherFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Simple one-line Fast Global Smoother filter call. If you have multiple images to filter with the same
  guide then use FastGlobalSmootherFilter interface to avoid extra computations.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    image serving as guide for filtering. It should have 8-bit depth and either 1 or 3 channels.

  - **src**: `Evision.Mat`.

    source image for filtering with unsigned 8-bit or signed 16-bit or floating-point 32-bit depth and up to 4 channels.

  - **lambda**: `double`.

    parameter defining the amount of regularization

  - **sigma_color**: `double`.

    parameter, that is similar to color space sigma in bilateralFilter.

  ##### Keyword Arguments
  - **lambda_attenuation**: `double`.

    internal parameter, defining how much lambda decreases after each iteration. Normally,
    it should be 0.25. Setting it to 1.0 may lead to streaking artifacts.

  - **num_iter**: `integer()`.

    number of iterations used for filtering, 3 is usually enough.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image.

  Python prototype (for reference only):
  ```python3
  fastGlobalSmootherFilter(guide, src, lambda, sigma_color[, dst[, lambda_attenuation[, num_iter]]]) -> dst
  ```
  """
  @spec fastGlobalSmootherFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def fastGlobalSmootherFilter(guide, src, lambda, sigma_color) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(lambda) and is_number(sigma_color)
  do
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      src: Evision.Internal.Structurise.from_struct(src),
      lambda: Evision.Internal.Structurise.from_struct(lambda),
      sigma_color: Evision.Internal.Structurise.from_struct(sigma_color)
    ]
    :evision_nif.ximgproc_fastGlobalSmootherFilter(positional)
    |> to_struct()
  end

  @doc """
  Finds ellipses fastly in an image using projective invariant pruning.

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    input image, could be gray or color.

  ##### Keyword Arguments
  - **scoreThreshold**: `float`.

    float, the threshold of ellipse score.

  - **reliabilityThreshold**: `float`.

    float, the threshold of reliability.

  - **centerDistanceThreshold**: `float`.

    float, the threshold of center distance.

  ##### Return
  - **ellipses**: `Evision.Mat.t()`.

    output vector of found ellipses. each vector is encoded as five float $x, y, a, b, radius, score$.

   The function detects ellipses in images using projective invariant pruning.
   For more details about this implementation, please see @cite jia2017fast
   Jia, Qi et al, (2017).
   A Fast Ellipse Detector using Projective Invariant Pruning. IEEE Transactions on Image Processing.

  Python prototype (for reference only):
  ```python3
  findEllipses(image[, ellipses[, scoreThreshold[, reliabilityThreshold[, centerDistanceThreshold]]]]) -> ellipses
  ```
  """
  @spec findEllipses(Evision.Mat.maybe_mat_in(), [{:centerDistanceThreshold, term()} | {:reliabilityThreshold, term()} | {:scoreThreshold, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def findEllipses(image, opts) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:centerDistanceThreshold, :reliabilityThreshold, :scoreThreshold])
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.ximgproc_findEllipses(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Finds ellipses fastly in an image using projective invariant pruning.

  ##### Positional Arguments
  - **image**: `Evision.Mat`.

    input image, could be gray or color.

  ##### Keyword Arguments
  - **scoreThreshold**: `float`.

    float, the threshold of ellipse score.

  - **reliabilityThreshold**: `float`.

    float, the threshold of reliability.

  - **centerDistanceThreshold**: `float`.

    float, the threshold of center distance.

  ##### Return
  - **ellipses**: `Evision.Mat.t()`.

    output vector of found ellipses. each vector is encoded as five float $x, y, a, b, radius, score$.

   The function detects ellipses in images using projective invariant pruning.
   For more details about this implementation, please see @cite jia2017fast
   Jia, Qi et al, (2017).
   A Fast Ellipse Detector using Projective Invariant Pruning. IEEE Transactions on Image Processing.

  Python prototype (for reference only):
  ```python3
  findEllipses(image[, ellipses[, scoreThreshold[, reliabilityThreshold[, centerDistanceThreshold]]]]) -> ellipses
  ```
  """
  @spec findEllipses(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def findEllipses(image) when (is_struct(image, Evision.Mat) or is_struct(image, Nx.Tensor) or is_number(image) or is_tuple(image))
  do
    positional = [
      image: Evision.Internal.Structurise.from_struct(image)
    ]
    :evision_nif.ximgproc_findEllipses(positional)
    |> to_struct()
  end

  @doc """
  Fourier descriptors for planed closed curves

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **nbElt**: `integer()`.
  - **nbFD**: `integer()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   For more details about this implementation, please see @cite PersoonFu1977

  Python prototype (for reference only):
  ```python3
  fourierDescriptor(src[, dst[, nbElt[, nbFD]]]) -> dst
  ```
  """
  @spec fourierDescriptor(Evision.Mat.maybe_mat_in(), [{:nbElt, term()} | {:nbFD, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def fourierDescriptor(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:nbElt, :nbFD])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_fourierDescriptor(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Fourier descriptors for planed closed curves

  ##### Positional Arguments
  - **src**: `Evision.Mat`

  ##### Keyword Arguments
  - **nbElt**: `integer()`.
  - **nbFD**: `integer()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   For more details about this implementation, please see @cite PersoonFu1977

  Python prototype (for reference only):
  ```python3
  fourierDescriptor(src[, dst[, nbElt[, nbFD]]]) -> dst
  ```
  """
  @spec fourierDescriptor(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def fourierDescriptor(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_fourierDescriptor(positional)
    |> to_struct()
  end

  @doc """
  Function for creating a disparity map visualization (clamped CV_8U image)

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    input disparity map (CV_16S depth)

  ##### Keyword Arguments
  - **scale**: `double`.

    disparity map will be multiplied by this value for visualization

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    output visualization

  Python prototype (for reference only):
  ```python3
  getDisparityVis(src[, dst[, scale]]) -> dst
  ```
  """
  @spec getDisparityVis(Evision.Mat.maybe_mat_in(), [{:scale, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def getDisparityVis(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:scale])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_getDisparityVis(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Function for creating a disparity map visualization (clamped CV_8U image)

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    input disparity map (CV_16S depth)

  ##### Keyword Arguments
  - **scale**: `double`.

    disparity map will be multiplied by this value for visualization

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    output visualization

  Python prototype (for reference only):
  ```python3
  getDisparityVis(src[, dst[, scale]]) -> dst
  ```
  """
  @spec getDisparityVis(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def getDisparityVis(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_getDisparityVis(positional)
    |> to_struct()
  end

  @doc """
  Simple one-line (Fast) Guided Filter call.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    guided image (or array of images) with up to 3 channels, if it have more then 3
    channels then only first 3 channels will be used.

  - **src**: `Evision.Mat`.

    filtering image with any numbers of channels.

  - **radius**: `integer()`.

    radius of Guided Filter.

  - **eps**: `double`.

    regularization term of Guided Filter. \\f${eps}^2\\f$ is similar to the sigma in the color
    space into bilateralFilter.

  ##### Keyword Arguments
  - **dDepth**: `integer()`.

    optional depth of the output image.

  - **scale**: `double`.

    subsample factor of Fast Guided Filter, use a scale less than 1 to speeds up computation
    with almost no visible degradation. (e.g. scale==0.5 shrinks the image by 2x inside the filter)

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    output image.

  If you have multiple images to filter with the same guided image then use GuidedFilter interface to
  avoid extra computations on initialization stage.

  @sa bilateralFilter, dtFilter, amFilter

  Python prototype (for reference only):
  ```python3
  guidedFilter(guide, src, radius, eps[, dst[, dDepth[, scale]]]) -> dst
  ```
  """
  @spec guidedFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), number(), [{:dDepth, term()} | {:scale, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def guidedFilter(guide, src, radius, eps, opts) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(radius) and is_number(eps) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:dDepth, :scale])
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      src: Evision.Internal.Structurise.from_struct(src),
      radius: Evision.Internal.Structurise.from_struct(radius),
      eps: Evision.Internal.Structurise.from_struct(eps)
    ]
    :evision_nif.ximgproc_guidedFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Simple one-line (Fast) Guided Filter call.

  ##### Positional Arguments
  - **guide**: `Evision.Mat`.

    guided image (or array of images) with up to 3 channels, if it have more then 3
    channels then only first 3 channels will be used.

  - **src**: `Evision.Mat`.

    filtering image with any numbers of channels.

  - **radius**: `integer()`.

    radius of Guided Filter.

  - **eps**: `double`.

    regularization term of Guided Filter. \\f${eps}^2\\f$ is similar to the sigma in the color
    space into bilateralFilter.

  ##### Keyword Arguments
  - **dDepth**: `integer()`.

    optional depth of the output image.

  - **scale**: `double`.

    subsample factor of Fast Guided Filter, use a scale less than 1 to speeds up computation
    with almost no visible degradation. (e.g. scale==0.5 shrinks the image by 2x inside the filter)

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    output image.

  If you have multiple images to filter with the same guided image then use GuidedFilter interface to
  avoid extra computations on initialization stage.

  @sa bilateralFilter, dtFilter, amFilter

  Python prototype (for reference only):
  ```python3
  guidedFilter(guide, src, radius, eps[, dst[, dDepth[, scale]]]) -> dst
  ```
  """
  @spec guidedFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def guidedFilter(guide, src, radius, eps) when (is_struct(guide, Evision.Mat) or is_struct(guide, Nx.Tensor) or is_number(guide) or is_tuple(guide)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(radius) and is_number(eps)
  do
    positional = [
      guide: Evision.Internal.Structurise.from_struct(guide),
      src: Evision.Internal.Structurise.from_struct(src),
      radius: Evision.Internal.Structurise.from_struct(radius),
      eps: Evision.Internal.Structurise.from_struct(eps)
    ]
    :evision_nif.ximgproc_guidedFilter(positional)
    |> to_struct()
  end

  @doc """
  Applies the joint bilateral filter to an image.

  ##### Positional Arguments
  - **joint**: `Evision.Mat`.

    Joint 8-bit or floating-point, 1-channel or 3-channel image.

  - **src**: `Evision.Mat`.

    Source 8-bit or floating-point, 1-channel or 3-channel image with the same depth as joint
    image.

  - **d**: `integer()`.

    Diameter of each pixel neighborhood that is used during filtering. If it is non-positive,
    it is computed from sigmaSpace .

  - **sigmaColor**: `double`.

    Filter sigma in the color space. A larger value of the parameter means that
    farther colors within the pixel neighborhood (see sigmaSpace ) will be mixed together, resulting in
    larger areas of semi-equal color.

  - **sigmaSpace**: `double`.

    Filter sigma in the coordinate space. A larger value of the parameter means that
    farther pixels will influence each other as long as their colors are close enough (see sigmaColor ).
    When d\\>0 , it specifies the neighborhood size regardless of sigmaSpace . Otherwise, d is
    proportional to sigmaSpace .

  ##### Keyword Arguments
  - **borderType**: `integer()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and type as src .

  **Note**: bilateralFilter and jointBilateralFilter use L1 norm to compute difference between colors.
  @sa bilateralFilter, amFilter

  Python prototype (for reference only):
  ```python3
  jointBilateralFilter(joint, src, d, sigmaColor, sigmaSpace[, dst[, borderType]]) -> dst
  ```
  """
  @spec jointBilateralFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), number(), number(), [{:borderType, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def jointBilateralFilter(joint, src, d, sigmaColor, sigmaSpace, opts) when (is_struct(joint, Evision.Mat) or is_struct(joint, Nx.Tensor) or is_number(joint) or is_tuple(joint)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(d) and is_number(sigmaColor) and is_number(sigmaSpace) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderType])
    positional = [
      joint: Evision.Internal.Structurise.from_struct(joint),
      src: Evision.Internal.Structurise.from_struct(src),
      d: Evision.Internal.Structurise.from_struct(d),
      sigmaColor: Evision.Internal.Structurise.from_struct(sigmaColor),
      sigmaSpace: Evision.Internal.Structurise.from_struct(sigmaSpace)
    ]
    :evision_nif.ximgproc_jointBilateralFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Applies the joint bilateral filter to an image.

  ##### Positional Arguments
  - **joint**: `Evision.Mat`.

    Joint 8-bit or floating-point, 1-channel or 3-channel image.

  - **src**: `Evision.Mat`.

    Source 8-bit or floating-point, 1-channel or 3-channel image with the same depth as joint
    image.

  - **d**: `integer()`.

    Diameter of each pixel neighborhood that is used during filtering. If it is non-positive,
    it is computed from sigmaSpace .

  - **sigmaColor**: `double`.

    Filter sigma in the color space. A larger value of the parameter means that
    farther colors within the pixel neighborhood (see sigmaSpace ) will be mixed together, resulting in
    larger areas of semi-equal color.

  - **sigmaSpace**: `double`.

    Filter sigma in the coordinate space. A larger value of the parameter means that
    farther pixels will influence each other as long as their colors are close enough (see sigmaColor ).
    When d\\>0 , it specifies the neighborhood size regardless of sigmaSpace . Otherwise, d is
    proportional to sigmaSpace .

  ##### Keyword Arguments
  - **borderType**: `integer()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and type as src .

  **Note**: bilateralFilter and jointBilateralFilter use L1 norm to compute difference between colors.
  @sa bilateralFilter, amFilter

  Python prototype (for reference only):
  ```python3
  jointBilateralFilter(joint, src, d, sigmaColor, sigmaSpace[, dst[, borderType]]) -> dst
  ```
  """
  @spec jointBilateralFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), number(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def jointBilateralFilter(joint, src, d, sigmaColor, sigmaSpace) when (is_struct(joint, Evision.Mat) or is_struct(joint, Nx.Tensor) or is_number(joint) or is_tuple(joint)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(d) and is_number(sigmaColor) and is_number(sigmaSpace)
  do
    positional = [
      joint: Evision.Internal.Structurise.from_struct(joint),
      src: Evision.Internal.Structurise.from_struct(src),
      d: Evision.Internal.Structurise.from_struct(d),
      sigmaColor: Evision.Internal.Structurise.from_struct(sigmaColor),
      sigmaSpace: Evision.Internal.Structurise.from_struct(sigmaSpace)
    ]
    :evision_nif.ximgproc_jointBilateralFilter(positional)
    |> to_struct()
  end

  @doc """
  Global image smoothing via L0 gradient minimization.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    source image for filtering with unsigned 8-bit or signed 16-bit or floating-point depth.

  ##### Keyword Arguments
  - **lambda**: `double`.

    parameter defining the smooth term weight.

  - **kappa**: `double`.

    parameter defining the increasing factor of the weight of the gradient data term.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image.

  For more details about L0 Smoother, see the original paper @cite xu2011image.

  Python prototype (for reference only):
  ```python3
  l0Smooth(src[, dst[, lambda[, kappa]]]) -> dst
  ```
  """
  @spec l0Smooth(Evision.Mat.maybe_mat_in(), [{:kappa, term()} | {:lambda, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def l0Smooth(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:kappa, :lambda])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_l0Smooth(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Global image smoothing via L0 gradient minimization.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    source image for filtering with unsigned 8-bit or signed 16-bit or floating-point depth.

  ##### Keyword Arguments
  - **lambda**: `double`.

    parameter defining the smooth term weight.

  - **kappa**: `double`.

    parameter defining the increasing factor of the weight of the gradient data term.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    destination image.

  For more details about L0 Smoother, see the original paper @cite xu2011image.

  Python prototype (for reference only):
  ```python3
  l0Smooth(src[, dst[, lambda[, kappa]]]) -> dst
  ```
  """
  @spec l0Smooth(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def l0Smooth(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_l0Smooth(positional)
    |> to_struct()
  end

  @doc """
  Performs thresholding on input images using Niblack's technique or some of the
  popular variations it inspired.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source 8-bit single-channel image.

  - **maxValue**: `double`.

    Non-zero value assigned to the pixels for which the condition is satisfied,
    used with the THRESH_BINARY and THRESH_BINARY_INV thresholding types.

  - **type**: `integer()`.

    Thresholding type, see cv::ThresholdTypes.

  - **blockSize**: `integer()`.

    Size of a pixel neighborhood that is used to calculate a threshold value
    for the pixel: 3, 5, 7, and so on.

  - **k**: `double`.

    The user-adjustable parameter used by Niblack and inspired techniques. For Niblack, this is
    normally a value between 0 and 1 that is multiplied with the standard deviation and subtracted from
    the mean.

  ##### Keyword Arguments
  - **binarizationMethod**: `integer()`.

    Binarization method to use. By default, Niblack's technique is used.
    Other techniques can be specified, see cv::ximgproc::LocalBinarizationMethods.

  - **r**: `double`.

    The user-adjustable parameter used by Sauvola's technique. This is the dynamic range
    of standard deviation.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and the same type as src.

  The function transforms a grayscale image to a binary image according to the formulae:
  - **THRESH_BINARY**
    \\f[dst(x,y) =  \\fork{\\texttt{maxValue}}{if \\(src(x,y) > T(x,y)\\)}{0}{otherwise}\\f]

  - **THRESH_BINARY_INV**
    \\f[dst(x,y) =  \\fork{0}{if \\(src(x,y) > T(x,y)\\)}{\\texttt{maxValue}}{otherwise}\\f]
    where \\f$T(x,y)\\f$ is a threshold calculated individually for each pixel.

  The threshold value \\f$T(x, y)\\f$ is determined based on the binarization method chosen. For
  classic Niblack, it is the mean minus \\f$ k \\f$ times standard deviation of
  \\f$\\texttt{blockSize} \\times\\texttt{blockSize}\\f$ neighborhood of \\f$(x, y)\\f$.
  The function can't process the image in-place.
  @sa  threshold, adaptiveThreshold

  Python prototype (for reference only):
  ```python3
  niBlackThreshold(_src, maxValue, type, blockSize, k[, _dst[, binarizationMethod[, r]]]) -> _dst
  ```
  """
  @spec niBlackThreshold(Evision.Mat.maybe_mat_in(), number(), integer(), integer(), number(), [{:binarizationMethod, term()} | {:r, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def niBlackThreshold(src, maxValue, type, blockSize, k, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(maxValue) and is_integer(type) and is_integer(blockSize) and is_number(k) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:binarizationMethod, :r])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      maxValue: Evision.Internal.Structurise.from_struct(maxValue),
      type: Evision.Internal.Structurise.from_struct(type),
      blockSize: Evision.Internal.Structurise.from_struct(blockSize),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.ximgproc_niBlackThreshold(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Performs thresholding on input images using Niblack's technique or some of the
  popular variations it inspired.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source 8-bit single-channel image.

  - **maxValue**: `double`.

    Non-zero value assigned to the pixels for which the condition is satisfied,
    used with the THRESH_BINARY and THRESH_BINARY_INV thresholding types.

  - **type**: `integer()`.

    Thresholding type, see cv::ThresholdTypes.

  - **blockSize**: `integer()`.

    Size of a pixel neighborhood that is used to calculate a threshold value
    for the pixel: 3, 5, 7, and so on.

  - **k**: `double`.

    The user-adjustable parameter used by Niblack and inspired techniques. For Niblack, this is
    normally a value between 0 and 1 that is multiplied with the standard deviation and subtracted from
    the mean.

  ##### Keyword Arguments
  - **binarizationMethod**: `integer()`.

    Binarization method to use. By default, Niblack's technique is used.
    Other techniques can be specified, see cv::ximgproc::LocalBinarizationMethods.

  - **r**: `double`.

    The user-adjustable parameter used by Sauvola's technique. This is the dynamic range
    of standard deviation.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and the same type as src.

  The function transforms a grayscale image to a binary image according to the formulae:
  - **THRESH_BINARY**
    \\f[dst(x,y) =  \\fork{\\texttt{maxValue}}{if \\(src(x,y) > T(x,y)\\)}{0}{otherwise}\\f]

  - **THRESH_BINARY_INV**
    \\f[dst(x,y) =  \\fork{0}{if \\(src(x,y) > T(x,y)\\)}{\\texttt{maxValue}}{otherwise}\\f]
    where \\f$T(x,y)\\f$ is a threshold calculated individually for each pixel.

  The threshold value \\f$T(x, y)\\f$ is determined based on the binarization method chosen. For
  classic Niblack, it is the mean minus \\f$ k \\f$ times standard deviation of
  \\f$\\texttt{blockSize} \\times\\texttt{blockSize}\\f$ neighborhood of \\f$(x, y)\\f$.
  The function can't process the image in-place.
  @sa  threshold, adaptiveThreshold

  Python prototype (for reference only):
  ```python3
  niBlackThreshold(_src, maxValue, type, blockSize, k[, _dst[, binarizationMethod[, r]]]) -> _dst
  ```
  """
  @spec niBlackThreshold(Evision.Mat.maybe_mat_in(), number(), integer(), integer(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def niBlackThreshold(src, maxValue, type, blockSize, k) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_number(maxValue) and is_integer(type) and is_integer(blockSize) and is_number(k)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      maxValue: Evision.Internal.Structurise.from_struct(maxValue),
      type: Evision.Internal.Structurise.from_struct(type),
      blockSize: Evision.Internal.Structurise.from_struct(blockSize),
      k: Evision.Internal.Structurise.from_struct(k)
    ]
    :evision_nif.ximgproc_niBlackThreshold(positional)
    |> to_struct()
  end

  @doc """
  calculates conjugate of a quaternion image.

  ##### Positional Arguments
  - **qimg**: `Evision.Mat`

  ##### Return
  - **qcimg**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  qconj(qimg[, qcimg]) -> qcimg
  ```
  """
  @spec qconj(Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def qconj(qimg, opts) when (is_struct(qimg, Evision.Mat) or is_struct(qimg, Nx.Tensor) or is_number(qimg) or is_tuple(qimg)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      qimg: Evision.Internal.Structurise.from_struct(qimg)
    ]
    :evision_nif.ximgproc_qconj(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  calculates conjugate of a quaternion image.

  ##### Positional Arguments
  - **qimg**: `Evision.Mat`

  ##### Return
  - **qcimg**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  qconj(qimg[, qcimg]) -> qcimg
  ```
  """
  @spec qconj(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def qconj(qimg) when (is_struct(qimg, Evision.Mat) or is_struct(qimg, Nx.Tensor) or is_number(qimg) or is_tuple(qimg))
  do
    positional = [
      qimg: Evision.Internal.Structurise.from_struct(qimg)
    ]
    :evision_nif.ximgproc_qconj(positional)
    |> to_struct()
  end

  @doc """
  Performs a forward or inverse Discrete quaternion Fourier transform of a 2D quaternion array.

  ##### Positional Arguments
  - **img**: `Evision.Mat`
  - **flags**: `integer()`
  - **sideLeft**: `bool`

  ##### Return
  - **qimg**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  qdft(img, flags, sideLeft[, qimg]) -> qimg
  ```
  """
  @spec qdft(Evision.Mat.maybe_mat_in(), integer(), boolean(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def qdft(img, flags, sideLeft, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and is_integer(flags) and is_boolean(sideLeft) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      flags: Evision.Internal.Structurise.from_struct(flags),
      sideLeft: Evision.Internal.Structurise.from_struct(sideLeft)
    ]
    :evision_nif.ximgproc_qdft(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Performs a forward or inverse Discrete quaternion Fourier transform of a 2D quaternion array.

  ##### Positional Arguments
  - **img**: `Evision.Mat`
  - **flags**: `integer()`
  - **sideLeft**: `bool`

  ##### Return
  - **qimg**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  qdft(img, flags, sideLeft[, qimg]) -> qimg
  ```
  """
  @spec qdft(Evision.Mat.maybe_mat_in(), integer(), boolean()) :: Evision.Mat.t() | {:error, String.t()}
  def qdft(img, flags, sideLeft) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and is_integer(flags) and is_boolean(sideLeft)
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img),
      flags: Evision.Internal.Structurise.from_struct(flags),
      sideLeft: Evision.Internal.Structurise.from_struct(sideLeft)
    ]
    :evision_nif.ximgproc_qdft(positional)
    |> to_struct()
  end

  @doc """
  Calculates the per-element quaternion product of two arrays

  ##### Positional Arguments
  - **src1**: `Evision.Mat`
  - **src2**: `Evision.Mat`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  qmultiply(src1, src2[, dst]) -> dst
  ```
  """
  @spec qmultiply(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def qmultiply(src1, src2, opts) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.ximgproc_qmultiply(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Calculates the per-element quaternion product of two arrays

  ##### Positional Arguments
  - **src1**: `Evision.Mat`
  - **src2**: `Evision.Mat`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  qmultiply(src1, src2[, dst]) -> dst
  ```
  """
  @spec qmultiply(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def qmultiply(src1, src2) when (is_struct(src1, Evision.Mat) or is_struct(src1, Nx.Tensor) or is_number(src1) or is_tuple(src1)) and (is_struct(src2, Evision.Mat) or is_struct(src2, Nx.Tensor) or is_number(src2) or is_tuple(src2))
  do
    positional = [
      src1: Evision.Internal.Structurise.from_struct(src1),
      src2: Evision.Internal.Structurise.from_struct(src2)
    ]
    :evision_nif.ximgproc_qmultiply(positional)
    |> to_struct()
  end

  @doc """
  divides each element by its modulus.

  ##### Positional Arguments
  - **qimg**: `Evision.Mat`

  ##### Return
  - **qnimg**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  qunitary(qimg[, qnimg]) -> qnimg
  ```
  """
  @spec qunitary(Evision.Mat.maybe_mat_in(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def qunitary(qimg, opts) when (is_struct(qimg, Evision.Mat) or is_struct(qimg, Nx.Tensor) or is_number(qimg) or is_tuple(qimg)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      qimg: Evision.Internal.Structurise.from_struct(qimg)
    ]
    :evision_nif.ximgproc_qunitary(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  divides each element by its modulus.

  ##### Positional Arguments
  - **qimg**: `Evision.Mat`

  ##### Return
  - **qnimg**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  qunitary(qimg[, qnimg]) -> qnimg
  ```
  """
  @spec qunitary(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def qunitary(qimg) when (is_struct(qimg, Evision.Mat) or is_struct(qimg, Nx.Tensor) or is_number(qimg) or is_tuple(qimg))
  do
    positional = [
      qimg: Evision.Internal.Structurise.from_struct(qimg)
    ]
    :evision_nif.ximgproc_qunitary(positional)
    |> to_struct()
  end

  @doc """
  Function for reading ground truth disparity maps. Supports basic Middlebury
  and MPI-Sintel formats. Note that the resulting disparity map is scaled by 16.

  ##### Positional Arguments
  - **src_path**: `String`.

    path to the image, containing ground-truth disparity map

  ##### Return
  - **retval**: `integer()`
  - **dst**: `Evision.Mat.t()`.

    output disparity map, CV_16S depth

  @result returns zero if successfully read the ground truth

  Python prototype (for reference only):
  ```python3
  readGT(src_path[, dst]) -> retval, dst
  ```
  """
  @spec readGT(binary(), [{atom(), term()},...] | nil) :: {integer(), Evision.Mat.t()} | {:error, String.t()}
  def readGT(src_path, opts) when is_binary(src_path) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src_path: Evision.Internal.Structurise.from_struct(src_path)
    ]
    :evision_nif.ximgproc_readGT(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Function for reading ground truth disparity maps. Supports basic Middlebury
  and MPI-Sintel formats. Note that the resulting disparity map is scaled by 16.

  ##### Positional Arguments
  - **src_path**: `String`.

    path to the image, containing ground-truth disparity map

  ##### Return
  - **retval**: `integer()`
  - **dst**: `Evision.Mat.t()`.

    output disparity map, CV_16S depth

  @result returns zero if successfully read the ground truth

  Python prototype (for reference only):
  ```python3
  readGT(src_path[, dst]) -> retval, dst
  ```
  """
  @spec readGT(binary()) :: {integer(), Evision.Mat.t()} | {:error, String.t()}
  def readGT(src_path) when is_binary(src_path)
  do
    positional = [
      src_path: Evision.Internal.Structurise.from_struct(src_path)
    ]
    :evision_nif.ximgproc_readGT(positional)
    |> to_struct()
  end

  @doc """
  Applies the rolling guidance filter to an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source 8-bit or floating-point, 1-channel or 3-channel image.

  ##### Keyword Arguments
  - **d**: `integer()`.

    Diameter of each pixel neighborhood that is used during filtering. If it is non-positive,
    it is computed from sigmaSpace .

  - **sigmaColor**: `double`.

    Filter sigma in the color space. A larger value of the parameter means that
    farther colors within the pixel neighborhood (see sigmaSpace ) will be mixed together, resulting in
    larger areas of semi-equal color.

  - **sigmaSpace**: `double`.

    Filter sigma in the coordinate space. A larger value of the parameter means that
    farther pixels will influence each other as long as their colors are close enough (see sigmaColor ).
    When d\\>0 , it specifies the neighborhood size regardless of sigmaSpace . Otherwise, d is
    proportional to sigmaSpace .

  - **numOfIter**: `integer()`.

    Number of iterations of joint edge-preserving filtering applied on the source image.

  - **borderType**: `integer()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and type as src.

  For more details, please see @cite zhang2014rolling

  **Note**: rollingGuidanceFilter uses jointBilateralFilter as the edge-preserving filter.
  @sa jointBilateralFilter, bilateralFilter, amFilter

  Python prototype (for reference only):
  ```python3
  rollingGuidanceFilter(src[, dst[, d[, sigmaColor[, sigmaSpace[, numOfIter[, borderType]]]]]]) -> dst
  ```
  """
  @spec rollingGuidanceFilter(Evision.Mat.maybe_mat_in(), [{:borderType, term()} | {:d, term()} | {:numOfIter, term()} | {:sigmaColor, term()} | {:sigmaSpace, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def rollingGuidanceFilter(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:borderType, :d, :numOfIter, :sigmaColor, :sigmaSpace])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_rollingGuidanceFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Applies the rolling guidance filter to an image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source 8-bit or floating-point, 1-channel or 3-channel image.

  ##### Keyword Arguments
  - **d**: `integer()`.

    Diameter of each pixel neighborhood that is used during filtering. If it is non-positive,
    it is computed from sigmaSpace .

  - **sigmaColor**: `double`.

    Filter sigma in the color space. A larger value of the parameter means that
    farther colors within the pixel neighborhood (see sigmaSpace ) will be mixed together, resulting in
    larger areas of semi-equal color.

  - **sigmaSpace**: `double`.

    Filter sigma in the coordinate space. A larger value of the parameter means that
    farther pixels will influence each other as long as their colors are close enough (see sigmaColor ).
    When d\\>0 , it specifies the neighborhood size regardless of sigmaSpace . Otherwise, d is
    proportional to sigmaSpace .

  - **numOfIter**: `integer()`.

    Number of iterations of joint edge-preserving filtering applied on the source image.

  - **borderType**: `integer()`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and type as src.

  For more details, please see @cite zhang2014rolling

  **Note**: rollingGuidanceFilter uses jointBilateralFilter as the edge-preserving filter.
  @sa jointBilateralFilter, bilateralFilter, amFilter

  Python prototype (for reference only):
  ```python3
  rollingGuidanceFilter(src[, dst[, d[, sigmaColor[, sigmaSpace[, numOfIter[, borderType]]]]]]) -> dst
  ```
  """
  @spec rollingGuidanceFilter(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def rollingGuidanceFilter(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_rollingGuidanceFilter(positional)
    |> to_struct()
  end

  @doc """
  Applies a binary blob thinning operation, to achieve a skeletization of the input image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source 8-bit single-channel image, containing binary blobs, with blobs having 255 pixel values.

  ##### Keyword Arguments
  - **thinningType**: `integer()`.

    Value that defines which thinning algorithm should be used. See cv::ximgproc::ThinningTypes

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and the same type as src. The function can work in-place.

  The function transforms a binary blob image into a skeletized form using the technique of Zhang-Suen.

  Python prototype (for reference only):
  ```python3
  thinning(src[, dst[, thinningType]]) -> dst
  ```
  """
  @spec thinning(Evision.Mat.maybe_mat_in(), [{:thinningType, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def thinning(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:thinningType])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_thinning(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Applies a binary blob thinning operation, to achieve a skeletization of the input image.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Source 8-bit single-channel image, containing binary blobs, with blobs having 255 pixel values.

  ##### Keyword Arguments
  - **thinningType**: `integer()`.

    Value that defines which thinning algorithm should be used. See cv::ximgproc::ThinningTypes

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Destination image of the same size and the same type as src. The function can work in-place.

  The function transforms a binary blob image into a skeletized form using the technique of Zhang-Suen.

  Python prototype (for reference only):
  ```python3
  thinning(src[, dst[, thinningType]]) -> dst
  ```
  """
  @spec thinning(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def thinning(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.ximgproc_thinning(positional)
    |> to_struct()
  end

  @doc """
  transform a contour

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **t**: `Evision.Mat`

  ##### Keyword Arguments
  - **fdContour**: `bool`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  transformFD(src, t[, dst[, fdContour]]) -> dst
  ```
  """
  @spec transformFD(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:fdContour, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def transformFD(src, t, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(t, Evision.Mat) or is_struct(t, Nx.Tensor) or is_number(t) or is_tuple(t)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:fdContour])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      t: Evision.Internal.Structurise.from_struct(t)
    ]
    :evision_nif.ximgproc_transformFD(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  transform a contour

  ##### Positional Arguments
  - **src**: `Evision.Mat`
  - **t**: `Evision.Mat`

  ##### Keyword Arguments
  - **fdContour**: `bool`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

  Python prototype (for reference only):
  ```python3
  transformFD(src, t[, dst[, fdContour]]) -> dst
  ```
  """
  @spec transformFD(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def transformFD(src, t) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(t, Evision.Mat) or is_struct(t, Nx.Tensor) or is_number(t) or is_tuple(t))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      t: Evision.Internal.Structurise.from_struct(t)
    ]
    :evision_nif.ximgproc_transformFD(positional)
    |> to_struct()
  end

  @doc """
  Applies weighted median filter to an image.

  ##### Positional Arguments
  - **joint**: `Evision.Mat`
  - **src**: `Evision.Mat`
  - **r**: `integer()`

  ##### Keyword Arguments
  - **sigma**: `double`.
  - **weightType**: `integer()`.
  - **mask**: `Evision.Mat`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   For more details about this implementation, please see @cite zhang2014100+

  @sa medianBlur, jointBilateralFilter

  Python prototype (for reference only):
  ```python3
  weightedMedianFilter(joint, src, r[, dst[, sigma[, weightType[, mask]]]]) -> dst
  ```
  """
  @spec weightedMedianFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer(), [{:mask, term()} | {:sigma, term()} | {:weightType, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def weightedMedianFilter(joint, src, r, opts) when (is_struct(joint, Evision.Mat) or is_struct(joint, Nx.Tensor) or is_number(joint) or is_tuple(joint)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(r) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:mask, :sigma, :weightType])
    positional = [
      joint: Evision.Internal.Structurise.from_struct(joint),
      src: Evision.Internal.Structurise.from_struct(src),
      r: Evision.Internal.Structurise.from_struct(r)
    ]
    :evision_nif.ximgproc_weightedMedianFilter(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Applies weighted median filter to an image.

  ##### Positional Arguments
  - **joint**: `Evision.Mat`
  - **src**: `Evision.Mat`
  - **r**: `integer()`

  ##### Keyword Arguments
  - **sigma**: `double`.
  - **weightType**: `integer()`.
  - **mask**: `Evision.Mat`.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

   For more details about this implementation, please see @cite zhang2014100+

  @sa medianBlur, jointBilateralFilter

  Python prototype (for reference only):
  ```python3
  weightedMedianFilter(joint, src, r[, dst[, sigma[, weightType[, mask]]]]) -> dst
  ```
  """
  @spec weightedMedianFilter(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def weightedMedianFilter(joint, src, r) when (is_struct(joint, Evision.Mat) or is_struct(joint, Nx.Tensor) or is_number(joint) or is_tuple(joint)) and (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(r)
  do
    positional = [
      joint: Evision.Internal.Structurise.from_struct(joint),
      src: Evision.Internal.Structurise.from_struct(src),
      r: Evision.Internal.Structurise.from_struct(r)
    ]
    :evision_nif.ximgproc_weightedMedianFilter(positional)
    |> to_struct()
  end

  @doc """
  Creates a graph based segmentor
  ##### Keyword Arguments
  - **sigma**: `double`.

    The sigma parameter, used to smooth image

  - **k**: `float`.

    The k parameter of the algorythm

  - **min_size**: `integer()`.

    The minimum size of segments

  ##### Return
  - **retval**: `Evision.XImgProc.GraphSegmentation.t()`

  Python prototype (for reference only):
  ```python3
  createGraphSegmentation([, sigma[, k[, min_size]]]) -> retval
  ```
  """
  @spec createGraphSegmentation([{:k, term()} | {:min_size, term()} | {:sigma, term()}] | nil) :: Evision.XImgProc.GraphSegmentation.t() | {:error, String.t()}
  def createGraphSegmentation(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:k, :min_size, :sigma])
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_createGraphSegmentation(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates a graph based segmentor
  ##### Keyword Arguments
  - **sigma**: `double`.

    The sigma parameter, used to smooth image

  - **k**: `float`.

    The k parameter of the algorythm

  - **min_size**: `integer()`.

    The minimum size of segments

  ##### Return
  - **retval**: `Evision.XImgProc.GraphSegmentation.t()`

  Python prototype (for reference only):
  ```python3
  createGraphSegmentation([, sigma[, k[, min_size]]]) -> retval
  ```
  """
  @spec createGraphSegmentation() :: Evision.XImgProc.GraphSegmentation.t() | {:error, String.t()}
  def createGraphSegmentation() do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_createGraphSegmentation(positional)
    |> to_struct()
  end

  @doc """
  Create a new SelectiveSearchSegmentation class.
  ##### Return
  - **retval**: `Evision.XImgProc.SelectiveSearchSegmentation.t()`

  Python prototype (for reference only):
  ```python3
  createSelectiveSearchSegmentation() -> retval
  ```
  """
  @spec createSelectiveSearchSegmentation() :: Evision.XImgProc.SelectiveSearchSegmentation.t() | {:error, String.t()}
  def createSelectiveSearchSegmentation() do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_createSelectiveSearchSegmentation(positional)
    |> to_struct()
  end

  @doc """
  Create a new color-based strategy
  ##### Return
  - **retval**: `Evision.XImgProc.SelectiveSearchSegmentationStrategyColor.t()`

  Python prototype (for reference only):
  ```python3
  createSelectiveSearchSegmentationStrategyColor() -> retval
  ```
  """
  @spec createSelectiveSearchSegmentationStrategyColor() :: Evision.XImgProc.SelectiveSearchSegmentationStrategyColor.t() | {:error, String.t()}
  def createSelectiveSearchSegmentationStrategyColor() do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_createSelectiveSearchSegmentationStrategyColor(positional)
    |> to_struct()
  end

  @doc """
  Create a new fill-based strategy
  ##### Return
  - **retval**: `Evision.XImgProc.SelectiveSearchSegmentationStrategyFill.t()`

  Python prototype (for reference only):
  ```python3
  createSelectiveSearchSegmentationStrategyFill() -> retval
  ```
  """
  @spec createSelectiveSearchSegmentationStrategyFill() :: Evision.XImgProc.SelectiveSearchSegmentationStrategyFill.t() | {:error, String.t()}
  def createSelectiveSearchSegmentationStrategyFill() do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_createSelectiveSearchSegmentationStrategyFill(positional)
    |> to_struct()
  end

  @doc """
  Create a new multiple strategy and set four subtrategies, with equal weights

  ##### Positional Arguments
  - **s1**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`.

    The first strategy

  - **s2**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`.

    The second strategy

  - **s3**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`.

    The third strategy

  - **s4**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`.

    The forth strategy

  ##### Return
  - **retval**: `Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t()`

  Python prototype (for reference only):
  ```python3
  createSelectiveSearchSegmentationStrategyMultiple(s1, s2, s3, s4) -> retval
  ```
  """
  @spec createSelectiveSearchSegmentationStrategyMultiple(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()) :: Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t() | {:error, String.t()}
  def createSelectiveSearchSegmentationStrategyMultiple(s1, s2, s3, s4) when is_struct(s1, Evision.XImgProc.SelectiveSearchSegmentationStrategy) and is_struct(s2, Evision.XImgProc.SelectiveSearchSegmentationStrategy) and is_struct(s3, Evision.XImgProc.SelectiveSearchSegmentationStrategy) and is_struct(s4, Evision.XImgProc.SelectiveSearchSegmentationStrategy)
  do
    positional = [
      s1: Evision.Internal.Structurise.from_struct(s1),
      s2: Evision.Internal.Structurise.from_struct(s2),
      s3: Evision.Internal.Structurise.from_struct(s3),
      s4: Evision.Internal.Structurise.from_struct(s4)
    ]
    :evision_nif.ximgproc_segmentation_createSelectiveSearchSegmentationStrategyMultiple(positional)
    |> to_struct()
  end

  @doc """
  Create a new multiple strategy and set three subtrategies, with equal weights

  ##### Positional Arguments
  - **s1**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`.

    The first strategy

  - **s2**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`.

    The second strategy

  - **s3**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`.

    The third strategy

  ##### Return
  - **retval**: `Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t()`

  Python prototype (for reference only):
  ```python3
  createSelectiveSearchSegmentationStrategyMultiple(s1, s2, s3) -> retval
  ```
  """
  @spec createSelectiveSearchSegmentationStrategyMultiple(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()) :: Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t() | {:error, String.t()}
  def createSelectiveSearchSegmentationStrategyMultiple(s1, s2, s3) when is_struct(s1, Evision.XImgProc.SelectiveSearchSegmentationStrategy) and is_struct(s2, Evision.XImgProc.SelectiveSearchSegmentationStrategy) and is_struct(s3, Evision.XImgProc.SelectiveSearchSegmentationStrategy)
  do
    positional = [
      s1: Evision.Internal.Structurise.from_struct(s1),
      s2: Evision.Internal.Structurise.from_struct(s2),
      s3: Evision.Internal.Structurise.from_struct(s3)
    ]
    :evision_nif.ximgproc_segmentation_createSelectiveSearchSegmentationStrategyMultiple(positional)
    |> to_struct()
  end

  @doc """
  Create a new multiple strategy and set two subtrategies, with equal weights

  ##### Positional Arguments
  - **s1**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`.

    The first strategy

  - **s2**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`.

    The second strategy

  ##### Return
  - **retval**: `Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t()`

  Python prototype (for reference only):
  ```python3
  createSelectiveSearchSegmentationStrategyMultiple(s1, s2) -> retval
  ```
  """
  @spec createSelectiveSearchSegmentationStrategyMultiple(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t(), Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()) :: Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t() | {:error, String.t()}
  def createSelectiveSearchSegmentationStrategyMultiple(s1, s2) when is_struct(s1, Evision.XImgProc.SelectiveSearchSegmentationStrategy) and is_struct(s2, Evision.XImgProc.SelectiveSearchSegmentationStrategy)
  do
    positional = [
      s1: Evision.Internal.Structurise.from_struct(s1),
      s2: Evision.Internal.Structurise.from_struct(s2)
    ]
    :evision_nif.ximgproc_segmentation_createSelectiveSearchSegmentationStrategyMultiple(positional)
    |> to_struct()
  end

  @doc """
  Create a new multiple strategy and set one subtrategy

  ##### Positional Arguments
  - **s1**: `Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()`.

    The first strategy

  ##### Return
  - **retval**: `Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t()`

  Python prototype (for reference only):
  ```python3
  createSelectiveSearchSegmentationStrategyMultiple(s1) -> retval
  ```
  """
  @spec createSelectiveSearchSegmentationStrategyMultiple(Evision.XImgProc.SelectiveSearchSegmentationStrategy.t()) :: Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t() | {:error, String.t()}
  def createSelectiveSearchSegmentationStrategyMultiple(s1) when is_struct(s1, Evision.XImgProc.SelectiveSearchSegmentationStrategy)
  do
    positional = [
      s1: Evision.Internal.Structurise.from_struct(s1)
    ]
    :evision_nif.ximgproc_segmentation_createSelectiveSearchSegmentationStrategyMultiple(positional)
    |> to_struct()
  end

  @doc """
  Create a new multiple strategy
  ##### Return
  - **retval**: `Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t()`

  Python prototype (for reference only):
  ```python3
  createSelectiveSearchSegmentationStrategyMultiple() -> retval
  ```
  """
  @spec createSelectiveSearchSegmentationStrategyMultiple() :: Evision.XImgProc.SelectiveSearchSegmentationStrategyMultiple.t() | {:error, String.t()}
  def createSelectiveSearchSegmentationStrategyMultiple() do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_createSelectiveSearchSegmentationStrategyMultiple(positional)
    |> to_struct()
  end

  @doc """
  Create a new size-based strategy
  ##### Return
  - **retval**: `Evision.XImgProc.SelectiveSearchSegmentationStrategySize.t()`

  Python prototype (for reference only):
  ```python3
  createSelectiveSearchSegmentationStrategySize() -> retval
  ```
  """
  @spec createSelectiveSearchSegmentationStrategySize() :: Evision.XImgProc.SelectiveSearchSegmentationStrategySize.t() | {:error, String.t()}
  def createSelectiveSearchSegmentationStrategySize() do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_createSelectiveSearchSegmentationStrategySize(positional)
    |> to_struct()
  end

  @doc """
  Create a new size-based strategy
  ##### Return
  - **retval**: `Evision.XImgProc.SelectiveSearchSegmentationStrategyTexture.t()`

  Python prototype (for reference only):
  ```python3
  createSelectiveSearchSegmentationStrategyTexture() -> retval
  ```
  """
  @spec createSelectiveSearchSegmentationStrategyTexture() :: Evision.XImgProc.SelectiveSearchSegmentationStrategyTexture.t() | {:error, String.t()}
  def createSelectiveSearchSegmentationStrategyTexture() do
    positional = [
    ]
    :evision_nif.ximgproc_segmentation_createSelectiveSearchSegmentationStrategyTexture(positional)
    |> to_struct()
  end
end
