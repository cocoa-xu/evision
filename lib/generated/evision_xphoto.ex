defmodule Evision.XPhoto do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `XPhoto` struct.

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
  def to_struct({:ok, %{class: Evision.XPhoto, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.XPhoto, ref: ref}) do
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
  Implements an efficient fixed-point approximation for applying channel gains, which is
  the last step of multiple white balance algorithms.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Input three-channel image in the BGR color space (either CV_8UC3 or CV_16UC3)

  - **gainB**: `float`.

    gain for the B channel

  - **gainG**: `float`.

    gain for the G channel

  - **gainR**: `float`.

    gain for the R channel

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Output image of the same size and type as src.

  Python prototype (for reference only):
  ```python3
  applyChannelGains(src, gainB, gainG, gainR[, dst]) -> dst
  ```
  """
  @spec applyChannelGains(Evision.Mat.maybe_mat_in(), number(), number(), number(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def applyChannelGains(src, gainB, gainG, gainR, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_float(gainB) and is_float(gainG) and is_float(gainR) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      gainB: Evision.Internal.Structurise.from_struct(gainB),
      gainG: Evision.Internal.Structurise.from_struct(gainG),
      gainR: Evision.Internal.Structurise.from_struct(gainR)
    ]
    :evision_nif.xphoto_applyChannelGains(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Implements an efficient fixed-point approximation for applying channel gains, which is
  the last step of multiple white balance algorithms.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Input three-channel image in the BGR color space (either CV_8UC3 or CV_16UC3)

  - **gainB**: `float`.

    gain for the B channel

  - **gainG**: `float`.

    gain for the G channel

  - **gainR**: `float`.

    gain for the R channel

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Output image of the same size and type as src.

  Python prototype (for reference only):
  ```python3
  applyChannelGains(src, gainB, gainG, gainR[, dst]) -> dst
  ```
  """
  @spec applyChannelGains(Evision.Mat.maybe_mat_in(), number(), number(), number()) :: Evision.Mat.t() | {:error, String.t()}
  def applyChannelGains(src, gainB, gainG, gainR) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_float(gainB) and is_float(gainG) and is_float(gainR)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      gainB: Evision.Internal.Structurise.from_struct(gainB),
      gainG: Evision.Internal.Structurise.from_struct(gainG),
      gainR: Evision.Internal.Structurise.from_struct(gainR)
    ]
    :evision_nif.xphoto_applyChannelGains(positional)
    |> to_struct()
  end

  @doc """
  Performs image denoising using the Block-Matching and 3D-filtering algorithm
  <http://www.cs.tut.fi/~foi/GCF-BM3D/BM3D_TIP_2007.pdf> with several computational
  optimizations. Noise expected to be a gaussian white noise.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Input 8-bit or 16-bit 1-channel image.

  ##### Keyword Arguments
  - **h**: `float`.

    Parameter regulating filter strength. Big h value perfectly removes noise but also
    removes image details, smaller h value preserves details but also preserves some noise.

  - **templateWindowSize**: `integer()`.

    Size in pixels of the template patch that is used for block-matching.
    Should be power of 2.

  - **searchWindowSize**: `integer()`.

    Size in pixels of the window that is used to perform block-matching.
    Affect performance linearly: greater searchWindowsSize - greater denoising time.
    Must be larger than templateWindowSize.

  - **blockMatchingStep1**: `integer()`.

    Block matching threshold for the first step of BM3D (hard thresholding),
    i.e. maximum distance for which two blocks are considered similar.
    Value expressed in euclidean distance.

  - **blockMatchingStep2**: `integer()`.

    Block matching threshold for the second step of BM3D (Wiener filtering),
    i.e. maximum distance for which two blocks are considered similar.
    Value expressed in euclidean distance.

  - **groupSize**: `integer()`.

    Maximum size of the 3D group for collaborative filtering.

  - **slidingStep**: `integer()`.

    Sliding step to process every next reference block.

  - **beta**: `float`.

    Kaiser window parameter that affects the sidelobe attenuation of the transform of the
    window. Kaiser window is used in order to reduce border effects. To prevent usage of the window,
    set beta to zero.

  - **normType**: `integer()`.

    Norm used to calculate distance between blocks. L2 is slower than L1
    but yields more accurate results.

  - **step**: `integer()`.

    Step of BM3D to be executed. Possible variants are: step 1, step 2, both steps.

  - **transformType**: `integer()`.

    Type of the orthogonal transform used in collaborative filtering step.
    Currently only Haar transform is supported.

  ##### Return
  - **dstStep1**: `Evision.Mat.t()`.

    Output image of the first step of BM3D with the same size and type as src.

  - **dstStep2**: `Evision.Mat.t()`.

    Output image of the second step of BM3D with the same size and type as src.

  This function expected to be applied to grayscale images. Advanced usage of this function
  can be manual denoising of colored image in different colorspaces.
  @sa
  fastNlMeansDenoising

  Python prototype (for reference only):
  ```python3
  bm3dDenoising(src, dstStep1[, dstStep2[, h[, templateWindowSize[, searchWindowSize[, blockMatchingStep1[, blockMatchingStep2[, groupSize[, slidingStep[, beta[, normType[, step[, transformType]]]]]]]]]]]]) -> dstStep1, dstStep2
  ```
  """
  @spec bm3dDenoising(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:beta, term()} | {:blockMatchingStep1, term()} | {:blockMatchingStep2, term()} | {:groupSize, term()} | {:h, term()} | {:normType, term()} | {:searchWindowSize, term()} | {:slidingStep, term()} | {:step, term()} | {:templateWindowSize, term()} | {:transformType, term()}] | nil) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def bm3dDenoising(src, dstStep1, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(dstStep1, Evision.Mat) or is_struct(dstStep1, Nx.Tensor) or is_number(dstStep1) or is_tuple(dstStep1)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:beta, :blockMatchingStep1, :blockMatchingStep2, :groupSize, :h, :normType, :searchWindowSize, :slidingStep, :step, :templateWindowSize, :transformType])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dstStep1: Evision.Internal.Structurise.from_struct(dstStep1)
    ]
    :evision_nif.xphoto_bm3dDenoising(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  Performs image denoising using the Block-Matching and 3D-filtering algorithm
  <http://www.cs.tut.fi/~foi/GCF-BM3D/BM3D_TIP_2007.pdf> with several computational
  optimizations. Noise expected to be a gaussian white noise.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Input 8-bit or 16-bit 1-channel image.

  ##### Keyword Arguments
  - **h**: `float`.

    Parameter regulating filter strength. Big h value perfectly removes noise but also
    removes image details, smaller h value preserves details but also preserves some noise.

  - **templateWindowSize**: `integer()`.

    Size in pixels of the template patch that is used for block-matching.
    Should be power of 2.

  - **searchWindowSize**: `integer()`.

    Size in pixels of the window that is used to perform block-matching.
    Affect performance linearly: greater searchWindowsSize - greater denoising time.
    Must be larger than templateWindowSize.

  - **blockMatchingStep1**: `integer()`.

    Block matching threshold for the first step of BM3D (hard thresholding),
    i.e. maximum distance for which two blocks are considered similar.
    Value expressed in euclidean distance.

  - **blockMatchingStep2**: `integer()`.

    Block matching threshold for the second step of BM3D (Wiener filtering),
    i.e. maximum distance for which two blocks are considered similar.
    Value expressed in euclidean distance.

  - **groupSize**: `integer()`.

    Maximum size of the 3D group for collaborative filtering.

  - **slidingStep**: `integer()`.

    Sliding step to process every next reference block.

  - **beta**: `float`.

    Kaiser window parameter that affects the sidelobe attenuation of the transform of the
    window. Kaiser window is used in order to reduce border effects. To prevent usage of the window,
    set beta to zero.

  - **normType**: `integer()`.

    Norm used to calculate distance between blocks. L2 is slower than L1
    but yields more accurate results.

  - **step**: `integer()`.

    Step of BM3D to be executed. Possible variants are: step 1, step 2, both steps.

  - **transformType**: `integer()`.

    Type of the orthogonal transform used in collaborative filtering step.
    Currently only Haar transform is supported.

  ##### Return
  - **dstStep1**: `Evision.Mat.t()`.

    Output image of the first step of BM3D with the same size and type as src.

  - **dstStep2**: `Evision.Mat.t()`.

    Output image of the second step of BM3D with the same size and type as src.

  This function expected to be applied to grayscale images. Advanced usage of this function
  can be manual denoising of colored image in different colorspaces.
  @sa
  fastNlMeansDenoising

  Python prototype (for reference only):
  ```python3
  bm3dDenoising(src, dstStep1[, dstStep2[, h[, templateWindowSize[, searchWindowSize[, blockMatchingStep1[, blockMatchingStep2[, groupSize[, slidingStep[, beta[, normType[, step[, transformType]]]]]]]]]]]]) -> dstStep1, dstStep2
  ```
  #### Variant 2:
  Performs image denoising using the Block-Matching and 3D-filtering algorithm
  <http://www.cs.tut.fi/~foi/GCF-BM3D/BM3D_TIP_2007.pdf> with several computational
  optimizations. Noise expected to be a gaussian white noise.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Input 8-bit or 16-bit 1-channel image.

  ##### Keyword Arguments
  - **h**: `float`.

    Parameter regulating filter strength. Big h value perfectly removes noise but also
    removes image details, smaller h value preserves details but also preserves some noise.

  - **templateWindowSize**: `integer()`.

    Size in pixels of the template patch that is used for block-matching.
    Should be power of 2.

  - **searchWindowSize**: `integer()`.

    Size in pixels of the window that is used to perform block-matching.
    Affect performance linearly: greater searchWindowsSize - greater denoising time.
    Must be larger than templateWindowSize.

  - **blockMatchingStep1**: `integer()`.

    Block matching threshold for the first step of BM3D (hard thresholding),
    i.e. maximum distance for which two blocks are considered similar.
    Value expressed in euclidean distance.

  - **blockMatchingStep2**: `integer()`.

    Block matching threshold for the second step of BM3D (Wiener filtering),
    i.e. maximum distance for which two blocks are considered similar.
    Value expressed in euclidean distance.

  - **groupSize**: `integer()`.

    Maximum size of the 3D group for collaborative filtering.

  - **slidingStep**: `integer()`.

    Sliding step to process every next reference block.

  - **beta**: `float`.

    Kaiser window parameter that affects the sidelobe attenuation of the transform of the
    window. Kaiser window is used in order to reduce border effects. To prevent usage of the window,
    set beta to zero.

  - **normType**: `integer()`.

    Norm used to calculate distance between blocks. L2 is slower than L1
    but yields more accurate results.

  - **step**: `integer()`.

    Step of BM3D to be executed. Allowed are only BM3D_STEP1 and BM3D_STEPALL.
    BM3D_STEP2 is not allowed as it requires basic estimate to be present.

  - **transformType**: `integer()`.

    Type of the orthogonal transform used in collaborative filtering step.
    Currently only Haar transform is supported.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Output image with the same size and type as src.

  This function expected to be applied to grayscale images. Advanced usage of this function
  can be manual denoising of colored image in different colorspaces.
  @sa
  fastNlMeansDenoising

  Python prototype (for reference only):
  ```python3
  bm3dDenoising(src[, dst[, h[, templateWindowSize[, searchWindowSize[, blockMatchingStep1[, blockMatchingStep2[, groupSize[, slidingStep[, beta[, normType[, step[, transformType]]]]]]]]]]]]) -> dst
  ```

  """
  @spec bm3dDenoising(Evision.Mat.maybe_mat_in(), [{:beta, term()} | {:blockMatchingStep1, term()} | {:blockMatchingStep2, term()} | {:groupSize, term()} | {:h, term()} | {:normType, term()} | {:searchWindowSize, term()} | {:slidingStep, term()} | {:step, term()} | {:templateWindowSize, term()} | {:transformType, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def bm3dDenoising(src, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:beta, :blockMatchingStep1, :blockMatchingStep2, :groupSize, :h, :normType, :searchWindowSize, :slidingStep, :step, :templateWindowSize, :transformType])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.xphoto_bm3dDenoising(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec bm3dDenoising(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: {Evision.Mat.t(), Evision.Mat.t()} | {:error, String.t()}
  def bm3dDenoising(src, dstStep1) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(dstStep1, Evision.Mat) or is_struct(dstStep1, Nx.Tensor) or is_number(dstStep1) or is_tuple(dstStep1))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dstStep1: Evision.Internal.Structurise.from_struct(dstStep1)
    ]
    :evision_nif.xphoto_bm3dDenoising(positional)
    |> to_struct()
  end

  @doc """
  Performs image denoising using the Block-Matching and 3D-filtering algorithm
  <http://www.cs.tut.fi/~foi/GCF-BM3D/BM3D_TIP_2007.pdf> with several computational
  optimizations. Noise expected to be a gaussian white noise.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Input 8-bit or 16-bit 1-channel image.

  ##### Keyword Arguments
  - **h**: `float`.

    Parameter regulating filter strength. Big h value perfectly removes noise but also
    removes image details, smaller h value preserves details but also preserves some noise.

  - **templateWindowSize**: `integer()`.

    Size in pixels of the template patch that is used for block-matching.
    Should be power of 2.

  - **searchWindowSize**: `integer()`.

    Size in pixels of the window that is used to perform block-matching.
    Affect performance linearly: greater searchWindowsSize - greater denoising time.
    Must be larger than templateWindowSize.

  - **blockMatchingStep1**: `integer()`.

    Block matching threshold for the first step of BM3D (hard thresholding),
    i.e. maximum distance for which two blocks are considered similar.
    Value expressed in euclidean distance.

  - **blockMatchingStep2**: `integer()`.

    Block matching threshold for the second step of BM3D (Wiener filtering),
    i.e. maximum distance for which two blocks are considered similar.
    Value expressed in euclidean distance.

  - **groupSize**: `integer()`.

    Maximum size of the 3D group for collaborative filtering.

  - **slidingStep**: `integer()`.

    Sliding step to process every next reference block.

  - **beta**: `float`.

    Kaiser window parameter that affects the sidelobe attenuation of the transform of the
    window. Kaiser window is used in order to reduce border effects. To prevent usage of the window,
    set beta to zero.

  - **normType**: `integer()`.

    Norm used to calculate distance between blocks. L2 is slower than L1
    but yields more accurate results.

  - **step**: `integer()`.

    Step of BM3D to be executed. Allowed are only BM3D_STEP1 and BM3D_STEPALL.
    BM3D_STEP2 is not allowed as it requires basic estimate to be present.

  - **transformType**: `integer()`.

    Type of the orthogonal transform used in collaborative filtering step.
    Currently only Haar transform is supported.

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Output image with the same size and type as src.

  This function expected to be applied to grayscale images. Advanced usage of this function
  can be manual denoising of colored image in different colorspaces.
  @sa
  fastNlMeansDenoising

  Python prototype (for reference only):
  ```python3
  bm3dDenoising(src[, dst[, h[, templateWindowSize[, searchWindowSize[, blockMatchingStep1[, blockMatchingStep2[, groupSize[, slidingStep[, beta[, normType[, step[, transformType]]]]]]]]]]]]) -> dst
  ```
  """
  @spec bm3dDenoising(Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def bm3dDenoising(src) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src)
    ]
    :evision_nif.xphoto_bm3dDenoising(positional)
    |> to_struct()
  end

  @doc """
  Creates an instance of GrayworldWB
  ##### Return
  - **retval**: `Evision.XPhoto.GrayworldWB.t()`

  Python prototype (for reference only):
  ```python3
  createGrayworldWB() -> retval
  ```
  """
  @spec createGrayworldWB() :: Evision.XPhoto.GrayworldWB.t() | {:error, String.t()}
  def createGrayworldWB() do
    positional = [
    ]
    :evision_nif.xphoto_createGrayworldWB(positional)
    |> to_struct()
  end

  @doc """
  Creates an instance of LearningBasedWB
  ##### Keyword Arguments
  - **path_to_model**: `String`.

    Path to a .yml file with the model. If not specified, the default model is used

  ##### Return
  - **retval**: `Evision.XPhoto.LearningBasedWB.t()`

  Python prototype (for reference only):
  ```python3
  createLearningBasedWB([, path_to_model]) -> retval
  ```
  """
  @spec createLearningBasedWB([{:path_to_model, term()}] | nil) :: Evision.XPhoto.LearningBasedWB.t() | {:error, String.t()}
  def createLearningBasedWB(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:path_to_model])
    positional = [
    ]
    :evision_nif.xphoto_createLearningBasedWB(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates an instance of LearningBasedWB
  ##### Keyword Arguments
  - **path_to_model**: `String`.

    Path to a .yml file with the model. If not specified, the default model is used

  ##### Return
  - **retval**: `Evision.XPhoto.LearningBasedWB.t()`

  Python prototype (for reference only):
  ```python3
  createLearningBasedWB([, path_to_model]) -> retval
  ```
  """
  @spec createLearningBasedWB() :: Evision.XPhoto.LearningBasedWB.t() | {:error, String.t()}
  def createLearningBasedWB() do
    positional = [
    ]
    :evision_nif.xphoto_createLearningBasedWB(positional)
    |> to_struct()
  end

  @doc """
  Creates an instance of SimpleWB
  ##### Return
  - **retval**: `Evision.XPhoto.SimpleWB.t()`

  Python prototype (for reference only):
  ```python3
  createSimpleWB() -> retval
  ```
  """
  @spec createSimpleWB() :: Evision.XPhoto.SimpleWB.t() | {:error, String.t()}
  def createSimpleWB() do
    positional = [
    ]
    :evision_nif.xphoto_createSimpleWB(positional)
    |> to_struct()
  end

  @doc """
  Creates TonemapDurand object
  ##### Keyword Arguments
  - **gamma**: `float`.

    gamma value for gamma correction. See createTonemap

  - **contrast**: `float`.

    resulting contrast on logarithmic scale, i. e. log(max / min), where max and min
    are maximum and minimum luminance values of the resulting image.

  - **saturation**: `float`.

    saturation enhancement value. See createTonemapDrago

  - **sigma_color**: `float`.

    bilateral filter sigma in color space

  - **sigma_space**: `float`.

    bilateral filter sigma in coordinate space

  ##### Return
  - **retval**: `Evision.XPhoto.TonemapDurand.t()`

  You need to set the OPENCV_ENABLE_NONFREE option in cmake to use those. Use them at your own risk.

  Python prototype (for reference only):
  ```python3
  createTonemapDurand([, gamma[, contrast[, saturation[, sigma_color[, sigma_space]]]]]) -> retval
  ```
  """
  @spec createTonemapDurand([{:contrast, term()} | {:gamma, term()} | {:saturation, term()} | {:sigma_color, term()} | {:sigma_space, term()}] | nil) :: Evision.XPhoto.TonemapDurand.t() | {:error, String.t()}
  def createTonemapDurand(opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:contrast, :gamma, :saturation, :sigma_color, :sigma_space])
    positional = [
    ]
    :evision_nif.xphoto_createTonemapDurand(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Creates TonemapDurand object
  ##### Keyword Arguments
  - **gamma**: `float`.

    gamma value for gamma correction. See createTonemap

  - **contrast**: `float`.

    resulting contrast on logarithmic scale, i. e. log(max / min), where max and min
    are maximum and minimum luminance values of the resulting image.

  - **saturation**: `float`.

    saturation enhancement value. See createTonemapDrago

  - **sigma_color**: `float`.

    bilateral filter sigma in color space

  - **sigma_space**: `float`.

    bilateral filter sigma in coordinate space

  ##### Return
  - **retval**: `Evision.XPhoto.TonemapDurand.t()`

  You need to set the OPENCV_ENABLE_NONFREE option in cmake to use those. Use them at your own risk.

  Python prototype (for reference only):
  ```python3
  createTonemapDurand([, gamma[, contrast[, saturation[, sigma_color[, sigma_space]]]]]) -> retval
  ```
  """
  @spec createTonemapDurand() :: Evision.XPhoto.TonemapDurand.t() | {:error, String.t()}
  def createTonemapDurand() do
    positional = [
    ]
    :evision_nif.xphoto_createTonemapDurand(positional)
    |> to_struct()
  end

  @doc """
  The function implements simple dct-based denoising

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    source image

  - **dst**: `Evision.Mat`.

    destination image

  - **sigma**: `double`.

    expected noise standard deviation

  ##### Keyword Arguments
  - **psize**: `integer()`.

    size of block side where dct is computed

  <http://www.ipol.im/pub/art/2011/ys-dct/>.

  @sa
  fastNlMeansDenoising

  Python prototype (for reference only):
  ```python3
  dctDenoising(src, dst, sigma[, psize]) -> None
  ```
  """
  @spec dctDenoising(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number(), [{:psize, term()}] | nil) :: :ok | {:error, String.t()}
  def dctDenoising(src, dst, sigma, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(dst, Evision.Mat) or is_struct(dst, Nx.Tensor) or is_number(dst) or is_tuple(dst)) and is_number(sigma) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:psize])
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dst: Evision.Internal.Structurise.from_struct(dst),
      sigma: Evision.Internal.Structurise.from_struct(sigma)
    ]
    :evision_nif.xphoto_dctDenoising(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  The function implements simple dct-based denoising

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    source image

  - **dst**: `Evision.Mat`.

    destination image

  - **sigma**: `double`.

    expected noise standard deviation

  ##### Keyword Arguments
  - **psize**: `integer()`.

    size of block side where dct is computed

  <http://www.ipol.im/pub/art/2011/ys-dct/>.

  @sa
  fastNlMeansDenoising

  Python prototype (for reference only):
  ```python3
  dctDenoising(src, dst, sigma[, psize]) -> None
  ```
  """
  @spec dctDenoising(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), number()) :: :ok | {:error, String.t()}
  def dctDenoising(src, dst, sigma) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(dst, Evision.Mat) or is_struct(dst, Nx.Tensor) or is_number(dst) or is_tuple(dst)) and is_number(sigma)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      dst: Evision.Internal.Structurise.from_struct(dst),
      sigma: Evision.Internal.Structurise.from_struct(sigma)
    ]
    :evision_nif.xphoto_dctDenoising(positional)
    |> to_struct()
  end

  @doc """
  The function implements different single-image inpainting algorithms.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    source image
    - #INPAINT_SHIFTMAP: it could be of any type and any number of channels from 1 to 4. In case of
      3- and 4-channels images the function expect them in CIELab colorspace or similar one, where first
      color component shows intensity, while second and third shows colors. Nonetheless you can try any
      colorspaces.
    - #INPAINT_FSR_BEST or #INPAINT_FSR_FAST: 1-channel grayscale or 3-channel BGR image.

  - **mask**: `Evision.Mat`.

    mask (#CV_8UC1), where non-zero pixels indicate valid image area, while zero pixels
    indicate area to be inpainted

  - **dst**: `Evision.Mat`.

    destination image

  - **algorithmType**: `integer()`.

    see xphoto::InpaintTypes

  See the original papers @cite He2012 (Shiftmap) or @cite GenserPCS2018 and @cite SeilerTIP2015 (FSR) for details.

  Python prototype (for reference only):
  ```python3
  inpaint(src, mask, dst, algorithmType) -> None
  ```
  """
  @spec inpaint(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), integer()) :: :ok | {:error, String.t()}
  def inpaint(src, mask, dst, algorithmType) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(mask, Evision.Mat) or is_struct(mask, Nx.Tensor) or is_number(mask) or is_tuple(mask)) and (is_struct(dst, Evision.Mat) or is_struct(dst, Nx.Tensor) or is_number(dst) or is_tuple(dst)) and is_integer(algorithmType)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      mask: Evision.Internal.Structurise.from_struct(mask),
      dst: Evision.Internal.Structurise.from_struct(dst),
      algorithmType: Evision.Internal.Structurise.from_struct(algorithmType)
    ]
    :evision_nif.xphoto_inpaint(positional)
    |> to_struct()
  end

  @doc """
  oilPainting
  See the book @cite Holzmann1988 for details.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Input three-channel or one channel image (either CV_8UC3 or CV_8UC1)

  - **size**: `integer()`.

    neighbouring size is 2-size+1

  - **dynRatio**: `integer()`.

    image is divided by dynRatio before histogram processing

  - **code**: `integer()`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Output image of the same size and type as src.

  Python prototype (for reference only):
  ```python3
  oilPainting(src, size, dynRatio, code[, dst]) -> dst
  ```
  """
  @spec oilPainting(Evision.Mat.maybe_mat_in(), integer(), integer(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def oilPainting(src, size, dynRatio, code, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(size) and is_integer(dynRatio) and is_integer(code) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      size: Evision.Internal.Structurise.from_struct(size),
      dynRatio: Evision.Internal.Structurise.from_struct(dynRatio),
      code: Evision.Internal.Structurise.from_struct(code)
    ]
    :evision_nif.xphoto_oilPainting(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  #### Variant 1:
  oilPainting
  See the book @cite Holzmann1988 for details.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Input three-channel or one channel image (either CV_8UC3 or CV_8UC1)

  - **size**: `integer()`.

    neighbouring size is 2-size+1

  - **dynRatio**: `integer()`.

    image is divided by dynRatio before histogram processing

  - **code**: `integer()`

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Output image of the same size and type as src.

  Python prototype (for reference only):
  ```python3
  oilPainting(src, size, dynRatio, code[, dst]) -> dst
  ```
  #### Variant 2:
  oilPainting
  See the book @cite Holzmann1988 for details.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Input three-channel or one channel image (either CV_8UC3 or CV_8UC1)

  - **size**: `integer()`.

    neighbouring size is 2-size+1

  - **dynRatio**: `integer()`.

    image is divided by dynRatio before histogram processing

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Output image of the same size and type as src.

  Python prototype (for reference only):
  ```python3
  oilPainting(src, size, dynRatio[, dst]) -> dst
  ```

  """
  @spec oilPainting(Evision.Mat.maybe_mat_in(), integer(), integer(), [{atom(), term()},...] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def oilPainting(src, size, dynRatio, opts) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(size) and is_integer(dynRatio) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      size: Evision.Internal.Structurise.from_struct(size),
      dynRatio: Evision.Internal.Structurise.from_struct(dynRatio)
    ]
    :evision_nif.xphoto_oilPainting(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end
  @spec oilPainting(Evision.Mat.maybe_mat_in(), integer(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def oilPainting(src, size, dynRatio, code) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(size) and is_integer(dynRatio) and is_integer(code)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      size: Evision.Internal.Structurise.from_struct(size),
      dynRatio: Evision.Internal.Structurise.from_struct(dynRatio),
      code: Evision.Internal.Structurise.from_struct(code)
    ]
    :evision_nif.xphoto_oilPainting(positional)
    |> to_struct()
  end

  @doc """
  oilPainting
  See the book @cite Holzmann1988 for details.

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    Input three-channel or one channel image (either CV_8UC3 or CV_8UC1)

  - **size**: `integer()`.

    neighbouring size is 2-size+1

  - **dynRatio**: `integer()`.

    image is divided by dynRatio before histogram processing

  ##### Return
  - **dst**: `Evision.Mat.t()`.

    Output image of the same size and type as src.

  Python prototype (for reference only):
  ```python3
  oilPainting(src, size, dynRatio[, dst]) -> dst
  ```
  """
  @spec oilPainting(Evision.Mat.maybe_mat_in(), integer(), integer()) :: Evision.Mat.t() | {:error, String.t()}
  def oilPainting(src, size, dynRatio) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(size) and is_integer(dynRatio)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      size: Evision.Internal.Structurise.from_struct(size),
      dynRatio: Evision.Internal.Structurise.from_struct(dynRatio)
    ]
    :evision_nif.xphoto_oilPainting(positional)
    |> to_struct()
  end
end
