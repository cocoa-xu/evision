defmodule Evision.CCM.ColorCorrectionModel do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CCM.ColorCorrectionModel` struct.

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
  def to_struct({:ok, %{class: Evision.CCM.ColorCorrectionModel, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CCM.ColorCorrectionModel, ref: ref}) do
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
  Color Correction Model

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    detected colors of ColorChecker patches;\\n
    the color type is RGB not BGR, and the color values are in [0, 1];

  - **colors**: `Evision.Mat`.

    the reference color values, the color values are in [0, 1].

  - **ref_cs**: `COLOR_SPACE`.

    the corresponding color space
    If the color type is some RGB, the format is RGB not BGR;

  - **colored**: `Evision.Mat`.

    mask of colored color

  ##### Return
  - **self**: `ColorCorrectionModel`

  Python prototype (for reference only):
  ```python3
  ColorCorrectionModel(src, colors, ref_cs, colored) -> <ccm_ColorCorrectionModel object>
  ```
  """
  @spec colorCorrectionModel(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.CCM.COLOR_SPACE.enum(), Evision.Mat.maybe_mat_in()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def colorCorrectionModel(src, colors, ref_cs, colored) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(colors, Evision.Mat) or is_struct(colors, Nx.Tensor) or is_number(colors) or is_tuple(colors)) and is_integer(ref_cs) and (is_struct(colored, Evision.Mat) or is_struct(colored, Nx.Tensor) or is_number(colored) or is_tuple(colored))
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      colors: Evision.Internal.Structurise.from_struct(colors),
      ref_cs: Evision.Internal.Structurise.from_struct(ref_cs),
      colored: Evision.Internal.Structurise.from_struct(colored)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_ColorCorrectionModel(positional)
    |> to_struct()
  end

  @doc """
  Color Correction Model

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    detected colors of ColorChecker patches;\\n
    the color type is RGB not BGR, and the color values are in [0, 1];

  - **colors**: `Evision.Mat`.

    the reference color values, the color values are in [0, 1].\\n

  - **ref_cs**: `COLOR_SPACE`.

    the corresponding color space
    If the color type is some RGB, the format is RGB not BGR;\\n

  ##### Return
  - **self**: `ColorCorrectionModel`

  Python prototype (for reference only):
  ```python3
  ColorCorrectionModel(src, colors, ref_cs) -> <ccm_ColorCorrectionModel object>
  ```
  """
  @spec colorCorrectionModel(Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), Evision.CCM.COLOR_SPACE.enum()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def colorCorrectionModel(src, colors, ref_cs) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and (is_struct(colors, Evision.Mat) or is_struct(colors, Nx.Tensor) or is_number(colors) or is_tuple(colors)) and is_integer(ref_cs)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      colors: Evision.Internal.Structurise.from_struct(colors),
      ref_cs: Evision.Internal.Structurise.from_struct(ref_cs)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_ColorCorrectionModel(positional)
    |> to_struct()
  end

  @doc """
  Color Correction Model

  ##### Positional Arguments
  - **src**: `Evision.Mat`.

    detected colors of ColorChecker patches;\\n
    the color type is RGB not BGR, and the color values are in [0, 1];

  - **constcolor**: `CONST_COLOR`.

    the Built-in color card

  ##### Return
  - **self**: `ColorCorrectionModel`

  Supported list of color cards:
  - @ref COLORCHECKER_Macbeth, the Macbeth ColorChecker
  - @ref COLORCHECKER_Vinyl, the DKK ColorChecker
  - @ref COLORCHECKER_DigitalSG, the DigitalSG ColorChecker with 140 squares

  Python prototype (for reference only):
  ```python3
  ColorCorrectionModel(src, constcolor) -> <ccm_ColorCorrectionModel object>
  ```
  """
  @spec colorCorrectionModel(Evision.Mat.maybe_mat_in(), Evision.CCM.CONST_COLOR.enum()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def colorCorrectionModel(src, constcolor) when (is_struct(src, Evision.Mat) or is_struct(src, Nx.Tensor) or is_number(src) or is_tuple(src)) and is_integer(constcolor)
  do
    positional = [
      src: Evision.Internal.Structurise.from_struct(src),
      constcolor: Evision.Internal.Structurise.from_struct(constcolor)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_ColorCorrectionModel(positional)
    |> to_struct()
  end

  @doc """
  getCCM

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getCCM() -> retval
  ```
  """
  @spec getCCM(Evision.CCM.ColorCorrectionModel.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getCCM(self) do
    positional = [
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_getCCM(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getLoss

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`

  ##### Return
  - **retval**: `double`

  Python prototype (for reference only):
  ```python3
  getLoss() -> retval
  ```
  """
  @spec getLoss(Evision.CCM.ColorCorrectionModel.t()) :: number() | {:error, String.t()}
  def getLoss(self) do
    positional = [
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_getLoss(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMask

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getMask() -> retval
  ```
  """
  @spec getMask(Evision.CCM.ColorCorrectionModel.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getMask(self) do
    positional = [
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_getMask(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getWeights

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  getWeights() -> retval
  ```
  """
  @spec getWeights(Evision.CCM.ColorCorrectionModel.t()) :: Evision.Mat.t() | {:error, String.t()}
  def getWeights(self) do
    positional = [
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_getWeights(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  get_dst_rgbl

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  get_dst_rgbl() -> retval
  ```
  """
  @spec get_dst_rgbl(Evision.CCM.ColorCorrectionModel.t()) :: Evision.Mat.t() | {:error, String.t()}
  def get_dst_rgbl(self) do
    positional = [
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_get_dst_rgbl(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  get_src_rgbl

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  get_src_rgbl() -> retval
  ```
  """
  @spec get_src_rgbl(Evision.CCM.ColorCorrectionModel.t()) :: Evision.Mat.t() | {:error, String.t()}
  def get_src_rgbl(self) do
    positional = [
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_get_src_rgbl(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  Infer using fitting ccm.

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **img**: `Evision.Mat`.

    the input image.

  ##### Keyword Arguments
  - **islinear**: `bool`.

    default false.

  ##### Return
  - **retval**: `Evision.Mat.t()`

  @return the output array.

  Python prototype (for reference only):
  ```python3
  infer(img[, islinear]) -> retval
  ```
  """
  @spec infer(Evision.CCM.ColorCorrectionModel.t(), Evision.Mat.maybe_mat_in(), [{:islinear, term()}] | nil) :: Evision.Mat.t() | {:error, String.t()}
  def infer(self, img, opts) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:islinear])
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_infer(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  Infer using fitting ccm.

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **img**: `Evision.Mat`.

    the input image.

  ##### Keyword Arguments
  - **islinear**: `bool`.

    default false.

  ##### Return
  - **retval**: `Evision.Mat.t()`

  @return the output array.

  Python prototype (for reference only):
  ```python3
  infer(img[, islinear]) -> retval
  ```
  """
  @spec infer(Evision.CCM.ColorCorrectionModel.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def infer(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_infer(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  make color correction

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`

  Python prototype (for reference only):
  ```python3
  run() -> None
  ```
  """
  @spec run(Evision.CCM.ColorCorrectionModel.t()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def run(self) do
    positional = [
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_run(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set ccm_type

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **ccm_type**: `CCM_TYPE`.

    the shape of color correction matrix(CCM);\\n
    default: @ref CCM_3x3

  Python prototype (for reference only):
  ```python3
  setCCM_TYPE(ccm_type) -> None
  ```
  """
  @spec setCCM_TYPE(Evision.CCM.ColorCorrectionModel.t(), Evision.CCM.CCM_TYPE.enum()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def setCCM_TYPE(self, ccm_type) when is_integer(ccm_type)
  do
    positional = [
      ccm_type: Evision.Internal.Structurise.from_struct(ccm_type)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_setCCM_TYPE(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set ColorSpace

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **cs**: `COLOR_SPACE`.

    the absolute color space that detected colors convert to;\\n
    default: @ref COLOR_SPACE_sRGB

  **Note**: It should be some RGB color space;
  Supported list of color cards:
  - @ref COLOR_SPACE_sRGB
  - @ref COLOR_SPACE_AdobeRGB
  - @ref COLOR_SPACE_WideGamutRGB
  - @ref COLOR_SPACE_ProPhotoRGB
  - @ref COLOR_SPACE_DCI_P3_RGB
  - @ref COLOR_SPACE_AppleRGB
  - @ref COLOR_SPACE_REC_709_RGB
  - @ref COLOR_SPACE_REC_2020_RGB

  Python prototype (for reference only):
  ```python3
  setColorSpace(cs) -> None
  ```
  """
  @spec setColorSpace(Evision.CCM.ColorCorrectionModel.t(), Evision.CCM.COLOR_SPACE.enum()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def setColorSpace(self, cs) when is_integer(cs)
  do
    positional = [
      cs: Evision.Internal.Structurise.from_struct(cs)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_setColorSpace(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set Distance

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **distance**: `DISTANCE_TYPE`.

    the type of color distance;\\n
    default: @ref DISTANCE_CIE2000

  Python prototype (for reference only):
  ```python3
  setDistance(distance) -> None
  ```
  """
  @spec setDistance(Evision.CCM.ColorCorrectionModel.t(), Evision.CCM.DISTANCE_TYPE.enum()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def setDistance(self, distance) when is_integer(distance)
  do
    positional = [
      distance: Evision.Internal.Structurise.from_struct(distance)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_setDistance(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set Epsilon

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **epsilon**: `double`.

    used in MinProblemSolver-DownhillSolver;\\n
    Terminal criteria to the algorithm;\\n
    default: 1e-4;

  Python prototype (for reference only):
  ```python3
  setEpsilon(epsilon) -> None
  ```
  """
  @spec setEpsilon(Evision.CCM.ColorCorrectionModel.t(), number()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def setEpsilon(self, epsilon) when is_number(epsilon)
  do
    positional = [
      epsilon: Evision.Internal.Structurise.from_struct(epsilon)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_setEpsilon(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set InitialMethod

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **initial_method_type**: `INITIAL_METHOD_TYPE`.

    the method of calculating CCM initial value;\\n
    default: INITIAL_METHOD_LEAST_SQUARE

  Python prototype (for reference only):
  ```python3
  setInitialMethod(initial_method_type) -> None
  ```
  """
  @spec setInitialMethod(Evision.CCM.ColorCorrectionModel.t(), Evision.CCM.INITIAL_METHOD_TYPE.enum()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def setInitialMethod(self, initial_method_type) when is_integer(initial_method_type)
  do
    positional = [
      initial_method_type: Evision.Internal.Structurise.from_struct(initial_method_type)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_setInitialMethod(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set Linear

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **linear_type**: `LINEAR_TYPE`.

    the method of linearization;\\n
    default: @ref LINEARIZATION_GAMMA

  Python prototype (for reference only):
  ```python3
  setLinear(linear_type) -> None
  ```
  """
  @spec setLinear(Evision.CCM.ColorCorrectionModel.t(), Evision.CCM.LINEAR_TYPE.enum()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def setLinear(self, linear_type) when is_integer(linear_type)
  do
    positional = [
      linear_type: Evision.Internal.Structurise.from_struct(linear_type)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_setLinear(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set degree

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **deg**: `integer()`.

    the degree of linearization polynomial;\\n
    default: 3

  **Note**: only valid when linear is set to
  - @ref LINEARIZATION_COLORPOLYFIT
  - @ref LINEARIZATION_GRAYPOLYFIT
  - @ref LINEARIZATION_COLORLOGPOLYFIT
  - @ref LINEARIZATION_GRAYLOGPOLYFIT

  Python prototype (for reference only):
  ```python3
  setLinearDegree(deg) -> None
  ```
  """
  @spec setLinearDegree(Evision.CCM.ColorCorrectionModel.t(), integer()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def setLinearDegree(self, deg) when is_integer(deg)
  do
    positional = [
      deg: Evision.Internal.Structurise.from_struct(deg)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_setLinearDegree(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set Gamma

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **gamma**: `double`.

    the gamma value of gamma correction;\\n
    default: 2.2;

  **Note**: only valid when linear is set to "gamma";\\n

  Python prototype (for reference only):
  ```python3
  setLinearGamma(gamma) -> None
  ```
  """
  @spec setLinearGamma(Evision.CCM.ColorCorrectionModel.t(), number()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def setLinearGamma(self, gamma) when is_number(gamma)
  do
    positional = [
      gamma: Evision.Internal.Structurise.from_struct(gamma)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_setLinearGamma(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set MaxCount

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **max_count**: `integer()`.

    used in MinProblemSolver-DownhillSolver;\\n
    Terminal criteria to the algorithm;\\n
    default: 5000;

  Python prototype (for reference only):
  ```python3
  setMaxCount(max_count) -> None
  ```
  """
  @spec setMaxCount(Evision.CCM.ColorCorrectionModel.t(), integer()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def setMaxCount(self, max_count) when is_integer(max_count)
  do
    positional = [
      max_count: Evision.Internal.Structurise.from_struct(max_count)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_setMaxCount(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set SaturatedThreshold.
  The colors in the closed interval [lower, upper] are reserved to participate
  in the calculation of the loss function and initialization parameters

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **lower**: `double`.

    the lower threshold to determine saturation;\\n
    default: 0;

  - **upper**: `double`.

    the upper threshold to determine saturation;\\n
    default: 0

  Python prototype (for reference only):
  ```python3
  setSaturatedThreshold(lower, upper) -> None
  ```
  """
  @spec setSaturatedThreshold(Evision.CCM.ColorCorrectionModel.t(), number(), number()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def setSaturatedThreshold(self, lower, upper) when is_number(lower) and is_number(upper)
  do
    positional = [
      lower: Evision.Internal.Structurise.from_struct(lower),
      upper: Evision.Internal.Structurise.from_struct(upper)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_setSaturatedThreshold(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set WeightCoeff

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **weights_coeff**: `double`.

    the exponent number of L* component of the reference color in CIE Lab color space;\\n
    default: 0

  Python prototype (for reference only):
  ```python3
  setWeightCoeff(weights_coeff) -> None
  ```
  """
  @spec setWeightCoeff(Evision.CCM.ColorCorrectionModel.t(), number()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def setWeightCoeff(self, weights_coeff) when is_number(weights_coeff)
  do
    positional = [
      weights_coeff: Evision.Internal.Structurise.from_struct(weights_coeff)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_setWeightCoeff(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  set WeightsList

  ##### Positional Arguments
  - **self**: `Evision.CCM.ColorCorrectionModel.t()`
  - **weights_list**: `Evision.Mat`.

    the list of weight of each color;\\n
    default: empty array

  Python prototype (for reference only):
  ```python3
  setWeightsList(weights_list) -> None
  ```
  """
  @spec setWeightsList(Evision.CCM.ColorCorrectionModel.t(), Evision.Mat.maybe_mat_in()) :: Evision.CCM.ColorCorrectionModel.t() | {:error, String.t()}
  def setWeightsList(self, weights_list) when (is_struct(weights_list, Evision.Mat) or is_struct(weights_list, Nx.Tensor) or is_number(weights_list) or is_tuple(weights_list))
  do
    positional = [
      weights_list: Evision.Internal.Structurise.from_struct(weights_list)
    ]
    :evision_nif.ccm_ccm_ColorCorrectionModel_setWeightsList(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
