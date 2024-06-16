defmodule Evision.LineMod.Detector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineMod.Detector` struct.

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
  def to_struct({:ok, %{class: Evision.LineMod.Detector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineMod.Detector, ref: ref}) do
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
  Detector

  ##### Positional Arguments
  - **modalities**: `[Ptr_Modality]`
  - **t_pyramid**: `[integer()]`

  ##### Return
  - **self**: `Detector`

   \\brief Constructor.
   \\param modalities       Modalities to use (color gradients, depth normals, ...).
   \\param T_pyramid        Value of the sampling step T at each pyramid level. The
                           number of pyramid levels is T_pyramid.size().

  Python prototype (for reference only):
  ```python3
  Detector(modalities, T_pyramid) -> <linemod_Detector object>
  ```
  """
  @spec detector(list(Evision.LineMod.Ptr_Modality.t()), list(integer())) :: Evision.LineMod.Detector.t() | {:error, String.t()}
  def detector(modalities, t_pyramid) when is_list(modalities) and is_list(t_pyramid)
  do
    positional = [
      modalities: Evision.Internal.Structurise.from_struct(modalities),
      t_pyramid: Evision.Internal.Structurise.from_struct(t_pyramid)
    ]
    :evision_nif.linemod_linemod_Detector_Detector(positional)
    |> to_struct()
  end

  @doc """
  Detector
  ##### Return
  - **self**: `Detector`

   \\brief Empty constructor, initialize with read().

  Python prototype (for reference only):
  ```python3
  Detector() -> <linemod_Detector object>
  ```
  """
  @spec detector() :: Evision.LineMod.Detector.t() | {:error, String.t()}
  def detector() do
    positional = [
    ]
    :evision_nif.linemod_linemod_Detector_Detector(positional)
    |> to_struct()
  end

  @doc """
  addSyntheticTemplate

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`
  - **templates**: `[Evision.LineMod.Template]`
  - **class_id**: `String`

  ##### Return
  - **retval**: `integer()`

   \\brief Add a new object template computed by external means.

  Python prototype (for reference only):
  ```python3
  addSyntheticTemplate(templates, class_id) -> retval
  ```
  """
  @spec addSyntheticTemplate(Evision.LineMod.Detector.t(), list(Evision.LineMod.Template.t()), binary()) :: integer() | {:error, String.t()}
  def addSyntheticTemplate(self, templates, class_id) when is_list(templates) and is_binary(class_id)
  do
    positional = [
      templates: Evision.Internal.Structurise.from_struct(templates),
      class_id: Evision.Internal.Structurise.from_struct(class_id)
    ]
    :evision_nif.linemod_linemod_Detector_addSyntheticTemplate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  addTemplate

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`
  - **sources**: `[Evision.Mat]`
  - **class_id**: `String`
  - **object_mask**: `Evision.Mat`

  ##### Return
  - **retval**: `integer()`
  - **bounding_box**: `Rect*`

   \\brief Add new object template.
   \\param      sources      Source images, one for each modality.
   \\param      class_id     Object class ID.
   \\param      object_mask  Mask separating object from background.
   \\param[out] bounding_box Optionally return bounding box of the extracted features.
   \\return Template ID, or -1 if failed to extract a valid template.

  Python prototype (for reference only):
  ```python3
  addTemplate(sources, class_id, object_mask) -> retval, bounding_box
  ```
  """
  @spec addTemplate(Evision.LineMod.Detector.t(), list(Evision.Mat.maybe_mat_in()), binary(), Evision.Mat.maybe_mat_in()) :: {integer(), {number(), number(), number(), number()}} | {:error, String.t()}
  def addTemplate(self, sources, class_id, object_mask) when is_list(sources) and is_binary(class_id) and (is_struct(object_mask, Evision.Mat) or is_struct(object_mask, Nx.Tensor) or is_number(object_mask) or is_tuple(object_mask))
  do
    positional = [
      sources: Evision.Internal.Structurise.from_struct(sources),
      class_id: Evision.Internal.Structurise.from_struct(class_id),
      object_mask: Evision.Internal.Structurise.from_struct(object_mask)
    ]
    :evision_nif.linemod_linemod_Detector_addTemplate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  classIds

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`

  ##### Return
  - **retval**: `[String]`

  Python prototype (for reference only):
  ```python3
  classIds() -> retval
  ```
  """
  @spec classIds(Evision.LineMod.Detector.t()) :: list(binary()) | {:error, String.t()}
  def classIds(self) do
    positional = [
    ]
    :evision_nif.linemod_linemod_Detector_classIds(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getModalities

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`

  ##### Return
  - **retval**: `[Modality]`

   \\brief Get the modalities used by this detector.
   You are not permitted to add/remove modalities, but you may dynamic_cast them to
   tweak parameters.

  Python prototype (for reference only):
  ```python3
  getModalities() -> retval
  ```
  """
  @spec getModalities(Evision.LineMod.Detector.t()) :: list(Evision.LineMod.Modality.t()) | {:error, String.t()}
  def getModalities(self) do
    positional = [
    ]
    :evision_nif.linemod_linemod_Detector_getModalities(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getT

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`
  - **pyramid_level**: `integer()`

  ##### Return
  - **retval**: `integer()`

   \\brief Get sampling step T at pyramid_level.

  Python prototype (for reference only):
  ```python3
  getT(pyramid_level) -> retval
  ```
  """
  @spec getT(Evision.LineMod.Detector.t(), integer()) :: integer() | {:error, String.t()}
  def getT(self, pyramid_level) when is_integer(pyramid_level)
  do
    positional = [
      pyramid_level: Evision.Internal.Structurise.from_struct(pyramid_level)
    ]
    :evision_nif.linemod_linemod_Detector_getT(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getTemplates

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`
  - **class_id**: `String`
  - **template_id**: `integer()`

  ##### Return
  - **retval**: `[Evision.LineMod.Template]`

   \\brief Get the template pyramid identified by template_id.
   For example, with 2 modalities (Gradient, Normal) and two pyramid levels
   (L0, L1), the order is (GradientL0, NormalL0, GradientL1, NormalL1).

  Python prototype (for reference only):
  ```python3
  getTemplates(class_id, template_id) -> retval
  ```
  """
  @spec getTemplates(Evision.LineMod.Detector.t(), binary(), integer()) :: list(Evision.LineMod.Template.t()) | {:error, String.t()}
  def getTemplates(self, class_id, template_id) when is_binary(class_id) and is_integer(template_id)
  do
    positional = [
      class_id: Evision.Internal.Structurise.from_struct(class_id),
      template_id: Evision.Internal.Structurise.from_struct(template_id)
    ]
    :evision_nif.linemod_linemod_Detector_getTemplates(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  match

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`
  - **sources**: `[Evision.Mat]`
  - **threshold**: `float`

  ##### Keyword Arguments
  - **class_ids**: `[String]`.
  - **masks**: `[Evision.Mat]`.

  ##### Return
  - **matches**: `[Match]`
  - **quantized_images**: `[Evision.Mat]`.

   \\brief Detect objects by template matching.
   Matches globally at the lowest pyramid level, then refines locally stepping up the pyramid.
   \\param      sources   Source images, one for each modality.
   \\param      threshold Similarity threshold, a percentage between 0 and 100.
   \\param[out] matches   Template matches, sorted by similarity score.
   \\param      class_ids If non-empty, only search for the desired object classes.
   \\param[out] quantized_images Optionally return vector<Mat> of quantized images.
   \\param      masks     The masks for consideration during matching. The masks should be CV_8UC1
                         where 255 represents a valid pixel.  If non-empty, the vector must be
                         the same size as sources.  Each element must be
                         empty or the same size as its corresponding source.

  Python prototype (for reference only):
  ```python3
  match(sources, threshold[, class_ids[, quantized_images[, masks]]]) -> matches, quantized_images
  ```
  """
  @spec match(Evision.LineMod.Detector.t(), list(Evision.Mat.maybe_mat_in()), number(), [{:class_ids, term()} | {:masks, term()}] | nil) :: {list(Evision.LineMod.Match.t()), list(Evision.Mat.t())} | {:error, String.t()}
  def match(self, sources, threshold, opts) when is_list(sources) and is_float(threshold) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:class_ids, :masks])
    positional = [
      sources: Evision.Internal.Structurise.from_struct(sources),
      threshold: Evision.Internal.Structurise.from_struct(threshold)
    ]
    :evision_nif.linemod_linemod_Detector_match(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  match

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`
  - **sources**: `[Evision.Mat]`
  - **threshold**: `float`

  ##### Keyword Arguments
  - **class_ids**: `[String]`.
  - **masks**: `[Evision.Mat]`.

  ##### Return
  - **matches**: `[Match]`
  - **quantized_images**: `[Evision.Mat]`.

   \\brief Detect objects by template matching.
   Matches globally at the lowest pyramid level, then refines locally stepping up the pyramid.
   \\param      sources   Source images, one for each modality.
   \\param      threshold Similarity threshold, a percentage between 0 and 100.
   \\param[out] matches   Template matches, sorted by similarity score.
   \\param      class_ids If non-empty, only search for the desired object classes.
   \\param[out] quantized_images Optionally return vector<Mat> of quantized images.
   \\param      masks     The masks for consideration during matching. The masks should be CV_8UC1
                         where 255 represents a valid pixel.  If non-empty, the vector must be
                         the same size as sources.  Each element must be
                         empty or the same size as its corresponding source.

  Python prototype (for reference only):
  ```python3
  match(sources, threshold[, class_ids[, quantized_images[, masks]]]) -> matches, quantized_images
  ```
  """
  @spec match(Evision.LineMod.Detector.t(), list(Evision.Mat.maybe_mat_in()), number()) :: {list(Evision.LineMod.Match.t()), list(Evision.Mat.t())} | {:error, String.t()}
  def match(self, sources, threshold) when is_list(sources) and is_float(threshold)
  do
    positional = [
      sources: Evision.Internal.Structurise.from_struct(sources),
      threshold: Evision.Internal.Structurise.from_struct(threshold)
    ]
    :evision_nif.linemod_linemod_Detector_match(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  numClasses

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  numClasses() -> retval
  ```
  """
  @spec numClasses(Evision.LineMod.Detector.t()) :: integer() | {:error, String.t()}
  def numClasses(self) do
    positional = [
    ]
    :evision_nif.linemod_linemod_Detector_numClasses(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  numTemplates

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`
  - **class_id**: `String`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  numTemplates(class_id) -> retval
  ```
  """
  @spec numTemplates(Evision.LineMod.Detector.t(), binary()) :: integer() | {:error, String.t()}
  def numTemplates(self, class_id) when is_binary(class_id)
  do
    positional = [
      class_id: Evision.Internal.Structurise.from_struct(class_id)
    ]
    :evision_nif.linemod_linemod_Detector_numTemplates(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  numTemplates

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`

  ##### Return
  - **retval**: `integer()`

  Python prototype (for reference only):
  ```python3
  numTemplates() -> retval
  ```
  """
  @spec numTemplates(Evision.LineMod.Detector.t()) :: integer() | {:error, String.t()}
  def numTemplates(self) do
    positional = [
    ]
    :evision_nif.linemod_linemod_Detector_numTemplates(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  pyramidLevels

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`

  ##### Return
  - **retval**: `integer()`

   \\brief Get number of pyramid levels used by this detector.

  Python prototype (for reference only):
  ```python3
  pyramidLevels() -> retval
  ```
  """
  @spec pyramidLevels(Evision.LineMod.Detector.t()) :: integer() | {:error, String.t()}
  def pyramidLevels(self) do
    positional = [
    ]
    :evision_nif.linemod_linemod_Detector_pyramidLevels(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  read

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`
  - **func**: `Evision.FileNode`

  Python prototype (for reference only):
  ```python3
  read(fn) -> None
  ```
  """
  @spec read(Evision.LineMod.Detector.t(), Evision.FileNode.t()) :: Evision.LineMod.Detector.t() | {:error, String.t()}
  def read(self, func) when is_struct(func, Evision.FileNode)
  do
    positional = [
      func: Evision.Internal.Structurise.from_struct(func)
    ]
    :evision_nif.linemod_linemod_Detector_read(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  readClasses

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`
  - **class_ids**: `[String]`

  ##### Keyword Arguments
  - **format**: `String`.

  Python prototype (for reference only):
  ```python3
  readClasses(class_ids[, format]) -> None
  ```
  """
  @spec readClasses(Evision.LineMod.Detector.t(), list(binary()), [{:format, term()}] | nil) :: Evision.LineMod.Detector.t() | {:error, String.t()}
  def readClasses(self, class_ids, opts) when is_list(class_ids) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:format])
    positional = [
      class_ids: Evision.Internal.Structurise.from_struct(class_ids)
    ]
    :evision_nif.linemod_linemod_Detector_readClasses(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  readClasses

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`
  - **class_ids**: `[String]`

  ##### Keyword Arguments
  - **format**: `String`.

  Python prototype (for reference only):
  ```python3
  readClasses(class_ids[, format]) -> None
  ```
  """
  @spec readClasses(Evision.LineMod.Detector.t(), list(binary())) :: Evision.LineMod.Detector.t() | {:error, String.t()}
  def readClasses(self, class_ids) when is_list(class_ids)
  do
    positional = [
      class_ids: Evision.Internal.Structurise.from_struct(class_ids)
    ]
    :evision_nif.linemod_linemod_Detector_readClasses(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  writeClasses

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`

  ##### Keyword Arguments
  - **format**: `String`.

  Python prototype (for reference only):
  ```python3
  writeClasses([, format]) -> None
  ```
  """
  @spec writeClasses(Evision.LineMod.Detector.t(), [{:format, term()}] | nil) :: Evision.LineMod.Detector.t() | {:error, String.t()}
  def writeClasses(self, opts) when opts == nil or (is_list(opts) and is_tuple(hd(opts)))
  do
    Keyword.validate!(opts || [], [:format])
    positional = [
    ]
    :evision_nif.linemod_linemod_Detector_writeClasses(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  writeClasses

  ##### Positional Arguments
  - **self**: `Evision.LineMod.Detector.t()`

  ##### Keyword Arguments
  - **format**: `String`.

  Python prototype (for reference only):
  ```python3
  writeClasses([, format]) -> None
  ```
  """
  @spec writeClasses(Evision.LineMod.Detector.t()) :: Evision.LineMod.Detector.t() | {:error, String.t()}
  def writeClasses(self) do
    positional = [
    ]
    :evision_nif.linemod_linemod_Detector_writeClasses(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
