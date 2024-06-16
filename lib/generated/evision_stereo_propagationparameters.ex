defmodule Evision.Stereo.PropagationParameters do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Stereo.PropagationParameters` struct.

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
  def to_struct({:ok, %{class: Evision.Stereo.PropagationParameters, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Stereo.PropagationParameters, ref: ref}) do
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
  @spec get_borderX(Evision.Stereo.PropagationParameters.t()) :: integer()
  def get_borderX(self) do
    :evision_nif.stereo_PropagationParameters_get_borderX(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_borderX(Evision.Stereo.PropagationParameters.t(), integer()) :: Evision.Stereo.PropagationParameters.t()
  def set_borderX(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_borderX(
        Evision.Internal.Structurise.from_struct(self),
        [borderX: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_borderY(Evision.Stereo.PropagationParameters.t()) :: integer()
  def get_borderY(self) do
    :evision_nif.stereo_PropagationParameters_get_borderY(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_borderY(Evision.Stereo.PropagationParameters.t(), integer()) :: Evision.Stereo.PropagationParameters.t()
  def set_borderY(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_borderY(
        Evision.Internal.Structurise.from_struct(self),
        [borderY: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_corrWinSizeX(Evision.Stereo.PropagationParameters.t()) :: integer()
  def get_corrWinSizeX(self) do
    :evision_nif.stereo_PropagationParameters_get_corrWinSizeX(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_corrWinSizeX(Evision.Stereo.PropagationParameters.t(), integer()) :: Evision.Stereo.PropagationParameters.t()
  def set_corrWinSizeX(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_corrWinSizeX(
        Evision.Internal.Structurise.from_struct(self),
        [corrWinSizeX: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_corrWinSizeY(Evision.Stereo.PropagationParameters.t()) :: integer()
  def get_corrWinSizeY(self) do
    :evision_nif.stereo_PropagationParameters_get_corrWinSizeY(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_corrWinSizeY(Evision.Stereo.PropagationParameters.t(), integer()) :: Evision.Stereo.PropagationParameters.t()
  def set_corrWinSizeY(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_corrWinSizeY(
        Evision.Internal.Structurise.from_struct(self),
        [corrWinSizeY: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_correlationThreshold(Evision.Stereo.PropagationParameters.t()) :: number()
  def get_correlationThreshold(self) do
    :evision_nif.stereo_PropagationParameters_get_correlationThreshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_correlationThreshold(Evision.Stereo.PropagationParameters.t(), number()) :: Evision.Stereo.PropagationParameters.t()
  def set_correlationThreshold(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_correlationThreshold(
        Evision.Internal.Structurise.from_struct(self),
        [correlationThreshold: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_disparityGradient(Evision.Stereo.PropagationParameters.t()) :: integer()
  def get_disparityGradient(self) do
    :evision_nif.stereo_PropagationParameters_get_disparityGradient(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_disparityGradient(Evision.Stereo.PropagationParameters.t(), integer()) :: Evision.Stereo.PropagationParameters.t()
  def set_disparityGradient(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_disparityGradient(
        Evision.Internal.Structurise.from_struct(self),
        [disparityGradient: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_gftMaxNumFeatures(Evision.Stereo.PropagationParameters.t()) :: integer()
  def get_gftMaxNumFeatures(self) do
    :evision_nif.stereo_PropagationParameters_get_gftMaxNumFeatures(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_gftMaxNumFeatures(Evision.Stereo.PropagationParameters.t(), integer()) :: Evision.Stereo.PropagationParameters.t()
  def set_gftMaxNumFeatures(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_gftMaxNumFeatures(
        Evision.Internal.Structurise.from_struct(self),
        [gftMaxNumFeatures: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_gftMinSeperationDist(Evision.Stereo.PropagationParameters.t()) :: integer()
  def get_gftMinSeperationDist(self) do
    :evision_nif.stereo_PropagationParameters_get_gftMinSeperationDist(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_gftMinSeperationDist(Evision.Stereo.PropagationParameters.t(), integer()) :: Evision.Stereo.PropagationParameters.t()
  def set_gftMinSeperationDist(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_gftMinSeperationDist(
        Evision.Internal.Structurise.from_struct(self),
        [gftMinSeperationDist: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_gftQualityThres(Evision.Stereo.PropagationParameters.t()) :: number()
  def get_gftQualityThres(self) do
    :evision_nif.stereo_PropagationParameters_get_gftQualityThres(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_gftQualityThres(Evision.Stereo.PropagationParameters.t(), number()) :: Evision.Stereo.PropagationParameters.t()
  def set_gftQualityThres(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_gftQualityThres(
        Evision.Internal.Structurise.from_struct(self),
        [gftQualityThres: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_lkPyrLvl(Evision.Stereo.PropagationParameters.t()) :: integer()
  def get_lkPyrLvl(self) do
    :evision_nif.stereo_PropagationParameters_get_lkPyrLvl(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_lkPyrLvl(Evision.Stereo.PropagationParameters.t(), integer()) :: Evision.Stereo.PropagationParameters.t()
  def set_lkPyrLvl(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_lkPyrLvl(
        Evision.Internal.Structurise.from_struct(self),
        [lkPyrLvl: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_lkTemplateSize(Evision.Stereo.PropagationParameters.t()) :: integer()
  def get_lkTemplateSize(self) do
    :evision_nif.stereo_PropagationParameters_get_lkTemplateSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_lkTemplateSize(Evision.Stereo.PropagationParameters.t(), integer()) :: Evision.Stereo.PropagationParameters.t()
  def set_lkTemplateSize(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_lkTemplateSize(
        Evision.Internal.Structurise.from_struct(self),
        [lkTemplateSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_lkTermParam1(Evision.Stereo.PropagationParameters.t()) :: integer()
  def get_lkTermParam1(self) do
    :evision_nif.stereo_PropagationParameters_get_lkTermParam1(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_lkTermParam1(Evision.Stereo.PropagationParameters.t(), integer()) :: Evision.Stereo.PropagationParameters.t()
  def set_lkTermParam1(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_lkTermParam1(
        Evision.Internal.Structurise.from_struct(self),
        [lkTermParam1: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_lkTermParam2(Evision.Stereo.PropagationParameters.t()) :: number()
  def get_lkTermParam2(self) do
    :evision_nif.stereo_PropagationParameters_get_lkTermParam2(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_lkTermParam2(Evision.Stereo.PropagationParameters.t(), number()) :: Evision.Stereo.PropagationParameters.t()
  def set_lkTermParam2(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_lkTermParam2(
        Evision.Internal.Structurise.from_struct(self),
        [lkTermParam2: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_neighborhoodSize(Evision.Stereo.PropagationParameters.t()) :: integer()
  def get_neighborhoodSize(self) do
    :evision_nif.stereo_PropagationParameters_get_neighborhoodSize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_neighborhoodSize(Evision.Stereo.PropagationParameters.t(), integer()) :: Evision.Stereo.PropagationParameters.t()
  def set_neighborhoodSize(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_neighborhoodSize(
        Evision.Internal.Structurise.from_struct(self),
        [neighborhoodSize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_textrureThreshold(Evision.Stereo.PropagationParameters.t()) :: number()
  def get_textrureThreshold(self) do
    :evision_nif.stereo_PropagationParameters_get_textrureThreshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_textrureThreshold(Evision.Stereo.PropagationParameters.t(), number()) :: Evision.Stereo.PropagationParameters.t()
  def set_textrureThreshold(self, prop) do
    :evision_nif.stereo_PropagationParameters_set_textrureThreshold(
        Evision.Internal.Structurise.from_struct(self),
        [textrureThreshold: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
