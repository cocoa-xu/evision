defmodule Evision.TrackerCSRT.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `TrackerCSRT.Params` struct.

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
  def to_struct({:ok, %{class: Evision.TrackerCSRT.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.TrackerCSRT.Params, ref: ref}) do
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
  TrackerCSRT_Params
  ##### Return
  - **self**: `Evision.TrackerCSRT.Params.t()`

  Python prototype (for reference only):
  ```python3
  TrackerCSRT_Params() -> <TrackerCSRT_Params object>
  ```
  """
  @spec params() :: Evision.TrackerCSRT.Params.t() | {:error, String.t()}
  def params() do
    positional = [
    ]
    :evision_nif.trackerCSRT_Params_TrackerCSRT_Params(positional)
    |> to_struct()
  end
  @spec get_admm_iterations(Evision.TrackerCSRT.Params.t()) :: integer()
  def get_admm_iterations(self) do
    :evision_nif.trackerCSRT_Params_get_admm_iterations(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_admm_iterations(Evision.TrackerCSRT.Params.t(), integer()) :: Evision.TrackerCSRT.Params.t()
  def set_admm_iterations(self, prop) do
    :evision_nif.trackerCSRT_Params_set_admm_iterations(
        Evision.Internal.Structurise.from_struct(self),
        [admm_iterations: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_background_ratio(Evision.TrackerCSRT.Params.t()) :: integer()
  def get_background_ratio(self) do
    :evision_nif.trackerCSRT_Params_get_background_ratio(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_background_ratio(Evision.TrackerCSRT.Params.t(), integer()) :: Evision.TrackerCSRT.Params.t()
  def set_background_ratio(self, prop) do
    :evision_nif.trackerCSRT_Params_set_background_ratio(
        Evision.Internal.Structurise.from_struct(self),
        [background_ratio: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_cheb_attenuation(Evision.TrackerCSRT.Params.t()) :: number()
  def get_cheb_attenuation(self) do
    :evision_nif.trackerCSRT_Params_get_cheb_attenuation(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_cheb_attenuation(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_cheb_attenuation(self, prop) do
    :evision_nif.trackerCSRT_Params_set_cheb_attenuation(
        Evision.Internal.Structurise.from_struct(self),
        [cheb_attenuation: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_filter_lr(Evision.TrackerCSRT.Params.t()) :: number()
  def get_filter_lr(self) do
    :evision_nif.trackerCSRT_Params_get_filter_lr(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_filter_lr(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_filter_lr(self, prop) do
    :evision_nif.trackerCSRT_Params_set_filter_lr(
        Evision.Internal.Structurise.from_struct(self),
        [filter_lr: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_gsl_sigma(Evision.TrackerCSRT.Params.t()) :: number()
  def get_gsl_sigma(self) do
    :evision_nif.trackerCSRT_Params_get_gsl_sigma(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_gsl_sigma(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_gsl_sigma(self, prop) do
    :evision_nif.trackerCSRT_Params_set_gsl_sigma(
        Evision.Internal.Structurise.from_struct(self),
        [gsl_sigma: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_histogram_bins(Evision.TrackerCSRT.Params.t()) :: integer()
  def get_histogram_bins(self) do
    :evision_nif.trackerCSRT_Params_get_histogram_bins(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_histogram_bins(Evision.TrackerCSRT.Params.t(), integer()) :: Evision.TrackerCSRT.Params.t()
  def set_histogram_bins(self, prop) do
    :evision_nif.trackerCSRT_Params_set_histogram_bins(
        Evision.Internal.Structurise.from_struct(self),
        [histogram_bins: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_histogram_lr(Evision.TrackerCSRT.Params.t()) :: number()
  def get_histogram_lr(self) do
    :evision_nif.trackerCSRT_Params_get_histogram_lr(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_histogram_lr(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_histogram_lr(self, prop) do
    :evision_nif.trackerCSRT_Params_set_histogram_lr(
        Evision.Internal.Structurise.from_struct(self),
        [histogram_lr: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_hog_clip(Evision.TrackerCSRT.Params.t()) :: number()
  def get_hog_clip(self) do
    :evision_nif.trackerCSRT_Params_get_hog_clip(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_hog_clip(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_hog_clip(self, prop) do
    :evision_nif.trackerCSRT_Params_set_hog_clip(
        Evision.Internal.Structurise.from_struct(self),
        [hog_clip: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_hog_orientations(Evision.TrackerCSRT.Params.t()) :: number()
  def get_hog_orientations(self) do
    :evision_nif.trackerCSRT_Params_get_hog_orientations(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_hog_orientations(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_hog_orientations(self, prop) do
    :evision_nif.trackerCSRT_Params_set_hog_orientations(
        Evision.Internal.Structurise.from_struct(self),
        [hog_orientations: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_kaiser_alpha(Evision.TrackerCSRT.Params.t()) :: number()
  def get_kaiser_alpha(self) do
    :evision_nif.trackerCSRT_Params_get_kaiser_alpha(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_kaiser_alpha(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_kaiser_alpha(self, prop) do
    :evision_nif.trackerCSRT_Params_set_kaiser_alpha(
        Evision.Internal.Structurise.from_struct(self),
        [kaiser_alpha: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_num_hog_channels_used(Evision.TrackerCSRT.Params.t()) :: integer()
  def get_num_hog_channels_used(self) do
    :evision_nif.trackerCSRT_Params_get_num_hog_channels_used(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_num_hog_channels_used(Evision.TrackerCSRT.Params.t(), integer()) :: Evision.TrackerCSRT.Params.t()
  def set_num_hog_channels_used(self, prop) do
    :evision_nif.trackerCSRT_Params_set_num_hog_channels_used(
        Evision.Internal.Structurise.from_struct(self),
        [num_hog_channels_used: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_number_of_scales(Evision.TrackerCSRT.Params.t()) :: integer()
  def get_number_of_scales(self) do
    :evision_nif.trackerCSRT_Params_get_number_of_scales(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_number_of_scales(Evision.TrackerCSRT.Params.t(), integer()) :: Evision.TrackerCSRT.Params.t()
  def set_number_of_scales(self, prop) do
    :evision_nif.trackerCSRT_Params_set_number_of_scales(
        Evision.Internal.Structurise.from_struct(self),
        [number_of_scales: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_padding(Evision.TrackerCSRT.Params.t()) :: number()
  def get_padding(self) do
    :evision_nif.trackerCSRT_Params_get_padding(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_padding(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_padding(self, prop) do
    :evision_nif.trackerCSRT_Params_set_padding(
        Evision.Internal.Structurise.from_struct(self),
        [padding: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_psr_threshold(Evision.TrackerCSRT.Params.t()) :: number()
  def get_psr_threshold(self) do
    :evision_nif.trackerCSRT_Params_get_psr_threshold(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_psr_threshold(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_psr_threshold(self, prop) do
    :evision_nif.trackerCSRT_Params_set_psr_threshold(
        Evision.Internal.Structurise.from_struct(self),
        [psr_threshold: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_scale_lr(Evision.TrackerCSRT.Params.t()) :: number()
  def get_scale_lr(self) do
    :evision_nif.trackerCSRT_Params_get_scale_lr(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_scale_lr(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_scale_lr(self, prop) do
    :evision_nif.trackerCSRT_Params_set_scale_lr(
        Evision.Internal.Structurise.from_struct(self),
        [scale_lr: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_scale_model_max_area(Evision.TrackerCSRT.Params.t()) :: number()
  def get_scale_model_max_area(self) do
    :evision_nif.trackerCSRT_Params_get_scale_model_max_area(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_scale_model_max_area(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_scale_model_max_area(self, prop) do
    :evision_nif.trackerCSRT_Params_set_scale_model_max_area(
        Evision.Internal.Structurise.from_struct(self),
        [scale_model_max_area: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_scale_sigma_factor(Evision.TrackerCSRT.Params.t()) :: number()
  def get_scale_sigma_factor(self) do
    :evision_nif.trackerCSRT_Params_get_scale_sigma_factor(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_scale_sigma_factor(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_scale_sigma_factor(self, prop) do
    :evision_nif.trackerCSRT_Params_set_scale_sigma_factor(
        Evision.Internal.Structurise.from_struct(self),
        [scale_sigma_factor: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_scale_step(Evision.TrackerCSRT.Params.t()) :: number()
  def get_scale_step(self) do
    :evision_nif.trackerCSRT_Params_get_scale_step(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_scale_step(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_scale_step(self, prop) do
    :evision_nif.trackerCSRT_Params_set_scale_step(
        Evision.Internal.Structurise.from_struct(self),
        [scale_step: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_template_size(Evision.TrackerCSRT.Params.t()) :: number()
  def get_template_size(self) do
    :evision_nif.trackerCSRT_Params_get_template_size(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_template_size(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_template_size(self, prop) do
    :evision_nif.trackerCSRT_Params_set_template_size(
        Evision.Internal.Structurise.from_struct(self),
        [template_size: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_use_channel_weights(Evision.TrackerCSRT.Params.t()) :: boolean()
  def get_use_channel_weights(self) do
    :evision_nif.trackerCSRT_Params_get_use_channel_weights(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_use_channel_weights(Evision.TrackerCSRT.Params.t(), boolean()) :: Evision.TrackerCSRT.Params.t()
  def set_use_channel_weights(self, prop) do
    :evision_nif.trackerCSRT_Params_set_use_channel_weights(
        Evision.Internal.Structurise.from_struct(self),
        [use_channel_weights: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_use_color_names(Evision.TrackerCSRT.Params.t()) :: boolean()
  def get_use_color_names(self) do
    :evision_nif.trackerCSRT_Params_get_use_color_names(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_use_color_names(Evision.TrackerCSRT.Params.t(), boolean()) :: Evision.TrackerCSRT.Params.t()
  def set_use_color_names(self, prop) do
    :evision_nif.trackerCSRT_Params_set_use_color_names(
        Evision.Internal.Structurise.from_struct(self),
        [use_color_names: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_use_gray(Evision.TrackerCSRT.Params.t()) :: boolean()
  def get_use_gray(self) do
    :evision_nif.trackerCSRT_Params_get_use_gray(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_use_gray(Evision.TrackerCSRT.Params.t(), boolean()) :: Evision.TrackerCSRT.Params.t()
  def set_use_gray(self, prop) do
    :evision_nif.trackerCSRT_Params_set_use_gray(
        Evision.Internal.Structurise.from_struct(self),
        [use_gray: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_use_hog(Evision.TrackerCSRT.Params.t()) :: boolean()
  def get_use_hog(self) do
    :evision_nif.trackerCSRT_Params_get_use_hog(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_use_hog(Evision.TrackerCSRT.Params.t(), boolean()) :: Evision.TrackerCSRT.Params.t()
  def set_use_hog(self, prop) do
    :evision_nif.trackerCSRT_Params_set_use_hog(
        Evision.Internal.Structurise.from_struct(self),
        [use_hog: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_use_rgb(Evision.TrackerCSRT.Params.t()) :: boolean()
  def get_use_rgb(self) do
    :evision_nif.trackerCSRT_Params_get_use_rgb(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_use_rgb(Evision.TrackerCSRT.Params.t(), boolean()) :: Evision.TrackerCSRT.Params.t()
  def set_use_rgb(self, prop) do
    :evision_nif.trackerCSRT_Params_set_use_rgb(
        Evision.Internal.Structurise.from_struct(self),
        [use_rgb: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_use_segmentation(Evision.TrackerCSRT.Params.t()) :: boolean()
  def get_use_segmentation(self) do
    :evision_nif.trackerCSRT_Params_get_use_segmentation(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_use_segmentation(Evision.TrackerCSRT.Params.t(), boolean()) :: Evision.TrackerCSRT.Params.t()
  def set_use_segmentation(self, prop) do
    :evision_nif.trackerCSRT_Params_set_use_segmentation(
        Evision.Internal.Structurise.from_struct(self),
        [use_segmentation: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_weights_lr(Evision.TrackerCSRT.Params.t()) :: number()
  def get_weights_lr(self) do
    :evision_nif.trackerCSRT_Params_get_weights_lr(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_weights_lr(Evision.TrackerCSRT.Params.t(), number()) :: Evision.TrackerCSRT.Params.t()
  def set_weights_lr(self, prop) do
    :evision_nif.trackerCSRT_Params_set_weights_lr(
        Evision.Internal.Structurise.from_struct(self),
        [weights_lr: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_window_function(Evision.TrackerCSRT.Params.t()) :: binary()
  def get_window_function(self) do
    :evision_nif.trackerCSRT_Params_get_window_function(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_window_function(Evision.TrackerCSRT.Params.t(), binary()) :: Evision.TrackerCSRT.Params.t()
  def set_window_function(self, prop) do
    :evision_nif.trackerCSRT_Params_set_window_function(
        Evision.Internal.Structurise.from_struct(self),
        [window_function: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
