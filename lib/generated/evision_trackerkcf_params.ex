defmodule Evision.TrackerKCF.Params do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `TrackerKCF.Params` struct.

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
  def to_struct({:ok, %{class: Evision.TrackerKCF.Params, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.TrackerKCF.Params, ref: ref}) do
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
  TrackerKCF_Params
  ##### Return
  - **self**: `Evision.TrackerKCF.Params.t()`

  Python prototype (for reference only):
  ```python3
  TrackerKCF_Params() -> <TrackerKCF_Params object>
  ```
  """
  @spec params() :: Evision.TrackerKCF.Params.t() | {:error, String.t()}
  def params() do
    positional = [
    ]
    :evision_nif.trackerKCF_Params_TrackerKCF_Params(positional)
    |> to_struct()
  end
  @spec get_compress_feature(Evision.TrackerKCF.Params.t()) :: boolean()
  def get_compress_feature(self) do
    :evision_nif.trackerKCF_Params_get_compress_feature(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_compress_feature(Evision.TrackerKCF.Params.t(), boolean()) :: Evision.TrackerKCF.Params.t()
  def set_compress_feature(self, prop) do
    :evision_nif.trackerKCF_Params_set_compress_feature(
        Evision.Internal.Structurise.from_struct(self),
        [compress_feature: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_compressed_size(Evision.TrackerKCF.Params.t()) :: integer()
  def get_compressed_size(self) do
    :evision_nif.trackerKCF_Params_get_compressed_size(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_compressed_size(Evision.TrackerKCF.Params.t(), integer()) :: Evision.TrackerKCF.Params.t()
  def set_compressed_size(self, prop) do
    :evision_nif.trackerKCF_Params_set_compressed_size(
        Evision.Internal.Structurise.from_struct(self),
        [compressed_size: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_desc_npca(Evision.TrackerKCF.Params.t()) :: integer()
  def get_desc_npca(self) do
    :evision_nif.trackerKCF_Params_get_desc_npca(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_desc_npca(Evision.TrackerKCF.Params.t(), integer()) :: Evision.TrackerKCF.Params.t()
  def set_desc_npca(self, prop) do
    :evision_nif.trackerKCF_Params_set_desc_npca(
        Evision.Internal.Structurise.from_struct(self),
        [desc_npca: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_desc_pca(Evision.TrackerKCF.Params.t()) :: integer()
  def get_desc_pca(self) do
    :evision_nif.trackerKCF_Params_get_desc_pca(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_desc_pca(Evision.TrackerKCF.Params.t(), integer()) :: Evision.TrackerKCF.Params.t()
  def set_desc_pca(self, prop) do
    :evision_nif.trackerKCF_Params_set_desc_pca(
        Evision.Internal.Structurise.from_struct(self),
        [desc_pca: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_detect_thresh(Evision.TrackerKCF.Params.t()) :: number()
  def get_detect_thresh(self) do
    :evision_nif.trackerKCF_Params_get_detect_thresh(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_detect_thresh(Evision.TrackerKCF.Params.t(), number()) :: Evision.TrackerKCF.Params.t()
  def set_detect_thresh(self, prop) do
    :evision_nif.trackerKCF_Params_set_detect_thresh(
        Evision.Internal.Structurise.from_struct(self),
        [detect_thresh: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_interp_factor(Evision.TrackerKCF.Params.t()) :: number()
  def get_interp_factor(self) do
    :evision_nif.trackerKCF_Params_get_interp_factor(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_interp_factor(Evision.TrackerKCF.Params.t(), number()) :: Evision.TrackerKCF.Params.t()
  def set_interp_factor(self, prop) do
    :evision_nif.trackerKCF_Params_set_interp_factor(
        Evision.Internal.Structurise.from_struct(self),
        [interp_factor: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_lambda(Evision.TrackerKCF.Params.t()) :: number()
  def get_lambda(self) do
    :evision_nif.trackerKCF_Params_get_lambda(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_lambda(Evision.TrackerKCF.Params.t(), number()) :: Evision.TrackerKCF.Params.t()
  def set_lambda(self, prop) do
    :evision_nif.trackerKCF_Params_set_lambda(
        Evision.Internal.Structurise.from_struct(self),
        [lambda: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_max_patch_size(Evision.TrackerKCF.Params.t()) :: integer()
  def get_max_patch_size(self) do
    :evision_nif.trackerKCF_Params_get_max_patch_size(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_max_patch_size(Evision.TrackerKCF.Params.t(), integer()) :: Evision.TrackerKCF.Params.t()
  def set_max_patch_size(self, prop) do
    :evision_nif.trackerKCF_Params_set_max_patch_size(
        Evision.Internal.Structurise.from_struct(self),
        [max_patch_size: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_output_sigma_factor(Evision.TrackerKCF.Params.t()) :: number()
  def get_output_sigma_factor(self) do
    :evision_nif.trackerKCF_Params_get_output_sigma_factor(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_output_sigma_factor(Evision.TrackerKCF.Params.t(), number()) :: Evision.TrackerKCF.Params.t()
  def set_output_sigma_factor(self, prop) do
    :evision_nif.trackerKCF_Params_set_output_sigma_factor(
        Evision.Internal.Structurise.from_struct(self),
        [output_sigma_factor: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_pca_learning_rate(Evision.TrackerKCF.Params.t()) :: number()
  def get_pca_learning_rate(self) do
    :evision_nif.trackerKCF_Params_get_pca_learning_rate(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_pca_learning_rate(Evision.TrackerKCF.Params.t(), number()) :: Evision.TrackerKCF.Params.t()
  def set_pca_learning_rate(self, prop) do
    :evision_nif.trackerKCF_Params_set_pca_learning_rate(
        Evision.Internal.Structurise.from_struct(self),
        [pca_learning_rate: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_resize(Evision.TrackerKCF.Params.t()) :: boolean()
  def get_resize(self) do
    :evision_nif.trackerKCF_Params_get_resize(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_resize(Evision.TrackerKCF.Params.t(), boolean()) :: Evision.TrackerKCF.Params.t()
  def set_resize(self, prop) do
    :evision_nif.trackerKCF_Params_set_resize(
        Evision.Internal.Structurise.from_struct(self),
        [resize: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_sigma(Evision.TrackerKCF.Params.t()) :: number()
  def get_sigma(self) do
    :evision_nif.trackerKCF_Params_get_sigma(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_sigma(Evision.TrackerKCF.Params.t(), number()) :: Evision.TrackerKCF.Params.t()
  def set_sigma(self, prop) do
    :evision_nif.trackerKCF_Params_set_sigma(
        Evision.Internal.Structurise.from_struct(self),
        [sigma: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_split_coeff(Evision.TrackerKCF.Params.t()) :: boolean()
  def get_split_coeff(self) do
    :evision_nif.trackerKCF_Params_get_split_coeff(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_split_coeff(Evision.TrackerKCF.Params.t(), boolean()) :: Evision.TrackerKCF.Params.t()
  def set_split_coeff(self, prop) do
    :evision_nif.trackerKCF_Params_set_split_coeff(
        Evision.Internal.Structurise.from_struct(self),
        [split_coeff: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_wrap_kernel(Evision.TrackerKCF.Params.t()) :: boolean()
  def get_wrap_kernel(self) do
    :evision_nif.trackerKCF_Params_get_wrap_kernel(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_wrap_kernel(Evision.TrackerKCF.Params.t(), boolean()) :: Evision.TrackerKCF.Params.t()
  def set_wrap_kernel(self, prop) do
    :evision_nif.trackerKCF_Params_set_wrap_kernel(
        Evision.Internal.Structurise.from_struct(self),
        [wrap_kernel: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
