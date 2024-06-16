defmodule Evision.LineDescriptor.LSDParam do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `LineDescriptor.LSDParam` struct.

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
  def to_struct({:ok, %{class: Evision.LineDescriptor.LSDParam, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.LineDescriptor.LSDParam, ref: ref}) do
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
  LSDParam
  ##### Return
  - **self**: `Evision.LineDescriptor.LSDParam.t()`

  Python prototype (for reference only):
  ```python3
  LSDParam() -> <line_descriptor_LSDParam object>
  ```
  """
  @spec lSDParam() :: Evision.LineDescriptor.LSDParam.t() | {:error, String.t()}
  def lSDParam() do
    positional = [
    ]
    :evision_nif.line_descriptor_line_descriptor_LSDParam_LSDParam(positional)
    |> to_struct()
  end
  @spec get_ang_th(Evision.LineDescriptor.LSDParam.t()) :: number()
  def get_ang_th(self) do
    :evision_nif.line_descriptor_LSDParam_get_ang_th(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_ang_th(Evision.LineDescriptor.LSDParam.t(), number()) :: Evision.LineDescriptor.LSDParam.t()
  def set_ang_th(self, prop) do
    :evision_nif.line_descriptor_LSDParam_set_ang_th(
        Evision.Internal.Structurise.from_struct(self),
        [ang_th: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_density_th(Evision.LineDescriptor.LSDParam.t()) :: number()
  def get_density_th(self) do
    :evision_nif.line_descriptor_LSDParam_get_density_th(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_density_th(Evision.LineDescriptor.LSDParam.t(), number()) :: Evision.LineDescriptor.LSDParam.t()
  def set_density_th(self, prop) do
    :evision_nif.line_descriptor_LSDParam_set_density_th(
        Evision.Internal.Structurise.from_struct(self),
        [density_th: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_log_eps(Evision.LineDescriptor.LSDParam.t()) :: number()
  def get_log_eps(self) do
    :evision_nif.line_descriptor_LSDParam_get_log_eps(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_log_eps(Evision.LineDescriptor.LSDParam.t(), number()) :: Evision.LineDescriptor.LSDParam.t()
  def set_log_eps(self, prop) do
    :evision_nif.line_descriptor_LSDParam_set_log_eps(
        Evision.Internal.Structurise.from_struct(self),
        [log_eps: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_n_bins(Evision.LineDescriptor.LSDParam.t()) :: integer()
  def get_n_bins(self) do
    :evision_nif.line_descriptor_LSDParam_get_n_bins(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_n_bins(Evision.LineDescriptor.LSDParam.t(), integer()) :: Evision.LineDescriptor.LSDParam.t()
  def set_n_bins(self, prop) do
    :evision_nif.line_descriptor_LSDParam_set_n_bins(
        Evision.Internal.Structurise.from_struct(self),
        [n_bins: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_quant(Evision.LineDescriptor.LSDParam.t()) :: number()
  def get_quant(self) do
    :evision_nif.line_descriptor_LSDParam_get_quant(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_quant(Evision.LineDescriptor.LSDParam.t(), number()) :: Evision.LineDescriptor.LSDParam.t()
  def set_quant(self, prop) do
    :evision_nif.line_descriptor_LSDParam_set_quant(
        Evision.Internal.Structurise.from_struct(self),
        [quant: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_scale(Evision.LineDescriptor.LSDParam.t()) :: number()
  def get_scale(self) do
    :evision_nif.line_descriptor_LSDParam_get_scale(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_scale(Evision.LineDescriptor.LSDParam.t(), number()) :: Evision.LineDescriptor.LSDParam.t()
  def set_scale(self, prop) do
    :evision_nif.line_descriptor_LSDParam_set_scale(
        Evision.Internal.Structurise.from_struct(self),
        [scale: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_sigma_scale(Evision.LineDescriptor.LSDParam.t()) :: number()
  def get_sigma_scale(self) do
    :evision_nif.line_descriptor_LSDParam_get_sigma_scale(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_sigma_scale(Evision.LineDescriptor.LSDParam.t(), number()) :: Evision.LineDescriptor.LSDParam.t()
  def set_sigma_scale(self, prop) do
    :evision_nif.line_descriptor_LSDParam_set_sigma_scale(
        Evision.Internal.Structurise.from_struct(self),
        [sigma_scale: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
