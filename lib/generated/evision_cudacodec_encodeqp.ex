defmodule Evision.CUDACodec.EncodeQp do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `CUDACodec.EncodeQp` struct.

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
  def to_struct({:ok, %{class: Evision.CUDACodec.EncodeQp, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.CUDACodec.EncodeQp, ref: ref}) do
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
  @spec get_qpInterB(Evision.CUDACodec.EncodeQp.t()) :: integer()
  def get_qpInterB(self) do
    :evision_nif.cudacodec_EncodeQp_get_qpInterB(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_qpInterB(Evision.CUDACodec.EncodeQp.t(), integer()) :: Evision.CUDACodec.EncodeQp.t()
  def set_qpInterB(self, prop) do
    :evision_nif.cudacodec_EncodeQp_set_qpInterB(
        Evision.Internal.Structurise.from_struct(self),
        [qpInterB: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_qpInterP(Evision.CUDACodec.EncodeQp.t()) :: integer()
  def get_qpInterP(self) do
    :evision_nif.cudacodec_EncodeQp_get_qpInterP(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_qpInterP(Evision.CUDACodec.EncodeQp.t(), integer()) :: Evision.CUDACodec.EncodeQp.t()
  def set_qpInterP(self, prop) do
    :evision_nif.cudacodec_EncodeQp_set_qpInterP(
        Evision.Internal.Structurise.from_struct(self),
        [qpInterP: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_qpIntra(Evision.CUDACodec.EncodeQp.t()) :: integer()
  def get_qpIntra(self) do
    :evision_nif.cudacodec_EncodeQp_get_qpIntra(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_qpIntra(Evision.CUDACodec.EncodeQp.t(), integer()) :: Evision.CUDACodec.EncodeQp.t()
  def set_qpIntra(self, prop) do
    :evision_nif.cudacodec_EncodeQp_set_qpIntra(
        Evision.Internal.Structurise.from_struct(self),
        [qpIntra: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
