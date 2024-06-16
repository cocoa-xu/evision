defmodule Evision.Detail.CameraParams do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Detail.CameraParams` struct.

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
  def to_struct({:ok, %{class: Evision.Detail.CameraParams, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Detail.CameraParams, ref: ref}) do
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
  K

  ##### Positional Arguments
  - **self**: `Evision.Detail.CameraParams.t()`

  ##### Return
  - **retval**: `Evision.Mat.t()`

  Python prototype (for reference only):
  ```python3
  K() -> retval
  ```
  """
  @spec k(Evision.Detail.CameraParams.t()) :: Evision.Mat.t() | {:error, String.t()}
  def k(self) do
    positional = [
    ]
    :evision_nif.detail_detail_CameraParams_K(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
  @spec get_R(Evision.Detail.CameraParams.t()) :: Evision.Mat.t()
  def get_R(self) do
    :evision_nif.detail_CameraParams_get_R(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_R(Evision.Detail.CameraParams.t(), Evision.Mat.maybe_mat_in()) :: Evision.Detail.CameraParams.t()
  def set_R(self, prop) do
    :evision_nif.detail_CameraParams_set_R(
        Evision.Internal.Structurise.from_struct(self),
        [R: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_aspect(Evision.Detail.CameraParams.t()) :: number()
  def get_aspect(self) do
    :evision_nif.detail_CameraParams_get_aspect(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_aspect(Evision.Detail.CameraParams.t(), number()) :: Evision.Detail.CameraParams.t()
  def set_aspect(self, prop) do
    :evision_nif.detail_CameraParams_set_aspect(
        Evision.Internal.Structurise.from_struct(self),
        [aspect: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_focal(Evision.Detail.CameraParams.t()) :: number()
  def get_focal(self) do
    :evision_nif.detail_CameraParams_get_focal(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_focal(Evision.Detail.CameraParams.t(), number()) :: Evision.Detail.CameraParams.t()
  def set_focal(self, prop) do
    :evision_nif.detail_CameraParams_set_focal(
        Evision.Internal.Structurise.from_struct(self),
        [focal: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_ppx(Evision.Detail.CameraParams.t()) :: number()
  def get_ppx(self) do
    :evision_nif.detail_CameraParams_get_ppx(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_ppx(Evision.Detail.CameraParams.t(), number()) :: Evision.Detail.CameraParams.t()
  def set_ppx(self, prop) do
    :evision_nif.detail_CameraParams_set_ppx(
        Evision.Internal.Structurise.from_struct(self),
        [ppx: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_ppy(Evision.Detail.CameraParams.t()) :: number()
  def get_ppy(self) do
    :evision_nif.detail_CameraParams_get_ppy(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_ppy(Evision.Detail.CameraParams.t(), number()) :: Evision.Detail.CameraParams.t()
  def set_ppy(self, prop) do
    :evision_nif.detail_CameraParams_set_ppy(
        Evision.Internal.Structurise.from_struct(self),
        [ppy: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
  @spec get_t(Evision.Detail.CameraParams.t()) :: Evision.Mat.t()
  def get_t(self) do
    :evision_nif.detail_CameraParams_get_t(Evision.Internal.Structurise.from_struct(self))
    |> to_struct()
  end
  @spec set_t(Evision.Detail.CameraParams.t(), Evision.Mat.maybe_mat_in()) :: Evision.Detail.CameraParams.t()
  def set_t(self, prop) do
    :evision_nif.detail_CameraParams_set_t(
        Evision.Internal.Structurise.from_struct(self),
        [t: Evision.Internal.Structurise.from_struct(prop)]
    )
    |> to_struct()
  end
end
