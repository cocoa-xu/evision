defmodule Evision.Stereo do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Stereo` struct.

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
  def to_struct({:ok, %{class: Evision.Stereo, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Stereo, ref: ref}) do
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
  @type enum :: integer()
  @doc enum: true
  def cv_CV_SPECKLE_REMOVAL_ALGORITHM, do: 0
  @doc enum: true
  def cv_CV_SPECKLE_REMOVAL_AVG_ALGORITHM, do: 1
  @doc enum: true
  def cv_CV_QUADRATIC_INTERPOLATION, do: 0
  @doc enum: true
  def cv_CV_SIMETRICV_INTERPOLATION, do: 1
  @doc enum: true
  def cv_CV_DENSE_CENSUS, do: 0
  @doc enum: true
  def cv_CV_SPARSE_CENSUS, do: 1
  @doc enum: true
  def cv_CV_CS_CENSUS, do: 2
  @doc enum: true
  def cv_CV_MODIFIED_CS_CENSUS, do: 3
  @doc enum: true
  def cv_CV_MODIFIED_CENSUS_TRANSFORM, do: 4
  @doc enum: true
  def cv_CV_MEAN_VARIATION, do: 5
  @doc enum: true
  def cv_CV_STAR_KERNEL, do: 6

end
