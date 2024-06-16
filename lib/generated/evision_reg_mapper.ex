defmodule Evision.Reg.Mapper do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Reg.Mapper` struct.

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
  def to_struct({:ok, %{class: Evision.Reg.Mapper, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Reg.Mapper, ref: ref}) do
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
  calculate

  ##### Positional Arguments
  - **self**: `Evision.Reg.Mapper.t()`
  - **img1**: `Evision.Mat`
  - **img2**: `Evision.Mat`

  ##### Keyword Arguments
  - **init**: `Map`.

  ##### Return
  - **retval**: `cv::Ptr<Map>`

  Python prototype (for reference only):
  ```python3
  calculate(img1, img2[, init]) -> retval
  ```
  """
  @spec calculate(Evision.Reg.Mapper.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in(), [{:init, term()}] | nil) :: Evision.Reg.Map.t() | {:error, String.t()}
  def calculate(self, img1, img2, opts) when (is_struct(img1, Evision.Mat) or is_struct(img1, Nx.Tensor) or is_number(img1) or is_tuple(img1)) and (is_struct(img2, Evision.Mat) or is_struct(img2, Nx.Tensor) or is_number(img2) or is_tuple(img2)) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:init])
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1),
      img2: Evision.Internal.Structurise.from_struct(img2)
    ]
    :evision_nif.reg_reg_Mapper_calculate(Evision.Internal.Structurise.from_struct(self), positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  calculate

  ##### Positional Arguments
  - **self**: `Evision.Reg.Mapper.t()`
  - **img1**: `Evision.Mat`
  - **img2**: `Evision.Mat`

  ##### Keyword Arguments
  - **init**: `Map`.

  ##### Return
  - **retval**: `cv::Ptr<Map>`

  Python prototype (for reference only):
  ```python3
  calculate(img1, img2[, init]) -> retval
  ```
  """
  @spec calculate(Evision.Reg.Mapper.t(), Evision.Mat.maybe_mat_in(), Evision.Mat.maybe_mat_in()) :: Evision.Reg.Map.t() | {:error, String.t()}
  def calculate(self, img1, img2) when (is_struct(img1, Evision.Mat) or is_struct(img1, Nx.Tensor) or is_number(img1) or is_tuple(img1)) and (is_struct(img2, Evision.Mat) or is_struct(img2, Nx.Tensor) or is_number(img2) or is_tuple(img2))
  do
    positional = [
      img1: Evision.Internal.Structurise.from_struct(img1),
      img2: Evision.Internal.Structurise.from_struct(img2)
    ]
    :evision_nif.reg_reg_Mapper_calculate(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end

  @doc """
  getMap

  ##### Positional Arguments
  - **self**: `Evision.Reg.Mapper.t()`

  ##### Return
  - **retval**: `cv::Ptr<Map>`

  Python prototype (for reference only):
  ```python3
  getMap() -> retval
  ```
  """
  @spec getMap(Evision.Reg.Mapper.t()) :: Evision.Reg.Map.t() | {:error, String.t()}
  def getMap(self) do
    positional = [
    ]
    :evision_nif.reg_reg_Mapper_getMap(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
