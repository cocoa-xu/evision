defmodule Evision.MCC.CCheckerDraw do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `MCC.CCheckerDraw` struct.

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
  def to_struct({:ok, %{class: Evision.MCC.CCheckerDraw, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.MCC.CCheckerDraw, ref: ref}) do
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
  create

  ##### Positional Arguments
  - **pChecker**: `CChecker`

  ##### Keyword Arguments
  - **color**: `Evision.scalar()`.
  - **thickness**: `integer()`.

  ##### Return
  - **retval**: `CCheckerDraw`

  \\brief Create a new CCheckerDraw object.
   \\param pChecker The checker which will be drawn by this object.
   \\param color The color by with which the squares of the checker
                will be drawn
   \\param thickness The thickness with which the sqaures will be
                    drawn
   \\return A pointer to the implementation of the CCheckerDraw

  Python prototype (for reference only):
  ```python3
  create(pChecker[, color[, thickness]]) -> retval
  ```
  """
  @spec create(Evision.MCC.CChecker.t(), [{:color, term()} | {:thickness, term()}] | nil) :: Evision.MCC.CCheckerDraw.t() | {:error, String.t()}
  def create(pChecker, opts) when is_struct(pChecker, Evision.MCC.CChecker) and (opts == nil or (is_list(opts) and is_tuple(hd(opts))))
  do
    Keyword.validate!(opts || [], [:color, :thickness])
    positional = [
      pChecker: Evision.Internal.Structurise.from_struct(pChecker)
    ]
    :evision_nif.mcc_mcc_CCheckerDraw_create_static(positional ++ Evision.Internal.Structurise.from_struct(opts || []))
     |> to_struct()
  end

  @doc """
  create

  ##### Positional Arguments
  - **pChecker**: `CChecker`

  ##### Keyword Arguments
  - **color**: `Evision.scalar()`.
  - **thickness**: `integer()`.

  ##### Return
  - **retval**: `CCheckerDraw`

  \\brief Create a new CCheckerDraw object.
   \\param pChecker The checker which will be drawn by this object.
   \\param color The color by with which the squares of the checker
                will be drawn
   \\param thickness The thickness with which the sqaures will be
                    drawn
   \\return A pointer to the implementation of the CCheckerDraw

  Python prototype (for reference only):
  ```python3
  create(pChecker[, color[, thickness]]) -> retval
  ```
  """
  @spec create(Evision.MCC.CChecker.t()) :: Evision.MCC.CCheckerDraw.t() | {:error, String.t()}
  def create(pChecker) when is_struct(pChecker, Evision.MCC.CChecker)
  do
    positional = [
      pChecker: Evision.Internal.Structurise.from_struct(pChecker)
    ]
    :evision_nif.mcc_mcc_CCheckerDraw_create_static(positional)
    |> to_struct()
  end

  @doc """
  draw

  ##### Positional Arguments
  - **self**: `Evision.MCC.CCheckerDraw.t()`

  ##### Return
  - **img**: `Evision.Mat.t()`

  \\brief Draws the checker to the given image.
   \\param img image in color space BGR

  Python prototype (for reference only):
  ```python3
  draw(img) -> img
  ```
  """
  @spec draw(Evision.MCC.CCheckerDraw.t(), Evision.Mat.maybe_mat_in()) :: Evision.Mat.t() | {:error, String.t()}
  def draw(self, img) when (is_struct(img, Evision.Mat) or is_struct(img, Nx.Tensor) or is_number(img) or is_tuple(img))
  do
    positional = [
      img: Evision.Internal.Structurise.from_struct(img)
    ]
    :evision_nif.mcc_mcc_CCheckerDraw_draw(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
