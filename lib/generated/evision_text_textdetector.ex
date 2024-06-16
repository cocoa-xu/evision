defmodule Evision.Text.TextDetector do
  import Kernel, except: [apply: 2, apply: 3]

  @typedoc """
  Type that represents an `Text.TextDetector` struct.

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
  def to_struct({:ok, %{class: Evision.Text.TextDetector, ref: ref}}) do
    {:ok, %T{ref: ref}}
  end

  @doc false
  def to_struct(%{class: Evision.Text.TextDetector, ref: ref}) do
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
  Method that provides a quick and simple interface to detect text inside an image

  ##### Positional Arguments
  - **self**: `Evision.Text.TextDetector.t()`
  - **inputImage**: `Evision.Mat`.

    an image to process

  ##### Return
  - **bbox**: `[Rect]`.

    a vector of Rect that will store the detected word bounding box

  - **confidence**: `[float]`.

    a vector of float that will be updated with the confidence the classifier has for the selected bounding box

  Python prototype (for reference only):
  ```python3
  detect(inputImage) -> Bbox, confidence
  ```
  """
  @spec detect(Evision.Text.TextDetector.t(), Evision.Mat.maybe_mat_in()) :: {list({number(), number(), number(), number()}), list(number())} | {:error, String.t()}
  def detect(self, inputImage) when (is_struct(inputImage, Evision.Mat) or is_struct(inputImage, Nx.Tensor) or is_number(inputImage) or is_tuple(inputImage))
  do
    positional = [
      inputImage: Evision.Internal.Structurise.from_struct(inputImage)
    ]
    :evision_nif.text_text_TextDetector_detect(Evision.Internal.Structurise.from_struct(self), positional)
    |> to_struct()
  end
end
