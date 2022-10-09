defmodule Evision.HighGui do
  @moduledoc """
  OpenCV High-level Graphical User Interface
  """

  import Evision.Errorize

  @doc """
  Show a mat in a named window

  #### Positional Arguments

  - **winname**. `String`

    The name of the window.

  - **mat**. `Evision.Mat`

    The image.

  #### Example

  ```elixir
  iex> mat = Evision.imread!("example.jpg")
  iex> Evision.imshow("OpenCV", mat)
  # the following line may be necessary on your system
  # will try to improve this later
  iex> Evision.waitkey(0)
  ```

  """
  @doc namespace: :"cv.highgui"
  @spec imshow(String.t(), reference() | Evision.Mat.t() | Nx.Tensor.t()) :: :ok | {:error, String.t()}
  def imshow(winname, mat) when is_binary(winname) and (is_reference(mat) or is_struct(mat))do
    mat = Evision.Internal.Structurise.from_struct(mat)
    :evision_nif.imshow(winname: winname, mat: mat)
  end

  deferror(imshow(winname, mat))

  @doc """
  Wait for user keyboard event for a `delay` amount of time (ms)

  #### Positional Arguments

  - **delay**. `int`

    Wait for `delay` ms.

  """
  @doc namespace: :"cv.highgui"
  @spec waitkey(integer()) :: :ok | {:error, String.t()}
  def waitkey(delay) when is_integer(delay) do
    :evision_nif.waitKey(delay: delay)
  end

  deferror(waitkey(delay))

  @doc """
  Close a named window

  #### Positional Arguments

  - **winname**. `String`

    The name of the window.

  """
  @doc namespace: :"cv.highgui"
  @spec destroyWindow(String.t()) :: :ok | {:error, String.t()}
  def destroyWindow(winname) when is_binary(winname) do
    :evision_nif.destroyWindow(winname: winname)
  end

  deferror(destroyWindow(winname))

  @doc """
  Close all windows
  """
  @doc namespace: :"cv.highgui"
  @spec destroyAllWindows() :: :ok
  def destroyAllWindows do
    :evision_nif.destroyAllWindows()
  end

  deferror(destroyAllWindows())
end
