defmodule Evision.HighGui do
  @moduledoc """
  High-level Graphical User Interface
  """

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
  @spec imshow(String.t(), Evision.Mat.maybe_mat_in() | Nx.Tensor.t()) ::
          :ok | {:error, String.t()}
  def imshow(winname, mat) when is_binary(winname) and (is_reference(mat) or is_struct(mat)) do
    mat = Evision.Internal.Structurise.from_struct(mat)
    :evision_nif.imshow(winname: winname, mat: mat)
  end

  @doc """
  Wait for user keyboard event for a `delay` amount of time (ms)

  #### Positional Arguments

  - **delay**. `int`

    Wait for `delay` ms.

  """
  @doc namespace: :"cv.highgui"
  @spec waitKey(integer()) :: :ok | {:error, String.t()}
  def waitKey(delay) when is_integer(delay) do
    :evision_nif.waitKey(delay: delay)
  end

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

  @doc """
  Close all windows
  """
  @doc namespace: :"cv.highgui"
  @spec destroyAllWindows() :: :ok
  def destroyAllWindows do
    :evision_nif.destroyAllWindows()
  end
end
