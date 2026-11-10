defmodule Evision.HighGui do
  @moduledoc """
  High-level Graphical User Interface
  """

  @doc """
  Converts an image to 8-bit RGB, scaling it the way `imshow/2` displays it

  This mirrors `convertToShow` in OpenCV's highgui:

  - `{:s, 8}`: `x + 127`
  - `{:s, 16}`: `x / 255 + 127`
  - `{:u, 16}`: `x / 255`
  - `{:f, _}` and `{:bf, 16}`: `x * 255`, for values in `[0, 1]`

  The image is first passed through `Evision.Mat.to_2d_image/1`, then `Evision.Mat.to_rgb/1`.
  """
  @spec convertToShow(Evision.Mat.t() | Nx.Tensor.t()) :: Evision.Mat.maybe_mat_out()
  def convertToShow(image) do
    with %Evision.Mat{type: type} = mat <- Evision.Mat.to_2d_image(image),
         %Evision.Mat{} = u8 <- to_u8(mat, type) do
      Evision.Mat.to_rgb(u8)
    end
  end

  defp to_u8(mat, {:u, 8}), do: mat
  defp to_u8(mat, {:s, 8}), do: Evision.convertScaleAbs(mat, alpha: 1, beta: 127)
  defp to_u8(mat, {:s, 16}), do: Evision.convertScaleAbs(mat, alpha: 1 / 255, beta: 127)
  defp to_u8(mat, {:u, 16}), do: Evision.convertScaleAbs(mat, alpha: 1 / 255)

  defp to_u8(mat, {:f, bits}) when bits in [32, 64] do
    # convertTo(CV_8U, 255). Evision.multiply with a scalar would scale only the first channel.
    Evision.addWeighted(mat, 255.0, mat, 0.0, 0.0, dtype: Evision.Constant.cv_8U())
  end

  defp to_u8(mat, {:f, 16}), do: mat |> Evision.Mat.as_type({:f, 32}) |> to_u8({:f, 32})
  defp to_u8(mat, {:bf, 16}), do: mat |> Evision.Mat.as_type({:f, 32}) |> to_u8({:f, 32})
  defp to_u8(_mat, type), do: {:error, "cannot show an image of type #{inspect(type)}"}

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
