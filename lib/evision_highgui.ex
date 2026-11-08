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

  @doc """
  Set the mouse callback of a window, or remove it with `nil`

  `on_mouse` receives `event, x, y, flags`, see `cv_EVENT_*` and `cv_EVENT_FLAG_*` in `Evision.Constant`.

  OpenCV delivers events only while `waitKey/1` is waiting. Callbacks run one at a time in a separate process.
  """
  @doc namespace: :"cv.highgui"
  @spec setMouseCallback(String.t(), (integer(), integer(), integer(), integer() -> any()) | nil) ::
          :ok | {:error, String.t()}
  def setMouseCallback(winname, on_mouse)
      when is_binary(winname) and (is_function(on_mouse, 4) or is_nil(on_mouse)) do
    with_runner(fn -> :evision_nif.setMouseCallback(winname: winname, on_mouse: on_mouse) end)
  end

  @doc """
  Create a trackbar in a window

  `on_change` receives the new position. It runs like the callback of `setMouseCallback/2`.
  """
  @doc namespace: :"cv.highgui"
  @spec createTrackbar(String.t(), String.t(), integer(), integer(), (integer() -> any()) | nil) ::
          :ok | {:error, String.t()}
  def createTrackbar(trackbarname, winname, value, count, on_change \\ nil)
      when is_binary(trackbarname) and is_binary(winname) and is_integer(value) and
             is_integer(count) and (is_function(on_change, 1) or is_nil(on_change)) do
    with_runner(fn ->
      :evision_nif.createTrackbar(
        trackbarname: trackbarname,
        winname: winname,
        value: value,
        count: count,
        on_change: on_change
      )
    end)
  end

  defp with_runner(fun) do
    if Evision.HighGui.Runner.running?() do
      fun.()
    else
      {:error, "Evision.HighGui.Runner is not running, start the :evision application first"}
    end
  end
end
