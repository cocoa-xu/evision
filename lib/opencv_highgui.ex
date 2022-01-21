defmodule OpenCV.Gui do
  @moduledoc """
  OpenCV High-level Graphical User Interface
  """

  @doc """
  Show a mat in a named window

  - **winname**. The name of the window.
  - **mat**. The image.

  ## Example
  ```elixir
  {:ok, mat} = OpenCV.imread("example.jpg")
  OpenCV.imshow("OpenCV", mat)
  # the following line may be necessary on your system
  # will try to improve this later
  OpenCV.waitkey(0)
  ```
  """
  @doc namespace: :"cv.highgui"
  def imshow(winname, mat) when is_binary(winname) do
    :erl_cv_nif.evision_cv_imshow(winname: winname, mat: mat)
  end

  @doc """
  wait for user keyboard event for a `delay` amount of time (ms)
  """
  @doc namespace: :"cv.highgui"
  def waitkey(delay) when is_integer(delay) do
    :erl_cv_nif.evision_cv_waitKey(delay: delay)
  end

  @doc """
  close a named window
  """
  @doc namespace: :"cv.highgui"
  def destroy_window(winname) when is_binary(winname) do
    :erl_cv_nif.evision_cv_destroyWindow(winname: winname)
  end

  @doc """
  close all windows
  """
  @doc namespace: :"cv.highgui"
  def destroy_all_windows do
    :erl_cv_nif.evision_cv_destroyAllWindows()
  end
end
