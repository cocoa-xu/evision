defmodule Evision.Gui do
  @moduledoc """
  OpenCV High-level Graphical User Interface
  """

  import Evision.Errorize

  @doc """
  Show a mat in a named window

  - **winname**. The name of the window.
  - **mat**. The image.

  ## Example
  ```elixir
  {:ok, mat} = Evision.imread("example.jpg")
  Evision.imshow("OpenCV", mat)
  # the following line may be necessary on your system
  # will try to improve this later
  Evision.waitkey(0)
  ```
  """
  @doc namespace: :"cv.highgui"
  def imshow(winname, mat) when is_binary(winname) do
    :erl_cv_nif.evision_cv_imshow(winname: winname, mat: mat)
  end

  deferror(imshow(winname, mat))

  @doc """
  wait for user keyboard event for a `delay` amount of time (ms)
  """
  @doc namespace: :"cv.highgui"
  def waitkey(delay) when is_integer(delay) do
    :erl_cv_nif.evision_cv_waitKey(delay: delay)
  end

  deferror(waitkey(delay))

  @doc """
  close a named window
  """
  @doc namespace: :"cv.highgui"
  def destroyWindow(winname) when is_binary(winname) do
    :erl_cv_nif.evision_cv_destroyWindow(winname: winname)
  end

  deferror(destroyWindow(winname))

  @doc """
  close all windows
  """
  @doc namespace: :"cv.highgui"
  def destroyAllWindows do
    :erl_cv_nif.evision_cv_destroyAllWindows()
  end

  deferror(destroyAllWindows())
end
