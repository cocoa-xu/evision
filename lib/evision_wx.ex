defmodule Evision.Wx do
  @moduledoc """
  HighGui-style windows backed by Erlang's `:wx`.

  Requires Erlang/OTP built with wxWidgets. Window names are global to the node.

  ```elixir
  img = Evision.imread("photo.jpg")
  Evision.Wx.namedWindow("demo", Evision.Constant.cv_WINDOW_NORMAL())
  Evision.Wx.createTrackbar("blur", "demo", 1, 31, fn k ->
    Evision.Wx.imshow("demo", Evision.blur(img, {max(k, 1), max(k, 1)}))
  end)
  Evision.Wx.setMouseCallback("demo", fn event, x, y, _flags ->
    if event == Evision.Constant.cv_EVENT_LBUTTONDOWN(), do: IO.inspect({x, y})
  end)
  Evision.Wx.imshow("demo", img)
  Evision.Wx.waitKey()
  Evision.Wx.destroyAllWindows()
  ```

  Mouse and trackbar callbacks run in order in a process owned by the window, so
  they may call any function in this module. Unlike OpenCV, they also run while
  nobody is calling `waitKey/1`.
  """

  import Bitwise

  @compile {:no_warn_undefined, Evision.Wx.Server}

  @timeout 30_000

  @type window_rect :: {integer(), integer(), integer(), integer()}

  @doc """
  Create a window. `flags` takes `cv_WINDOW_AUTOSIZE` (default), `cv_WINDOW_NORMAL`
  and `cv_WINDOW_FREERATIO`. Does nothing if the window exists.
  """
  @spec namedWindow(String.t(), integer()) :: :ok | {:error, String.t()}
  def namedWindow(winname, flags \\ Evision.Constant.cv_WINDOW_AUTOSIZE())
      when is_binary(winname) and is_integer(flags) do
    check_wx!()

    if (flags &&& Evision.Constant.cv_WINDOW_OPENGL()) != 0 do
      {:error, "OpenGL windows are not supported"}
    else
      with {:ok, _pid} <- Evision.Wx.Server.window(winname, flags), do: :ok
    end
  end

  @doc """
  Show an image, creating an autosize window if needed. Images are converted
  for display like `cv::imshow`.
  """
  @spec imshow(String.t(), Evision.Mat.maybe_mat_in() | Nx.Tensor.t()) ::
          :ok | {:error, String.t()}
  def imshow(winname, image) when is_binary(winname) do
    check_wx!()

    with {:ok, rgb, width, height} <- Evision.Wx.Display.to_rgb(image),
         {:ok, pid} <- Evision.Wx.Server.window(winname, Evision.Constant.cv_WINDOW_AUTOSIZE()) do
      call_window(winname, pid, {:imshow, rgb, width, height})
    end
  end

  @doc """
  Wait `delay` ms (forever if `delay <= 0`) for a key press in any window.

  Returns the low 8 bits of the key code, or -1 on timeout or when no window is
  open. Keys pressed while nobody is waiting are queued.
  """
  @spec waitKey(integer()) :: integer()
  def waitKey(delay \\ 0) when is_integer(delay) do
    case waitKeyEx(delay) do
      -1 -> -1
      key -> key &&& 0xFF
    end
  end

  @doc """
  Like `waitKey/1`, but returns the full key code. Keys without a character,
  such as arrows, return wx key codes (`WXK_*`).
  """
  @spec waitKeyEx(integer()) :: integer()
  def waitKeyEx(delay \\ 0) when is_integer(delay) do
    check_wx!()
    Evision.Wx.Server.wait_key(if delay <= 0, do: :infinity, else: delay)
  end

  @doc """
  Return a queued key press without waiting, or -1.
  """
  @spec pollKey() :: integer()
  def pollKey do
    check_wx!()
    Evision.Wx.Server.wait_key(0)
  end

  @spec destroyWindow(String.t()) :: :ok
  def destroyWindow(winname) when is_binary(winname) do
    check_wx!()
    Evision.Wx.Server.destroy(winname)
  end

  @spec destroyAllWindows() :: :ok
  def destroyAllWindows do
    check_wx!()
    Evision.Wx.Server.destroy_all()
  end

  @doc """
  Call `on_mouse.(event, x, y, flags)` for mouse events inside the image, with
  `x` and `y` in image coordinates. Pass `nil` to remove the callback.
  """
  @spec setMouseCallback(String.t(), (integer(), integer(), integer(), integer() -> any()) | nil) ::
          :ok | {:error, String.t()}
  def setMouseCallback(winname, on_mouse)
      when is_binary(winname) and (is_function(on_mouse, 4) or is_nil(on_mouse)) do
    call_window(winname, {:set_mouse_callback, on_mouse})
  end

  @doc """
  The wheel delta of a `cv_EVENT_MOUSEWHEEL` event, from its flags.
  """
  @spec getMouseWheelDelta(integer()) :: integer()
  def getMouseWheelDelta(flags) when is_integer(flags) do
    Evision.Wx.Display.mouse_wheel_delta(flags)
  end

  @doc """
  Add a trackbar from 0 to `count` to a window. `on_change.(pos)` is called
  when the position changes.
  """
  @spec createTrackbar(String.t(), String.t(), integer(), integer(), (integer() -> any()) | nil) ::
          :ok | {:error, String.t()}
  def createTrackbar(trackbarname, winname, value, count, on_change \\ nil)
      when is_binary(trackbarname) and is_binary(winname) and is_integer(value) and
             is_integer(count) and count >= 0 and (is_function(on_change, 1) or is_nil(on_change)) do
    call_window(winname, {:create_trackbar, trackbarname, value, count, on_change})
  end

  @spec getTrackbarPos(String.t(), String.t()) :: integer() | {:error, String.t()}
  def getTrackbarPos(trackbarname, winname) when is_binary(trackbarname) and is_binary(winname) do
    call_window(winname, {:get_trackbar_pos, trackbarname})
  end

  @doc """
  Move a trackbar, calling its `on_change` if the position changes.
  """
  @spec setTrackbarPos(String.t(), String.t(), integer()) :: :ok | {:error, String.t()}
  def setTrackbarPos(trackbarname, winname, pos)
      when is_binary(trackbarname) and is_binary(winname) and is_integer(pos) do
    call_window(winname, {:set_trackbar_pos, trackbarname, pos})
  end

  @spec setTrackbarMin(String.t(), String.t(), integer()) :: :ok | {:error, String.t()}
  def setTrackbarMin(trackbarname, winname, minval)
      when is_binary(trackbarname) and is_binary(winname) and is_integer(minval) do
    call_window(winname, {:set_trackbar_range, trackbarname, minval, :keep})
  end

  @spec setTrackbarMax(String.t(), String.t(), integer()) :: :ok | {:error, String.t()}
  def setTrackbarMax(trackbarname, winname, maxval)
      when is_binary(trackbarname) and is_binary(winname) and is_integer(maxval) do
    call_window(winname, {:set_trackbar_range, trackbarname, :keep, maxval})
  end

  @spec moveWindow(String.t(), integer(), integer()) :: :ok | {:error, String.t()}
  def moveWindow(winname, x, y) when is_binary(winname) and is_integer(x) and is_integer(y) do
    call_window(winname, {:move, x, y})
  end

  @doc """
  Resize the image area of a `cv_WINDOW_NORMAL` window. Autosize windows are
  left unchanged.
  """
  @spec resizeWindow(String.t(), pos_integer(), pos_integer()) :: :ok | {:error, String.t()}
  def resizeWindow(winname, width, height)
      when is_binary(winname) and is_integer(width) and width > 0 and is_integer(height) and
             height > 0 do
    call_window(winname, {:resize, width, height})
  end

  @spec setWindowTitle(String.t(), String.t()) :: :ok | {:error, String.t()}
  def setWindowTitle(winname, title) when is_binary(winname) and is_binary(title) do
    call_window(winname, {:set_title, title})
  end

  @doc """
  Read a `cv_WND_PROP_*` property. Returns -1.0 for unsupported properties and
  for windows that do not exist.
  """
  @spec getWindowProperty(String.t(), integer()) :: float()
  def getWindowProperty(winname, prop_id) when is_binary(winname) and is_integer(prop_id) do
    case call_window(winname, {:get_property, prop_id}) do
      {:error, _} -> -1.0
      value -> value
    end
  end

  @doc """
  Set `cv_WND_PROP_FULLSCREEN`, `cv_WND_PROP_TOPMOST` or `cv_WND_PROP_ASPECT_RATIO`.
  """
  @spec setWindowProperty(String.t(), integer(), number()) :: :ok | {:error, String.t()}
  def setWindowProperty(winname, prop_id, prop_value)
      when is_binary(winname) and is_integer(prop_id) and is_number(prop_value) do
    call_window(winname, {:set_property, prop_id, prop_value})
  end

  @doc """
  Where the image is drawn, in screen coordinates.
  """
  @spec getWindowImageRect(String.t()) :: window_rect() | {:error, String.t()}
  def getWindowImageRect(winname) when is_binary(winname) do
    call_window(winname, :image_rect)
  end

  defp call_window(winname, request) do
    check_wx!()

    with {:ok, pid} <- Evision.Wx.Server.window(winname) do
      call_window(winname, pid, request)
    end
  end

  defp call_window(winname, pid, request) do
    GenServer.call(pid, request, @timeout)
  catch
    :exit, {reason, _} when reason in [:noproc, :normal, :shutdown] ->
      {:error, "window #{inspect(winname)} was closed"}
  end

  defp check_wx! do
    unless Code.ensure_loaded?(Evision.Wx.Server) do
      raise RuntimeError, "Evision.Wx requires Erlang/OTP built with wxWidgets"
    end
  end
end
