defmodule Evision.Wx.Display do
  @moduledoc false

  import Bitwise

  alias Evision.Constant

  def mouse_event_types, do: Map.keys(mouse_events())

  def to_rgb(%struct{} = image) when struct in [Evision.Mat, Nx.Tensor] do
    with %Evision.Mat{shape: {height, width, _}} = rgb <- Evision.HighGui.convertToShow(image),
         binary when is_binary(binary) <- Evision.Mat.to_binary(rgb) do
      {:ok, binary, width, height}
    end
  end

  def to_rgb(other) do
    {:error, "expected an Evision.Mat or Nx.Tensor, got: #{inspect(other)}"}
  end

  def fit_rect({image_w, image_h}, {canvas_w, canvas_h}, keep_ratio?) do
    {w, h} =
      if keep_ratio? do
        scale = min(canvas_w / image_w, canvas_h / image_h)
        {max(round(image_w * scale), 1), max(round(image_h * scale), 1)}
      else
        {max(canvas_w, 1), max(canvas_h, 1)}
      end

    {div(canvas_w - w, 2), div(canvas_h - h, 2), w, h}
  end

  def to_image_point({x, y}, {rect_x, rect_y, rect_w, rect_h}, {image_w, image_h}) do
    image_x = floor((x - rect_x) * image_w / rect_w)
    image_y = floor((y - rect_y) * image_h / rect_h)

    if image_x in 0..(image_w - 1)//1 and image_y in 0..(image_h - 1)//1 do
      {image_x, image_y}
    end
  end

  def mouse_event(type), do: Map.get(mouse_events(), type)

  defp mouse_events do
    %{
      motion: Constant.cv_EVENT_MOUSEMOVE(),
      left_down: Constant.cv_EVENT_LBUTTONDOWN(),
      right_down: Constant.cv_EVENT_RBUTTONDOWN(),
      middle_down: Constant.cv_EVENT_MBUTTONDOWN(),
      left_up: Constant.cv_EVENT_LBUTTONUP(),
      right_up: Constant.cv_EVENT_RBUTTONUP(),
      middle_up: Constant.cv_EVENT_MBUTTONUP(),
      left_dclick: Constant.cv_EVENT_LBUTTONDBLCLK(),
      right_dclick: Constant.cv_EVENT_RBUTTONDBLCLK(),
      middle_dclick: Constant.cv_EVENT_MBUTTONDBLCLK(),
      mousewheel: Constant.cv_EVENT_MOUSEWHEEL()
    }
  end

  def mouse_flags(event) do
    flags =
      flag(event.leftDown, Constant.cv_EVENT_FLAG_LBUTTON()) |||
        flag(event.rightDown, Constant.cv_EVENT_FLAG_RBUTTON()) |||
        flag(event.middleDown, Constant.cv_EVENT_FLAG_MBUTTON()) |||
        flag(event.controlDown, Constant.cv_EVENT_FLAG_CTRLKEY()) |||
        flag(event.shiftDown, Constant.cv_EVENT_FLAG_SHIFTKEY()) |||
        flag(event.altDown, Constant.cv_EVENT_FLAG_ALTKEY())

    if event.type == :mousewheel do
      flags ||| event.wheelRotation <<< 16
    else
      flags
    end
  end

  defp flag(true, bit), do: bit
  defp flag(_, _bit), do: 0

  def key_code(_key_code, uni_char) when is_integer(uni_char) and uni_char > 0, do: uni_char
  def key_code(key_code, _uni_char), do: key_code

  def mouse_wheel_delta(flags) do
    delta = flags >>> 16 &&& 0xFFFF
    if delta >= 0x8000, do: delta - 0x10000, else: delta
  end
end
