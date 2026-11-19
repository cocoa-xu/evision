defmodule Evision.Wx.Display.Test do
  use ExUnit.Case, async: true

  import Bitwise
  alias Evision.Wx.Display

  defp mat(binary, type, {h, w, c}), do: Evision.Mat.from_binary(binary, type, h, w, c)

  describe "to_rgb/1" do
    test "returns the RGB bytes with width and height" do
      bgr = mat(<<1, 2, 3, 4, 5, 6>>, {:u, 8}, {1, 2, 3})
      assert {:ok, <<3, 2, 1, 6, 5, 4>>, 2, 1} = Display.to_rgb(bgr)

      f32 = mat(<<0.5::float-32-native>>, {:f, 32}, {1, 1, 1})
      assert {:ok, <<128, 128, 128>>, 1, 1} = Display.to_rgb(f32)

      tensor = Nx.tensor([[[1, 2, 3]], [[4, 5, 6]]], type: :u8)
      assert {:ok, <<3, 2, 1, 6, 5, 4>>, 1, 2} = Display.to_rgb(tensor)
    end

    test "rejects images it cannot show" do
      assert {:error, _} = Display.to_rgb(mat(<<1, 2>>, {:u, 8}, {1, 1, 2}))
      assert {:error, _} = Display.to_rgb(mat(<<1::32-native>>, {:s, 32}, {1, 1, 1}))
      assert {:error, _} = Display.to_rgb(:not_an_image)
    end
  end

  test "fit_rect/3 centers the scaled image" do
    assert Display.fit_rect({100, 50}, {300, 100}, true) == {50, 0, 200, 100}
    assert Display.fit_rect({100, 50}, {300, 100}, false) == {0, 0, 300, 100}
  end

  test "to_image_point/3 maps canvas points and drops points outside the image" do
    rect = {50, 0, 200, 100}
    image = {100, 50}
    assert Display.to_image_point({50, 0}, rect, image) == {0, 0}
    assert Display.to_image_point({249, 99}, rect, image) == {99, 49}
    assert Display.to_image_point({49, 0}, rect, image) == nil
    assert Display.to_image_point({250, 0}, rect, image) == nil
  end

  test "mouse events and flags use OpenCV codes" do
    assert Display.mouse_event(:motion) == Evision.Constant.cv_EVENT_MOUSEMOVE()
    assert Display.mouse_event(:left_down) == Evision.Constant.cv_EVENT_LBUTTONDOWN()
    assert Display.mouse_event(:right_up) == Evision.Constant.cv_EVENT_RBUTTONUP()
    assert Display.mouse_event(:middle_dclick) == Evision.Constant.cv_EVENT_MBUTTONDBLCLK()
    assert Display.mouse_event(:mousewheel) == Evision.Constant.cv_EVENT_MOUSEWHEEL()
    assert Display.mouse_event(:enter_window) == nil

    event = %{
      type: :left_down,
      leftDown: true,
      rightDown: false,
      middleDown: false,
      controlDown: true,
      shiftDown: false,
      altDown: false,
      wheelRotation: 0
    }

    assert Display.mouse_flags(event) ==
             (Evision.Constant.cv_EVENT_FLAG_LBUTTON() |||
                Evision.Constant.cv_EVENT_FLAG_CTRLKEY())

    wheel = %{
      event
      | type: :mousewheel,
        leftDown: false,
        controlDown: false,
        shiftDown: true,
        wheelRotation: -120
    }

    flags = Display.mouse_flags(wheel)
    assert (flags &&& 0xFFFF) == Evision.Constant.cv_EVENT_FLAG_SHIFTKEY()
    assert Evision.Wx.getMouseWheelDelta(flags) == -120

    assert Evision.Wx.getMouseWheelDelta(Display.mouse_flags(%{wheel | wheelRotation: 240})) ==
             240
  end

  test "key_code/2 prefers the character" do
    assert Display.key_code(?Q, ?q) == ?q
    assert Display.key_code(27, 27) == 27
    assert Display.key_code(314, 0) == 314
  end
end
