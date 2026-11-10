if Code.ensure_loaded?(Evision.Wx.Server) do
  defmodule Evision.Wx.Test do
    use ExUnit.Case, async: false

    import ExUnit.CaptureLog
    require Record

    alias Evision.Constant
    alias Evision.Wx

    @moduletag :wx

    @mouse_fields Keyword.keys(Record.extract(:wxMouse, from_lib: "wx/include/wx.hrl"))
    @key_fields Keyword.keys(Record.extract(:wxKey, from_lib: "wx/include/wx.hrl"))

    setup do
      on_exit(fn -> Wx.destroyAllWindows() end)

      %{
        image:
          Evision.Mat.zeros({300, 400}, :u8) |> Evision.cvtColor(Constant.cv_COLOR_GRAY2BGR())
      }
    end

    defp window_pid(name) do
      {:ok, pid} = Evision.Wx.Server.window(name)
      pid
    end

    defp inject(name, event, user_data \\ []) do
      pid = window_pid(name)
      send(pid, {:wx, -1, :undefined, user_data, event})
      # Wait until the window has handled the event.
      GenServer.call(pid, :image_rect)
    end

    defp mouse(type, x, y, fields \\ []) do
      values =
        Keyword.merge(
          [
            type: type,
            x: x,
            y: y,
            leftDown: false,
            middleDown: false,
            rightDown: false,
            controlDown: false,
            shiftDown: false,
            altDown: false,
            metaDown: false,
            wheelRotation: 0,
            wheelDelta: 120,
            linesPerAction: 3
          ],
          fields
        )

      List.to_tuple([:wxMouse | Enum.map(@mouse_fields, &Keyword.get(values, &1))])
    end

    defp key(char) do
      values = [type: :char, x: 0, y: 0, keyCode: char, uniChar: char, rawCode: 0, rawFlags: 0]
      List.to_tuple([:wxKey | Enum.map(@key_fields, &Keyword.get(values, &1, false))])
    end

    test "imshow opens an autosize window the size of the image", %{image: image} do
      assert :ok = Wx.imshow("autosize", image)
      assert Wx.getWindowProperty("autosize", Constant.cv_WND_PROP_VISIBLE()) == 1.0
      assert Wx.getWindowProperty("autosize", Constant.cv_WND_PROP_AUTOSIZE()) == 1.0
      assert {_, _, 400, 300} = Wx.getWindowImageRect("autosize")

      assert :ok = Wx.imshow("autosize", Nx.broadcast(Nx.tensor(128, type: :u8), {100, 200, 3}))
      assert {_, _, 200, 100} = Wx.getWindowImageRect("autosize")
    end

    test "a normal window scales the image to the requested size", %{image: image} do
      assert :ok = Wx.namedWindow("normal", Constant.cv_WINDOW_NORMAL())
      assert Wx.getWindowProperty("normal", Constant.cv_WND_PROP_AUTOSIZE()) == 0.0
      assert :ok = Wx.imshow("normal", image)
      assert :ok = Wx.resizeWindow("normal", 200, 150)
      assert {_, _, 200, 150} = Wx.getWindowImageRect("normal")
    end

    test "mouse callbacks get image coordinates and OpenCV flags", %{image: image} do
      test = self()
      :ok = Wx.namedWindow("mouse", Constant.cv_WINDOW_NORMAL())
      :ok = Wx.imshow("mouse", image)
      :ok = Wx.resizeWindow("mouse", 200, 150)
      :ok = Wx.setMouseCallback("mouse", &send(test, {:mouse, &1, &2, &3, &4}))

      inject("mouse", mouse(:left_down, 100, 50, leftDown: true, shiftDown: true))
      assert_receive {:mouse, event, 200, 100, flags}
      assert event == Constant.cv_EVENT_LBUTTONDOWN()

      assert flags ==
               Bitwise.bor(Constant.cv_EVENT_FLAG_LBUTTON(), Constant.cv_EVENT_FLAG_SHIFTKEY())

      inject("mouse", mouse(:mousewheel, 0, 0, wheelRotation: -120))
      assert_receive {:mouse, event, 0, 0, flags}
      assert event == Constant.cv_EVENT_MOUSEWHEEL()
      assert Wx.getMouseWheelDelta(flags) == -120

      inject("mouse", mouse(:motion, 500, 500))
      refute_receive {:mouse, _, _, _, _}, 50

      :ok = Wx.setMouseCallback("mouse", nil)
      inject("mouse", mouse(:motion, 10, 10))
      refute_receive {:mouse, _, _, _, _}, 50
    end

    test "callbacks can use the window and do not take it down when they fail", %{image: image} do
      test = self()
      :ok = Wx.imshow("callbacks", image)

      :ok =
        Wx.setMouseCallback("callbacks", fn _event, x, _y, _flags ->
          if x == 0, do: raise("boom")
          send(test, {:imshow, Wx.imshow("callbacks", image)})
        end)

      assert capture_log(fn ->
               inject("callbacks", mouse(:left_down, 0, 0))
               inject("callbacks", mouse(:left_down, 1, 0))
               assert_receive {:imshow, :ok}
             end) =~ "boom"
    end

    test "waitKey returns queued and incoming keys", %{image: image} do
      :ok = Wx.imshow("keys", image)
      assert Wx.pollKey() == -1
      assert Wx.waitKey(10) == -1

      inject("keys", key(?q))
      assert Wx.pollKey() == ?q

      waiter = Task.async(fn -> Wx.waitKey() end)
      Process.sleep(50)
      inject("keys", key(0x263A))
      assert Task.await(waiter) == 0x3A

      inject("keys", key(0x263A))
      assert Wx.waitKeyEx(1000) == 0x263A
    end

    test "waitKey returns -1 when the last window goes away", %{image: image} do
      assert Wx.waitKey() == -1

      :ok = Wx.imshow("last", image)
      waiter = Task.async(fn -> Wx.waitKey() end)
      Process.sleep(50)
      :ok = Wx.destroyWindow("last")
      assert Task.await(waiter) == -1
    end

    test "trackbars", %{image: image} do
      test = self()
      :ok = Wx.imshow("trackbars", image)
      assert {:error, _} = Wx.getTrackbarPos("t", "trackbars")

      :ok = Wx.createTrackbar("t", "trackbars", 5, 10, &send(test, {:pos, &1}))
      assert Wx.getTrackbarPos("t", "trackbars") == 5
      assert {_, _, 400, 300} = Wx.getWindowImageRect("trackbars")

      :ok = Wx.setTrackbarPos("t", "trackbars", 7)
      assert_receive {:pos, 7}
      :ok = Wx.setTrackbarPos("t", "trackbars", 7)
      refute_receive {:pos, _}, 50

      :ok = Wx.setTrackbarMax("t", "trackbars", 8)
      :ok = Wx.setTrackbarPos("t", "trackbars", 20)
      assert_receive {:pos, 8}

      :ok = Wx.setTrackbarMin("t", "trackbars", 2)
      :ok = Wx.setTrackbarPos("t", "trackbars", 0)
      assert_receive {:pos, 2}

      inject("trackbars", {:wxCommand, :command_slider_updated, [], 3, 0}, "t")
      assert_receive {:pos, 3}
    end

    test "window management", %{image: image} do
      :ok = Wx.imshow("manage", image)
      assert :ok = Wx.setWindowTitle("manage", "标题")
      assert :ok = Wx.moveWindow("manage", 40, 60)
      assert :ok = Wx.setWindowProperty("manage", Constant.cv_WND_PROP_TOPMOST(), 1)
      assert Wx.getWindowProperty("manage", Constant.cv_WND_PROP_TOPMOST()) == 1.0
      assert {:error, _} = Wx.setWindowProperty("manage", Constant.cv_WND_PROP_VSYNC(), 1)
      assert Wx.getWindowProperty("manage", Constant.cv_WND_PROP_OPENGL()) == -1.0
      assert {:error, _} = Wx.namedWindow("gl", Constant.cv_WINDOW_OPENGL())
      assert {:error, _} = Wx.imshow("manage", Evision.Mat.zeros({2, 2, 2}, :u8))
    end

    test "a window closed by the user is gone and can be opened again", %{image: image} do
      :ok = Wx.imshow("closed", image)
      ref = Process.monitor(window_pid("closed"))
      send(window_pid("closed"), {:wx, -1, :undefined, [], {:wxClose, :close_window}})
      assert_receive {:DOWN, ^ref, :process, _, :normal}

      assert Wx.getWindowProperty("closed", Constant.cv_WND_PROP_VISIBLE()) == -1.0
      assert {:error, _} = Wx.setMouseCallback("closed", nil)
      assert :ok = Wx.imshow("closed", image)
      assert Wx.getWindowProperty("closed", Constant.cv_WND_PROP_VISIBLE()) == 1.0
    end
  end
else
  defmodule Evision.Wx.Test do
    use ExUnit.Case

    @moduletag :wx

    test "Evision.Wx is compiled" do
      flunk("Evision.Wx was not compiled: this Erlang/OTP has no wx")
    end
  end
end
