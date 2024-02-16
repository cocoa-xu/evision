defmodule Evision.Wx do
  @moduledoc """
  Interact with wxWidgets
  """

  if Code.ensure_loaded?(:wx_object) do
    # use GenServer
    @behaviour :wx_object

    @wxVERTICAL 8
    @wxEXPAND 8192
    @wxFULL_REPAINT_ON_RESIZE 65536
    @process_env_key "Evision.Wx.windows"

    defp check_wx! do
      case Code.ensure_compiled(:wx) do
        {:error, _} ->
          raise RuntimeError,
                ":wx is not available, please ensure Erlang was compiled with wxWidgets."

        _ ->
          :ok
      end
    end

    @spec imshow(String.t(), Evision.Mat.maybe_mat_in()) :: :ok
    def imshow(window_name, image)
        when is_binary(window_name) and is_struct(image, Evision.Mat) do
      check_wx!()

      windows = Process.get(@process_env_key, %{})
      window = Map.get(windows, window_name)

      wx_pid =
        case window do
          nil -> nil
          {_, _, _, pid} -> pid
        end

      window =
        if window == nil || !Process.alive?(wx_pid) do
          wx_window = :wx.new()

          wx_obj =
            {:wx_ref, _, :wxFrame, window} =
            :wx_object.start(__MODULE__, {window_name, wx_window}, [])

          Process.put(@process_env_key, Map.put(windows, window_name, wx_obj))
          window
        else
          {:wx_ref, _, :wxFrame, window} = window
          window
        end

      GenServer.call(window, {:imshow, image})
    end

    @spec destroyWindow(String.t()) :: :ok
    def destroyWindow(window_name) do
      check_wx!()

      windows = Process.get(@process_env_key, %{})
      window = Map.get(windows, window_name)

      if window do
        try do
          :wxFrame.close(window)
        catch
          :error, {_, {:wxWindow, :close, _}} -> :ok
        end

        Process.put(@process_env_key, Map.delete(windows, window_name))
      end

      :ok
    end

    @spec destroyAllWindows() :: :ok
    def destroyAllWindows() do
      check_wx!()

      windows = Process.get(@process_env_key, %{})

      Enum.each(Map.values(windows), fn window ->
        try do
          :wxFrame.close(window)
        catch
          :error, {_, {:wxWindow, :close, _}} -> :ok
        end
      end)

      Process.put(@process_env_key, %{})
      :ok
    end

    @impl true
    def init({window_name, window}) do
      :wx.batch(fn ->
        frame = :wxFrame.new(window, 0, String.to_charlist(window_name))
        panel = :wxPanel.new(frame, [])

        mainSizer = :wxBoxSizer.new(@wxVERTICAL)
        sizer = :wxStaticBoxSizer.new(@wxVERTICAL, panel)

        canvas = :wxPanel.new(panel, style: @wxFULL_REPAINT_ON_RESIZE)

        :wxPanel.connect(canvas, :paint, [:callback])
        :wxPanel.connect(canvas, :size)
        :wxPanel.connect(canvas, :left_down)
        :wxPanel.connect(canvas, :left_up)
        :wxPanel.connect(canvas, :motion)

        :wxSizer.add(sizer, canvas, flag: @wxEXPAND, proportion: 1)
        :wxSizer.add(mainSizer, sizer, flag: @wxEXPAND, proportion: 1)
        :wxPanel.setSizer(panel, mainSizer)
        :wxSizer.layout(mainSizer)

        {w, h} = :wxPanel.getSize(canvas)
        bitmap = :wxBitmap.new(max(w, 30), max(h, 30))
        :wxFrame.show(frame)

        {frame,
         %{
           window_name: window_name,
           window: window,
           frame: frame,
           panel: panel,
           canvas: canvas,
           bitmap: bitmap,
           image: nil
         }}
      end)
    end

    @impl true
    def handle_event(
          {:wx, _, _, _, {:wxSize, :size, {w, h}, _}},
          state = %{canvas: canvas, bitmap: prev, image: image}
        ) do
      if w > 0 and h > 0 do
        bitmap = :wxBitmap.new(w, h)
        draw_canvas(image, canvas, bitmap)
        :wxBitmap.destroy(prev)
        {:noreply, %{state | bitmap: bitmap}}
      else
        {:noreply, state}
      end
    end

    @impl true
    def handle_event(_wx, state) do
      {:noreply, state}
    end

    @impl true
    def handle_sync_event({:wx, _, _, _, {:wxPaint, :paint}}, _obj, %{
          canvas: canvas,
          bitmap: bitmap,
          image: image
        }) do
      draw_canvas(image, canvas, bitmap)
      :ok
    end

    @impl true
    def handle_sync_event(_wx, _obj, _state) do
      :ok
    end

    @impl true
    def handle_call({:imshow, image}, _from, state = %{canvas: canvas, bitmap: bitmap}) do
      case draw_canvas(image, canvas, bitmap) do
        :ok ->
          {:reply, :ok, %{state | image: image}}

        :error ->
          {:reply, :error, state}
      end
    end

    defp draw_canvas(nil, _canvas, _bitmap) do
      :ok
    end

    defp draw_canvas(image, canvas, bitmap) when is_struct(image, Evision.Mat) do
      {canvasW, canvasH} = :wxPanel.getSize(canvas)
      canvas_w = trunc(canvasW)
      canvas_h = trunc(canvasH)

      {image_h, image_w} =
        case Evision.Mat.shape(image) do
          {h, w} ->
            {h, w}

          {h, w, c} when c in [1, 3, 4] ->
            {h, w}

          _ ->
            {0, 0}
        end

      with true <- image_h > 0 and image_w > 0 and canvas_h > 0 and canvas_w > 0,
           proportional = min(canvas_w / image_w, canvas_h / image_h),
           {target_w, target_h} = {trunc(image_w * proportional), trunc(image_h * proportional)},
           resized = Evision.resize(image, {target_w, target_h}),
           true <- is_struct(resized, Evision.Mat),
           rgb = Evision.cvtColor(resized, Evision.Constant.cv_COLOR_BGR2RGB()),
           true <- is_struct(rgb, Evision.Mat),
           binary = Evision.Mat.to_binary(rgb),
           true <- is_binary(binary) do
        img = :wxImage.new(target_w, target_h, binary)
        bmp = :wxBitmap.new(img)

        memoryDC = :wxMemoryDC.new(bitmap)
        :wxDC.clear(memoryDC)
        :wxDC.drawBitmap(memoryDC, bmp, {0, 0})
        canvasDC = :wxWindowDC.new(canvas)

        :wxDC.blit(
          canvasDC,
          {0, 0},
          {:wxBitmap.getWidth(bitmap), :wxBitmap.getHeight(bitmap)},
          memoryDC,
          {0, 0}
        )

        :wxWindowDC.destroy(canvasDC)
        :wxMemoryDC.destroy(memoryDC)

        :wxBitmap.destroy(bmp)
        :wxImage.destroy(img)
        :ok
      else
        _ ->
          :error
      end
    end

    defp draw_canvas(_, _canvas, _bitmap) do
      :ok
    end
  else
    def imshow(_window_name, _image) do
      raise RuntimeError,
            """
            :wx_object is not available, please ensure :wx_object was compiled with the application.
            """
    end

    def destroyWindow(_window_name) do
      raise RuntimeError,
            """
            :wx_object is not available, please ensure :wx_object was compiled with the application.
            """
    end

    def destroyAllWindows() do
      raise RuntimeError,
            """
            :wx_object is not available, please ensure :wx_object was compiled with the application.
            """
    end
  end
end
