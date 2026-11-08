if Code.ensure_loaded?(:wx_object) do
  defmodule Evision.Wx.Window do
    @moduledoc false

    @behaviour :wx_object

    import Bitwise
    require Logger
    require Record

    alias Evision.Wx.Display

    @wxID_ANY -1
    @wxHORIZONTAL 4
    @wxVERTICAL 8
    @wxALL 240
    @wxALIGN_CENTER_VERTICAL 2048
    @wxEXPAND 8192
    @wxRESIZE_BORDER 64
    @wxMAXIMIZE_BOX 512
    @wxSTAY_ON_TOP 32768
    @wxWANTS_CHARS 262_144
    @wxFULL_REPAINT_ON_RESIZE 65536
    @wxDEFAULT_FRAME_STYLE 541_072_960

    @window_autosize 1
    @window_freeratio 0x100
    @window_fullscreen 1
    @wnd_prop_fullscreen 0
    @wnd_prop_autosize 1
    @wnd_prop_aspect_ratio 2
    @wnd_prop_visible 4
    @wnd_prop_topmost 5

    @initial_canvas_size {320, 240}
    @background {0, 0, 0}

    @mouse_fields Keyword.keys(Record.extract(:wxMouse, from_lib: "wx/include/wx.hrl"))
    @key_fields Keyword.keys(Record.extract(:wxKey, from_lib: "wx/include/wx.hrl"))

    def start_link(env, server, name, flags) do
      case :wx_object.start_link(__MODULE__, {env, server, name, flags}, []) do
        {:error, _} = error -> error
        ref -> {:ok, :wx_object.get_pid(ref)}
      end
    end

    @impl true
    def init({env, server, name, flags}) do
      :wx.set_env(env)
      autosize? = (flags &&& @window_autosize) != 0

      style =
        if autosize?,
          do: @wxDEFAULT_FRAME_STYLE &&& ~~~(@wxRESIZE_BORDER ||| @wxMAXIMIZE_BOX),
          else: @wxDEFAULT_FRAME_STYLE

      frame = :wxFrame.new(:wx.null(), @wxID_ANY, name, style: style)
      panel = :wxPanel.new(frame)
      canvas = :wxPanel.new(panel, style: @wxFULL_REPAINT_ON_RESIZE ||| @wxWANTS_CHARS)
      :wxWindow.setBackgroundStyle(canvas, :wxe_util.get_const(:wxBG_STYLE_PAINT))
      :wxWindow.setMinSize(canvas, @initial_canvas_size)

      trackbar_sizer = :wxBoxSizer.new(@wxVERTICAL)
      panel_sizer = :wxBoxSizer.new(@wxVERTICAL)
      :wxSizer.add(panel_sizer, trackbar_sizer, flag: @wxEXPAND)
      :wxSizer.add(panel_sizer, canvas, proportion: 1, flag: @wxEXPAND)
      :wxWindow.setSizer(panel, panel_sizer)

      frame_sizer = :wxBoxSizer.new(@wxVERTICAL)
      :wxSizer.add(frame_sizer, panel, proportion: 1, flag: @wxEXPAND)
      :wxWindow.setSizer(frame, frame_sizer)

      :wxEvtHandler.connect(canvas, :paint, [:callback])
      :wxEvtHandler.connect(canvas, :size)
      :wxEvtHandler.connect(canvas, :char)
      Enum.each(Display.mouse_event_types(), &:wxEvtHandler.connect(canvas, &1))
      :wxEvtHandler.connect(frame, :close_window)

      :wxWindow.fit(frame)
      unless autosize?, do: :wxWindow.setMinSize(canvas, {1, 1})
      :wxFrame.show(frame)
      :wxWindow.raise(frame)
      :wxWindow.setFocus(canvas)

      state = %{
        server: server,
        name: name,
        frame: frame,
        panel: panel,
        canvas: canvas,
        trackbar_sizer: trackbar_sizer,
        autosize?: autosize?,
        keep_ratio?: (flags &&& @window_freeratio) == 0,
        sized?: false,
        image: nil,
        image_size: nil,
        bitmap: nil,
        rect: nil,
        on_mouse: nil,
        trackbars: %{},
        # Callbacks must not run in this process: they may call Evision.Wx on this window.
        runner: spawn_link(&run_callbacks/0)
      }

      {frame, state}
    end

    @impl true
    def handle_call({:imshow, rgb, width, height}, _from, state) do
      image = :wxImage.new(width, height, rgb)
      if state.image, do: :wxImage.destroy(state.image)
      previous_size = state.image_size
      state = %{state | image: image, image_size: {width, height}}

      state =
        cond do
          state.autosize? and previous_size != {width, height} ->
            fit_canvas(state, {width, height})

          not state.autosize? and not state.sized? ->
            %{fit_canvas(state, {width, height}) | sized?: true}

          true ->
            state
        end

      {:reply, :ok, redraw(state)}
    end

    def handle_call({:set_mouse_callback, on_mouse}, _from, state) do
      {:reply, :ok, %{state | on_mouse: on_mouse}}
    end

    def handle_call({:create_trackbar, name, value, count, on_change}, _from, state) do
      case Map.fetch(state.trackbars, name) do
        {:ok, trackbar} ->
          :wxSlider.setRange(trackbar.slider, 0, count)
          :wxSlider.setValue(trackbar.slider, clamp(value, 0, count))
          show_value(trackbar)
          trackbar = %{trackbar | on_change: on_change}
          {:reply, :ok, put_in(state.trackbars[name], trackbar)}

        :error ->
          {:reply, :ok, add_trackbar(state, name, clamp(value, 0, count), count, on_change)}
      end
    end

    def handle_call({:get_trackbar_pos, name}, _from, state) do
      with_trackbar(state, name, fn trackbar -> :wxSlider.getValue(trackbar.slider) end)
    end

    def handle_call({:set_trackbar_pos, name, pos}, _from, state) do
      with_trackbar(state, name, fn %{slider: slider} = trackbar ->
        pos = clamp(pos, :wxSlider.getMin(slider), :wxSlider.getMax(slider))

        if pos != :wxSlider.getValue(slider) do
          :wxSlider.setValue(slider, pos)
          trackbar_changed(state, trackbar, pos)
        end

        :ok
      end)
    end

    def handle_call({:set_trackbar_range, name, min, max}, _from, state) do
      with_trackbar(state, name, fn %{slider: slider} = trackbar ->
        min = if min == :keep, do: :wxSlider.getMin(slider), else: min
        max = if max == :keep, do: :wxSlider.getMax(slider), else: max
        :wxSlider.setRange(slider, min, max)
        show_value(trackbar)
        :ok
      end)
    end

    def handle_call({:move, x, y}, _from, state) do
      :wxWindow.move(state.frame, x, y)
      {:reply, :ok, state}
    end

    def handle_call({:resize, _width, _height}, _from, %{autosize?: true} = state) do
      {:reply, :ok, state}
    end

    def handle_call({:resize, width, height}, _from, state) do
      state = %{fit_canvas(state, {width, height}) | sized?: true}
      {:reply, :ok, redraw(state)}
    end

    def handle_call({:set_title, title}, _from, state) do
      :wxTopLevelWindow.setTitle(state.frame, title)
      {:reply, :ok, state}
    end

    def handle_call({:get_property, prop}, _from, state) do
      {:reply, property(state, prop), state}
    end

    def handle_call({:set_property, prop, value}, _from, state) do
      case set_property(state, prop, value) do
        {:ok, state} -> {:reply, :ok, state}
        {:error, _} = error -> {:reply, error, state}
      end
    end

    def handle_call(:image_rect, _from, state) do
      {x, y, w, h} =
        case state.rect do
          nil ->
            {w, h} = :wxWindow.getClientSize(state.canvas)
            {0, 0, w, h}

          rect ->
            rect
        end

      {screen_x, screen_y} = :wxWindow.clientToScreen(state.canvas, {x, y})
      {:reply, {screen_x, screen_y, w, h}, state}
    end

    def handle_call(:destroy, _from, state) do
      {:stop, :normal, :ok, state}
    end

    @impl true
    def handle_cast(_request, state), do: {:noreply, state}

    @impl true
    def handle_info(_message, state), do: {:noreply, state}

    @impl true
    def handle_event({:wx, _, _, _, {:wxSize, :size, _, _}}, state) do
      {:noreply, redraw(state)}
    end

    def handle_event({:wx, _, _, name, {:wxCommand, :command_slider_updated, _, pos, _}}, state) do
      case Map.fetch(state.trackbars, name) do
        {:ok, trackbar} -> trackbar_changed(state, trackbar, pos)
        :error -> :ok
      end

      {:noreply, state}
    end

    def handle_event({:wx, _, _, _, {:wxClose, :close_window}}, state) do
      {:stop, :normal, state}
    end

    def handle_event({:wx, _, _, _, event}, state) when elem(event, 0) == :wxKey do
      key = to_map(event, @key_fields)
      send(state.server, {:key, Display.key_code(key.keyCode, key.uniChar)})
      {:noreply, state}
    end

    def handle_event({:wx, _, _, _, event}, state) when elem(event, 0) == :wxMouse do
      mouse = to_map(event, @mouse_fields)
      if mouse.type == :left_down, do: :wxWindow.setFocus(state.canvas)

      with fun when is_function(fun, 4) <- state.on_mouse,
           code when is_integer(code) <- Display.mouse_event(mouse.type),
           {_, _, _, _} = rect <- state.rect,
           {x, y} <- Display.to_image_point({mouse.x, mouse.y}, rect, state.image_size) do
        send(state.runner, {:call, fun, [code, x, y, Display.mouse_flags(mouse)]})
      end

      {:noreply, state}
    end

    def handle_event(_event, state), do: {:noreply, state}

    @impl true
    def handle_sync_event({:wx, _, _, _, {:wxPaint, :paint}}, _object, state) do
      dc = :wxPaintDC.new(state.canvas)
      brush = :wxBrush.new(@background)
      :wxDC.setBackground(dc, brush)
      :wxDC.clear(dc)

      with {x, y, _, _} <- state.rect, bitmap when bitmap != nil <- state.bitmap do
        :wxDC.drawBitmap(dc, bitmap, {x, y})
      end

      :wxBrush.destroy(brush)
      :wxPaintDC.destroy(dc)
      :ok
    end

    @impl true
    def terminate(_reason, state) do
      Process.unlink(state.runner)
      Process.exit(state.runner, :kill)
      if state.bitmap, do: :wxBitmap.destroy(state.bitmap)
      if state.image, do: :wxImage.destroy(state.image)
      :wxFrame.destroy(state.frame)
      :ok
    end

    @impl true
    def code_change(_old_vsn, state, _extra), do: {:ok, state}

    defp add_trackbar(state, name, value, count, on_change) do
      canvas_size = canvas_target_size(state)
      row = :wxBoxSizer.new(@wxHORIZONTAL)
      label = :wxStaticText.new(state.panel, @wxID_ANY, name)
      slider = :wxSlider.new(state.panel, @wxID_ANY, value, 0, count)
      value_label = :wxStaticText.new(state.panel, @wxID_ANY, Integer.to_string(value))

      :wxSizer.add(row, label, flag: @wxALIGN_CENTER_VERTICAL ||| @wxALL, border: 4)
      :wxSizer.add(row, slider, proportion: 1, flag: @wxEXPAND ||| @wxALL, border: 4)
      :wxSizer.add(row, value_label, flag: @wxALIGN_CENTER_VERTICAL ||| @wxALL, border: 4)
      :wxSizer.add(state.trackbar_sizer, row, flag: @wxEXPAND)

      :wxEvtHandler.connect(slider, :command_slider_updated, userData: name)
      :wxEvtHandler.connect(slider, :char, skip: true)

      trackbar = %{slider: slider, value_label: value_label, on_change: on_change}

      %{state | trackbars: Map.put(state.trackbars, name, trackbar)}
      |> fit_canvas(canvas_size)
      |> redraw()
    end

    defp with_trackbar(state, name, fun) do
      case Map.fetch(state.trackbars, name) do
        {:ok, trackbar} ->
          {:reply, fun.(trackbar), state}

        :error ->
          {:reply,
           {:error, "trackbar #{inspect(name)} does not exist in window #{inspect(state.name)}"},
           state}
      end
    end

    defp trackbar_changed(state, trackbar, pos) do
      show_value(trackbar)

      if is_function(trackbar.on_change, 1) do
        send(state.runner, {:call, trackbar.on_change, [pos]})
      end
    end

    defp show_value(trackbar) do
      :wxStaticText.setLabel(
        trackbar.value_label,
        Integer.to_string(:wxSlider.getValue(trackbar.slider))
      )

      :wxWindow.layout(:wxWindow.getParent(trackbar.value_label))
    end

    defp property(state, @wnd_prop_fullscreen),
      do: bool_to_float(:wxTopLevelWindow.isFullScreen(state.frame))

    defp property(state, @wnd_prop_autosize), do: bool_to_float(state.autosize?)

    defp property(state, @wnd_prop_aspect_ratio),
      do: if(state.keep_ratio?, do: 0.0, else: @window_freeratio * 1.0)

    defp property(state, @wnd_prop_visible), do: bool_to_float(:wxWindow.isShown(state.frame))

    defp property(state, @wnd_prop_topmost),
      do: bool_to_float((:wxWindow.getWindowStyleFlag(state.frame) &&& @wxSTAY_ON_TOP) != 0)

    defp property(_state, _prop), do: -1.0

    defp set_property(state, @wnd_prop_fullscreen, value) do
      :wxTopLevelWindow.showFullScreen(state.frame, trunc(value) == @window_fullscreen)
      {:ok, state}
    end

    defp set_property(state, @wnd_prop_topmost, value) do
      style = :wxWindow.getWindowStyleFlag(state.frame)

      style =
        if value != 0,
          do: style ||| @wxSTAY_ON_TOP,
          else: style &&& ~~~@wxSTAY_ON_TOP

      :wxWindow.setWindowStyleFlag(state.frame, style)
      {:ok, state}
    end

    defp set_property(state, @wnd_prop_aspect_ratio, value) do
      {:ok, redraw(%{state | keep_ratio?: (trunc(value) &&& @window_freeratio) == 0})}
    end

    defp set_property(_state, prop, _value) do
      {:error, "window property #{prop} cannot be set"}
    end

    defp bool_to_float(true), do: 1.0
    defp bool_to_float(_), do: 0.0

    defp canvas_target_size(%{autosize?: true, image_size: {_, _} = size}), do: size
    defp canvas_target_size(state), do: :wxWindow.getClientSize(state.canvas)

    defp fit_canvas(state, size) do
      :wxWindow.setMinSize(state.canvas, size)
      :wxWindow.fit(state.frame)
      unless state.autosize?, do: :wxWindow.setMinSize(state.canvas, {1, 1})
      :wxWindow.layout(state.frame)
      state
    end

    defp redraw(%{image: nil} = state), do: state

    defp redraw(state) do
      {canvas_w, canvas_h} = :wxWindow.getClientSize(state.canvas)
      {image_w, image_h} = state.image_size

      {_, _, w, h} =
        rect =
        if state.autosize?,
          do: {div(canvas_w - image_w, 2), div(canvas_h - image_h, 2), image_w, image_h},
          else: Display.fit_rect(state.image_size, {canvas_w, canvas_h}, state.keep_ratio?)

      bitmap =
        if {w, h} == state.image_size do
          :wxBitmap.new(state.image)
        else
          quality = :wxe_util.get_const(:wxIMAGE_QUALITY_HIGH)
          scaled = :wxImage.scale(state.image, w, h, quality: quality)
          bitmap = :wxBitmap.new(scaled)
          :wxImage.destroy(scaled)
          bitmap
        end

      if state.bitmap, do: :wxBitmap.destroy(state.bitmap)
      :wxWindow.refresh(state.canvas, eraseBackground: false)
      %{state | bitmap: bitmap, rect: rect}
    end

    defp to_map(record, fields) do
      fields |> Enum.zip(tl(Tuple.to_list(record))) |> Map.new()
    end

    defp clamp(value, min, max), do: value |> max(min) |> min(max)

    defp run_callbacks do
      receive do
        {:call, fun, args} ->
          try do
            apply(fun, args)
          catch
            kind, reason ->
              Logger.error(
                "Evision.Wx callback failed: " <> Exception.format(kind, reason, __STACKTRACE__)
              )
          end

          run_callbacks()
      end
    end
  end
end
