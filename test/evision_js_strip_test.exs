defmodule Evision.JSStripTest do
  # Replaces the in-memory BEAMs of several `Evision.*` modules with their
  # stripped equivalents in `setup_all`, then restores the originals in
  # `on_exit`. Must run alone to avoid any parallel test seeing the
  # transient stripped state.
  use ExUnit.Case, async: false

  @moduletag :js_correspondence

  # Picked to cover every shape `Evision.JS` exposes plus the runtime helper
  # itself. None of these modules declare `@on_load`, so `:code.load_binary`
  # does not re-enter NIF registration. The NIF lives in `Evision.NIF` which
  # this test leaves untouched.
  @target_modules [
    Evision,
    Evision.QRCodeDetector,
    Evision.FishEye,
    Evision.ArUco.Dictionary,
    Evision.JS
  ]

  setup_all do
    Enum.each(@target_modules, &Code.ensure_loaded!/1)

    originals = capture_originals(@target_modules)
    stripped = strip_each(originals)

    load_each(stripped)
    on_exit(fn -> load_each(originals) end)

    :ok
  end

  describe "@js attributes survive `:beam_lib.strip(_, [~c\"Attr\"])` on the loaded BEAM" do
    test "Evision root carries free-function entries (canny)" do
      entries = js_entries(Evision)

      assert length(entries) > 0

      for arity <- [3, 4, 5] do
        assert %{js_kind: :function, js_name: "cv.Canny"} =
                 find_entry(entries, :canny, arity)
      end

      # arg_plan (CCD-54) rides the same Attr chunk, so it survives stripping too.
      assert %{arg_plan: [:in, :out, :in, :in]} = find_entry(entries, :canny, 3)
    end

    test "Evision.QRCodeDetector carries constructor + method entries" do
      entries = js_entries(Evision.QRCodeDetector)

      assert %{js_kind: :constructor, js_class: "cv.QRCodeDetector"} =
               find_entry(entries, :qrCodeDetector, 0)

      assert %{
               js_kind: :method,
               js_class: "cv.QRCodeDetector",
               js_method: "detect"
             } = find_entry(entries, :detect, 2)
    end

    test "Evision.FishEye fisheye_-prefix entry survives stripping" do
      entries = js_entries(Evision.FishEye)

      assert %{js_kind: :function, js_name: "cv.fisheye_initUndistortRectifyMap"} =
               find_entry(entries, :initUndistortRectifyMap, 6)
    end

    test "Evision.ArUco.Dictionary namespace_prefix_override entry survives stripping" do
      entries = js_entries(Evision.ArUco.Dictionary)

      assert %{js_kind: :constructor, js_class: "cv.Dictionary"} =
               find_entry(entries, :dictionary, 0)
    end
  end

  describe "Evision.JS runtime helper under release-mode stripping" do
    # `@whitelist` / `@index` are regular module attributes inlined into the
    # function bodies (not persistent attributes), so the helper is
    # strip-invariant by construction — `Attr` keep is irrelevant here. The
    # tests confirm the helper actually compiles and runs against the
    # loaded stripped BEAM, which is the property D25 consumers depend on.

    test "runnable?/3 returns true for known triples" do
      assert Evision.JS.runnable?(Evision, :canny, 3)
      assert Evision.JS.runnable?(Evision.QRCodeDetector, :detect, 2)
      assert Evision.JS.runnable?(Evision.FishEye, :initUndistortRectifyMap, 6)
    end

    test "runnable?/3 returns false for evision-specific bindings" do
      refute Evision.JS.runnable?(Evision.Mat, :from_binary, 2)
    end

    test "lookup/3 returns the expected entry for Evision.canny/3" do
      assert {:ok,
              %{
                module: Evision,
                fun: :canny,
                arity: 3,
                js_kind: :function,
                js_name: "cv.Canny"
              }} = Evision.JS.lookup(Evision, :canny, 3)
    end

    test "whitelist/0 still returns the full entry set" do
      whitelist = Evision.JS.whitelist()

      assert length(whitelist) > 0

      assert Enum.any?(whitelist, fn entry ->
               entry.module == Evision.QRCodeDetector and
                 entry.fun == :detect and
                 entry.arity == 2
             end)
    end
  end

  defp js_entries(module) do
    module.module_info(:attributes)
    |> Keyword.get_values(:js)
    |> List.flatten()
  end

  defp find_entry(entries, fun, arity) do
    Enum.find(entries, &(&1.fun == fun and &1.arity == arity))
  end

  defp capture_originals(modules) do
    Map.new(modules, fn module ->
      case :code.which(module) do
        path when is_list(path) ->
          {:ok, bytes} = File.read(path)
          {module, {path, bytes}}

        other ->
          raise "module #{inspect(module)}: :code.which returned #{inspect(other)}, expected a charlist path"
      end
    end)
  end

  defp strip_each(originals) do
    Map.new(originals, fn {module, {path, bytes}} ->
      {:ok, {^module, stripped}} = :beam_lib.strip(bytes, [~c"Attr"])
      {module, {path, stripped}}
    end)
  end

  defp load_each(modules_map) do
    for {module, {path, bytes}} <- modules_map do
      :code.purge(module)
      {:module, ^module} = :code.load_binary(module, path, bytes)
    end

    :ok
  end
end
