defmodule Evision.JSRuntimeTest do
  use ExUnit.Case, async: true

  describe "whitelist/0" do
    test "returns a non-empty list of maps with the expected shape" do
      whitelist = Evision.JS.whitelist()

      assert is_list(whitelist)
      assert length(whitelist) > 0

      for entry <- whitelist do
        assert is_atom(entry.module)
        assert is_atom(entry.fun)
        assert is_integer(entry.arity) and entry.arity >= 0
        assert entry.js_kind in [:function, :method, :constructor]

        case entry.js_kind do
          :function ->
            assert is_binary(entry.js_name)
            refute Map.has_key?(entry, :js_class)
            refute Map.has_key?(entry, :js_method)

          :method ->
            assert is_binary(entry.js_class)
            assert is_binary(entry.js_method)
            refute Map.has_key?(entry, :js_name)

          :constructor ->
            assert is_binary(entry.js_class)
            refute Map.has_key?(entry, :js_method)
            refute Map.has_key?(entry, :js_name)
        end
      end
    end

    test "per-module count matches the corresponding @js attributes on the loaded module" do
      by_module = Enum.group_by(Evision.JS.whitelist(), & &1.module)

      for {module, runtime_entries} <- by_module do
        Code.ensure_loaded!(module)

        tag_entries =
          module.module_info(:attributes) |> Keyword.get_values(:js) |> List.flatten()

        assert length(tag_entries) == length(runtime_entries),
               "for #{inspect(module)}: runtime has #{length(runtime_entries)} entries, module has #{length(tag_entries)} @js attrs"
      end
    end
  end

  describe "lookup/3" do
    test "returns {:ok, entry} for a known free function (Evision.canny/3)" do
      assert {:ok, entry} = Evision.JS.lookup(Evision, :canny, 3)

      assert %{
               module: Evision,
               fun: :canny,
               arity: 3,
               js_kind: :function,
               js_name: "cv.Canny"
             } = entry
    end

    test "returns {:ok, entry} for a known method (Evision.CascadeClassifier.detectMultiScale/2)" do
      assert {:ok, entry} =
               Evision.JS.lookup(Evision.CascadeClassifier, :detectMultiScale, 2)

      assert %{
               module: Evision.CascadeClassifier,
               fun: :detectMultiScale,
               arity: 2,
               js_kind: :method,
               js_class: "cv.CascadeClassifier",
               js_method: "detectMultiScale"
             } = entry
    end

    test "returns {:ok, entry} for a known constructor (Evision.CascadeClassifier.cascadeClassifier/0)" do
      assert {:ok, entry} =
               Evision.JS.lookup(Evision.CascadeClassifier, :cascadeClassifier, 0)

      assert %{
               module: Evision.CascadeClassifier,
               fun: :cascadeClassifier,
               arity: 0,
               js_kind: :constructor,
               js_class: "cv.CascadeClassifier"
             } = entry
    end

    test "returns :error for an evision binding with no opencv.js counterpart" do
      assert :error = Evision.JS.lookup(Evision.Mat, :from_binary, 2)
    end

    test "returns :error for an unknown module" do
      assert :error = Evision.JS.lookup(Evision.NotAModule, :nope, 0)
    end

    test "returns :error when arity is wrong even if (module, fun) exists" do
      {:ok, _} = Evision.JS.lookup(Evision, :canny, 3)
      assert :error = Evision.JS.lookup(Evision, :canny, 99)
    end
  end

  describe "runnable?/3" do
    test "true for a free function known to opencv.js" do
      assert Evision.JS.runnable?(Evision, :canny, 3)
    end

    test "true for a class method known to opencv.js" do
      assert Evision.JS.runnable?(Evision.CascadeClassifier, :detectMultiScale, 2)
    end

    test "false for evision-specific binding (Evision.Mat.from_binary/2)" do
      refute Evision.JS.runnable?(Evision.Mat, :from_binary, 2)
    end

    test "false for unknown triple" do
      refute Evision.JS.runnable?(Evision.NotAModule, :nope, 0)
    end
  end

  describe "round-trip" do
    test "every entry in whitelist/0 round-trips through lookup/3" do
      for entry <- Evision.JS.whitelist() do
        assert {:ok, ^entry} =
                 Evision.JS.lookup(entry.module, entry.fun, entry.arity)
      end
    end

    test "every entry in whitelist/0 is runnable?/3" do
      for entry <- Evision.JS.whitelist() do
        assert Evision.JS.runnable?(entry.module, entry.fun, entry.arity),
               "entry #{inspect(entry)} should be runnable?"
      end
    end
  end

  describe "fisheye + namespace_prefix_override coverage (CCD-18 refinements)" do
    test "Evision.FishEye.initUndistortRectifyMap/6 → cv.fisheye_initUndistortRectifyMap" do
      assert {:ok, %{js_kind: :function, js_name: "cv.fisheye_initUndistortRectifyMap"}} =
               Evision.JS.lookup(Evision.FishEye, :initUndistortRectifyMap, 6)
    end

    test "Evision.ArUco.Dictionary constructor projects to cv.Dictionary (override strips prefix)" do
      assert {:ok, %{js_kind: :constructor, js_class: "cv.Dictionary"}} =
               Evision.JS.lookup(Evision.ArUco.Dictionary, :dictionary, 0)
    end
  end
end
