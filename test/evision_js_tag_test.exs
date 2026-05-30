defmodule Evision.JSTagTest do
  use ExUnit.Case, async: true

  defp js_entries(module) do
    module.module_info(:attributes)
    |> Keyword.get_values(:js)
    |> List.flatten()
  end

  defp find_entry(entries, fun, arity) do
    Enum.find(entries, &(&1.fun == fun and &1.arity == arity))
  end

  defp strip_with_attr(beam) do
    {:ok, {_mod, chunks}} =
      :beam_lib.chunks(beam, :beam_lib.significant_chunks() ++ [~c"Attr"],
        [:allow_missing_chunks])

    chunks = for {name, chunk} <- chunks, is_binary(chunk), do: {name, chunk}
    {:ok, stripped} = :beam_lib.build_module(chunks)
    stripped
  end

  describe "free functions on Evision" do
    setup do
      %{entries: js_entries(Evision)}
    end

    test "canny (all arities tagged)", %{entries: entries} do
      for arity <- [3, 4, 5] do
        assert %{js_kind: :function, js_name: "cv.Canny"} =
                 find_entry(entries, :canny, arity)
      end
    end

    test "cvtColor", %{entries: entries} do
      assert %{js_kind: :function, js_name: "cv.cvtColor"} =
               find_entry(entries, :cvtColor, 2)
    end

    test "gaussianBlur", %{entries: entries} do
      assert %{js_kind: :function, js_name: "cv.GaussianBlur"} =
               find_entry(entries, :gaussianBlur, 3)
    end

    test "blur", %{entries: entries} do
      assert %{js_kind: :function, js_name: "cv.blur"} =
               find_entry(entries, :blur, 2)
    end

    test "resize", %{entries: entries} do
      assert %{js_kind: :function, js_name: "cv.resize"} =
               find_entry(entries, :resize, 2)
    end

    test "threshold", %{entries: entries} do
      assert %{js_kind: :function, js_name: "cv.threshold"} =
               find_entry(entries, :threshold, 4)
    end

    test "arg_plan records the opencv.js positional layout (CCD-54)", %{entries: entries} do
      # opencv.js takes the dst OutputArray positionally; evision returns it.
      # arg_plan records where dst sits so the workflow→opencv.js compiler can
      # place the pre-allocated Mat. cvtColor(src, dst, code); add(s1, s2, dst).
      assert %{arg_plan: [:in, :out, :in]} = find_entry(entries, :cvtColor, 2)
      assert %{arg_plan: [:in, :out, :in, :in]} = find_entry(entries, :canny, 3)
      assert %{arg_plan: [:in, :out, :in, :in]} = find_entry(entries, :gaussianBlur, 3)
      assert %{arg_plan: [:in, :out, :in, :in, :in]} = find_entry(entries, :threshold, 4)
      assert %{arg_plan: [:in, :out, :in]} = find_entry(entries, :blur, 2)
      assert %{arg_plan: [:in, :in, :out]} = find_entry(entries, :add, 2)
    end

    test "ambiguous same-arity overloads carry no arg_plan (canny/4)", %{entries: entries} do
      # canny/4 is produced by two C++ variants with different layouts
      # (image, t1, t2, opts vs dx, dy, t1, t2), so the plan is omitted — the
      # compiler hard-errors on such a node rather than guessing the overload.
      refute Map.has_key?(find_entry(entries, :canny, 4), :arg_plan)
    end
  end

  describe "class methods + constructor on Evision.CascadeClassifier" do
    setup do
      %{entries: js_entries(Evision.CascadeClassifier)}
    end

    test "constructor (arity 0 and 1)", %{entries: entries} do
      for arity <- [0, 1] do
        assert %{js_kind: :constructor, js_class: "cv.CascadeClassifier"} =
                 find_entry(entries, :cascadeClassifier, arity)
      end
    end

    test "detectMultiScale method", %{entries: entries} do
      assert %{
               js_kind: :method,
               js_class: "cv.CascadeClassifier",
               js_method: "detectMultiScale"
             } = find_entry(entries, :detectMultiScale, 2)
    end

    test "method arg_plan covers post-receiver args only (CCD-54)", %{entries: entries} do
      # The receiver is the wire's `recv`; arg_plan describes only the args
      # after it. detectMultiScale(self, image) → opencv.js (image, objects/dst).
      assert %{arg_plan: [:in, :out]} = find_entry(entries, :detectMultiScale, 2)
    end
  end

  describe "fisheye namespace flattening (refinement #4)" do
    test "Evision.FishEye.initUndistortRectifyMap carries fisheye_ prefix" do
      entries = js_entries(Evision.FishEye)

      assert %{js_kind: :function, js_name: "cv.fisheye_initUndistortRectifyMap"} =
               find_entry(entries, :initUndistortRectifyMap, 6)
    end
  end

  describe "class with no constructor (refinement #3)" do
    @tag :dnn
    test "Evision.DNN.Net has methods but no constructor entry" do
      entries = js_entries(Evision.DNN.Net)

      refute Enum.any?(entries, &(&1.js_kind == :constructor))

      assert %{
               js_kind: :method,
               js_class: "cv.Net",
               js_method: "forward"
             } = find_entry(entries, :forward, 2)
    end
  end

  describe "namespace_prefix_override (refinement #2)" do
    test "aruco_Dictionary projects to cv.Dictionary (override strips prefix)" do
      entries = js_entries(Evision.ArUco.Dictionary)

      assert %{js_kind: :constructor, js_class: "cv.Dictionary"} =
               find_entry(entries, :dictionary, 0)
    end
  end

  describe "Mat.from_binary is evision-specific (no @js tag)" do
    test "no @js entry for from_binary" do
      entries = js_entries(Evision.Mat)
      refute Enum.any?(entries, &(&1.fun == :from_binary))
    end
  end

  describe "Erlang generated source emits -js attributes" do
    test "evision.erl carries -js lines for free functions" do
      path = Path.join([File.cwd!(), "src", "generated", "evision.erl"])
      assert File.exists?(path), "generated Erlang source missing: #{path}"

      source = File.read!(path)

      assert source =~
               ~r/-js\(#\{fun => canny, arity => \d+, js_kind => function, js_name => <<"cv\.Canny">>\}\)\./
    end

    test "evision_cascadeclassifier.erl emits method + constructor entries" do
      source =
        Path.join([File.cwd!(), "src", "generated", "evision_cascadeclassifier.erl"])
        |> File.read!()

      assert source =~
               ~r/-js\(#\{fun => cascadeClassifier, arity => 0, js_kind => constructor, js_class => <<"cv\.CascadeClassifier">>\}\)\./

      assert source =~
               ~r/-js\(#\{fun => detectMultiScale, arity => 2, js_kind => method, js_class => <<"cv\.CascadeClassifier">>, js_method => <<"detectMultiScale">>, arg_plan => \[in, out\]\}\)\./
    end
  end

  describe "post-strip survival (release-mode property)" do
    # The release-mode property to validate is: when a Mix release strips
    # BEAM files, the `Attr` chunk (carrying `@js` persistent attributes)
    # survives. The realistic strip used by Mix is `:beam_lib.strip_release/2`
    # with an explicit additional-chunks list — that's the contracted API
    # (the `Attr` chunk is guaranteed to survive when listed). `mix release`
    # currently includes `"Attr"` in its default keep list but documents it
    # as "not guaranteed to remain in future versions" — so D25 deployments
    # MUST opt in explicitly via `strip_beams: [keep: ["Attr"]]`.

    test "@js attributes survive strip_release-style strip on Evision.CascadeClassifier" do
      path = :code.which(Evision.CascadeClassifier)
      assert is_list(path), "expected loaded BEAM path, got #{inspect(path)}"

      {:ok, original} = File.read(path)

      original_js =
        Evision.CascadeClassifier
        |> js_entries()
        |> length()

      assert original_js > 0, "test prerequisite: module has @js attributes pre-strip"

      stripped = strip_with_attr(original)

      assert byte_size(stripped) < byte_size(original),
             "strip should remove at least Docs/Dbgi chunks"

      {:ok, {_mod, [{~c"Attr", attr_bin}]}} =
        :beam_lib.chunks(stripped, [~c"Attr"])

      attrs = :erlang.binary_to_term(attr_bin)
      stripped_js = attrs |> Keyword.get_values(:js) |> List.flatten()

      assert length(stripped_js) == original_js,
             "@js count must survive strip: pre=#{original_js} post=#{length(stripped_js)}"

      assert %{js_kind: :method, js_method: "detectMultiScale"} =
               Enum.find(stripped_js, &(&1.fun == :detectMultiScale and &1.arity == 2))
    end

    test "@js attributes survive strip_release-style strip on Evision (free functions)" do
      path = :code.which(Evision)
      {:ok, original} = File.read(path)

      stripped = strip_with_attr(original)

      {:ok, {_mod, [{~c"Attr", attr_bin}]}} =
        :beam_lib.chunks(stripped, [~c"Attr"])

      attrs = :erlang.binary_to_term(attr_bin)
      stripped_js = attrs |> Keyword.get_values(:js) |> List.flatten()

      assert %{js_kind: :function, js_name: "cv.Canny"} =
               Enum.find(stripped_js, &(&1.fun == :canny and &1.arity == 3))
    end

    test "bare :beam_lib.strip/1 DROPS Attr — documenting the release config requirement" do
      # This negative assertion guards against any reader assuming the bare
      # strip/1 form preserves Attr. It doesn't: only strip_release/2 with
      # an explicit keep list (or Mix release's current — undocumented —
      # default) does. If OTP ever changes strip/1 to preserve Attr this
      # test will flip and force a doc update.
      path = :code.which(Evision.CascadeClassifier)
      {:ok, original} = File.read(path)
      {:ok, {Evision.CascadeClassifier, stripped}} = :beam_lib.strip(original)

      assert {:error, :beam_lib, {:missing_chunk, _, _}} =
               :beam_lib.chunks(stripped, [~c"Attr"])
    end
  end
end
