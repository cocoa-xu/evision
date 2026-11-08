defmodule Evision.StructSetter.Test do
  use ExUnit.Case

  describe "plain struct property setters" do
    @describetag :video

    test "assign the value" do
      params =
        Evision.TrackerNano.Params.params()
        |> Evision.TrackerNano.Params.set_backbone("backbone.onnx")
        |> Evision.TrackerNano.Params.set_backend(3)

      assert Evision.TrackerNano.Params.get_backbone(params) == "backbone.onnx"
      assert Evision.TrackerNano.Params.get_backend(params) == 3
    end

    test "mutate the underlying object in place" do
      params = Evision.TrackerNano.Params.params()
      _ = Evision.TrackerNano.Params.set_backend(params, 3)
      assert Evision.TrackerNano.Params.get_backend(params) == 3
    end
  end

  test "every generated setter accepts the value its getter returns" do
    failures =
      for {module, ctor} <- default_constructible_modules(),
          object = initialized_object(module, ctor),
          is_struct(object, module),
          {prop, getter, setter} <- properties(module),
          value = apply(module, getter, [object]),
          not match?({:error, _}, value),
          result = apply(module, setter, [object, value]),
          not is_struct(result, module) do
        {module, prop, result}
      end

    assert failures == [],
           "#{length(failures)} setter(s) rejected their getter's value:\n" <>
             Enum.map_join(failures, "\n", fn {module, prop, result} ->
               "  #{inspect(module)}.set_#{prop}/2 -> #{inspect(result)}"
             end)
  end

  # These OpenCV constructors leave scalar fields uninitialized, and a garbage
  # NaN float makes the getter raise. Set known values before any getter reads
  # them, and check the round trip.
  @known_values %{
    Evision.LineDescriptor.KeyLine => [
      angle: 0.5,
      class_id: 3,
      octave: 1,
      pt: {2.0, 3.0},
      response: 0.75,
      size: 8.0,
      startPointX: 1.0,
      startPointY: 2.0,
      endPointX: 3.0,
      endPointY: 4.0,
      sPointInOctaveX: 0.5,
      sPointInOctaveY: 1.0,
      ePointInOctaveX: 1.5,
      ePointInOctaveY: 2.0,
      lineLength: 2.5,
      numOfPixels: 4
    ],
    Evision.LineMod.Match => [x: 1, y: 2, similarity: 0.5, class_id: "a", template_id: 3]
  }

  defp initialized_object(module, ctor) do
    @known_values
    |> Map.get(module, [])
    |> Enum.reduce(apply(module, ctor, []), &set_and_check(module, &1, &2))
  end

  defp set_and_check(module, {prop, value}, object) do
    result = apply(module, String.to_atom("set_#{prop}"), [object, value])
    assert is_struct(result, module), "#{inspect(module)}.set_#{prop}/2: #{inspect(result)}"
    assert apply(module, String.to_atom("get_#{prop}"), [result]) == value
    result
  end

  defp default_constructible_modules do
    {:ok, modules} = :application.get_key(:evision, :modules)

    for module <- modules,
        String.starts_with?(Atom.to_string(module), "Elixir.Evision."),
        Code.ensure_loaded?(module),
        ctor = default_constructor(module),
        ctor != nil,
        do: {module, ctor}
  end

  defp default_constructor(module) do
    class_name = module |> Module.split() |> List.last() |> String.downcase()

    Enum.find_value(module.__info__(:functions), fn
      {name, 0} ->
        lowered = String.downcase(Atom.to_string(name))

        if (lowered == "create" or String.contains?(lowered, class_name)) and
             is_struct(apply(module, name, []), module),
           do: name

      _ ->
        nil
    end)
  end

  defp properties(module) do
    functions = module.__info__(:functions)

    for {name, 2} <- functions,
        "set_" <> prop <- [Atom.to_string(name)],
        getter = String.to_atom("get_" <> prop),
        {getter, 1} in functions,
        do: {prop, getter, name}
  end
end
