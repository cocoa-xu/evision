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
          object = apply(module, ctor, []),
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
