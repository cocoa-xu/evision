defmodule Evision.SmartCell.Helper do
  @moduledoc false

  def quoted_var(nil), do: nil
  def quoted_var(string), do: {String.to_atom(string), [], nil}

  def to_update(value, type, opts \\ [])

  def to_update(value, :string, opts) do
    must_in = opts[:must_in] || [value]

    if Enum.member?(must_in, value) do
      value
    else
      nil
    end
  end

  def to_update(value, :boolean, _opts) do
    if is_binary(value) do
      case value do
        "true" -> true
        "false" -> false
        _ -> nil
      end
    else
      if is_boolean(value) do
        value
      else
        nil
      end
    end
  end

  def to_update(value, :integer, opts) do
    minimum = opts[:minimum]
    maximum = opts[:maximum]

    case Integer.parse(value) do
      {n, ""} ->
        n =
          if minimum != nil and n < minimum do
            minimum
          else
            n
          end

        if maximum != nil and n > maximum do
          maximum
        else
          n
        end

      _ ->
        nil
    end
  end

  def to_update(value, :number, opts) do
    minimum = opts[:minimum]
    maximum = opts[:maximum]

    case Float.parse(value) do
      {n, ""} ->
        n =
          if minimum != nil and n < minimum do
            minimum
          else
            n
          end

        if maximum != nil and n > maximum do
          maximum
        else
          n
        end

      _ ->
        nil
    end
  end

  @evision_types [:f32, :f64, :u8, :u16, :s8, :s16, :s32]
  def to_update(value, :type, opts) do
    allowed_types = opts[:allowed_types] || @evision_types

    if Enum.member?(allowed_types, String.to_atom(value)) do
      value
    else
      nil
    end
  end

  def update_key_with_module(fields, key, module, conflicit \\ :loaded, on_condition) do
    if on_condition.(fields, key) do
      fill_or_merge_defaults(fields, key, module, conflicit)
    else
      fields
    end
  end

  def fill_or_merge_defaults(fields, key, module, conflicit \\ :loaded) do
    if fields[key] == nil do
      Map.put(fields, key, module.defaults())
    else
      merge_properties(fields[key], module.defaults(), conflicit)
    end
  end

  def to_inner_updates(inner_name, inner_module, field, value, ctx) do
    inner_fields = ctx.assigns.fields[inner_name]
    updated_fields = inner_module.to_updates(inner_fields, field, value)
    %{inner_name => Map.merge(inner_fields, updated_fields)}
  end

  def merge_properties(loaded, default, conflicit \\ :loaded) do
    Map.merge(loaded, default, fn _key, loaded_value, default_specs ->
      case conflicit do
        :loaded ->
          if loaded_value != nil do
            loaded_value
          else
            default_specs[:default]
          end

        :default ->
          default_specs[:default]
      end
    end)
  end
end
