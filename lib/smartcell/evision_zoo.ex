if Code.ensure_loaded?(Kino.SmartCell) do
  defmodule Evision.SmartCell.Zoo do
    @moduledoc false

    use Kino.JS, assets_path: "lib/assets"
    use Kino.JS.Live
    use Kino.SmartCell, name: "Evision: OpenCV Model Zoo"

    @smartcell_id "evision.zoo"

    @spec id :: String.t()
    def id, do: @smartcell_id

    @type tasks :: %{String.t() => module()}
    @tasks %{
      Evision.Zoo.FaceDetection.id() => Evision.Zoo.FaceDetection,
      Evision.Zoo.ImageSegmentation.id() => Evision.Zoo.ImageSegmentation,
      Evision.Zoo.ImageClassification.id() => Evision.Zoo.ImageClassification,
      Evision.Zoo.FaceRecognition.id() => Evision.Zoo.FaceRecognition,
      Evision.Zoo.TextDetection.id() => Evision.Zoo.TextDetection,
      Evision.Zoo.TextRecognition.id() => Evision.Zoo.TextRecognition
    }
    @spec tasks :: tasks()
    def tasks, do: @tasks

    @spec tasks_list :: [%{id: String.t(), label: String.t(), variants: [%{}]}, ...]
    def tasks_list, do: Enum.map(Map.values(@tasks), fn task -> task.smartcell_tasks() end)

    @spec default_task_id :: String.t()
    def default_task_id do
      get_in(tasks_list(), [Access.at!(0), :id])
    end

    @spec default_variant_id :: String.t()
    def default_variant_id do
      get_in(tasks_list(), [Access.at!(0), :variants, Access.at!(0), :id])
    end

    @impl true
    def init(attrs, ctx) do
      task_id = attrs["task_id"] || default_task_id()
      variant_id = attrs["variant_id"] || default_variant_id()

      fields = %{
        "task_id" => task_id,
        "variant_id" => variant_id,
        "backend" => attrs["backend"] || "opencv",
        "target" => attrs["target"] || "cpu"
      }

      fields =
        for {field, default} <- field_defaults_for(task_id, variant_id),
            into: fields,
            do: {field, attrs[field] || default}

      {:ok,
       assign(ctx,
         id: @smartcell_id,
         fields: fields
       )}
    end

    defp field_defaults_for(task_id, variant_id) do
      variant = variant_by_id(task_id, variant_id)

      if variant == nil do
        %{}
      else
        for param <- variant.params, pparam <- param.params, into: %{} do
          {pparam.field, pparam.default}
        end
      end
    end

    @impl true
    def handle_connect(ctx) do
      {backend_options, target_options} = Evision.Zoo.available_backend_and_target()

      {:ok,
       %{
         id: ctx.assigns.id,
         fields: ctx.assigns.fields,
         tasks: tasks_list(),
         backend_options: backend_options,
         target_options: target_options
       }, ctx}
    end

    @impl true
    def handle_event("update_field", %{"field" => "task_id", "value" => task_id}, ctx) do
      task = task_by_id(task_id)
      variant_id = hd(task.variants).id
      param_fields = field_defaults_for(task_id, variant_id)

      fields =
        Map.merge(
          %{
            "task_id" => task_id,
            "variant_id" => variant_id
          },
          param_fields
        )

      ctx = assign(ctx, fields: fields)

      broadcast_event(ctx, "update", %{"fields" => fields})

      {:noreply, ctx}
    end

    def handle_event("update_field", %{"field" => "variant_id", "value" => variant_id}, ctx) do
      task_id = ctx.assigns.fields["task_id"]
      param_fields = field_defaults_for(task_id, variant_id)

      fields =
        Map.merge(
          Map.merge(param_fields, ctx.assigns.fields),
          %{
            "task_id" => task_id,
            "variant_id" => variant_id
          }
        )

      ctx = assign(ctx, fields: fields)

      broadcast_event(ctx, "update", %{"fields" => fields})

      {:noreply, ctx}
    end

    def handle_event("update_field", %{"field" => "backend", "value" => backend}, ctx) do
      task_id = ctx.assigns.fields["task_id"]
      variant_id = ctx.assigns.fields["variant_id"]
      param_fields = field_defaults_for(task_id, variant_id)

      fields =
        Map.merge(
          %{
            "backend" => backend,
            "task_id" => task_id,
            "variant_id" => variant_id
          },
          param_fields
        )

      ctx = assign(ctx, fields: fields)

      broadcast_event(ctx, "update", %{"fields" => fields})

      {:noreply, ctx}
    end

    def handle_event("update_field", %{"field" => "target", "value" => target}, ctx) do
      task_id = ctx.assigns.fields["task_id"]
      variant_id = ctx.assigns.fields["variant_id"]

      param_fields = field_defaults_for(task_id, variant_id)

      fields =
        Map.merge(
          %{
            "target" => target,
            "task_id" => task_id,
            "variant_id" => variant_id
          },
          param_fields
        )

      ctx = assign(ctx, fields: fields)

      broadcast_event(ctx, "update", %{"fields" => fields})

      {:noreply, ctx}
    end

    def handle_event("update_field", %{"field" => field, "value" => value}, ctx) do
      current_task_id = ctx.assigns.fields["task_id"]
      current_variant_id = ctx.assigns.fields["variant_id"]

      type =
        Enum.find_value(tasks_list(), fn task ->
          task.id == current_task_id &&
            Enum.find_value(task[:variants], fn variant ->
              variant.id == current_variant_id &&
                Enum.find_value(variant.params, fn param ->
                  Enum.find_value(param.params, fn pparam ->
                    pparam.field == field && pparam.type
                  end)
                end)
            end)
        end)

      updated_fields = to_updates(field, value, type)
      ctx = update(ctx, :fields, &Map.merge(&1, updated_fields))

      broadcast_event(ctx, "update", %{"fields" => updated_fields})

      {:noreply, ctx}
    end

    defp to_updates(field, value, type), do: %{field => parse_value(value, type)}

    defp parse_value("", _type), do: nil
    defp parse_value(value, :number), do: String.to_integer(value)
    defp parse_value(value, :float), do: String.to_float(value)

    defp parse_value(value, :string) do
      if is_atom(value) do
        Atom.to_string(value)
      else
        value
      end
    end

    defp parse_value(value, _type), do: value

    @impl true
    def to_attrs(ctx) do
      ctx.assigns.fields
    end

    @impl true
    def to_source(attrs) do
      for quoted <- to_quoted(attrs), do: Kino.SmartCell.quoted_to_string(quoted)
    end

    defp to_quoted(%{"task_id" => task_id} = attrs) do
      task = Map.get(tasks(), task_id)

      if task do
        task.to_quoted(attrs)
      else
        []
      end
    end

    defp to_quoted(_) do
      []
    end

    defp task_by_id(task_id) do
      Enum.find(tasks_list(), &(&1.id == task_id))
    end

    defp variant_by_id(task_id, variant_id) do
      task = task_by_id(task_id)
      Enum.find(task[:variants], &(&1.id == variant_id))
    end
  end
end
