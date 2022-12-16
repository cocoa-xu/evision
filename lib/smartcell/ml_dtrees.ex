if !Code.ensure_loaded?(Kino.SmartCell) do
  defmodule Evision.SmartCell.ML.DTrees do
  end
else
  defmodule Evision.SmartCell.ML.DTrees do
    use Kino.JS, assets_path: "lib/assets"
    use Kino.JS.Live
    use Kino.SmartCell, name: "Evision: Decision Tree"

    alias Evision.SmartCell.Helper, as: ESCH
    alias Evision.SmartCell.ML.TrainData

    @smartcell_id "evision.ml.dtrees"

    @properties %{
      "data_from" => %{
        :type => :string,
        :opts => [must_in: ["traindata_var", "traindata"]],
        :default => "traindata_var"
      },
      "traindata_var" => %{
        :type => :string,
        :default => "dataset"
      },

      # DTrees
      "max_depth" => %{
        :type => :integer,
        :opts => [minimum: 1],
        :default => 4
      },
      "max_categories" => %{
        :type => :integer,
        :opts => [minimum: 2],
        :default => 2
      },
      "min_sample_count" => %{
        :type => :integer,
        :opts => [minimum: 1],
        :default => 10
      },
      "cv_folds" => %{
        :type => :integer,
        :opts => [minimum: 0],
        :default => 0
      },
      "to_variable" => %{
        :type => :string,
        :default => "dtree"
      }
    }
    @inner_to_module %{
      "traindata" => TrainData
    }

    @spec id :: String.t()
    def id, do: @smartcell_id

    @spec properties :: map()
    def properties, do: @properties

    @spec defaults :: map()
    def defaults do
      Map.new(
        Enum.map(@properties, fn {field, field_specs} ->
          {field, field_specs[:default]}
        end)
      )
    end

    @impl true
    def init(attrs, ctx) do
      # load from file or fill empty entries with default values
      fields =
        Map.new(
          Enum.map(@properties, fn {field, field_specs} ->
            {field, attrs[field] || field_specs[:default]}
          end)
        )

      # traindata
      key = "traindata"

      fields =
        ESCH.update_key_with_module(fields, key, @inner_to_module[key], fn fields, key ->
          fields["data_from"] == key
        end)

      info = [id: @smartcell_id, fields: fields]
      {:ok, assign(ctx, info)}
    end

    @impl true
    def handle_connect(ctx) do
      {:ok, %{id: ctx.assigns.id, fields: ctx.assigns.fields}, ctx}
    end

    @impl true
    def handle_event("update_field", %{"field" => field, "value" => value}, ctx) do
      updated_fields =
        case String.split(field, ".", parts: 2) do
          [inner, forward] ->
            ESCH.to_inner_updates(inner, @inner_to_module[inner], forward, value, ctx)

          [field] ->
            to_updates(ctx.assigns.fields, field, value)
        end

      ctx = update(ctx, :fields, &Map.merge(&1, updated_fields))
      broadcast_event(ctx, "update", %{"fields" => updated_fields})
      {:noreply, ctx}
    end

    def to_updates(_fields, name = "data_from", value) do
      property = @properties[name]
      fields = %{name => ESCH.to_update(value, property[:type], Access.get(property, :opts))}

      key = "traindata"

      ESCH.update_key_with_module(fields, key, @inner_to_module[key], fn fields, key ->
        fields["data_from"] == key
      end)
    end

    def to_updates(_fields, name, value) do
      property = @properties[name]
      %{name => ESCH.to_update(value, property[:type], Access.get(property, :opts))}
    end

    @impl true
    def to_attrs(%{assigns: %{fields: fields}}) do
      fields
    end

    @impl true
    def to_source(attrs) do
      get_quoted_code(attrs)
      |> Kino.SmartCell.quoted_to_string()
    end

    def get_quoted_code(attrs) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          Evision.ML.DTrees.create()
          |> Evision.ML.DTrees.setMaxDepth(unquote(attrs["max_depth"]))
          |> Evision.ML.DTrees.setMaxCategories(unquote(attrs["max_categories"]))
          |> Evision.ML.DTrees.setCVFolds(unquote(attrs["cv_folds"]))
          |> Evision.ML.DTrees.setMinSampleCount(unquote(attrs["min_sample_count"]))

        unquote(train_on_dataset(attrs))
      end
    end

    defp train_on_dataset(%{
           "data_from" => "traindata_var",
           "traindata_var" => traindata_var,
           "to_variable" => to_variable
         }) do
      quote do
        Evision.ML.DTrees.train(
          unquote(ESCH.quoted_var(to_variable)),
          unquote(ESCH.quoted_var(traindata_var))
        )

        unquote(TrainData.get_calc_error(Evision.ML.SVM, traindata_var, to_variable))
      end
    end

    defp train_on_dataset(%{
           "data_from" => "traindata",
           "traindata" => traindata_attrs,
           "to_variable" => to_variable
         }) do
      dataset_variable = traindata_attrs["to_variable"]

      quote do
        unquote(TrainData.get_quoted_code(traindata_attrs))

        Evision.ML.DTrees.train(
          unquote(ESCH.quoted_var(to_variable)),
          unquote(ESCH.quoted_var(dataset_variable))
        )

        unquote(TrainData.get_calc_error(Evision.ML.SVM, dataset_variable, to_variable))
      end
    end
  end
end
