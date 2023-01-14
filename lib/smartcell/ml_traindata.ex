if !Code.ensure_loaded?(Kino.SmartCell) do
  defmodule Evision.SmartCell.ML.TrainData do
  end
else
  defmodule Evision.SmartCell.ML.TrainData do
    use Kino.JS, assets_path: "lib/assets"
    use Kino.JS.Live
    use Kino.SmartCell, name: "Evision: Train Data"

    alias Evision.SmartCell.Helper, as: ESCH

    @smartcell_id "evision.ml.traindata"

    @properties %{
      "x" => %{
        :type => :string
      },
      "x_type" => %{
        :type => :string,
        :opts => [must_in: ["s32", "f32"]],
        :default => "f32"
      },
      "y" => %{
        :type => :string
      },
      "y_type" => %{
        :type => :string,
        :opts => [must_in: ["s32", "f32"]],
        :default => "s32"
      },
      "data_layout" => %{
        :type => :string,
        :opts => [must_in: ["row", "col"]],
        :default => "row"
      },
      "split_ratio" => %{
        :type => :number,
        :opts => [minimum: 0.0, maximum: 1.0],
        :default => 0.8
      },
      "shuffle_dataset" => %{
        :type => :boolean,
        :default => true
      },
      "to_variable" => %{
        :type => :string,
        :default => "dataset"
      }
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
      fields =
        Enum.map(@properties, fn {field, field_specs} ->
          {field, attrs[field] || field_specs[:default]}
        end)

      {:ok, assign(ctx, fields: Map.new(fields), id: @smartcell_id)}
    end

    @impl true
    def handle_connect(ctx) do
      {:ok, %{fields: ctx.assigns.fields, id: @smartcell_id}, ctx}
    end

    @impl true
    def handle_event("update_field", %{"field" => field, "value" => value}, ctx) do
      updated_fields = to_updates(ctx.assigns.fields, field, value)
      ctx = update(ctx, :fields, &Map.merge(&1, updated_fields))
      broadcast_event(ctx, "update", %{"fields" => updated_fields})
      {:noreply, ctx}
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
          Evision.ML.TrainData.create(
            Evision.Mat.from_nx(
              Nx.tensor(unquote(ESCH.quoted_var(attrs["x"])),
                type: unquote(String.to_atom(attrs["x_type"])),
                backend: Evision.Backend
              )
            ),
            unquote(data_layout(attrs["data_layout"])),
            Evision.Mat.from_nx(
              Nx.tensor(unquote(ESCH.quoted_var(attrs["y"])),
                type: unquote(String.to_atom(attrs["y_type"])),
                backend: Evision.Backend
              )
            )
          )
          |> Evision.ML.TrainData.setTrainTestSplitRatio(unquote(attrs["split_ratio"]),
            shuffle: unquote(attrs["shuffle_dataset"])
          )

        IO.puts(
          "#Samples: #{Evision.ML.TrainData.getNSamples(unquote(ESCH.quoted_var(attrs["to_variable"])))}"
        )

        IO.puts(
          "#Training samples: #{Evision.ML.TrainData.getNTrainSamples(unquote(ESCH.quoted_var(attrs["to_variable"])))}"
        )

        IO.puts(
          "#Test samples: #{Evision.ML.TrainData.getNTestSamples(unquote(ESCH.quoted_var(attrs["to_variable"])))}"
        )
      end
    end

    def data_layout("row") do
      quote do
        Evision.Constant.cv_ROW_SAMPLE()
      end
    end

    def data_layout("col") do
      quote do
        Evision.Constant.cv_COL_SAMPLE()
      end
    end

    def get_calc_error(module, traindata_var, to_variable) do
      quote do
        unquote(ESCH.quoted_var(to_variable))
        |> unquote(module).calcError(unquote(ESCH.quoted_var(traindata_var)), false)
        |> then(fn r ->
          case r do
            {:error, error_message} ->
              raise error_message

            {error, _} ->
              IO.puts("Training Error: #{error}")
          end
        end)

        unquote(ESCH.quoted_var(to_variable))
        |> unquote(module).calcError(unquote(ESCH.quoted_var(traindata_var)), true)
        |> then(fn r ->
          case r do
            {:error, error_message} ->
              raise error_message

            {error, _} ->
              IO.puts("Test Error: #{error}")
          end
        end)
      end
    end
  end
end
