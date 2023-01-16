if !Code.ensure_loaded?(Kino.SmartCell) do
  defmodule Evision.SmartCell.ML.RTrees do
  end
else
  defmodule Evision.SmartCell.ML.RTrees do
    use Kino.JS, assets_path: "lib/assets"
    use Kino.JS.Live
    use Kino.SmartCell, name: "Evision: Random Forest"

    alias Evision.SmartCell.Helper, as: ESCH
    alias Evision.SmartCell.ML.DTrees
    alias Evision.SmartCell.ML.TrainData

    @smartcell_id "evision.ml.rtrees"

    @properties %{
      "data_from" => %{
        :type => :string,
        :opts => [must_in: ["traindata", "traindata_var"]],
        :default => "traindata"
      },
      "traindata_var" => %{
        :type => :string,
        :default => "dataset"
      },

      # RTrees
      "active_var_count" => %{
        :type => :integer,
        :opts => [minimum: 0],
        :default => 0
      },
      "calculate_var_importance" => %{
        :type => :boolean,
        :default => false
      },
      "to_variable" => %{
        :type => :string,
        :default => "rtree"
      },

      # TermCriteria
      "term_criteria_type" => %{
        :type => :string,
        :opts => [must_in: ["max_count", "eps", "max_count+eps"]],
        :default => "max_count"
      },
      "term_criteria_count" => %{
        :type => :integer,
        :opts => [minimum: 0],
        :default => 10
      },
      "term_criteria_eps" => %{
        :type => :number,
        :default => 0
      }
    }
    @inner_to_module %{
      "dtrees" => DTrees,
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

      key = "dtrees"

      fields =
        ESCH.update_key_with_module(fields, key, @inner_to_module[key], fn _, _ -> true end)

      info = [id: @smartcell_id, fields: fields]
      {:ok, assign(ctx, info)}
    end

    @impl true
    def handle_connect(ctx) do
      {:ok, %{fields: ctx.assigns.fields, id: @smartcell_id}, ctx}
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

    def to_updates(_fields, name = "data_from", key = "traindata") do
      %{name => key, key => @inner_to_module[key].defaults()}
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

    def get_quoted_code(attrs = %{"dtrees" => dtrees_attrs}) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          Evision.ML.RTrees.create()
          |> Evision.ML.RTrees.setMaxDepth(unquote(dtrees_attrs["max_depth"]))
          |> Evision.ML.RTrees.setMaxCategories(unquote(dtrees_attrs["max_categories"]))
          |> Evision.ML.RTrees.setCVFolds(unquote(dtrees_attrs["cv_folds"]))
          |> Evision.ML.RTrees.setMinSampleCount(unquote(dtrees_attrs["min_sample_count"]))
          |> Evision.ML.RTrees.setActiveVarCount(unquote(attrs["active_var_count"]))
          |> Evision.ML.RTrees.setCalculateVarImportance(
            unquote(attrs["calculate_var_importance"])
          )

        unquote(set_term_criteria(attrs))
        unquote(train_on_dataset(attrs))
      end
    end

    defp set_term_criteria(
           attrs = %{
             "term_criteria_type" => "max_count",
             "term_criteria_count" => count,
             "term_criteria_eps" => eps
           }
         ) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          Evision.ML.RTrees.setTermCriteria(
            unquote(ESCH.quoted_var(attrs["to_variable"])),
            {Evision.Constant.cv_MAX_ITER(), unquote(count), unquote(eps)}
          )
      end
    end

    defp set_term_criteria(
           attrs = %{
             "term_criteria_type" => "eps",
             "term_criteria_count" => count,
             "term_criteria_eps" => eps
           }
         ) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          Evision.ML.RTrees.setTermCriteria(
            unquote(ESCH.quoted_var(attrs["to_variable"])),
            {Evision.Constant.cv_EPS(), unquote(count), unquote(eps)}
          )
      end
    end

    defp set_term_criteria(
           attrs = %{
             "term_criteria_type" => "max_count+eps",
             "term_criteria_count" => count,
             "term_criteria_eps" => eps
           }
         ) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          Evision.ML.RTrees.setTermCriteria(
            unquote(ESCH.quoted_var(attrs["to_variable"])),
            {Evision.Constant.cv_MAX_ITER() + Evision.Constant.cv_EPS(), unquote(count),
             unquote(eps)}
          )
      end
    end

    defp train_on_dataset(%{
           "data_from" => "traindata_var",
           "traindata_var" => traindata_var,
           "to_variable" => to_variable
         }) do
      quote do
        Evision.ML.RTrees.train(
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

        Evision.ML.RTrees.train(
          unquote(ESCH.quoted_var(to_variable)),
          unquote(ESCH.quoted_var(dataset_variable))
        )

        unquote(TrainData.get_calc_error(Evision.ML.SVM, dataset_variable, to_variable))
      end
    end
  end
end
