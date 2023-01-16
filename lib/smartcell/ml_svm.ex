if !Code.ensure_loaded?(Kino.SmartCell) do
  defmodule Evision.SmartCell.ML.SVM do
  end
else
  defmodule Evision.SmartCell.ML.SVM do
    use Kino.JS, assets_path: "lib/assets"
    use Kino.JS.Live
    use Kino.SmartCell, name: "Evision: Support Vector Machine"

    alias Evision.SmartCell.Helper, as: ESCH
    alias Evision.SmartCell.ML.TrainData

    @smartcell_id "evision.ml.svm"

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

      # SVM
      "type" => %{
        :type => :string,
        :opts => [must_in: ["C_SVC", "NU_SVC", "ONE_CLASS", "EPS_SVR", "NU_SVR"]],
        :default => "C_SVC"
      },
      "kernel_type" => %{
        :type => :string,
        :opts => [must_in: ["LINEAR", "POLY", "RBF", "SIGMOID", "CHI2", "INTER", "CUSTOM"]],
        :default => "RBF"
      },
      "to_variable" => %{
        :type => :string,
        :default => "svm"
      },

      ## -- kernel parameter --
      ## Only used for SVM if its kernel type is one of
      ## [SVM::POLY, SVM::RBF, SVM::SIGMOID, SVM::CHI2]
      "gamma" => %{
        :type => :number,
        :default => 1
      },
      ## Only used for SVM if its kernel type is one of
      ## [SVM::POLY, SVM::SIGMOID]
      "coef0" => %{
        :type => :number,
        :default => 0
      },
      ## Only used for SVM if its kernel type is one of
      ## [SVM::POLY]
      "degree" => %{
        :type => :number,
        :default => 0
      },

      ## -- svm parameter --
      ## Only used for SVM if its type is one of
      ## [SVM::C_SVC, SVM::EPS_SVR, SVM::NU_SVR]
      "c" => %{
        :type => :number,
        :default => 1
      },
      ## Only used for SVM if its type is one of
      ## [SVM::NU_SVC, SVM::ONE_CLASS or SVM::NU_SVR]
      "nu" => %{
        :type => :number,
        :default => 0
      },
      ## Only used for SVM if its type is one of
      ## [SVM::EPS_SVR]
      "p" => %{
        :type => :number,
        :default => 0
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
          Evision.ML.SVM.create()
          |> Evision.ML.SVM.setType(
            unquote(ESCH.quoted_var("Evision.Constant.cv_#{attrs["type"]}()"))
          )
          |> Evision.ML.SVM.setKernel(
            unquote(ESCH.quoted_var("Evision.Constant.cv_#{attrs["kernel_type"]}()"))
          )

        unquote(set_svm_param(attrs))
        unquote(set_kernel_param(attrs))
        unquote(set_term_criteria(attrs))
        unquote(train_on_dataset(attrs))
      end
    end

    defp set_svm_param(attrs = %{"type" => "C_SVC"}) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          Evision.ML.SVM.setC(unquote(ESCH.quoted_var(attrs["to_variable"])), unquote(attrs["c"]))
      end
    end

    defp set_svm_param(attrs = %{"type" => "NU_SVC"}) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          Evision.ML.SVM.setNu(
            unquote(ESCH.quoted_var(attrs["to_variable"])),
            unquote(attrs["nu"])
          )
      end
    end

    defp set_svm_param(attrs = %{"type" => "ONE_CLASS"}) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          Evision.ML.SVM.setNu(
            unquote(ESCH.quoted_var(attrs["to_variable"])),
            unquote(attrs["nu"])
          )
      end
    end

    defp set_svm_param(attrs = %{"type" => "EPS_SVR"}) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          unquote(ESCH.quoted_var(attrs["to_variable"]))
          |> Evision.ML.SVM.setC(unquote(attrs["c"]))
          |> Evision.ML.SVM.setP(unquote(attrs["p"]))
      end
    end

    defp set_svm_param(attrs = %{"type" => "NU_SVR"}) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          unquote(ESCH.quoted_var(attrs["to_variable"]))
          |> Evision.ML.SVM.setC(unquote(attrs["c"]))
          |> Evision.ML.SVM.setNu(unquote(attrs["nu"]))
      end
    end

    defp set_svm_param(_) do
    end

    defp set_kernel_param(attrs = %{"kernel_type" => "POLY"}) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          unquote(ESCH.quoted_var(attrs["to_variable"]))
          |> Evision.ML.SVM.setGamma(unquote(attrs["gamma"]))
          |> Evision.ML.SVM.setCoef0(unquote(attrs["coef0"]))
          |> Evision.ML.SVM.setDegree(unquote(attrs["degree"]))
      end
    end

    defp set_kernel_param(attrs = %{"kernel_type" => "RBF"}) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          Evision.ML.SVM.setGamma(
            unquote(ESCH.quoted_var(attrs["to_variable"])),
            unquote(attrs["gamma"])
          )
      end
    end

    defp set_kernel_param(attrs = %{"kernel_type" => "SIGMOID"}) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          unquote(ESCH.quoted_var(attrs["to_variable"]))
          |> Evision.ML.SVM.setGamma(unquote(attrs["gamma"]))
          |> Evision.ML.SVM.setCoef0(unquote(attrs["coef0"]))
      end
    end

    defp set_kernel_param(attrs = %{"kernel_type" => "CHI2"}) do
      quote do
        unquote(ESCH.quoted_var(attrs["to_variable"])) =
          Evision.ML.SVM.setGamma(
            unquote(ESCH.quoted_var(attrs["to_variable"])),
            unquote(attrs["gamma"])
          )
      end
    end

    defp set_kernel_param(_) do
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
          Evision.ML.SVM.setTermCriteria(
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
          Evision.ML.SVM.setTermCriteria(
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
          Evision.ML.SVM.setTermCriteria(
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
        Evision.ML.SVM.train(
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

        Evision.ML.SVM.train(
          unquote(ESCH.quoted_var(to_variable)),
          unquote(ESCH.quoted_var(dataset_variable))
        )

        unquote(TrainData.get_calc_error(Evision.ML.SVM, dataset_variable, to_variable))
      end
    end
  end
end
