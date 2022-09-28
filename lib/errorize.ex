defmodule Evision.Errorize do
  @moduledoc false

  # Original version by José
  # https://gist.github.com/josevalim/7a5ed50ed86a2260d907603ca8223448
  # modified a tiny bit by Cocoa
  defmacro deferror(fun) do
    {name, args} = Macro.decompose_call(fun)

    doc = """
    Raising version of `#{name}/#{length(args)}`.
    """

    quote do
      @doc unquote(doc)
      def unquote(:"#{name}!")(unquote_splicing(args)) do
        case unquote(fun) do
          {:ok, res} ->
            if is_struct(res) do
              res
            else
              Evision.Internal.Structurise.to_struct(res)
            end
          {:error, message} when is_list(message) -> raise List.to_string(message)
          {:error, message} when is_binary(message) -> raise message
          res when is_struct(res) -> res
          res -> Evision.Internal.Structurise.to_struct(res)
        end
      end
    end
  end
end
