defmodule Evision.Internal.Structurise do
  @moduledoc false

  @spec to_struct(term()) :: {:ok, term()} | {:error, String.t()} | term()
  def to_struct(any, opts \\ [])

  def to_struct({:ok, ret}, opts), do: to_struct_ok(ret, opts)

  def to_struct(ret = %{:class => module_name}, opts) when is_atom(module_name) do
    if Code.ensure_loaded?(module_name) do
      module_name.__to_struct__(ret, opts)
    else
      ret
    end
  end

  def to_struct(tuple, opts) when is_tuple(tuple) do
    Enum.map(Tuple.to_list(tuple), fn elem ->
      to_struct(elem, opts)
    end)
    |> List.to_tuple()
  end

  def to_struct(list, opts) when is_list(list) do
    Enum.map(list, fn elem ->
      to_struct(elem, opts)
    end)
  end

  def to_struct(pass_through, _opts), do: pass_through

  @spec to_struct_ok(term()) :: {:ok, term()}
  def to_struct_ok(any, opts \\ [])

  def to_struct_ok(ret = %{:class => _module_name}, opts) do
    {:ok, to_struct(ret, opts)}
  end

  def to_struct_ok(tuple, opts) when is_tuple(tuple) do
    {:ok, to_struct(tuple, opts)}
  end

  def to_struct_ok(list, opts) when is_list(list) do
    {:ok, to_struct(list, opts)}
  end

  def to_struct_ok(pass_through, _opts), do: {:ok, pass_through}

  @spec from_struct(Nx.Tensor.t()) :: {reference(), module() | nil}
  def from_struct(%Nx.Tensor{} = tensor) do
    case Evision.Mat.from_nx(tensor) do
      {:error, msg} ->
        raise RuntimeError, msg

      %Evision.Mat{ref: ref} ->
        if is_struct(tensor.data) do
          {ref, tensor.data.__struct__}
        else
          {ref, nil}
        end
    end
  end

  @spec from_struct(%{ref: reference()}) :: reference()
  def from_struct(%{ref: ref}) do
    ref
  end

  @spec from_struct(tuple()) :: tuple()
  def from_struct(tuple) when is_tuple(tuple) do
    from_struct(Tuple.to_list(tuple))
    |> List.to_tuple()
  end

  @spec from_struct(list()) :: list()
  def from_struct(list) when is_list(list) do
    if Keyword.keyword?(list) do
      Enum.map(Keyword.keys(list), fn key ->
        {key, from_struct(Keyword.fetch!(list, key))}
      end)
      |> Keyword.new()
    else
      Enum.map(list, fn elem ->
        from_struct(elem)
      end)
    end
  end

  @spec from_struct(term()) :: term()
  def from_struct(pass_through), do: pass_through
end
