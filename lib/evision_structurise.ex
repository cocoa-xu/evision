defmodule Evision.Internal.Structurise do
  @moduledoc false

  @spec to_struct(term()) :: {:ok, term()} | {:error, String.t()} | term()
  def to_struct(any)

  def to_struct({:ok, ret}), do: to_struct_ok(ret)

  def to_struct(ret = %{:class => module_name}) when is_atom(module_name) do
    if Code.ensure_loaded?(module_name) do
      module_name.to_struct(ret)
    else
      ret
    end
  end

  def to_struct(tuple) when is_tuple(tuple) do
    Enum.map(Tuple.to_list(tuple), fn elem ->
      to_struct(elem)
    end)
    |> List.to_tuple()
  end

  def to_struct(list) when is_list(list) do
    Enum.map(list, fn elem ->
      to_struct(elem)
    end)
  end

  def to_struct(pass_through), do: pass_through

  @spec to_struct_ok(term()) :: {:ok, term()}
  def to_struct_ok(any)

  def to_struct_ok(ret = %{:class => _module_name}) do
    {:ok, to_struct(ret)}
  end

  def to_struct_ok(tuple) when is_tuple(tuple) do
    {:ok, to_struct(tuple)}
  end

  def to_struct_ok(list) when is_list(list) do
    {:ok, to_struct(list)}
  end

  def to_struct_ok(pass_through), do: {:ok, pass_through}

  def to_compact_type({:u, 8}), do: :u8
  def to_compact_type({:u, 16}), do: :u16
  def to_compact_type({:u, 32}), do: :u32
  def to_compact_type({:u, 64}), do: :u64
  def to_compact_type({:s, 8}), do: :s8
  def to_compact_type({:s, 16}), do: :s16
  def to_compact_type({:s, 32}), do: :s32
  def to_compact_type({:s, 64}), do: :s64
  def to_compact_type({:f, 16}), do: :f16
  def to_compact_type({:f, 32}), do: :f32
  def to_compact_type({:f, 64}), do: :f64
  def to_compact_type({:c, 32}), do: :c32
  def to_compact_type({:c, 64}), do: :c64

  def to_compact_type(atom) when is_atom(atom) do
    atom
  end

  # specialised for Evision.Backend
  @spec from_struct(Nx.Tensor.t()) :: reference()
  def from_struct(%Nx.Tensor{data: %Evision.Backend{ref: %Evision.Mat{ref: ref}}}) do
    ref
  end

  # for generic Nx.Tensor
  @spec from_struct(Nx.Tensor.t()) :: reference()
  def from_struct(%Nx.Tensor{} = tensor) do
    %{
      tensor
      | data: Nx.to_binary(tensor),
        type: to_compact_type(tensor.type),
        __struct__: :nx_tensor
    }
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
