defmodule Evision.Internal.Structurise do
  def to_struct(any)

  def to_struct({:ok, ret}), do: to_struct_ok(ret)

  def to_struct(mat = %{:class => :Mat}) do
    Evision.Mat.__make_struct__(mat)
  end

  def to_struct(cap = %{:class => :VideoCapture}) do
    Evision.VideoCapture.__make_struct__(cap)
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

  def to_struct_ok(mat = %{:class => :Mat}) do
    {:ok, to_struct(mat)}
  end

  def to_struct_ok(cap = %{:class => :VideoCapture}) do
    {:ok, to_struct(cap)}
  end

  def to_struct_ok(tuple) when is_tuple(tuple) do
    {:ok, to_struct(tuple)}
  end

  def to_struct_ok(list) when is_list(list) do
    {:ok, to_struct(list)}
  end

  def to_struct_ok(pass_through), do: {:ok, pass_through}

  def from_struct(%Evision.Mat{ref: ref}) do
    ref
  end

  def from_struct(%Nx.Tensor{}=tensor) do
    %Evision.Mat{ref: ref} = Evision.Nx.to_mat!(tensor)
    ref
  end

  def from_struct(%{ref: ref}) do
    ref
  end

  def from_struct(tuple) when is_tuple(tuple) do
    from_struct(Tuple.to_list(tuple))
    |> List.to_tuple()
  end

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

  def from_struct(pass_through), do: pass_through
end
