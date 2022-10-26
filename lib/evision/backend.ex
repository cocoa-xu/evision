defmodule Evision.Backend do
  @behaviour Nx.Backend
  defstruct [:ref]

  alias Nx.Tensor, as: T
  alias Evision.Backend, as: EB

  @dialyzer :no_contracts

  @spec reject_error(Evision.Mat.maybe_mat_out()) :: Evision.Mat.t()
  defp reject_error(maybe_error) do
    case maybe_error do
      {:error, message} ->
        raise RuntimeError, message
      mat ->
        Evision.Internal.Structurise.to_struct(mat)
    end
  end

  ## Creation

  @impl true
  @doc """
  Tensor with constant values.

  ## Example

      Nx.tensor(1.0, backend: Evision.Backend)
      #Nx.Tensor<
        f32
        Evision.Backend
        1.0
      >

  """
  @spec constant(Nx.Tensor.t(), number(), any()) :: Nx.Tensor.t()
  def constant(%T{shape: {}, type: type} = out, scalar, _backend_options) do
    Evision.Mat.number(scalar, type)
    |> reject_error()
    |> to_nx(out)
  end

  @spec constant(Nx.Tensor.t(), number(), any()) :: Nx.Tensor.t()
  def constant(%T{shape: shape, type: type} = out, scalar, _backend_options) do
    Evision.Mat.full(shape, scalar, type)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec from_binary(Nx.Tensor.t(), binary(), any()) :: Nx.Tensor.t()
  def from_binary(%T{shape: shape, type: type} = out, binary, _backend_options) when is_binary(binary) do
    Evision.Mat.from_binary_by_shape(binary, type, shape)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec eye(Nx.Tensor.t(), any()) :: Nx.Tensor.t()
  def eye(%T{shape: {n, n}, type: type} = out, _backend_options) do
    Evision.Mat.eye(n, type)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec iota(Nx.Tensor.t(), nil | non_neg_integer(), any()) :: Nx.Tensor.t()
  def iota(%T{shape: {}, type: type} = out, nil, _backend_options) do
    Evision.Mat.arange(0, 1, 1, type)
    |> reject_error()
    |> to_nx(out)
  end

  def iota(%T{shape: shape, type: type} = out, nil, _backend_options) do
    Evision.Mat.arange(
      0,
      Nx.size(shape),
      1,
      type,
      shape
    )
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  def iota(%T{shape: {n}, type: type} = out, 0, _backend_options) do
    Evision.Mat.arange(0, n, 1, type)
    |> reject_error()
    |> to_nx(out)
  end

  def iota(%T{shape: shape, type: type} = out, axis, _backend_options) do
    # gets the size of iota
    dim = elem(shape, axis)

    # build the iota in one dimension
    aten = reject_error(Evision.Mat.arange(0, dim, 1, type))

    # reshape the tensor above to be have shape where everything is 1, except for dim
    reshape = Tuple.duplicate(1, Nx.rank(shape)) |> put_elem(axis, dim)
    aten = reject_error(Evision.Mat.reshape(aten, reshape))

    # Now broadcast the tensor using the original shape
    Evision.Mat.broadcast_to(aten, shape)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec random_uniform(Nx.Tensor.t(), number(), number(), any()) :: Nx.Tensor.t()
  def random_uniform(%T{type: {s, _} = type, shape: shape} = out, min, max, _backend_options)
      when s in [:u, :s, :f] do
    min = to_number(min)
    max = to_number(max)

    Evision.randu(
      reject_error(Evision.Mat.zeros(shape, type)),
      reject_error(Evision.Mat.number(min, type)),
      reject_error(Evision.Mat.number(max, type))
    )
    |> reject_error()
    |> Evision.Mat.as_type(type)
    |> reject_error()
    |> to_nx(out)
  end

  def random_uniform(%T{type: {:c, _}, shape: _shape} = _out, _min, _max, _backend_options) do
    raise ArgumentError, "Complex number is not support yet"
  end

  @impl true
  @spec random_normal(Nx.Tensor.t(), number | Nx.Tensor.t(), number | Nx.Tensor.t(), any) ::
          Nx.Tensor.t()
  def random_normal(%T{type: {s, _} = type, shape: shape} = out, mu, sigma, _backend_options)
      when s in [:u, :s, :f] do
    mu = to_number(mu)
    sigma = to_number(sigma)

    Evision.randn(
      reject_error(Evision.Mat.zeros(shape, type)),
      reject_error(Evision.Mat.number(mu, type)),
      reject_error(Evision.Mat.number(sigma, type))
    )
    |> reject_error()
    |> Evision.Mat.as_type(type)
    |> reject_error()
    |> to_nx(out)
  end

  def random_normal(%T{type: {:c, _}, shape: _shape} = _out, _min, _max, _backend_options) do
    raise ArgumentError, "Complex number is not support yet"
  end

  @impl true
  @spec backend_copy(Nx.Tensor.t(), atom, any) :: Nx.Tensor.t()
  def backend_copy(tensor, Nx.Tensor, opts) do
    backend_copy(tensor, Nx.BinaryBackend, opts)
  end

  def backend_copy(tensor, Evision.Backend, _opts) do
    Evision.Mat.clone(from_nx(tensor))
    |> reject_error()
    |> to_nx(tensor)
  end

  def backend_copy(tensor, backend, opts) do
    case Evision.Mat.to_binary(from_nx(tensor), 0) do
      {:error, msg} ->
        raise RuntimeError, msg
      binary ->
        backend.from_binary(tensor, binary, opts)
    end
  end

  @impl true
  @spec backend_transfer(Nx.Tensor.t(), atom, any) :: Nx.Tensor.t()
  def backend_transfer(tensor, backend, opts) do
    backend_copy(tensor, backend, opts)
  end

  @impl true
  def backend_deallocate(_tensor) do
    :ok
  end

  @impl true
  def to_batched(out, %T{shape: shape} = tensor, opts) do
    leftover = opts[:leftover]
    batch_size = elem(out.shape, 0)
    Evision.Mat.to_batched(from_nx(tensor), batch_size, shape, leftover: leftover)
    |> Enum.map(&to_nx(&1, out))
  end

  @impl true
  @spec to_binary(Nx.Tensor.t(), non_neg_integer) :: binary | {:error, binary}
  def to_binary(%T{data: %EB{ref: mat}}, limit) when is_struct(mat, Evision.Mat) and is_integer(limit) and limit >= 0 do
    Evision.Mat.to_binary(mat, limit)
  end

  @impl true
  @spec inspect(Nx.Tensor.t(), Inspect.Opts.t()) :: String.t()
  def inspect(%T{data: %EB{ref: mat}} = tensor, inspect_opts) do
    limit = if inspect_opts.limit == :infinity, do: :infinity, else: inspect_opts.limit + 1

    Evision.Mat.to_binary(mat, min(limit, Nx.size(tensor)))
    |> then(&Nx.Backend.inspect(tensor, &1, inspect_opts))
    |> maybe_add_signature(tensor)
  end

  @impl true
  @spec as_type(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def as_type(%T{type: type} = out, %T{data: %EB{ref: mat}}) do
    Evision.Mat.as_type(mat, type)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec bitcast(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def bitcast(out, tensor) do
    case Evision.Mat.to_binary(from_nx(tensor), 0) do
      {:error, msg} ->
        raise RuntimeError, msg
      binary ->
        from_binary(out, binary, [])
    end
  end

  @impl true
  @spec reshape(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def reshape(%T{shape: shape} = out, %T{data: %EB{ref: mat}}) do
    Evision.Mat.reshape(mat, Tuple.to_list(shape))
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec squeeze(Nx.Tensor.t(), Nx.Tensor.t(), term()) :: Nx.Tensor.t()
  def squeeze(out, %T{data: %EB{ref: mat}}, _axes) do
    Evision.Mat.squeeze(mat)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec broadcast(Nx.Tensor.t(), Nx.Tensor.t(), tuple, any) :: Nx.Tensor.t()
  def broadcast(out, %T{} = t, shape, axes) do
    {tensor, reshape} = maybe_reshape(t, shape, axes)
    Evision.Mat.broadcast_to(tensor |> from_nx(), shape, reshape)
    |> reject_error()
    |> to_nx(out)
  end

  defp maybe_reshape(%T{shape: {n}} = t, {n, _}, [0]) do
    {Nx.reshape(t, {n, 1}), {n, 1}}
  end

  defp maybe_reshape(%T{shape: shape} = t, to_shape, _axes) do
    l_shape = Tuple.to_list(shape)
    l_to_shape = Tuple.to_list(to_shape)

    if length(l_shape) != length(l_to_shape) do
      src_shape_axis = length(l_shape) - 1
      {_src_shape_axis, force_shape} =
        for axis <- Enum.to_list(length(l_to_shape)-1..0), reduce: {src_shape_axis, []} do
          {src_shape_axis, acc} ->
            if src_shape_axis == -1 do
              {src_shape_axis, [1 | acc]}
            else
              case {elem(shape, src_shape_axis), elem(to_shape, axis)} do
                {1, _d} ->
                  {src_shape_axis - 1, [1 | acc]}
                {d, d} ->
                  {src_shape_axis - 1, [d | acc]}
              end
            end
        end
      force_shape = List.to_tuple(force_shape)

      {Nx.reshape(t, force_shape), force_shape}
    else
      {t, shape}
    end
  end

  @impl true
  @spec dot(Nx.Tensor.t(), Nx.Tensor.t(), any, [], Nx.Tensor.t(), any, []) :: Nx.Tensor.t()
  def dot(
    %T{type: out_type} = out,
    a, _left_axes, [],
    b, _right_axes, []) do
    Evision.Mat.matrix_multiply(from_nx(a), from_nx(b), out_type)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec clip(Nx.Tensor.t(), Nx.Tensor.t(), Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def clip(%T{} = out, %T{} = t, %T{} = min, %T{} = max) do
    t
    |> Nx.as_type(out.type)
    |> from_nx()
    |> Evision.Mat.clip(to_number(min), to_number(max))
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec transpose(Nx.Tensor.t(), Nx.Tensor.t(), [integer]) :: Nx.Tensor.t()
  def transpose(out, %T{shape: shape} = tensor, axes) do
    Evision.Mat.transpose(from_nx(tensor), axes, as_shape: shape)
    |> to_nx(out)
  end

  @impl true
  def pad(_out, tensor, _constant, config) do
    case tensor.shape do
      {} ->
        tensor
      {_} ->
        [{_edge_low, _edge_high, _interior}] = config
      _ ->
        permutation = for i <- 0..(Nx.rank(tensor) - 2), do: i
        _permutation = [Nx.rank(tensor) - 1 | permutation]
    end
    raise RuntimeError, "not implemented yet"
  end

  @impl true
  @spec add(Nx.Tensor.t(), Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def add(%T{type: type, shape: out_shape}=out, l, r) do
    {l, r} = enforce_same_shape(l, r, out_shape)
    Evision.Mat.add(from_nx(l), from_nx(r), type)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec subtract(Nx.Tensor.t(), Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def subtract(%T{type: type, shape: out_shape}=out, l, r) do
    {l, r} = enforce_same_shape(l, r, out_shape)
    Evision.Mat.subtract(from_nx(l), from_nx(r), type)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  def multiply(%T{type: type}=out, %T{}=l, %T{}=r) do
    case check_mul_div_kind(l, r) do
      :per_element ->
        Evision.Mat.multiply(from_nx(l), from_nx(r), type)

      :matrix ->
        l = maybe_cast_type_for_matrix_mul_div(l)
        r = maybe_cast_type_for_matrix_mul_div(r)
        Evision.Mat.matrix_multiply(l, r)
        |> Evision.Mat.as_type(type)
    end
    |> to_nx(out)
  end

  @impl true
  def divide(%T{type: type, shape: shape}=out, l, r) do
    case check_mul_div_kind(l, r) do
      :per_element ->
        Evision.Mat.divide(from_nx(l), from_nx(r), type)

      :matrix ->
        l = Nx.broadcast(l, shape)
        r = Nx.broadcast(r, shape)
        l = maybe_cast_type_for_matrix_mul_div(l)
        r = maybe_cast_type_for_matrix_mul_div(r)
        Evision.Mat.divide(l, r, type)
    end
    |> to_nx(out)
  end

  defp check_mul_div_kind(%T{shape: {}}, _r) do
    :per_element
  end
  defp check_mul_div_kind(_l, %T{shape: {}}) do
    :per_element
  end
  defp check_mul_div_kind(_l, _r) do
    :matrix
  end

  defp maybe_cast_type_for_matrix_mul_div(%T{type: {:f, _}}=tensor) do
    from_nx(tensor)
  end
  defp maybe_cast_type_for_matrix_mul_div(tensor) do
    Evision.Mat.as_type(from_nx(tensor), {:f, 64})
  end

  @impl true
  def min(%T{shape: out_shape}=out, l, r) do
    shape =
      case out_shape do
        {} -> {1}
        _ -> out_shape
      end
    l = Nx.broadcast(l, shape)
    r = Nx.broadcast(r, shape)
    {l, r} = enforce_same_type(l, r)
    l = Evision.Mat.reshape(from_nx(l), shape)
    r = Evision.Mat.reshape(from_nx(r), shape)
    ret = Evision.min(l, r)
    to_nx(ret, %T{out | type: Evision.Mat.type(ret)})
  end

  @impl true
  def max(%T{shape: out_shape}=out, l, r) do
    shape =
      case out_shape do
        {} -> {1}
        _ -> out_shape
      end
    l = Nx.broadcast(l, shape)
    r = Nx.broadcast(r, shape)
    {l, r} = enforce_same_type(l, r)
    l = Evision.Mat.reshape(from_nx(l), shape)
    r = Evision.Mat.reshape(from_nx(r), shape)
    ret = Evision.max(l, r)
    to_nx(ret, %T{out | type: Evision.Mat.type(ret)})
  end

  defp enforce_same_type(%T{type: type}=a, %T{type: type}=b), do: {a, b}
  defp enforce_same_type(%T{type: a_type={:f, 64}}=a, %T{type: _b_type}=b) do
    new_type = Evision.Mat.as_type(from_nx(b), a_type)
    b = %T{b | type: a_type}
    {a, to_nx(new_type, b)}
  end
  defp enforce_same_type(%T{type: _a_type}=a, %T{type: b_type={:f, 64}}=b) do
    new_type = Evision.Mat.as_type(from_nx(a), b_type)
    a = %T{a | type: b_type}
    {to_nx(new_type, a), b}
  end

  defp enforce_same_type(%T{type: a_type={:f, 32}}=a, %T{type: _b_type}=b) do
    new_type = Evision.Mat.as_type(from_nx(b), a_type)
    b = %T{b | type: a_type}
    {a, to_nx(new_type, b)}
  end
  defp enforce_same_type(%T{type: _a_type}=a, %T{type: b_type={:f, 32}}=b) do
    new_type = Evision.Mat.as_type(from_nx(a), b_type)
    a = %T{a | type: b_type}
    {to_nx(new_type, a), b}
  end

  defp enforce_same_type(%T{type: {:u, a_bits}}=a, %T{type: {:u, b_bits}}=b) do
    new_type = {:u, Kernel.max(a_bits, b_bits)}
    new_typed_a = Evision.Mat.as_type(from_nx(a), new_type)
    a = %T{a | type: new_type}
    new_typed_b = Evision.Mat.as_type(from_nx(b), new_type)
    b = %T{b | type: new_type}
    {to_nx(new_typed_a, a), to_nx(new_typed_b, b)}
  end
  defp enforce_same_type(%T{type: {:s, a_bits}}=a, %T{type: {:s, b_bits}}=b) do
    new_type = {:s, Kernel.max(a_bits, b_bits)}
    new_typed_a = Evision.Mat.as_type(from_nx(a), new_type)
    a = %T{a | type: new_type}
    new_typed_b = Evision.Mat.as_type(from_nx(b), new_type)
    b = %T{b | type: new_type}
    {to_nx(new_typed_a, a), to_nx(new_typed_b, b)}
  end

  defp enforce_same_type(%T{type: {:s, a_bits}}=a, %T{type: {:u, b_bits}}=b) do
    new_type = {:s, Kernel.max(a_bits, b_bits)}
    new_typed_a = Evision.Mat.as_type(from_nx(a), new_type)
    a = %T{a | type: new_type}
    new_typed_b = Evision.Mat.as_type(from_nx(b), new_type)
    b = %T{b | type: new_type}
    {to_nx(new_typed_a, a), to_nx(new_typed_b, b)}
  end
  defp enforce_same_type(%T{type: {:u, a_bits}}=a, %T{type: {:s, b_bits}}=b) do
    new_type = {:s, Kernel.max(a_bits, b_bits)}
    new_typed_a = Evision.Mat.as_type(from_nx(a), new_type)
    a = %T{a | type: new_type}
    new_typed_b = Evision.Mat.as_type(from_nx(b), new_type)
    b = %T{b | type: new_type}
    {to_nx(new_typed_a, a), to_nx(new_typed_b, b)}
  end

  @impl true
  def bitwise_and(out, l, r) do
    {left, right} = maybe_cast_u8(l, r)

    %T{type: {_, size_left}} = left
    %T{type: {_, size_right}} = right

    if size_left >= size_right do
      Evision.Mat.bitwise_and(from_nx(left), from_nx(right))
    else
      Evision.Mat.bitwise_and(from_nx(right), from_nx(left))
    end
    |> to_nx(out)
  end

  @impl true
  def bitwise_or(out, l, r) do
    {left, right} = maybe_cast_u8(l, r)

    %T{type: {_, size_left}} = left
    %T{type: {_, size_right}} = right

    if size_left >= size_right do
      Evision.Mat.bitwise_or(from_nx(left), from_nx(right))
    else
      Evision.Mat.bitwise_or(from_nx(right), from_nx(left))
    end
    |> to_nx(out)
  end

  @impl true
  @spec bitwise_xor(Nx.Tensor.t(), Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def bitwise_xor(out, l, r) do
    {left, right} = maybe_cast_u8(l, r)

    %T{type: {_, size_left}} = left
    %T{type: {_, size_right}} = right

    if size_left >= size_right do
      Evision.Mat.bitwise_xor(from_nx(left), from_nx(right))
    else
      Evision.Mat.bitwise_xor(from_nx(right), from_nx(left))
    end
    |> to_nx(out)
  end

  @spec maybe_cast_u8(Nx.Tensor.t(), Nx.Tensor.t()) :: {Nx.Tensor.t(), Nx.Tensor.t()}
  defp maybe_cast_u8(%T{type: {t, _}} = left, %T{type: {t, _}} = right),
       do: {left, right}

  defp maybe_cast_u8(%T{type: {:u, 8}} = left, %T{} = right),
       do: {Nx.as_type(left, {:s, 16}), right}

  defp maybe_cast_u8(%T{} = left, %T{type: {:u, 8}} = right),
       do: {left, Nx.as_type(right, {:s, 16})}

  defp maybe_cast_u8(left, right),
       do: {left, right}

  @impl true
  @spec equal(Nx.Tensor.t(), Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def equal(%T{shape: out_shape} = out, l, r) do
    {l, r} = enforce_same_shape(l, r, out_shape)
    {l, r} = enforce_same_type(l, r)
    from_nx(l)
    |> Evision.Mat.cmp(from_nx(r), :eq)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec not_equal(Nx.Tensor.t(), Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def not_equal(%T{shape: out_shape} = out, l, r) do
    {l, r} = enforce_same_shape(l, r, out_shape)
    {l, r} = enforce_same_type(l, r)
    from_nx(l)
    |> Evision.Mat.cmp(from_nx(r), :ne)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  def greater(%T{shape: out_shape} = out, l, r) do
    {l, r} = enforce_same_shape(l, r, out_shape)
    {l, r} = enforce_same_type(l, r)
    from_nx(l)
    |> Evision.Mat.cmp(from_nx(r), :gt)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec less(Nx.Tensor.t(), Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def less(%T{shape: out_shape} = out, l, r) do
    {l, r} = enforce_same_shape(l, r, out_shape)
    {l, r} = enforce_same_type(l, r)
    from_nx(l)
    |> Evision.Mat.cmp(from_nx(r), :lt)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec greater_equal(Nx.Tensor.t(), Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def greater_equal(%T{shape: out_shape} = out, l, r) do
    {l, r} = enforce_same_shape(l, r, out_shape)
    {l, r} = enforce_same_type(l, r)
    from_nx(l)
    |> Evision.Mat.cmp(from_nx(r), :ge)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  def less_equal(%T{shape: out_shape} = out, l, r) do
    {l, r} = enforce_same_shape(l, r, out_shape)
    {l, r} = enforce_same_type(l, r)
    from_nx(l)
    |> Evision.Mat.cmp(from_nx(r), :le)
    |> to_nx(out)
  end

  @spec enforce_same_shape(Nx.Tensor.t(), Nx.Tensor.t(), tuple()) :: {Nx.Tensor.t(), Nx.Tensor.t()}
  defp enforce_same_shape(l, r, out_shape) do
    l_mat = Evision.Mat.reshape(from_nx(Nx.broadcast(l, out_shape)), out_shape)
    b_mat = Evision.Mat.reshape(from_nx(Nx.broadcast(r, out_shape)), out_shape)
    {to_nx(l_mat, %T{l | shape: out_shape}), to_nx(b_mat, %T{r | shape: out_shape})}
  end

  @impl true
  @spec logical_and(Nx.Tensor.t(), Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def logical_and(%T{shape: out_shape} = out, l, r) do
    {l, r} = enforce_same_shape(l, r, out_shape)
    {l, r} = enforce_same_type(l, r)
    Evision.Mat.logical_and(from_nx(l), from_nx(r))
    |> reject_error()
    |> to_nx(out)
    |> Nx.not_equal(0)
  end

  @impl true
  @spec logical_or(Nx.Tensor.t(), Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def logical_or(%T{shape: out_shape} = out, l, r) do
    {l, r} = enforce_same_shape(l, r, out_shape)
    {l, r} = enforce_same_type(l, r)
    Evision.Mat.logical_or(from_nx(l), from_nx(r))
    |> reject_error()
    |> to_nx(out)
    |> Nx.not_equal(0)
  end

  @impl true
  @spec logical_xor(Nx.Tensor.t(), Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def logical_xor(%T{shape: out_shape} =out, l, r) do
    {l, r} = enforce_same_shape(l, r, out_shape)
    {l, r} = enforce_same_type(l, r)
    Evision.Mat.logical_xor(from_nx(l), from_nx(r))
    |> reject_error()
    |> to_nx(out)
    |> Nx.not_equal(0)
  end

  @impl true
  @spec abs(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def abs(%T{} = out, tensor) do
    Evision.Mat.abs(from_nx(tensor))
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec bitwise_not(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def bitwise_not(%T{type: {s, _}} = out, tensor) when s in [:s, :u] do
    Evision.Mat.bitwise_not(from_nx(tensor))
    |> reject_error()
    |> to_nx(out)
  end

  def bitwise_not(%T{type: type} = _out, _tensor) do
    raise ArgumentError, "bitwise operators expect integer tensors as inputs " <>
                         "and outputs an integer tensor, got: #{inspect(type)}"
  end

  @impl true
  @spec ceil(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def ceil(%T{type: {s, _}} = out, tensor) when s == :f do
    Evision.Mat.ceil(from_nx(tensor))
    |> reject_error()
    |> to_nx(out)
  end

  def ceil(%T{} = out, tensor) do
    to_nx(from_nx(tensor), out)
  end

  @impl true
  @spec floor(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def floor(%T{type: {s, _}} = out, tensor) when s == :f do
    Evision.Mat.floor(from_nx(tensor))
    |> reject_error()
    |> to_nx(out)
  end

  def floor(%T{} = out, tensor) do
    to_nx(from_nx(tensor), out)
  end

  @impl true
  @spec negate(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def negate(%T{} = out, tensor) do
    Evision.Mat.negate(from_nx(tensor))
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec round(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def round(%T{type: {s, _}} = out, tensor) when s == :f do
    Evision.Mat.round(from_nx(tensor))
    |> reject_error()
    |> to_nx(out)
  end

  def round(%T{} = out, tensor) do
    to_nx(from_nx(tensor), out)
  end

  @impl true
  @spec sign(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def sign(%T{} = out, tensor) do
    Evision.Mat.sign(from_nx(tensor))
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec exp(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def exp(%T{} = out, tensor) do
    Evision.exp(from_nx(tensor))
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec expm1(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def expm1(%T{} = out, tensor) do
    Evision.Mat.expm1(from_nx(tensor))
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  @spec log(Nx.Tensor.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def log(%T{} = out, tensor) do
    Evision.log(from_nx(tensor))
    |> reject_error()
    |> to_nx(out)
  end

  @doc false
  def from_nx(%T{data: %EB{ref: mat_ref}}), do: mat_ref
  def from_nx(%T{} = tensor) do
    Nx.backend_transfer(tensor, EB)
    |> from_nx()
  end

  @doc false
  @spec to_nx(Evision.Mat.t(), Nx.Tensor.t()) :: Nx.Tensor.t()
  def to_nx(mat_ref, %T{shape: shape} = t) when is_struct(mat_ref, Evision.Mat) do
    type = Evision.Mat.type(mat_ref)
    %{t | type: type, data: %__MODULE__{ref: check_shape_and_type(mat_ref, shape, type)}}
  end

  @spec to_number(number() | Nx.Tensor.t()) :: number()
  defp to_number(n) when is_number(n), do: n
  defp to_number(%T{} = t) do
    case Evision.Mat.at(from_nx(t), 0) do
      n when is_number(n) -> n
      {:error, msg} -> raise RuntimeError, msg
    end
  end

  if Application.compile_env(:evision, :check_shape_and_type, false) do
    @spec check_shape_and_type(Evision.Mat.t(), tuple, Evision.Mat.mat_type()) :: Evision.Mat.t()
    defp check_shape_and_type(mat_ref, shape, type) do
      current_type = Evision.Mat.type(mat_ref)

      if current_type != type do
        raise "type mismatch in Evision: expected #{inspect(type)}, got: #{inspect(current_type)}. " <>
              "Please report this bug"
      end

      current_shape = Evision.Mat.shape(mat_ref)

      if current_shape != shape do
        raise "shape mismatch in Torchx: expected #{inspect(shape)}, got: #{inspect(current_shape)}. " <>
              "Please report this bug"
      end

      mat_ref
    end
  else
    @spec check_shape_and_type(Evision.Mat.t(), any, any) :: Evision.Mat.t()
    defp check_shape_and_type(mat_ref, _, _), do: mat_ref
  end

  if Application.compile_env(:evision, :add_backend_on_inspect, true) do
    defp maybe_add_signature(result, %T{data: %EB{ref: _mat_ref}}) do
      Inspect.Algebra.concat([
        "Evision.Backend",
        Inspect.Algebra.line(),
        result
      ])
    end
  else
    defp maybe_add_signature(result, _tensor) do
      result
    end
  end

  funs = Nx.Backend.behaviour_info(:callbacks) -- Module.definitions_in(__MODULE__, :def)

  @doc false
  def __unimplemented__, do: unquote(funs)

  for {fun, arity} <- funs do
    args = Macro.generate_arguments(arity, __MODULE__)

    @impl true
    def unquote(fun)(unquote_splicing(args)) do
      raise "operation #{unquote(fun)} is not yet supported on Evision.Backend.\n" <>
      "Please use another backend like Nx.BinaryBackend or Torchx.Backend.\n" <>
      "  To use Torchx.Backend, :torchx should be added to your app's deps.\n" <>
      "  Please see https://github.com/elixir-nx/nx/tree/main/torchx for more information on how to install and use it.\n" <>
      "To convert the tensor to another backend, please use Evision.Mat.to_nx(tensor, Backend.ModuleName)\n" <>
      "  for example, Evision.Mat.to_nx(tensor, Nx.BinaryBackend) or Evision.Mat.to_nx(tensor, Torchx.Backend).\n" <>
      "Pull request would be more than welcomed if you'd like to implmenent this function and make contributions."
    end
  end
end
