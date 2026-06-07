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
  @spec init(keyword()) :: keyword()
  def init(opts), do: opts

  @impl true
  def block(%Nx.Block.LogicalNot{}, out, [tensor], _fun), do: logical_not(out, tensor)
  def block(%Nx.Block.AllClose{} = opts, out, [a, b], _fun), do: all_close(out, a, b, Map.from_struct(opts))
  def block(%Nx.Block.CumulativeSum{} = opts, out, [tensor], _fun), do: cumulative_sum(out, tensor, Map.from_struct(opts))
  def block(%Nx.Block.CumulativeProduct{} = opts, out, [tensor], _fun), do: cumulative_product(out, tensor, Map.from_struct(opts))
  def block(%Nx.Block.CumulativeMin{} = opts, out, [tensor], _fun), do: cumulative_min(out, tensor, Map.from_struct(opts))
  def block(%Nx.Block.CumulativeMax{} = opts, out, [tensor], _fun), do: cumulative_max(out, tensor, Map.from_struct(opts))
  def block(%Nx.Block.Take{} = opts, out, [tensor, indices], _fun), do: take(out, tensor, indices, opts.axis)
  def block(%Nx.Block.TakeAlongAxis{} = opts, out, [tensor, indices], _fun), do: take_along_axis(out, tensor, indices, opts.axis)
  def block(%Nx.Block.TopK{} = opts, out, [tensor], _fun), do: top_k(out, tensor, Map.from_struct(opts))
  def block(%Nx.Block.LinAlg.Cholesky{}, out, [tensor], _fun), do: cholesky(out, tensor)
  def block(%Nx.Block.LinAlg.Solve{}, out, [a, b], _fun), do: solve(out, a, b)
  def block(%Nx.Block.LinAlg.QR{} = opts, out, [tensor], _fun), do: qr(out, tensor, Map.from_struct(opts))
  def block(%Nx.Block.LinAlg.Eigh{} = opts, out, [tensor], _fun), do: eigh(out, tensor, Map.from_struct(opts))
  def block(%Nx.Block.LinAlg.SVD{} = opts, out, [tensor], _fun), do: svd(out, tensor, Map.from_struct(opts))
  def block(%Nx.Block.LinAlg.LU{}, out, [tensor], _fun), do: lu(out, tensor, [])
  def block(%Nx.Block.LinAlg.Determinant{}, out, [tensor], _fun), do: determinant(out, tensor)
  def block(struct, _output, args, fun), do: apply(fun, [struct | args])

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
  def broadcast(out, %T{shape: src_shape} = t, to_shape, axes) do
    Evision.Mat.broadcast_to(from_nx(t), to_shape, force_src_shape(src_shape, to_shape, axes))
    |> reject_error()
    |> to_nx(out)
  end

  # Place each source dim onto the target axis Nx assigns it (other axes are 1),
  # so the NIF stretches the size-1 axes. This mirrors Nx broadcasting for both
  # the default right-aligned case and explicit `:axes`.
  defp force_src_shape(src_shape, to_shape, axes) do
    placed = Map.new(Enum.zip(Tuple.to_list(src_shape), axes), fn {dim, axis} -> {axis, dim} end)

    for(axis <- 0..(tuple_size(to_shape) - 1)//1, do: Map.get(placed, axis, 1))
    |> List.to_tuple()
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
        Evision.Mat.multiply(from_nx(float_scalar(l)), from_nx(float_scalar(r)), type)

      :matrix ->
        l = maybe_cast_type_for_matrix_mul_div(l)
        r = maybe_cast_type_for_matrix_mul_div(r)
        Evision.Mat.matrix_multiply(l, r)
        |> Evision.Mat.as_type(type)
    end
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  def divide(%T{type: type, shape: shape}=out, l, r) do
    case check_mul_div_kind(l, r) do
      :per_element ->
        Evision.Mat.divide(from_nx(float_scalar(l)), from_nx(float_scalar(r)), type)

      :matrix ->
        l = Nx.broadcast(l, shape)
        r = Nx.broadcast(r, shape)
        l = maybe_cast_type_for_matrix_mul_div(l)
        r = maybe_cast_type_for_matrix_mul_div(r)
        Evision.Mat.divide(l, r, type)
    end
    |> reject_error()
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

  # OpenCV's arithmetic only treats a 1x1 operand as a scalar when it is f64 (an
  # integer or f32 scalar is rejected, especially as the left operand), so cast a
  # 0-d operand to f64 -- a single element -- instead of broadcasting it to a full
  # array. Widening a float scalar is lossless and the other operand is untouched.
  defp float_scalar(%T{shape: {}} = scalar), do: Nx.as_type(scalar, {:f, 64})
  defp float_scalar(scalar), do: scalar

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
  defp enforce_same_shape(%T{} = l, %T{} = r, out_shape) do
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

  ## Aggregates

  @impl true
  def argmax(out, tensor, opts), do: arg_aggregate(:max, out, tensor, opts)

  @impl true
  def argmin(out, tensor, opts), do: arg_aggregate(:min, out, tensor, opts)

  defp arg_aggregate(which, %T{type: out_type, shape: out_shape} = out, %T{shape: shape} = tensor, opts) do
    last_index? = opts[:tie_break] == :high
    mat = from_nx(tensor)

    reduced =
      if opts[:axis] == nil or Nx.rank(shape) <= 1 do
        flat = reject_error(Evision.Mat.reshape(mat, [1, Nx.size(shape)]))
        reduce_arg(flat, which, 1, last_index?)
      else
        reduce_arg(mat, which, opts[:axis], last_index?)
      end

    reshape_to =
      case Tuple.to_list(out_shape) do
        [] -> [1]
        dims -> dims
      end

    reduced
    |> reject_error()
    |> Evision.Mat.as_type(out_type)
    |> reject_error()
    |> Evision.Mat.reshape(reshape_to)
    |> reject_error()
    |> to_nx(out)
  end

  defp reduce_arg(mat, :max, axis, last_index?),
    do: Evision.reduceArgMax(mat, axis, lastIndex: last_index?)

  defp reduce_arg(mat, :min, axis, last_index?),
    do: Evision.reduceArgMin(mat, axis, lastIndex: last_index?)

  ## Sorting

  # cv::sort/sortIdx support these depths directly; wider integer depths go through
  # a custom NIF, and half floats are widened to f32 (lossless) so cv can handle them.
  @wide_int_types [{:u, 32}, {:s, 64}, {:u, 64}]
  @half_types [{:f, 16}, {:bf, 16}]
  @float_sort_types [{:f, 32}, {:f, 64}]
  @custom_sort_types @wide_int_types ++ @float_sort_types

  @impl true
  def sort(out, %T{shape: shape, type: type} = tensor, opts) do
    descending? = opts[:direction] == :desc

    from_nx(tensor)
    |> along_axis(shape, opts[:axis], &sort_values(&1, type, descending?))
    |> to_nx(out)
  end

  @impl true
  def argsort(%T{type: out_type} = out, %T{shape: shape, type: type} = tensor, opts) do
    descending? = opts[:direction] == :desc

    from_nx(tensor)
    |> along_axis(shape, opts[:axis], fn m2d ->
      m2d
      |> sort_indices(type, descending?)
      |> Evision.Mat.as_type(out_type)
      |> reject_error()
    end)
    |> to_nx(out)
  end

  defp sort_values(m2d, type, descending?) when type in @custom_sort_types do
    reject_error(Evision.Mat.sort_rows(m2d, descending?))
  end

  defp sort_values(m2d, type, descending?) when type in @half_types do
    m2d
    |> as_mat_type({:f, 32})
    |> Evision.Mat.sort_rows(descending?)
    |> reject_error()
    |> as_mat_type(type)
  end

  defp sort_values(m2d, _type, descending?) do
    reject_error(Evision.sort(m2d, sort_flags(descending?)))
  end

  defp sort_indices(m2d, type, descending?) when type in @custom_sort_types do
    reject_error(Evision.Mat.argsort_rows(m2d, descending?))
  end

  defp sort_indices(m2d, type, descending?) when type in @half_types do
    m2d
    |> as_mat_type({:f, 32})
    |> Evision.Mat.argsort_rows(descending?)
    |> reject_error()
  end

  # cv::sortIdx is stable ascending but not descending, so sort the order-reversing
  # complement ascending to reproduce Nx's stable descending order.
  defp sort_indices(m2d, type, descending?) do
    work = if type in @half_types, do: as_mat_type(m2d, {:f, 32}), else: m2d
    work = if descending?, do: complement(work), else: work
    reject_error(Evision.sortIdx(work, 0))
  end

  # SORT_EVERY_ROW = 0, SORT_ASCENDING = 0, SORT_DESCENDING = 16.
  defp sort_flags(true), do: 16
  defp sort_flags(false), do: 0

  defp complement(mat) do
    case Evision.Mat.type(mat) do
      {t, _} when t in [:f, :bf] -> reject_error(Evision.Mat.negate(mat))
      {t, _} when t in [:s, :u] -> reject_error(Evision.Mat.bitwise_not(mat))
    end
  end

  defp as_mat_type(mat, type), do: reject_error(Evision.Mat.as_type(mat, type))

  # Reduce an n-d row-wise op along `axis` to cv's 2D per-row form: move the
  # target axis last, flatten to [n, axis_len], apply `fun`, then restore.
  defp along_axis(mat, shape, axis, fun) do
    rank = tuple_size(shape)
    identity = Enum.to_list(0..(rank - 1))
    perm = (identity -- [axis]) ++ [axis]
    axis_len = elem(shape, axis)
    n = div(Nx.size(shape), max(axis_len, 1))
    t_shape = perm |> Enum.map(&elem(shape, &1)) |> List.to_tuple()

    transposed =
      if perm == identity,
        do: mat,
        else: reject_error(Evision.Mat.transpose(mat, perm, as_shape: shape))

    result_t =
      transposed
      |> Evision.Mat.reshape([n, axis_len])
      |> reject_error()
      |> fun.()
      |> Evision.Mat.reshape(Tuple.to_list(t_shape))
      |> reject_error()

    if perm == identity,
      do: result_t,
      else: reject_error(Evision.Mat.transpose(result_t, inverse_perm(perm), as_shape: t_shape))
  end

  defp inverse_perm(perm) do
    perm
    |> Enum.with_index()
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map(&elem(&1, 1))
  end

  ## Cumulative

  @scan_sum 0
  @scan_product 1
  @scan_min 2
  @scan_max 3

  def cumulative_sum(out, tensor, opts), do: cumulative(out, tensor, opts, @scan_sum)

  def cumulative_product(out, tensor, opts), do: cumulative(out, tensor, opts, @scan_product)

  def cumulative_min(out, tensor, opts), do: cumulative(out, tensor, opts, @scan_min)

  def cumulative_max(out, tensor, opts), do: cumulative(out, tensor, opts, @scan_max)

  # Prefix scan along `axis` (output keeps the input shape and type). Half floats
  # are widened to f32 (lossless) since the scan NIF covers only real C types.
  defp cumulative(%T{type: type, shape: shape} = out, tensor, opts, op) do
    reverse? = opts[:reverse]

    from_nx(tensor)
    |> along_axis(shape, opts[:axis], &scan(&1, type, op, reverse?))
    |> to_nx(out)
  end

  defp scan(m2d, type, op, reverse?) when type in @half_types do
    m2d
    |> as_mat_type({:f, 32})
    |> Evision.Mat.cumulative(op, reverse?)
    |> reject_error()
    |> as_mat_type(type)
  end

  defp scan(m2d, _type, op, reverse?) do
    reject_error(Evision.Mat.cumulative(m2d, op, reverse?))
  end

  ## Elementwise unary math

  @uop_sin 0
  @uop_cos 1
  @uop_tan 2
  @uop_asin 3
  @uop_acos 4
  @uop_atan 5
  @uop_sinh 6
  @uop_cosh 7
  @uop_tanh 8
  @uop_asinh 9
  @uop_acosh 10
  @uop_atanh 11
  @uop_erf 12
  @uop_erfc 13
  @uop_cbrt 14
  @uop_log1p 15
  @uop_rsqrt 16
  @uop_sigmoid 17

  # sqrt reuses cv::sqrt (the fast path, like exp/log); the rest have no cv
  # primitive and go through the element-wise math NIF.
  @impl true
  def sqrt(out, tensor), do: unary_math(out, tensor, &Evision.sqrt/1)

  @impl true
  def sin(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_sin))

  @impl true
  def cos(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_cos))

  @impl true
  def tan(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_tan))

  @impl true
  def asin(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_asin))

  @impl true
  def acos(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_acos))

  @impl true
  def atan(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_atan))

  @impl true
  def sinh(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_sinh))

  @impl true
  def cosh(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_cosh))

  @impl true
  def tanh(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_tanh))

  @impl true
  def asinh(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_asinh))

  @impl true
  def acosh(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_acosh))

  @impl true
  def atanh(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_atanh))

  @impl true
  def erf(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_erf))

  @impl true
  def erfc(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_erfc))

  @impl true
  def cbrt(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_cbrt))

  @impl true
  def log1p(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_log1p))

  @impl true
  def rsqrt(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_rsqrt))

  @impl true
  def sigmoid(out, tensor), do: unary_math(out, tensor, &Evision.Mat.unary_math(&1, @uop_sigmoid))

  # Cast the input to the (float) output type, widening f16/bf16 to f32 (the math
  # NIF/cv::sqrt handle only f32/f64) and narrowing the result back.
  defp unary_math(%T{type: out_type} = out, tensor, fun) do
    work_type = if out_type in @half_types, do: {:f, 32}, else: out_type

    from_nx(tensor)
    |> as_mat_type(work_type)
    |> fun.()
    |> reject_error()
    |> maybe_narrow(work_type, out_type)
    |> to_nx(out)
  end

  ## Reductions

  @reduce_sum 0
  @reduce_product 1
  @reduce_max 2
  @reduce_min 3

  @impl true
  def sum(out, tensor, opts), do: reduce_aggregate(out, tensor, opts, @reduce_sum)

  @impl true
  def product(out, tensor, opts), do: reduce_aggregate(out, tensor, opts, @reduce_product)

  @impl true
  def reduce_max(out, tensor, opts), do: reduce_aggregate(out, tensor, opts, @reduce_max)

  @impl true
  def reduce_min(out, tensor, opts), do: reduce_aggregate(out, tensor, opts, @reduce_min)

  @impl true
  def all(out, tensor, opts), do: bool_reduce(out, tensor, opts, @reduce_min)

  @impl true
  def any(out, tensor, opts), do: bool_reduce(out, tensor, opts, @reduce_max)

  # all = every element nonzero (min over the {0,1} nonzero mask); any = some nonzero (max).
  defp bool_reduce(out, %T{} = tensor, opts, op) do
    mask = tensor |> from_nx() |> nonzero_mask()
    reduce_aggregate(out, to_nx(mask, tensor), opts, op)
  end

  defp nonzero_mask(mat) do
    mat |> Evision.Mat.sign() |> reject_error() |> Evision.Mat.abs() |> reject_error()
  end

  @impl true
  def select(%T{shape: shape, type: out_type} = out, pred, on_true, on_false) do
    mask =
      pred
      |> Nx.broadcast(shape)
      |> from_nx()
      |> nonzero_mask()
      |> as_mat_type({:u, 8})

    t = on_true |> Nx.as_type(out_type) |> Nx.broadcast(shape) |> from_nx()
    f = on_false |> Nx.as_type(out_type) |> Nx.broadcast(shape) |> from_nx()

    Evision.Mat.select(mask, t, f)
    |> reject_error()
    |> to_nx(out)
  end

  ## Predicates

  @pred_is_nan 0
  @pred_is_infinity 1
  @pred_is_zero 2

  @impl true
  def is_nan(out, tensor), do: predicate(out, tensor, @pred_is_nan)

  @impl true
  def is_infinity(out, tensor), do: predicate(out, tensor, @pred_is_infinity)

  def logical_not(out, tensor), do: predicate(out, tensor, @pred_is_zero)

  # Elementwise predicate -> u8. f16/bf16 widen to f32 (the NIF covers only real C types).
  defp predicate(%T{shape: out_shape} = out, %T{type: type} = tensor, op) do
    mat = from_nx(tensor)
    mat = if type in @half_types, do: as_mat_type(mat, {:f, 32}), else: mat

    mat
    |> Evision.Mat.predicate(op)
    |> reject_error()
    |> Evision.Mat.reshape(reduce_out_dims(out_shape))
    |> reject_error()
    |> to_nx(out)
  end

  ## Bitwise

  @bit_clz 0
  @bit_popcount 1
  @shift_left 0
  @shift_right 1

  @impl true
  def count_leading_zeros(out, tensor), do: bit_unary(out, tensor, @bit_clz)

  @impl true
  def population_count(out, tensor), do: bit_unary(out, tensor, @bit_popcount)

  defp bit_unary(out, tensor, op) do
    Evision.Mat.bit_unary(from_nx(tensor), op) |> reject_error() |> to_nx(out)
  end

  @impl true
  def left_shift(out, l, r), do: shift(out, l, r, @shift_left)

  @impl true
  def right_shift(out, l, r), do: shift(out, l, r, @shift_right)

  # l and r are broadcast to the output shape and cast to the (integer) output type.
  defp shift(%T{shape: shape, type: type} = out, l, r, op) do
    {l, r} = enforce_same_shape(l, r, shape)
    lm = as_mat_type(from_nx(l), type)
    rm = as_mat_type(from_nx(r), type)
    Evision.Mat.shift(lm, rm, op) |> reject_error() |> to_nx(out)
  end

  ## All-close

  # Replicates Nx's vectorized_all_close out of already-implemented ops (the optional
  # fallback is shadowed by the catch-all, so it must be built here). Scalars go second
  # in multiply (Evision's scalar-first multiply path rejects constants), and the
  # `select(nan, 1, inf)` over {0,1} masks is written as logical_or.
  def all_close(_out, a, b, opts) do
    atol = opts[:atol]
    rtol = opts[:rtol]
    finite = Nx.less_equal(Nx.abs(Nx.subtract(a, b)), Nx.add(Nx.multiply(Nx.abs(b), rtol), atol))

    if Nx.Type.integer?(a.type) and Nx.Type.integer?(b.type) do
      Nx.all(finite)
    else
      inf_entries =
        Nx.select(Nx.logical_or(Nx.is_infinity(a), Nx.is_infinity(b)), Nx.equal(a, b), finite)

      if opts[:equal_nan] do
        nan_entries = Nx.logical_and(Nx.is_nan(a), Nx.is_nan(b))
        Nx.all(Nx.logical_or(nan_entries, inf_entries))
      else
        Nx.all(inf_entries)
      end
    end
  end

  ## Elementwise binary (no cv primitive)

  @bop_atan2 0
  @bop_pow 1
  @bop_quotient 2
  @bop_remainder 3

  @impl true
  def atan2(out, l, r), do: binary_math(out, l, r, @bop_atan2)

  @impl true
  def pow(out, l, r), do: binary_math(out, l, r, @bop_pow)

  @impl true
  def quotient(out, l, r), do: binary_math(out, l, r, @bop_quotient)

  @impl true
  def remainder(out, l, r), do: binary_math(out, l, r, @bop_remainder)

  # Broadcast l and r to the output shape and cast both to the output type (f16/bf16
  # widened to f32 since the NIF covers only real C types), then narrow the result back.
  defp binary_math(%T{type: out_type, shape: shape} = out, l, r, op) do
    work_type = if out_type in @half_types, do: {:f, 32}, else: out_type
    {l, r} = enforce_same_shape(l, r, shape)
    lm = as_mat_type(from_nx(l), work_type)
    rm = as_mat_type(from_nx(r), work_type)

    Evision.Mat.binop(lm, rm, op)
    |> reject_error()
    |> maybe_narrow(work_type, out_type)
    |> to_nx(out)
  end

  ## Slicing

  @impl true
  def slice(%T{shape: out_shape} = out, %T{shape: shape} = tensor, start_indices, lengths, strides) do
    starts = clamp_starts(start_indices, shape, lengths)

    Evision.Mat.strided_copy(
      from_nx(tensor),
      Tuple.to_list(shape),
      Tuple.to_list(out_shape),
      starts,
      strides
    )
    |> reject_error()
    |> Evision.Mat.reshape(reduce_out_dims(out_shape))
    |> reject_error()
    |> to_nx(out)
  end

  defp clamp_starts(start_indices, shape, lengths) do
    [Tuple.to_list(shape), start_indices, lengths]
    |> Enum.zip_with(fn [dim, idx, len] -> min(max(to_number(idx), 0), dim - len) end)
  end

  @impl true
  def reverse(%T{shape: shape} = out, tensor, axes) do
    dims = Tuple.to_list(shape)
    axset = MapSet.new(axes)

    {starts, strides} =
      tuple_size(shape)
      |> all_axes()
      |> Enum.map(fn k ->
        if MapSet.member?(axset, k), do: {elem(shape, k) - 1, -1}, else: {0, 1}
      end)
      |> Enum.unzip()

    Evision.Mat.strided_copy(from_nx(tensor), dims, dims, starts, strides)
    |> reject_error()
    |> Evision.Mat.reshape(reduce_out_dims(shape))
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  def put_slice(%T{shape: shape} = out, tensor, start_indices, slice) do
    starts = clamp_starts(start_indices, shape, Tuple.to_list(slice.shape))
    do_put_slice(out, tensor, slice, starts)
  end

  defp do_put_slice(%T{type: out_type, shape: shape} = out, base, %T{shape: slice_shape} = slice, starts) do
    base_mat = as_mat_type(from_nx(base), out_type)
    slice_mat = as_mat_type(from_nx(slice), out_type)

    Evision.Mat.put_slice(base_mat, slice_mat, Tuple.to_list(shape), Tuple.to_list(slice_shape), starts)
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  def concatenate(%T{type: out_type, shape: shape} = out, tensors, 0) do
    bin =
      tensors
      |> Enum.map(fn t -> t |> from_nx() |> as_mat_type(out_type) |> to_binary_mat!() end)
      |> IO.iodata_to_binary()

    Evision.Mat.from_binary_by_shape(bin, out_type, shape)
    |> reject_error()
    |> to_nx(out)
  end

  def concatenate(%T{type: out_type, shape: shape} = out, tensors, axis) do
    rank = tuple_size(shape)
    base = Evision.Mat.full(shape, 0, out_type) |> reject_error() |> to_nx(out)

    tensors
    |> Enum.reduce({base, 0}, fn t, {acc, offset} ->
      starts = List.duplicate(0, rank) |> List.replace_at(axis, offset)
      {do_put_slice(out, acc, t, starts), offset + elem(t.shape, axis)}
    end)
    |> elem(0)
  end

  def take_along_axis(%T{shape: out_shape} = out, %T{shape: shape} = tensor, indices, axis) do
    idx = as_mat_type(from_nx(indices), {:s, 64})

    Evision.Mat.take_along_axis(from_nx(tensor), idx, Tuple.to_list(shape), Tuple.to_list(out_shape), axis)
    |> reject_error()
    |> Evision.Mat.reshape(reduce_out_dims(out_shape))
    |> reject_error()
    |> to_nx(out)
  end

  # top_k is optional (the fallback is shadowed by the catch-all), so it is built here
  # from argsort + take_along_axis + slice along the last axis.
  def top_k(_out, tensor, opts) do
    axis = Nx.rank(tensor) - 1
    indices = Nx.argsort(tensor, axis: axis, direction: :desc)
    values = Nx.take_along_axis(tensor, indices, axis: axis)
    k = opts[:k]

    {Nx.slice_along_axis(values, 0, k, axis: axis), Nx.slice_along_axis(indices, 0, k, axis: axis)}
  end

  # Reduce `op` (0 sum, 1 product, 2 max, 3 min) over `opts[:axes]` (nil = all axes).
  # The reduce NIF reads the native dtype and promotes per element (sum/product use a
  # s64/u64/f64 accumulator to match Nx), so no input cast is needed. The reduced axes
  # are flattened against the kept axes: trailing -> reduce each row, leading -> reduce
  # each column (no transpose), interleaved -> transpose them last then reduce per row.
  defp reduce_aggregate(%T{type: out_type, shape: out_shape} = out, %T{shape: shape} = tensor, opts, op) do
    rank = tuple_size(shape)
    axes = all_axes(rank)
    reduce_axes = Enum.sort(opts[:axes] || axes)
    keep_axes = axes -- reduce_axes
    keep_size = sizes_product(shape, keep_axes)
    reduce_size = sizes_product(shape, reduce_axes)
    mat = from_nx(tensor)

    reduced =
      cond do
        keep_axes ++ reduce_axes == axes ->
          mat
          |> Evision.Mat.reshape([keep_size, reduce_size])
          |> reject_error()
          |> Evision.Mat.reduce_rows(op)

        reduce_axes ++ keep_axes == axes ->
          mat
          |> Evision.Mat.reshape([reduce_size, keep_size])
          |> reject_error()
          |> Evision.Mat.reduce_cols(op)

        true ->
          mat
          |> Evision.Mat.transpose(keep_axes ++ reduce_axes, as_shape: shape)
          |> reject_error()
          |> Evision.Mat.reshape([keep_size, reduce_size])
          |> reject_error()
          |> Evision.Mat.reduce_rows(op)
      end

    reduced
    |> reject_error()
    |> as_mat_type(out_type)
    |> Evision.Mat.reshape(reduce_out_dims(out_shape))
    |> reject_error()
    |> to_nx(out)
  end

  defp all_axes(0), do: []
  defp all_axes(rank), do: Enum.to_list(0..(rank - 1))

  defp sizes_product(shape, axes), do: Enum.reduce(axes, 1, &(elem(shape, &1) * &2))

  defp reduce_out_dims({}), do: [1]
  defp reduce_out_dims(shape), do: Tuple.to_list(shape)

  ## Indexing

  def take(%T{shape: out_shape} = out, %T{shape: shape} = tensor, indices, axis) do
    dims = Tuple.to_list(shape)
    outer = dims |> Enum.take(axis) |> Enum.product()
    inner = dims |> Enum.drop(axis + 1) |> Enum.product()
    idx = as_mat_type(from_nx(indices), {:s, 64})

    Evision.Mat.take(from_nx(tensor), idx, outer, elem(shape, axis), inner)
    |> reject_error()
    |> Evision.Mat.reshape(reduce_out_dims(out_shape))
    |> reject_error()
    |> to_nx(out)
  end

  @impl true
  def gather(%T{shape: out_shape} = out, tensor, %T{shape: indices_shape} = indices, opts) do
    axes = opts[:axes]
    tensor_axes = Nx.axes(tensor)

    tensor =
      if List.starts_with?(tensor_axes, axes) do
        tensor
      else
        Nx.transpose(tensor, axes: axes ++ (tensor_axes -- axes))
      end

    depth = elem(indices_shape, tuple_size(indices_shape) - 1)
    {lead, leftover} = tensor.shape |> Tuple.to_list() |> Enum.split(depth)
    dims = from_nx(Nx.tensor(lead, type: {:s, 64}))
    idx = as_mat_type(from_nx(indices), {:s, 64})

    Evision.Mat.gather(from_nx(tensor), idx, dims, Enum.product(leftover))
    |> reject_error()
    |> Evision.Mat.reshape(reduce_out_dims(out_shape))
    |> reject_error()
    |> to_nx(out)
  end

  @scatter_put 0
  @scatter_add 1

  @impl true
  def indexed_add(out, target, indices, updates, opts),
    do: indexed_op(out, target, indices, updates, opts, @scatter_add)

  @impl true
  def indexed_put(out, target, indices, updates, opts),
    do: indexed_op(out, target, indices, updates, opts, @scatter_put)

  # Mirrors Nx.BinaryBackend's with_permutation: scatter into `target`, transposing
  # the indexed `axes` to the front first (and the result back) when they are not
  # already leading, so the leftover axes form one contiguous block per coordinate.
  defp indexed_op(%T{shape: out_shape} = out, target, indices, updates, opts, op) do
    axes = opts[:axes]
    target_axes = Nx.axes(target)

    if List.starts_with?(target_axes, axes) do
      scatter(out, target, indices, updates, op)
    else
      perm = axes ++ (target_axes -- axes)
      perm_shape = perm |> Enum.map(&elem(out_shape, &1)) |> List.to_tuple()

      scatter(%{out | shape: perm_shape}, Nx.transpose(target, axes: perm), indices, updates, op)
      |> Nx.transpose(axes: inverse_perm(perm))
    end
  end

  defp scatter(%T{type: out_type, shape: shape} = out, target, %T{shape: indices_shape} = indices, updates, op) do
    work_type = if op == @scatter_add and out_type in @half_types, do: {:f, 32}, else: out_type
    depth = elem(indices_shape, 1)
    {lead, leftover} = shape |> Tuple.to_list() |> Enum.split(depth)
    dims = from_nx(Nx.tensor(lead, type: {:s, 64}))
    idx = as_mat_type(from_nx(indices), {:s, 64})
    base = as_mat_type(from_nx(target), work_type)
    upd = as_mat_type(from_nx(updates), work_type)

    base
    |> Evision.Mat.indexed(idx, upd, dims, Enum.product(leftover), op)
    |> reject_error()
    |> maybe_narrow(work_type, out_type)
    |> to_nx(out)
  end

  defp maybe_narrow(mat, type, type), do: mat
  defp maybe_narrow(mat, _work_type, out_type), do: as_mat_type(mat, out_type)

  ## Linear algebra

  # Each callback receives `out` tensors already shaped and typed by Nx, so the only job
  # is to produce matching values. cv covers determinant/solve/SVD/eigen; cholesky, lu, qr
  # and triangular_solve have no cv factor extraction and go through custom NIFs. All ops
  # compute in f64 (cast back to out.type) and loop over the leading batch dims.

  def determinant(%T{type: out_type, shape: out_shape} = out, %T{shape: shape} = tensor) do
    {_batch, n, _n} = mat_dims(shape)

    bin =
      for m <- split_2d(tensor, n, n), into: <<>> do
        <<determinant_one(m)::float-64-native>>
      end

    Evision.Mat.from_binary_by_shape(bin, {:f, 64}, out_shape)
    |> reject_error()
    |> as_mat_type(out_type)
    |> to_nx(out)
  end

  defp determinant_one(mat) do
    case Evision.determinant(mat) do
      {:error, msg} -> raise RuntimeError, msg
      d when is_number(d) -> d * 1.0
    end
  end

  def solve(out, %T{shape: a_shape} = a, %T{shape: b_shape} = b) do
    {batch, n, _n} = mat_dims(a_shape)
    {b_trailing, vector?} = rhs_trailing(a_shape, b_shape)

    Enum.zip_with(split_2d(a, n, n), split_into(b, batch, b_trailing), fn am, bm ->
      bm = if vector?, do: reject_error(Evision.Mat.reshape(bm, [n, 1])), else: bm
      solve_one(am, bm)
    end)
    |> then(&stack_f64(out, &1))
  end

  defp solve_one(a, b) do
    case Evision.solve(a, b, flags: Evision.DecompTypes.cv_DECOMP_LU()) do
      %Evision.Mat{} = x -> x
      {:error, msg} -> raise RuntimeError, msg
      _ -> raise ArgumentError, "can't solve for singular matrix"
    end
  end

  def svd({u_out, s_out, vt_out}, %T{shape: shape} = tensor, opts) do
    {_batch, m, n} = mat_dims(shape)
    flags = if opts[:full_matrices?], do: Evision.SVD.Flags.cv_FULL_UV(), else: 0

    results =
      for am <- split_2d(tensor, m, n) do
        {w, u, vt} = Evision.svdDecomp(am, flags: flags)
        {u, w, vt}
      end

    {stack_f64(u_out, Enum.map(results, &elem(&1, 0))),
     stack_f64(s_out, Enum.map(results, &elem(&1, 1))),
     stack_f64(vt_out, Enum.map(results, &elem(&1, 2)))}
  end

  def eigh({vals_out, vecs_out}, %T{shape: shape} = tensor, _opts) do
    {_batch, n, _n} = mat_dims(shape)

    results =
      for am <- split_2d(tensor, n, n) do
        {evals, evecs} = Evision.eigen(am)
        # cv stores eigenvectors as rows in descending-eigenvalue order; Nx wants them as
        # columns (column i pairing with eigenvalue i), so transpose.
        {evals, reject_error(Evision.Mat.transpose(evecs, [1, 0], as_shape: {n, n}))}
      end

    {stack_f64(vals_out, Enum.map(results, &elem(&1, 0))),
     stack_f64(vecs_out, Enum.map(results, &elem(&1, 1)))}
  end

  def cholesky(%T{shape: shape} = out, tensor) do
    {_batch, n, _n} = mat_dims(shape)

    split_2d(tensor, n, n)
    |> Enum.map(&reject_error(Evision.Mat.cholesky(&1, n)))
    |> then(&stack_f64(out, &1))
  end

  def lu({p_out, l_out, u_out}, %T{shape: shape} = tensor, _opts) do
    {_batch, n, _n} = mat_dims(shape)
    results = split_2d(tensor, n, n) |> Enum.map(&reject_error_tuple(Evision.Mat.lu(&1, n)))

    {stack_f64(p_out, Enum.map(results, &elem(&1, 0))),
     stack_f64(l_out, Enum.map(results, &elem(&1, 1))),
     stack_f64(u_out, Enum.map(results, &elem(&1, 2)))}
  end

  def qr({q_out, r_out}, %T{shape: shape} = tensor, opts) do
    {_batch, m, n} = mat_dims(shape)
    complete = bool_int(opts[:mode] == :complete)
    results = split_2d(tensor, m, n) |> Enum.map(&reject_error_tuple(Evision.Mat.qr(&1, m, n, complete)))

    {stack_f64(q_out, Enum.map(results, &elem(&1, 0))),
     stack_f64(r_out, Enum.map(results, &elem(&1, 1)))}
  end

  @impl true
  def triangular_solve(out, %T{shape: a_shape} = a, %T{shape: b_shape} = b, opts) do
    {batch, n, _n} = mat_dims(a_shape)
    {b_trailing, vector?} = rhs_trailing(a_shape, b_shape)
    lower = bool_int(opts[:lower])
    left = bool_int(opts[:left_side])
    trans = bool_int(opts[:transform_a] == :transpose)

    Enum.zip_with(split_2d(a, n, n), split_into(b, batch, b_trailing), fn am, bm ->
      bm = if vector?, do: reject_error(Evision.Mat.reshape(bm, [n, 1])), else: bm
      reject_error(Evision.Mat.triangular_solve(am, bm, n, lower, left, trans))
    end)
    |> then(&stack_f64(out, &1))
  end

  ## Windowed (pooling) ops

  @window_sum 0
  @window_product 1
  @window_max 2
  @window_min 3

  @impl true
  def window_sum(out, tensor, window_dims, opts),
    do: window_reduce_op(out, tensor, window_dims, opts, @window_sum)

  @impl true
  def window_product(out, tensor, window_dims, opts),
    do: window_reduce_op(out, tensor, window_dims, opts, @window_product)

  @impl true
  def window_max(out, tensor, window_dims, opts),
    do: window_reduce_op(out, tensor, window_dims, opts, @window_max)

  @impl true
  def window_min(out, tensor, window_dims, opts),
    do: window_reduce_op(out, tensor, window_dims, opts, @window_min)

  # Sliding-window reduction; output keeps the input type (f16/bf16 widened to f32). `out`
  # already carries the pooled shape; padding/strides/dilations come normalized from Nx.
  defp window_reduce_op(%T{type: out_type, shape: out_shape} = out, %T{shape: in_shape} = tensor, window_dims, opts, op) do
    rank = tuple_size(in_shape)
    work_type = if out_type in @half_types, do: {:f, 32}, else: out_type
    pad_lo = Enum.map(opts[:padding], &elem(&1, 0))

    from_nx(tensor)
    |> as_mat_type(work_type)
    |> Evision.Mat.window_reduce(
      Tuple.to_list(in_shape),
      Tuple.to_list(out_shape),
      Tuple.to_list(window_dims),
      norm_list(opts[:strides], rank),
      pad_lo,
      norm_list(opts[:window_dilations], rank),
      op
    )
    |> reject_error()
    |> maybe_narrow(work_type, out_type)
    |> Evision.Mat.reshape(reduce_out_dims(out_shape))
    |> reject_error()
    |> to_nx(out)
  end

  @window_scatter_max 0
  @window_scatter_min 1

  @impl true
  def window_scatter_max(out, tensor, source, init_value, window_dims, opts),
    do: window_scatter(out, tensor, source, init_value, window_dims, opts, @window_scatter_max)

  @impl true
  def window_scatter_min(out, tensor, source, init_value, window_dims, opts),
    do: window_scatter(out, tensor, source, init_value, window_dims, opts, @window_scatter_min)

  # Select-and-scatter; input + source cast to the output type (f16/bf16 widened to f32). The
  # source carries one value per window (its shape is the window grid).
  defp window_scatter(%T{type: out_type, shape: in_shape} = out, tensor, %T{shape: grid} = source, init_value, window_dims, opts, op) do
    rank = tuple_size(in_shape)
    work_type = if out_type in @half_types, do: {:f, 32}, else: out_type
    pad_lo = Enum.map(opts[:padding], &elem(&1, 0))
    src = as_mat_type(from_nx(tensor), work_type)
    vals = as_mat_type(from_nx(source), work_type)

    Evision.Mat.window_scatter(
      src,
      vals,
      to_number(init_value) * 1.0,
      Tuple.to_list(in_shape),
      Tuple.to_list(window_dims),
      norm_list(opts[:strides], rank),
      pad_lo,
      Tuple.to_list(grid),
      op
    )
    |> reject_error()
    |> maybe_narrow(work_type, out_type)
    |> Evision.Mat.reshape(reduce_out_dims(in_shape))
    |> reject_error()
    |> to_nx(out)
  end

  defp norm_list(v, rank) when is_integer(v), do: List.duplicate(v, rank)
  defp norm_list(v, rank) when is_nil(v), do: List.duplicate(1, rank)
  defp norm_list(v, _rank) when is_list(v), do: v

  ## Convolution

  # N-d cross-correlation. The input/kernel/output permutations and the float cast are done
  # here (via the implemented transpose/as_type), so the NIF only does the canonical-layout
  # correlation; the result is permuted back to the output layout. f16/bf16 widen to f32.
  @impl true
  def conv(%T{type: out_type, shape: out_shape} = out, %T{shape: in_shape} = tensor, kernel, opts) do
    work_type = if out_type in @half_types, do: {:f, 32}, else: out_type
    op = opts[:output_permutation]
    d = tuple_size(in_shape) - 2

    in_t = tensor |> Nx.transpose(axes: opts[:input_permutation]) |> Nx.as_type(work_type)
    k_t = kernel |> Nx.transpose(axes: opts[:kernel_permutation]) |> Nx.as_type(work_type)
    canon_shape = op |> Enum.map(&elem(out_shape, &1)) |> List.to_tuple()

    Evision.Mat.conv(
      from_nx(in_t),
      from_nx(k_t),
      Tuple.to_list(in_t.shape),
      Tuple.to_list(k_t.shape),
      Tuple.to_list(canon_shape),
      norm_list(opts[:strides], d),
      Enum.map(opts[:padding], &elem(&1, 0)),
      norm_list(opts[:input_dilation], d),
      norm_list(opts[:kernel_dilation], d),
      opts[:feature_group_size],
      opts[:batch_group_size],
      conv_im2row_budget()
    )
    |> reject_error()
    |> Evision.Mat.reshape(Tuple.to_list(canon_shape))
    |> reject_error()
    |> to_nx(%{out | shape: canon_shape, type: work_type})
    |> Nx.transpose(axes: inverse_perm(op))
    |> Nx.as_type(out_type)
    |> from_nx()
    |> to_nx(out)
  end

  # Memory budget (bytes) for the conv im2row fast-path buffers; 0 = NIF default. The
  # process-local override is set per-test (each ExUnit test runs in its own process, so
  # it never leaks across parallel tests); the application config is the global knob.
  defp conv_im2row_budget do
    Process.get(:evision_conv_im2row_budget_bytes) ||
      Application.get_env(:evision, :conv_im2row_budget_bytes, 0)
  end

  # {product of leading dims, second-last dim, last dim} for a batched matrix shape.
  defp mat_dims(shape) do
    [n, m | lead] = shape |> Tuple.to_list() |> Enum.reverse()
    {Enum.product(lead), m, n}
  end

  # b's per-matrix trailing shape (drop a's leading batch dims) + whether it is a vector.
  defp rhs_trailing(a_shape, b_shape) do
    trailing =
      b_shape |> Tuple.to_list() |> Enum.drop(tuple_size(a_shape) - 2) |> List.to_tuple()

    {trailing, tuple_size(trailing) == 1}
  end

  # Split a batched {batch.., rows, cols} tensor into a list of {rows, cols} f64 mats.
  defp split_2d(tensor, rows, cols),
    do: split_into(tensor, div(Nx.size(tensor), rows * cols), {rows, cols})

  defp split_into(tensor, batch, trailing) do
    bin = tensor |> from_nx() |> as_mat_type({:f, 64}) |> to_binary_f64()
    chunk = div(byte_size(bin), batch)

    for i <- 0..(batch - 1) do
      binary_part(bin, i * chunk, chunk)
      |> Evision.Mat.from_binary_by_shape({:f, 64}, trailing)
      |> reject_error()
    end
  end

  defp to_binary_f64(mat), do: to_binary_mat!(mat)

  defp to_binary_mat!(mat) do
    case Evision.Mat.to_binary(mat, 0) do
      {:error, msg} -> raise RuntimeError, msg
      binary -> binary
    end
  end

  # Concatenate f64 mats (batch order) then reshape/cast to the output template.
  defp stack_f64(%T{type: out_type, shape: shape} = out, mats) do
    bin = mats |> Enum.map(&to_binary_f64/1) |> IO.iodata_to_binary()

    Evision.Mat.from_binary_by_shape(bin, {:f, 64}, shape)
    |> reject_error()
    |> as_mat_type(out_type)
    |> to_nx(out)
  end

  defp bool_int(true), do: 1
  defp bool_int(_), do: 0

  defp reject_error_tuple({:error, msg}), do: raise(RuntimeError, msg)
  defp reject_error_tuple(tuple) when is_tuple(tuple), do: tuple

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
