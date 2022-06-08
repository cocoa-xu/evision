defmodule Evision.Backend do
  @behaviour Nx.Backend
  defstruct [:ref]

  alias Nx.Tensor, as: T
  alias Evision.Backend, as: EB

  ## Creation

  @impl true
  def constant(%T{shape: {}, type: type} = out, scalar, backend_options) do
    Evision.Mat.number!(scalar, type)
    |> to_nx(out)
  end

  def constant(%T{shape: shape, type: type} = out, scalar, backend_options) do
    shape
    |> Evision.Mat.full!(
         scalar,
         type
       )
    |> to_nx(out)
  end

  @impl true
  def from_binary(%T{shape: {count}, type: type} = out, binary, _backend_options) when is_binary(binary) do
      Evision.Mat.from_binary!(binary, type, 1, count, 1) |> to_nx(out)
  end

  def from_binary(%T{shape: {rows, cols}, type: type} = out, binary, _backend_options) when is_binary(binary) do
    Evision.Mat.from_binary!(binary, type, rows, cols, 1) |> to_nx(out)
  end

  def from_binary(%T{shape: shape, type: type} = out, binary, _backend_options) when is_binary(binary) do
    Evision.Mat.from_binary_by_shape!(binary, type, shape) |> to_nx(out)
  end

  @impl true
  def eye(%T{shape: {n, n}, type: type} = out, _backend_options) do
    Evision.Mat.eye!(n, type) |> to_nx(out)
  end

  @impl true
  def iota(%T{shape: {}, type: type} = out, nil, backend_options) do
    Evision.Mat.arange!(0, 1, 1, type)
    |> to_nx(out)
  end

  def iota(%T{shape: shape, type: type} = out, nil, backend_options) do
    Evision.Mat.arange!(
      0,
      Nx.size(shape),
      1,
      type,
      shape
    )
    |> to_nx(out)
  end

  @impl true
  def iota(%T{shape: {n}, type: type} = out, 0, backend_options) do
    Evision.Mat.arange!(0, n, 1, type) |> to_nx(out)
  end

  def iota(%T{shape: shape, type: type} = out, axis, backend_options) do
    # gets the size of iota
    dim = elem(shape, axis)

    # build the iota in one dimension
    aten = Evision.Mat.arange!(0, dim, 1, type)

    # reshape the tensor above to be have shape where everything is 1, except for dim
    reshape = Tuple.duplicate(1, Nx.rank(shape)) |> put_elem(axis, dim)
    aten = Evision.Mat.reshape!(aten, reshape)

    # Now broadcast the tensor using the original shape
    Evision.Mat.broadcast_to!(aten, shape) |> to_nx(out)
  end

  @impl true
  def random_uniform(%T{type: {s, _} = type, shape: shape} = out, min, max, backend_options)
      when s in [:u, :s, :f] do
    min = to_number(min)
    max = to_number(max)

    Evision.randu!(
      Evision.Mat.zeros!(shape, type),
      Evision.Mat.number!(min, type),
      Evision.Mat.number!(max, type)
    )
    |> Evision.Mat.as_type!(type)
    |> to_nx(out)
  end

  def random_uniform(%T{type: {:c, s}, shape: _shape} = _out, _min, _max, _backend_options) do
    raise ArgumentError, "Complex number is not support at Evision.Backend"
  end

  @impl true
  def random_normal(%T{type: {s, _} = type, shape: shape} = out, mu, sigma, backend_options)
      when s in [:u, :s, :f] do
    mu = to_number(mu)
    sigma = to_number(sigma)

    Evision.randn!(
      Evision.Mat.zeros!(shape, type),
      Evision.Mat.number!(mu, type),
      Evision.Mat.number!(sigma, type)
    )
    |> Evision.Mat.as_type!(type)
    |> to_nx(out)
  end

  def random_normal(%T{type: {:c, s}, shape: _shape} = _out, _min, _max, _backend_options) do
    raise ArgumentError, "Complex number is not support at Evision.Backend"
  end

  @impl true
  def backend_deallocate(%T{data: %EB{ref: mat}} = t) do
    Evision.Mat.release(mat)
  end

  @impl true
  def backend_copy(tensor, Nx.Tensor, opts) do
    backend_copy(tensor, Nx.BinaryBackend, opts)
  end

  def backend_copy(tensor, Evision.Backend, _opts) do
    Evision.Mat.clone!(from_nx(tensor)) |> to_nx(tensor)
  end

  def backend_copy(tensor, backend, opts) do
    backend.from_binary(tensor, Evision.Mat.to_binary!(from_nx(tensor), 0), opts)
  end

  @impl true
  def backend_transfer(tensor, backend, opts) do
    backend_copy(tensor, backend, opts)
  after
    backend_deallocate(tensor)
  end

  @impl true
  def to_binary(%T{data: %EB{ref: mat}} = tensor, limit) when is_reference(mat) and is_integer(limit) and limit >= 0 do
    Evision.Mat.to_binary!(mat, limit)
  end

  @impl true
  def inspect(%T{data: %EB{ref: mat}} = tensor, inspect_opts) do
    limit = if inspect_opts.limit == :infinity, do: :infinity, else: inspect_opts.limit + 1

    mat
    |> Evision.Mat.to_binary!(min(limit, Nx.size(tensor)))
    |> then(&Nx.Backend.inspect(tensor, &1, inspect_opts))
    |> maybe_add_signature(tensor)
  end

  @impl true
  def as_type(%T{type: type} = out, %T{data: %EB{ref: mat}} = t) do
    Evision.Mat.as_type!(mat, type) |> to_nx(out)
  end

  @impl true
  def bitcast(out, tensor), do: from_binary(out, Evision.Mat.to_binary!(from_nx(tensor), 0), [])

  @impl true
  def reshape(%T{shape: shape} = out, %T{data: %EB{ref: mat}} = t) do
    Evision.Mat.reshape!(mat, Tuple.to_list(shape)) |> to_nx(out)
  end

  @impl true
  def squeeze(out, %T{data: %EB{ref: mat}} = t, _axes) do
    Evision.Mat.squeeze!(mat) |> to_nx(out)
  end

  @impl true
  def broadcast(out, %T{} = t, shape, axes) do
    {tensor, reshape} = maybe_reshape(t, shape, axes)
    Evision.Mat.broadcast_to!(tensor |> from_nx(), shape, reshape)
    |> to_nx(out)
  end

  defp maybe_reshape(%T{shape: {n}} = t, {n, _}, [0]), do: {Nx.reshape(t, {n, 1}), {n, 1}}
  defp maybe_reshape(%T{shape: shape} = t, to_shape, axes) do
    l_shape = Tuple.to_list(shape)
    l_to_shape = Tuple.to_list(to_shape)

    if length(l_shape) != length(l_to_shape) do
      src_shape_axis = length(l_shape) - 1
      {src_shape_axis, force_shape} =
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
  def clip(%T{} = out, %T{} = t, %T{} = min, %T{} = max) do
    t
    |> Nx.as_type(out.type)
    |> from_nx()
    |> Evision.Mat.clip!(to_number(min), to_number(max))
    |> to_nx(out)
  end

  @impl true
  def abs(%T{type: type} = out, tensor) do
    Evision.Mat.abs!(from_nx(tensor))
    |> to_nx(out)
  end

  @impl true
  def bitwise_not(%T{type: {s, _}} = out, tensor) when s in [:s, :u] do
    Evision.Mat.bitwise_not!(from_nx(tensor))
    |> to_nx(out)
  end

  def bitwise_not(%T{type: type} = out, tensor) do
    raise ArgumentError, "bitwise operators expect integer tensors as inputs " <>
                         "and outputs an integer tensor, got: #{inspect(type)}"
  end

  @impl true
  def ceil(%T{type: {s, _}} = out, tensor) when s == :f do
    Evision.Mat.ceil!(from_nx(tensor))
    |> to_nx(out)
  end

  def ceil(%T{} = out, tensor) do
    to_nx(from_nx(tensor), out)
  end

  @impl true
  def floor(%T{type: {s, _}} = out, tensor) when s == :f do
    Evision.Mat.floor!(from_nx(tensor))
    |> to_nx(out)
  end

  def floor(%T{} = out, tensor) do
    to_nx(from_nx(tensor), out)
  end

  @impl true
  def negate(%T{} = out, tensor) do
    Evision.Mat.negate!(from_nx(tensor))
    |> to_nx(out)
  end

  @impl true
  def round(%T{type: {s, _}} = out, tensor) when s == :f do
    Evision.Mat.round!(from_nx(tensor))
    |> to_nx(out)
  end

  def round(%T{} = out, tensor) do
    to_nx(from_nx(tensor), out)
  end

  @impl true
  def sign(%T{} = out, tensor) do
    Evision.Mat.sign!(from_nx(tensor))
    |> to_nx(out)
  end

  @impl true
  def exp(%T{type: type} = out, tensor) do
    Evision.exp!(from_nx(tensor))
    |> to_nx(out)
  end

  @impl true
  def expm1(%T{type: type} = out, tensor) do
    Evision.Mat.expm1!(from_nx(tensor))
    |> to_nx(out)
  end

  @impl true
  def log(%T{type: type} = out, tensor) do
    Evision.log!(from_nx(tensor))
    |> to_nx(out)
  end

  @doc false
  def from_nx(%T{data: %EB{ref: mat_ref}}), do: mat_ref
  def from_nx(%T{} = tensor), do: Nx.backend_transfer(tensor, EB) |> from_nx()

  @doc false
  def to_nx(mat_ref, %T{type: type, shape: shape} = t)
      when is_reference(mat_ref) do
    %{t | data: %__MODULE__{ref: check_shape_and_type!(mat_ref, shape, type)}}
  end

  defp to_number(n) when is_number(n), do: n
  defp to_number(%T{} = t), do: t |> from_nx() |> Evision.Mat.at!(0)

  if Application.compile_env(:evision, :check_shape_and_type, false) do
    defp check_shape_and_type!(mat_ref, shape, type) do
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
    defp check_shape_and_type!(mat_ref, _, _), do: mat_ref
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
end
