defmodule Evision.Backend do
  @behaviour Nx.Backend
  defstruct [:ref]

  alias Nx.Tensor, as: T
  alias Evision.Backend, as: EB

  ## Creation

  @impl true
  def from_binary(%T{shape: {count}, type: type} = out, binary, _backend_options) when is_binary(binary) do
      Evision.Mat.from_binary!(binary, type, 1, count, 1) |> to_nx(out)
  end

  def from_binary(%T{shape: {rows, cols}, type: type} = out, binary, _backend_options) when is_binary(binary) do
    Evision.Mat.from_binary!(binary, type, rows, cols, 1) |> to_nx(out)
  end

  def from_binary(%T{shape: {rows, cols, channels}, type: type} = out, binary, _backend_options) when is_binary(binary) do
    Evision.Mat.from_binary!(binary, type, rows, cols, channels) |> to_nx(out)
  end

  def from_binary(%T{shape: shape, type: type} = out, binary, _backend_options) when is_binary(binary) do
    Evision.Mat.from_binary_by_shape!(binary, type, shape) |> to_nx(out)
  end

  @impl true
  def eye(%T{shape: {n, n}, type: type} = out, _backend_options) do
    Evision.Mat.eye!(n, type) |> to_nx(out)
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
  def reshape(%T{shape: shape} = out, %T{data: %EB{ref: mat}} = t) do
    Evision.Mat.reshape!(mat, Tuple.to_list(shape)) |> to_nx(out)
  end

  @impl true
  def squeeze(out, %T{data: %EB{ref: mat}} = t, _axes) do
    Evision.Mat.squeeze!(mat) |> to_nx(out)
  end

  @doc false
  def to_nx(mat_ref, %T{type: type, shape: shape} = t)
      when is_reference(mat_ref) do
    %{t | data: %__MODULE__{ref: check_shape_and_type!(mat_ref, shape, type)}}
  end

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