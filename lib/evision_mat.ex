defmodule Evision.Mat do
  @moduledoc """
  OpenCV Mat
  """

  import Evision.Errorize
  import Kernel, except: [abs: 1, floor: 1, ceil: 1, round: 1]

  @typedoc """
  Types for mat
  """
  @type mat_type ::
          {:u, 8}
          | {:u, 16}
          | {:s, 8}
          | {:s, 16}
          | {:s, 32}
          | {:f, 32}
          | {:f, 64}
  @type channels_from_binary ::
          1 | 3 | 4

  @doc namespace: :"cv.Mat"
  def number(number, type) do
    type = check_unsupported_type(type)
    Evision.Mat.full({1, 1}, number, type)
  end

  deferror(number(number, type))

  @doc namespace: :"cv.Mat"
  @spec at(reference(), non_neg_integer()) :: {:ok, number()} | {:error, String.t()}
  def at(mat, position) when is_reference(mat) and is_integer(position) and position >= 0 do
    :evision_nif.mat_at(img: mat, pos: position)
  end

  deferror(at(mat, position))

  @doc namespace: :"cv.Mat"
  @spec add(reference(), reference()) :: {:ok, reference()} | {:error, String.t()}
  def add(lhs, rhs) when is_reference(lhs) and is_reference(rhs) do
    :evision_nif.mat_add(l: lhs, r: rhs)
  end

  deferror(add(lhs, rhs))

  @doc namespace: :"cv.Mat"
  @spec add(reference(), reference(), mat_type()) :: {:ok, reference()} | {:error, String.t()}
  def add(lhs, rhs, type) when is_reference(lhs) and is_reference(rhs) do
    {t, l} = check_unsupported_type(type)
    :evision_nif.mat_add_typed(lhs: lhs, rhs: rhs, t: t, l: l)
  end

  deferror(add(lhs, rhs, type))

  @doc namespace: :"cv.Mat"
  @spec subtract(reference(), reference()) :: {:ok, reference()} | {:error, String.t()}
  def subtract(lhs, rhs) when is_reference(lhs) and is_reference(rhs) do
    :evision_nif.mat_subtract(l: lhs, r: rhs)
  end

  deferror(subtract(lhs, rhs))

  @doc namespace: :"cv.Mat"
  @spec subtract(reference(), reference(), mat_type()) ::
          {:ok, reference()} | {:error, String.t()}
  def subtract(lhs, rhs, type) when is_reference(lhs) and is_reference(rhs) do
    {t, l} = check_unsupported_type(type)
    :evision_nif.mat_subtract_typed(lhs: lhs, rhs: rhs, t: t, l: l)
  end

  deferror(subtract(lhs, rhs, type))

  @doc namespace: :"cv.Mat"
  @spec multiply(reference(), reference()) :: {:ok, reference()} | {:error, String.t()}
  def multiply(lhs, rhs) when is_reference(lhs) and is_reference(rhs) do
    :evision_nif.mat_multiply(l: lhs, r: rhs)
  end

  deferror(multiply(lhs, rhs))

  @doc namespace: :"cv.Mat"
  @spec multiply(reference(), reference(), mat_type()) ::
          {:ok, reference()} | {:error, String.t()}
  def multiply(lhs, rhs, type) when is_reference(lhs) and is_reference(rhs) do
    {t, l} = check_unsupported_type(type)
    :evision_nif.mat_multiply_typed(lhs: lhs, rhs: rhs, t: t, l: l)
  end

  deferror(multiply(lhs, rhs, type))

  @doc namespace: :"cv.Mat"
  @spec matrix_multiply(reference(), reference(), mat_type() | nil) ::
          {:ok, reference()} | {:error, String.t()}
  def matrix_multiply(lhs, rhs, out_type = {t, l} \\ nil)
      when is_reference(lhs) and is_reference(rhs) do
    if out_type == nil do
      :evision_nif.mat_matrix_multiply(lhs: lhs, rhs: rhs, t: nil, l: 0)
    else
      :evision_nif.mat_matrix_multiply(lhs: lhs, rhs: rhs, t: t, l: l)
    end
  end

  deferror(matrix_multiply(lhs, rhs))
  deferror(matrix_multiply(lhs, rhs, out_type))

  @doc namespace: :"cv.Mat"
  @spec divide(reference(), reference()) :: {:ok, reference()} | {:error, String.t()}
  def divide(lhs, rhs) when is_reference(lhs) and is_reference(rhs) do
    :evision_nif.mat_divide(l: lhs, r: rhs)
  end

  deferror(divide(lhs, rhs))

  @doc namespace: :"cv.Mat"
  @spec divide(reference(), reference(), mat_type()) :: {:ok, reference()} | {:error, String.t()}
  def divide(lhs, rhs, type) when is_reference(lhs) and is_reference(rhs) do
    {t, l} = check_unsupported_type(type)
    :evision_nif.mat_divide_typed(lhs: lhs, rhs: rhs, t: t, l: l)
  end

  deferror(divide(lhs, rhs, type))

  @doc namespace: :"cv.Mat"
  @spec bitwise_and(reference(), reference()) :: {:ok, reference()} | {:error, String.t()}
  def bitwise_and(lhs, rhs) when is_reference(lhs) and is_reference(rhs) do
    :evision_nif.mat_bitwise_and(l: lhs, r: rhs)
  end

  deferror(bitwise_and(lhs, rhs))

  @doc namespace: :"cv.Mat"
  @spec bitwise_or(reference(), reference()) :: {:ok, reference()} | {:error, String.t()}
  def bitwise_or(lhs, rhs) when is_reference(lhs) and is_reference(rhs) do
    :evision_nif.mat_bitwise_or(l: lhs, r: rhs)
  end

  deferror(bitwise_or(lhs, rhs))

  @doc namespace: :"cv.Mat"
  @spec bitwise_xor(reference(), reference()) :: {:ok, reference()} | {:error, String.t()}
  def bitwise_xor(lhs, rhs) when is_reference(lhs) and is_reference(rhs) do
    :evision_nif.mat_bitwise_xor(l: lhs, r: rhs)
  end

  deferror(bitwise_xor(lhs, rhs))

  @doc namespace: :"cv.Mat"
  @spec cmp(reference(), reference(), atom()) :: {:ok, reference()} | {:error, String.t()}
  def cmp(lhs, rhs, op)
      when is_reference(lhs) and is_reference(rhs) and op in [:eq, :gt, :ge, :lt, :le, :ne] do
    :evision_nif.mat_cmp(l: lhs, r: rhs, type: op)
  end

  deferror(cmp(lhs, rhs, op))

  @doc namespace: :"cv.Mat"
  @spec logical_and(reference(), reference()) :: {:ok, reference()} | {:error, String.t()}
  def logical_and(lhs, rhs) when is_reference(lhs) and is_reference(rhs) do
    :evision_nif.mat_logical_and(l: lhs, r: rhs)
  end

  deferror(logical_and(lhs, rhs))

  @doc namespace: :"cv.Mat"
  @spec logical_or(reference(), reference()) :: {:ok, reference()} | {:error, String.t()}
  def logical_or(lhs, rhs) when is_reference(lhs) and is_reference(rhs) do
    :evision_nif.mat_logical_or(l: lhs, r: rhs)
  end

  deferror(logical_or(lhs, rhs))

  @doc namespace: :"cv.Mat"
  @spec logical_xor(reference(), reference()) :: {:ok, reference()} | {:error, String.t()}
  def logical_xor(lhs, rhs) when is_reference(lhs) and is_reference(rhs) do
    :evision_nif.mat_logical_xor(l: lhs, r: rhs)
  end

  deferror(logical_xor(lhs, rhs))

  @doc namespace: :"cv.Mat"
  @spec abs(reference()) :: {:ok, reference()} | {:error, String.t()}
  def abs(mat) when is_reference(mat) do
    :evision_nif.mat_abs(img: mat)
  end

  deferror(abs(mat))

  @doc namespace: :"cv.Mat"
  @spec expm1(reference()) :: {:ok, reference()} | {:error, String.t()}
  def expm1(mat) when is_reference(mat) do
    :evision_nif.mat_expm1(img: mat)
  end

  deferror(expm1(mat))

  @doc namespace: :"cv.Mat"
  @spec clip(reference(), number(), number()) :: {:ok, reference()} | {:error, String.t()}
  def clip(mat, lower, upper)
      when is_reference(mat) and is_number(lower) and
             is_number(upper) and lower <= upper do
    :evision_nif.mat_clip(img: mat, lower: lower, upper: upper)
  end

  deferror(clip(mat, lower, upper))

  @doc """
  Transpose a matrix

  ## Parameters

    - `mat`. The matrix.
    - `axes`. list of ints.
        It must be a list which contains a permutation of [0,1,..,N-1]
        where N is the number of axes of `mat`. The i’th axis of the returned array will correspond to the
        axis numbered axes[i] of the input.

    - `opts`. Keyword options.
        - `as_shape`. A tuple or list which overwrites the shape of the matrix (the total number of elements
          must be equal to the one as in its original shape). For example, a 4x4 matrix can be treated as a
          2x2x2x2 matrix and transposed with `axes=[2,1,3,0]` in a single call.

          When specified, it combines the reshape and transpose operation in a single NIF call.

  """
  @spec transpose(reference(), [non_neg_integer()], keyword()) ::
          {:ok, reference()} | {:error, String.t()}
  def transpose(mat, axes, opts \\ []) do
    as_shape = opts[:as_shape] || shape(mat)

    as_shape =
      case is_tuple(as_shape) do
        true ->
          Tuple.to_list(as_shape)

        _ ->
          as_shape
      end

    ndims = Enum.count(as_shape)

    uniq_axes =
      Enum.uniq(axes)
      |> Enum.reject(fn axis ->
        axis < 0 or axis > ndims
      end)

    if Enum.count(uniq_axes) != ndims do
      {:error, "invalid transpose axes #{inspect(axes)} for shape #{inspect(as_shape)}"}
    else
      :evision_nif.mat_transpose(img: mat, axes: uniq_axes, as_shape: as_shape)
    end
  end

  deferror(transpose(mat, axes, opts))

  @doc """
  Transpose a matrix

  ## Parameters

    - `mat`. The matrix.
      by default it reverses the order of the axes.

  """
  @spec transpose(reference(), keyword()) :: {:ok, reference()} | {:error, String.t()}
  def transpose(mat) do
    as_shape = shape(mat)
    ndims = Enum.count(as_shape)
    uniq_axes = Enum.reverse(0..(ndims - 1))
    :evision_nif.mat_transpose(img: mat, axes: uniq_axes, as_shape: as_shape)
  end

  deferror(transpose(mat))

  @doc namespace: :"cv.Mat"
  @spec type(reference()) :: {:ok, mat_type()} | {:error, String.t()}
  def type(mat) when is_reference(mat) do
    :evision_nif.mat_type(img: mat)
  end

  deferror(type(mat))

  @doc namespace: :"cv.Mat"
  @spec bitwise_not(reference()) :: {:ok, reference()} | {:error, String.t()}
  def bitwise_not(mat) when is_reference(mat) do
    type = {s, _} = Evision.Mat.type!(mat)

    if s in [:s, :u] do
      :evision_nif.mat_bitwise_not(img: mat)
    else
      {:error,
       "bitwise operators expect integer tensors as inputs and outputs an integer tensor, got: #{inspect(type)}"}
    end
  end

  deferror(bitwise_not(mat))

  @doc namespace: :"cv.Mat"
  @spec ceil(reference()) :: {:ok, reference()} | {:error, String.t()}
  def ceil(mat) when is_reference(mat) do
    :evision_nif.mat_ceil(img: mat)
  end

  deferror(ceil(mat))

  @doc namespace: :"cv.Mat"
  @spec floor(reference()) :: {:ok, reference()} | {:error, String.t()}
  def floor(mat) when is_reference(mat) do
    :evision_nif.mat_floor(img: mat)
  end

  deferror(floor(mat))

  @doc namespace: :"cv.Mat"
  @spec negate(reference()) :: {:ok, reference()} | {:error, String.t()}
  def negate(mat) when is_reference(mat) do
    :evision_nif.mat_negate(img: mat)
  end

  deferror(negate(mat))

  @doc namespace: :"cv.Mat"
  @spec round(reference()) :: {:ok, reference()} | {:error, String.t()}
  def round(mat) when is_reference(mat) do
    :evision_nif.mat_round(img: mat)
  end

  deferror(round(mat))

  @doc namespace: :"cv.Mat"
  @spec sign(reference()) :: {:ok, reference()} | {:error, String.t()}
  def sign(mat) when is_reference(mat) do
    :evision_nif.mat_sign(img: mat)
  end

  deferror(sign(mat))

  @doc namespace: :"cv.Mat"
  @spec setTo(reference(), number(), reference()) :: {:ok, reference()} | {:error, String.t()}
  def setTo(mat, value, mask) when is_reference(mat) do
    :evision_nif.mat_set_to(img: mat, value: value, mask: mask)
  end

  deferror(setTo(mat, value, mask))

  @doc namespace: :"cv.Mat"
  @spec dot(reference(), reference()) :: {:ok, reference()} | {:error, String.t()}
  def dot(mat_a, mat_b) when is_reference(mat_a) and is_reference(mat_b) do
    :evision_nif.mat_dot(a: mat_a, b: mat_b)
  end

  deferror(dot(mat_a, mat_b))

  @doc namespace: :"cv.Mat"
  @spec as_type(reference(), mat_type()) :: {:ok, reference()} | {:error, String.t()}
  def as_type(mat, _type = {t, l}) when is_reference(mat) and is_atom(t) and l > 0 do
    :evision_nif.mat_as_type(img: mat, t: t, l: l)
  end

  deferror(as_type(mat, type))

  @doc namespace: :"cv.Mat"
  def shape(mat) when is_reference(mat) do
    :evision_nif.mat_shape(img: mat)
  end

  deferror(shape(mat))

  @doc namespace: :"cv.Mat"
  def zeros(shape, type) when is_tuple(shape) do
    {t, l} = check_unsupported_type(type)

    :evision_nif.mat_zeros(
      shape: Tuple.to_list(shape),
      t: t,
      l: l
    )
  end

  deferror(zeros(shape, type))

  @doc namespace: :"cv.Mat"
  def ones(shape, type) when is_tuple(shape) do
    {t, l} = check_unsupported_type(type)

    :evision_nif.mat_ones(
      shape: Tuple.to_list(shape),
      t: t,
      l: l
    )
  end

  deferror(ones(shape, type))

  def arange(from, to, step, type) when step != 0 do
    {t, l} = check_unsupported_type(type)

    with {:ok, mat} <-
           :evision_nif.mat_arange(
             from: from,
             to: to,
             step: step,
             t: t,
             l: l
           ) do
      {length, _} = Evision.Mat.shape!(mat)
      Evision.Mat.reshape(mat, {1, length})
    else
      error -> error
    end
  end

  deferror(arange(from, to, step, type))

  def arange(from, to, step, type, shape) when step != 0 do
    {t, l} = check_unsupported_type(type)

    with {:ok, mat} <-
           :evision_nif.mat_arange(
             from: from,
             to: to,
             step: step,
             t: t,
             l: l
           ) do
      Evision.Mat.reshape(mat, shape)
    else
      error -> error
    end
  end

  deferror(arange(from, to, step, type, shape))

  def full(shape, number, type) do
    {t, l} = check_unsupported_type(type)

    :evision_nif.mat_full(
      number: number,
      t: t,
      l: l,
      shape: Tuple.to_list(shape)
    )
  end

  deferror(full(shape, number, type))

  @doc namespace: :"cv.Mat"
  @spec clone(reference()) :: {:ok, reference()} | {:error, String.t()}
  def clone(mat) when is_reference(mat) do
    :evision_nif.mat_clone(img: mat)
  end

  deferror(clone(mat))

  @doc namespace: :"cv.Mat"
  @spec(empty() :: :ok, reference())
  def empty() do
    :evision_nif.mat_empty()
  end

  deferror(empty())

  @spec to_batched(reference(), pos_integer(), keyword()) ::
          {:ok, [reference()]} | {:error, String.t()}
  def to_batched(mat, batch_size, opts)
      when is_integer(batch_size) and batch_size >= 1 and is_list(opts) do
    leftover = opts[:leftover] || :repeat

    :evision_nif.mat_to_batched(
      img: mat,
      batch_size: batch_size,
      as_shape: shape!(mat),
      leftover: leftover
    )
  end

  deferror(to_batched(mat, batch_size, opts))

  @spec to_batched(reference(), pos_integer(), keyword()) ::
          {:ok, [reference()]} | {:error, String.t()}
  def to_batched(mat, batch_size, as_shape, opts)
      when is_integer(batch_size) and batch_size >= 1 and is_tuple(as_shape) and is_list(opts) do
    leftover = opts[:leftover] || :repeat

    :evision_nif.mat_to_batched(
      img: mat,
      batch_size: batch_size,
      as_shape: Tuple.to_list(as_shape),
      leftover: leftover
    )
  end

  deferror(to_batched(mat, batch_size, as_shape, opts))

  @doc namespace: :"cv.Mat"
  @spec to_binary(reference(), non_neg_integer()) :: {:ok, binary()} | {:error, String.t()}
  def to_binary(mat, limit \\ 0) when is_reference(mat) and is_integer(limit) and limit >= 0 do
    :evision_nif.mat_to_binary(img: mat, limit: limit)
  end

  deferror(to_binary(mat, limit))
  deferror(to_binary(mat))

  @doc """
  Create Mat from binary (pixel) data

  - **binary**. The binary pixel data
  - **type**. `type={t, l}` is one of [{:u, 8}, {:s, 8}, {:u, 16}, {:s, 16}, {:s, 32}, {:f, 32}, {:f, 64}]
  - **rows**. Number of rows (i.e., the height of the image)
  - **cols**. Number of cols (i.e., the width of the image)
  - **channels**. Number of channels, only valid if in [1, 3, 4]
  """
  @doc namespace: :"cv.Mat"
  @spec from_binary(binary(), mat_type(), pos_integer(), pos_integer(), channels_from_binary()) ::
          {:ok, reference()} | {:error, String.t()}
  def from_binary(binary, _type = {t, l}, rows, cols, channels)
      when is_binary(binary) and rows > 0 and cols > 0 and channels > 0 and
             is_atom(t) and is_integer(l) do
    :evision_nif.mat_from_binary(
      binary: binary,
      t: t,
      l: l,
      cols: cols,
      rows: rows,
      channels: channels
    )
  end

  deferror(from_binary(binary, type, rows, cols, channels))

  defp check_unsupported_type({:f, 32} = type), do: type
  defp check_unsupported_type({:f, 64} = type), do: type
  defp check_unsupported_type({:u, 8} = type), do: type
  defp check_unsupported_type({:u, 16} = type), do: type
  defp check_unsupported_type({:s, 8} = type), do: type
  defp check_unsupported_type({:s, 16} = type), do: type
  defp check_unsupported_type({:s, 32} = type), do: type
  defp check_unsupported_type(:f32), do: {:f, 32}
  defp check_unsupported_type(:f64), do: {:f, 64}
  defp check_unsupported_type(:u8), do: {:u, 8}
  defp check_unsupported_type(:u16), do: {:u, 16}
  defp check_unsupported_type(:s8), do: {:s, 8}
  defp check_unsupported_type(:s16), do: {:s, 16}
  defp check_unsupported_type(:s32), do: {:s, 32}

  defp check_unsupported_type(type) do
    case type do
      {t, l} when is_atom(t) and l > 0 ->
        :ok

      type when is_atom(type) ->
        :ok

      true ->
        raise_unsupported_type(type)
    end

    new_type =
      with {:ok, unsupported_type_map} <- Application.fetch_env(:evision, :unsupported_type_map) do
        Map.get(unsupported_type_map, type, :error)
      else
        _ -> :error
      end

    if new_type == :error do
      raise_unsupported_type(type)
    else
      check_unsupported_type(new_type)
    end
  end

  defp raise_unsupported_type(type) do
    raise ArgumentError,
          "#{inspect(type)} is not supported by OpenCV. However, it is possible to set an " <>
            "`unsupported_type_map` in config/config.exs to allow evision do type conversion automatically. " <>
            "Please see https://github.com/cocoa-xu/evision#unsupported-type-map for more details and examples."
  end

  @doc namespace: :"cv.Mat"
  def from_binary_by_shape(binary, _type = {t, l}, shape)
      when is_binary(binary) and is_atom(t) and is_integer(l) and is_tuple(shape) do
    from_binary_by_shape(binary, {t, l}, Tuple.to_list(shape))
  end

  @doc namespace: :"cv.Mat"
  def from_binary_by_shape(binary, _type = {t, l}, shape)
      when is_binary(binary) and is_atom(t) and is_integer(l) and is_list(shape) do
    :evision_nif.mat_from_binary_by_shape(
      binary: binary,
      t: t,
      l: l,
      shape: shape
    )
  end

  deferror(from_binary_by_shape(binary, type, shape))

  @doc namespace: :"cv.Mat"
  def eye(n, type) when is_integer(n) and n > 0 do
    {t, l} = check_unsupported_type(type)

    :evision_nif.mat_eye(
      n: n,
      t: t,
      l: l
    )
  end

  deferror(eye(n, type))

  @doc namespace: :"cv.Mat"
  def reshape(mat, shape) when is_reference(mat) and is_tuple(shape) do
    :evision_nif.mat_reshape(
      mat: mat,
      shape: Tuple.to_list(shape)
    )
  end

  def reshape(mat, shape) when is_reference(mat) and is_list(shape) do
    :evision_nif.mat_reshape(
      mat: mat,
      shape: shape
    )
  end

  deferror(reshape(mat, shape))

  def squeeze(mat) when is_reference(mat) do
    shape = Tuple.to_list(Evision.Mat.shape!(mat))
    Evision.Mat.reshape(mat, Enum.reject(shape, fn d -> d == 1 end))
  end

  deferror(squeeze(mat))

  def broadcast_to(mat, to_shape) do
    :evision_nif.mat_broadcast_to(
      img: mat,
      to_shape: Tuple.to_list(to_shape),
      force_src_shape: []
    )
  end

  deferror(broadcast_to(mat, to_shape))

  def broadcast_to(mat, to_shape, force_src_shape) do
    :evision_nif.mat_broadcast_to(
      img: mat,
      to_shape: Tuple.to_list(to_shape),
      force_src_shape: Tuple.to_list(force_src_shape)
    )
  end

  deferror(broadcast_to(mat, to_shape, force_src_shape))
end
