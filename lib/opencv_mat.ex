defmodule Evision.Mat do
  @moduledoc """
  OpenCV Mat
  """

  import Evision.Errorize
  import Kernel, except: [abs: 1]

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
    Evision.Mat.full({1, 1}, number, type)
  end

  deferror(number(number, type))

  @doc namespace: :"cv.Mat"
  @spec at(reference(), non_neg_integer()) :: {:ok, number()} | {:error, String.t()}
  def at(mat, position) when is_reference(mat) and is_integer(position) and position >= 0 do
    :erl_cv_nif.evision_cv_mat_at(img: mat, pos: position)
  end

  deferror(at(mat, position))

  @doc namespace: :"cv.Mat"
  @spec abs(reference()) :: {:ok, reference()} | {:error, String.t()}
  def abs(mat) when is_reference(mat) do
    :erl_cv_nif.evision_cv_mat_abs(img: mat)
  end

  deferror(abs(mat))

  @doc namespace: :"cv.Mat"
  @spec expm1(reference()) :: {:ok, reference()} | {:error, String.t()}
  def expm1(mat) when is_reference(mat) do
    :erl_cv_nif.evision_cv_mat_expm1(img: mat)
  end

  deferror(expm1(mat))

  @doc namespace: :"cv.Mat"
  @spec clip(reference(), number(), number()) :: {:ok, reference()} | {:error, String.t()}
  def clip(mat, lower, upper) when is_reference(mat) and is_number(lower) and
                                   is_number(upper) and lower <= upper do
    :erl_cv_nif.evision_cv_mat_clip(img: mat, lower: lower, upper: upper)
  end

  deferror(clip(mat, lower, upper))

  @doc namespace: :"cv.Mat"
  @spec type(reference()) :: {:ok, mat_type()} | {:error, String.t()}
  def type(mat) when is_reference(mat) do
    :erl_cv_nif.evision_cv_mat_type(img: mat)
  end

  deferror(type(mat))

  @doc namespace: :"cv.Mat"
  @spec setTo(reference(), number(), reference()) :: {:ok, mat_type()} | {:error, String.t()}
  def setTo(mat, value, mask) when is_reference(mat) do
    :erl_cv_nif.evision_cv_mat_set_to(img: mat, value: value, mask: mask)
  end

  deferror(setTo(mat, value, mask))

  @doc namespace: :"cv.Mat"
  @spec as_type(reference(), mat_type()) :: {:ok, reference()} | {:error, String.t()}
  def as_type(mat, _type = {t, l}) when is_reference(mat) and is_atom(t) and l > 0 do
    :erl_cv_nif.evision_cv_mat_as_type(img: mat, t: t, l: l)
  end

  deferror(as_type(mat, type))

  @doc namespace: :"cv.Mat"
  def shape(mat) when is_reference(mat) do
    :erl_cv_nif.evision_cv_mat_shape(img: mat)
  end

  deferror(shape(mat))

  @doc namespace: :"cv.Mat"
  def zeros(shape, _type = {t, l}) when is_tuple(shape) do
    :erl_cv_nif.evision_cv_mat_zeros(
      shape: Tuple.to_list(shape),
      t: t,
      l: l
    )
  end

  deferror(zeros(shape, type))

  @doc namespace: :"cv.Mat"
  def ones(shape, _type = {t, l}) when is_tuple(shape) do
    :erl_cv_nif.evision_cv_mat_ones(
      shape: Tuple.to_list(shape),
      t: t,
      l: l
    )
  end

  deferror(ones(shape, type))

  def arange(from, to, step, _type = {t, l}) when step != 0 do
    with {:ok, mat} <- :erl_cv_nif.evision_cv_mat_arange(
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

  def arange(from, to, step, _type = {t, l}, shape) when step != 0 do
    with {:ok, mat} <- :erl_cv_nif.evision_cv_mat_arange(
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

  def full(shape, number, _type = {t, l}) do
    :erl_cv_nif.evision_cv_mat_full(
      number: number,
      t: t,
      l: l,
      shape: Tuple.to_list(shape)
    )
  end

  deferror(full(shape, number, type))

  @doc namespace: :"cv.Mat"
  @spec release(reference()) :: :ok
  def release(mat) when is_reference(mat) do
    :erl_cv_nif.evision_cv_mat_release(img: mat)
  end

  @doc namespace: :"cv.Mat"
  @spec clone(reference()) :: {:ok, reference()} | {:error, String.t()}
  def clone(mat) when is_reference(mat) do
    :erl_cv_nif.evision_cv_mat_clone(img: mat)
  end

  deferror(clone(mat))

  @doc namespace: :"cv.Mat"
  @spec empty() :: :ok, reference()
  def empty() do
    :erl_cv_nif.evision_cv_mat_empty()
  end

  deferror(empty())

  @doc namespace: :"cv.Mat"
  @spec to_binary(reference(), non_neg_integer()) :: {:ok, binary()} | {:error, String.t()}
  def to_binary(mat, limit \\ 0) when is_reference(mat) and is_integer(limit) and limit >= 0 do
    :erl_cv_nif.evision_cv_mat_to_binary(img: mat, limit: limit)
  end

  deferror(to_binary(mat, limit))

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
    :erl_cv_nif.evision_cv_mat_from_binary(
      binary: binary,
      t: t,
      l: l,
      cols: cols,
      rows: rows,
      channels: channels
    )
  end

  deferror(from_binary(binary, type, rows, cols, channels))

  @doc namespace: :"cv.Mat"
  def from_binary_by_shape(binary, _type = {t, l}, shape)
      when is_binary(binary) and is_atom(t) and is_integer(l) and is_tuple(shape) do
    from_binary_by_shape(binary, {t, l}, Tuple.to_list(shape))
  end

  @doc namespace: :"cv.Mat"
  def from_binary_by_shape(binary, _type = {t, l}, shape)
      when is_binary(binary) and is_atom(t) and is_integer(l) and is_list(shape) do
    :erl_cv_nif.evision_cv_mat_from_binary_by_shape(
      binary: binary,
      t: t,
      l: l,
      shape: shape
    )
  end

  deferror(from_binary_by_shape(binary, type, shape))

  @doc namespace: :"cv.Mat"
  def eye(n, _type = {t, l}) when is_integer(n) and n > 0 do
    :erl_cv_nif.evision_cv_mat_eye(
      n: n,
      t: t,
      l: l
    )
  end

  deferror(eye(n, type))

  @doc namespace: :"cv.Mat"
  def reshape(mat, shape) when is_reference(mat) and is_tuple(shape) do
    :erl_cv_nif.evision_cv_mat_reshape(
      mat: mat,
      shape: Tuple.to_list(shape)
    )
  end

  def reshape(mat, shape) when is_reference(mat) and is_list(shape) do
    :erl_cv_nif.evision_cv_mat_reshape(
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
end
