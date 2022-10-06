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
          | {:f, 16}
          | :u8
          | :u16
          | :s8
          | :s16
          | :s32
          | :f32
          | :f64
          | :f16
  @type channels_from_binary ::
          1 | 2 | 3 | 4

  defstruct [:channels, :dims, :type, :raw_type, :shape, :ref]
  alias __MODULE__, as: T
  @type t :: %{__struct__: atom()}

  @doc false
  def __make_struct__(%{:channels => channels, :dims => dims, :type => type, :raw_type => raw_type, :shape => shape, :ref => ref}) do
    %T{
      channels: channels,
      dims: dims,
      type: type,
      raw_type: raw_type,
      shape: shape,
      ref: ref
    }
  end

  @doc false
  def __from_struct__(%T{ref: ref}) do
    ref
  end

  def __from_struct__(%Nx.Tensor{}=tensor) do
    Evision.Internal.Structurise.from_struct(tensor)
  end

  def __from_struct__(ref) when is_reference(ref) do
    ref
  end

  if Code.ensure_loaded?(Kino.Render) do
    defimpl Kino.Render do
      defp is_2d_image(%Evision.Mat{dims: 2}), do: true
      defp is_2d_image(%Evision.Mat{channels: c, shape: {_h, _w, c}}) when c in [1, 3, 4] do
        true
      end

      defp is_2d_image(_), do: true

      def to_livebook(mat) do
        with true <- is_2d_image(mat),
            {:ok, encoded} <- Evision.imencode(".png", mat) do
          raw = Kino.Inspect.new(mat)
          image = Kino.Image.new(encoded, :png)
          tabs = Kino.Layout.tabs("Raw": raw, "Image": image)
          Kino.Render.to_livebook(tabs)
        else
          _ ->
            Kino.Output.inspect(mat)
        end
      end
    end
  end

  @doc namespace: :"cv.Mat"
  @doc """
  Create an `Evision.Mat` from list literals.

  ### Example

  Creating `Evision.Mat` from empty list literal (`[]`) is the same as calling `Evision.Mat.empty()`.

  ```elixir
  iex> Evision.Mat.literal!([])
  %Evision.Mat{
    channels: 1,
    dims: 0,
    type: {:u, 8},
    raw_type: 0,
    shape: {},
    ref: #Reference<0.1204050731.2031747092.46781>
  }
  ```

  By default, the shape of the Mat will stay as is.
  ```elixir
  iex> Evision.Mat.literal!([[[1,1,1],[2,2,2],[3,3,3]]], :u8)
  %Evision.Mat{
    channels: 1,
    dims: 3,
    type: {:u, 8},
    raw_type: 0,
    shape: {1, 3, 3},
    ref: #Reference<0.512519210.691404819.106300>
  }
  ```

  `Evision.Mat.literal/3` will return a vaild 2D image
  if the keyword argument, `as_2d`, is set to `true`
  and if the list literal can be represented as a 2D image.
  ```elixir
  iex> Evision.Mat.literal!([[[1,1,1],[2,2,2],[3,3,3]]], :u8, as_2d: true)
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {1, 3, 3},
    ref: #Reference<0.512519210.691404820.106293>
  }
  ```

  """
  @spec literal(list(), mat_type(), Keyword.t()) :: {:ok, %T{}} | {:error, String.t()}
  def literal([]) do
    empty()
  end
  deferror(literal([]))

  def literal(literal, type, opts \\ [])
  def literal([], _type, _opts) do
    empty()
  end

  def literal(literal, type, opts) when is_list(literal) do
    # leave all the checks to Nx.tensor/2
    as_2d_image = opts[:as_2d] || false
    tensor = Nx.tensor(literal, type: type, backend: Evision.Backend)
    if as_2d_image do
      Evision.Nx.to_mat_2d(tensor)
    else
      Evision.Nx.to_mat(tensor)
    end
  end

  deferror(literal(literal, type))
  deferror(literal(literal, type, opts))

  @doc namespace: :"cv.Mat"
  def number(number, type) do
    type = check_unsupported_type(type)
    Evision.Mat.full({1, 1}, number, type)
  end

  deferror(number(number, type))

  @doc namespace: :"cv.Mat"
  def at(mat, position) when is_integer(position) and position >= 0 do
    mat = __from_struct__(mat)
    :evision_nif.mat_at(img: mat, pos: position)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(at(mat, position))

  @doc namespace: :"cv.Mat"
  def add(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_add(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(add(lhs, rhs))

  @doc namespace: :"cv.Mat"
  def add(lhs, rhs, type) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    {t, l} = check_unsupported_type(type)
    :evision_nif.mat_add_typed(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(add(lhs, rhs, type))

  @doc namespace: :"cv.Mat"
  def subtract(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_subtract(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(subtract(lhs, rhs))

  @doc namespace: :"cv.Mat"
  def subtract(lhs, rhs, type) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    {t, l} = check_unsupported_type(type)
    :evision_nif.mat_subtract_typed(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(subtract(lhs, rhs, type))

  @doc namespace: :"cv.Mat"
  def multiply(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_multiply(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(multiply(lhs, rhs))

  @doc namespace: :"cv.Mat"
  def multiply(lhs, rhs, type) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    {t, l} = check_unsupported_type(type)
    :evision_nif.mat_multiply_typed(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(multiply(lhs, rhs, type))

  @doc namespace: :"cv.Mat"
  def matrix_multiply(lhs, rhs, out_type = {t, l} \\ nil) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    if out_type == nil do
      :evision_nif.mat_matrix_multiply(lhs: lhs, rhs: rhs, t: nil, l: 0)
    else
      :evision_nif.mat_matrix_multiply(lhs: lhs, rhs: rhs, t: t, l: l)
    end
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(matrix_multiply(lhs, rhs))
  deferror(matrix_multiply(lhs, rhs, out_type))

  @doc namespace: :"cv.Mat"
  def divide(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_divide(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(divide(lhs, rhs))

  @doc namespace: :"cv.Mat"
  def divide(lhs, rhs, type) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    {t, l} = check_unsupported_type(type)
    :evision_nif.mat_divide_typed(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(divide(lhs, rhs, type))

  @doc namespace: :"cv.Mat"
  def bitwise_and(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_bitwise_and(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(bitwise_and(lhs, rhs))

  @doc namespace: :"cv.Mat"
  def bitwise_or(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_bitwise_or(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(bitwise_or(lhs, rhs))

  @doc namespace: :"cv.Mat"
  def bitwise_xor(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_bitwise_xor(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(bitwise_xor(lhs, rhs))

  @doc namespace: :"cv.Mat"
  def cmp(lhs, rhs, op) when op in [:eq, :gt, :ge, :lt, :le, :ne] do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_cmp(l: lhs, r: rhs, type: op)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(cmp(lhs, rhs, op))

  @doc namespace: :"cv.Mat"
  def logical_and(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_logical_and(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(logical_and(lhs, rhs))

  @doc namespace: :"cv.Mat"
  def logical_or(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_logical_or(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(logical_or(lhs, rhs))

  @doc namespace: :"cv.Mat"
  def logical_xor(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_logical_xor(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(logical_xor(lhs, rhs))

  @doc namespace: :"cv.Mat"
  def abs(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_abs(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(abs(mat))

  @doc namespace: :"cv.Mat"
  def expm1(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_expm1(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(expm1(mat))

  @doc namespace: :"cv.Mat"
  def clip(mat, lower, upper)
      when is_number(lower) and is_number(upper) and lower <= upper do
    mat = __from_struct__(mat)
    :evision_nif.mat_clip(img: mat, lower: lower, upper: upper)
    |> Evision.Internal.Structurise.to_struct()
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
  def transpose(mat, axes, opts \\ []) do
    mat = __from_struct__(mat)
    as_shape = opts[:as_shape] || shape!(mat)

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
      |> Evision.Internal.Structurise.to_struct()
    end
  end

  deferror(transpose(mat, axes))
  deferror(transpose(mat, axes, opts))

  @doc """
  Transpose a matrix

  ## Parameters

    - `mat`. The matrix.
      by default it reverses the order of the axes.

  """
  def transpose(mat) do
    mat = __from_struct__(mat)
    as_shape = shape!(mat)
    ndims = Enum.count(as_shape)
    uniq_axes = Enum.reverse(0..(ndims - 1))
    :evision_nif.mat_transpose(img: mat, axes: uniq_axes, as_shape: as_shape)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(transpose(mat))

  @doc namespace: :"cv.Mat"
  @doc """
  This method returns the type-tuple used by Nx. To get the raw value of `cv::Mat.type()`, please use
  `Evision.Mat.raw_type/1`.
  """
  def type(%T{type: type}) do
    {:ok, type}
  end

  def type(mat) when is_struct(mat) do
    mat = Evision.Internal.Structurise.from_struct(mat)
    :evision_nif.mat_type(img: mat)
  end

  def type(mat) when is_reference(mat) do
    :evision_nif.mat_type(img: mat)
  end

  deferror(type(mat))

  @doc namespace: :"cv.Mat"
  def bitwise_not(mat) do
    mat = __from_struct__(mat)
    type = {s, _} = Evision.Mat.type!(mat)

    if s in [:s, :u] do
      :evision_nif.mat_bitwise_not(img: mat)
      |> Evision.Internal.Structurise.to_struct()
    else
      {:error,
       "bitwise operators expect integer tensors as inputs and outputs an integer tensor, got: #{inspect(type)}"}
    end
  end

  deferror(bitwise_not(mat))

  @doc namespace: :"cv.Mat"
  def ceil(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_ceil(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(ceil(mat))

  @doc namespace: :"cv.Mat"
  @spec floor(reference()) :: {:ok, reference()} | {:error, String.t()}
  def floor(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_floor(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(floor(mat))

  @doc namespace: :"cv.Mat"
  def negate(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_negate(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(negate(mat))

  @doc namespace: :"cv.Mat"
  def round(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_round(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(round(mat))

  @doc namespace: :"cv.Mat"
  def sign(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_sign(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(sign(mat))

  @doc namespace: :"cv.Mat"
  def setTo(mat, value, mask) do
    mat = __from_struct__(mat)
    mask = __from_struct__(mask)
    :evision_nif.mat_set_to(img: mat, value: value, mask: mask)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(setTo(mat, value, mask))

  @doc namespace: :"cv.Mat"
  def dot(mat_a, mat_b) do
    mat_a = __from_struct__(mat_a)
    mat_b = __from_struct__(mat_b)
    :evision_nif.mat_dot(a: mat_a, b: mat_b)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(dot(mat_a, mat_b))

  @doc namespace: :"cv.Mat"
  def as_type(mat, _type = {t, l}) when is_atom(t) and l > 0 do
    mat = __from_struct__(mat)
    :evision_nif.mat_as_type(img: mat, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(as_type(mat, type))

  @doc namespace: :"cv.Mat"
  def shape(%T{shape: shape}) do
    {:ok, shape}
  end

  def shape(mat) when is_struct(mat) do
    mat = Evision.Internal.Structurise.from_struct(mat)
    :evision_nif.mat_shape(img: mat)
  end

  def shape(mat) when is_reference(mat) do
    :evision_nif.mat_shape(img: mat)
  end

  deferror(shape(mat))

  @doc namespace: :"cv.Mat"
  @doc """
  The method returns the number of matrix channels.
  """
  def channels(%T{channels: channels}) do
    channels
  end

  def channels(mat) when is_struct(mat) do
    mat = Evision.Internal.Structurise.from_struct(mat)
    :evision_nif.mat_type(img: mat)
  end

  def channels(mat) when is_reference(mat) do
    :evision_nif.mat_channels(img: mat)
  end

  deferror(channels(mat))

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the depth of a matrix element.

  The method returns the identifier of the matrix element depth (the type of each individual channel).
  For example, for a 16-bit signed element array, the method returns CV_16S. A complete list of
  matrix types contains the following values:

    -   CV_8U - 8-bit unsigned integers ( 0..255 )
    -   CV_8S - 8-bit signed integers ( -128..127 )
    -   CV_16U - 16-bit unsigned integers ( 0..65535 )
    -   CV_16S - 16-bit signed integers ( -32768..32767 )
    -   CV_32S - 32-bit signed integers ( -2147483648..2147483647 )
    -   CV_32F - 32-bit floating-point numbers ( -FLT_MAX..FLT_MAX, INF, NAN )
    -   CV_64F - 64-bit floating-point numbers ( -DBL_MAX..DBL_MAX, INF, NAN )

  """
  def depth(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_depth(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(depth(mat))

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the type of a matrix.

  As `Evision.Mat.type/1` returns the type used by Nx, this method gives the raw value of
  `cv::Mat.type()`
  """
  def raw_type(%T{raw_type: raw_type}) do
    raw_type
  end

  def raw_type(mat) when is_struct(mat) do
    mat = Evision.Internal.Structurise.from_struct(mat)
    :evision_nif.mat_raw_type(img: mat)
  end

  def raw_type(mat) when is_reference(mat) do
    :evision_nif.mat_raw_type(img: mat)
  end

  deferror(raw_type(mat))

  @doc namespace: :"cv.Mat"
  def isSubmatrix(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_isSubmatrix(img: mat)
  end

  deferror(isSubmatrix(mat))

  @doc namespace: :"cv.Mat"
  def isContinuous(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_isContinuous(img: mat)
  end

  deferror(isContinuous(mat))

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the matrix element size in bytes.

  The method returns the matrix element size in bytes. For example, if the matrix type is CV_16SC3,
  the method returns 3\*sizeof(short) or 6.
  """
  def elemSize(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_elemSize(img: mat)
  end

  deferror(elemSize(mat))

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the size of each matrix element channel in bytes.

  The method returns the matrix element channel size in bytes, that is, it ignores the number of
  channels. For example, if the matrix type is CV_16SC3 , the method returns sizeof(short) or 2.
  """
  def elemSize1(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_elemSize1(img: mat)
  end

  deferror(elemSize1(mat))

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the `cv::MatSize` of the matrix.

  The method returns a tuple `{dims, p}` where `dims` is the number of dimensions, and `p` is a list with `dims` elements.
  """
  def size(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_size(img: mat)
  end

  deferror(size(mat))

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the total number of array elements.

  The method returns the number of array elements (a number of pixels if the array represents an image).
  """
  def total(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_total(img: mat, start_dim: -1, end_dim: 0xFFFFFFFF)
  end

  deferror(total(mat))

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the total number of array elements.

  The method returns the number of elements within a certain sub-array slice with start_dim <= dim < end_dim
  """
  def total(mat, start_dim, end_dim \\ 0xFFFFFFFF) do
    mat = __from_struct__(mat)
    :evision_nif.mat_total(img: mat, start_dim: start_dim, end_dim: end_dim)
  end

  deferror(total(mat, start_dim))
  deferror(total(mat, start_dim, end_dim))

  @doc namespace: :"cv.Mat"
  @doc """
  This function would convert the input tensor with dims `[height, width, dims]` to a `dims`-channel image with dims `[height, width]`.

  Note that OpenCV has limitation on the number of channels. Currently the maximum number of channels is `512`.
  """
  def last_dim_as_channel(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_last_dim_as_channel(src: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(last_dim_as_channel(mat))

  @doc """
  This function does the opposite as to `Evision.Mat.last_dim_as_channel/1`.

  If the number of channels of the input Evision.Mat is greater than 1,
  then this function would convert the input Evision.Mat with dims `dims=list(int())` to a `1`-channel Evision.Mat with dims `[dims | channels]`.

  If the number of channels of the input Evision.Mat is equal to 1,
  - if dims == shape, then nothing happens
  - otherwise, a new Evision.Mat that has dims=`[dims | channels]` will be returned
  """
  def channel_as_last_dim(mat) when is_struct(mat) do
    mat = Evision.Internal.Structurise.from_struct(mat)
    channel_as_last_dim(mat)
  end

  def channel_as_last_dim(mat) when is_reference(mat) do
    {num_dims, _} = size!(mat)
    shape = shape!(mat)
    num_shape = tuple_size(shape)
    if num_shape == num_dims do
      mat
    else
      Evision.Mat.as_shape(mat, shape)
    end
  end
  deferror(channel_as_last_dim(mat))

  @doc namespace: :"cv.Mat"
  def zeros(shape, type) when is_tuple(shape) do
    {t, l} = check_unsupported_type(type)

    :evision_nif.mat_zeros(
      shape: Tuple.to_list(shape),
      t: t,
      l: l
    )
    |> Evision.Internal.Structurise.to_struct()
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
    |> Evision.Internal.Structurise.to_struct()
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
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(full(shape, number, type))

  @doc namespace: :"cv.Mat"
  def clone(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_clone(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(clone(mat))

  @doc namespace: :"cv.Mat"
  def empty() do
    :evision_nif.mat_empty()
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(empty())

  def to_batched(mat, batch_size, opts)
      when is_integer(batch_size) and batch_size >= 1 and is_list(opts) do
    leftover = opts[:leftover] || :repeat
    mat = __from_struct__(mat)

    :evision_nif.mat_to_batched(
      img: mat,
      batch_size: batch_size,
      as_shape: shape!(mat),
      leftover: leftover
    )
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(to_batched(mat, batch_size, opts))

  def to_batched(mat, batch_size, as_shape, opts)
      when is_integer(batch_size) and batch_size >= 1 and is_tuple(as_shape) and is_list(opts) do
    mat = __from_struct__(mat)
    leftover = opts[:leftover] || :repeat

    :evision_nif.mat_to_batched(
      img: mat,
      batch_size: batch_size,
      as_shape: Tuple.to_list(as_shape),
      leftover: leftover
    )
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(to_batched(mat, batch_size, as_shape, opts))

  @doc namespace: :"cv.Mat"
  def to_binary(mat, limit \\ 0) when is_integer(limit) and limit >= 0 do
    mat = __from_struct__(mat)
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
    |> Evision.Internal.Structurise.to_struct()
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
    |> Evision.Internal.Structurise.to_struct()
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
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(eye(n, type))

  @doc namespace: :"cv.Mat"
  def reshape(mat, shape) when is_tuple(shape) do
    mat = __from_struct__(mat)
    :evision_nif.mat_reshape(
      mat: mat,
      shape: Tuple.to_list(shape)
    )
    |> Evision.Internal.Structurise.to_struct()
  end

  def reshape(mat, shape) when is_list(shape) do
    mat = __from_struct__(mat)
    :evision_nif.mat_reshape(
      mat: mat,
      shape: shape
    )
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(reshape(mat, shape))

  @doc namespace: :"cv.Mat"
  @doc """
  This method does not change the underlying data. It only changes the steps when accessing the matrix.

  If intended to change the underlying data to the new shape, please use `Evision.Mat.reshape/2`.
  """
  def as_shape(mat, as_shape) when is_tuple(as_shape) do
    as_shape(mat, Tuple.to_list(as_shape))
  end

  def as_shape(mat, as_shape) when is_list(as_shape) do
    mat = __from_struct__(mat)
    with {:ok, old_shape} <- Evision.Mat.shape(mat) do
      if Tuple.product(old_shape) == Enum.product(as_shape) do
        :evision_nif.mat_as_shape(
          img: mat,
          as_shape: as_shape
        )
        |> Evision.Internal.Structurise.to_struct()
      else
        {:error, "Cannot treat mat with shape #{inspect(old_shape)} as the requested new shape #{inspect(as_shape)}: mismatching number of elements"}
      end
    else
      error -> error
    end
  end

  deferror(as_shape(mat, as_shape))

  def squeeze(mat) do
    mat = __from_struct__(mat)
    shape = Tuple.to_list(Evision.Mat.shape!(mat))
    Evision.Mat.reshape(mat, Enum.reject(shape, fn d -> d == 1 end))
  end

  deferror(squeeze(mat))

  def broadcast_to(mat, to_shape) do
    mat = __from_struct__(mat)
    :evision_nif.mat_broadcast_to(
      img: mat,
      to_shape: Tuple.to_list(to_shape),
      force_src_shape: []
    )
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(broadcast_to(mat, to_shape))

  def broadcast_to(mat, to_shape, force_src_shape) do
    mat = __from_struct__(mat)
    :evision_nif.mat_broadcast_to(
      img: mat,
      to_shape: Tuple.to_list(to_shape),
      force_src_shape: Tuple.to_list(force_src_shape)
    )
    |> Evision.Internal.Structurise.to_struct()
  end

  deferror(broadcast_to(mat, to_shape, force_src_shape))
end
