defmodule Evision.Mat do
  @moduledoc """
  Evision Mat
  """

  import Kernel, except: [abs: 1, floor: 1, ceil: 1, round: 1]

  @typedoc """
  Types for `Evision.Mat`

  #### Shorthand

  - `:u8`
  - `:u16`
  - `:s16`
  - `:s32`
  - `:f32`
  - `:f64`
  - `:f16`

  #### Tuple Form
  - `{:u, 8}`
  - `{:u, 16}`
  - `{:s, 8}`
  - `{:s, 16}`
  - `{:s, 32}`
  - `{:f, 32}`
  - `{:f, 64}`
  - `{:f, 16}`

  """
  @type mat_type ::
          {:u, 8 | 16}
          | {:s, 8 | 16 | 32}
          | {:f, 32 | 64 | 16}
          | :u8
          | :u16
          | :s8
          | :s16
          | :s32
          | :f32
          | :f64
          | :f16

  @type mat_type_tuple_form ::
          {:u, 8 | 16}
          | {:s, 8 | 16 | 32}
          | {:f, 32 | 64 | 16}

  @typedoc """
  Type that represents an `Evision.Mat` struct.

  - **channels**: `int`.

    The number of matrix channels.

  - **dims**: `int`.

    Matrix dimensionality.

  - **type**: `mat_type`.

    Type of the matrix elements, following `:nx`'s convention.

  - **raw_type**: `int`.

    The raw value returned from `int cv::Mat::type()`.

  - **shape**: `tuple`.

    The shape of the matrix.

  - **ref**: `reference`.

    The underlying erlang resource variable.

  """
  @type t :: %__MODULE__{
    channels: integer(),
    dims: integer(),
    type: mat_type(),
    raw_type: integer(),
    shape: tuple(),
    ref: reference()
  }
  @enforce_keys [:channels, :dims, :type, :raw_type, :shape, :ref]
  defstruct [:channels, :dims, :type, :raw_type, :shape, :ref]

  alias __MODULE__, as: T

  @typedoc """
  The resulting ok-error tuple when a NIF function can return `Evision.Mat`.
  """
  @type maybe_mat_out :: Evision.Mat.t() | {:error, String.t()}

  @typedoc """
  Input argument, `Evision.Mat`, `Nx.Tensor` or `#reference`.

  - `Evision.Mat`, recommended to use.
  - `Nx.Tensor`

    Accepting this type so that it's easier to interact with a `Nx.Tensor`.

  - `reference()`, not recommended.

    Only some internal functions will pass the raw reference variables around.

  """
  @type maybe_mat_in :: reference() | Evision.Mat.t() | Nx.Tensor.t()

  @doc false
  def __to_struct__(%{:channels => channels, :dims => dims, :type => type, :raw_type => raw_type, :shape => shape, :ref => ref}) do
    %T{
      channels: channels,
      dims: dims,
      type: type,
      raw_type: raw_type,
      shape: shape,
      ref: ref
    }
  end

  def __to_struct__(ret) do
    Evision.Internal.Structurise.to_struct(ret)
  end

  @doc false
  @spec __from_struct__(Evision.Mat.t() | Nx.Tensor.t() | reference()) :: reference()
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
      defp is_2d_image(%Evision.Mat{channels: 1, shape: {_h, _w, 1}}) do
        true
      end

      defp is_2d_image(_), do: false

      @spec to_livebook(Evision.Mat.t()) :: Kino.Output.t()
      def to_livebook(mat) when is_struct(mat, Evision.Mat) do
        raw = Kino.Inspect.new(mat)
        numerical = Kino.Inspect.new(Evision.Nx.to_nx(mat))
        with true <- is_2d_image(mat),
            encoded <- Evision.imencode(".png", mat) do
          image = Kino.Image.new(encoded, :png)
          tabs = Kino.Layout.tabs([{"Raw", raw}, {"Image", image}, {"Numerical", numerical}])
          Kino.Render.to_livebook(tabs)
        else
          false ->
            tabs = Kino.Layout.tabs([{"Raw", raw}, {"Numerical", numerical}])
            Kino.Render.to_livebook(tabs)
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
  @spec literal(list(), mat_type(), Keyword.t()) :: maybe_mat_out()
  def literal([]) do
    empty()
  end

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

  @doc namespace: :"cv.Mat"
  @spec number(number(), mat_type()) :: maybe_mat_out()
  def number(number, type) do
    type = check_unsupported_type(type)
    Evision.Mat.full({1, 1}, number, type)
  end

  @doc namespace: :"cv.Mat"
  @spec at(maybe_mat_in(), integer()) :: number() | {:error, String.t()}
  def at(mat, position) when is_integer(position) and position >= 0 do
    mat = __from_struct__(mat)
    :evision_nif.mat_at(img: mat, pos: position)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec add(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def add(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_add(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec add(maybe_mat_in(), maybe_mat_in(), mat_type()) :: maybe_mat_out()
  def add(lhs, rhs, type) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    {t, l} = check_unsupported_type(type)
    :evision_nif.mat_add_typed(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec subtract(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def subtract(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_subtract(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec subtract(maybe_mat_in(), maybe_mat_in(), mat_type()) :: maybe_mat_out()
  def subtract(lhs, rhs, type) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    {t, l} = check_unsupported_type(type)
    :evision_nif.mat_subtract_typed(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec multiply(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def multiply(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_multiply(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec multiply(maybe_mat_in(), maybe_mat_in(), mat_type()) :: maybe_mat_out()
  def multiply(lhs, rhs, type) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    {t, l} = check_unsupported_type(type)
    :evision_nif.mat_multiply_typed(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec multiply(maybe_mat_in(), maybe_mat_in(), mat_type() | nil) :: maybe_mat_out()
  def matrix_multiply(lhs, rhs, out_type \\ nil)

  def matrix_multiply(lhs, rhs, nil) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_matrix_multiply(lhs: lhs, rhs: rhs, t: nil, l: 0)
    |> Evision.Internal.Structurise.to_struct()
  end

  def matrix_multiply(lhs, rhs, out_type) when is_atom(out_type) do
    {t, l} = check_unsupported_type(out_type)
    matrix_multiply(lhs, rhs, {t, l})
  end

  def matrix_multiply(lhs, rhs, {t, l}) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_matrix_multiply(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec divide(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def divide(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_divide(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec divide(maybe_mat_in(), maybe_mat_in(), mat_type()) :: maybe_mat_out()
  def divide(lhs, rhs, type) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    {t, l} = check_unsupported_type(type)
    :evision_nif.mat_divide_typed(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec bitwise_and(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def bitwise_and(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_bitwise_and(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec bitwise_or(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def bitwise_or(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_bitwise_or(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec bitwise_xor(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def bitwise_xor(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_bitwise_xor(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec cmp(maybe_mat_in(), maybe_mat_in(), :eq | :gt | :ge | :lt | :le | :ne) :: maybe_mat_out()
  def cmp(lhs, rhs, op) when op in [:eq, :gt, :ge, :lt, :le, :ne] do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_cmp(l: lhs, r: rhs, type: op)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec logical_and(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def logical_and(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_logical_and(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec logical_or(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def logical_or(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_logical_or(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec logical_xor(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def logical_xor(lhs, rhs) do
    lhs = __from_struct__(lhs)
    rhs = __from_struct__(rhs)
    :evision_nif.mat_logical_xor(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec abs(maybe_mat_in()) :: maybe_mat_out()
  def abs(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_abs(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec expm1(maybe_mat_in()) :: maybe_mat_out()
  def expm1(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_expm1(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec clip(maybe_mat_in(), number(), number()) :: maybe_mat_out()
  def clip(mat, lower, upper)
      when is_number(lower) and is_number(upper) and lower <= upper do
    mat = __from_struct__(mat)
    :evision_nif.mat_clip(img: mat, lower: lower, upper: upper)
    |> Evision.Internal.Structurise.to_struct()
  end

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
  @spec transpose(maybe_mat_in(), [integer()], Keyword.t()) :: maybe_mat_out()
  def transpose(mat, axes, opts \\ []) do
    mat = __from_struct__(mat)
    # todo: check return value of shape(mat)
    as_shape = opts[:as_shape] || shape(mat)

    as_shape =
      case as_shape do
        {:error, msg} ->
          raise RuntimeError, msg
        _ ->
          case is_tuple(as_shape) do
            true ->
              Tuple.to_list(as_shape)

            _ ->
              as_shape
          end
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

  @doc """
  Transpose a matrix

  ## Parameters

    - `mat`. The matrix.
      by default it reverses the order of the axes.

  """
  @spec transpose(maybe_mat_in()) :: maybe_mat_out()
  def transpose(mat) do
    mat = __from_struct__(mat)
    with {:error, message} <- shape(mat) do
      {:error, message}
    else
      as_shape ->
        ndims = Enum.count(as_shape)
        uniq_axes = Enum.reverse(0..(ndims - 1))
        :evision_nif.mat_transpose(img: mat, axes: uniq_axes, as_shape: as_shape)
        |> Evision.Internal.Structurise.to_struct()
    end
  end

  @doc namespace: :"cv.Mat"
  @doc """
  This method returns the type-tuple used by Nx. To get the raw value of `cv::Mat.type()`, please use
  `Evision.Mat.raw_type/1`.
  """
  @spec type(maybe_mat_in()) :: mat_type() | {:error, String.t()}
  def type(mat)

  def type(%T{type: type}) do
    type
  end

  def type(mat) when is_struct(mat) do
    mat = Evision.Internal.Structurise.from_struct(mat)
    :evision_nif.mat_type(img: mat)
  end

  def type(mat) when is_reference(mat) do
    :evision_nif.mat_type(img: mat)
  end

  @doc namespace: :"cv.Mat"
  @spec bitwise_not(maybe_mat_in()) :: maybe_mat_out()
  def bitwise_not(mat) do
    mat = __from_struct__(mat)
    case Evision.Mat.type(mat) do
      {:error, msg} ->
        {:error, msg}
      type ->
        {s, _} = check_unsupported_type(type)
        if s in [:s, :u] do
          :evision_nif.mat_bitwise_not(img: mat)
          |> Evision.Internal.Structurise.to_struct()
        else
          {:error,
           "bitwise operators expect integer tensors as inputs and outputs an integer tensor, got: #{inspect(type)}"}
        end
    end
  end

  @doc namespace: :"cv.Mat"
  @spec ceil(maybe_mat_in()) :: maybe_mat_out()
  def ceil(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_ceil(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec floor(maybe_mat_in()) :: maybe_mat_out()
  def floor(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_floor(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec negate(maybe_mat_in()) :: maybe_mat_out()
  def negate(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_negate(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec round(maybe_mat_in()) :: maybe_mat_out()
  def round(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_round(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec sign(maybe_mat_in()) :: maybe_mat_out()
  def sign(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_sign(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec setTo(maybe_mat_in(), number(), maybe_mat_in()) :: maybe_mat_out()
  def setTo(mat, value, mask) do
    mat = __from_struct__(mat)
    mask = __from_struct__(mask)
    :evision_nif.mat_set_to(img: mat, value: value, mask: mask)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec dot(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def dot(mat_a, mat_b) do
    mat_a = __from_struct__(mat_a)
    mat_b = __from_struct__(mat_b)
    :evision_nif.mat_dot(a: mat_a, b: mat_b)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec as_type(maybe_mat_in(), mat_type()) :: maybe_mat_out()
  def as_type(mat, type)

  def as_type(mat, type) when is_atom(type) do
    as_type(mat, check_unsupported_type(type))
  end

  def as_type(mat, {t, l}) when is_atom(t) and l > 0 do
    mat = __from_struct__(mat)
    :evision_nif.mat_as_type(img: mat, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec shape(maybe_mat_in()) :: tuple() | {:error, String.t()}
  def shape(mat)

  def shape(%T{shape: shape}) do
    shape
  end

  def shape(mat) when is_struct(mat) do
    mat = Evision.Internal.Structurise.from_struct(mat)
    :evision_nif.mat_shape(img: mat)
  end

  def shape(mat) when is_reference(mat) do
    :evision_nif.mat_shape(img: mat)
  end

  @doc namespace: :"cv.Mat"
  @doc """
  The method returns the number of matrix channels.
  """
  @spec channels(maybe_mat_in()) :: non_neg_integer() | {:error, String.t()}
  def channels(mat)

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
  @spec depth(maybe_mat_in()) :: maybe_mat_out()
  def depth(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_depth(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the type of a matrix.

  As `Evision.Mat.type/1` returns the type used by Nx, this method gives the raw value of
  `cv::Mat.type()`
  """
  @spec raw_type(maybe_mat_in()) :: integer() | {:error, String.t()}
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

  @doc namespace: :"cv.Mat"
  @spec isSubmatrix(maybe_mat_in()) :: true | false
  def isSubmatrix(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_isSubmatrix(img: mat)
  end

  @doc namespace: :"cv.Mat"
  @spec isContinuous(maybe_mat_in()) :: true | false
  def isContinuous(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_isContinuous(img: mat)
  end

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the matrix element size in bytes.

  The method returns the matrix element size in bytes. For example, if the matrix type is CV_16SC3,
  the method returns 3\*sizeof(short) or 6.
  """
  @spec elemSize(maybe_mat_in()) :: integer()
  def elemSize(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_elemSize(img: mat)
  end

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the size of each matrix element channel in bytes.

  The method returns the matrix element channel size in bytes, that is, it ignores the number of
  channels. For example, if the matrix type is CV_16SC3 , the method returns sizeof(short) or 2.
  """
  @spec elemSize1(maybe_mat_in()) :: integer()
  def elemSize1(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_elemSize1(img: mat)
  end

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the `cv::MatSize` of the matrix.

  The method returns a tuple `{dims, p}` where `dims` is the number of dimensions, and `p` is a list with `dims` elements.
  """
  @spec size(maybe_mat_in()) :: {non_neg_integer(), [non_neg_integer()]} | {:error, String.t()}
  def size(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_size(img: mat)
  end

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the total number of array elements.

  The method returns the number of array elements (a number of pixels if the array represents an image).
  """
  @spec total(maybe_mat_in()) :: non_neg_integer() | {:error, String.t()}
  def total(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_total(img: mat, start_dim: -1, end_dim: 0xFFFFFFFF)
  end

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the total number of array elements.

  The method returns the number of elements within a certain sub-array slice with start_dim <= dim < end_dim
  """
  @spec total(maybe_mat_in(), non_neg_integer(), non_neg_integer()) :: non_neg_integer() | {:error, String.t()}
  def total(mat, start_dim, end_dim \\ 0xFFFFFFFF) do
    mat = __from_struct__(mat)
    :evision_nif.mat_total(img: mat, start_dim: start_dim, end_dim: end_dim)
  end

  @doc namespace: :"cv.Mat"
  @doc """
  This function would convert the input tensor with dims `[height, width, dims]` to a `dims`-channel image with dims `[height, width]`.

  Note that OpenCV has limitation on the number of channels. Currently the maximum number of channels is `512`.
  """
  @spec last_dim_as_channel(maybe_mat_in()) :: maybe_mat_out()
  def last_dim_as_channel(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_last_dim_as_channel(src: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc """
  This function does the opposite as to `Evision.Mat.last_dim_as_channel/1`.

  If the number of channels of the input Evision.Mat is greater than 1,
  then this function would convert the input Evision.Mat with dims `dims=list(int())` to a `1`-channel Evision.Mat with dims `[dims | channels]`.

  If the number of channels of the input Evision.Mat is equal to 1,
  - if dims == shape, then nothing happens
  - otherwise, a new Evision.Mat that has dims=`[dims | channels]` will be returned
  """
  @spec channel_as_last_dim(maybe_mat_in()) :: maybe_mat_out()
  def channel_as_last_dim(mat) when is_struct(mat) do
    mat = Evision.Internal.Structurise.from_struct(mat)
    channel_as_last_dim(mat)
  end

  def channel_as_last_dim(mat) when is_reference(mat) do
    with {:error, msg} <- size(mat) do
      {:error, msg}
    else
      {num_dims, _} ->
        with {:error, msg} <- shape(mat) do
          {:error, msg}
        else
          shape ->
            num_shape = tuple_size(shape)
            if num_shape == num_dims do
              mat
            else
              Evision.Mat.as_shape(mat, shape)
            end
        end
    end
  end

  @doc namespace: :"cv.Mat"
  @spec zeros(tuple(), mat_type()) :: maybe_mat_out()
  def zeros(shape, type) when is_tuple(shape) do
    {t, l} = check_unsupported_type(type)

    :evision_nif.mat_zeros(
      shape: Tuple.to_list(shape),
      t: t,
      l: l
    )
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec ones(tuple(), mat_type()) :: maybe_mat_out()
  def ones(shape, type) when is_tuple(shape) do
    {t, l} = check_unsupported_type(type)

    :evision_nif.mat_ones(
      shape: Tuple.to_list(shape),
      t: t,
      l: l
    )
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc """
  Generate a Mat with shape `{1, length}`, where `length` is the amount of
  numbers starting from `from` to `to` with step size `step`
  """
  @spec arange(integer(), integer(), integer(), mat_type()) :: maybe_mat_out()
  def arange(from, to, step, type) when step != 0 do
    {t, l} = check_unsupported_type(type)

    with {:ok, mat} <-
           :evision_nif.mat_arange(
             from: from,
             to: to,
             step: step,
             t: t,
             l: l
           ),
          ret = {length, _} <- Evision.Mat.shape(mat),
          {:mat_shape_error, false, _} <- {:mat_shape_error, length == :error, ret}
      do
      Evision.Mat.reshape(mat, {1, length})
    else
      {:mat_shape_error, true, error} ->
        error
      {:error, msg} ->
        {:error, msg}
    end
  end

  @doc """
  Generate a Mat with a list of number starting from `from` to `to` with step size `step`.
  The genrated Mat will then be reshaped to the requested `shape` if applicable.
  """
  @spec arange(integer(), integer(), integer(), mat_type(), tuple()) :: maybe_mat_out()
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

  @doc """
  Generate a Mat with all of its elements equal to `number`.

  ##### Positional Arguments

  - **shape**. `tuple`

    The expected shape of the resulting `Evision.Mat`

  - **number**. `number`

    Element value.

  - **type**.

    Value type.

  """
  @spec full(tuple(), number(), mat_type()) :: maybe_mat_out()
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

  @doc namespace: :"cv.Mat"
  @doc """
  Get a clone an `Evision.Mat`.
  Data will be copied to the resulting `Evision.Mat`.
  """
  @spec clone(maybe_mat_in()) :: maybe_mat_out()
  def clone(mat) do
    mat = __from_struct__(mat)
    :evision_nif.mat_clone(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @doc """
  Create an empty `Evision.Mat`.
  This function is the Elixir equvilent of calling `cv::Mat()` in C++.
  """
  @spec empty() :: maybe_mat_out()
  def empty() do
    :evision_nif.mat_empty()
    |> Evision.Internal.Structurise.to_struct()
  end

  @spec to_batched(maybe_mat_in(), non_neg_integer(), Keyword.t()) :: maybe_mat_out()
  def to_batched(mat, batch_size, opts)
      when is_integer(batch_size) and batch_size >= 1 and is_list(opts) do
    leftover = opts[:leftover] || :repeat
    mat = __from_struct__(mat)
    with {:error, msg} <- shape(mat) do
      {:error, msg}
    else
      as_shape ->
        :evision_nif.mat_to_batched(
          img: mat,
          batch_size: batch_size,
          as_shape: as_shape,
          leftover: leftover
        )
        |> Evision.Internal.Structurise.to_struct()
    end
  end

  @spec to_batched(maybe_mat_in(), non_neg_integer(), tuple(), Keyword.t()) :: maybe_mat_out()
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

  @doc namespace: :"cv.Mat"
  @spec to_binary(maybe_mat_in(), non_neg_integer()) :: binary() | {:error, String.t()}
  def to_binary(mat, limit \\ 0) when is_integer(limit) and limit >= 0 do
    mat = __from_struct__(mat)
    :evision_nif.mat_to_binary(img: mat, limit: limit)
  end

  @doc """
  Create Mat from binary (pixel) data

  - **binary**.

    The binary pixel data

  - **type**.

    one of `[{:u, 8}, {:s, 8}, {:u, 16}, {:s, 16}, {:s, 32}, {:f, 32}, {:f, 64}]` and their corresponding shorthands.

  - **rows**. `int`

    Number of rows (i.e., the height of the image)

  - **cols**. `int`

    Number of cols (i.e., the width of the image)

  - **channels**. `int`

    Number of channels.

  """
  @doc namespace: :"cv.Mat"
  @spec from_binary(binary(), mat_type(), pos_integer(), pos_integer(), non_neg_integer()) :: maybe_mat_out()
  def from_binary(binary, type, rows, cols, channels)

  def from_binary(binary, type, rows, cols, channels)
      when is_binary(binary) and rows > 0 and cols > 0 and channels > 0 and is_atom(type) do
    from_binary(binary, check_unsupported_type(type), rows, cols, channels)
  end

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

  @spec check_unsupported_type(mat_type()) :: mat_type_tuple_form()
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
  @spec from_binary_by_shape(binary(), mat_type(), tuple()) :: maybe_mat_out()
  def from_binary_by_shape(binary, type, shape)

  def from_binary_by_shape(binary, type, shape)
  when is_binary(binary) and is_atom(type) and is_tuple(shape) do
    from_binary_by_shape_impl(binary, check_unsupported_type(type), shape)
  end

  def from_binary_by_shape(binary, {t, l}, shape)
      when is_binary(binary) and is_atom(t) and is_integer(l) and is_tuple(shape) do
    from_binary_by_shape_impl(binary, {t, l}, shape)
  end

  defp from_binary_by_shape_impl(binary, {t, l}, shape)
      when is_binary(binary) and is_atom(t) and is_integer(l) and is_tuple(shape) do
    :evision_nif.mat_from_binary_by_shape(
      binary: binary,
      t: t,
      l: l,
      shape: Tuple.to_list(shape)
    )
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec eye(non_neg_integer(), mat_type()) :: maybe_mat_out()
  def eye(n, type) when is_integer(n) and n > 0 do
    {t, l} = check_unsupported_type(type)

    :evision_nif.mat_eye(
      n: n,
      t: t,
      l: l
    )
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec reshape(maybe_mat_in(), tuple() | list()) :: maybe_mat_out()
  def reshape(mat, shape)

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

  @doc namespace: :"cv.Mat"
  @doc """
  This method does not change the underlying data. It only changes the steps when accessing the matrix.

  If intended to change the underlying data to the new shape, please use `Evision.Mat.reshape/2`.
  """
  @spec as_shape(maybe_mat_in(), tuple() | list()) :: maybe_mat_out()
  def as_shape(mat, as_shape)

  def as_shape(mat, as_shape) when is_tuple(as_shape) do
    as_shape(mat, Tuple.to_list(as_shape))
  end

  def as_shape(mat, as_shape) when is_list(as_shape) do
    mat = __from_struct__(mat)
    case Evision.Mat.shape(mat) do
      {:error, msg} ->
        {:error, msg}
      old_shape ->
        if Tuple.product(old_shape) == Enum.product(as_shape) do
          :evision_nif.mat_as_shape(
            img: mat,
            as_shape: as_shape
          )
          |> Evision.Internal.Structurise.to_struct()
        else
          {:error, "Cannot treat mat with shape #{inspect(old_shape)} as the requested new shape #{inspect(as_shape)}: mismatching number of elements"}
        end
    end
  end

  @spec squeeze(maybe_mat_in()) :: maybe_mat_out()
  def squeeze(mat) do
    mat = __from_struct__(mat)
    case Evision.Mat.shape(mat) do
      {:error, msg} ->
        {:error, msg}
      mat_shape ->
        shape = Tuple.to_list(mat_shape)
        Evision.Mat.reshape(mat, Enum.reject(shape, fn d -> d == 1 end))
    end
  end

  @spec broadcast_to(maybe_mat_in(), tuple()) :: maybe_mat_out()
  def broadcast_to(mat, to_shape) do
    mat = __from_struct__(mat)
    :evision_nif.mat_broadcast_to(
      img: mat,
      to_shape: Tuple.to_list(to_shape),
      force_src_shape: []
    )
    |> Evision.Internal.Structurise.to_struct()
  end

  @spec broadcast_to(maybe_mat_in(), tuple(), tuple()) :: maybe_mat_out()
  def broadcast_to(mat, to_shape, force_src_shape) do
    mat = __from_struct__(mat)
    :evision_nif.mat_broadcast_to(
      img: mat,
      to_shape: Tuple.to_list(to_shape),
      force_src_shape: Tuple.to_list(force_src_shape)
    )
    |> Evision.Internal.Structurise.to_struct()
  end
end
