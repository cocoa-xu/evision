defmodule Evision.Mat do
  @moduledoc """
  Evision Mat
  """

  require Logger
  import Kernel, except: [abs: 1, floor: 1, ceil: 1, round: 1]
  @behaviour Access

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
  Input argument, `Evision.Mat`, `Nx.Tensor`, `#reference`, or a single number, or a n-tuple of n numbers.

  - `Evision.Mat`, recommended to use.
  - `Nx.Tensor`, recommended to use.

    Accepting this type so that it's easier to interact with a `Nx.Tensor`.

  - `number()`.
  - `tuple()`, should be a n-tuple of n numbers.

  - `reference()`, not recommended.

    Only some internal functions will pass the raw reference variables around.

  """
  @type maybe_mat_in :: reference() | Evision.Mat.t() | Nx.Tensor.t() | number() | tuple()

  @type enum :: integer()
  @doc enum: true
  def cv_MAGIC_VAL, do: 1_124_007_936
  @doc enum: true
  def cv_AUTO_STEP, do: 0
  @doc enum: true
  def cv_MAGIC_MASK, do: 4_294_901_760
  @doc enum: true
  def cv_TYPE_MASK, do: 4095
  @doc enum: true
  def cv_DEPTH_MASK, do: 7

  @doc false
  def to_struct(%{
        :channels => channels,
        :dims => dims,
        :type => type,
        :raw_type => raw_type,
        :shape => shape,
        :ref => ref
      }) do
    %T{
      channels: channels,
      dims: dims,
      type: type,
      raw_type: raw_type,
      shape: shape,
      ref: ref
    }
  end

  def to_struct(ret) do
    Evision.Internal.Structurise.to_struct(ret)
  end

  @doc false
  @spec from_struct(Evision.Mat.t() | Nx.Tensor.t() | reference()) :: reference()
  def from_struct(%T{ref: ref}) do
    ref
  end

  @doc false
  def from_struct(%Nx.Tensor{} = tensor) do
    Evision.Internal.Structurise.from_struct(tensor)
  end

  @doc false
  def from_struct(%{__struct__: :nx_tensor} = cast_tensor) do
    cast_tensor
  end

  @doc false
  def from_struct(ref) when is_reference(ref) do
    ref
  end

  defimpl Nx.LazyContainer do
    def traverse(%Evision.Mat{type: type, shape: shape} = mat, acc, fun) do
      fun.(Nx.template(shape, type), fn -> Evision.Mat.to_nx(mat) end, acc)
    end
  end

  @doc false
  @spec __from_elixir_range__(Range.t(), Keyword.t()) :: {number(), number()}
  def __from_elixir_range__(first..last//step, opts \\ []) do
    swap_if_neg_step = opts[:swap_if_neg_step] || false
    allowed_step_size = opts[:allowed_step_size]

    if is_list(allowed_step_size) and !Enum.member?(allowed_step_size, step) do
      raise "Invalid step size, only supports step size in #{inspect({allowed_step_size})} at the moment."
    end

    if swap_if_neg_step and step < 0 do
      {last, first}
    else
      {first, last}
    end
  end

  defp __handle_negative_range__(value, bound) when value < 0 do
    value + bound
  end

  defp __handle_negative_range__(value, _bound), do: value

  @doc false
  def __standardise_range_list__(ranges, shape, inclusive_range) do
    Enum.map(Enum.zip(ranges, Tuple.to_list(shape)), fn {r, dim} ->
      case r do
        :all ->
          :all

        {first, last} ->
          # {_, _} is cv::Range
          # hence we don't need to do anything to it
          first = __handle_negative_range__(first, dim)
          last = __handle_negative_range__(last, dim)
          {first, last}

        first..last//step ->
          # first..last//step is Elixir.Range
          # 0..0 should give [0] if `inclusive_range` is true
          step =
            if step == -1 and (last < 0 or first < 0) do
              1
            else
              step
            end

          first = __handle_negative_range__(first, dim)
          last = __handle_negative_range__(last, dim)
          {first, last} = __from_elixir_range__(first..last//step, allowed_step_size: [1])

          if inclusive_range do
            # note that what we are going return is cv::Range
            # which is [start, end)
            {first, last + 1}
          else
            {first, last}
          end

        number when is_integer(number) ->
          number = __handle_negative_range__(number, dim)
          # cv::Range is [start, end)
          # while Elixir.Range is [first, last]
          if inclusive_range do
            # [first, last), [0, 1) will give [0]
            # note that what we are going return is cv::Range
            # which is [start, end)
            {number, number + 1}
          else
            # [first, last], [0, 0] will give []
            {number, number}
          end

        unknown ->
          raise "Cannot convert from `#{inspect(unknown)}` to a valid range."
      end
    end)
  end

  @doc """
  Extracts a rectangular submatrix.

  The submatrix data is copied.

  #### Variant 1
  ##### Positional Arguments

  - **mat**. `maybe_mat_in()`

    The matrix.

  - **rowRange**. `{int, int} | :all`.

    Start and end row of the extracted submatrix. The upper boundary is not included.

  - **colRange**. `{int, int} | :all`.

    Start and end column of the extracted submatrix. The upper boundary is not included.

  ##### Return

  Extracted submatrix (data is copied).

  #### Variant 2
  ##### Positional Arguments

  - **mat**. `maybe_mat_in()`

    The matrix.

  - **rowRange**. `Range.t(step: 1)`.

    Start and end row of the extracted submatrix. The upper boundary is not included.

  - **colRange**. `Range.t(step: 1)`.

    Start and end column of the extracted submatrix. The upper boundary is not included.

  ##### Return

  Extracted submatrix (data is copied).

  """
  @spec roi(maybe_mat_in(), {integer(), integer()} | :all, {integer(), integer()} | :all) ::
          maybe_mat_out()
  def roi(mat, rowRange, colRange)
      when (is_tuple(rowRange) or rowRange == :all) and (is_tuple(colRange) or colRange == :all) do
    mat = from_struct(mat)

    :evision_nif.mat_roi(mat: mat, rowRange: rowRange, colRange: colRange)
    |> Evision.Internal.Structurise.to_struct()
  end

  @spec roi(maybe_mat_in(), Range.t(), Range.t()) :: maybe_mat_out()
  def roi(mat, firstRow..lastRow//1, firstCol..lastCol//1) do
    roi(mat, {firstRow, lastRow}, {firstCol, lastCol})
  end

  def roi(_, _.._//_, _.._//_) do
    raise ArgumentError, "Evision.Mat.roi does not support step size other than 1."
  end

  @doc """
  Extracts a rectangular submatrix.

  #### Variant 1
  ##### Positional Arguments

  - **mat**. `maybe_mat_in()`

    The matrix.

  - **rect**. `{int, int, int, int}`

    The rect that specifies `{x, y, width, height}`.

  ##### Return

  Extracted submatrix specified as a rectangle. (data is copied)

  #### Variant 2
  ##### Positional Arguments

  - **mat**. `maybe_mat_in()`

    The matrix.

  - **ranges**. `[{int, int} | :all]`

    Array of selected ranges along each array dimension.

  ##### Return

  Extracted submatrix. (data is copied)

  """
  @spec roi(maybe_mat_in(), {integer(), integer(), integer(), integer()}) :: maybe_mat_out()
  def roi(mat, rect = {_, _, _, _}) when is_tuple(rect) do
    mat = from_struct(mat)

    :evision_nif.mat_roi(mat: mat, rect: rect)
    |> Evision.Internal.Structurise.to_struct()
  end

  @spec roi(maybe_mat_in(), [{integer(), integer()} | Range.t() | :all]) :: maybe_mat_out()
  def roi(mat, ranges) when is_list(ranges) do
    shape = mat.shape
    mat = from_struct(mat)
    ranges = __standardise_range_list__(ranges, shape, true)

    :evision_nif.mat_roi(mat: mat, ranges: ranges)
    |> Evision.Internal.Structurise.to_struct()
  end

  @spec update_roi(maybe_mat_in(), [{integer(), integer()} | Range.t() | :all], maybe_mat_in()) ::
          maybe_mat_out()
  def update_roi(mat, ranges, with_mat) do
    {mat, bring_back} =
      if mat.dims != tuple_size(mat.shape) do
        {Evision.Mat.channel_as_last_dim(mat), true}
      else
        {mat, false}
      end

    with_mat =
      if with_mat.dims != tuple_size(with_mat.shape) do
        Evision.Mat.channel_as_last_dim(with_mat)
      else
        with_mat
      end

    ranges = __standardise_range_list__(ranges, mat.shape, true)

    ranges =
      if tuple_size(mat.shape) > Enum.count(ranges) do
        extend =
          for i <- Enum.count(ranges)..(tuple_size(mat.shape) - 1), reduce: [] do
            acc ->
              [{0, elem(mat.shape, i)} | acc]
          end

        ranges ++ Enum.reverse(extend)
      else
        ranges
      end

    with_mat = from_struct(with_mat)
    mat = from_struct(mat)

    res =
      Evision.Internal.Structurise.to_struct(
        :evision_nif.mat_update_roi(mat: mat, ranges: ranges, with_mat: with_mat)
      )

    if bring_back do
      Evision.Mat.last_dim_as_channel(res)
    else
      res
    end
  end

  @doc """
  Display inline image in terminal for iTerm2 users.

  This function will check the value of `:display_inline_image_iterm2` in the application config.

  If is `true`, then it will detect if current session is running in `iTerm2` (by checking the environment variable `LC_TERMINAL`).

  If both are `true`, we next check if the image is a 2D image, also if its size is within the limits.

  The maximum size can be set in the application config, for example,

  ```elixir
  config :evision, display_inline_image_iterm2: true
  config :evision, display_inline_image_max_size: {8192, 8192}
  ```

  If it passes all the checks, then it will be displayed as an inline image in iTerm2.
  """
  @spec quicklook(Nx.Tensor.t()) :: Nx.Tensor.t()
  def quicklook(%Nx.Tensor{} = tensor) do
    case Evision.Mat.from_nx_2d(tensor) do
      %Evision.Mat{} = mat ->
        quicklook(mat)
    end

    tensor
  end

  @spec quicklook(Evision.Mat.t()) :: Evision.Mat.t()
  def quicklook(%Evision.Mat{dims: dims, channels: c, shape: shape} = mat) do
    if Application.get_env(:evision, :display_inline_image_max_size) == :error do
      Application.put_env(:evision, :display_inline_image_max_size, {8192, 8192},
        persistent: true
      )
    end

    is_2d_image =
      (dims == 2 and Enum.member?([1, 3, 4], c)) or
        (c == 1 and tuple_size(shape) == 3 and elem(shape, 2) == 1)

    with {:is_2d, true} <- {:is_2d, is_2d_image},
         {:display_image_if_in_iterm2, {:ok, true}} <-
           {:display_image_if_in_iterm2,
            Application.fetch_env(:evision, :display_inline_image_iterm2)},
         {:is_iterm2, true} <- {:is_iterm2, System.get_env("LC_TERMINAL") == "iTerm2"},
         {:get_maximum_size, {h, w}} <-
           {:get_maximum_size, Application.get_env(:evision, :display_inline_image_max_size)},
         {:within_maximum_size, true} <-
           {:within_maximum_size,
            ((0 < h and elem(shape, 0) < h) or h == :infinity) and
              ((0 < w and elem(shape, 1) < w) or w == :infinity)} do
      {osc, st} =
        if String.starts_with?(System.get_env("TERM"), "screen") do
          {"\ePtmux;\e\e", "\a\e\\\r\n"}
        else
          {"\e", "\e\\\r\n"}
        end

      binary = Evision.imencode(".png", mat)
      b64 = Base.encode64(binary)
      bin_size = byte_size(binary)

      IO.puts([
        "#{osc}]1337;File=size=#{bin_size};inline=1:",
        b64,
        st
      ])
    end

    mat
  end

  @spec quicklook(term()) :: term()
  def quicklook(any) do
    any
  end

  @default_kino_render_image_encoding Application.compile_env(
                                        :evision,
                                        :kino_render_image_encoding,
                                        :png
                                      )
  @doc """
  Get preferred image encoding when rendering in Kino.

  Default value is `Application.compile_env(:evision, :kino_render_image_encoding, :png)`.
  """
  @spec kino_render_image_encoding() :: term()
  def kino_render_image_encoding() do
    Process.get(:evision_kino_render_image_encoding, @default_kino_render_image_encoding)
  end

  @doc """
  Set preferred image encoding when rendering in Kino.

  Only valid when `:kino` >= 0.7 and using in livebook

  ##### Positional Arguments
  - **encoding**. `:png | :jpeg`.

    When rendering a 2D image with Kino in Livebook
    the image will first be encoded into either :png or :jpeg

    - `:png` usually has better quality because it is lossless compression,
      however, it uses more bandwidth to transfer

    - `:jpeg` require less bandwidth to pass from the backend to the livebook frontend,
      but it is lossy compression

  """
  @spec set_kino_render_image_encoding(:png | :jpeg | term()) :: term()
  def set_kino_render_image_encoding(encoding) when encoding in [:png, :jpeg] do
    Process.put(:evision_kino_render_image_encoding, encoding)
  end

  def set_kino_render_image_encoding(encoding) do
    raise RuntimeError, """
    Unknown image encoding `#{inspect(encoding)}`. Supported encoding are either :png or :jpeg.
    """
  end

  @default_kino_render_image_max_size Application.compile_env(
                                        :evision,
                                        :kino_render_image_max_size,
                                        {8192, 8192}
                                      )
  @doc """
  Get the maximum allowed image size to render in Kino.

  Default value is `Application.compile_env(:evision, :kino_render_image_max_size, {8192, 8192})`.
  """
  @spec kino_render_image_max_size() :: term()
  def kino_render_image_max_size() do
    Process.get(:evision_kino_render_image_max_size, @default_kino_render_image_max_size)
  end

  @doc """
  Set the maximum allowed image size to render in Kino.

  Only valid when `:kino` >= 0.7 and using in livebook

  ##### Positional Arguments
  - **size**. `{height, width}`.
  """
  @spec set_kino_render_image_max_size({pos_integer(), pos_integer()}) :: term()
  def set_kino_render_image_max_size({height, width})
      when is_integer(height) and height > 0 and is_integer(width) and width > 0 do
    Process.put(:evision_kino_render_image_max_size, {height, width})
  end

  def set_kino_render_image_max_size(size) do
    raise RuntimeError, """
    Invalid value for setting image max size `#{inspect(size)}`, expecting a 2-tuple with positive integers, `{height, width}`.
    """
  end

  @kino_render_tab_order Enum.uniq(
                           Application.compile_env(:evision, :kino_render_tab_order, [
                             :image,
                             :raw,
                             :numerical
                           ])
                         )
  @doc """
  Get preferred order of Kino.Layout tabs for `Evision.Mat` in Livebook.

  Default value is `Enum.uniq(Application.compile_env(:evision, :kino_render_tab_order, [:image, :raw, :numerical]))`.
  """
  @spec kino_render_tab_order() :: term()
  def kino_render_tab_order() do
    Process.get(:evision_kino_render_tab_order, @kino_render_tab_order)
  end

  @supported_kino_render_tab_order [:image, :raw, :numerical]
  @doc """
  Set preferred order of Kino.Layout tabs for `Evision.Mat` in Livebook.

  Only valid when `:kino` >= 0.7 and using in Livebook.

  ##### Positional Arguments
  - **order**: `[atom()]`

    Default order is `[:image, :raw, :numerical]`, and the corresponding tabs will be:

      Image | Raw | Numerical

    Note that the `:image` tab will not show if the `Evision.Mat` is not a 2D image.

    Also, it's possible to specify any combination (any subset) of these tabs,
    including the empty one, `[]`, and in that case, the output content in the livebook
    cell will be the same as `:raw` but without any tabs.

    Simply put, `[]` means to only do the basic inspect and not use Kino.Layout.tabs

    **It's worth noting that `[] != nil`, because `nil` is default return value when `kino_render_tab_order`**
    **is not configured -- hence evision will use the default order, `[:image, :raw, :numerical]` in such case**

    When only specifying one type, i.e., `[:image]`, `[:raw]` or `[:numerical]`, only one tab will be shown.

    Furthermore, when `kino_render_tab_order` is configured to `[:image]` and when the `Evision.Mat` is not a 2D image,
    it will automatically fallback to `:raw`.

    Simply put, `[:image]` in this case (when only specifying one type) means:

    displaying the `Evision.Mat` as an image whenever possible, and fallback to `:raw`
    if it's not a 2D image

  """
  @spec set_kino_render_tab_order([atom()] | term()) :: term()
  def set_kino_render_tab_order(order) when is_list(order) do
    render_types =
      Enum.map(order, fn t ->
        supported? = Enum.member?(@supported_kino_render_tab_order, t)

        if !supported? do
          Logger.warning("""
          Unknown type `#{inspect(t)}` found in `config :evision, kino_render_tab_order`.
          Supported types are `#{inspect(@supported_kino_render_tab_order)}` and their combinations.
          """)

          nil
        else
          t
        end
      end)
      |> Enum.reject(fn a -> a == nil end)

    Process.put(:evision_kino_render_tab_order, render_types)
  end

  def set_kino_render_tab_order(types) do
    raise RuntimeError, """
    Unknown types `#{inspect(types)}`. Supported types are `#{inspect(@supported_kino_render_tab_order)}` and their combinations.
    """
  end

  if Code.ensure_loaded?(Kino.Render) do
    defimpl Kino.Render do
      require Logger

      defp is_2d_image(%Evision.Mat{dims: 2}), do: true

      defp is_2d_image(%Evision.Mat{channels: 1, shape: {_h, _w, 1}}) do
        true
      end

      defp is_2d_image(_), do: false

      defp within_maximum_size(mat) do
        {max_height, max_width} = Evision.Mat.kino_render_image_max_size()

        case Evision.Mat.shape(mat) do
          {h, w} ->
            h <= max_height and w <= max_width

          {h, w, _c} ->
            h <= max_height and w <= max_width

          _ ->
            false
        end
      end

      def to_livebook(mat) when is_struct(mat, Evision.Mat) do
        render_types = Evision.Mat.kino_render_tab_order()

        Enum.map(render_types, fn
          :raw ->
            {"Raw", Kino.Inspect.new(mat)}

          :numerical ->
            {"Numerical", Kino.Inspect.new(Evision.Mat.to_nx(mat))}

          :image ->
            {ext, format} =
              case Evision.Mat.kino_render_image_encoding() do
                :jpg ->
                  {".jpg", :jpeg}

                :jpeg ->
                  {".jpeg", :jpeg}

                :png ->
                  {".png", :png}

                unknown ->
                  raise RuntimeError, "Cannot render image with encoding `#{inspect(unknown)}`"
              end

            with true <- is_2d_image(mat),
                 true <- within_maximum_size(mat),
                 encoded <- Evision.imencode(ext, mat),
                 true <- is_binary(encoded) do
              {"Image", Kino.Image.new(encoded, format)}
            else
              _ ->
                nil
            end
        end)
        |> Enum.reject(fn a -> a == nil end)
        |> to_livebook_tabs(render_types, mat)
      end

      defp to_livebook_tabs([], [:image], mat) do
        Kino.Layout.tabs([{"Raw", Kino.Inspect.new(mat)}])
        |> Kino.Render.to_livebook()
      end

      defp to_livebook_tabs(_tabs, [], mat) do
        Kino.Inspect.new(mat)
        |> Kino.Render.to_livebook()
      end

      defp to_livebook_tabs(tabs, _types, _mat) do
        Kino.Layout.tabs(tabs)
        |> Kino.Render.to_livebook()
      end
    end
  end

  @doc false
  def __generate_complete_range__(dims, maybe_incomplete) when is_list(maybe_incomplete) do
    indices_given = Enum.count(maybe_incomplete)

    if indices_given <= dims do
      maybe_incomplete ++ List.duplicate(:all, dims - indices_given)
    else
      if indices_given == dims + 1 do
        maybe_incomplete
      else
        {:error,
         "too many indices, got #{indices_given} index ranges, while the matrix.dims is #{dims}"}
      end
    end
  end

  @impl Access
  @doc """
  Access.fetch implementation for Evision.Mat.

  ```elixir
  iex> img = Evision.imread("test/qr_detector_test.png")
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {300, 300, 3},
    ref: #Reference<0.809884129.802291734.78316>
  }

  # Same behaviour as Nx.
  # Also, img[0] gives the same result as img[[0]]
  # For this example, they are both equvilent of img[[0, :all, :all]]
  iex> img[[0]]
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {1, 300, 3},
    ref: #Reference<0.809884129.802291731.77296>
  }

  # same as img[[0..100, 50..200, :all]]
  # however, currently we only support ranges with step size 1
  #
  # **IMPORTANT NOTE**
  #
  # also, please note that we are using Elixir.Range here
  # and Elixir.Range is **inclusive**, i.e, [start, end]
  # while cv::Range `{integer(), integer()}` is `[start, end)`
  # the difference can be observed in the `shape` field
  iex> img[[0..100, 50..200]]
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {101, 151, 3},
    ref: #Reference<0.809884129.802291731.77297>
  }
  iex> img[[{0, 100}, {50, 200}]]
  %Evision.Mat{
    channels: 3,
    dims: 2,
    type: {:u, 8},
    raw_type: 16,
    shape: {100, 150, 3},
    ref: #Reference<0.809884129.802291731.77297>
  }

  # for this example, the result is the same as `Evision.extractChannel(img, 0)`
  iex> img[[:all, :all, 0]]
  %Evision.Mat{
    channels: 1,
    dims: 2,
    type: {:u, 8},
    raw_type: 0,
    shape: {300, 300},
    ref: #Reference<0.809884129.802291731.77298>
  }
  iex> img[[:all, :all, 0..1]]
  %Evision.Mat{
    channels: 2,
    dims: 2,
    type: {:u, 8},
    raw_type: 8,
    shape: {300, 300, 2},
    ref: #Reference<0.809884129.802291731.77299>
  }

  # when index is out of bounds
  iex> img[[:all, :all, 42]]
  {:error, "index 42 is out of bounds for axis 2 with size 3"}

  # it works the same way for any dimensional Evision.Mat
  iex> mat = Evision.Mat.ones({10, 10, 10, 10, 10}, :u8)
  iex> mat[[1..7, :all, 2..6, 3..9, :all]]
  %Evision.Mat{
    channels: 1,
    dims: 5,
    type: {:u, 8},
    raw_type: 0,
    shape: {7, 10, 5, 7, 10},
    ref: #Reference<0.3015448455.3766878228.259075>
  }
  ```
  """
  @spec fetch(Evision.Mat.t(), list() | integer()) :: {:ok, maybe_mat_out() | nil}
  def fetch(mat, key) when is_list(key) do
    ranges = __generate_complete_range__(mat.dims, key)
    ranges = __standardise_range_list__(ranges, mat.shape, true)
    {:ok, roi(mat, ranges)}
  end

  def fetch(mat, key) when is_integer(key) do
    # cv::Range is [start, end)
    fetch(mat, [{key, key + 1}])
  end

  def fetch(_mat, _key) do
    {:ok, nil}
  end

  @doc """
  Get raw pointers
  """
  def to_pointer(img) do
    img = from_struct(img)
    :evision_nif.mat_to_pointer(img: img, mode: :local)
  end

  def to_pointer(img, opts) when is_list(opts) do
    opts = Keyword.validate!(opts || [], mode: :local)
    img = from_struct(img)
    :evision_nif.mat_to_pointer([img: img] ++ opts)
  end

  @impl Access
  @doc """
  Access.get_and_update/3 implementation for Evision.Mat

  ```elixir
  iex> mat = Evision.Mat.zeros({5, 5}, :u8)
  iex> Evision.Mat.to_nx(mat)
  #Nx.Tensor<
    u8[5][5]
    Evision.Backend
    [
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0]
    ]
  >
  iex> {old, new} = Evision.Mat.get_and_update(mat, [1..3, 1..3], fn roi ->
      {roi, Nx.broadcast(Nx.tensor(255, type: roi.type), roi.shape)}
  end)
  iex> Evision.Mat.to_nx(new)
  #Nx.Tensor<
    u8[5][5]
    Evision.Backend
    [
      [0, 0, 0, 0, 0],
      [0, 255, 255, 255, 0],
      [0, 255, 255, 255, 0],
      [0, 255, 255, 255, 0],
      [0, 0, 0, 0, 0]
    ]
  >
  ```
  """
  @spec get_and_update(Evision.Mat.t(), term(), (Evision.Mat.t() -> Evision.Mat.t())) ::
          {Evision.Mat.t(), Evision.Mat.t()}
  def get_and_update(mat, key, function) when is_list(key) do
    ranges = __generate_complete_range__(mat.dims, key)
    roi = roi(mat, ranges)

    case function.(roi) do
      {^roi, modified_roi} ->
        if is_struct(modified_roi, Nx.Tensor) or is_struct(modified_roi, Evision.Mat) do
          {mat, update_roi(mat, ranges, modified_roi)}
        else
          raise RuntimeError,
                "Cannot update the requested sub-matrix with unsupported value #{inspect(modified_roi)}"
        end

      _ ->
        {mat, mat}
    end
  end

  def get_and_update(mat, key, function) when is_integer(key) do
    get_and_update(mat, [{key, key + 1}], function)
  end

  def get_and_update(_mat, key, _function) do
    raise RuntimeError, "Evision.Mat.get_and_update/3: unknown/unsupported key #{inspect(key)}"
  end

  @impl Access
  @doc """
  Access.pop/2 is not implemented yet
  """
  @spec pop(any, any) :: none
  def pop(_mat, _key) do
    raise RuntimeError, "Evision.Mat does not support Access.pop/2 yet"
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

  `Evision.Mat.literal/3` will return a valid 2D image
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
      Evision.Mat.from_nx_2d(tensor)
    else
      Evision.Mat.from_nx(tensor)
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
    mat = from_struct(mat)

    :evision_nif.mat_at(img: mat, pos: position)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec add(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def add(lhs, rhs) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)

    :evision_nif.mat_add(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec add(maybe_mat_in(), maybe_mat_in(), mat_type()) :: maybe_mat_out()
  def add(lhs, rhs, type) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)
    {t, l} = check_unsupported_type(type)

    :evision_nif.mat_add_typed(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec subtract(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def subtract(lhs, rhs) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)

    :evision_nif.mat_subtract(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec subtract(maybe_mat_in(), maybe_mat_in(), mat_type()) :: maybe_mat_out()
  def subtract(lhs, rhs, type) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)
    {t, l} = check_unsupported_type(type)

    :evision_nif.mat_subtract_typed(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec multiply(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def multiply(lhs, rhs) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)

    :evision_nif.mat_multiply(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec multiply(maybe_mat_in(), maybe_mat_in(), mat_type()) :: maybe_mat_out()
  def multiply(lhs, rhs, type) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)
    {t, l} = check_unsupported_type(type)

    :evision_nif.mat_multiply_typed(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec matrix_multiply(maybe_mat_in(), maybe_mat_in(), mat_type() | nil) :: maybe_mat_out()
  def matrix_multiply(lhs, rhs, out_type \\ nil)

  def matrix_multiply(lhs, rhs, nil) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)

    :evision_nif.mat_matrix_multiply(lhs: lhs, rhs: rhs, t: nil, l: 0)
    |> Evision.Internal.Structurise.to_struct()
  end

  def matrix_multiply(lhs, rhs, out_type) when is_atom(out_type) do
    {t, l} = check_unsupported_type(out_type)
    matrix_multiply(lhs, rhs, {t, l})
  end

  def matrix_multiply(lhs, rhs, {t, l}) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)

    :evision_nif.mat_matrix_multiply(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec divide(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def divide(lhs, rhs) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)

    :evision_nif.mat_divide(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec divide(maybe_mat_in(), maybe_mat_in(), mat_type()) :: maybe_mat_out()
  def divide(lhs, rhs, type) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)
    {t, l} = check_unsupported_type(type)

    :evision_nif.mat_divide_typed(lhs: lhs, rhs: rhs, t: t, l: l)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec bitwise_and(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def bitwise_and(lhs, rhs) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)

    :evision_nif.mat_bitwise_and(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec bitwise_or(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def bitwise_or(lhs, rhs) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)

    :evision_nif.mat_bitwise_or(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec bitwise_xor(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def bitwise_xor(lhs, rhs) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)

    :evision_nif.mat_bitwise_xor(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec cmp(maybe_mat_in(), maybe_mat_in(), :eq | :gt | :ge | :lt | :le | :ne) :: maybe_mat_out()
  def cmp(lhs, rhs, op) when op in [:eq, :gt, :ge, :lt, :le, :ne] do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)

    :evision_nif.mat_cmp(l: lhs, r: rhs, type: op)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec logical_and(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def logical_and(lhs, rhs) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)

    :evision_nif.mat_logical_and(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec logical_or(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def logical_or(lhs, rhs) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)

    :evision_nif.mat_logical_or(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec logical_xor(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def logical_xor(lhs, rhs) do
    lhs = from_struct(lhs)
    rhs = from_struct(rhs)

    :evision_nif.mat_logical_xor(l: lhs, r: rhs)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec abs(maybe_mat_in()) :: maybe_mat_out()
  def abs(mat) do
    mat = from_struct(mat)

    :evision_nif.mat_abs(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec expm1(maybe_mat_in()) :: maybe_mat_out()
  def expm1(mat) do
    mat = from_struct(mat)

    :evision_nif.mat_expm1(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec clip(maybe_mat_in(), number(), number()) :: maybe_mat_out()
  def clip(mat, lower, upper)
      when is_number(lower) and is_number(upper) and lower <= upper do
    mat = from_struct(mat)

    :evision_nif.mat_clip(img: mat, lower: lower, upper: upper)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc """
  Transform an `Evision.Mat` to `Nx.tensor`.

  ##### Positional Arguments
  - **mat**: `maybe_mat_in()`

    Evision.Mat

  - **backend**: `module()`

    Nx backend.

  ##### Return
  If the input `Evision.Mat` represents a 2D image, the resulting tensor
  will have shape `{height, width, channels}`.

  ### Example

  ```elixir
  iex> %Evision.Mat{} = mat = Evision.imread("/path/to/exist/img.png")
  iex> nx_tensor = Evision.Mat.to_nx(mat)
  #Nx.Tensor<
    u8[1080][1920][3]
    [[ ... pixel data ... ]]
  >
  ```

  """
  @spec to_nx(maybe_mat_in(), module()) :: Nx.Tensor.t() | {:error, String.t()}
  def to_nx(mat, backend \\ Evision.Backend) when is_struct(mat, Evision.Mat) do
    mat = from_struct(mat)

    with mat_type <- Evision.Mat.type(mat),
         mat_shape <- Evision.Mat.shape(mat),
         {:not_empty_shape, true} <- {:not_empty_shape, tuple_size(mat_shape) > 0},
         {:not_error, true, _} <- {:not_error, elem(mat_shape, 0) != :error, mat_shape},
         bin <- Evision.Mat.to_binary(mat),
         {:is_binary, true, _} <- {:is_binary, is_binary(bin), bin} do
      Nx.reshape(Nx.from_binary(bin, mat_type, backend: backend), mat_shape)
    else
      {:error, reason} ->
        {:error, reason}

      {:not_empty_shape, false} ->
        {:error, "shape is {}"}

      {:not_error, false, error} ->
        error

      {:is_binary, false, error} ->
        error
    end
  end

  @doc """
  Converts a tensor from `Nx.Tensor` to `Evision.Mat`.

  ##### Positional Arguments

  - **t**. `Nx.Tensor`

  ##### Return
  An `Evision.Mat` that has the same shape and type.
  (except for `:s64`, `:u32` and `:u64`, please see more details at https://github.com/cocoa-xu/evision/issues/48).
  """
  @spec from_nx(Nx.t()) :: Evision.Mat.t() | {:error, String.t()}
  def from_nx(t) when is_struct(t, Nx.Tensor) do
    case Nx.shape(t) do
      {} ->
        Evision.Mat.from_binary_by_shape(Nx.to_binary(t), Nx.type(t), {1})

      shape ->
        Evision.Mat.from_binary_by_shape(Nx.to_binary(t), Nx.type(t), shape)
    end
  end

  @doc """
  Converts a tensor from `Nx.Tensor` to `Evision.Mat`.

  ##### Positional Arguments

  - **t**. `Nx.Tensor`
  - **as_shape**. `tuple`.

  ##### Return
  An `Evision.Mat` that has the specified shape (if the number of elements matches) and the same type as the given tensor.
  (except for `:s64`, `:u32` and `:u64`, please see more details at https://github.com/cocoa-xu/evision/issues/48).
  """
  @spec from_nx(Nx.Tensor.t(), tuple()) :: Evision.Mat.t() | {:error, String.t()}
  def from_nx(t, as_shape) when is_struct(t, Nx.Tensor) do
    case Nx.shape(t) do
      {} ->
        Evision.Mat.from_binary_by_shape(Nx.to_binary(t), Nx.type(t), {1})

      shape ->
        if Tuple.product(shape) == Tuple.product(as_shape) do
          Evision.Mat.from_binary_by_shape(Nx.to_binary(t), Nx.type(t), as_shape)
        else
          {:error,
           "cannot convert tensor(#{inspect(shape)}) to mat as shape #{inspect(as_shape)}"}
        end
    end
  end

  @doc """
  Converts a tensor from `Nx.Tensor` to a 2D `Evision.Mat`.

  If the tuple size of the shape is 3, the resulting `Evision.Mat` will be a `c`-channel 2D image,
  where `c` is the last number in the shape tuple.

  If the tuple size of the shape is 2, the resulting `Evision.Mat` will be a 1-channel 2D image.

  Otherwise, it's not possible to convert the tensor to a 2D image.
  """
  @spec from_nx_2d(Nx.t()) :: Evision.Mat.t() | {:error, String.t()}
  def from_nx_2d(t) do
    case Nx.shape(t) do
      {height, width} ->
        Evision.Mat.from_binary(Nx.to_binary(t), Nx.type(t), height, width, 1)

      {height, width, channels} ->
        Evision.Mat.from_binary(Nx.to_binary(t), Nx.type(t), height, width, channels)

      shape ->
        {:error, "Cannot convert tensor(#{inspect(shape)}) to a 2D image"}
    end
  end

  @doc """
  Transpose a matrix

  ## Parameters

    - **mat**. `Evision.Mat`
    - **axes**. `[int]`

        It must be a list which contains a permutation of [0,1,..,N-1]
        where N is the number of axes of `mat`. The i’th axis of the returned array will correspond to the
        axis numbered axes[i] of the input.

    - **opts**. Keyword options.

        - `as_shape`. A tuple or list which overwrites the shape of the matrix (the total number of elements
          must be equal to the one as in its original shape). For example, a 4x4 matrix can be treated as a
          2x2x2x2 matrix and transposed with `axes=[2,1,3,0]` in a single call.

          When specified, it combines the reshape and transpose operation in a single NIF call.

  """
  @spec transpose(maybe_mat_in(), [integer()], Keyword.t()) :: maybe_mat_out()
  def transpose(mat, axes, opts \\ []) do
    mat = from_struct(mat)

    self_shape =
      case shape(mat) do
        {:error, msg} ->
          raise RuntimeError, msg

        self_shape ->
          Tuple.to_list(self_shape)
      end

    as_shape = opts[:as_shape] || self_shape

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
      as_shaped = as_shape != self_shape

      :evision_nif.mat_transpose(
        img: mat,
        axes: uniq_axes,
        as_shape: as_shape,
        as_shaped: as_shaped
      )
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
    mat = from_struct(mat)

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
    mat = from_struct(mat)

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
    mat = from_struct(mat)

    :evision_nif.mat_ceil(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec floor(maybe_mat_in()) :: maybe_mat_out()
  def floor(mat) do
    mat = from_struct(mat)

    :evision_nif.mat_floor(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec negate(maybe_mat_in()) :: maybe_mat_out()
  def negate(mat) do
    mat = from_struct(mat)

    :evision_nif.mat_negate(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec round(maybe_mat_in()) :: maybe_mat_out()
  def round(mat) do
    mat = from_struct(mat)

    :evision_nif.mat_round(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec sign(maybe_mat_in()) :: maybe_mat_out()
  def sign(mat) do
    mat = from_struct(mat)

    :evision_nif.mat_sign(img: mat)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec setTo(maybe_mat_in(), number(), maybe_mat_in()) :: maybe_mat_out()
  def setTo(mat, value, mask) do
    mat = from_struct(mat)
    mask = from_struct(mask)

    :evision_nif.mat_set_to(img: mat, value: value, mask: mask)
    |> Evision.Internal.Structurise.to_struct()
  end

  @doc namespace: :"cv.Mat"
  @spec dot(maybe_mat_in(), maybe_mat_in()) :: maybe_mat_out()
  def dot(mat_a, mat_b) do
    mat_a = from_struct(mat_a)
    mat_b = from_struct(mat_b)

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
    mat = from_struct(mat)

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
    mat = from_struct(mat)

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
    mat = from_struct(mat)
    :evision_nif.mat_isSubmatrix(img: mat)
  end

  @doc namespace: :"cv.Mat"
  @spec isContinuous(maybe_mat_in()) :: true | false
  def isContinuous(mat) do
    mat = from_struct(mat)
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
    mat = from_struct(mat)
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
    mat = from_struct(mat)
    :evision_nif.mat_elemSize1(img: mat)
  end

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the `cv::MatSize` of the matrix.

  The method returns a tuple `{dims, p}` where `dims` is the number of dimensions, and `p` is a list with `dims` elements.
  """
  @spec size(maybe_mat_in()) :: {non_neg_integer(), [non_neg_integer()]} | {:error, String.t()}
  def size(mat) do
    mat = from_struct(mat)
    :evision_nif.mat_size(img: mat)
  end

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the total number of array elements.

  The method returns the number of array elements (a number of pixels if the array represents an image).
  """
  @spec total(maybe_mat_in()) :: non_neg_integer() | {:error, String.t()}
  def total(mat) do
    mat = from_struct(mat)
    :evision_nif.mat_total(img: mat, start_dim: -1, end_dim: 0xFFFFFFFF)
  end

  @doc namespace: :"cv.Mat"
  @doc """
  Returns the total number of array elements.

  The method returns the number of elements within a certain sub-array slice with start_dim <= dim < end_dim
  """
  @spec total(maybe_mat_in(), non_neg_integer(), non_neg_integer()) ::
          non_neg_integer() | {:error, String.t()}
  def total(mat, start_dim, end_dim \\ 0xFFFFFFFF) do
    mat = from_struct(mat)
    :evision_nif.mat_total(img: mat, start_dim: start_dim, end_dim: end_dim)
  end

  @doc namespace: :"cv.Mat"
  @doc """
  This function would convert the input tensor with dims `[height, width, dims]` to a `dims`-channel image with dims `[height, width]`.

  Note that OpenCV has limitation on the number of channels. Currently the maximum number of channels is `512`.
  """
  @spec last_dim_as_channel(maybe_mat_in()) :: maybe_mat_out()
  def last_dim_as_channel(mat) do
    mat = from_struct(mat)

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

    arange =
      :evision_nif.mat_arange(
        from: from,
        to: to,
        step: step,
        t: t,
        l: l
      )

    mat = Evision.Internal.Structurise.to_struct(arange)

    with %Evision.Mat{} <- mat,
         ret = {length, _} <- Evision.Mat.shape(mat),
         {:mat_shape_error, false, _} <- {:mat_shape_error, length == :error, ret} do
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
  The generated Mat will then be reshaped to the requested `shape` if applicable.
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
    mat = from_struct(mat)

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
    mat = from_struct(mat)

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
    mat = from_struct(mat)
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
    mat = from_struct(mat)

    case mat do
      %{__struct__: :nx_tensor, data: data} when is_binary(data) ->
        if limit == 0 do
          data
        else
          :binary.part(data, 0, limit)
        end

      _ ->
        :evision_nif.mat_to_binary(img: mat, limit: limit)
    end
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
  @spec from_binary(binary(), mat_type(), pos_integer(), pos_integer(), non_neg_integer()) ::
          maybe_mat_out()
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
    mat = from_struct(mat)

    :evision_nif.mat_reshape(
      mat: mat,
      shape: Tuple.to_list(shape)
    )
    |> Evision.Internal.Structurise.to_struct()
  end

  def reshape(mat, shape) when is_list(shape) do
    mat = from_struct(mat)

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
    mat = from_struct(mat)

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
          {:error,
           "Cannot treat mat with shape #{inspect(old_shape)} as the requested new shape #{inspect(as_shape)}: mismatching number of elements"}
        end
    end
  end

  @spec squeeze(maybe_mat_in()) :: maybe_mat_out()
  def squeeze(mat) do
    mat = from_struct(mat)

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
    mat = from_struct(mat)

    :evision_nif.mat_broadcast_to(
      img: mat,
      to_shape: Tuple.to_list(to_shape),
      force_src_shape: []
    )
    |> Evision.Internal.Structurise.to_struct()
  end

  @spec broadcast_to(maybe_mat_in(), tuple(), tuple()) :: maybe_mat_out()
  def broadcast_to(mat, to_shape, force_src_shape) do
    mat = from_struct(mat)

    :evision_nif.mat_broadcast_to(
      img: mat,
      to_shape: Tuple.to_list(to_shape),
      force_src_shape: Tuple.to_list(force_src_shape)
    )
    |> Evision.Internal.Structurise.to_struct()
  end
end
