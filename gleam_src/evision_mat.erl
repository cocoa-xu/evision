-module(evision_mat).
-compile(nowarn_export_all).
-compile([export_all]).

-include("evision.hrl").

to_struct(#{class := 'Elixir.Evision.Mat', channels := Channels, dims := Dims, type := Type, raw_type := RawType, shape := Shape, ref := Ref}) ->
    #evision_mat{
        channels = Channels,
        dims = Dims,
        type = Type,
        raw_type = RawType,
        shape = Shape, 
        ref = Ref
    };
to_struct(Any) ->
    evision_internal_structurise:to_struct(Any).

from_struct(#evision_mat{ref=Ref}) ->
    Ref.

to_pointer(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
  MatRef = from_struct(Mat),
  evision_nif:mat_at([{img, MatRef}, {mode, local}]).

to_pointer(Mat, Mode) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat, is_atom(Mode) ->
  MatRef = from_struct(Mat),
  evision_nif:mat_at([{img, MatRef}, {mode, Mode}]).

full(Shape, Number, {T, L}) when is_tuple(Shape) ->
    ToShape = [element(I, Shape) || I <- lists:seq(1, tuple_size(Shape))],
    Ret = evision_nif:mat_full([{number, Number}, {t, T}, {l, L}, {shape, ToShape}]),
    evision_internal_structurise:to_struct(Ret).

number(Number, Type) ->
    full({1, 1}, Number, Type).

at(Mat, Position) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat, Position >= 0 ->
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_at([{img, MatRef}, {pos, Position}]),
    evision_internal_structurise:to_struct(Ret).

add(Lhs, Rhs) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_add([{l, LhsRef}, {r, RhsRef}]),
    evision_internal_structurise:to_struct(Ret).

add(Lhs, Rhs, {T, L}) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_add_typed([{l, LhsRef}, {r, RhsRef}, {t, T}, {l, L}]),
    evision_internal_structurise:to_struct(Ret).

subtract(Lhs, Rhs) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_subtract([{l, LhsRef}, {r, RhsRef}]),
    evision_internal_structurise:to_struct(Ret).

subtract(Lhs, Rhs, {T, L}) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_subtract_typed([{l, LhsRef}, {r, RhsRef}, {t, T}, {l, L}]),
    evision_internal_structurise:to_struct(Ret).

multiply(Lhs, Rhs) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_multiply([{l, LhsRef}, {r, RhsRef}]),
    evision_internal_structurise:to_struct(Ret).

multiply(Lhs, Rhs, {T, L}) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_multiply_typed([{l, LhsRef}, {r, RhsRef}, {t, T}, {l, L}]),
    evision_internal_structurise:to_struct(Ret).

matrix_multiply(Lhs, Rhs) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_matrix_multiply([{l, LhsRef}, {r, RhsRef}, {t, nil}, {l, 0}]),
    evision_internal_structurise:to_struct(Ret).

matrix_multiply(Lhs, Rhs, {T, L}) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_matrix_multiply([{l, LhsRef}, {r, RhsRef}, {t, T}, {l, L}]),
    evision_internal_structurise:to_struct(Ret).

divide(Lhs, Rhs) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_divide([{l, LhsRef}, {r, RhsRef}]),
    evision_internal_structurise:to_struct(Ret).

divide(Lhs, Rhs, {T, L}) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_divide_typed([{l, LhsRef}, {r, RhsRef}, {t, T}, {l, L}]),
    evision_internal_structurise:to_struct(Ret).

bitwise_and(Lhs, Rhs) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_bitwise_and([{l, LhsRef}, {r, RhsRef}]),
    evision_internal_structurise:to_struct(Ret).

bitwise_or(Lhs, Rhs) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_bitwise_or([{l, LhsRef}, {r, RhsRef}]),
    evision_internal_structurise:to_struct(Ret).

bitwise_xor(Lhs, Rhs) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_bitwise_xor([{l, LhsRef}, {r, RhsRef}]),
    evision_internal_structurise:to_struct(Ret).

cmp(Lhs, Rhs, Op) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    case lists:member(Op, [eq, gt, ge, lt, le, ne]) of
        true ->
            LhsRef = from_struct(Lhs),
            RhsRef = from_struct(Rhs),
            Ret = evision_nif:mat_cmp([{l, LhsRef}, {r, RhsRef}, {type, Op}]),
            evision_internal_structurise:to_struct(Ret);
        false ->
            {error, "Unknown cmp operator"}
    end.

logical_and(Lhs, Rhs) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_logical_and([{l, LhsRef}, {r, RhsRef}]),
    evision_internal_structurise:to_struct(Ret).

logical_or(Lhs, Rhs) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_logical_or([{l, LhsRef}, {r, RhsRef}]),
    evision_internal_structurise:to_struct(Ret).

logical_xor(Lhs, Rhs) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_logical_xor([{l, LhsRef}, {r, RhsRef}]),
    evision_internal_structurise:to_struct(Ret).

abs(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_abs([{img, MatRef}]),
    evision_internal_structurise:to_struct(Ret).

expm1(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_expm1([{img, MatRef}]),
    evision_internal_structurise:to_struct(Ret).

clip(Mat, Lower, Upper) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_clip([{img, MatRef}, {lower, Lower}, {upper, Upper}]),
    evision_internal_structurise:to_struct(Ret).

transpose(Mat, Axes, AsShape) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    TheShape = case is_tuple(AsShape) of
        true ->
            [element(I, AsShape) || I <- lists:seq(1, tuple_size(AsShape))];
        false ->
            AsShape
    end,
    Ndims = length(TheShape),
    UniqAxes = lists:filter(fun(Elem) -> Elem < (Ndims + 1) end, lists:filter(fun(Elem) -> 0 < Elem end, lists:uniq(Axes))),
    case length(UniqAxes) == Ndims of
        true ->
            MatRef = from_struct(Mat),
            Ret = evision_nif:mat_transpose([{img, MatRef}, {axes, UniqAxes}, {as_shape, TheShape}]),
            evision_internal_structurise:to_struct(Ret);
        false ->
            {error, "invalid transpose parameters"}
    end.

transpose(Mat, Axes) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    {ok, Shape} = shape(Mat),
    transpose(Mat, Axes, Shape).

transpose(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    {ok, Shape} = shape(Mat),
    Ndims = tuple_size(Shape),
    UniqAxes = lists:reverse(lists:seq(0, Ndims - 1)),
    transpose(Mat, UniqAxes, Shape).

type(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_type([{img, MatRef}]).

bitwise_not(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    {ok, {S, _}} = type(Mat),
    case lists:member(S, [s, u]) of
        true ->
            MatRef = from_struct(Mat),
            evision_nif:bitwise_not([{img, MatRef}]);
        false ->
            {error, "bitwise operators expect integer tensors as inputs and outputs an integer tensor"}
    end.

ceil(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_ceil([{img, MatRef}]),
    evision_internal_structurise:to_struct(Ret).

floor(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_floor([{img, MatRef}]),
    evision_internal_structurise:to_struct(Ret).

negate(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_negate([{img, MatRef}]),
    evision_internal_structurise:to_struct(Ret).

round(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_round([{img, MatRef}]),
    evision_internal_structurise:to_struct(Ret).

sign(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_sign([{img, MatRef}]),
    evision_internal_structurise:to_struct(Ret).

setTo(Mat, Value, Mask) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat, is_tuple(Mask), element(1, Mask) == evision_mat ->
    MatRef = from_struct(Mat),
    MaskRef = from_struct(Mask),
    Ret = evision_nif:mat_set_to([{img, MatRef}, {value, Value}, {mask, MaskRef}]),
    evision_internal_structurise:to_struct(Ret).

dot(Lhs, Rhs) when is_tuple(Lhs), tuple_size(Lhs) > 0, element(1, Lhs) == evision_mat, is_tuple(Rhs), tuple_size(Rhs) > 0, element(1, Rhs) == evision_mat ->
    LhsRef = from_struct(Lhs),
    RhsRef = from_struct(Rhs),
    Ret = evision_nif:mat_dot([{a, LhsRef}, {b, RhsRef}]),
    evision_internal_structurise:to_struct(Ret).

as_type(Mat, {T, L}) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat, is_atom(T), L > 0 ->
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_as_type([{img, MatRef}, {t, T}, {l, L}]),
    evision_internal_structurise:to_struct(Ret).

shape(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_shape([{img, MatRef}]).

channels(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_channels([{img, MatRef}]).

depth(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_depth([{img, MatRef}]).

raw_type(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_raw_type([{img, MatRef}]).

isSubmatrix(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_isSubmatrix([{img, MatRef}]).

isContinuous(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_isContinuous([{img, MatRef}]).

elemSize(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_elemSize([{img, MatRef}]).

elemSize1(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_elemSize1([{img, MatRef}]).

size(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_size([{img, MatRef}]).

total(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_total([{img, MatRef}, {start_dim, -1}, {end_dim, 4294967295}]).

total(Mat, StartDim) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_total([{img, MatRef}, {start_dim, StartDim}, {end_dim, 4294967295}]).

total(Mat, StartDim, EndDim) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_total([{img, MatRef}, {start_dim, StartDim}, {end_dim, EndDim}]).

last_dim_as_channel(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    evision_nif:mat_last_dim_as_channel([{src, MatRef}]).

zeros(Shape, {T, L}) when is_tuple(Shape) ->
    MatShape = [element(I, Shape) || I <- lists:seq(1, tuple_size(Shape))],
    Ret = evision_nif:mat_zeros([{shape, MatShape}, {t, T}, {l, L}]),
    evision_internal_structurise:to_struct(Ret).

ones(Shape, {T, L}) when is_tuple(Shape) ->
    MatShape = [element(I, Shape) || I <- lists:seq(1, tuple_size(Shape))],
    Ret = evision_nif:mat_ones([{shape, MatShape}, {t, T}, {l, L}]),
    evision_internal_structurise:to_struct(Ret).

reshape(Mat, Shape) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat, is_tuple(Shape) ->
    MatShape = [element(I, Shape) || I <- lists:seq(1, tuple_size(Shape))],
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_reshape([{mat, MatRef}, {shape, MatShape}]),
    evision_internal_structurise:to_struct(Ret);
reshape(Mat, Shape) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat, is_list(Shape) ->
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_reshape([{mat, MatRef}, {shape, Shape}]),
    evision_internal_structurise:to_struct(Ret).

arange(From, To, Step, {T, L}) ->
    Ret = evision_nif:mat_arange([{from, From}, {to, To}, {step, Step}, {t, T}, {l, L}]),
    Mat = evision_internal_structurise:to_struct(Ret),
    {ok, {Length, _}} = shape(Mat),
    reshape(Mat, {1, Length}).

arange(From, To, Step, {T, L}, Shape) ->
    Ret = evision_nif:mat_arange([{from, From}, {to, To}, {step, Step}, {t, T}, {l, L}]),
    Mat = evision_internal_structurise:to_struct(Ret),
    reshape(Mat, Shape).

to_batched(Mat, BatchSize, Leftover) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    {ok, Shape} = shape(Mat),
    MatShape = [element(I, Shape) || I <- lists:seq(1, tuple_size(Shape))],
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_to_batched([{img, MatRef}, {batch_size, BatchSize}, {as_shape, MatShape}, {leftover, Leftover}]),
    evision_internal_structurise:to_struct(Ret).

to_batched(Mat, BatchSize, AsShape, Leftover) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatShape = [element(I, AsShape) || I <- lists:seq(1, tuple_size(AsShape))],
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_to_batched([{img, MatRef}, {batch_size, BatchSize}, {as_shape, MatShape}, {leftover, Leftover}]),
    evision_internal_structurise:to_struct(Ret).

clone(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_clone([{img, MatRef}]),
    evision_internal_structurise:to_struct(Ret).

empty() ->
    Ret = evision_nif:mat_empty(),
    evision_internal_structurise:to_struct(Ret).

to_binary(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_to_binary([{img, MatRef}]),
    evision_internal_structurise:to_struct(Ret).

from_binary(Bin, {T, L}, Rows, Cols, Channels) when is_binary(Bin), is_atom(T), L > 0, Rows > 0, Cols > 0, Channels > 0 ->
    Ret = evision_nif:mat_from_binary([{b, Bin}, {t, T}, {l, L}, {rows, Rows}, {cols, Cols}, {channels, Channels}]),
    evision_internal_structurise:to_struct(Ret).

from_binary_by_shape(Bin, {T, L}, Shape) when is_binary(Bin), is_atom(T), L > 0, is_tuple(Shape) ->
    from_binary_by_shape(Bin, {T, L}, [element(I,Shape) || I <- lists:seq(1,tuple_size(Shape))]);
from_binary_by_shape(Bin, {T, L}, Shape) when is_binary(Bin), is_atom(T), L > 0, is_list(Shape) ->
    Ret = evision_nif:mat_from_binary_by_shape([{b, Bin}, {t, T}, {l, L}, {shape, Shape}]),
    evision_internal_structurise:to_struct(Ret).

eye(N, {T, L}) when N > 0 ->
    Ret = evision_nif:mat_eye([{n, N}, {t, T}, {l, L}]),
    evision_internal_structurise:to_struct(Ret).

as_shape(Mat, AsShape) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    TheShape = [element(I,AsShape) || I <- lists:seq(1,tuple_size(AsShape))],
    {ok, MatShape} = shape(Mat),
    MatShapeList = [element(I,MatShape) || I <- lists:seq(1,tuple_size(MatShape))],
    OldTotal = lists:foldl(fun(X, Prod) -> X * Prod end, 1, MatShapeList),
    AsTotal = lists:foldl(fun(X, Prod) -> X * Prod end, 1, TheShape),
    case OldTotal == AsTotal of
        true ->
            MatRef = from_struct(Mat),
            Ret = evision_nif:mat_as_shape([{img, MatRef}, {as_shape, TheShape}]),
            evision_internal_structurise:to_struct(Ret);
        false ->
            {error, "Cannot treat mat as the requested new shape: mismatching number of elements"}
    end.

squeeze(Mat) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    {ok, MatShape} = shape(Mat),
    MatShapeList = [element(I,MatShape) || I <- lists:seq(1,tuple_size(MatShape))],
    MatShapeList1 = lists:filter(fun(Elem) -> Elem == 1 end, MatShapeList),
    reshape(Mat, MatShapeList1).

broadcast_to(Mat, ToShape) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    ToShapeList = [element(I,ToShape) || I <- lists:seq(1,tuple_size(ToShape))],
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_broadcast_to([{img, MatRef}, {to_shape, ToShapeList}, {force_src_shape, []}]),
    evision_internal_structurise:to_struct(Ret).

broadcast_to(Mat, ToShape, ForceSrcShape) when is_tuple(Mat), tuple_size(Mat) > 0, element(1, Mat) == evision_mat ->
    ToShapeList = [element(I,ToShape) || I <- lists:seq(1,tuple_size(ToShape))],
    ForceSrcShapeList = [element(I,ForceSrcShape) || I <- lists:seq(1,tuple_size(ForceSrcShape))],
    MatRef = from_struct(Mat),
    Ret = evision_nif:mat_broadcast_to([{img, MatRef}, {to_shape, ToShapeList}, {force_src_shape, ForceSrcShapeList}]),
    evision_internal_structurise:to_struct(Ret).
