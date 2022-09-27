-module(evision_mat).
-compile(nowarn_export_all).
-compile([export_all]).

full(Shape, Number, {T, L}) when is_tuple(Shape) ->
    ToShape = [element(I, Shape) || I <- lists:seq(1, tuple_size(Shape))],
    evision_nif:mat_full([{number, Number}, {t, T}, {l, L}, {shape, ToShape}]).

number(Number, Type) ->
    full({1, 1}, Number, Type).

at(Mat, Position) when is_reference(Mat), Position >= 0 ->
    evision_nif:mat_at([{img, Mat}, {pos, Position}]).

add(Lhs, Rhs) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_add([{l, Lhs}, {r, Rhs}]).

add(Lhs, Rhs, {T, L}) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_add_typed([{l, Lhs}, {r, Rhs}, {t, T}, {l, L}]).

subtract(Lhs, Rhs) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_subtract([{l, Lhs}, {r, Rhs}]).

subtract(Lhs, Rhs, {T, L}) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_subtract_typed([{l, Lhs}, {r, Rhs}, {t, T}, {l, L}]).

multiply(Lhs, Rhs) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_multiply([{l, Lhs}, {r, Rhs}]).

multiply(Lhs, Rhs, {T, L}) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_multiply_typed([{l, Lhs}, {r, Rhs}, {t, T}, {l, L}]).

matrix_multiply(Lhs, Rhs) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_matrix_multiply([{l, Lhs}, {r, Rhs}, {t, nil}, {l, 0}]).

matrix_multiply(Lhs, Rhs, {T, L}) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_matrix_multiply([{l, Lhs}, {r, Rhs}, {t, T}, {l, L}]).

divide(Lhs, Rhs) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_divide([{l, Lhs}, {r, Rhs}]).

divide(Lhs, Rhs, {T, L}) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_divide_typed([{l, Lhs}, {r, Rhs}, {t, T}, {l, L}]).

bitwise_and(Lhs, Rhs) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_bitwise_and([{l, Lhs}, {r, Rhs}]).

bitwise_or(Lhs, Rhs) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_bitwise_or([{l, Lhs}, {r, Rhs}]).

bitwise_xor(Lhs, Rhs) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_bitwise_xor([{l, Lhs}, {r, Rhs}]).

cmp(Lhs, Rhs, Op) when is_reference(Lhs), is_reference(Rhs) ->
    case lists:member(Op, [eq, gt, ge, lt, le, ne]) of
        true ->
            evision_nif:mat_cmp([{l, Lhs}, {r, Rhs}, {type, Op}]);
        false ->
            {error, "Unknown cmp opeator"}
    end.

logical_and(Lhs, Rhs) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_logical_and([{l, Lhs}, {r, Rhs}]).

logical_or(Lhs, Rhs) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_logical_or([{l, Lhs}, {r, Rhs}]).

logical_xor(Lhs, Rhs) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_logical_xor([{l, Lhs}, {r, Rhs}]).

abs(Mat) when is_reference(Mat) ->
    evision_nif:mat_abs([{img, Mat}]).

expm1(Mat) when is_reference(Mat) ->
    evision_nif:mat_expm1([{img, Mat}]).

clip(Mat, Lower, Upper) when is_reference(Mat) ->
    evision_nif:mat_clip([{img, Mat}, {lower, Lower}, {upper, Upper}]).

transpose(Mat, Axes, AsShape) when is_reference(Mat) ->
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
            evision_nif:mat_transpose([{img, Mat}, {axes, UniqAxes}, {as_shape, TheShape}]);
        false ->
            {error, "invalid transpose parameters"}
    end.

transpose(Mat, Axes) when is_reference(Mat) ->
    {ok, Shape} = shape(Mat),
    transpose(Mat, Axes, Shape).

transpose(Mat) when is_reference(Mat) ->
    {ok, Shape} = shape(Mat),
    Ndims = tuple_size(Shape),
    UniqAxes = lists:reverse(lists:seq(0, Ndims - 1)),
    transpose(Mat, UniqAxes, Shape).

type(Mat) when is_reference(Mat) ->
    evision_nif:mat_type([{img, Mat}]).

bitwise_not(Mat) when is_reference(Mat) ->
    {ok, {S, _}} = type(Mat),
    case lists:member(S, [s, u]) of
        true ->
            evision_nif:bitwise_not([{img, Mat}]);
        false ->
            {error, "bitwise operators expect integer tensors as inputs and outputs an integer tensor"}
    end.

ceil(Mat) when is_reference(Mat) ->
    evision_nif:mat_ceil([{img, Mat}]).

floor(Mat) when is_reference(Mat) ->
    evision_nif:mat_floor([{img, Mat}]).

negate(Mat) when is_reference(Mat) ->
    evision_nif:mat_negate([{img, Mat}]).

round(Mat) when is_reference(Mat) ->
    evision_nif:mat_round([{img, Mat}]).

sign(Mat) when is_reference(Mat) ->
    evision_nif:mat_sign([{img, Mat}]).

setTo(Mat, Value, Mask) when is_reference(Mat), is_reference(Mask) ->
    evision_nif:mat_set_to([{img, Mat}, {value, Value}, {mask, Mask}]).

dot(Lhs, Rhs) when is_reference(Lhs), is_reference(Rhs) ->
    evision_nif:mat_dot([{a, Lhs}, {b, Rhs}]).

as_type(Mat, {T, L}) when is_reference(Mat), is_atom(T), L > 0 ->
    evision_nif:mat_as_type([{img, Mat}, {t, T}, {l, L}]).

shape(Mat) when is_reference(Mat) ->
    evision_nif:mat_shape([{img, Mat}]).

channels(Mat) when is_reference(Mat) ->
    evision_nif:mat_channels([{img, Mat}]).

depth(Mat) when is_reference(Mat) ->
    evision_nif:mat_depth([{img, Mat}]).

raw_type(Mat) when is_reference(Mat) ->
    evision_nif:mat_raw_type([{img, Mat}]).

isSubmatrix(Mat) when is_reference(Mat) ->
    evision_nif:mat_isSubmatrix([{img, Mat}]).

isContinuous(Mat) when is_reference(Mat) ->
    evision_nif:mat_isContinuous([{img, Mat}]).

elemSize(Mat) when is_reference(Mat) ->
    evision_nif:mat_elemSize([{img, Mat}]).

elemSize1(Mat) when is_reference(Mat) ->
    evision_nif:mat_elemSize1([{img, Mat}]).

size(Mat) when is_reference(Mat) ->
    evision_nif:mat_size([{img, Mat}]).

total(Mat) when is_reference(Mat) ->
    evision_nif:mat_total([{img, Mat}, {start_dim, -1}, {end_dim, 4294967295}]).

total(Mat, StartDim) when is_reference(Mat) ->
    evision_nif:mat_total([{img, Mat}, {start_dim, StartDim}, {end_dim, 4294967295}]).

total(Mat, StartDim, EndDim) when is_reference(Mat) ->
    evision_nif:mat_total([{img, Mat}, {start_dim, StartDim}, {end_dim, EndDim}]).

last_dim_as_channel(Mat) when is_reference(Mat) ->
    evision_nif:mat_last_dim_as_channel([{src, Mat}]).

zeros(Shape, {T, L}) when is_tuple(Shape) ->
    MatShape = [element(I, Shape) || I <- lists:seq(1, tuple_size(Shape))],
    evision_nif:mat_zeros([{shape, MatShape}, {t, T}, {l, L}]).

ones(Shape, {T, L}) when is_tuple(Shape) ->
    MatShape = [element(I, Shape) || I <- lists:seq(1, tuple_size(Shape))],
    evision_nif:mat_ones([{shape, MatShape}, {t, T}, {l, L}]).

reshape(Mat, Shape) when is_tuple(Shape) ->
    MatShape = [element(I, Shape) || I <- lists:seq(1, tuple_size(Shape))],
    evision_nif:mat_reshape([{mat, Mat}, {shape, MatShape}]);
reshape(Mat, Shape) when is_list(Shape) ->
    evision_nif:mat_reshape([{mat, Mat}, {shape, Shape}]).

arange(From, To, Step, {T, L}) ->
    {ok, Mat} = evision_nif:mat_arange([{from, From}, {to, To}, {step, Step}, {t, T}, {l, L}]),
    {ok, {Length, _}} = shape(Mat),
    reshape(Mat, {1, Length}).

arange(From, To, Step, {T, L}, Shape) ->
    {ok, Mat} = evision_nif:mat_arange([{from, From}, {to, To}, {step, Step}, {t, T}, {l, L}]),
    reshape(Mat, Shape).

to_batched(Mat, BatchSize, Leftover) ->
    {ok, Shape} = shape(Mat),
    MatShape = [element(I, Shape) || I <- lists:seq(1, tuple_size(Shape))],
    evision_nif:mat_to_batched([{img, Mat}, {batch_size, BatchSize}, {as_shape, MatShape}, {leftover, Leftover}]).

to_batched(Mat, BatchSize, AsShape, Leftover) ->
    MatShape = [element(I, AsShape) || I <- lists:seq(1, tuple_size(AsShape))],
    evision_nif:mat_to_batched([{img, Mat}, {batch_size, BatchSize}, {as_shape, MatShape}, {leftover, Leftover}]).

clone(Mat) when is_reference(Mat) ->
    evision_nif:mat_clone([{img, Mat}]).

empty() ->
    evision_nif:mat_empty().

to_binary(Mat) when is_reference(Mat) ->
    evision_nif:mat_to_binary([{img, Mat}]).

from_binary(Bin, {T, L}, Rows, Cols, Channels) when is_binary(Bin), is_atom(T), L > 0, Rows > 0, Cols > 0, Channels > 0 ->
    evision_nif:mat_from_binary([{b, Bin}, {t, T}, {l, L}, {rows, Rows}, {cols, Cols}, {channels, Channels}]).

from_binary_by_shape(Bin, {T, L}, Shape) when is_binary(Bin), is_atom(T), L > 0, is_tuple(Shape) ->
    from_binary_by_shape(Bin, {T, L}, [element(I,Shape) || I <- lists:seq(1,tuple_size(Shape))]);
from_binary_by_shape(Bin, {T, L}, Shape) when is_binary(Bin), is_atom(T), L > 0, is_list(Shape) ->
    evision_nif:mat_from_binary_by_shape([{b, Bin}, {t, T}, {l, L}, {shape, Shape}]).

eye(N, {T, L}) when N > 0 ->
    evision_nif:mat_eye([{n, N}, {t, T}, {l, L}]).

as_shape(Mat, AsShape) when is_reference(Mat) ->
    TheShape = [element(I,AsShape) || I <- lists:seq(1,tuple_size(AsShape))],
    {ok, MatShape} = shape(Mat),
    MatShapeList = [element(I,MatShape) || I <- lists:seq(1,tuple_size(MatShape))],
    OldTotal = lists:foldl(fun(X, Prod) -> X * Prod end, 1, MatShapeList),
    AsTotal = lists:foldl(fun(X, Prod) -> X * Prod end, 1, TheShape),
    case OldTotal == AsTotal of
        true ->
            evision_nif:mat_as_shape([{img, Mat}, {as_shape, TheShape}]);
        false ->
            {error, "Cannot treat mat as the requested new shape: mismatching number of elements"}
    end.

squeeze(Mat) when is_reference(Mat) ->
    {ok, MatShape} = shape(Mat),
    MatShapeList = [element(I,MatShape) || I <- lists:seq(1,tuple_size(MatShape))],
    MatShapeList1 = lists:filter(fun(Elem) -> Elem == 1 end, MatShapeList),
    reshape(Mat, MatShapeList1).

broadcast_to(Mat, ToShape) ->
    ToShapeList = [element(I,ToShape) || I <- lists:seq(1,tuple_size(ToShape))],
    evision_nif:mat_broadcast_to([{img, Mat}, {to_shape, ToShapeList}, {force_src_shape, []}]).

broadcast_to(Mat, ToShape, ForceSrcShape) ->
    ToShapeList = [element(I,ToShape) || I <- lists:seq(1,tuple_size(ToShape))],
    ForceSrcShapeList = [element(I,ForceSrcShape) || I <- lists:seq(1,tuple_size(ForceSrcShape))],
    evision_nif:mat_broadcast_to([{img, Mat}, {to_shape, ToShapeList}, {force_src_shape, ForceSrcShapeList}]).
