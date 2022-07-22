-module(evision_mat).
-compile(nowarn_export_all).
-compile([export_all]).

type(Mat) when is_reference(Mat) ->
    evision_nif:mat_type([{img, Mat}]).

as_type(Mat, {T, L}) when is_reference(Mat), is_atom(T), L > 0 ->
    evision_nif:mat_as_type([{img, Mat}, {t, T}, {l, L}]).

shape(Mat) when is_reference(Mat) ->
    evision_nif:mat_shape([{img, Mat}]).

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
