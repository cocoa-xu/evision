-module(evision_internal_structurise).
-export([to_struct/1, from_struct/1]).

to_struct(Ret) when is_map(Ret) ->
    case maps:is_key(class, Ret) of
        true ->
            Class = maps:get(class, Ret),
            ClassName = atom_to_list(Class),
            Module = case string:prefix(ClassName, "Elixir.Evision.") of
                nomatch ->
                    list_to_atom(string:to_lower(io_lib:fwrite("evision_~s", [ClassName])));
                Name ->
                    list_to_atom(string:to_lower("evision_" ++ Name))
            end,
            try Module:to_struct(Ret) of
                Result ->
                    Result
            catch
                _ ->
                    Ret
            end;
        false ->
            Ret
    end;
to_struct(Tuple) when is_tuple(Tuple) ->
    List = [to_struct(element(I, Tuple)) || I <- lists:seq(1, tuple_size(Tuple))],
    list_to_tuple(List);
to_struct(List) when is_list(List) ->
    lists:map(fun (X) -> to_struct(X) end, List);
to_struct(PassThrough) ->
    PassThrough.

from_struct(MaybeRecord) when is_tuple(MaybeRecord), tuple_size(MaybeRecord) > 0, is_atom(element(1, MaybeRecord)) ->
    RecordName = element(1, MaybeRecord),
    try RecordName:from_struct(MaybeRecord) of
        Result ->
            Result
    catch 
        _ ->
            List = [to_struct(element(I, MaybeRecord)) || I <- lists:seq(1, tuple_size(MaybeRecord))],
            list_to_tuple(List)
    end;
from_struct(Tuple) when is_tuple(Tuple) ->
    List = [to_struct(element(I, Tuple)) || I <- lists:seq(1, tuple_size(Tuple))],
    list_to_tuple(List);
from_struct(List) when is_list(List) ->
    lists:map(fun (X) -> from_struct(X) end, List);
from_struct(PassThrough) ->
    PassThrough.
