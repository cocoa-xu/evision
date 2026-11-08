-module(evision_internal_structurise).
-export([to_struct/1, from_struct/1]).

to_struct(Ret) when is_map(Ret) ->
    case maps:find(class, Ret) of
        {ok, Class} ->
            Module = class_module(Class),
            case code:ensure_loaded(Module) of
                {module, Module} -> Module:to_struct(Ret);
                {error, _} -> Ret
            end;
        error ->
            Ret
    end;
to_struct(Tuple) when is_tuple(Tuple) ->
    List = [to_struct(element(I, Tuple)) || I <- lists:seq(1, tuple_size(Tuple))],
    list_to_tuple(List);
to_struct(List) when is_list(List) ->
    lists:map(fun (X) -> to_struct(X) end, List);
to_struct(PassThrough) ->
    PassThrough.

from_struct(Tuple) when is_tuple(Tuple), tuple_size(Tuple) > 0, is_atom(element(1, Tuple)) ->
    Name = element(1, Tuple),
    case lists:prefix("evision_", atom_to_list(Name)) of
        true -> Name:from_struct(Tuple);
        false -> list_to_tuple(from_struct(tuple_to_list(Tuple)))
    end;
from_struct(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(from_struct(tuple_to_list(Tuple)));
from_struct(List) when is_list(List) ->
    lists:map(fun from_struct/1, List);
from_struct(PassThrough) ->
    PassThrough.

%% 'Elixir.Evision.DNN.Net' -> evision_dnn_net
class_module(Class) ->
    Name = case atom_to_list(Class) of
        "Elixir.Evision." ++ Rest -> Rest;
        Other -> Other
    end,
    list_to_atom(string:lowercase(lists:flatten(["evision_", string:replace(Name, ".", "_", all)]))).
