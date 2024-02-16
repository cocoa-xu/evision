-module(evision_windows_fix).
-compile(nowarn_export_all).
-compile([export_all]).

-on_load(init/0).

-define(APPNAME, evision).
-define(LIBNAME, windows_fix).

init() ->
    case os:type() of
        {win32, _} ->
            SoName = case code:priv_dir(?APPNAME) of
                {error, bad_name} ->
                    case filelib:is_dir(filename:join(["..", priv])) of
                        true ->
                            filename:join(["..", priv, ?LIBNAME]);
                        _ ->
                            filename:join([priv, ?LIBNAME])
                    end;
                Dir ->
                    filename:join(Dir, ?LIBNAME)
            end,
            erlang:load_nif(SoName, 0),
            run_once();
        _ ->
            ok
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

run_once() ->
    case os:type() of
        {win32, _} ->
            case erlang:load_nif(filename:join(code:priv_dir(?APPNAME), ?LIBNAME), 0) of
                true ->
                    ok;
                _ ->
                    not_loaded(?LINE)
            end;
        _ ->
            ok
    end.

