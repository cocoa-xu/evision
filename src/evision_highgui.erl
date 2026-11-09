-module(evision_highgui).
-behaviour(gen_server).
-compile(nowarn_export_all).
-compile([export_all]).

imshow(WinName, Mat) when is_binary(WinName) ->
    evision_nif:imshow([{winname, WinName}, {mat, evision_internal_structurise:from_struct(Mat)}]).

waitKey(Delay) when is_integer(Delay) ->
    evision_nif:waitKey([{delay, Delay}]).

destroyWindow(WinName) when is_binary(WinName) ->
    evision_nif:destroyWindow([{winname, WinName}]).

destroyAllWindows() ->
    evision_nif:destroyAllWindows().

setMouseCallback(WinName, OnMouse) when is_binary(WinName), (is_function(OnMouse, 4) orelse OnMouse =:= nil) ->
    with_runner(fun() -> evision_nif:setMouseCallback([{winname, WinName}, {on_mouse, OnMouse}]) end).

createTrackbar(TrackbarName, WinName, Value, Count) ->
    createTrackbar(TrackbarName, WinName, Value, Count, nil).

createTrackbar(TrackbarName, WinName, Value, Count, OnChange)
  when is_binary(TrackbarName), is_binary(WinName), is_integer(Value), is_integer(Count),
       (is_function(OnChange, 1) orelse OnChange =:= nil) ->
    with_runner(fun() ->
        evision_nif:createTrackbar([{trackbarname, TrackbarName}, {winname, WinName},
                                    {value, Value}, {count, Count}, {on_change, OnChange}])
    end).

%% The native GUI thread sends callbacks to the process registered as evision_highgui.
with_runner(Fun) ->
    whereis(?MODULE) =:= undefined andalso gen_server:start({local, ?MODULE}, ?MODULE, [], []),
    Fun().

init([]) ->
    {ok, nil}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({evision_highgui_callback, Fun, Args}, State) ->
    try
        apply(Fun, Args)
    catch
        Kind:Reason:Stacktrace ->
            logger:error("Evision.HighGui callback failed: ~ts",
                         [erl_error:format_exception(Kind, Reason, Stacktrace)])
    end,
    {noreply, State};
handle_info({evision_highgui_error, Message}, State) ->
    logger:error("Evision.HighGui: ~ts", [Message]),
    {noreply, State};
%% Callbacks run in this process and may leave messages behind, such as late replies.
handle_info(_Message, State) ->
    {noreply, State}.
