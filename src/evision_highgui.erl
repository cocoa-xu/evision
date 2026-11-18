-module(evision_highgui).
-behaviour(gen_server).
-compile(nowarn_export_all).
-compile([export_all]).

-include("generated/evision.hrl").

convertToShow(Image) ->
    case evision_mat:to_2d_image(Image) of
        #evision_mat{type = Type} = Mat ->
            case to_u8(Mat, Type) of
                #evision_mat{} = U8 -> evision_mat:to_rgb(U8);
                Error -> Error
            end;
        Error ->
            Error
    end.

to_u8(Mat, {u, 8}) -> Mat;
to_u8(Mat, {s, 8}) -> evision:convertScaleAbs(Mat, [{alpha, 1}, {beta, 127}]);
to_u8(Mat, {s, 16}) -> evision:convertScaleAbs(Mat, [{alpha, 1 / 255}, {beta, 127}]);
to_u8(Mat, {u, 16}) -> evision:convertScaleAbs(Mat, [{alpha, 1 / 255}]);
to_u8(Mat, {f, Bits}) when Bits == 32; Bits == 64 ->
    evision:addWeighted(Mat, 255.0, Mat, 0.0, 0.0, [{dtype, evision_nif:cv_8U()}]);
to_u8(Mat, Type) when Type == {f, 16}; Type == {bf, 16} ->
    case evision_mat:as_type(Mat, {f, 32}) of
        #evision_mat{} = F32 -> to_u8(F32, {f, 32});
        Error -> Error
    end;
to_u8(_Mat, Type) ->
    {error, iolist_to_binary(io_lib:format("cannot show an image of type ~p", [Type]))}.

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
