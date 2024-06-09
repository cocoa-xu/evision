-module(evision_highgui).
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
