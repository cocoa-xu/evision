-module(evision_highgui).
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
to_u8(Mat, {f, 16}) -> to_u8(evision_mat:as_type(Mat, {f, 32}), {f, 32});
to_u8(Mat, {bf, 16}) -> to_u8(evision_mat:as_type(Mat, {f, 32}), {f, 32});
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
