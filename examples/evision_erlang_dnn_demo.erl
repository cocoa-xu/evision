%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
%% Copyright Cocoa Xu 2022
%% modified from the `ex_canvas` exmaple
%%

-module(evision_erlang_dnn_demo).

-behaviour(wx_object).

%% Client API
-export([start/1, start/0]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2, handle_sync_event/3]).

-include_lib("wx/include/wx.hrl").

-record(state, 
	{
      config,
      button,
	  win,
      bitmap,
      canvas,
      overlay,
      pos,
      parent,
      timer,
      run,
      videocapture,
      model,
      slider
	}).

start() ->
    start("demo.mp4").

start(VideoFilePath) ->
    wx:new(),
    wx_object:start_link(?MODULE, VideoFilePath, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% helper functions for detection model

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).

load_model(LabelFile, Opts) ->
    case evision_nif:dnn_readNet(Opts) of
        {ok, Model} ->
            {ok, OutNames} = evision_dnn_net:getUnconnectedOutLayersNames(Model),
            Labels = readlines(LabelFile),
            {Model, Labels, OutNames, [300, 300]};
        Error ->
            io:format("Cannot open model: ~p~n", [Error]),
            throw(Error)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(VideoFilePath) ->
    % http://download.tensorflow.org/models/object_detection/ssd_mobilenet_v2_coco_2018_03_29.tar.gz
    ModelFilePath = "examples/ssd_mobilenet_v2_coco_2018_03_29/frozen_inference_graph.pb",
    % https://raw.githubusercontent.com/opencv/opencv_extra/master/testdata/dnn/ssd_mobilenet_v2_coco_2018_03_29.pbtxt
    ModelOpts = [{model, ModelFilePath}, {config, "examples/ssd_mobilenet_v2_coco_2018_03_29.pbtxt"}, {framework, ""}],
    % https://raw.githubusercontent.com/cocoa-xu/evision/main/test/models/coco_names.txt
    ModelLabelFile = "examples/coco_names.txt",
    wx:batch(fun() -> do_init(VideoFilePath, ModelLabelFile, ModelOpts) end).

do_init(VideoFilePath, ModelLabelFile, ModelOpts) ->
    io:format("VideoFilePath: ~p~n", [VideoFilePath]),
    {Model, Labels, OutNames, InputShape} = load_model(ModelLabelFile, ModelOpts),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Evision DNN Demo with Erlang"),
    Panel = wxPanel:new(Frame, []),
    {ok, VideoCapture} = evision_videocapture:videoCapture(VideoFilePath),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				 [{label, "DNN Video Detection with Evision"}]),

    Button = wxButton:new(Panel, ?wxID_ANY, [{label, "Run"}]),

    Canvas = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),

    wxPanel:connect(Canvas, paint, [callback]),
    wxPanel:connect(Canvas, size),
    wxPanel:connect(Canvas, left_down),
    wxPanel:connect(Canvas, left_up),
    wxPanel:connect(Canvas, motion),

    wxPanel:connect(Button, command_button_clicked),

    %% Add to sizers
    wxSizer:add(Sizer, Button, [{border, 5}, {flag, ?wxALL}]),
    wxSizer:addSpacer(Sizer, 5),
    wxSizer:add(Sizer, Canvas, [{flag, ?wxEXPAND},
				{proportion, 1}]),

    Slider = wxSlider:new(Panel, 1, 60, 0, 100,
				[{style, ?wxSL_HORIZONTAL bor
				  ?wxSL_LABELS}]),
    wxSizer:add(Sizer, Slider, [{border, 5}, {flag, ?wxEXPAND}]),

    wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND},
				   {proportion, 1}]),

    wxPanel:setSizer(Panel, MainSizer),
    wxSizer:layout(MainSizer),

    {W,H} = wxPanel:getSize(Canvas),
    Bitmap = wxBitmap:new(
        erlang:max(W,30),
        erlang:max(30,H)),
    wxFrame:show(Frame),
    process_flag(trap_exit, true),
    self() ! {self(), update},
    State = #state{win=Frame, parent=Panel, bitmap=Bitmap, 
        button=Button,
        canvas=Canvas, overlay=wxOverlay:new(), config=undefined, 
        run=false,
        videocapture=VideoCapture,
        slider=Slider,
        model={Model, Labels, OutNames, InputShape}},
    {Frame, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync event from callback events, paint event must be handled in callbacks
%% otherwise nothing will be drawn on windows.
handle_sync_event(#wx{event = #wxPaint{}}, _wxObj,
		  #state{canvas=Canvas, bitmap=Bitmap}) ->
    DC = wxPaintDC:new(Canvas),
    redraw(DC, Bitmap),
    wxPaintDC:destroy(DC),
    ok.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxCommand{type = command_button_clicked}},
	     State = #state{run=Run, button=Button}) ->
    if Run == true ->
        wxButton:setLabel(Button, "Continue"),
        {noreply, State#state{run=false}};
       true ->
        self() ! {self(), update},
        wxButton:setLabel(Button, "Pause"),
        {noreply, State#state{run=true}}
    end;
handle_event(#wx{event = #wxSize{size={W,H}}},
	     State = #state{bitmap=Prev, canvas=Canvas}) ->
    if W > 0 andalso H > 0 ->
	    Bitmap = wxBitmap:new(W,H),
	    draw(Canvas, Bitmap, fun(DC) -> wxDC:clear(DC) end),
	    wxBitmap:destroy(Prev),
	    {noreply, State#state{bitmap = Bitmap}};
       true ->
	    {noreply, State}
    end;
handle_event(#wx{event = #wxMouse{type=left_down, x=X, y=Y}}, State) ->
    {noreply, State#state{pos={X,Y}}};
handle_event(#wx{event = #wxMouse{type=motion, x=X1, y=Y1}},
	     #state{pos=Start, overlay=Overlay, canvas=Canvas} = State) ->
    case Start of
	undefined -> ignore;
	{X0,Y0} ->
	    DC = wxClientDC:new(Canvas),
	    DCO = wxDCOverlay:new(Overlay, DC),
	    wxDCOverlay:clear(DCO),
	    wxDC:setPen(DC, ?wxLIGHT_GREY_PEN),
	    wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
	    wxDC:drawRectangle(DC, {X0,Y0, X1-X0, Y1-Y0}),
	    wxDCOverlay:destroy(DCO),
	    wxClientDC:destroy(DC)
    end,
    {noreply, State};
handle_event(#wx{event = #wxMouse{type=left_up}},
	     #state{overlay=Overlay, canvas=Canvas} = State) ->
    DC = wxClientDC:new(Canvas),
    DCO = wxDCOverlay:new(Overlay, DC),
    wxDCOverlay:clear(DCO),
    wxDCOverlay:destroy(DCO),
    wxClientDC:destroy(DC),
    wxOverlay:reset(Overlay),
    {noreply, State#state{pos=undefined}};

handle_event(Ev = #wx{}, State = #state{}) ->
    demo:format(State#state.config, "Got Event ~p\n", [Ev]),
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info({'EXIT',_, wx_deleted}, State) ->
    {noreply,State#state{run=false}};
handle_info({'EXIT',_, normal}, State) ->
    {noreply,State#state{run=false}};
handle_info({_, update}, State=#state{run=Run, videocapture=VideoCapture, slider=Slider, model={Model, Labels, OutNames, InputShape}}) ->
    if Run == true ->
        {CanvasW, CanvasH} = wxPanel:getSize(State#state.canvas),
        W = trunc(CanvasW),
        H = trunc(CanvasH),
        case evision_videocapture:read(VideoCapture) of
            {ok, Frame} ->
                Opts = [
                    {name, ""},
                    {scalefactor, 1.0},
                    {mean, [0, 0, 0]},
                    {swapRB, true},
                    {size, InputShape}
                ],
                {ok, FrameForInference} = evision:resize(Frame, InputShape),
                {ok, Blob} = evision_dnn:blobFromImage(FrameForInference, Opts),
                {ok, ReadyModel} = evision_dnn_net:setInput(Model, Blob, Opts),
                {ok, Detections} = evision_dnn_net:forward(ReadyModel, [{outBlobNames, OutNames}]),
                TranslatedOuts = postprocess(Frame, Detections, ReadyModel, wxSlider:getValue(Slider)/100),
                ResultFrame = visualise_pred(Frame, Labels, TranslatedOuts),

                {ok, NewFrame} = evision:resize(ResultFrame, [W, H]),
                {ok, RGBFrame} = evision:cvtColor(NewFrame, evision:cv_COLOR_BGR2RGB()),
                {ok, Binary} = evision_mat:to_binary(RGBFrame),
                Img = wxImage:new(W, H, Binary),
                Bmp = wxBitmap:new(Img),
                Fun = fun(DC) ->
                    wxDC:clear(DC),
                    wxDC:drawBitmap(DC, Bmp, {0, 0})
                end,
                draw(State#state.canvas, State#state.bitmap, Fun),
                wxBitmap:destroy(Bmp),
                wxImage:destroy(Img),
                self() ! {self(), update},
                {noreply, State};
            Error ->
                io:format("Error reading video: ~p\n",
                            [Error]),
                {noreply, State#state{run=false}}
        end;
       true ->
        {noreply, State}
    end;
handle_info(Msg, State) ->
    io:format("Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};
handle_call(Msg, _From, State) ->
    io:format("Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, #state{overlay=Overlay}) ->
    wxOverlay:destroy(Overlay),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Buffered makes it all appear on the screen at the same time
draw(Canvas, Bitmap, Fun) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    Fun(MemoryDC),

    CDC = wxWindowDC:new(Canvas),
    wxDC:blit(CDC, {0,0},
	      {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
	      MemoryDC, {0,0}),    
    wxWindowDC:destroy(CDC),
    wxMemoryDC:destroy(MemoryDC).

redraw(DC, Bitmap) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    wxDC:blit(DC, {0,0},
	      {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
	      MemoryDC, {0,0}),
    wxMemoryDC:destroy(MemoryDC).

postprocess(Mat, Detections, Model, ConfidenceThreshold) ->
    {ok, OutLayers} = evision_dnn_net:getUnconnectedOutLayers(Model),
    [OutLayer0 | _] = OutLayers,
    {ok, OutLayer} = evision_dnn_net:getLayer(Model, OutLayer0),
    OutLayerType = evision_dnn_layer:get_type(OutLayer),
    postprocess_by_outtype(Mat, Detections, Model, ConfidenceThreshold, OutLayerType, []).

postprocess_by_outtype(_, [], _, _, "DetectionOutput", Acc) ->
    lists:reverse(Acc);
postprocess_by_outtype(Mat, [Outs | Detections], Model, ConfidenceThreshold, OutType="DetectionOutput", Acc) ->
    {ok, Data} = evision_mat:to_binary(Outs),
    {ok, {H, W, _}} = evision_mat:shape(Mat),
    TranslatedOuts = translate_outs(ConfidenceThreshold, Data, H, W, []),
    postprocess_by_outtype(Mat, Detections, Model, ConfidenceThreshold, OutType, [TranslatedOuts | Acc]).

translate_outs(_, <<>>, _, _, Acc) ->
    Acc;
translate_outs(ConfidenceThreshold, <<_:1/little-float-unit:32,
    ClassID:1/little-float-unit:32, Confidence:1/little-float-unit:32, 
    Left:1/little-float-unit:32, 
    Top:1/little-float-unit:32, 
    Right:1/little-float-unit:32,
    Bottom:1/little-float-unit:32,
    Rest/binary>>, H, W, Acc) ->
    if Confidence > ConfidenceThreshold ->
        Width = Right - Left + 1,
        Height = Bottom - Top + 1,
        [L, T, R, B] =
            if (Width < 2) or (Height < 2) ->
                [Left * W, Top * H, Right * W, Bottom * H];
               true ->
                [Left, Top, Right, Bottom]
            end,
        translate_outs(ConfidenceThreshold, Rest, H, W, [
            {trunc(ClassID), Confidence, trunc(L), trunc(T), trunc(R), trunc(B)} | Acc
        ]);
       true ->
        translate_outs(ConfidenceThreshold, Rest, H, W, Acc)
    end.


visualise_pred(Mat, _, []) ->
    Mat;
visualise_pred(Mat, Labels, TranslatedOuts) ->
    do_visualise_pred(Mat, Labels, TranslatedOuts).

do_visualise_pred(Mat, _, []) ->
    Mat;
do_visualise_pred(Mat, _, [[]]) ->
    Mat;
do_visualise_pred(Mat, Labels, [Group | Rest]) when is_list(Group) ->
    NewMat = do_visualise_pred(Mat, Labels, Group),
    do_visualise_pred(NewMat, Labels, Rest);
do_visualise_pred(Mat, Labels, [{ClassID, Confidence, L, T, R, B} | Outs]) ->
    ClassLabel = lists:nth(ClassID, Labels),
    ClassText = erlang:list_to_binary(io_lib:format("~s: ~f", [ClassLabel, Confidence])),
    {ok, NewMat} = evision:rectangle(Mat, [L, T], [R, B], [255, 0, 0]),

    {ok, {{LabelWeight, LabelHeight}, Baseline}} = evision_nif:getTextSize([
        {text, ClassText},
        {fontFace, evision:cv_FONT_HERSHEY_SIMPLEX()},
        {fontScale, 0.5},
        {thickness, 1}]),
    LabelWeightI = trunc(LabelWeight),
    LabelHeightI = trunc(LabelHeight),
    Top = max(T, LabelHeightI),
    {ok, NewNewMat} =
      evision:rectangle(NewMat, [L, Top - LabelHeightI], [L + LabelWeightI, Top + Baseline], [
        255,
        255,
        255
      ], [{thickness, -1}]),
    {ok, MatWithText} =
      evision:putText(NewNewMat, binary:bin_to_list(ClassText), [L, Top], evision:cv_FONT_HERSHEY_SIMPLEX(), 0.5, [0, 0, 255]),
    do_visualise_pred(MatWithText, Labels, Outs).
