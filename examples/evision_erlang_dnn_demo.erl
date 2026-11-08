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
%% modified from the `ex_canvas` example
%%

-module(evision_erlang_dnn_demo).

-behaviour(wx_object).

%% Client API
-export([start/1, start/0]).

%% Detection API, usable without the GUI
-export([load_model/2, detect/3, draw_detections/2]).

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
%%% Detection model: OpenCV's ONNX export of TensorFlow's
%%% ssd_mobilenet_v2_coco_2018_03_29. It takes a 300x300 RGB uint8 image in
%%% NHWC layout and returns normalized {Y1, X1, Y2, X2} boxes, scores,
%%% 1-based COCO class ids and the number of detections.

-define(SSD_OUTPUTS, [<<"detection_boxes:0">>, <<"detection_scores:0">>,
                      <<"detection_classes:0">>, <<"num_detections:0">>]).

load_model(ModelFile, LabelFile) ->
    {ok, Data} = file:read_file(LabelFile),
    {evision_dnn:readNet(ModelFile), binary:split(Data, [<<"\n">>], [global])}.

detect({Net, Labels}, Frame, Threshold) ->
    {H, W, _} = evision_mat:shape(Frame),
    RGB = evision:cvtColor(Frame, evision_constant:cv_COLOR_BGR2RGB()),
    Pixels = evision_mat:to_binary(evision:resize(RGB, {300, 300})),
    Input = evision_mat:from_binary_by_shape(Pixels, {u, 8}, {1, 300, 300, 3}),
    [Boxes, Scores, Classes, Count] =
        evision_dnn_net:forward(evision_dnn_net:setInput(Net, Input), [{outBlobNames, ?SSD_OUTPUTS}]),
    Detections = lists:zip3(boxes(floats(Boxes)), floats(Scores), floats(Classes)),
    [{lists:nth(trunc(Class), Labels), Score,
      {trunc(X1 * W), trunc(Y1 * H), trunc(X2 * W), trunc(Y2 * H)}}
     || {{Y1, X1, Y2, X2}, Score, Class} <- lists:sublist(Detections, trunc(hd(floats(Count)))),
        Score > Threshold].

draw_detections(Frame, Detections) ->
    Font = evision_constant:cv_FONT_HERSHEY_SIMPLEX(),
    lists:foldl(
      fun({Label, Score, {L, T, R, B}}, Mat) ->
              Text = unicode:characters_to_binary(io_lib:format("~s: ~.2f", [Label, Score])),
              Boxed = evision:rectangle(Mat, {L, T}, {R, B}, {255, 0, 0}, [{thickness, 2}]),
              evision:putText(Boxed, Text, {L, max(T - 5, 12)}, Font, 0.5, {0, 0, 255})
      end, Frame, Detections).

floats(Mat) ->
    [F || <<F:32/float-native>> <= evision_mat:to_binary(Mat)].

boxes([Y1, X1, Y2, X2 | Rest]) -> [{Y1, X1, Y2, X2} | boxes(Rest)];
boxes([]) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(VideoFilePath) ->
    % https://huggingface.co/opencv/opencv_contribution/resolve/main/ssd_mobilenet_v2_coco_2018_03_29/ssd_mobilenet_v2_coco_2018_03_29_2026jul.onnx?download=true
    ModelFile = "examples/ssd_mobilenet_v2_coco_2018_03_29.onnx",
    % https://raw.githubusercontent.com/cocoa-xu/evision/main/test/testdata/models/coco_names.txt
    LabelFile = "examples/coco_names.txt",
    wx:batch(fun() -> do_init(VideoFilePath, load_model(ModelFile, LabelFile)) end).

do_init(VideoFilePath, Model) ->
    io:format("VideoFilePath: ~p~n", [VideoFilePath]),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Evision DNN Demo with Erlang"),
    Panel = wxPanel:new(Frame, []),
    VideoCapture = evision_videocapture:videoCapture(VideoFilePath),

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
        model=Model},
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
handle_info({_, update}, State=#state{run=Run, videocapture=VideoCapture, slider=Slider, model=Model}) ->
    if Run == true ->
        {CanvasW, CanvasH} = wxPanel:getSize(State#state.canvas),
        W = trunc(CanvasW),
        H = trunc(CanvasH),
        case evision_videocapture:read(VideoCapture) of
            Frame when is_tuple(Frame), element(1, Frame) == evision_mat ->
                Detections = detect(Model, Frame, wxSlider:getValue(Slider) / 100),
                ResultFrame = evision:resize(draw_detections(Frame, Detections), {W, H}),
                RGBFrame = evision:cvtColor(ResultFrame, evision_constant:cv_COLOR_BGR2RGB()),
                Img = wxImage:new(W, H, evision_mat:to_binary(RGBFrame)),
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
