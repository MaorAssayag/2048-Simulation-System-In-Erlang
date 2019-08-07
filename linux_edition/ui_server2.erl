%%%-------------------------------------------------------------------
%%% @author Maor Assayag, Refheal Shetrit
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2019 13:41
%%%-------------------------------------------------------------------
-module(ui_server2).
-author("Maor Assayag, Refheal Shetrit").

-include_lib("wx/include/wx.hrl").
-include("header.erl").

-behaviour(wx_object).

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {panel, frame, board, game_over, freeTiles, score, mainNode, grid_bit}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start() ->
  Wx = wx:new(),
  wx_object:start(?MODULE, [Wx], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Wx]) ->
  initUI(Wx).

initUI(Wx) ->
  Frame = wxFrame:new(Wx, -1, "2048"),
  wxWindow:setSize(Frame,0,0,540,580),
  wxWindow:setBackgroundColour(Frame, {250,248,239}),
  wxFrame:show(Frame),
  
  Panel = wxPanel:new(Frame, []),
  %wxFrame:connect(Frame, enter_window, [{callback, fun(_,_) ->
	%						     wxWindow:setFocus(Panel)
	%
%						     end}]),

  wxWindow:setFocus(Panel),
  wxWindow:connect(Panel, key_down),

  %% Grid background image
  CDC = wxClientDC:new(Panel),

  % Score rectangle
  wxDC:setPen(CDC, wxPen:new({205,193,180,255})),
  wxDC:setBrush(CDC, wxBrush:new({205,193,180,255})),
  wxDC:drawRectangle(CDC, {412,30,100,60}), % Base line : {75,125}
  Font3 = wxFont:new(17, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxDC:setTextForeground(CDC,{250,248,239}),
  wxDC:setFont(CDC,Font3),
  wxDC:drawText(CDC, "Score", {428,35}),


   % Title of the game
  NumList = integer_to_list(2048),
  Font11 = wxFont:new(40, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxDC:setTextForeground(CDC,{119,110,101}),
  wxDC:setFont(CDC,Font11),
  wxDC:drawText(CDC, NumList, {60,10}),
  Font12 = wxFont:new(18, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxDC:setFont(CDC,Font12),
  wxDC:drawText(CDC, "In Erlang", {63,73}),
  
  Logo = wxBitmap:new(wxImage:scale(wxImage:new("src/grid.PNG"),450,450)),
  wxDC:drawBitmap(CDC, Logo, {60,110}),
  draw_new_score(CDC, 0, Logo),
  timer:sleep(100),  
  % create new board with 2 tiles
  gen_server:cast(main_server,{reset_game}),
  {Frame, #state{panel=Panel, frame=Frame, game_over=false, grid_bit=Logo}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
  State :: #state{}) ->
{reply, Reply :: term(), NewState :: #state{}} |
{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
{noreply, NewState :: #state{}} |
{noreply, NewState :: #state{}, timeout() | hibernate} |
{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
{stop, Reason :: term(), NewState :: #state{}}).

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
{noreply, NewState :: #state{}} |
{noreply, NewState :: #state{}, timeout() | hibernate} |
{stop, Reason :: term(), NewState :: #state{}}).


handle_cast({update_game, Board, Score, GameOver, Won}, State = #state{panel=Panel, grid_bit=Logo}) ->
    CDC = wxClientDC:new(Panel),
    case GameOver of
      true -> draw_message_over(CDC, Won);
      false ->draw_new_score(CDC, Score, Logo),
              redrew_board(CDC, Board)
    end,
    wxClientDC:destroy(CDC),
    {noreply, State#state{game_over=GameOver}};

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
{noreply, NewState :: #state{}} |
{noreply, NewState :: #state{}, timeout() | hibernate} |
{stop, Reason :: term(), NewState :: #state{}}).

handle_info(Msg, State) ->
  io:format("frame got unexpected message ~p~n", [Msg]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling keystrokes from the user
%%
%% @spec handle_event(Event, State) -> {noreply, State}
%% @end
%%--------------------------------------------------------------------
%handle_event(#wx{event=#wxMouse{type=enter_window}}, State = #state{frame=Frame}) ->
%  io:format("focus ~n"),
%  wxWindow:setFocus(Frame), %% Get keyboard focus
%  {noreply,State};
 

handle_event(#wx{event=#wxKey{keyCode=?SPACEBAR}}, State) ->
  io:format("SAPCEBAR ~n"),
  gen_server:cast(main_server, {reset_game}),
  {noreply, State};

% then the game is over
handle_event(#wx{event=#wxKey{}}, State = #state{game_over=true}) ->
  io:format("GAME OVER!~n"),
  {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=?DOWN}}, State) ->
  io:format("DOWN ~n"),
  gen_server:cast(main_server, {game_move, down}),
  {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=?UP}}, State) ->
  io:format("UP ~n"),
  gen_server:cast(main_server, {game_move, up}),
  {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=?RIGHT}}, State) ->
    io:format("RIGHT ~n"),
    gen_server:cast(main_server, {game_move, right}),
    {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=?LEFT}}, State) ->
    io:format("LEFT ~n"),
    gen_server:cast(main_server, {game_move, left}),
    {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=_}}, State) ->
    {noreply, State};


handle_event(#wx{event = #wxClose{}}, State) ->
    gen_server:call(main_server,shutdown),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%%
%%% Internal functions
%%%
%%%===================================================================
%% 16 tiles of {{#row,#col},value}

%% Take the Board tuple and draw each tile in it accordingly
redrew_board(CDC, Board)->
    redrew_board_rec(CDC, Board, 16).
redrew_board_rec(_, _, TileNum) when TileNum =:= 0 ->
    ok;
redrew_board_rec(CDC, Board, TileNum) ->
  add_tile(CDC,element(TileNum,Board)),
  redrew_board_rec(CDC, Board,TileNum-1).

draw_new_score(CDC, Score, Logo) ->
  case Score of
    % we are drawing a new game, draw the grid background
    0 ->  wxDC:drawBitmap(CDC, Logo, {60,110});
    _ -> ok
  end,
  wxDC:setPen(CDC, wxPen:new({205,193,180,255})),
  wxDC:setBrush(CDC, wxBrush:new({205,193,180,255})),
  wxDC:drawRectangle(CDC, {412,60,100,30}), % Base line : {75,125}
  Font = wxFont:new(16, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxDC:setTextForeground(CDC,{250,248,239}),
  wxDC:setFont(CDC,Font),
  wxDC:drawText(CDC, integer_to_list(Score), {428,63}).


%% draw new tile in Position and value
add_tile(CDC, {Position, 0}) ->
  % return 0 if it's free tile
  clear_tile(CDC, Position);

%              {#row, #col}
add_tile(CDC, {{PosY, PosX},Value}) ->
  %% Draw 1 rectangle
  RXInit = 75,
  RYInit = 125,
  {Offset, {R,G,B}, TextColor, TextSize} = tile_spec_selector(Value),
  wxDC:setPen(CDC, wxPen:new({R,G,B,255})),
  wxDC:setBrush(CDC, wxBrush:new({R,G,B,255})),
  wxDC:drawRectangle(CDC, {(PosX-1)*109+RXInit, RYInit+(PosY-1)*109 ,95,95}), % Base line : {75,125}
  %% Draw Value in the rectangle
  XInit = 108,
  YInit = 145,
  Font = wxFont:new(TextSize, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  NumList = integer_to_list(Value),
  wxDC:setTextForeground(CDC,TextColor),
  wxDC:setFont(CDC,Font),
  wxDC:drawText(CDC, NumList, {(PosX-1)*110 + XInit + Offset, (35-TextSize) + YInit + (PosY-1)*110}),
  % return 1 if it's not a free tile
  1.

clear_tile(CDC, {PosY, PosX}) ->
  RXInit = 75,
  RYInit = 125,
  wxDC:setPen(CDC, wxPen:new({205,193,180,255})),
  wxDC:setBrush(CDC, wxBrush:new({205,193,180,255})),
  wxDC:drawRectangle(CDC, {(PosX-1)*109+RXInit, RYInit+(PosY-1)*109 ,95,95}),
  0. % Base line : {75,125}


% rectangle color of 2 : 238,228,218
% rectangle color of 4 : 238,225,201
% rectangle color of 8 : 243,178,122  // white text
% rectangle color of 16 : 246,150,100 // white text
% rectangle color of 32 : 247,124,95
% rectangle color of 64 : 247,95,59
% rectangle color of 128 : 237,208,115
% rectangle color of 256 : 247,203,91
% rectangle color of 512 : 238,199,80
% rectangle color of 1024 : 242,196,58
% rectangle color of 2048 : 239,193,46
% tile_spec_selector -> {offset, {rectabgle color}, {text color}}
tile_spec_selector(2) ->
    {0, {238,228,218}, {119,110,101}, 36};
tile_spec_selector(4) ->
    {0, {238,225,201}, {119,110,101}, 36};
tile_spec_selector(8) ->
    {0, {243,178,122}, {244,244,244}, 36};
tile_spec_selector(16) ->
    {-10, {246,150,100}, {244,244,244}, 32};
tile_spec_selector(32) ->
    {-10, {247,124,95}, {244,244,244}, 32};
tile_spec_selector(64) ->
    {-10, {247,95,59}, {244,244,244}, 32};
tile_spec_selector(128) ->
    {-20, {237,208,115}, {244,244,244}, 28};
tile_spec_selector(256) ->
    {-20, {247,203,91}, {244,244,244}, 28};
tile_spec_selector(512) ->
    {-20, {238,199,80}, {244,244,244}, 28};
tile_spec_selector(1024) ->
    {-25, {242,196,58}, {244,244,244}, 24};
tile_spec_selector(2048) ->
    {-25, {239,193,46}, {244,244,244}, 24};
tile_spec_selector(_) ->
    {0, {238,228,218}, {244,244,244}, 36}.

draw_message_over(CDC, Won) ->
  wxDC:setPen(CDC, wxPen:new({205,193,180,255})),
  wxDC:setBrush(CDC, wxBrush:new({205,193,180,255})),
  wxDC:drawRectangle(CDC, {75,234,425,207}), % Base line : {75,125}
  %% Draw Value in the rectangle
  Font = wxFont:new(40, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxDC:setFont(CDC,Font),
  case Won of
    true -> wxDC:setTextForeground(CDC,{218,165,32}),
            wxDC:drawText(CDC, "You won!", {160, 300});
    false ->wxDC:setTextForeground(CDC,{119,110,101}),
            wxDC:drawText(CDC, "Game Over!", {120, 300})
  end.
