%%%-------------------------------------------------------------------
%%% @author Maor Assayag, Refheal Shetrit
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2019 13:41
%%%-------------------------------------------------------------------
-module(ui_server).
-author("Maor Assayag, Refheal Shetrit").

-include_lib("wx/include/wx.hrl").
-include("header.erl").

-behaviour(wx_object).

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {frame, board, game_over, freeTiles, score, botsContainer, totalNodes,
                totalBots, liveBots, totalWins, totalLosts, avgScoreWins, avgMovesWins,
                listCtrlNodeStatus, nodesStatus}).

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
  wxWindow:setSize(Frame,0,0,940,650),
  wxWindow:setBackgroundColour(Frame, {250,248,239}),
  wxFrame:createStatusBar(Frame),
  draw_stats(Frame, 0, 0, 0, 0, 0, 0.0, 0.0),
  wxFrame:show(Frame),

  wxFrame:connect(Frame, enter_window),
  [wxWindow:connect(Frame, Types) || Types <- [key_down]],

  %% Grid background image
  Logo = wxBitmap:new(wxImage:scale(wxImage:new("src/grid.png"),450,450)),
  CDC = wxClientDC:new(Frame),
  wxDC:drawBitmap(CDC, Logo, {60,110}),

  % Title of the game
  Font = wxFont:new(45, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  NumList = integer_to_list(2048),
  wxDC:setTextForeground(CDC,{119,110,101}),
  wxDC:setFont(CDC,Font),
  wxDC:drawText(CDC, NumList, {60,10}),
  Font2 = wxFont:new(20, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxDC:setFont(CDC,Font2),
  wxDC:drawText(CDC, "In Erlang", {63,73}),

  % Score rectangle
  wxDC:setPen(CDC, wxPen:new({205,193,180,255})),
  wxDC:setBrush(CDC, wxBrush:new({205,193,180,255})),
  wxDC:drawRectangle(CDC, {412,30,100,60}), % Base line : {75,125}
  Font3 = wxFont:new(18, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxDC:setTextForeground(CDC,{250,248,239}),
  wxDC:setFont(CDC,Font3),
  wxDC:drawText(CDC, "Score", {428,30}),
  draw_new_score(CDC, 0),

  % Bottom UI
  CreateBots = wxButton:new(Frame, 10, [{label,"Deploy"}, {pos, {640,515}}, {size, {60,35}}]),
  %StopBots = wxButton:new(Frame, 11, [{label,"Stop bots"}, {pos, {540,575}}, {size, {80,35}}]),
  NumOfBotsLabel = wxTextCtrl:new(Frame, 12, [{value, "0"}, {pos, {540,515}}, {size, {80,35}}]),
  Font4 = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxTextCtrl:setFont(NumOfBotsLabel, Font4),
  wxButton:connect(CreateBots, command_button_clicked),
  %wxButton:connect(StopBots, command_button_clicked),

  % wxChoice for bot's algorithm choice
  AlgoChoices = ["Random","Max Score","Stay Alive","Max Tiles Merged", "Heuristic scoring"],
  ChoiceAlgo = wxChoice:new(Frame, 13, [{choices, AlgoChoices}, {pos, {540,145}}, {size, {130,-1}}]),
  wxChoice:setToolTip(ChoiceAlgo, "Choose the decision algorithm for the bots"),
  wxChoice:connect(ChoiceAlgo,command_choice_selected),

  %wxListBox for the bots threshold Max Tile (down from 2048)
  ThresholdChoices = ["2048","1024","512","256","128"],
  ChoiceThres = wxChoice:new(Frame, 14, [{choices, ThresholdChoices}, {pos, {540,225}}, {size, {130,-1}}]),
  wxChoice:setToolTip(ChoiceThres, "Choose the max value tile for a bot to win"),
  wxChoice:connect(ChoiceThres,command_choice_selected),

  %ListCtrlNodeStatus for the node status table
  ListCtrlNodeStatus = wxListCtrl:new(Frame, [{style,?wxLC_REPORT},{pos,{540,305}},{size,{360,150}}]),
  wxListCtrl:setTextColour(ListCtrlNodeStatus, {255,255,255}),
  wxListCtrl:insertColumn(ListCtrlNodeStatus, 0, "Node PID", [{width,130}]),
  wxListCtrl:insertColumn(ListCtrlNodeStatus, 1, "Status", [{width,50}]),
  wxListCtrl:insertColumn(ListCtrlNodeStatus, 2, "#Simulations", [{width,90}]),
  wxListCtrl:insertColumn(ListCtrlNodeStatus, 3, "#Finished", [{width,90}]),

  % Titles
  Font6 = wxFont:new(18, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxDC:setFont(CDC,Font6),
  wxDC:setTextForeground(CDC,{119,110,101}),
  wxDC:drawText(CDC, "Simulation Console", {540,60}),
  Font7 = wxFont:new(12, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxDC:setFont(CDC,Font7),
  wxDC:drawText(CDC, "Bot Win Threshold", {540,195}),
  wxDC:drawText(CDC, "Decision algorithm", {540,115}),
  wxDC:drawText(CDC, "Nodes Status", {540,275}),
  wxDC:drawText(CDC, "Create Bots", {540,485}),

  % create new board with 2 tiles
  NewBoard = new_game_tile(new_game_tile(get_default_board(),true,false),true,false),
  redrew_board(CDC, NewBoard),
  {Frame, #state{frame=Frame, board=NewBoard, game_over=false, score=0, botsContainer=NumOfBotsLabel,
                totalNodes=0, totalBots=0, liveBots=0, totalWins=0, totalLosts=0,
                avgScoreWins=0, avgMovesWins=0, listCtrlNodeStatus=ListCtrlNodeStatus, nodesStatus=[]}}.

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

handle_cast({update_stats, NewNodes, NewBots, NewLiveBots, NewWins, NewLosts, NewAvgScoreWins, NewAvgMovesWins}, State = #state{frame=Frame}) ->
  io:format("ui_server: update stats~n"),
  % draw the new stats
  draw_stats(Frame, NewNodes, NewBots, NewLiveBots, NewWins, NewLosts, NewAvgScoreWins, NewAvgMovesWins),
  {noreply, State#state{totalNodes=NewNodes, totalBots=NewBots,liveBots=NewLiveBots, totalWins=NewWins,
                      totalLosts=NewLosts, avgScoreWins=NewAvgScoreWins, avgMovesWins=NewAvgMovesWins}};

handle_cast({update_node_status, NodePID, Status, NumOfSimulations, NumOfFinished}, State = #state{listCtrlNodeStatus=ListCtrlNodeStatus, nodesStatus=NodesStatus}) ->
  case lists:keyfind(NodePID, 1, NodesStatus) of
    % A new node has been connected, add it to the data base and update list UI
    false ->
      NewList = NodesStatus ++ [{NodePID, Status, NumOfSimulations, NumOfFinished, length(NodesStatus)}],
      update_node_list(ListCtrlNodeStatus, 1, length(NodesStatus), NodePID, Status, NumOfSimulations, NumOfFinished);
    T ->
      {_,_,_,_,NodeIndex} = lists:keyfind(NodePID, 1, NodesStatus),
      NewList = lists:delete(T, NodesStatus) ++ [{NodePID, Status, NumOfSimulations, NumOfFinished, NodeIndex}],
      update_node_list(ListCtrlNodeStatus, 0, NodeIndex, NodePID, Status, NumOfSimulations, NumOfFinished)
    end,
    {noreply, State#state{nodesStatus=NewList}};

handle_cast(_Msg, State) ->
  {noreply, State}.

update_node_list(ListCtrl, AddNewNode, Index, NodePID, Status, NumOfSimulations, NumOfFinished) ->
  case AddNewNode of
    0 ->
      wxListCtrl:setItem(ListCtrl, Index, 0, atom_to_list(NodePID)),
      wxListCtrl:setItem(ListCtrl, Index, 2, integer_to_list(NumOfSimulations)),
      wxListCtrl:setItem(ListCtrl, Index, 3, integer_to_list(NumOfFinished)),
      case Status of
        1 ->
          wxListCtrl:setItem(ListCtrl, Index, 1, "Live"),
          wxListCtrl:setItemBackgroundColour(ListCtrl,Index,{0,230,0});
        0 ->
          wxListCtrl:setItem(ListCtrl, Index, 1, "Down"),
          wxListCtrl:setItemBackgroundColour(ListCtrl,Index,{194,76,60})
      end;
    1 ->
      wxListCtrl:insertItem(ListCtrl, Index, ""),
      wxListCtrl:setItem(ListCtrl, Index, 0, atom_to_list(NodePID)),
      wxListCtrl:setItem(ListCtrl, Index, 1, "Live"),
      wxListCtrl:setItem(ListCtrl, Index, 2, integer_to_list(NumOfSimulations)),
      wxListCtrl:setItem(ListCtrl, Index, 3, integer_to_list(0)),
      wxListCtrl:setItemBackgroundColour(ListCtrl,Index,{0,230,0})
    end.

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
handle_event(#wx{event=#wxMouse{type=enter_window}}, State = #state{frame=Frame}) ->
  wxWindow:setFocus(Frame), %% Get keyboard focus
  {noreply,State};

handle_event(#wx{event=#wxKey{keyCode=?SPACEBAR}}, State = #state{frame=Frame}) ->
  %io:format("SAPCEBAR ~n"),
  % create a new board with 2 tiles
  CDC = wxClientDC:new(Frame),
  Logo = wxBitmap:new(wxImage:scale(wxImage:new("src/grid.png"),450,450)),
  wxDC:drawBitmap(CDC, Logo, {60,110}),
  NewBoard = new_game_tile(new_game_tile(get_default_board(),true,false),true,false),
  NewState = State#state{board = NewBoard, game_over=false, score=0, freeTiles=true},
  redrew_board(CDC, NewBoard),
  draw_new_score(CDC,0),
  wxClientDC:destroy(CDC),
  {noreply, NewState};

% then the game is over
handle_event(#wx{event=#wxKey{}}, State = #state{game_over=true}) ->
  io:format("GAME OVER!~n"),
  {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=?DOWN}}, State = #state{frame=Frame, board=Board, score=OldScore}) ->
  %io:format("DOWN ~n"),
  CDC = wxClientDC:new(Frame),
  {BoardAfterOperation, NewAddition2Score} = down_operation(Board),
  {GameOver, HasFreeTiles, Won} = is_game_over(tuple_to_list(BoardAfterOperation)),
  NewBoard = new_game_tile(BoardAfterOperation, HasFreeTiles,BoardAfterOperation=:=Board),
  case GameOver of
    true -> draw_message_over(CDC, Won);
    false -> redrew_board(CDC, NewBoard),
            draw_new_score(CDC, NewAddition2Score + OldScore)
  end,
  wxClientDC:destroy(CDC),
  NewState = State#state{board = NewBoard, game_over=GameOver, freeTiles=HasFreeTiles, score=OldScore+NewAddition2Score},
  {noreply, NewState};

handle_event(#wx{event=#wxKey{keyCode=?UP}}, State = #state{frame=Frame, board=Board, score=OldScore}) ->
  %io:format("UP ~n"),
  CDC = wxClientDC:new(Frame),
  {BoardAfterOperation, NewAddition2Score} = up_operation(Board),
  {GameOver, HasFreeTiles, Won} = is_game_over(tuple_to_list(BoardAfterOperation)),
  NewBoard = new_game_tile(BoardAfterOperation, HasFreeTiles,BoardAfterOperation=:=Board),
  case GameOver of
    true -> draw_message_over(CDC, Won);
    false -> redrew_board(CDC, NewBoard),
            draw_new_score(CDC, NewAddition2Score + OldScore)
  end,
  wxClientDC:destroy(CDC),
  NewState = State#state{board = NewBoard, game_over=GameOver, freeTiles=HasFreeTiles, score=OldScore+NewAddition2Score},
  {noreply, NewState};

handle_event(#wx{event=#wxKey{keyCode=?RIGHT}}, State = #state{frame=Frame, board=Board, score=OldScore}) ->
    %io:format("RIGHT ~n"),
    CDC = wxClientDC:new(Frame),
    {BoardAfterOperation, NewAddition2Score} = right_operation(Board),
    {GameOver, HasFreeTiles, Won} = is_game_over(tuple_to_list(BoardAfterOperation)),
    NewBoard = new_game_tile(BoardAfterOperation, HasFreeTiles,BoardAfterOperation=:=Board),
    case GameOver of
      true -> draw_message_over(CDC, Won);
      false -> redrew_board(CDC, NewBoard),
              draw_new_score(CDC, NewAddition2Score + OldScore)
    end,
    wxClientDC:destroy(CDC),
    NewState = State#state{board = NewBoard, game_over=GameOver, freeTiles=HasFreeTiles, score=OldScore+NewAddition2Score},
    {noreply, NewState};

handle_event(#wx{event=#wxKey{keyCode=?LEFT}}, State = #state{frame=Frame, board=Board, score=OldScore}) ->
    %io:format("LEFT ~n"),
    CDC = wxClientDC:new(Frame),
    {BoardAfterOperation, NewAddition2Score} = left_operation(Board),
    {GameOver, HasFreeTiles, Won} = is_game_over(tuple_to_list(BoardAfterOperation)),
    NewBoard = new_game_tile(BoardAfterOperation, HasFreeTiles,BoardAfterOperation=:=Board),
    case GameOver of
      true -> draw_message_over(CDC, Won);
      false -> redrew_board(CDC, NewBoard),
               draw_new_score(CDC, NewAddition2Score + OldScore)
    end,
    wxClientDC:destroy(CDC),
    NewState = State#state{board = NewBoard, game_over=GameOver, freeTiles=HasFreeTiles, score=OldScore+NewAddition2Score},
    {noreply, NewState};

handle_event(#wx{event=#wxKey{keyCode=_}}, State) ->
    {noreply, State};

handle_event(#wx{id = Id, event = #wxCommand{type = command_button_clicked}},State=#state{botsContainer=NumOfBotsLabel})->
  Num = list_to_integer(wxTextCtrl:getValue(NumOfBotsLabel)),
  case Id of
    10 ->
      %% Create Bots
      case Num of
        0 ->
          {noreply, State};

        _ ->
          wxTextCtrl:setValue(NumOfBotsLabel, "0"),
          gen_server:cast(main_server,{request_bots, Num}),
          {noreply, State}
      end;
    20 ->
      %% Kill botsNum
      ok;
    _ -> {noreply, State}
  end;

handle_event(#wx{id=ID, event=#wxCommand{type=command_choice_selected,cmdString=Value, commandInt=Index}}, State=#state{}) ->
  case ID of
    % Bot Algorithm Choice
    13 ->
        gen_server:cast(main_server,{update_simulation_params, 1, Index}),
        io:format("ui_server: Bot Decision ID changed to : ~p ~n", [Value]),
        {noreply, State};
    % Threshold choice
    14 ->
      gen_server:cast(main_server,{update_simulation_params, 2, element(Index+1,{2048,1024,512,256,128})}),
      io:format("ui_server: The threshold value of a tile for a bot to win changed to : ~p ~n", [Value]),
      {noreply, State};
    _ -> {noreply, State}
  end;

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
%% sys:terminate(player1, Reason),
%% sys:terminate(player2, Reason), wx:destroy(),
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
get_default_board() ->
  {{{1,1},0}, {{1,2},0}, {{1,3},0}, {{1,4},0}, {{2,1},0}, {{2,2},0},
  {{2,3},0}, {{2,4},0}, {{3,1},0}, {{3,2},0}, {{3,3},0}, {{3,4},0}, {{4,1},0},
  {{4,2},0}, {{4,3},0}, {{4,4},0}}.

%% Take the Board tuple and draw each tile in it accordingly
redrew_board(CDC, Board)->
    redrew_board_rec(CDC, Board, 16).
redrew_board_rec(_, _, TileNum) when TileNum =:= 0 ->
    ok;
redrew_board_rec(CDC, Board, TileNum) ->
  add_tile(CDC,element(TileNum,Board)),
  redrew_board_rec(CDC, Board,TileNum-1).

draw_new_score(CDC, Score) ->
  wxDC:setPen(CDC, wxPen:new({205,193,180,255})),
  wxDC:setBrush(CDC, wxBrush:new({205,193,180,255})),
  wxDC:drawRectangle(CDC, {412,60,100,30}), % Base line : {75,125}
  Font = wxFont:new(16, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxDC:setTextForeground(CDC,{250,248,239}),
  wxDC:setFont(CDC,Font),
  wxDC:drawText(CDC, integer_to_list(Score), {428,63}).

draw_stats(Frame, TotalNodes, TotalBots, LiveBots, TotalWins, TotalLosts, AvgScoreWins, AvgMovesWins) ->
  wxFrame:setStatusText(Frame, "Live Nodes:  " ++ integer_to_list(TotalNodes)
                                ++ "        Total Bots:  " ++ integer_to_list(TotalBots)
                                ++ "        Live Games(Bots):  " ++ integer_to_list(LiveBots)
                                ++ "        Wins:  " ++ integer_to_list(TotalWins)
                                ++ "        Losses:  " ++ integer_to_list(TotalLosts)
                                ++ "        Average Win score:  " ++ float_to_list(AvgScoreWins,[{decimals,0}])
                                ++ "        Average moves in a win:  " ++ float_to_list(AvgMovesWins,[{decimals,0}])).

is_game_over(BoardList) ->
  IsAlive = lists:any(fun({{X,Y},Val}) ->
      Alive1 = case lists:keyfind({X-1,Y},1,BoardList) of
                  {_,ValT1}-> Val =:= ValT1;
                   false -> false
               end,
      Alive2 = case lists:keyfind({X,Y-1},1,BoardList) of
                  {_,ValT2}-> Val =:= ValT2;
                  false -> false
               end,
      Alive1 or Alive2
  end,BoardList),
  HasFreeTiles = lists:any(fun({_,Val}) ->
      FreeTile = case Val of
                  0 -> true;
                  _ -> false
                 end,
      FreeTile
  end,BoardList),
  Won = lists:any(fun({_,Val}) ->
      FreeTile = case Val of
                  2048 -> true;
                  _ -> false
                 end,
      FreeTile
  end,BoardList),
  {(not (IsAlive or HasFreeTiles)) or Won, HasFreeTiles, Won}.

push_elemnt_right(List) ->
  Ans = [Res || Res <- List, Res > 0],
  padding_zeroes_right(Ans).

padding_zeroes_right(List) ->
  case length(List) of
    4 -> Ans = List;
    3 -> Ans = [0] ++ List;
    2 -> Ans = [0,0] ++ List;
    1 -> Ans = [0,0,0] ++ List;
    0 -> Ans = [0,0,0,0];
    _ -> Ans = List
  end,
  Ans.

push_elemnt_left(List) ->
  Ans = [Res || Res <- List, Res > 0],
  padding_zeroes_left(Ans).

padding_zeroes_left(List) ->
  case length(List) of
    4 -> Ans = List;
    3 -> Ans = List ++ [0];
    2 -> Ans = List ++ [0,0];
    1 -> Ans = List ++ [0,0,0];
    0 -> Ans = [0,0,0,0];
    _ -> Ans = List
  end,
  Ans.

%down operation that can be called from handle_event #key_down
down_operation(Board) ->
  down_operation(down_operation(down_operation(down_operation({Board,0},1),2),3),4).
down_operation({Board,AccScore}, Col) ->
    [{TP1,TV1},{TP2,TV2}, {TP3,TV3},{TP4,TV4}] =[element(Col,Board),
    element(4 + Col,Board),
    element(8 + Col,Board),
    element(12 + Col,Board)],
    % it gets score in here
    {T11, T22, T33, T44, NewScore} = down_column([TV1,TV2,TV3,TV4]),
    {setelement(12 + Col,
    setelement(8 + Col,
    setelement(4 + Col,
    setelement(Col,Board,{TP1,T11}),
    {TP2,T22}),
    {TP3,T33}),
    {TP4,T44}), AccScore+NewScore}.

%% gets tile values of a column, when TV1 belongs to a tile in the 1'st row
down_column(List) ->
  [TV1,TV2,TV3,TV4] = push_elemnt_right(List),
  if TV3 =:= TV4 andalso TV1=:=TV2 ->
        Score = TV4*2+TV2*2,
        T33 = TV2*2, T44 = TV4*2, T22 = 0, T11 = 0;
    TV3 =:= TV4 ->
        Score = TV4*2,
        T44 = TV4*2, T33 = TV2, T22 = TV1, T11 = 0;
    TV1 =:= TV2 andalso TV2 =:= TV3 ->
        Score = TV3*2,
        T44 = TV4, T33 = TV3*2, T22 = TV2, T11 = 0;
    TV1 =:= TV2 ->
        Score = TV2*2,
        T44 = TV4, T33 = TV3, T22 = TV2*2, T11 = 0;
    TV3 =:= TV2 ->
        Score = TV3*2,
        T44 = TV4, T33 = TV3*2, T22 = TV1, T11 = 0;
    true ->
        Score = 0,
        T11 = TV1, T22 = TV2, T33 = TV3, T44 = TV4
  end,
  % Score = 0 for now
  {T11,T22,T33,T44,Score}.

%ופ operation that can be called from handle_event #key_down
up_operation(Board) ->
  up_operation(up_operation(up_operation(up_operation({Board,0},1),2),3),4).
up_operation({Board,AccScore}, Col) ->
    [{TP1,TV1},{TP2,TV2}, {TP3,TV3},{TP4,TV4}] =[element(Col,Board),
    element(4 + Col,Board),
    element(8 + Col,Board),
    element(12 + Col,Board)],
    % it gets score in here
    {T11, T22, T33, T44, NewScore} = up_column([TV1,TV2,TV3,TV4]),
    {setelement(12 + Col,
    setelement(8 + Col,
    setelement(4 + Col,
    setelement(Col,Board,{TP1,T11}),
    {TP2,T22}),
    {TP3,T33}),
    {TP4,T44}), AccScore+NewScore}.

%% gets tile values of a column, when TV1 belongs to a tile in the 1'st row
up_column(List) ->
  [TV1,TV2,TV3,TV4] = push_elemnt_left(List),
  if TV1 =:= TV2 andalso TV3=:=TV4 ->
        Score = TV1*2+TV3*2,
        T11 = TV1*2, T22 = TV3*2,T33 = 0, T44 = 0;
    TV1 =:= TV2 ->
        Score = TV1*2,
        T11 = TV1*2, T22 = TV3, T33 = TV4, T44 = 0;
    TV3 =:= TV4 andalso TV3 =:= TV2->
        Score = TV2*2,
        T11 = TV1, T22 = TV2*2, T33 = TV4, T44 = 0;
    TV3 =:= TV4 ->
        Score = TV3*2,
        T11 = TV1, T22 = TV2, T33 = TV3*2, T44 = 0;
    TV3 =:= TV2 ->
        Score = TV2*2,
        T11 = TV1, T22 = TV2*2,T33 = TV4, T44 = 0;
    true ->
        Score = 0,
        T11 = TV1, T22 = TV2, T33 = TV3, T44 = TV4
  end,
  {T11,T22,T33,T44,Score}.

%right operation that can be called from handle_event #key_down
right_operation(Board) ->
  right_operation(right_operation(right_operation(right_operation({Board,0},1),2),3),4).
right_operation({Board,AccScore}, Row) ->
    [{TP1,TV1},{TP2,TV2},{TP3,TV3},{TP4,TV4}] =[element((Row-1)*4 + 1,Board),
    element((Row-1)*4 + 2,Board),
    element((Row-1)*4 + 3,Board),
    element((Row-1)*4 + 4,Board)],
    % it gets score in here
    {T11, T22, T33, T44, NewScore} = right_row([TV1,TV2,TV3,TV4]),
    {setelement((Row-1)*4 + 4,
    setelement((Row-1)*4 + 3,
    setelement((Row-1)*4 + 2,
    setelement((Row-1)*4 + 1,Board,{TP1,T11}),
    {TP2,T22}),
    {TP3,T33}),
    {TP4,T44}), AccScore+NewScore}.

%% gets tile values of a column, when TV1 belongs to a tile in the 1'st row
right_row(List) ->
  [TV1,TV2,TV3,TV4] = push_elemnt_right(List),
  if TV3 =:= TV4 andalso TV1=:=TV2 ->
          Score = TV4*2+TV2*2,
          T33 = TV2*2, T44 = TV4*2, T22 = 0, T11 = 0;
      TV3 =:= TV4 ->
          Score = TV4*2,
          T44 = TV4*2, T33 = TV2, T22 = TV1, T11 = 0;
      TV1 =:= TV2 andalso TV2 =:= TV3 ->
          Score = TV3*2,
          T44 = TV4, T33 = TV3*2, T22 = TV2, T11 = 0;
      TV1 =:= TV2 ->
          Score = TV2*2,
          T44 = TV4, T33 = TV3, T22 = TV2*2, T11 = 0;
      TV3 =:= TV2 ->
          Score = TV3*2,
          T44 = TV4, T33 = TV3*2, T22 = TV1, T11 = 0;
      true ->
          Score = 0,
          T11 = TV1, T22 = TV2, T33 = TV3, T44 = TV4
  end,
  {T11,T22,T33,T44,Score}.

%left operation that can be called from handle_event #key_down
left_operation(Board) ->
  left_operation(left_operation(left_operation(left_operation({Board,0},1),2),3),4).
left_operation({Board,AccScore}, Row) ->
    [{TP1,TV1},{TP2,TV2}, {TP3,TV3},{TP4,TV4}] =[element((Row-1)*4 + 1,Board),
    element((Row-1)*4 + 2,Board),
    element((Row-1)*4 + 3,Board),
    element((Row-1)*4 + 4,Board)],
    % it gets score in here
    {T11, T22, T33, T44, NewScore} = left_row([TV1,TV2,TV3,TV4]),
    {setelement((Row-1)*4 + 4,
    setelement((Row-1)*4 + 3,
    setelement((Row-1)*4 + 2,
    setelement((Row-1)*4 + 1,Board,{TP1,T11}),
    {TP2,T22}),
    {TP3,T33}),
    {TP4,T44}), AccScore+NewScore}.

%% gets tile values of a column, when TV1 belongs to a tile in the 1'st row
left_row(List) ->
  [TV1,TV2,TV3,TV4] = push_elemnt_left(List),
  if TV3 =:= TV4 andalso TV1=:=TV2 ->
        Score = TV4*2+TV2*2,
        T22 = TV3*2, T11= TV2*2, T33 = 0, T44 = 0;
      TV1 =:= TV2 ->
        Score = TV1*2,
        T11 = TV1*2, T22 = TV3, T33 = TV4, T44 = 0;
    TV3 =:= TV4 andalso TV2 =:= TV3->
        Score = TV2*2,
        T11 = TV1, T22 = TV2*2, T33 = TV3, T44 = 0;
    TV3 =:= TV4 ->
        Score = TV3*2,
        T11 = TV1, T22 = TV2, T33 = TV3*2, T44 = 0;
    TV3 =:= TV2 ->
        Score = TV2*2,
        T11 = TV1, T22 = TV2*2, T33 = TV4, T44 = 0;
    true ->
        Score = 0,
        T11 = TV1, T22 = TV2,T33 = TV3, T44 = TV4
    end,
  {T11,T22,T33,T44,Score}.

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

%% create a new tile on the grid with the value 2 or 4, used in each new step
new_game_tile(Board, _, true) ->
  Board;
new_game_tile(Board, false,_) ->
  Board;
new_game_tile(Board, _, _) ->
  generate_game_tile(Board, rand:uniform(16)).
generate_game_tile(Board, TileNum) ->
    Tile = element(TileNum,Board),
    case element(2,Tile) of
      %% its a free tile, we can create there a tile
      % 20% chance for new tile with the value 4, 80% chance for the value 2
      0 -> NewBoard = setelement(TileNum,Board,setelement(2,Tile,2*(floor(1+(rand:uniform(120)/100)))));
      %% try again
      _ -> NewBoard = generate_game_tile(Board, rand:uniform(16))
    end,
    NewBoard.

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
    {0, {238,228,218}, {119,110,101}, 40};
tile_spec_selector(4) ->
    {0, {238,225,201}, {119,110,101}, 40};
tile_spec_selector(8) ->
    {0, {243,178,122}, {244,244,244}, 40};
tile_spec_selector(16) ->
    {-10, {246,150,100}, {244,244,244}, 36};
tile_spec_selector(32) ->
    {-10, {247,124,95}, {244,244,244}, 36};
tile_spec_selector(64) ->
    {-10, {247,95,59}, {244,244,244}, 36};
tile_spec_selector(128) ->
    {-20, {237,208,115}, {244,244,244}, 32};
tile_spec_selector(256) ->
    {-20, {247,203,91}, {244,244,244}, 32};
tile_spec_selector(512) ->
    {-20, {238,199,80}, {244,244,244}, 32};
tile_spec_selector(1024) ->
    {-25, {242,196,58}, {244,244,244}, 26};
tile_spec_selector(2048) ->
    {-25, {239,193,46}, {244,244,244}, 26};
tile_spec_selector(_) ->
    {0, {238,228,218}, {244,244,244}, 40}.

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
          wxDC:drawText(CDC, "Game Over!", {140, 300})
end.