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
-record(state, {panel, frame, botsContainer, totalNodes,
                totalBots, liveBots, totalWins, totalLosts, avgScoreWins, avgMovesWins,
                listCtrlNodeStatus, nodesStatus, backupNode, mainNode}).
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
  wxWindow:setSize(Frame,0,0,950,600),
  wxWindow:setBackgroundColour(Frame, {250,248,239}),
  wxFrame:createStatusBar(Frame),
  draw_stats(Frame, 0, 0, 0, 0, 0, 0.0, 0.0),
  wxFrame:show(Frame),
  wxWindow:setFocus(Frame),
  %Panel = wxPanel:new(Frame, []),
  Panel=Frame,

  % Bottom UI
  CreateBots = wxButton:new(Panel, 10, [{label,"Deploy"}, {pos, {640,515}}, {size, {60,35}}]),
  %StopBots = wxButton:new(Frame, 11, [{label,"Stop bots"}, {pos, {540,575}}, {size, {80,35}}]),
  NumOfBotsLabel = wxTextCtrl:new(Panel, 12, [{value, "0"}, {pos, {540,515}}, {size, {80,35}}]),
  Font4 = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxTextCtrl:setFont(NumOfBotsLabel, Font4),
  wxButton:connect(CreateBots, command_button_clicked),
  %wxButton:connect(StopBots, command_button_clicked),

  % wxChoice for bot's algorithm choice
  AlgoChoices = ["Random","Max Score","Stay Alive","Max Tiles Merged", "Heuristic scoring"],
  ChoiceAlgo = wxChoice:new(Panel, 13, [{choices, AlgoChoices}, {pos, {540,145}}, {size, {130,-1}}]),
  wxChoice:setToolTip(ChoiceAlgo, "Choose the decision algorithm for the bots"),
  wxChoice:connect(ChoiceAlgo,command_choice_selected),

  %wxListBox for the bots threshold Max Tile (down from 2048)
  ThresholdChoices = ["2048","1024","512","256","128"],
  ChoiceThres = wxChoice:new(Panel, 14, [{choices, ThresholdChoices}, {pos, {540,225}}, {size, {130,-1}}]),
  wxChoice:setToolTip(ChoiceThres, "Choose the max value tile for a bot to win"),
  wxChoice:connect(ChoiceThres,command_choice_selected),

  %ListCtrlNodeStatus for the node status table
  ListCtrlNodeStatus = wxListCtrl:new(Panel, [{style,?wxLC_REPORT},{pos,{540,305}},{size,{370,150}}]),
  wxListCtrl:setTextColour(ListCtrlNodeStatus, {255,255,255}),
  wxListCtrl:insertColumn(ListCtrlNodeStatus, 0, "Node name", [{width,130}]),
  wxListCtrl:insertColumn(ListCtrlNodeStatus, 1, "Status", [{width,70}]),
  wxListCtrl:insertColumn(ListCtrlNodeStatus, 2, "#Simulations", [{width,90}]),
  wxListCtrl:insertColumn(ListCtrlNodeStatus, 3, "#Finished", [{width,80}]),

  % Titles
  CDC = wxClientDC:new(Frame),
  Font1 = wxFont:new(12, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxDC:setTextForeground(CDC,{119,110,101}),
  wxDC:setFont(CDC,Font1),
  wxDC:drawText(CDC, "Bot Win Threshold", {540,195}),
  wxDC:drawText(CDC, "Decision algorithm", {540,115}),
  wxDC:drawText(CDC, "Nodes Status", {540,275}),
  wxDC:drawText(CDC, "Create Bots", {540,485}),
  
  Font2 = wxFont:new(18, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ? wxFONTWEIGHT_BOLD),
  wxDC:setFont(CDC,Font2),
  wxDC:setTextForeground(CDC,{119,110,101}),
  wxDC:drawText(CDC, "Simulation Console", {540,60}),
  
  % create new board with 2 tiles
  {Frame, #state{panel=Panel, frame=Frame, botsContainer=NumOfBotsLabel,
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

handle_cast({update_node_status, NodePID, Status, NumOfSimulations, NumOfFinished}, State = #state{listCtrlNodeStatus=ListCtrlNodeStatus, nodesStatus=NodesStatus, panel=Panel}) ->
  case lists:keyfind(NodePID, 1, NodesStatus) of
    % A new node has been connected, add it to the data base and update list UI
    false ->
      NewList = NodesStatus ++ [{NodePID, Status, NumOfSimulations, NumOfFinished, length(NodesStatus)}],
      update_node_list(ListCtrlNodeStatus, 1, length(NodesStatus)+2, NodePID, Status, NumOfSimulations, NumOfFinished, Panel);
    T ->
      {_,_,_,_,NodeIndex} = T,
      NewList = lists:delete(T, NodesStatus) ++ [{NodePID, Status, NumOfSimulations, NumOfFinished, NodeIndex}],
      update_node_list(ListCtrlNodeStatus, 0, NodeIndex+2, NodePID, Status, NumOfSimulations, NumOfFinished, Panel)
    end,
    {noreply, State#state{nodesStatus=NewList}};

handle_cast({update_main_backup_nodes, MainNode, BackupNode}, State = #state{listCtrlNodeStatus=ListCtrlNodeStatus, panel=Panel}) ->
    update_node_list(ListCtrlNodeStatus, 2, 0, MainNode, 0, 0, 0, Panel),
    update_node_list(ListCtrlNodeStatus, 3, 1, BackupNode, 0, 0, 0, Panel),
    {noreply, State#state{mainNode=MainNode, backupNode=BackupNode}};

handle_cast(_Msg, State) ->
  {noreply, State}.

update_node_list(ListCtrl, AddNewNode, Index, NodePID, Status, NumOfSimulations, NumOfFinished, _) ->
  case AddNewNode of
    0 ->
      wxListCtrl:setItem(ListCtrl, Index, 0, atom_to_list(NodePID)),
      wxListCtrl:setItem(ListCtrl, Index, 2, integer_to_list(NumOfSimulations)),
      wxListCtrl:setItem(ListCtrl, Index, 3, integer_to_list(NumOfFinished)),
      case Status of
        1 ->
          wxListCtrl:setItem(ListCtrl, Index, 1, "Live"),
          wxListCtrl:setItemBackgroundColour(ListCtrl,Index,{0,190,0});
        0 ->
          wxListCtrl:setItem(ListCtrl, Index, 1, "Down"),
          wxListCtrl:setItemBackgroundColour(ListCtrl,Index,{194,76,60})
      end;
    1 ->
      wxListCtrl:insertItem(ListCtrl, Index, ""),
      wxListCtrl:setItem(ListCtrl, Index, 0, atom_to_list(NodePID)),
      wxListCtrl:setItem(ListCtrl, Index, 2, integer_to_list(NumOfSimulations)),
      wxListCtrl:setItem(ListCtrl, Index, 3, integer_to_list(NumOfFinished)),
      case Status of
        1 ->
          wxListCtrl:setItem(ListCtrl, Index, 1, "Live"),
          wxListCtrl:setItemBackgroundColour(ListCtrl,Index,{0,190,0});
        0 ->
          wxListCtrl:setItem(ListCtrl, Index, 1, "Down"),
          wxListCtrl:setItemBackgroundColour(ListCtrl,Index,{194,76,60})
      end;
    % main node
    2 ->
      wxListCtrl:insertItem(ListCtrl, Index, ""),
      wxListCtrl:deleteItem(ListCtrl, Index),
      wxListCtrl:insertItem(ListCtrl, Index, ""),
      wxListCtrl:setItem(ListCtrl, Index, 0, atom_to_list(NodePID)),
      wxListCtrl:setItem(ListCtrl, Index, 1, "Main"),
      wxListCtrl:setItem(ListCtrl, Index, 2, []),
      wxListCtrl:setItem(ListCtrl, Index, 3, []),
      wxListCtrl:setItemBackgroundColour(ListCtrl,Index,{0,120,255});
    % backup node
    3 ->
      wxListCtrl:insertItem(ListCtrl, Index, ""),
      wxListCtrl:deleteItem(ListCtrl, Index),
      wxListCtrl:insertItem(ListCtrl, Index, ""),
      wxListCtrl:setItem(ListCtrl, Index, 1, "Backup"),
      wxListCtrl:setItem(ListCtrl, Index, 2, []),
      wxListCtrl:setItem(ListCtrl, Index, 3, []),
      case NodePID of
        [] ->
          wxListCtrl:setItem(ListCtrl, Index, 0, ""),
          wxListCtrl:setItemBackgroundColour(ListCtrl,Index,{194,76,60});
        _ ->
          wxListCtrl:setItem(ListCtrl, Index, 0, atom_to_list(NodePID)),
          wxListCtrl:setItemBackgroundColour(ListCtrl,Index,{0,120,255})
        end
    end.
    %wxWindow:setFocus(Panel).

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
 

handle_event(#wx{id = Id, event = #wxCommand{type = command_button_clicked}},State=#state{botsContainer=NumOfBotsLabel})->
  Num = list_to_integer(wxTextCtrl:getValue(NumOfBotsLabel)),
  case Id of
    10 ->
      %% Create Bots
      case Num of
        0 ->
          %wxWindow:setFocus(Panel),
          {noreply, State};

        _ ->
          wxTextCtrl:setValue(NumOfBotsLabel, "0"),
          gen_server:cast(main_server,{request_bots, Num}),
          %wxWindow:setFocus(Panel),
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
        %wxWindow:setFocus(Panel),
        {noreply, State};
    % Threshold choice
    14 ->
      gen_server:cast(main_server,{update_simulation_params, 2, element(Index+1,{2048,1024,512,256,128})}),
      io:format("ui_server: The threshold value of a tile for a bot to win changed to : ~p ~n", [Value]),
      %wxWindow:setFocus(Panel),
      {noreply, State};
    _ -> 
   	  %wxWindow:setFocus(Panel),
      {noreply, State}
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

draw_stats(Frame, TotalNodes, TotalBots, LiveBots, TotalWins, TotalLosts, AvgScoreWins, AvgMovesWins) ->
  wxFrame:setStatusText(Frame, "Live Nodes:  " ++ integer_to_list(TotalNodes)
                                ++ "        Total Bots:  " ++ integer_to_list(TotalBots)
                                ++ "        Live Games(Bots):  " ++ integer_to_list(LiveBots)
                                ++ "        Wins:  " ++ integer_to_list(TotalWins)
                                ++ "        Losses:  " ++ integer_to_list(TotalLosts)
                                ++ "        Average Win score:  " ++ float_to_list(AvgScoreWins,[{decimals,0}])
                                ++ "        Average moves in a win:  " ++ float_to_list(AvgMovesWins,[{decimals,0}])).

