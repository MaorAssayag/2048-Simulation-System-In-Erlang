%%%-------------------------------------------------------------------
%%% @author Maor Assayag, Refheal Shetrit
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2019 18:41
%%%-------------------------------------------------------------------
-module(main_server).
-author("Maor Assayag, Refheal Shetrit").

-include("header.erl").

-behaviour(gen_server).

-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {ui_server_pid, totalBots, liveBots, totalWins, totalLosts, avgScoreWins,
                avgMovesWins, nodes, totalNodes, bot_threshold, bot_decisionID, downNodes,
                boardProcesses, score}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(BackupNode) ->
  {ok, Pid :: pid()} |  ignore | {error, Reason :: term()} when BackupNode::node()).
start(BackupNode) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [BackupNode], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([BackupNode]) ->
  put(server,{backup_server,BackupNode}),
   % start user interface server
  UI_server_PID = ui_server:start(),
  put(ui_server_PID,UI_server_PID),
  % start main game master process
  register(master_game_process, spawn(fun() -> receiveMsgMaster(-1,{},[]) end)),
  % init main game master process (which communicate with 4 TOP processes)
  master_game_process ! init,
  % init ets data base by category
  init_ets(),
  % gen_server:call(get(server),{connect,node()}),
  % wx_object:cast(UI_server_PID, {update_main_backup_nodes, node(), BackupNode}),
  try
    % connect to the backup server
    monitor_node(gen_server:call(get(server),{connect,node()}),true),
    wx_object:cast(UI_server_PID, {update_main_backup_nodes, node(), BackupNode})
    % ui_server:cast ui green light on backup
  catch
    error :_ -> io:format("main_server: backup_server has not responed~n"),
                 wx_object:cast(UI_server_PID, {update_main_backup_nodes, node(), []});
    exit :_ ->  io:format("main_server: backup_server has not responed~n"),
                 wx_object:cast(UI_server_PID, {update_main_backup_nodes, node(), []})
  end,
  {ok, #state{ui_server_pid = UI_server_PID, totalBots=0, liveBots=0, totalWins=0,
   totalLosts=0, avgScoreWins=0.0, avgMovesWins=0.0, nodes=[], downNodes=[], totalNodes=0, bot_threshold=2048, bot_decisionID=0,
   boardProcesses={}, score=0}}.

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

%% handle_call({connect,Node})
%% Connect a slave(simulation) server to this main server
handle_call({connect,Node}, From, State = #state{nodes = Nodes,ui_server_pid = UI_server_PID,
            liveBots=LiveBots, totalWins=TotalWins,totalLosts=TotalLosts, avgScoreWins=AvgScoreWins,
            avgMovesWins=AvgMovesWins, totalNodes=TotalNodes, totalBots=TotalBots}) ->
  {PID, _} = From,
  io:format("main_server: receive connect request from node ~p ~n", [atom_to_list(Node)]),
  monitor_node(Node, true),
  % update the ui with the new simulation server informations
  wx_object:cast(UI_server_PID, {update_stats, TotalNodes+1, TotalBots, LiveBots, TotalWins, TotalLosts, AvgScoreWins, AvgMovesWins}),
  wx_object:cast(UI_server_PID, {update_node_status, Node, 1, 0, 0}),
  % update all ets database about a new simulation server
  update_all_ets(PID,Node),
  % Notify the backup server about a new simulation server
  gen_server:cast(get(server), {new_simulation_node, PID, Node}),
  {reply, ok, State#state{nodes = Nodes ++ [{PID, Node, 0, 0}], totalNodes = TotalNodes+1}};

%% handle_call(shutdown)
handle_call(shutdown, _From, State) ->
  io:format("main_server: shutdown"),
  {stop,shutdown, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

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

%% handle_cast({request_bots, Num})
%% Request to distrebute game bots from the UI server to the simulation server
handle_cast({request_bots, Num}, State = #state{ui_server_pid= UI_server_PID, nodes = Nodes, totalNodes=TotalNodes, bot_decisionID=DecisionID, bot_threshold=WinThreshold}) ->
  io:format("main_server: request bots~n"),
  UpdatedNodes = distribute_bot_request(Nodes, TotalNodes, Num, DecisionID, WinThreshold, UI_server_PID),
  {noreply, State#state{nodes=UpdatedNodes}};


%% handle_cast({update_stats, Node, NewBots, NewWins, NewLosts, NewAvgScoreWins, NewAvgMovesWins})
%% An update from one of the simulation server - the current stats of its simulations, which need to
%% be updated in the UI
handle_cast({update_stats, Node, NewBots, NewWins, NewLosts, NewAvgScoreWins, NewAvgMovesWins},State = #state{
                              ui_server_pid = UI_server_PID, nodes=Nodes, liveBots=LiveBots,
                              totalWins=TotalWins,totalLosts=TotalLosts, avgScoreWins=AvgScoreWins,
                              avgMovesWins=AvgMovesWins, totalNodes=TotalNodes, totalBots=TotalBots}) ->
  io:format("main_server: got an update of stats~n"),
  % {totalNodes, totalBots, liveBots, totalWins, totalLosts, avgScoreWins, avgMovesWins}
  % update the current list of nodes with this new information (# of games finished etc)
  T = lists:keyfind(Node, 2, Nodes),
  {NodePID, _, SimulationsDone, SimulationsTotal} = T,
  NewNodeList = lists:delete(T, Nodes) ++ [{NodePID, Node, SimulationsDone+NewWins+NewLosts,SimulationsTotal}],
  case NewWins of
    % then we dont neet to calculate NewAvgScores or NewAvgMoves
    0 ->
      % update the ui with the new stats
      wx_object:cast(UI_server_PID, {update_stats, TotalNodes, TotalBots+NewBots, LiveBots+NewBots-NewLosts, TotalWins, TotalLosts+NewLosts, AvgScoreWins, AvgMovesWins}),
      wx_object:cast(UI_server_PID, {update_node_status, Node, 1, SimulationsTotal, SimulationsDone+NewLosts}),
      {noreply, State#state{nodes=NewNodeList, totalBots=TotalBots+NewBots, liveBots=LiveBots+NewBots-NewWins-NewLosts,totalLosts=TotalLosts+NewLosts}};
    _ ->
      NewWinsTotal = TotalWins + NewWins,
      NewAvgScores = ((AvgScoreWins*TotalWins) + (NewWins*NewAvgScoreWins)) / NewWinsTotal,
      NewAvgMoves = ((AvgMovesWins*TotalWins) + (NewWins*NewAvgMovesWins)) / NewWinsTotal,
      % update the ui with the new stats
      wx_object:cast(UI_server_PID, {update_stats, TotalNodes, TotalBots+NewBots, LiveBots+NewBots-NewWins-NewLosts, NewWinsTotal, TotalLosts+NewLosts, NewAvgScores, NewAvgMoves}),
      wx_object:cast(UI_server_PID, {update_node_status, Node, 1, SimulationsTotal, SimulationsDone+NewWins+NewLosts}),
      {noreply, State#state{nodes=NewNodeList, totalBots=TotalBots+NewBots, liveBots=LiveBots+NewBots-NewWins-NewLosts, totalWins=NewWinsTotal,totalLosts=TotalLosts+NewLosts, avgScoreWins=NewAvgScores, avgMovesWins=NewAvgMoves}}
  end;


%% handle_cast({update_simulation_params, Param, Value})
%% An update from the UI about the current simulation parameters choosen by the user
%% We are maintining a record (in a ets) for each paramer combintion (statistic about prev simulation etc)
handle_cast({update_simulation_params, Param, Value}, State = #state{bot_decisionID=DecisionID, bot_threshold=Threshold, score=Score, boardProcesses=BoardProcesses}) ->
  case Param of
    1 ->
      io:format("main_server: Bot Decision ID changed to : ~p ~n", [Value]),
      % save the current state by category
      ets:insert(state, {get_key(DecisionID,Threshold), State}),
      % backup ets on the backupserver if exists
      gen_server:cast(get(server),{update_ets, state, get_key(DecisionID,Threshold), State}),
      % load the new state by the new category
      [{_,NewState}] = ets:lookup(state,get_key(Value,Threshold)),
      update_ui_stats(NewState),
      % return the new state
      {noreply, NewState#state{bot_decisionID=Value, bot_threshold=Threshold, boardProcesses=BoardProcesses, score=Score}};
    2 ->
      io:format("main_server: Win threshold value changed to : ~p ~n", [Value]),
      % save the current state by category
      ets:insert(state, {get_key(DecisionID,Threshold), State}),
      % backup ets on the backupserver if exists
      gen_server:cast(get(server),{update_ets, state, get_key(DecisionID,Threshold), State}),
      % load the new state by the new category
      [{_,NewState}] = ets:lookup(state,get_key(DecisionID,Value)),
      update_ui_stats(NewState),
      % return the new state
      {noreply, NewState#state{bot_decisionID=DecisionID, bot_threshold=Value, score=Score, boardProcesses=BoardProcesses}}
  end;


%% handle_cast({update_user_game, NewBoard, NewAddition2Score, GameOver, Won})
%% An update of the live user game state from the 'Master game process'
handle_cast({update_user_game, NewBoard, NewAddition2Score, GameOver, Won}, State = #state{ui_server_pid=UI_server_PID, score=OldScore, boardProcesses=BoardProcesses}) ->
  % update the game on the UI
  wx_object:cast(UI_server_PID, {update_game, NewBoard, NewAddition2Score+OldScore, GameOver, Won}),
  % adjust processes accordingly to new/removed tiles (create, update or kill processes)
  NewBoardProcesses = adjust_processes(tuple_to_list(NewBoard), BoardProcesses),
  io:format("handle cast update_user_game new process board : ~p ~n", [tuple_to_list(NewBoardProcesses)]),
  {noreply, State#state{score=OldScore+NewAddition2Score, boardProcesses=NewBoardProcesses}};


%% handle_cast({game_move, Operation})
%% Message from the ui server, the user press one of the arrows key (new move)
handle_cast({game_move, Operation}, State = #state{boardProcesses=BoardProcesses}) ->
  io:format("main server received move request of operation: ~p ~n", [atom_to_list(Operation)]),
  % notify the master game process about the new move
  master_game_process ! {get_board, {BoardProcesses, Operation}},
  {noreply, State};


%% handle_cast({reset_game})
%% Message from the ui server, the user press SPACEBAR which reset the live game
handle_cast({reset_game}, State = #state{ui_server_pid=UI_server_PID, boardProcesses=BoardProcesses}) ->
  % kill current slave processes
  CleanBoardProcesses = kill_all_processes(tuple_to_list(BoardProcesses)),
  % get new tiles for the new game
  NewBoard = new_game_tile(new_game_tile(get_default_board(),true,false),true,false),
  % create 2 procceses accordingly
  NewBoardProcesses = init_processes(tuple_to_list(NewBoard), CleanBoardProcesses),
  io:format("game reset, new board processes: ~p~n", [tuple_to_list(NewBoardProcesses)]),
  master_game_process ! {"reset"},
  % notify the ui to start a new game
  % cast:{update_game, Board, Score, GameOver, Won}
  wx_object:cast(UI_server_PID, {update_game, NewBoard, 0, false, true}),
  {noreply, State#state{boardProcesses=NewBoardProcesses, score=0}};


%% handle_cast({deploy_backup, EtsTableList, Nodes})
%% Message from the backup server(if up and runing), deploy the ets backup
%% this message will be sent after the previous main node exit (Fall protection)
handle_cast({deploy_backup, EtsTableList, Nodes}, #state{ui_server_pid=UI_server_PID, score=Score, boardProcesses=BoardProcesses}) ->
  io:format("main_server: got deploy_backup from the backup server~n"),
  % Inital the ets data base to the backup
  lists:foreach(fun({Key, Value}) -> ets:insert(state, {Key, Value}) end,
                EtsTableList),
  % Monitor live simulation servers & update them about this new Main Node
  io:format("main_server: deploy with live Nodes ~p~n", [Nodes]),
  lists:foreach(fun({NodePID, CurrNode}) -> monitor_node(CurrNode,true),
                                      gen_server:cast(NodePID, {update_main_node, node()})
                                    end,
                Nodes),
  % Update all states with the Ui server Pid
  update_all_ets_key_value(UI_server_PID),
  % Get the backup state of the default parameters
  [{_,NewState}] = ets:lookup(state,get_key(0,2048)),
  io:format("main_server deploy with NewState ~p~n", [tuple_to_list(NewState)]),
  update_ui_stats(NewState),
  {noreply, NewState#state{bot_decisionID=0, bot_threshold=2048, score=Score, boardProcesses=BoardProcesses}};

handle_cast(_Request, State) ->
  {noreply, State}.

%% update_ui_stats(#state)
%% Update the UI with a new stats (displayed in the lower status bar & the node List)
update_ui_stats(#state{ui_server_pid = UI_server_PID, nodes=Nodes, liveBots=LiveBots,
                              totalWins=TotalWins,totalLosts=TotalLosts, avgScoreWins=AvgScoreWins,
                              avgMovesWins=AvgMovesWins, totalNodes=TotalNodes, totalBots=TotalBots, downNodes=DownNodes}) ->
    % update live nodes status
    lists:foreach(fun({_, Node, SimulationsDone, SimulationsTotal}) ->
              wx_object:cast(UI_server_PID, {update_node_status, Node, 1, SimulationsTotal, SimulationsDone}) end,
            Nodes),
    % update down nodes status
    lists:foreach(fun({_, Node, SimulationsDone, SimulationsTotal}) ->
              wx_object:cast(UI_server_PID, {update_node_status, Node, 0, SimulationsTotal, SimulationsDone}) end,
            DownNodes),
    % update status bar
    wx_object:cast(UI_server_PID, {update_stats, TotalNodes, TotalBots, LiveBots, TotalWins, TotalLosts, AvgScoreWins, AvgMovesWins}).

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

%% handle_info({nodedown,Node})
%% When node (simulation_server) suddenly disconnect, distrebute the remaining simulations if exists
handle_info({nodedown,Node}, State = #state{ui_server_pid = UI_server_PID, nodes = Nodes,
             totalNodes = NumOfNodes, liveBots=LiveBots, totalWins=TotalWins,totalLosts=TotalLosts,
             avgScoreWins=AvgScoreWins, avgMovesWins=AvgMovesWins, totalBots=TotalBots,
             bot_decisionID=DecisionID, bot_threshold=WinThreshold, downNodes=DownNodes}) ->
  {_,BackupNode} = get(server),
  case Node of
    BackupNode ->
      io:format("Backup Node down assign to  ~p ~n", [atom_to_list(Node)]),
      wx_object:cast(UI_server_PID, {update_main_backup_nodes, node(), []}),
      {noreply, State};
    _ ->
    {NodePID, NodeName, SimulationsDone, SimulationsTotal} = lists:keyfind(Node, 2, Nodes),
    NewNodes = lists:keydelete(Node, 2, Nodes),
    % update the ui node table
    wx_object:cast(UI_server_PID, {update_stats, NumOfNodes-1, TotalBots, LiveBots, TotalWins, TotalLosts, AvgScoreWins, AvgMovesWins}),
    wx_object:cast(UI_server_PID, {update_node_status, Node, 0, SimulationsTotal, SimulationsDone}),
    io:format("Node down assign to  ~p ~n", [atom_to_list(Node)]),
    io:format("Distributing to exisiting simulation servers additional ~p games ~n", [integer_to_list(SimulationsTotal-SimulationsDone)]),
    % distrebute remaining simulation to the other servers
    UpdatedNodes = distribute_bot_request(NewNodes, NumOfNodes-1, SimulationsTotal-SimulationsDone, DecisionID, WinThreshold, UI_server_PID),
    % notify the backup server database
    gen_server:cast(get(server),{simulation_node_down,Node}),
    % update local ets database
    update_all_ets_nodedown(Node),
    {noreply, State#state{nodes = UpdatedNodes, totalNodes = NumOfNodes-1, downNodes=DownNodes++[{NodePID, NodeName, SimulationsDone, SimulationsTotal}]}}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

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
get_default_board() ->
  {{{1,1},0}, {{1,2},0}, {{1,3},0}, {{1,4},0}, {{2,1},0}, {{2,2},0},
  {{2,3},0}, {{2,4},0}, {{3,1},0}, {{3,2},0}, {{3,3},0}, {{3,4},0}, {{4,1},0},
  {{4,2},0}, {{4,3},0}, {{4,4},0}}.

%% Distribute the simulation request across live nodes
% if NumOfBots requested = 0
distribute_bot_request(Nodes,_,0,_,_,_) -> Nodes;
% if TotalNodes alive = 0
distribute_bot_request(Nodes,0,_,_,_,_) -> Nodes;
% Recursive sending finished
distribute_bot_request([],_,_,_,_,_) -> [];
distribute_bot_request([{PID, Node, SimulationsDone, SimulationsTotal}|T], TotalNodes, NumOfBots, DecisionID, WinThreshold, UI_server_PID) ->
  CurrentShare = round(NumOfBots/TotalNodes),
  gen_server:cast(PID, {request_bots, CurrentShare, DecisionID, WinThreshold}),
  wx_object:cast(UI_server_PID, {update_node_status, Node, 1, SimulationsTotal+CurrentShare, SimulationsDone}),
  [{PID, Node, SimulationsDone, SimulationsTotal+CurrentShare}] ++ distribute_bot_request(T, TotalNodes-1, NumOfBots-CurrentShare, DecisionID, WinThreshold,UI_server_PID).

%% init_ets()
%% Initializes the ETS data base
init_ets() ->
  ets:new(state,[set,named_table,public]),
  lists:foreach(fun(Key) ->insert_ets(Key) end,
  [rand_2048,rand_1024,rand_512,rand_256,rand_128,
  maxScore_2048,maxScore_1024,maxScore_512,maxScore_256,maxScore_128,
  keepAlive_2048,keepAlive_1024,keepAlive_512,keepAlive_256,keepAlive_128,
  maxMerged_2048,maxMerged_1024,maxMerged_512,maxMerged_256,maxMerged_128,
  heuristic_2048,heuristic_1024,heuristic_512,heuristic_256,heuristic_128]).


%% insert_ets(Key)
%% update the UI PID server in the current state
insert_ets(Key) ->
  UI_server_PID = get(ui_server_PID),
  ets:insert(state, {Key, #state{ui_server_pid = UI_server_PID, totalBots=0, liveBots=0, totalWins=0,
   totalLosts=0, avgScoreWins=0.0, avgMovesWins=0.0, nodes=[], totalNodes=0,downNodes=[], bot_threshold=2048, bot_decisionID=0}}).


%% get_key(DecisionID, Threshold)
%% Get the atom associated with (DecisionID, Threshold)
get_key(DecisionID, Threshold) ->
  Decision = element(DecisionID+1, {"rand_", "maxScore_", "keepAlive_", "maxMerged_", "heuristic_"}),
  list_to_atom(Decision++integer_to_list(Threshold)).


%% update_all_ets(Key, PID, Node)
%% Update all ets records with a new node (simulation server)
update_all_ets(Key, PID, Node) ->
  [{_,State = #state{nodes=Nodes, totalNodes=TotalNodes}}] = ets:lookup(state,Key),
  ets:insert(state, {Key,State#state{nodes = Nodes ++ [{PID, Node, 0, 0}], totalNodes = TotalNodes+1}}).


%% update_all_ets(PID,Node)
%% Update all ets records with a new node (simulation server)
update_all_ets(PID,Node) ->
  lists:foreach(fun(Key) ->update_all_ets(Key, PID, Node) end,
  [rand_2048,rand_1024,rand_512,rand_256,rand_128,
  maxScore_2048,maxScore_1024,maxScore_512,maxScore_256,maxScore_128,
  keepAlive_2048,keepAlive_1024,keepAlive_512,keepAlive_256,keepAlive_128,
  maxMerged_2048,maxMerged_1024,maxMerged_512,maxMerged_256,maxMerged_128,
  heuristic_2048,heuristic_1024,heuristic_512,heuristic_256,heuristic_128]).


%% update_all_ets_nodedown(Key, Node)
%% Update all ets records when a node (simulation server) exit
update_all_ets_nodedown(Key, Node) ->
  [{_,State = #state{nodes=Nodes, totalNodes=TotalNodes, downNodes=DownNodes}}] = ets:lookup(state,Key),
  NodeDown = lists:keyfind(Node,2,Nodes),
  ets:insert(state, {Key,State#state{nodes = lists:keydelete(Node, 2, Nodes), totalNodes = TotalNodes-1, downNodes=DownNodes++[NodeDown]}}).

update_all_ets_nodedown(Node) ->
  lists:foreach(fun(Key) -> update_all_ets_nodedown(Key,Node) end,
  [rand_2048,rand_1024,rand_512,rand_256,rand_128,
  maxScore_2048,maxScore_1024,maxScore_512,maxScore_256,maxScore_128,
  keepAlive_2048,keepAlive_1024,keepAlive_512,keepAlive_256,keepAlive_128,
  maxMerged_2048,maxMerged_1024,maxMerged_512,maxMerged_256,maxMerged_128,
  heuristic_2048,heuristic_1024,heuristic_512,heuristic_256,heuristic_128]).

% KeyValue = atom, e.g. {ui_server_pid, <0.14.0>} will update ui_server_pid in all states in the ETS
update_all_ets_key_value(Value) ->
  lists:foreach(fun(Key) ->update_all_ets_key_value(Key,Value) end,
  [rand_2048,rand_1024,rand_512,rand_256,rand_128,
  maxScore_2048,maxScore_1024,maxScore_512,maxScore_256,maxScore_128,
  keepAlive_2048,keepAlive_1024,keepAlive_512,keepAlive_256,keepAlive_128,
  maxMerged_2048,maxMerged_1024,maxMerged_512,maxMerged_256,maxMerged_128,
  heuristic_2048,heuristic_1024,heuristic_512,heuristic_256,heuristic_128]).
update_all_ets_key_value(Key, Value) ->
  [{_,State}] = ets:lookup(state,Key),
  ets:insert(state, {Key, State#state{ui_server_pid=Value}}).

%% is_game_over(BoardList)
%% Check if a current state of a 2048 is game over (win/lose) or not
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


%% push_elemnt_right(List)
%% return a list without spaces between items to the right
push_elemnt_right(List) ->
  Ans = [Res || Res <- List, Res > 0],
  padding_zeroes_right(Ans).

%% padding_zeroes_right(List)
%% return a list with the length of 4 (padding zero to the right)
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

%% push_elemnt_right(List)
%% return a list without spaces between items to the right
push_elemnt_left(List) ->
  Ans = [Res || Res <- List, Res > 0],
  padding_zeroes_left(Ans).

%% padding_zeroes_left(List)
%% return a list with the length of 4 (padding zero to the left)
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

%% down_operation(Board)
%% Apply down operation on the current state of the 2048 game
%% This function that can be called from handle_event #key_down
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

%% up_operation(Board)
%% Apply up operation on the current state of the 2048 game
%% This function that can be called from handle_event #key_up
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

%% right_operation(Board)
%% Apply up operation on the current state of the 2048 game
%% This function that can be called from handle_event #key_right
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

%% left_operation(Board)
%% Apply up operation on the current state of the 2048 game
%% This function that can be called from handle_event #key_left
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

%% new_game_tile(Board, GameOver, isTilesAvailable)
%% create a new tile on the grid with the value 2 or 4, used in each new step
%% return the new board
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

%%%===================================================================
%%%
%%% Game distrbution functions
%%% this function used to maintain the ui game
%%%
%%%===================================================================

%% receiveMsgMaster(MsgLeft, Board, Operation)
%% Main loop for the master game process
receiveMsgMaster(0, Board, Operation)->
  %io:format("top master: board collected ~p~n", [tuple_to_list(Board)]),
  % Master process finished collecting the current board data from all the slave processes
  io:format("Top master : distrbuted board has been received ~p ~n", [tuple_to_list(Board)]),
  % apply the user operation to the board
  {BoardAfterOperation, NewAddition2Score} = apply_operation(Operation, Board),
  % check if the game is over
  {GameOver, HasFreeTiles, Won} = is_game_over(tuple_to_list(BoardAfterOperation)),
  % create a new tile
  NewBoard = new_game_tile(BoardAfterOperation, HasFreeTiles,BoardAfterOperation=:=Board),
  % update the main server with the new game state
  gen_server:cast(main_server,{update_user_game, NewBoard, NewAddition2Score, GameOver, Won}),
  % keep looping
  receiveMsgMaster(-1,[],[]);

receiveMsgMaster(MsgLeft, Board, Operation)->
  receive
    {"reset"} ->
      receiveMsgMaster(-1,[],[]);

    % get the 2048 board state from all the slave processes
    {get_board, {BoardProcesses, NewOperation}} ->
      case MsgLeft of
        (-1) ->
          io:format("top master: get_board request ~p ~p~n", [atom_to_list(NewOperation), tuple_to_list(BoardProcesses)]),
          % request all 4 top processes to gain the board data from the slave processes
          top1 ! {request_data, [element(1,BoardProcesses), element(2,BoardProcesses), element(3,BoardProcesses), element(4,BoardProcesses)]},
          top2 ! {request_data, [element(5,BoardProcesses), element(6,BoardProcesses), element(7,BoardProcesses), element(8,BoardProcesses)]},
          top3 ! {request_data, [element(9,BoardProcesses), element(10,BoardProcesses), element(11,BoardProcesses), element(12,BoardProcesses)]},
          top4 ! {request_data, [element(13,BoardProcesses), element(14,BoardProcesses), element(15,BoardProcesses), element(16,BoardProcesses)]},
          % wait for 4 responses
          receiveMsgMaster(4, get_default_board(), NewOperation);
        _ ->
          % we still in operation, ignore new requests
          receiveMsgMaster(MsgLeft, Board, Operation)
        end;

    % message from top process 1, with the information about the first row of the board
    {top1, {T1,T2,T3,T4}} ->
      io:format("top master: receivied from top1 ~p~n", [tuple_to_list({T1,T2,T3,T4})]),
      NewBoard = setelement(4,
                  setelement(3,
                    setelement(2,
                      setelement(1,Board,T1),
                    T2),
                  T3),
                T4),
      receiveMsgMaster(MsgLeft-1, NewBoard, Operation);

    % message from top process 2, with the information about the second row of the board
    {top2, {T1,T2,T3,T4}} ->
      io:format("top master: receivied from top2 ~p~n", [tuple_to_list({T1,T2,T3,T4})]),
      NewBoard = setelement(8,
                  setelement(7,
                    setelement(6,
                      setelement(5,Board,T1),
                    T2),
                  T3),
                T4),
      receiveMsgMaster(MsgLeft-1, NewBoard,Operation);

    % message from top process 3, with the information about the third row of the board
    {top3, {T1,T2,T3,T4}} ->
      io:format("top master: receivied from top3 ~p~n", [tuple_to_list({T1,T2,T3,T4})]),
      NewBoard = setelement(12,
                  setelement(11,
                    setelement(10,
                      setelement(9,Board,T1),
                    T2),
                  T3),
                T4),
      receiveMsgMaster(MsgLeft-1, NewBoard,Operation);

    % message from top process 4, with the information about the fourth row of the board
    {top4, {T1,T2,T3,T4}} ->
      io:format("top master: receivied from top4 ~p~n", [tuple_to_list({T1,T2,T3,T4})]),
      NewBoard = setelement(16,
                  setelement(15,
                    setelement(14,
                      setelement(13,Board,T1),
                    T2),
                  T3),
                T4),
      receiveMsgMaster(MsgLeft-1, NewBoard,Operation);

    % Initialize 4 top processes
    init ->
      %receiveMsgTop(MasterPID, MsgLeft)
      register(top1, spawn(fun() -> receiveMsgTop(top1, [], -1) end)),
      register(top2, spawn(fun() -> receiveMsgTop(top2, [], -1) end)),
      register(top3, spawn(fun() -> receiveMsgTop(top3, [], -1) end)),
      register(top4, spawn(fun() -> receiveMsgTop(top4, [], -1) end)),
      receiveMsgMaster(MsgLeft,Board,Operation)
  end.


%% receiveMsgTop(ID, CurrAns, MsgLeft)
%% Main loop for the top game processes
receiveMsgTop(ID, Ans, 0) ->
  master_game_process ! {ID, Ans},
  receiveMsgTop(ID, [], -1);
receiveMsgTop(ID, CurrAns, MsgLeft) ->
  receive
    % message from one of the slave processes (a tile), get its value
    {answer,{{X,Y},Value}} ->
        receiveMsgTop(ID, setelement(Y,CurrAns,{{X,Y},Value}), MsgLeft-1);

    % message from the master game process with up to 4 tile processes, requesting to get thier values
    {request_data, ProcessList} ->
        io:format("Top ~p received request data with ~p~n", [atom_to_list(ID), ProcessList]),
        % for each process : if exists, send request to get the data, if not: set the position with value 0
        {CurrentAns , AnsLeft} = lists:foldr(fun({{X,Y}, PID}, {Curr, Num}) ->
                                          case PID of
                                            0 -> {setelement(Y,Curr,{{X,Y},0}), Num};
                                            _ -> PID ! {get_tile, self()},
                                                 {Curr,Num+1}
                                          end
                                        end,
                                  {{{},{},{},{}}, 0}, ProcessList),
        %io:format("Top ~p current ans ~p~n", [atom_to_list(ID), tuple_to_list(CurrentAns)]),
        %Ans = wait_for_slave(CurrentAns, MsgLeft),
        %io:format("Top ~p ans ~p~n", [atom_to_list(ID), tuple_to_list(Ans)]),
        %master_game_process ! {ID,Ans},
        receiveMsgTop(ID, CurrentAns, AnsLeft)
  end.

%% This method used by a top process
%% wait for the slaves proceses  to get the value they possess
% wait_for_slave(CurrentAns,0) -> CurrentAns;
% wait_for_slave(CurrentAns, MsgLeft) ->
%   receive
%     {answer,{{X,Y},Value}} ->
%         wait_for_slave(setelement(Y,CurrentAns,{{X,Y},Value}), MsgLeft-1);
%     {reset} -> wait_for_slave([],0)
%   end.


%% receiveMsgTop(ID, CurrAns, MsgLeft)
%% Main loop for the slave processes (tiles)
receiveMsgSlave({Pos,Value}) ->
  receive
    % send TopPID the value of this {Pos} tile
    {get_tile, TopPID} ->
                          TopPID ! {answer,{Pos,Value}},
                          receiveMsgSlave({Pos,Value});

    % update the current Value of the tile
    {update_tile, NewValue} ->
                          receiveMsgSlave({Pos,NewValue})
  end.

%% get_default_board_processes()
%% get the default grid of processes
get_default_board_processes() ->
  {{{1,1},0}, {{1,2},0}, {{1,3},0}, {{1,4},0}, {{2,1},0}, {{2,2},0},
  {{2,3},0}, {{2,4},0}, {{3,1},0}, {{3,2},0}, {{3,3},0}, {{3,4},0}, {{4,1},0},
  {{4,2},0}, {{4,3},0}, {{4,4},0}}.

kill_all_processes([{}]) ->
  get_default_board_processes();
%% Kill all non 0 PIDs in BoardProcessesList, returns a clean board processes tuple
kill_all_processes(BoardProcessesList) ->
  lists:foreach(fun({_, PID}) ->
                  case PID of
                    0 -> ok;
                    _ -> exit(PID,kill)
                  end
                end,
          BoardProcessesList),
  get_default_board_processes().

init_processes(BoardList, BoardProcesses) ->
  lists:foldr(fun({{X,Y}, Value}, CurrBoardProcesses) ->
                  case Value of
                    0 -> CurrBoardProcesses;
                    _ -> PID = spawn(fun() -> receiveMsgSlave({{X,Y},Value}) end),
                        % add the new process to the CurrBoardProcesses
                        setelement((X-1)*4 + Y, CurrBoardProcesses, {{X,Y},PID})
                  end
              end,
          BoardProcesses, BoardList).

adjust_processes(BoardList, BoardProcesses) ->
  lists:foldr(fun({{X,Y}, Value}, CurrBoardProcesses) ->
                  case Value of
                    % check if this tile have a process, if so kill it
                    0 ->
                      case element((X-1)*4 + Y, CurrBoardProcesses) of
                        {_,0} -> CurrBoardProcesses;
                        % the new value is 0 so we need to kill that process
                        {_,CurrPID} -> exit(CurrPID,exit),
                                        % clear the spot in the BoardProcesses
                                       setelement((X-1)*4 + Y, CurrBoardProcesses, {{X,Y},0})
                        end;
                    % otherwise create a new process for this tile or update its value
                    _ ->
                      case element((X-1)*4 + Y, CurrBoardProcesses) of
                        {_,0} ->
                          PID = spawn(fun() -> receiveMsgSlave({{X,Y},Value}) end),
                          % add the new process to the CurrBoardProcesses
                          setelement((X-1)*4 + Y, CurrBoardProcesses, {{X,Y},PID});
                        {_,CurrPID} ->
                          % update the value - slave process
                          CurrPID ! {update_tile, Value},
                          CurrBoardProcesses
                        end
                    end
                end,
          BoardProcesses, BoardList).

apply_operation(down, Board) ->
  down_operation(Board);
apply_operation(up, Board) ->
  up_operation(Board);
apply_operation(right, Board) ->
  right_operation(Board);
apply_operation(left, Board) ->
  left_operation(Board).
