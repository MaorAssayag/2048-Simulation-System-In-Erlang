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

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {ui_server_pid, totalBots, liveBots, totalWins, totalLosts, avgScoreWins,
                avgMovesWins, nodes, totalNodes, bot_threshold, bot_decisionID}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
  UI_server_PID = ui_server:start(), % start user interface server
  put(ui_server_PID,UI_server_PID),
  init_ets(),
  % update_all_ets(rand_2048,10,20),
  % io:format("~p~n",[ets:lookup(state,rand_2048)]),
  {ok, #state{ui_server_pid = UI_server_PID, totalBots=0, liveBots=0, totalWins=0,
   totalLosts=0, avgScoreWins=0.0, avgMovesWins=0.0, nodes=[], totalNodes=0, bot_threshold=2048, bot_decisionID=0}}.

init_ets() ->
  ets:new(state,[set,named_table,public]),
  foreach(fun(Key) ->insert_ets(Key) end,
  [rand_2048,rand_1024,rand_512,rand_256,rand_128,
  maxScore_2048,maxScore_1024,maxScore_512,maxScore_256,maxScore_128,
  keepAlive_2048,keepAlive_1024,keepAlive_512,keepAlive_256,keepAlive_128,
  maxMerged_2048,maxMerged_1024,maxMerged_512,maxMerged_256,maxMerged_128,
  heuristic_2048,heuristic_1024,heuristic_512,heuristic_256,heuristic_128,]).

insert_ets(Key) ->
  UI_server_PID = get(ui_server_PID),
  ets:insert(state, {Key, #state{ui_server_pid = UI_server_PID, totalBots=0, liveBots=0, totalWins=0,
   totalLosts=0, avgScoreWins=0.0, avgMovesWins=0.0, nodes=[], totalNodes=0, bot_threshold=2048, bot_decisionID=0}}).

get_key(DecisionID, Threshold) ->
  Decision = element(DecisionID+1, {"rand_", "maxScore_", "keepAlive_", "maxMerged_", "heuristic_"}),
  list_to_atom(Decision++integer_to_list(Threshold)).

update_all_ets(Key, PID, Node) ->
  [{_,State = #state{nodes=Nodes, totalNodes=TotalNodes}}] = ets:lookup(state,Key),
  ets:insert(state, {Key,State#state{nodes = Nodes ++ [{PID, Node, 0, 0}], totalNodes = TotalNodes+1}}).

update_all_ets(PID,Node) ->
  foreach(fun(Key) ->update_all_ets(Key, PID, Node) end,
  [rand_2048,rand_1024,rand_512,rand_256,rand_128,
  maxScore_2048,maxScore_1024,maxScore_512,maxScore_256,maxScore_128,
  keepAlive_2048,keepAlive_1024,keepAlive_512,keepAlive_256,keepAlive_128,
  maxMerged_2048,maxMerged_1024,maxMerged_512,maxMerged_256,maxMerged_128,
  heuristic_2048,heuristic_1024,heuristic_512,heuristic_256,heuristic_128,]).
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

% Connect a slave server to this main server
handle_call({connect,Node}, From, State = #state{nodes = Nodes,ui_server_pid = UI_server_PID,
            liveBots=LiveBots, totalWins=TotalWins,totalLosts=TotalLosts, avgScoreWins=AvgScoreWins,
            avgMovesWins=AvgMovesWins, totalNodes=TotalNodes, totalBots=TotalBots}) ->
  {PID, _} = From,
  io:format("main_server: receive connect request from node ~p ~n", [atom_to_list(Node)]),
  monitor_node(Node, true),
  wx_object:cast(UI_server_PID, {update_stats, TotalNodes+1, TotalBots, LiveBots, TotalWins, TotalLosts, AvgScoreWins, AvgMovesWins}),
  wx_object:cast(UI_server_PID, {update_node_status, Node, 1, 0, 0}),
  {reply, ok, State#state{nodes = Nodes ++ [{PID, Node, 0, 0}], totalNodes = TotalNodes+1}};

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

handle_cast({request_bots, Num}, State = #state{ui_server_pid= UI_server_PID, nodes = Nodes, totalNodes=TotalNodes, bot_decisionID=DecisionID, bot_threshold=WinThreshold}) ->
  io:format("main_server: request bots~n"),
  UpdatedNodes = distribute_bot_request(Nodes, TotalNodes, Num, DecisionID, WinThreshold, UI_server_PID),
  {noreply, State#state{nodes=UpdatedNodes}};

handle_cast({update_stats, Node, NewBots, NewWins, NewLosts, NewAvgScoreWins, NewAvgMovesWins},State = #state{
                              ui_server_pid = UI_server_PID, nodes=Nodes, liveBots=LiveBots,
                              totalWins=TotalWins,totalLosts=TotalLosts, avgScoreWins=AvgScoreWins,
                              avgMovesWins=AvgMovesWins, totalNodes=TotalNodes, totalBots=TotalBots}) ->
  io:format("main_server: got an update of stats~n"),
  % {totalNodes, totalBots, liveBots, totalWins, totalLosts, avgScoreWins, avgMovesWins}
  T = lists:keyfind(Node, 2, Nodes),
  {NodePID, _, SimulationsDone, SimulationsTotal} = T,
  NewNodeList = lists:delete(T, Nodes) ++ [{NodePID, Node, SimulationsDone+NewWins+NewLosts,SimulationsTotal}],
  case NewWins of
    0 ->
      wx_object:cast(UI_server_PID, {update_stats, TotalNodes, TotalBots+NewBots, LiveBots+NewBots-NewLosts, TotalWins, TotalLosts+NewLosts, AvgScoreWins, AvgMovesWins}),
      wx_object:cast(UI_server_PID, {update_node_status, Node, 1, SimulationsTotal, SimulationsDone+NewLosts}),
      {noreply, State#state{nodes=NewNodeList, totalBots=TotalBots+NewBots, liveBots=LiveBots+NewBots-NewWins-NewLosts,totalLosts=TotalLosts+NewLosts}};
    _ ->
      NewWinsTotal = TotalWins + NewWins,
      NewAvgScores = ((AvgScoreWins*TotalWins) + (NewWins*NewAvgScoreWins)) / NewWinsTotal,
      NewAvgMoves = ((AvgMovesWins*TotalWins) + (NewWins*NewAvgMovesWins)) / NewWinsTotal,
      wx_object:cast(UI_server_PID, {update_stats, TotalNodes, TotalBots+NewBots, LiveBots+NewBots-NewWins-NewLosts, NewWinsTotal, TotalLosts+NewLosts, NewAvgScores, NewAvgMoves}),
      wx_object:cast(UI_server_PID, {update_node_status, Node, 1, SimulationsTotal, SimulationsDone+NewWins+NewLosts}),
      {noreply, State#state{nodes=NewNodeList, totalBots=TotalBots+NewBots, liveBots=LiveBots+NewBots-NewWins-NewLosts, totalWins=NewWinsTotal,totalLosts=TotalLosts+NewLosts, avgScoreWins=NewAvgScores, avgMovesWins=NewAvgMoves}}
  end;

handle_cast({update_simulation_params, Param, Value}, State = #state{}) ->
  case Param of
    1 ->
      io:format("main_server: Bot Decision ID changed to : ~p ~n", [Value]),
      {noreply, State#state{bot_decisionID=Value}};
    2 ->
      io:format("main_server: Win threshold value changed to : ~p ~n", [Value]),
      {noreply, State#state{bot_threshold=Value}}
  end;

handle_cast(_Request, State) ->
  {noreply, State}.

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

% when node (slave_server) disconnect suddenly, send to other servers its roads
handle_info({nodedown,Node}, State = #state{ui_server_pid = UI_server_PID, nodes = Nodes,
             totalNodes = NumOfNodes, liveBots=LiveBots, totalWins=TotalWins,totalLosts=TotalLosts,
             avgScoreWins=AvgScoreWins, avgMovesWins=AvgMovesWins, totalBots=TotalBots,
             bot_decisionID=DecisionID, bot_threshold=WinThreshold}) ->

  {_, _, SimulationsDone, SimulationsTotal} = lists:keyfind(Node, 2, Nodes),
  NewNodes = lists:keydelete(Node, 2, Nodes),
  wx_object:cast(UI_server_PID, {update_stats, NumOfNodes-1, TotalBots, LiveBots, TotalWins, TotalLosts, AvgScoreWins, AvgMovesWins}),
  wx_object:cast(UI_server_PID, {update_node_status, Node, 0, SimulationsTotal, SimulationsDone}),
  io:format("Node down assign to  ~p ~n", [atom_to_list(Node)]),
  io:format("Distributing to exisiting simulation servers additional ~p games ~n", [integer_to_list(SimulationsTotal-SimulationsDone)]),
  UpdatedNodes = distribute_bot_request(NewNodes, NumOfNodes-1, SimulationsTotal-SimulationsDone, DecisionID, WinThreshold, UI_server_PID),
  %Stats = db_handler:get_gardeners_by_owner(ServerPid),
  %[{BackupServerPid,BackupServerNode}|T] = NewState#state.servers,
  %gen_server:cast(BackupServerPid,{backup,Gardeners,Flowers}),
  {noreply, State#state{nodes = UpdatedNodes, totalNodes = NumOfNodes-1}};

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
