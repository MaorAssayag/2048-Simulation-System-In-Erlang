%%%-------------------------------------------------------------------
%%% @author Maor Assayag, Refheal Shetrit
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2019 18:41
%%%-------------------------------------------------------------------
-module(backup_server).
-author("Maor Assayag, Refheal Shetrit").

-include("header.erl").

-behaviour(gen_server).

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(backup_state, {totalNodes, mainNode, nodes}).
-record(state, {ui_server_pid, ui_server_pid2, totalBots, liveBots, totalWins, totalLosts, avgScoreWins,
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
  {ok, State :: #backup_state{}} | {ok, State :: #backup_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([]) ->
  init_ets(),
  {ok, #backup_state{nodes=[], mainNode=[], totalNodes=0}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #backup_state{}) ->
  {reply, Reply :: term(), NewState :: #backup_state{}} |
  {reply, Reply :: term(), NewState :: #backup_state{}, timeout() | hibernate} |
  {noreply, NewState :: #backup_state{}} |
  {noreply, NewState :: #backup_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #backup_state{}} |
  {stop, Reason :: term(), NewState :: #backup_state{}}).

% Connect current main node to the backup server
handle_call({connect,Node}, _, State = #backup_state{nodes=Nodes}) ->
  io:format("backup_server: receive main node connect request from ~p ~n", [atom_to_list(Node)]),
  monitor_node(Node, true),
  gen_server:cast({main_server,Node}, {deploy_backup, ets:tab2list(state), Nodes}),
  {reply, node(), State#backup_state{mainNode=Node}};

handle_call(shutdown, _From, State) ->
  io:format("backup_server: shutdown"),
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
-spec(handle_cast(Request :: term(), State :: #backup_state{}) ->
  {noreply, NewState :: #backup_state{}} |
  {noreply, NewState :: #backup_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #backup_state{}}).

handle_cast({update_ets, TableName, KeyName, Value}, State) ->
  io:format("backup_server: update ets request of table: ~p and key: ~p ~n", [atom_to_list(TableName), atom_to_list(KeyName)]),
  ets:insert(TableName, {KeyName, Value}),
  {noreply, State};

handle_cast({new_simulation_node, PID, Node}, State = #backup_state{nodes=Nodes, totalNodes=TotalNodes}) ->
  io:format("backup_server: new simulation server recevied ~p ~n", [atom_to_list(Node)]),
  update_all_ets(PID,Node),
  {noreply, State#backup_state{nodes = Nodes ++ [{PID, Node}], totalNodes = TotalNodes+1}};

handle_cast({simulation_node_down,Node}, State = #backup_state{nodes=Nodes, totalNodes=TotalNodes}) ->
  io:format("backup_server: new simulation server recevied ~p ~n", [atom_to_list(Node)]),
  NewNodes = lists:keydelete(Node, 2, Nodes),
  update_all_ets_nodedown(Node),
  {noreply, State#backup_state{nodes = NewNodes, totalNodes = TotalNodes-1}};

handle_cast(_Request, State) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #backup_state{}) ->
  {noreply, NewState :: #backup_state{}} |
  {noreply, NewState :: #backup_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #backup_state{}}).

% when the main node disconnect
handle_info({nodedown,Node}, State = #backup_state{nodes=Nodes, totalNodes=TotalNodes}) ->
  io:format("backup_server: main server down ~p ~n", [atom_to_list(Node)]),
  case TotalNodes of
    0 ->
      io:format("backup_server: simulation servers not available~n"),
      {noreply, State};
    _ ->
      % get the first simulation server
      {_, NodeName} = lists:nth(1,Nodes),
      NewNodes = lists:keydelete(NodeName, 2, Nodes),
      io:format("backup_server: new live nodes ~p ~n",[NewNodes]),
      update_all_ets_nodedown(NodeName),
      % call this simulation server to become the main server (start main_srever and kill the simulation server)
      io:format("backup_server: requesting ~p to become main server~n",[atom_to_list(NodeName)]),
      NewMainPID = gen_server:call({simulation_server,NodeName}, {become_main, node()}),
      io:format("backup_server: recevied new main server pid ~p~n",[NewMainPID]),
      % removing the simulation server from the alive simulation servers table
      {noreply, State#backup_state{nodes = NewNodes, totalNodes = TotalNodes-1, mainNode=NodeName}}
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
    State :: #backup_state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #backup_state{},
    Extra :: term()) ->
  {ok, NewState :: #backup_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%%
%%% Internal functions
%%%
%%%===================================================================
init_ets() ->
  ets:new(state,[set,named_table,public]),
  lists:foreach(fun(Key) ->insert_ets(Key) end,
  [rand_2048,rand_1024,rand_512,rand_256,rand_128,
  maxScore_2048,maxScore_1024,maxScore_512,maxScore_256,maxScore_128,
  keepAlive_2048,keepAlive_1024,keepAlive_512,keepAlive_256,keepAlive_128,
  maxMerged_2048,maxMerged_1024,maxMerged_512,maxMerged_256,maxMerged_128,
  heuristic_2048,heuristic_1024,heuristic_512,heuristic_256,heuristic_128]).

insert_ets(Key) ->
  ets:insert(state, {Key, #state{ui_server_pid = [], ui_server_pid2 = [], totalBots=0, liveBots=0, totalWins=0,
   totalLosts=0, avgScoreWins=0.0, avgMovesWins=0.0, nodes=[], totalNodes=0,downNodes=[], bot_threshold=2048, bot_decisionID=0}}).

update_all_ets(Key, PID, Node) ->
  [{_,State = #state{nodes=Nodes, totalNodes=TotalNodes}}] = ets:lookup(state,Key),
  ets:insert(state, {Key,State#state{nodes = Nodes ++ [{PID, Node, 0, 0}], totalNodes = TotalNodes+1}}).

update_all_ets(PID,Node) ->
  lists:foreach(fun(Key) ->update_all_ets(Key, PID, Node) end,
  [rand_2048,rand_1024,rand_512,rand_256,rand_128,
  maxScore_2048,maxScore_1024,maxScore_512,maxScore_256,maxScore_128,
  keepAlive_2048,keepAlive_1024,keepAlive_512,keepAlive_256,keepAlive_128,
  maxMerged_2048,maxMerged_1024,maxMerged_512,maxMerged_256,maxMerged_128,
  heuristic_2048,heuristic_1024,heuristic_512,heuristic_256,heuristic_128]).

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
