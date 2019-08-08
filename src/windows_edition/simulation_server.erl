%%%-------------------------------------------------------------------
%%% @author Maor Assayag, Refheal Shetrit
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jul 2019 14:20
%%%-------------------------------------------------------------------
-module(simulation_server).
-author("Maor Assayag, Refheal Shetrit").

-include("header.erl").

-behaviour(gen_server).

-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {main_server_pid, totalBots, totalWins, totalLosts}).
-record(bot_game,{board, game_over, freeTiles, score, won}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(MainNode) ->
  {ok, Pid :: pid()} |  ignore | {error, Reason :: term()} when MainNode::node()).
start(MainNode) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [MainNode], []).


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
init([MainNode]) ->
  put(server,{main_server,MainNode}),
  ets:new(stats,[set,named_table,public]),
  ets:insert(stats, {threshold, 2048}),
  ets:insert(stats, {wins, 0}),
  ets:insert(stats, {losts, 0}),
  ets:insert(stats, {score_wins, 0}),
  ets:insert(stats, {moves_wins, 0}),
  ets:insert(stats, {simulations_left, 0}),
  ets:insert(stats, {update_stats, false}),
  spawn_link(fun() -> update_main_server_timer({simulation_server,node()}) end),
  gen_server:call(get(server),{connect,node()}),
  {ok, #state{main_server_pid = MainNode, totalBots=0, totalWins=0, totalLosts=0}}.

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

handle_call({become_main, BackupNode}, _From, State) ->
  io:format("simulation_server ~p: recevied request to become main server ~n", [atom_to_list(node())]),
  NewMainPID = spawn(main_server,start,[BackupNode]),
  gen_server:cast({simulation_server,node()}, {kill_simulation_server}),
  {reply, NewMainPID, State};

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

%{update_from_bot, Won, Score}
% handle_cast({update_from_bot, Won, Score, Moves}, State) ->
%   %io:format("simulation_server: update_from_bot~n"),
%   %io:format("GAME OVER with score:~p moves:~p ~n", [integer_to_list(Score),integer_to_list(Moves)]),
%   ets:update_counter(stats,simulations_left,-1),
%   case Won of
%     true ->
%       ets:update_counter(stats, wins, 1),
%       ets:update_counter(stats, score_wins, Score),
%       ets:update_counter(stats, moves_wins, Moves);
%     false ->
%       ets:update_counter(stats, losts, 1)
%     end,
%   ets:insert(stats, {update_stats, true}),
%   {noreply ,State};

handle_cast({update_stats}, State = #state{totalWins=TotalWins, totalLosts=TotalLosts}) ->
  case ets:lookup(stats, update_stats) of
    [{_,true}] ->
      ets:insert(stats, {update_stats, false}),
      % get current accumulating stats
      [{_,Wins}] = ets:lookup(stats, wins),
      [{_,Losts}] = ets:lookup(stats, losts),
      [{_,TotalScoreWins}] = ets:lookup(stats, score_wins),
      [{_,TotalMovesWins}] = ets:lookup(stats, moves_wins),
      % update the stats using atomic operation
      ets:update_counter(stats, wins, -Wins),
      ets:update_counter(stats, losts, -Losts),
      ets:update_counter(stats, score_wins, -TotalScoreWins),
      ets:update_counter(stats, moves_wins, -TotalMovesWins),

      % transmitted those stats
      case Wins of
        0 ->
          gen_server:cast(get(server),{update_stats,node(),0,0,Losts,0,0});
        _ ->
          gen_server:cast(get(server),{update_stats,node(),0,Wins,Losts,TotalScoreWins/Wins,TotalMovesWins/Wins})
      end,

      % check if we need to keep update for the loop-update process
      NewTotalWins = TotalWins + Wins,
      NewTotalLosts = TotalLosts + Losts,
      io:format("simulation_server: update_stats with ~p ~p ~n",[integer_to_list(NewTotalWins),integer_to_list(NewTotalLosts)]),
      {noreply ,State#state{totalWins=NewTotalWins, totalLosts=NewTotalLosts}};
    [{_,false}] ->
      {noreply ,State}
    end;

handle_cast({request_bots, Num, DecisionID, Threshold}, State = #state{totalBots=NumOfBots}) ->
  io:format("simulation_server: request bots ~p ~n", [integer_to_list(Num)]),
  ets:insert(stats, {threshold, Threshold}),
  ets:update_counter(stats,simulations_left,Num),
  gen_server:cast(get(server),{update_stats,node(),Num,0,0,0,0}),
  create_bots(Num, DecisionID, Threshold),
  %gen_server:cast(get(server),{update_stats,NumOfValidBots,0,0,0,0}),
  {noreply ,State#state{totalBots=NumOfBots+Num}};

handle_cast({update_main_node, MainNode}, State) ->
  io:format("simulation_server: update main node to ~p ~n", [atom_to_list(MainNode)]),
  put(server,{main_server,MainNode}),
  {noreply, State#state{main_server_pid = MainNode}};

handle_cast({kill_simulation_server}, State) ->
  io:format("simulation_server ~p: recevied request to kill simulation server ~p ~n", [atom_to_list(node()), self()]),
  exit(self(),exit),
  {noreply,State};

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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

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
%% loop each 0.5s and send cast message to this Simulation Server
%% to send an cast update message of the stats to the Main server
update_main_server_timer(SimulationServer)->
  timer:sleep(1000),
  case ets:lookup(stats, update_stats) of
    [{_,true}] -> %io:format("cast message has been sent from the timer loop ~n"),
                  gen_server:cast(SimulationServer, {update_stats});
    [{_,false}] -> ok
  end,
  update_main_server_timer(SimulationServer).

create_bots(0, _, _) -> ok;
create_bots(NumOfBots, DecisionID, ThresholdForWin) ->
  spawn(fun() -> process_simulation(DecisionID, ThresholdForWin) end),
  %check_working({simulation_server,node()}, DecisionID, ThresholdForWin),
  %io:format("simulation_server: create bot ~p~n", [integer_to_list(NumOfBots)]),
  create_bots(NumOfBots-1, DecisionID, ThresholdForWin).

% check_working(SimulationServer, DecisionID, ThresholdForWin) ->
%   io:format("starting a new game ~p ~p ~n", [integer_to_list(DecisionID),integer_to_list(ThresholdForWin)]),
%   {Won, Score, Moves} = game_simulation(#bot_game{
%                 board=new_game_tile(new_game_tile(get_default_board(),true,false),true,false),
%                 game_over=false, freeTiles=true, score=0, won=false}, 0, DecisionID, ThresholdForWin),
%   %io:format("done ~p ~n", [integer_to_list(Score)]),
%   % notify the sumilation server main process when finished
%   io:format("Game Over score: ~p moves: ~p ~n", [integer_to_list(Score),integer_to_list(Moves)]),
%   gen_server:cast(SimulationServer, {update_from_bot, Won, Score, Moves}).


process_simulation(DecisionID, ThresholdForWin) ->
  % Simulate a game
  {Won, Score, Moves} = game_simulation(#bot_game{
                board=new_game_tile(new_game_tile(get_default_board(),true,false),true,false),
                game_over=false, freeTiles=true, score=0, won=false}, 0, DecisionID, ThresholdForWin),
  %io:format("done ~p ~n", [integer_to_list(Score)]),
  % notify the sumilation server main process when finished
  % gen_server:cast(SimulationServer, {update_from_bot, Won, Score, Moves}),
  ets:update_counter(stats,simulations_left,-1),
  case Won of
    true ->
      ets:update_counter(stats, wins, 1),
      ets:update_counter(stats, score_wins, Score),
      ets:update_counter(stats, moves_wins, Moves);
    false ->
      ets:update_counter(stats, losts, 1)
    end,
  ets:insert(stats, {update_stats, true}),

  % self kill the current bot process
  exit(self(),kill).

%% bot_game = {board, game_over, freeTiles, score, won}
%% DecisionID = 0:random, 1:byScore, 2:byKeepPlaying, 3:byMaxMerges
game_simulation(#bot_game{game_over=true, won=Won, score=Score}, Moves, _, _) ->
  {Won, Score, Moves};
game_simulation(BotGame, Moves, DecisionID, ThresholdForWin) ->
  % io:format("New bot move ~p ~n", [integer_to_list(Moves)]),
  game_simulation(bot_move(DecisionID, BotGame, ThresholdForWin), Moves+1, DecisionID, ThresholdForWin).

%%bot_move(DecisionID, BotGame)
% DecisionID = 0, Choose a random move (up, down, right, left)
bot_move(0, BotGame = #bot_game{board=Board,score=OldScore}, ThresholdForWin) ->
  case element(rand:uniform(4),{?UP,?DOWN,?RIGHT,?LEFT}) of
    ?UP ->
      {BoardAfterOperation, NewAddition2Score} = up_operation(Board);
    ?DOWN ->
      {BoardAfterOperation, NewAddition2Score} = down_operation(Board);
    ?RIGHT ->
      {BoardAfterOperation, NewAddition2Score} = right_operation(Board);
    ?LEFT ->
      {BoardAfterOperation, NewAddition2Score} = left_operation(Board)
    end,
  {GameOver, HasFreeTiles, Won} = is_game_over(tuple_to_list(BoardAfterOperation), ThresholdForWin),
  NewBoard = new_game_tile(BoardAfterOperation, HasFreeTiles, BoardAfterOperation=:=Board),
  BotGame#bot_game{board=NewBoard,game_over=GameOver,freeTiles=HasFreeTiles,score=OldScore+NewAddition2Score,won=Won};

% DecisionID = 1, Choose the move that evaluate the max score
bot_move(1, BotGame = #bot_game{board=Board,score=OldScore}, ThresholdForWin) ->
  % compute all 4 possiblites
  {BoardAfterOperationUP, NewAddition2ScoreUP} = up_operation(Board),
  {BoardAfterOperationDOWN, NewAddition2ScoreDOWN} = down_operation(Board),
  {BoardAfterOperationRIGHT, NewAddition2ScoreRIGHT} = right_operation(Board),
  {BoardAfterOperationLEFT, NewAddition2ScoreLEFT} = left_operation(Board),
  MergedList = [{BoardAfterOperationUP, NewAddition2ScoreUP},{BoardAfterOperationDOWN, NewAddition2ScoreDOWN},
                {BoardAfterOperationRIGHT, NewAddition2ScoreRIGHT},{BoardAfterOperationLEFT, NewAddition2ScoreLEFT}],
  % Choose the board with the max score
  ChangedBoards = lists:filter(fun({BoardAfterOp,_}) -> not(BoardAfterOp=:=Board) end, MergedList),
  SortedList = lists:keysort(2,ChangedBoards),
  case length(SortedList) of
    0 ->  BotGame#bot_game{game_over=true,won=false};
    Length ->
        {BoardAfterOperation, NewAddition2Score} = lists:nth(Length,SortedList),
        % analyze the choosen board
        {GameOver, HasFreeTiles, Won} = is_game_over(tuple_to_list(BoardAfterOperation), ThresholdForWin),
        NewBoard = new_game_tile(BoardAfterOperation, HasFreeTiles, false),
        BotGame#bot_game{board=NewBoard,game_over=GameOver,freeTiles=HasFreeTiles,score=OldScore+NewAddition2Score,won=Won}
  end;

% DecisionID = 2, Choose the move that will keep us in the game
bot_move(2, BotGame = #bot_game{board=Board,score=OldScore}, ThresholdForWin) ->
  % compute all 4 possiblites
  {BoardAfterOperation, NewAddition2Score} = up_operation(Board),
  {GameOver, HasFreeTiles, Won} = is_game_over(tuple_to_list(BoardAfterOperation), ThresholdForWin),
  case (GameOver or (BoardAfterOperation=:=Board)) of
    % then try down operation
    true -> bot_move_ID2(?RIGHT, BotGame, ThresholdForWin);
    % if the game isnt over keep playing
    % analyze the choosen board
    false ->
            NewBoard = new_game_tile(BoardAfterOperation, HasFreeTiles, false),
            BotGame#bot_game{board=NewBoard,game_over=GameOver,freeTiles=HasFreeTiles,score=OldScore+NewAddition2Score,won=Won}
  end;

% DecisionID = 3, Choose the move with the max tile merged
bot_move(3, BotGame = #bot_game{board=Board,score=OldScore}, ThresholdForWin) ->
  % compute all 4 possiblites
  {BoardAfterOperationUP, NewAddition2ScoreUP} = up_operation(Board),
  {BoardAfterOperationDOWN, NewAddition2ScoreDOWN} = down_operation(Board),
  {BoardAfterOperationRIGHT, NewAddition2ScoreRIGHT} = right_operation(Board),
  {BoardAfterOperationLEFT, NewAddition2ScoreLEFT} = left_operation(Board),
  NumOfTilesUP = number_of_tiles(tuple_to_list(BoardAfterOperationUP)),
  NumOfTilesDOWN = number_of_tiles(tuple_to_list(BoardAfterOperationDOWN)),
  NumOfTilesRIGHT = number_of_tiles(tuple_to_list(BoardAfterOperationRIGHT)),
  NumOfTilesLEFT = number_of_tiles(tuple_to_list(BoardAfterOperationLEFT)),

  % Choose the board with the max merged tiles
  MergedList = [{BoardAfterOperationUP, NewAddition2ScoreUP, NumOfTilesUP},{BoardAfterOperationDOWN, NewAddition2ScoreDOWN, NumOfTilesDOWN},
                {BoardAfterOperationRIGHT, NewAddition2ScoreRIGHT, NumOfTilesRIGHT},{BoardAfterOperationLEFT, NewAddition2ScoreLEFT, NumOfTilesLEFT}],
  ChangedBoards = lists:filter(fun({BoardAfterOp,_,_}) -> not(BoardAfterOp=:=Board) end, MergedList),

  SortedList = lists:keysort(3,ChangedBoards),
  case length(SortedList) of
    0 ->  BotGame#bot_game{game_over=true,won=false};
    _ ->
        {BoardAfterOperation, NewAddition2Score, _} = lists:nth(1,SortedList),
        % analyze the choosen board
        {GameOver, HasFreeTiles, Won} = is_game_over(tuple_to_list(BoardAfterOperation), ThresholdForWin),
        NewBoard = new_game_tile(BoardAfterOperation, HasFreeTiles, false),
        BotGame#bot_game{board=NewBoard,game_over=GameOver,freeTiles=HasFreeTiles,score=OldScore+NewAddition2Score,won=Won}
  end;

% DecisionID = 4, Choose the move with the Heuristic scoring algorithm
bot_move(4, BotGame = #bot_game{board=Board,score=OldScore}, ThresholdForWin) ->
  % compute all 4 possiblites
  {BoardAfterOperationUP, NewAddition2ScoreUP} = up_operation(Board),
  {BoardAfterOperationDOWN, NewAddition2ScoreDOWN} = down_operation(Board),
  {BoardAfterOperationRIGHT, NewAddition2ScoreRIGHT} = right_operation(Board),
  {BoardAfterOperationLEFT, NewAddition2ScoreLEFT} = left_operation(Board),
  HeuristicScoreUP = comptue_Heuristic_score(BoardAfterOperationUP),
  HeuristicScoreDOWN = comptue_Heuristic_score(BoardAfterOperationDOWN),
  HeuristicScoreRIGHT = comptue_Heuristic_score(BoardAfterOperationRIGHT),
  HeuristicScoreLEFT = comptue_Heuristic_score(BoardAfterOperationLEFT),

  % Choose the board with the max merged tiles
  MergedList = [{BoardAfterOperationUP, NewAddition2ScoreUP, HeuristicScoreUP},{BoardAfterOperationDOWN, NewAddition2ScoreDOWN, HeuristicScoreDOWN},
                {BoardAfterOperationRIGHT, NewAddition2ScoreRIGHT, HeuristicScoreRIGHT},{BoardAfterOperationLEFT, NewAddition2ScoreLEFT, HeuristicScoreLEFT}],
  ChangedBoards = lists:filter(fun({BoardAfterOp,_,_}) -> not(BoardAfterOp=:=Board) end, MergedList),
  SortedList = lists:keysort(3,ChangedBoards),
  case length(SortedList) of
    0 ->  BotGame#bot_game{game_over=true,won=false};
    Length ->
        {BoardAfterOperation, NewAddition2Score, _} = lists:nth(Length,SortedList),
        % analyze the choosen board
        {GameOver, HasFreeTiles, Won} = is_game_over(tuple_to_list(BoardAfterOperation), ThresholdForWin),
        NewBoard = new_game_tile(BoardAfterOperation, HasFreeTiles, false),
        BotGame#bot_game{board=NewBoard,game_over=GameOver,freeTiles=HasFreeTiles,score=OldScore+NewAddition2Score,won=Won}
  end.

% Compute Heuristic score in a "snake" path 1, for more info see the design document
comptue_Heuristic_score(Board) ->
  lists:max([comptue_Heuristic_score_by_path(1,Board), comptue_Heuristic_score_by_path(2,Board)]).

comptue_Heuristic_score_by_path(1, Board)->
  comptue_Heuristic_partial_path(1,0,{element(16,Board),element(15,Board),element(14,Board),element(13,Board)}) +
  comptue_Heuristic_partial_path(1,4,{element(9,Board),element(10,Board),element(11,Board),element(12,Board)}) +
  comptue_Heuristic_partial_path(1,8,{element(5,Board),element(6,Board),element(7,Board),element(8,Board)}) +
  comptue_Heuristic_partial_path(1,12,{element(1,Board),element(2,Board),element(3,Board),element(4,Board)});

comptue_Heuristic_score_by_path(2, Board)->
  comptue_Heuristic_partial_path(1,0,{element(13,Board),element(9,Board),element(5,Board),element(1,Board)}) +
  comptue_Heuristic_partial_path(1,4,{element(2,Board),element(6,Board),element(10,Board),element(14,Board)}) +
  comptue_Heuristic_partial_path(1,8,{element(15,Board),element(11,Board),element(7,Board),element(3,Board)}) +
  comptue_Heuristic_partial_path(1,12,{element(16,Board),element(12,Board),element(8,Board),element(4,Board)}).

comptue_Heuristic_partial_path(1, N, {{_,T1},{_,T2},{_,T3},{_,T4}}) ->
  math:pow(0.5,N)*T1 + math:pow(0.5,N+1)*T2 + math:pow(0.5,N+2)*T3 + math:pow(0.5,N+3)*T4.

%% Compute the number of tiles with value > 0 in the board
number_of_tiles(Board) ->
  lists:foldr(fun({_,Val}, AccIn) ->
                case Val of
                  0 -> AccIn;
                  _ -> AccIn+1
                end
              end,
  0, Board).

%% DecisionID = 2, Choose the move that will keep us in the game, the bot will try right after up
bot_move_ID2(?RIGHT, BotGame = #bot_game{board=Board,score=OldScore}, ThresholdForWin) ->
  % compute all 4 possiblites
  {BoardAfterOperation, NewAddition2Score} = right_operation(Board),
  {GameOver, HasFreeTiles, Won} = is_game_over(tuple_to_list(BoardAfterOperation), ThresholdForWin),
  case (GameOver or (BoardAfterOperation=:=Board)) of
    % then try down operation
    true -> bot_move_ID2(?LEFT, BotGame, ThresholdForWin);
    % if the game isnt over keep playing
    false -> % analyze the choosen board
            NewBoard = new_game_tile(BoardAfterOperation, HasFreeTiles, false),
            BotGame#bot_game{board=NewBoard,game_over=GameOver,freeTiles=HasFreeTiles,score=OldScore+NewAddition2Score,won=Won}
  end;

%% DecisionID = 2, Choose the move that will keep us in the game, the bot will try left after right
bot_move_ID2(?LEFT, BotGame = #bot_game{board=Board,score=OldScore}, ThresholdForWin) ->
  % compute all 4 possiblites
  {BoardAfterOperation, NewAddition2Score} = left_operation(Board),
  {GameOver, HasFreeTiles, Won} = is_game_over(tuple_to_list(BoardAfterOperation), ThresholdForWin),
  case (GameOver or (BoardAfterOperation=:=Board)) of
    % then try down operation
    true -> bot_move_ID2(?DOWN, BotGame, ThresholdForWin);
    % if the game isnt over keep playing
    false -> % analyze the choosen board
            NewBoard = new_game_tile(BoardAfterOperation, HasFreeTiles, false),
            BotGame#bot_game{board=NewBoard,game_over=GameOver,freeTiles=HasFreeTiles,score=OldScore+NewAddition2Score,won=Won}
  end;

%% DecisionID = 2, Choose the move that will keep us in the game, the bot will finally try down
bot_move_ID2(?DOWN, BotGame = #bot_game{board=Board,score=OldScore}, ThresholdForWin) ->
  {BoardAfterOperation, NewAddition2Score} = down_operation(Board),
  case BoardAfterOperation=:=Board of
    true ->
      BotGame#bot_game{game_over=true,won=false};
    false ->
      {GameOver, HasFreeTiles, Won} = is_game_over(tuple_to_list(BoardAfterOperation), ThresholdForWin),
      NewBoard = new_game_tile(BoardAfterOperation, HasFreeTiles, BoardAfterOperation=:=Board),
      BotGame#bot_game{board=NewBoard,game_over=GameOver,freeTiles=HasFreeTiles,score=OldScore+NewAddition2Score,won=Won}
  end.


%% 16 tiles of {{#row,#col},value}
get_default_board() ->
  {{{1,1},0}, {{1,2},0}, {{1,3},0}, {{1,4},0}, {{2,1},0}, {{2,2},0},
  {{2,3},0}, {{2,4},0}, {{3,1},0}, {{3,2},0}, {{3,3},0}, {{3,4},0}, {{4,1},0},
  {{4,2},0}, {{4,3},0}, {{4,4},0}}.

%% create a new tile on the grid with the value 2 or 4, used in each new step
% new_game_tile(Board:tuple, HasFreeTiles:boolean, HasTheOperationChangeTheBoard?:boolean)
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

is_game_over(BoardList, Threshold) ->
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
                   case Val of
                    0 -> true;
                    _ -> false
                   end
    end,BoardList),
    Won = lists:any(fun({_,Val}) ->
                  case Val of
                    Threshold -> true;
                    _ -> false
                   end
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
