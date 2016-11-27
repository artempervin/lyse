-module(wgc). %% wolf, goat and cabbage
-behaviour (gen_server).
-record(state, {west=[], east=[]}).

-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([move/2, status/0, start_link/0, stop/0]).
-define(SIDES, [east, west]).
-define(ELEMENTS, [wolf, goat, cabbage]).

%%% Public API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:stop(?MODULE).

init(_Args) ->
  {ok, #state{west=?ELEMENTS}}.

move(What, Where) ->
  move(lists:member(What, ?ELEMENTS), lists:member(Where, ?SIDES), What, Where).
move(false, _, _, Where) ->
  io:format("Invalid destination: ~p! Only ~p accepted!~n", [Where, ?SIDES]);
move(_, false, What, _) ->
  io:format("Invalid element to move: ~p! Only ~p accepted!~n", [What, ?ELEMENTS]);
move(true, true, What, Where) ->
  gen_server:call(?MODULE, {move, What, Where}).

status() ->
  gen_server:call(?MODULE, status).

%%% gen_server callbacks
handle_call({move, What, Where}, _From, S=#state{west=West, east=East}) ->
  case validate_move(What, Where, East, West) of
    true ->
      {NewWest, NewEast} = transfer(What, Where, West, East),
      NewState=#state{west=NewWest, east=NewEast},
      Reply = validate_state(NewState),
      reply(Reply, NewState);
    false ->
      {reply, {invalid_move, lists:flatten(io_lib:format("Can't move ~p to ~p! East: ~p, West: ~p", [What, Where, East, West]))}, S}
  end;
handle_call(status, _From, State) ->
  {reply, state_to_string(State), State};
handle_call(_Msg, _From, State) ->
  {noreply, State}.

state_to_string(#state{west=West, east=East}) ->
  lists:flatten(io_lib:format("East: ~p, West: ~p~n", [East, West])).

reply(game_over, State) ->
  {stop, normal, "Game over", State};
reply(ok, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, State) ->
  io:format(state_to_string(State)),
  io:format("Game over! Let's play again!").

code_change(_OldVsn, State, _Extra) ->  {ok, State}.

% private functions
transfer(What, east, West, East) ->
  NewWest = West -- [What],
  NewEast = [What|East],
  {NewWest, NewEast};
transfer(What, west, West, East) ->
  NewWest = [What|West],
  NewEast = East -- [What],
  {NewWest, NewEast}.

validate_move(What, Where, East, West) ->
  case Where of
    east -> %% moving 'What' from West to East
      lists:member(What, West);
    west -> %% moving 'What' from East to West
      lists:member(What, East);
    _    -> false
  end.

select_side(West, East) ->
  {_, Side} = max({length(West), West}, {length(East), East}),
  Side.

validate_state(#state{west=West, east=East}) ->
  %% only need to consider a side that have 2 elements
  Side = lists:sort(select_side(West, East)),
  validate_state(Side, length(Side)).
validate_state(List, 2) ->
  First  = lists:nth(1, List),
  Second = lists:nth(2, List),
  validate_side(First, Second);
validate_state(_, _) -> %% no check needed
  ok.
validate_side(cabbage, goat) -> game_over;
validate_side(goat, wolf)    -> game_over;
validate_side(_, _)          -> ok.
