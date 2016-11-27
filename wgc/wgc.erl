-module(wgc). %% wolf, goat and cabbage
-behaviour (gen_server).
-record(state, {left=[],
                right=[],
                position}).

-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([move/1, move/0, status/0, start_link/0]).
-define(SIDES, [left, right]).
-define(ELEMENTS, [wolf, goat, cabbage]).

%%% Public API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, #state{left=?ELEMENTS, position=left}}.

%% move to other side without anything
move() ->
  Position = gen_server:call(?MODULE, get_current_side),
  gen_server:call(?MODULE, {move, nil, other_side(Position)}).
%% move to other side with something
move(What) ->
  Position = gen_server:call(?MODULE, get_current_side),
  move(lists:member(What, ?ELEMENTS), What, other_side(Position)).
move(false, What, _) ->
  io:format("Invalid element to move: ~p! Only ~p accepted!~n", [What, ?ELEMENTS]);
move(true, What, Where) ->
  gen_server:call(?MODULE, {move, What, Where}).

status() ->
  gen_server:call(?MODULE, status).

%% gen_server callbacks
handle_call({move, nil, Where}, _From, S) ->
  case validate_move(Where, S) of
    true  -> process_move(S);
    false -> reply_cant_move(nil, Where, S)
  end;
handle_call({move, What, Where}, _From, S=#state{position=Position}) ->
  case validate_move(What, Where, S) of
    true ->
      {NewLeft, NewRight} = transfer(What, Where, S),
      process_move(#state{left=NewLeft, right=NewRight, position=Position});
    false ->
      reply_cant_move(What, Where, S)
  end;
handle_call(status, _From, State) ->
  {reply, state_to_string(State), State};
handle_call(get_current_side, _From, S=#state{position=Position}) ->
  {reply, Position, S};
handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, State) ->
  io:format("~s~n", [state_to_string(State)]).

code_change(_OldVsn, State, _Extra) ->  {ok, State}.

% private functions
process_move(S=#state{position=Position}) ->
  NewState = S#state{position=other_side(Position)},
  Reply = validate_state(NewState),
  reply_new_state(Reply, NewState).

other_side(left) -> right;
other_side(_)    -> left.

reply_new_state(failure, State) ->
  {stop, normal, "Game over: you lose!", State};
reply_new_state(success, State) ->
  {stop, normal, "Game over: you won!", State};
reply_new_state(ok, State) ->
  {reply, state_to_string(State), State}.

reply_cant_move(What, Where, S) ->
  {reply, lists:flatten(io_lib:format("Can't move to ~p~s! ~s", [Where, elements_to_string(What), state_to_string(S)])), S}.
elements_to_string(nil) ->
  [];
elements_to_string(What) ->
  lists:flatten(io_lib:format(" with ~p", [What])).

state_to_string(#state{left=Left, right=Right, position=Position}) ->
  lists:flatten(io_lib:format("left: ~p, right: ~p, Farmer is at ~p side", [Left, Right, Position])).

transfer(What, right, #state{left=Left, right=Right}) ->
  NewLeft = Left -- [What],
  NewRight = [What|Right],
  {NewLeft, NewRight};
transfer(What, left, #state{left=Left, right=Right}) ->
  NewLeft = [What|Left],
  NewRight = Right -- [What],
  {NewLeft, NewRight}.

%% move with something
validate_move(What, Where, #state{left=Left, right=Right, position=Position}) ->
  validate_move(What, Where, Left, Right, Where =:= other_side(Position)).
validate_move(What, Where, Left, Right, true) ->
  case Where of
    right -> %% moving 'What' from Left to Right
      lists:member(What, Left);
    left -> %% moving 'What' from Right to Left
      lists:member(What, Right);
    _    -> false
  end;
validate_move(_,_,_,_,false) -> %% cannot move because Farmer is not there
  false.
%% move without anything
validate_move(Where, #state{position=Position}) ->
  Where =:= other_side(Position).

select_side(Left, Right) ->
  {SideElementsCount, SideElements, SideName} = max({length(Left), Left, left}, {length(Right), Right, right}),
  {SideName, lists:sort(SideElements), SideElementsCount}.

%% everything was transfered to another side
validate_state(#state{left=[]}) ->
  success;
%% transfer in progress, check if player has failed
validate_state(#state{left=Left, right=Right, position=Position}) ->
  %% only need to consider a side that have more elements
  {SideName, Elements, Count} = select_side(Left, Right),
  IsFarmerPresent = SideName =:= Position,
  validate_state(Elements, Count, IsFarmerPresent).
validate_state(List, Count, IsFarmerPresent) when Count =:= 2 ->
  First  = lists:nth(1, List),
  Second = lists:nth(2, List),
  validate_side(First, Second, IsFarmerPresent);
validate_state(_, Count, false) when Count =:= 3 ->
  failure;
validate_state(_, Count, true) when Count =:= 3 ->
  ok.

validate_side(cabbage, goat, false) -> failure;
validate_side(goat, wolf, false)    -> failure;
validate_side(_, _, _)              -> ok.
