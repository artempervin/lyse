-module(curling).
-export([start_link/2, set_teams/3, add_points/3, next_round/1, stop/1]).
-export([join_feed/2, leave_feed/2]).
-export([game_info/1]).

start_link(TeamA, TeamB) ->
	{ok, Pid} = gen_event:start_link(),
	gen_event:add_sup_handler(Pid, curling_scoreboard, []),
	gen_event:add_sup_handler(Pid, curling_accumulator, []),
	set_teams(Pid, TeamA, TeamB),
	{ok, Pid}.

stop(Pid) ->
	gen_event:stop(Pid).

game_info(Pid) ->
	gen_event:call(Pid, curling_accumulator, game_data).

set_teams(Pid, TeamA, TeamB) ->
	gen_event:notify(Pid, {set_teams, TeamA, TeamB}).

add_points(Pid, Team, N) ->
	gen_event:notify(Pid, {add_points, Team, N}).

next_round(Pid) ->
	gen_event:notify(Pid, next_round).

join_feed(Pid, ToPid) ->
	HandlerId = {curling_feed, make_ref()},
	gen_event:add_handler(Pid, HandlerId, [ToPid]),
	HandlerId.

leave_feed(Pid, HandlerId) ->
	gen_event:delete_handler(Pid, HandlerId, leave_feed).
