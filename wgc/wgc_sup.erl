-module (wgc_sup).
-behaviour (supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  {ok, {{one_for_one, 1, 3600},
    [{wgc,
      {wgc, start_link, []},
      permanent, 1000, worker, [wgc]}]}}.
