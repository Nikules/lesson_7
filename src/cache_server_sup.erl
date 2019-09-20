-module(cache_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
	Procs = [#{
        id => cache_server,
        start => {cache_server, start_link, []},
        restart => permanent,
        shutdawn => brutal_kill,
        type => worker,
        modules => []
}],
	{ok, {{one_for_one, 1, 5}, Procs}}.
