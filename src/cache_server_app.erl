-module(cache_server_app).

-export([start/2]).
-export([stop/1]).


start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/api/cache_server", handler1, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}).

stop(_State) ->
	ok.
