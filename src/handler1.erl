-module(handler1).

-export([init/2]).

init(Req0, Opts) ->
	{ok, Data, _Req} = cowboy_req:read_body(Req0),
        Map = jsx:decode(Data, [return_maps]),
    case maps:find(<<"action">>, Map) of
        {ok, <<"insert">>} ->
            Key = maps:find(<<"key">>, Map),
            Val = maps:find(<<"value">>, Map),
            cache_server:start_link(name, [{drop_interval, 3600}]),
            cache_server:insert(name, Key, Val, 3600),
            Reply = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/plain">>
            }, <<"{\"result\":\"ok\"}">>, Req0),
            {ok, Reply, Opts};
        {ok, <<"lookup">>} ->
            Key = maps:find(<<"key">>, Map),
            {ok, Value} = cache_server:lookup(name, Key),
            Json = jsx:encode([{<<"result">>, Value}]),
            Reply = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/plain">>
            }, Json, Req0),
            {ok, Reply, Opts};
        {ok, <<"lookup_by_date">>} ->
            DateFrom = maps:find(<<"date_from">>, Map),
            DateTo = maps:find(<<"date_to">>, Map),
            Value = cache_server:lookup_by_date(name, DateFrom, DateTo),
            Json = jsx:encode([<<"result">>, Value]),
            Reply = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/plain">>
}, Json, Req0),
            {ok, Reply, Opts}
    end.
