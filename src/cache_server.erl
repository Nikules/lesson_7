-module(cache_server).

%% API
-export([start_link/2]).
-export([stop/1]).
-export([process_init/2]).
-export([insert/4]).
-export([lookup/2]).
-export([lookup_by_date/3]).

-define(DATE, calendar:datetime_to_gregorian_seconds(calendar:local_time())).

start_link(TableName, [{drop_interval, Interval}]) ->
    Pid = spawn(?MODULE, process_init, [TableName, Interval]),
    register(TableName, Pid),
    {ok, Pid}.

stop(Pid) ->
    Pid ! stop.

process_init(TableName, Interval) ->
    timer:send_interval(Interval * 1000, TableName, {delete, TableName}),
    State = handler:create(TableName),
    process_loop(State).

insert(TableName, Key, Val, LifeTime) ->
   TableName ! {insert,TableName, Key, Val, LifeTime}.

lookup(TableName, Key) ->
    TableName ! {lookup, self(),TableName, Key},
    receive
        {result, Result} -> Result
    end.

lookup_by_date(TableName, DateFrom, DateTo) ->
    TableName ! {lookup_by, self(), TableName, DateFrom, DateTo},
    %io:format("DateF~n~p", [DateFrom]),
    receive
        {result, Result} -> Result
    end.

process_loop(State) ->
    receive
        {delete, TableName} ->
            New_State = handler:delete(TableName),
            process_loop(New_State);
        {insert, TableName, Key, Val, LifeTime} ->
            New_State = handler:insert(TableName, Key, Val, LifeTime),
            process_loop(New_State);
        {lookup, From, TableName, Key} ->
            Result = handler:lookup(TableName, Key),
            From ! {result, Result},
            process_loop(State);
        {lookup_by, From, TableName, DateFrom, DateTo} ->
            Result = handler:lookup_by_date(TableName, parse_date(DateFrom), parse_date(DateTo)),
            From ! {result, Result},
            process_loop(State);
        stop ->
            ok
    end.

parse_date({ok, Date}) -> parse_date(Date, <<>>, []).

parse_date(<<>>, Acc, [Sec, Min, Day, Month, Year]) ->
    Millisec = binary_to_integer(Acc),
    {{Year, Month, Day}, {Min, Sec, Millisec}};
parse_date(Date, Acc, Rez) ->
    <<El, Rest/binary>> = Date,
    case <<El>> of
        <<":">> -> parse_date(Rest, <<>>, [binary_to_integer(Acc)|Rez]);
        <<"/">> -> parse_date(Rest, <<>>, [binary_to_integer(Acc)|Rez]);
        <<" ">> -> parse_date(Rest, <<>>, [binary_to_integer(Acc)|Rez]);
        _ -> parse_date(Rest, <<Acc/binary, El>>, Rez)
    end.
