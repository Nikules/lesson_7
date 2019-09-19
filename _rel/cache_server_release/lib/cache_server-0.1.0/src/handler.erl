-module(handler).

-include_lib("stdlib/include/ms_transform.hrl").

-export([create/1]).
-export([insert/4]).
-export([lookup/2]).
-export([delete_obsolete/1]).
-export([delete/1]).
-export([lookup_by_date/3]).

-define(DATE, calendar:datetime_to_gregorian_seconds(calendar:local_time())).

-record(table,{
    key,
    value,
    lifetime,
    date=?DATE
}).

create(TableName) ->

    ets:new(TableName, [public,ordered_set,named_table,{keypos, #table.key}]),
ok.

insert(TableName, Key, Value, LifeTime) ->
    LifeDate = LifeTime + ?DATE,
    Record = #table{
        key=Key,
        value=Value,
        lifetime=LifeDate
    },
    ets:insert(TableName, Record).

lookup(TableName, Key) ->
    case ets:lookup(TableName, Key) of
        [Rec] ->
            case Rec#table.lifetime > ?DATE of
                true -> Rec#table.value;
                false -> ets:delete(TableName, Key),time_over
            end;
        [] -> non_existing
    end.

delete_obsolete(TableName) ->
    First = ets:first(TableName),
    delete_obsolete(TableName, First).

delete_obsolete(_TableName, '$end_of_table') -> ok;
delete_obsolete(TableName, Key) ->
    Next = ets:next(TableName, Key),
    [Rec] = ets:lookup(TableName, Key),
    case Rec#table.lifetime > ?DATE of
        true -> delete_obsolete(TableName, Next);
        false -> ets:delete(TableName, Key)
    end.

delete(TableName) ->
    ets:delete_all_objects(TableName).

lookup_by_date(TableName, DateFrom, DateTo) ->
    DateFrom1 = calendar:datetime_to_gregorian_seconds(DateFrom),
    DateTo1 = calendar:datetime_to_gregorian_seconds(DateTo),
    MS = ets:fun2ms(fun({Name, Key, Value,LifeTime, Date})
                          when DateFrom1 < Date , DateTo1 > Date ->
                            Value
                    end),
    Rez = ets:select(TableName, MS),
    Rez.
