-module(dot_memory).

%% API exports
-export([accumulated/0,
         independent/0,
         accumulated/1,
         independent/1,
         label/2]).

-include("dot.hrl").
%%====================================================================
%% API functions
%%====================================================================
accumulated() ->
    accumulated(dot_processes:ancestors()).

independent() ->
    independent(dot_processes:ancestors()).

accumulated(Processes) ->
    Mapped = dot_processes:map(Processes, fun map/1),
    Reduced = dot_processes:reduce(Mapped, fun reduce_sum/2),
    Sorted = dot_processes:sort(Reduced, fun sort/2),
    dot_processes:limit(Sorted, 15).

independent(Processes) ->
    Mapped = dot_processes:map(Processes, fun map/1),
    Reduced = dot_processes:reduce(Mapped, fun reduce_ident/2),
    dot_processes:sort(Reduced, fun sort/2).

label(_, Value) ->
    MB = erlang:round((Value / 1024 / 1024)*1000),
    [ io_lib:format("~.10B.~3.10.0B", [MB div 1000, MB rem 1000]), " MB"].

%%====================================================================
%% Internal functions
%%====================================================================
map(#dot_process{pid = Pid}) ->
    case erlang:process_info(Pid, memory) of
        {memory, Memory} -> Memory;
        _ -> 0
    end.

reduce_sum({_, Value}, Acc) ->
    Acc + Value.

reduce_ident({_, _}, Acc) ->
    Acc.

sort({_, Mem1}, {_, Mem2}) ->
    Mem1 > Mem2.
