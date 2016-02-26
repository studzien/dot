-module(dot_processes).

%% API exports
-export([ancestors/0,
         map/2,
         reduce/2,
         limit/2,
         sort/2]).

-include("dot.hrl").
%%====================================================================
%% API functions
%%====================================================================
ancestors() ->
    [ #dot_process{pid = Pid,
                   id = id(Pid),
                   ancestors = ancestors(Pid)} || Pid <- erlang:processes() ].

map(Processes, Fun) when is_function(Fun, 1) ->
    [ {Process, Fun(Process)} || Process <- Processes ].

reduce(ProcsValues, Fun) when is_function(Fun, 2) ->
    Groups = group_path_length(ProcsValues),
    lists:flatten(do_reduce(Groups, _LastGroup = [], Fun, _Acc = [])).

limit(ProcsValues, N) when is_integer(N) ->
    {Limited, Counts} = do_limit(ProcsValues, N, dict:new(), _Acc = []),
    overheads(Counts, N) ++ Limited.

sort(ProcsValues, Fun) when is_function(Fun, 2) ->
    lists:sort(Fun, ProcsValues).

%%====================================================================
%% Internal functions
%%====================================================================
id(Pid) ->
    case erlang:process_info(Pid, registered_name) of
        {registered_name, Name} -> Name;
        _ -> Pid
    end.

ancestors(Pid) ->
    {dictionary, Dict} = erlang:process_info(Pid, dictionary),
    proplists:get_value('$ancestors', Dict, []).

overheads(Counts, N) ->
    [ {#dot_process{id = make_ref(), ancestors = Ancestors}, M}
      || {Ancestors, M} <- dict:to_list(Counts), M > N ].

do_limit([], _, Counts, Acc) ->
    {lists:reverse(Acc), Counts};
do_limit([{#dot_process{ancestors = []}, _} = PV | Rest], N, Counts, Acc) ->
    do_limit(Rest, N, Counts, [PV|Acc]);
do_limit([{#dot_process{ancestors = Ancestors}, _} = PV | Rest],
         N, Counts0, Acc) ->
    Counts1 = dict:update_counter(Ancestors, 1, Counts0),
    case dict:fetch(Ancestors, Counts1) of
        M when M > N ->
            do_limit(Rest, N, Counts1, Acc);
        _ ->
            do_limit(Rest, N, Counts1, [PV|Acc])
    end.

do_reduce([], _, _, Acc) ->
    Acc;
do_reduce([Group0|Rest], LastGroup, Fun, Acc) ->
    Group1 = [ reduce_process(Process, LastGroup, Fun) || Process <- Group0 ],
    do_reduce(Rest, Group1, Fun, [Group1 | Acc]).

reduce_process({Proc, Value}, LastGroup, Fun) ->
    ChildrenValues = [ PV || {P, _}=PV <- LastGroup,
                             is_parent(Proc, P) ],
    ReducedValue = lists:foldl(Fun, Value, ChildrenValues),
    {Proc, ReducedValue}.

is_parent(#dot_process{id = Id, ancestors = Anc1},
          #dot_process{ancestors = Anc2}) ->
    [Id|Anc1] =:= Anc2.

group_path_length(PV0) ->
    PV1 = lists:sort(fun sort_path_length/2, PV0),
    group_path_length(PV1, _Last = undefined, _Acc = []).

group_path_length([], _, Acc) ->
    lists:reverse(Acc);
group_path_length([{#dot_process{ancestors = A}, _}=PV|Rest], Last, Acc)
  when length(A) < Last ->
    group_path_length(Rest, length(A), [[PV]|Acc]);
group_path_length([PV|Rest], Last, [Group|Acc]) ->
    group_path_length(Rest, Last, [[PV|Group]|Acc]).

sort_path_length({#dot_process{ancestors = A1}, _},
                 {#dot_process{ancestors = A2}, _}) when
      length(A1) > length(A2) ->
    true;
sort_path_length(_, _) -> false.
