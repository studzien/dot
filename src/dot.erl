-module(dot).

%% API exports
-export([generate/2,
         generate/3]).

-include("dot.hrl").
%%====================================================================
%% API functions
%%====================================================================
generate(Filename, ProcessValues) ->
    generate(Filename, ProcessValues, fun default_label/2).

generate(Filename, ProcessValues, LabelFun) when is_function(LabelFun, 2) -> 
    File = file(ProcessValues, LabelFun),
    file:write_file(Filename, File).

%%====================================================================
%% Internal functions
%%====================================================================
default_label(_, Value) ->
    to_list(Value).

file(ProcessValues, LabelFun) ->
    ["digraph {\n",
     content(ProcessValues, LabelFun),
     "\n}"].

content(ProcessValues, LabelFun) ->
    [vertices(ProcessValues, LabelFun),
     edges(ProcessValues)].

vertices(ProcessValues, LabelFun) ->
    [ vertex(Process, Value, LabelFun) || {Process, Value} <- ProcessValues ].

vertex(#dot_process{id = Ref}, {more, Value}, _) when is_reference(Ref) ->
    [ "\t\"", to_list(Ref),
      "\"[label=\"", to_list(Value), " more...\"];\n" ];
vertex(#dot_process{id = Id}=P, Value, LabelFun) ->
    [ "\t\"", to_list(Id),
      "\"[label=\"", to_list(Id), "\\n", LabelFun(P, Value), "\"];\n" ].

edges(ProcessValues) ->
    [ edge(Process) || {Process, _} <- ProcessValues ].

edge(#dot_process{id = Id} = Process) ->
    case parent(Process) of
        none ->
            [];
        Parent ->
            ["\t\"", to_list(Parent), "\" -> \"", to_list(Id), "\";\n"]
    end.

parent(#dot_process{ancestors = []}) ->
    none;
parent(#dot_process{ancestors = [Parent|_]}) ->
    Parent.

to_list(List) when is_list(List) ->
    List;
to_list(Pid) when is_pid(Pid) ->
    pid_to_list(Pid);
to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
to_list(Ref) when is_reference(Ref) ->
    erlang:ref_to_list(Ref).
