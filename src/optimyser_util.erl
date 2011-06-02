-module(optimyser_util).
-export([shuffle/1, new_guid/0, export_guid/0, dump_to_textfile/2, string_join/2, export_combid/1]).
-export([import_combid/1]).
-compile(export_all).

-include("optimyser.hrl").

export_combid(Combination) ->
    string_join(lists:map(fun erlang:integer_to_list/1, Combination), $-).

import_combid(String) ->
    try lists:map(fun erlang:list_to_integer/1, string:tokens(String, "-")) of
        Val -> {value, Val}
    catch
        _:_ -> false
    end.

string_join(Items, Sep) ->
    lists:flatten(lists:reverse(my_string_join1(Items, Sep, []))).

my_string_join1([Head | []], _Sep, Acc) ->
    [Head | Acc];
my_string_join1([Head | Tail], Sep, Acc) ->
    my_string_join1(Tail, Sep, [Sep, Head | Acc]).

dump_to_textfile(Table, File) ->
    dump_to_textfile(mnesia_lib:is_running(), file:open(File, [write]), Table).
dump_to_textfile(yes, {ok, F}, Table) ->
    % Tabs = lists:delete(schema, mnesia_lib:local_active_tables()),
    % Defs = lists:map(fun(T) -> {T, [{record_name, mnesia_lib:val({T, record_name})},
    % 				    {attributes, mnesia_lib:val({T, attributes})}]} 
    % 		     end,
    % 		     Tabs),
    % io:format(F, "~p.~n", [{tables, Defs}]),
    dump_tab(F, Table),
    file:close(F);
dump_to_textfile(_,_,_) -> error.

dump_tab(F, T) ->
    W = mnesia_lib:val({T, wild_pattern}),
    {atomic,All} = mnesia:transaction(fun() -> mnesia:match_object(T, W, read) end),
    lists:foreach(fun(Term) -> io:format(F,"~p.~n", [setelement(1, Term, T)]) end, All).

%% @spec shuffle(list()) -> list()
%% @doc Shuffles a list
shuffle(List) ->
    randomize(round(math:log(length(List)) + 0.5), List).

new_guid() -> {node(), erlang:now()}.

export_guid() ->
    export_guid(new_guid()).

export_guid({N,{Ms,S,Us}}) -> 
    lists:concat( [atom_to_list(N), ".", integer_to_list(Ms), ".", 
    integer_to_list(S), ".", integer_to_list(Us)]).
    
%% Internal functions
randomize(1, List) ->
   randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) -> randomize(Acc) end,randomize(List), 
        lists:seq(1, (T - 1))).

randomize(List) ->
   D = lists:map(fun(A) ->
                    {random:uniform(), A}
             end, List),
   {_, D1} = lists:unzip(lists:keysort(1, D)), 
   D1.
