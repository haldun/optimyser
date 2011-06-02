-module(optimyser_taguchi).
-behaviour(gen_server).

-include("optimyser.hrl").

-record(state, {table}).

-compile(export_all).

%% API
-export([start_link/0]).
-export([arrange_array/2]).
-export([is_taguchi/1, get_taguchi_array/1, get_stats/0, add_array/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

arrange_array(Scheme, Sections) ->
    gen_server:call(?MODULE, {arrange_array, Scheme, Sections}).

%%
%% TODO doc this
is_taguchi(Scheme) ->
	gen_server:call(?MODULE, {is_taguchi, Scheme}).

%% @spec get_taguchi_array(experiment()) -> array | undefined
%% @doc Returns true if the given combination scheme is applicable for 
%%      Taguchi method by looking at the taguchi orthogonal arrays at hand.
get_taguchi_array(Scheme) ->
	gen_server:call(?MODULE, {get_taguchi_array, Scheme}).

%%
%% TODO doc this
get_stats() ->
	gen_server:call(?MODULE, {get_stats}).

%%
%% TODO doc this
add_array(Scheme, Array) ->
	gen_server:call(?MODULE, {add_array, Scheme, Array}).

%% TODO doc this
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% TODO doc this
%%
init([]) ->
	TableId = ets:new(taguchi, [set]),
	case file:consult("src/standard_taguchi_arrays.data") of
		% TODO report this in a better way
		{error, _} -> 
			io:format("Cannot read standard taguchi arrays file.");
		{ok, [Arrays]} -> 
			lists:foreach(fun({Scheme, Combinations}) -> 
				ets:insert(TableId, {Scheme, Combinations}) 
			end, 
			Arrays)
	end,
    {ok, #state{table=TableId}}.

handle_call({arrange_array, Scheme, Sections}, _From, State) ->
    Reply = case ets:lookup(State#state.table, Scheme) of
        [] -> undefined;
        [{Sch, Array}] ->
            % BEWARE, this is probably this most cryptic code in optimyser.
            % Be very careful while editing this parts and related internal
            % functions.
            Scheme1 = scheme_to_list(Scheme),
            Sections1 = lists:map(fun([_, Vars]) -> length(Vars) end, 
                                 Sections),
            Indices = get_indices([S || S <- Sections1, S =/= 1], Scheme1),
            lists:map(fun(Row) ->
                make_array(Sections1, Indices, Row)
            end, Array)
    end,
    {reply, Reply, State};

handle_call({is_taguchi, Scheme}, _From, State) ->
	Reply = case ets:lookup(State#state.table, Scheme) of
		[] -> false;
		[_Val] -> true
	end,
	{reply, Reply, State};

handle_call({get_taguchi_array, Scheme}, _From, State) ->
	Reply = case ets:lookup(State#state.table, Scheme) of
		[] -> undefined;
		[{Scheme, Combinations}] -> {value, Combinations}
	end,
	{reply, Reply, State};
	
handle_call({get_stats}, _From, State) ->
	Schemes = lists:map(fun({Scheme,_Combs}) ->
		lists:map(fun erlang:tuple_to_list/1, tuple_to_list(Scheme))
		end,
		ets:tab2list(State#state.table)),
	{reply, Schemes, State};

handle_call({add_array, Scheme, Array}, _From, State) ->
	% TODO check validity
	ets:insert(State#state.table, {Scheme, Array}),
	{reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internal fuck
make_array(Sections, Indices, Row) ->
    make_array(Sections, Indices, Row, []).

make_array([1|Sections], Indices, Row, Acc) ->
    make_array(Sections, Indices, Row, [1|Acc]);
make_array([S|Sections], [I|Indices], Row, Acc) ->
    make_array(Sections, Indices, Row, [lists:nth(I,Row)|Acc]);
make_array(_,_,_,Acc) ->
    lists:reverse(Acc).

scheme_to_list(Scheme) ->
    lists:flatten(
        lists:map(
            fun({N,M}) -> lists:duplicate(M,N) end, tuple_to_list(Scheme))).

get_indices(Sections, Scheme) ->
    get_indices(Sections, Scheme, dict:new(), []).

get_indices([S|Sections], Scheme, Dict, Acc) ->
    case dict:find(S, Dict) of
        error ->
            Pos = get_positions(S, Scheme),
            get_indices(Sections, Scheme,
                        dict:store(S, {Pos, 1}, Dict),
                        [hd(Pos)|Acc]);
        {ok, {Pos, Count}} ->
            get_indices(Sections, Scheme,
                dict:store(S, {Pos, Count + 1}, Dict),
                [lists:nth(Count + 1, Pos)|Acc])
    end;
get_indices([], _, _, Acc) ->
    lists:reverse(Acc).

get_positions(Elt, List) ->
    get_positions(Elt, List, [], 1).
get_positions(Elt, [Elt|T], Acc, N) ->
    get_positions(Elt, T, [N|Acc], N+1);
get_positions(Elt, [_H|T], Acc, N) ->
    get_positions(Elt, T, Acc, N+1);
get_positions(_, [], Acc, _) ->
    lists:reverse(Acc).
