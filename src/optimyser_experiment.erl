-module(optimyser_experiment).
-export([get_experiment/1, save_experiment/1, init_experiment/1]).
-export([pick_combination/1, get_stats_json/1]).
-export([test_page_js/3, test_page_js_ab/3, get_stats/1]).
-export([encode/1, decode/1]).

%% TODO remove this in production code
-compile(export_all).

-include("optimyser.hrl").

%% @type experiment(). Optimyser experiment object
%% @type json(). A valid JSON string

%% @spec pick_combination(experiment()) -> combination
%% @doc Pick a combination for given experiment. This function is loyal to
%%      Taguchi methods.
pick_combination(Exp) ->
    case Exp#experiment.taguchi of
        true  -> 
        case Exp#experiment.taguchi_array of
            undefined -> pick_full(Exp);
            _ -> pick_taguchi(Exp)
        end;
        false -> pick_full(Exp)
    end.

%% TODO doc this
%% 
pick_taguchi(Exp) ->
    TaguchiArray = Exp#experiment.taguchi_array,
    LastSelected = update_last_selected(Exp, length(TaguchiArray)),
    lists:nth(LastSelected + 1, TaguchiArray).

%% TODO doc this
%%
pick_full(Exp) ->
    LastSelected = update_last_selected(Exp, Exp#experiment.number_of_combinations),
    RandomIndex = lists:nth(LastSelected + 1, Exp#experiment.shuffled_combinations),
    lists:nth(RandomIndex, Exp#experiment.combinations).

update_last_selected(Exp, Max) ->
    % Get last selected combination index
    LastSelected = case mnesia:dirty_select(last_selected, [{
            #last_selected{expid=Exp#experiment.id, _='_'},[],['$_']}]) of
        [Val] -> Val#last_selected.index;
        [] -> 1
    end,
    LastSelected1 = (LastSelected + 1) rem Max,
    % Update last selected value
    % TODO we're updating this value in a nontransactional way for speed. May we
    % misusing something? Can this be an error?
    mnesia:dirty_write(#last_selected{expid=Exp#experiment.id, index=LastSelected1}),
    LastSelected1.

%% @spec get_experiment(string()) -> {value, experiment()} | false
%% @doc Retrieves the experiment with given id
get_experiment(Id) when is_list(Id) ->
    get_experiment_int(list_to_binary(Id)).

get_experiment_int(Id) ->
    case ets:lookup(experiment, Id) of
        [Exp] -> {value, Exp};
        _ -> false
    end.

%% @spec save_experiment(experiment()) -> ok | {error, Reason}
%% @doc Save the given experiment to mnesia table.
save_experiment(Exp) ->
    case mnesia:transaction(fun() -> mnesia:write(Exp) end) of
        {aborted, Reason} -> {error, Reason};
        {atomic, _Result} -> ok
    end.

get_stats_json(Exp) ->
    {Visits, Conversions} = get_stats(Exp),
    F = fun(#visit{id={ExpId1,Comb1},count=Count1},
            #conversion{id={ExpId2,Comb2},count=Count2}) ->
        [Comb1,Count1,Count2]
    end,
    mochijson2:encode({struct, [{combinations, lists:zipwith(F, Visits, Conversions)}]}).

%% TODO doc this
%%
% get_stats(ExpId) ->
%     mnesia:dirty_select(visit, [{
%         #visit{id={Id,'_'}, _='_'}, [], ['$_']}]).
get_stats(Exp) ->
	Visits = mnesia:dirty_select(visit, 
	    [{ #visit{id={Exp#experiment.id,_='_'}, _='_'}, [], ['$_'] }]),
	Conversions = mnesia:dirty_select(conversion, 
	    [{ #conversion{id={Exp#experiment.id,_='_'}, _='_'}, [], ['$_'] }]),
	{lists:sort(Visits), lists:sort(Conversions)}.

%% @spec init_experiment(experiment()) -> experiment()
%% @doc Create combinations, scheme and determines if the experiment is 
%%      applicable to taguchi method.
init_experiment(Exp) ->
    Sections = Exp#experiment.sections,
    Combinations = gen_combinations(Sections),
    Scheme = gen_scheme(Exp),
	NumberOfCombinations = length(Combinations),
	TaguchiArray = optimyser_taguchi:arrange_array(Scheme, Sections),
    Exp#experiment{combinations=Combinations,
        sections=Sections,
		shuffled_combinations=optimyser_util:shuffle(
		    lists:seq(1, NumberOfCombinations)),
        number_of_combinations=NumberOfCombinations, 
        scheme=Scheme, 
        taguchi_array=TaguchiArray}.

%% @spec test_page_js(experiment(), combination, string()) -> string()
%% @doc Return the test page javascript code for given experiment and combination
test_page_js(Exp, Combination, VisitorId) ->
    Values = amalgamate_combination(Exp, Combination),
    % Note that in js code use {{ instead of {.
    mochifmt:bformat("(function(){{" 
    "function createCookie(name,value,days) {{"
    "if (days) {{"
    "	var date = new Date();"
    "	date.setTime(date.getTime()+(days*24*60*60*1000));"
    "	var expires = \"; expires=\"+date.toGMTString();"
    "}}"
    "else var expires = \"\";"
    "document.cookie = name+\"=\"+value+expires+\"; path=/\";"
    "}}"
    "createCookie('{0}', '{1}', 700);"
    "createCookie('{0}_v', '{2}', 700);"
    "}})();"
    "function optimyser_goal() {{"
    "    function rck(n) {{"
    "        var a = n + \"=\";"
    "        var ca = document.cookie.split(';');"
    "        for (var i = 0; i < ca.length; i++) {{"
    "            var c = ca[i];"
    "            while (c.charAt(0) == ' ') c = c.substring(1, c.length);"
    "            if (c.indexOf(a) == 0) return c.substring(a.length, c.length);"
    "        }}"
    "        return null;"
    "    }}"
    "    var l = document.location;"
    "    var pr = l.protocol;"
    "    var jp = \"http://app.optimyser.com/g\";"
    "    var jep = pr == 'https:' ? jp.replace('http', 'https') : jp;"
    "    var e = '{0}';"
    "    var sc = rck(e);"
    "    var sc_i = rck(e + '_i');"
    "    var u = rck(e + '_v');"
    "    var sg = rck(e + '_sg');"
    "    function mu(dc) {{"
    "        var r = '';"
    "        for (var k in dc) {{"
    "            r += k + '=' + dc[k] + '&';"
    "        }}"
    "        return r;"
    "    }}"
    "    var d = {{"
    "        \"e\": e,"
    "        \"u\": u,"
    "        \"c\": sc,"
    "        \"ci\": sc_i"
    "    }};"
    "    createCookie(e + '_sg', '1');"
    "    if (sc != null && !sg) {{"
    "        document.write('<sc' + 'ript src=\"' + jep + '?' + mu(d) + '\" type=\"text/javascript\" charset=\"utf-8\">' + '</sc' + 'ript>');"
    "    }}"
    "}};"
    "function opt_section(a){{"
    "var r={3};"
    "if(r[a]){{"
    "document.write(r[a]);"
    "document.write('<nosc'+'ript>');"
    "}}"
    "}}", [
        Exp#experiment.id,
        optimyser_util:export_combid(Combination),
        VisitorId,
        Values
    ]).

test_page_js_ab(Exp, Combination, VisitorId) ->
    [Section|_] = Exp#experiment.sections,
    [_,Vars] = Section,
    [Var|_] = Combination,
    % Values = amalgamate_combination(Exp, Combination),
    % Note that in js code use {{ instead of {.
    mochifmt:bformat("(function(){{" 
    "function createCookie(name,value,days) {{"
    "if (days) {{"
    "	var date = new Date();"
    "	date.setTime(date.getTime()+(days*24*60*60*1000));"
    "	var expires = \"; expires=\"+date.toGMTString();"
    "}}"
    "else var expires = \"\";"
    "document.cookie = name+\"=\"+value+expires+\"; path=/\";"
    "}}"
    "createCookie('{0}', '{1}', 700);"
    "createCookie('{0}_v', '{2}', 700);"
    "var loc='{3}';"
    "if(document.location.href!=loc){{"
    "document.location.href='{3}';"
    "document.write('<nosc'+'ript>');}}"
    "}})();"
    ""
    , [
        Exp#experiment.id,
        optimyser_util:export_combid(Combination),
        VisitorId,
        lists:nth(Var, Vars)
    ]).

amalgamate_combination(Exp, CombId) ->
    mochijson2:encode({struct, 
        amalgamate_combination(CombId, Exp#experiment.sections, [])}).

amalgamate_combination([I|Indexes], [[Name,Vars]|Sections], Acc) ->
    amalgamate_combination(Indexes, Sections, [{Name, lists:nth(I,Vars)}|Acc]);
amalgamate_combination([], [], Acc) ->
    Acc.

%% @spec encode(experiment()) -> json()
%% @doc Encode the given experiment to json string.
encode(Exp) ->
    Scheme = lists:map(fun erlang:tuple_to_list/1, tuple_to_list(Exp#experiment.scheme)),
    mochijson2:encode({struct, [
        {id, Exp#experiment.id},
        {name, Exp#experiment.name},
		{user, Exp#experiment.user},
        {sections, Exp#experiment.sections},
        {status, Exp#experiment.status},
        {number_of_combinations, length(Exp#experiment.combinations)},
        {combinations, Exp#experiment.combinations},
        {scheme, Scheme},
        {taguchi, Exp#experiment.taguchi}
    ]}).

%% @spec decode(json()) -> experiment()
%% @doc Decodes a json string to optimyser experiment record.
decode(Json) ->
    try decode_int(Json)
    catch
        _:Why -> {error, Why}
    end.

%% Internal functions
%% TODO add more validation tests to this function
decode_int(Json) ->
    {struct, Val} = mochijson2:decode(Json),
    {<<"id">>, Id} = proplists:lookup(<<"id">>, Val),
    {<<"name">>, Name} = proplists:lookup(<<"name">>, Val),
	{<<"user">>, User} = proplists:lookup(<<"user">>, Val),
    {<<"sections">>, Sections} = proplists:lookup(<<"sections">>, Val),
    {<<"status">>, Status} = proplists:lookup(<<"status">>, Val),
    {<<"taguchi">>, Taguchi} = proplists:lookup(<<"taguchi">>, Val),
    {<<"amount_of_traffic">>, Amount} = proplists:lookup(<<"amount_of_traffic">>, Val),
    #experiment{id=Id, name=Name, user=User, sections=Sections, 
                amount_of_traffic=Amount,
                taguchi=Taguchi,
                status=list_to_atom(binary_to_list(Status))}.

%% @spec gen_combinations(experiment()) -> combinations
%% @doc Generates the combinations of the given experiment
gen_combinations(Sections) ->
    % generate combinations
    Combinations = sofs:to_external(
        sofs:product(list_to_tuple(
            lists:map(fun ([_|[R]]) -> sofs:set(lists:seq(1,length(R))) end, 
            Sections)))),
    lists:map(fun erlang:tuple_to_list/1, Combinations).

%% @spec gen_scheme(Exp) -> scheme
%% @doc Generates the scheme of the experiment. 
%%      TODO define what a scheme is.
gen_scheme(Exp) ->
    list_to_tuple(gen_scheme(
        lists:sort([length(Vars) || [_,Vars] <- Exp#experiment.sections]),
        dict:new())).

gen_scheme([Count|Tail], Dict) ->
    gen_scheme(Tail, dict:update_counter(Count, 1, Dict));
gen_scheme(_, Dict) ->
    lists:sort([X || X <- dict:to_list(Dict), element(1,X) =/= 1]).
