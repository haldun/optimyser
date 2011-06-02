%% @author hb <hbayhantopcu@gmail.com>
%% @copyright 2008 hb.

%% @doc Web server for optimyser.

-module(optimyser_web).
-author('hb <hbayhantopcu@gmail.com>').

-export([start/1, stop/0, loop/2]).

-include("optimyser.hrl").

-define(GET_KEY_EXPERIMENT, "e").
-define(GET_KEY_VISITORID, "u").
-define(GET_KEY_COMBINATION, "c").
-define(GET_KEY_PREVIEW, "p").

%% External API

%% @doc Starts optimyser web server.
start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

%% @doc Stops optimyser web server
stop() ->
    mochiweb_http:stop(?MODULE).

%% @doc Main web server loop
loop(Req, DocRoot) ->
    {Method, Path, Req} = {Req:get(method), Req:get(path), Req},
    try handle_request(Method, Path, Req)
    catch
        % TODO report server errors
        throw:not_found -> 
            Req:respond({404, [], <<"{\"error\": \"not found\"}">>});
        throw:empty_req -> 
            Req:respond({200, [], " "});
        throw:server_error -> 
			Req:respond({500, [], <<"{\"error\": \"server error\"}">>});
        throw:permission_denied -> 
            Req:respond({401, [], <<"{\"error\": \"permission denied\"}">>});
        throw:invalid_input -> 
            Req:respond({200, [], <<"{\"error\": \"invalid parameters\"}">>})
    end.
    
%% Url handlers
%% Admin actions
handle_request('GET', "/admin/info", Req) ->
    Exp = ensure_experiment(Req),
    Req:ok({"text/javascript", mochifmt:bformat("{0}", [Exp])});
    
handle_request(_, "/admin/array_list/", Req) ->
    Stats = optimyser_taguchi:get_stats(),
    Req:ok({"text/javascript", io_lib:format("~p", [Stats])});

% TODO make this handle work for only POST
handle_request(_, "/admin/create/", Req) ->
    % decode json representation of experiment to erlang representation
    Exp = optimyser_experiment:decode(Req:recv_body()),
    case Exp of
        {error, _Why} -> throw(invalid_input);
        _ -> ok
    end,
    % compute some needed values for the experiment 
    Exp1 = optimyser_experiment:init_experiment(Exp),
    case optimyser_experiment:save_experiment(Exp1) of
        ok -> Req:ok({"text/html", optimyser_experiment:encode(Exp1)});
        % TODO report this error
        {error, Why} -> 
			io:format("~p~n", [Why]),
			throw(server_error)
    end;

handle_request(_, "/admin/pause", Req) ->
    Exp = ensure_experiment(Req),
    Exp1 = Exp#experiment{status=paused},
    optimyser_experiment:save_experiment(Exp1),
    Req:ok({"text/javascript", "\"ok\""});

handle_request(_, "/admin/start", Req) ->
    Exp = ensure_experiment(Req),
    Exp1 = Exp#experiment{status=running},
    optimyser_experiment:save_experiment(Exp1),
    Req:ok({"text/javascript", "\"ok\""});

handle_request(_, "/admin/enable_fractional", Req) ->
    Exp = ensure_experiment(Req),
    Exp1 = Exp#experiment{taguchi=true},
    optimyser_experiment:save_experiment(Exp1),
    Req:ok({"text/javascript", "\"ok\""});
    
handle_request(_, "/admin/disable_fractional", Req) ->
    Exp = ensure_experiment(Req),
    Exp1 = Exp#experiment{taguchi=false},
    optimyser_experiment:save_experiment(Exp1),
    Req:ok({"text/javascript", "\"ok\""});

handle_request(_, "/admin/stop", Req) ->
    Exp = ensure_experiment(Req),
    Combination = get_selected_combination(Req, Exp),
    case Combination of 
        false -> throw(invalid_input);
        _ -> ok
    end,
    Exp1 = Exp#experiment{status=stopped, preferred_combination=Combination},
    mnesia:transaction(fun() -> mnesia:write(Exp1) end),
    Req:ok({"text/javascript", "\"ok\""});

% TODO change the method to POST
handle_request(_, "/admin/taguchi/add", Req) ->
	% {"scheme": [[2,2]], "array": [[2,3,1,3], [1,4,2,1]]}
	% io:format("~p~n", [mochijson2:decode(Req:recv_body())])
	Req:ok({"text/javascript", "not implemented"});

%% Test urls which are called with embedded javascripts in user's page
%% Landing page url
handle_request('GET', "/t", Req) ->
    Exp = ensure_experiment(Req),
    SelectedCombIndex = get_selected_combination(Req, Exp),
    Response = 
    case get_param(?GET_KEY_PREVIEW, Req, get) of 
        {value, _} ->
            % This is a preview 
            case SelectedCombIndex of 
                false -> throw(empty_req);
                _ -> optimyser_experiment:test_page_js(Exp, SelectedCombIndex, 
                                                       <<"previewer">>)
            end;
        false ->
            if Exp#experiment.status == running ->
                % filter user if necessary
                case Exp#experiment.amount_of_traffic =/= 100 andalso
                 crypto:rand_uniform(0, 100) > Exp#experiment.amount_of_traffic - 0.5 of
                     true -> throw(empty_req);
                     false -> ok
                end,
                % tag visitors by unique ids
                VisitorId = case get_param(?GET_KEY_VISITORID, Req, get) of 
                    {value, []} -> optimyser_util:export_guid();
                    {value, _ValueId} -> _ValueId;
                    false -> optimyser_util:export_guid()
                end,
                % pick a combination for user
                Combination = case SelectedCombIndex of
                    false -> optimyser_experiment:pick_combination(Exp);
                    CombId -> CombId
                end,
                % if this is the first time, log this as a visit to test page.
                case SelectedCombIndex of 
                    false ->
                        optimyser_logger:log_test_page(
                            Exp, 
                            Combination, 
                            VisitorId, 
                            Req:get(peer));
                    _ -> ok
                end,
                % select the appropriate js and send to the user
                case get_param("ab", Req, get) of
                    false -> optimyser_experiment:test_page_js(
                                Exp,
                                Combination,
                                VisitorId);
                    _ -> optimyser_experiment:test_page_js_ab(
                                Exp,
                                Combination,
                                VisitorId)
                end;
            Exp#experiment.status == stopped ->
                case Exp#experiment.preferred_combination of
                    nil ->
                        io:format("we shouldn't be here. report this~n"),
                        throw(empty_req);
                    _ -> ok
                end,
                case get_param("ab", Req, get) of
                    false -> optimyser_experiment:test_page_js(
                        Exp,
                        Exp#experiment.preferred_combination,
                        "stopped");
                    _ -> optimyser_experiment:test_page_js_ab(
                        Exp,
                        Exp#experiment.preferred_combination,
                        "stopped")
                end;
            true ->
                throw(empty_req)
            end
        end,
    Req:ok({"text/javascript", Response});

%% Goal page url
handle_request('GET', "/g", Req) ->
    % get visitor id. visitor id is set in the test page, so if we haven't 
    % meet any visitor id here, then just drop it now.
    VisitorId = case get_param(?GET_KEY_VISITORID, Req, get) of 
        {value, _ValueId} -> _ValueId;
        false -> throw(empty_req)
    end,
    % get experiment
    Exp = ensure_experiment(Req),
    % get combination. if no combination value is given (which is not likely 
    % supposed to be), just drop the request.
    Combination = get_selected_combination(Req, Exp),
    case Combination of
        false -> throw(empty_req);
        _ -> ok
    end,
    % Now, it is time to update our stats.
    case Exp#experiment.status == running of
        true -> optimyser_logger:log_goal_page(Exp, Combination, VisitorId, 
                                               Req:get(peer));
        false -> throw(empty_req)
    end,
    Req:ok({"text/javascript", "   "});

%% All other urls give 404
handle_request(_, _, _) ->
    throw(not_found).

%% Internal API

%% @spec ensure_experiment(request()) -> experiment()
%% @doc Ensures that the request is done via a valid experiment id. If not,
%%      it throws empty_req or not_found exceptions.
ensure_experiment(Req) ->
    ExpId = case get_param(?GET_KEY_EXPERIMENT, Req, get) of 
        false -> throw(not_found); 
        {value, V} -> V 
    end,
    Exp = case optimyser_experiment:get_experiment(ExpId) of
        {value, _Exp} -> _Exp;
        false -> throw(not_found)
    end,
    Exp.

%% TODO doc this
%%
get_selected_combination(Req, Exp) ->
    SelectedCombIndex = case get_param(?GET_KEY_COMBINATION, Req, get) of
        {value, SelectedCombId1} ->
            case optimyser_util:import_combid(SelectedCombId1) of
                {value, Value} -> 
                    case lists:member(Value, Exp#experiment.combinations) of
                        true -> Value;
                        false -> false
                    end;
                false -> false
            end;
        false -> false
    end.

%% @spec get_param(string(), request, get | post) -> {value, string()} | false
%% @doc Get the parameter given from GET or POST
get_param(Key, Req, get) ->
    get_param_int(Key, Req:parse_qs());
get_param(Key, Req, post) ->
    get_param_int(Key, Req:parse_post()).

get_param_int(Key, Array) ->
    case lists:keysearch(Key, 1, Array) of
        false -> false;
        {value, {_Key, Val}} -> {value, Val}
    end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
