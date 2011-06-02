-module(optimyser_logger).

-behaviour(gen_server).
-include("optimyser.hrl").

-record(state, {db_handle, log_table}).

%% API
-export([start_link/0]).
-export([monitor_logs/0, update_impression/1, log_test_page/4, update_combination_visit/2]).
-export([log_goal_page/4, update_conversion/2]).

% debug
-export([export_to_mysql/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

export_to_mysql(Log) ->
    mochifmt:bformat("{0}").

%% 
%% 
monitor_logs() ->
	gen_server:cast(?MODULE, {monitor_logs}).

%% @spec update_impression(experiment()) -> ok
%% @doc Increments the total impression count for the experiment
update_impression(Exp) ->
    gen_server:call(?MODULE, {update_impression, Exp}).
    
%% @spec log_test_page(experiment(), combination, string(), string()) -> ok
%% @doc Log landing page visit to database.
log_test_page(Exp, Combination, VisitorId, VisitorIp) ->
    gen_server:call(?MODULE, {log_test_page, Exp, Combination, VisitorId, VisitorIp}).

%% @spec log_goal_page(experiment(), combination, string(), string()) -> ok
%% @doc Log goal page visit to database.
log_goal_page(Exp, Combination, VisitorId, VisitorIp) ->
    gen_server:call(?MODULE, {log_goal_page, Exp, Combination, VisitorId, VisitorIp}).

%% TODO doc this
%%
update_combination_visit(Exp, Combination) ->
    gen_server:call(?MODULE, {update_combination_visit, Exp, Combination}).

%% TODO doc this
%%
update_conversion(Exp, Combination) ->
    gen_server:call(?MODULE, {update_conversion, Exp, Combination}).

%% TODO doc this
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% TODO doc this
%%
init([]) ->
    % TODO make this configurable
    % TODO what if connection fails?
    {ok, _} = mysql:start_link(optimyser,
        "localhost",
        "root",
        "haldun",
        "optimyser"
    ),
    % mysql:start_link(optimyser,
    %     optimyser_config:get_option(mysql_server),
    %     optimyser_config:get_option(mysql_user),
    %     optimyser_config:get_option(mysql_password),
    %     optimyser_config:get_option(mysql_database)),
    {ok, #state{db_handle=optimyser, log_table=optimyser:log_table_name()}}.

handle_call({update_impression, Exp}, _From, State) ->
    Reply = mnesia:dirty_update_counter(impression, Exp#experiment.id, 1),
    {reply, Reply, State};

handle_call({log_test_page, Exp, Combination, VisitorId, VisitorIp}, _From, State) ->
    Reply = log_page(State#state.log_table, Exp, Combination, VisitorId, VisitorIp, test),
    {reply, Reply, State};

handle_call({log_goal_page, Exp, Combination, VisitorId, VisitorIp}, _From, State) ->
    Reply = log_page(State#state.log_table, Exp, Combination, VisitorId, VisitorIp, goal),
    {reply, Reply, State};

handle_call({update_combination_visit, Exp, Combination}, _From, State) ->
    Reply = mnesia:dirty_update_counter(visit, {Exp#experiment.id, Combination}, 1),
    {reply, Reply, State};
    
handle_call({update_conversion, Exp, Combination}, _From, State) ->
    Reply = mnesia:dirty_update_counter(conversion, {Exp#experiment.id, Combination}, 1),
    {reply, Reply, State}.

handle_cast({monitor_logs}, State) ->
    try carry_logs(State#state.log_table) 
    catch
        _:_ -> ok
    end,
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
log_page(LogTable, Exp, Combination, VisitorId, VisitorIp, PageType) ->
	Log = #log{
	        id={calendar:local_time(), now(), node()},
	        user=Exp#experiment.user,
	        expid=Exp#experiment.id, 
	        combination_id=Combination, 
	        visitor_id=VisitorId, 
	        visitor_ip=VisitorIp,
	        date={calendar:local_time(), now()},
	        page_type=PageType
	    },
	mnesia:dirty_write(LogTable, Log),
    ok.

carry_logs(LogTableName) ->
    MatchHead = #log{_='_'},
    Guard = [],
    Result = ['$_'],
    MatchSpec = [{MatchHead, Guard, Result}],
    % TODO hardcoded value 
    % TODO make transactional
    SelFun = fun() ->
        mnesia:select(LogTableName, MatchSpec, 5000, read)
    end,
    {Logs, _} = mnesia:activity(async_dirty, SelFun, []),
    Query = prepare_query(Logs),
    mysql:fetch(optimyser, Query),
    lists:foreach(fun(Record) -> mnesia:dirty_delete_object(LogTableName, Record) end, Logs),
    length(Logs).

prepare_query(Logs) ->
    Formatter = fun(Record) ->
        mochifmt:format("('{0}','{1}','{2}','{3}','{4}','{5}','{6}')",
            [
            list_to_binary(integer_to_list(Record#log.user)),
            Record#log.expid,
            list_to_binary(optimyser_util:export_combid(Record#log.combination_id)),
            list_to_binary(Record#log.visitor_id),
            list_to_binary(Record#log.visitor_ip),
            mochifmt:bformat("{0.0}-{0.1}-{0.2} {1.0}:{1.1}:{1.2}", element(1, Record#log.date)),
            list_to_binary(atom_to_list(Record#log.page_type))
            ])
    end,
    S = optimyser_util:string_join(lists:map(Formatter, Logs), $,),
    mochifmt:bformat("INSERT INTO log (user_id, experiment_id, combination_id, "
         "visitor_id, visitor_ip, created_on, page_type) VALUES {0}", 
         [S]).