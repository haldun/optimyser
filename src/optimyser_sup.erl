%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the optimyser application.

-module(optimyser_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("OWS_IP") of false -> "0.0.0.0"; Any -> Any end,
    Port = case os:getenv("OWS_PORT") of false -> 8080; _Val -> _Val end,
    WebConfig = [
         {ip, Ip},
         {port, Port},
         {docroot, optimyser_deps:local_path(["priv", "www"])}],
    Web = {optimyser_web,
           {optimyser_web, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
    Logger = {optimyser_logger,
        {optimyser_logger, start_link, []}, 
		permanent, 5000, worker, dynamic},
	Taguchi = {optimyser_taguchi,
		{optimyser_taguchi, start_link, []},
		permanent, 5000, worker, dynamic},
	Cron = {cron, {cron, start_link, [
			[{monitor_logs, fun optimyser_logger:monitor_logs/0, 10}] % seconds
		]},
		permanent, 5000, worker, dynamic},
        %     Amqp = {optimyser_amqp,
        %         {optimyser_amqp, start_link, []}, 
        % permanent, 5000, worker, dynamic},
    Processes = [Web, Logger, Taguchi, Cron],
    {ok, {{one_for_one, 10, 10}, Processes}}.
