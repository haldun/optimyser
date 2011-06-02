%% @author hb <hbayhantopcu@gmail.com>
%% @copyright 2008 hb

%% @doc TEMPLATE.

-module(optimyser).
-author('hb <hbayhantopcu@gmail.com>').
-export([start/0, stop/0, log_table_name/0, create_log_table/0]).
-include("optimyser.hrl").

%% 
%% @doc Ensure that an app is started already.
ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.
        
%% @spec start() -> ok
%% @doc Start the optimyser server.
start() ->
    optimyser_deps:ensure(),
    ensure_started(crypto),
    initdb(),
    % optimyser_config:start(),
    % TODO remove these. put a file for initial config values. 
    application:start(optimyser).

%% @spec stop() -> ok
%% @doc Stop the optimyser server.
stop() ->
    Res = application:stop(optimyser),
    application:stop(crypto),
    Res.

%% @spec initdb() -> ok
%% @doc Initialize mnesia tables and starts mnesia.
initdb() ->
    case mnesia:system_info(extra_db_nodes) of
        [] -> mnesia:create_schema([node()]);
        _  -> ok
    end,
    mnesia:start(),
    % experiment table
    mnesia:create_table(experiment, [
        {disc_copies, [node()]}, {attributes, record_info(fields, experiment)}
    ]),
    % last selected combination index of the experiments
    mnesia:create_table(last_selected, [
        {ram_copies, [node()]}, {attributes, record_info(fields, last_selected)}
    ]),
    % impression table
    mnesia:create_table(impression, [
        {ram_copies, [node()]}, {attributes, record_info(fields, impression)}
    ]),
    create_log_table(),
    % configuration table
    mnesia:create_table(config, [
        {disc_copies, [node()]}, {attributes, record_info(fields, config)}
    ]),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
    % some memory leakage workaround for mnesia
    mnesia_recover:start_garb(),
    mnesia_recover:allow_garb(),
    ok.

create_log_table() ->
    % log table 
    % NOTE that, log table is local to each node (or node pair). So we must
    % invent a new table name for each node in the network.
    mnesia:create_table(log_table_name(), [
        {ram_copies, [node()]}, 
        {record_name, log},
        {attributes, record_info(fields, log)}
    ]).

log_table_name() ->
    list_to_atom(atom_to_list(node()) ++ "_log" ).
