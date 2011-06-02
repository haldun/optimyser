%% @author hb <hbayhantopcu@gmail.com>
%% @copyright 2008 hb.

%% @doc Callbacks for the optimyser application.

-module(optimyser_app).
-author('hb <hbayhantopcu@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for optimyser.
start(_Type, _StartArgs) ->
    optimyser_deps:ensure(),
    optimyser_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for optimyser.
stop(_State) ->
    ok.
