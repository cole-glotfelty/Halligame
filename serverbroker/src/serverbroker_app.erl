%%%-------------------------------------------------------------------
%% @doc serverbroker public API
%% @end
%%%-------------------------------------------------------------------

-module(serverbroker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    serverbroker_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
