%%%-------------------------------------------------------------------
%% @doc serverbroker public API
%% @end
%%%-------------------------------------------------------------------

-module(serverbroker_app).

-behaviour(application).

-export([start/2, start/0, stop/1, stop/0]).

start() ->
    serverbroker_sup:start_link().

start(_StartType, _StartArgs) ->
    start().

stop() ->
    serverbroker_sup:stop().

stop(_State) ->
    stop().
%% internal functions
