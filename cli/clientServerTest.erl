%% .erl
%% Class: Concurrent Programming, Spring 2025

-module(clientServerTest).
-export([registerMyself/0]).


% Print out all messages received
waitCycle() ->
    receive
        stop -> init:stop();
        Message -> io:format("~p~n", [Message]),
                   waitCycle()
    end.

registerMyself() ->
    register(regclient, self()),
    waitCycle().
