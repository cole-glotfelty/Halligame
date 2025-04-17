-module(registerclient).

-export([join/0]).

-define(SERVER, {serverbroker, 'serverbroker@vm-projectweb3.eecs.tufts.edu'}).
-define(ME, string:trim(os:cmd("whoami"))).
% TODO: doc
join() ->
    % erlang:display(?ME).
    Reply = gen_server:call(?SERVER, {add_self, ?ME}),
    io:format("Got reply ~p~n", [Reply]).
