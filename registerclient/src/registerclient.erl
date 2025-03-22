-module(registerclient).

-export([join/0]).

-define(SERVER, {serverbroker, 'serverbroker@vm-projectweb3'}).

% TODO: doc
join() ->
    Reply = gen_server:call(?SERVER, {add_self}),
    io:format("Got reply ~p~n", [Reply]).