%% .erl
%% Class: Concurrent Programming, Spring 2025
%% the basic idea here is that if you run this without the shell, then this can communicate with the client.erl process that each user automatically runs on signin. client.erl can do further processing and contact the main server broker if necessary
%% assumes some information about the locations of games as well as the location of the clienter.erl process in the defines at the top of the module

%% run command: erl -pa ebin -noshell -eval "handleCLIRequest:newGame(chess)"

-module(handleCLIRequest).
-export([newGame/1]).

-define(GameDir, "games/").
-define(ClientServer, client).
-define(ClientNode, 'server@vm-hw06')

newGame(GameName) ->
    {ok, Games} = file:list_dir(?GameDir),
    GameNameString = atom_to_list(GameName),
    case lists:member(GameNameString, Games) of
        true -> {?ClientServer, ?ClientNode} ! filename:join(?GameDir, GameName),
                ok;
        false -> io:format("Game ~p not found.~n", [GameName]),
                 error
    end,
    init:stop().
