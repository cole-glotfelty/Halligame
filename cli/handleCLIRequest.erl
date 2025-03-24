%% .erl
%% Class: Concurrent Programming, Spring 2025
%% the basic idea here is that if you run this without the shell, then this can communicate with the client.erl process that each user automatically runs on signin. client.erl can do further processing and contact the main server broker if necessary
%% assumes some information about the locations of games as well as the location of the clienter.erl process in the defines at the top of the module

%% one weird thing to note is that every function needs to end with 
%% "init:stop()" to terminate the process

%% run command: erl -pa ebin -noshell -eval "handleCLIRequest:newGame(chess)"

%% TODO: investigate how bad using erlang:halt() is (instead of init:stop()) --> it's much much faster on shutdown, but also does an unclean shutdown which might cause problems.


-module(handleCLIRequest).
-export([help/0, newGame/1, joinGame/1, listGames/0]).

-define(GameDir, "games/").
-define(ClientServer, client).
-define(ClientNode, 'server@vm-hw06').

% TODO: finish writing usage
help() ->
    io:format("Usage:...\n"),
    init:stop().

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

%% TODO
joinGame(GameID) ->
    io:format("~p\n", [GameID]),
    init:stop().

%% TODO
listGames() ->
    io:format("(TODO) List games\n"),
    init:stop().

% % optional
% addFriend(FriendID) ->
%     init:stop().

% % optional
% removeFriend(FriendID) ->
%     init:stop().

% % this one is also a maybe. The idea is that this could be used to stop the 
% % register server if the user wants. This function might also be required for 
% % stopping the server on logout.
% stopRegisterServer() ->
%     init:stop().
