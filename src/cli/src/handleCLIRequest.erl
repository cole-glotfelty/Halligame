%% .erl
%% Class: Concurrent Programming, Spring 2025
%% the basic idea here is that if you run this without the shell, then this can communicate with the client.erl process that each user automatically runs on signin. client.erl can do further processing and contact the main server broker if necessary
%% assumes some information about the locations of games as well as the location of the clienter.erl process in the defines at the top of the module

%% one weird thing to note is that every function needs to end with 
%% "init:stop()" to terminate the process

%% run command: erl -pa ebin -noshell -eval "handleCLIRequest:newGame(chess)"

%% TODO: investigate how bad using erlang:halt() is (instead of init:stop()) --> it's much much faster on shutdown, but also does an unclean shutdown which might cause problems.


-module(handleCLIRequest).
-export([help/0, listGames/0, listActiveGames/0]).

-define(GameDir, "/h/wcordr01/cs21/final_project/Halligame/src/halligame/games/"). % the directory of games. TODO: change
-define(AvailableGames, "availableGames"). % the directory of games. TODO: change
-define(ME, string:trim(os:cmd("whoami"))).
-define(VM, os:getenv("HOST")).

% TODO: if the same user has multiple sessions signed in on the same VM, this won't be unique 
%       --> solution: when registering the registerClient process, instead of registering it under 'regclient', instead register it under the UTLN and the outgoing port of the client (second part of SSH_CONNECTION in the env). This still doesn't handle same user from multiple machines happening to use the same port, but that's definitely an edge case. To fix, maybe also include IP address?
-define(ClientRegisteredName, regclient).
-define(ClientNode, list_to_atom(?ME ++ "@" ++ ?VM)).

-define(SERVERBROKER, {serverbroker, 'serverbroker@vm-projectweb3'}).

% TODO: finish writing usage
help() ->
    io:format("Usage:...\n"),
    init:stop().

% -spec newGame(GameName :: atom(), NodeName :: node()) -> no_return().

% newGame(GameName, NodeName) ->
%     gen_server:call(?SERVERBROKER, {start_gameserver, atom_to_list(GameName)}),
%     init:stop().

%% Join an active gameserver
% joinGame(Game) ->
%     gen_server:call()

%% List all of the potential games that the user could start a room for and play
listGames() ->
    Games = gen_server:call(?SERVERBROKER, {list_games}),
    io:format("Available Games:~n"),
    lists:map(fun (Game) -> io:format("\t~p~n", [Game]) end, Games),
    halt().

%% List the active rooms
listActiveGames() ->
    Reply = gen_server:call(?SERVERBROKER, {list_gameservers}),
    io:format("~p~n", [Reply]),
    halt().

% sendMessage() ->
    
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
