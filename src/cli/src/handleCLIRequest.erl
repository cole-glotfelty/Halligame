%% handleCLIRequest.erl
%% Class: Concurrent Programming, Spring 2025
%% Handles certain requests coming from the "hg" command line tool.

%% One weird thing to note is that every function needs to end with
%% "init:stop()" to terminate the process.

-module(handleCLIRequest).
-export([listActiveGames/0, sendMessage/1, lookupGameServerID/1, listOnline/0]).

-define(SERVERBROKER, {serverbroker, 'serverbroker@vm-projectweb3'}).

%% List the active rooms on stdout.
-spec listActiveGames() -> no_return().
listActiveGames() ->
    Reply = gen_server:call(?SERVERBROKER, {list_gameservers}),
    io:fwrite("~p~n", [Reply]),
    init:stop().

-spec listOnline() -> no_return().
listOnline() ->
    Reply = gen_server:call(?SERVERBROKER, {list_logins}),
    lists:map(fun (X) -> io:fwrite("~s~n", [X]) end, Reply),
    init:stop().

% Send a user a message.
-spec sendMessage([string() | [string() | string() | []]]) -> no_return().
sendMessage([FromUser, ToUser, Message]) ->
    gen_server:cast(?SERVERBROKER, {message_user, FromUser, ToUser, Message}),
    init:stop().

-spec lookupGameServerID([string() | []]) -> no_return().
lookupGameServerID([GameServerID]) ->
    case gen_server:call(?SERVERBROKER, {lookupGameServerID, GameServerID}) of
        {GameName, NodeName} ->
            io:fwrite("~s~n~s~n", [GameName, NodeName]);
        notfound ->
            io:fwrite("notfound~n")
    end,
    init:stop().

% TODO: implement?
% addFriend(FriendID) ->
%     init:stop().

% TODO: implement?
% removeFriend(FriendID) ->
%     init:stop().