%% handleCLIRequest.erl
%% Class: Concurrent Programming, Spring 2025
%% Handles certain requests coming from the "hg" command line tool.

%% One weird thing to note is that every function needs to end with
%% "init:stop()" to terminate the process.

-module(handleCLIRequest).
-export([listActiveGames/0, sendMessage/1, lookupGameServerID/1, listOnline/0,
         sendInvite/1]).

-define(SERVERBROKER, {serverbroker, 'serverbroker@vm-projectweb3'}).

% @doc List the active rooms on stdout.
-spec listActiveGames() -> no_return().
listActiveGames() ->
    Reply = gen_server:call(?SERVERBROKER, {list_gameservers}),
    FormatPlayer = fun({gameclient, Login, _Pid}) -> Login end,
    FormatPlayers = fun (Players) ->
        case Players of
            [] -> "none";
            _  -> io_lib:fwrite("~p", [lists:map(FormatPlayer, Players)])
        end
    end,
    PrintFun = fun ({gameserver, GameName, _Pid, Players, NodeName}) ->
        {ID, _} = lists:split(6, NodeName),
        io:fwrite("Game: ~p; ID: ~p; Players: ~s~n",
                  [GameName, ID, FormatPlayers(Players)])
    end,
    case Reply of
        [] -> io:fwrite("No games are active currently.~n");
        _  -> lists:foreach(PrintFun, Reply)
    end,
    init:stop().

% @doc List all users who are online on stdout.
-spec listOnline() -> no_return().
listOnline() ->
    Reply = gen_server:call(?SERVERBROKER, {list_logins}),
    lists:foreach(fun (X) -> io:fwrite("~s~n", [X]) end, Reply),
    init:stop().

% @doc Send a user a message.
-spec sendMessage([string() | [string() | [string() | []]]]) -> no_return().
sendMessage([FromUser, ToUser, Message]) ->
    gen_server:cast(?SERVERBROKER, {message_user, FromUser, ToUser, Message}),
    init:stop().

% @doc Invite a user to a game.
-spec sendInvite([string() | [string() | [string() | [string() | []]]]]) ->
    no_return().
sendInvite([FromUser, ToUser, GameName, JoinCommand]) ->
    gen_server:cast(?SERVERBROKER,
                    {invite_user, FromUser, ToUser, GameName, JoinCommand}),
    init:stop().

% @doc Looks up the game server with the given ID.
% Prints to stdout the game name, node name, and pid, all seperated by newlines.
-spec lookupGameServerID([string() | []]) -> no_return().
lookupGameServerID([GameServerID]) ->
    case gen_server:call(?SERVERBROKER, {lookupGameServerID, GameServerID}) of
        {GameName, NodeName, PidBinary} ->
            io:fwrite("~s~n~s~n~s~n", [GameName, NodeName, PidBinary]);
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