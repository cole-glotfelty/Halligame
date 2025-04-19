%% handleCLIRequest.erl
%% Class: Concurrent Programming, Spring 2025
%% Handles certain requests coming from the "hg" command line tool.

%% One weird thing to note is that every function needs to end with 
%% "init:stop()" to terminate the process.

-module(handleCLIRequest).
-export([listGames/0, listActiveGames/0, sendMessage/1]).

-define(SERVERBROKER, {serverbroker, 'serverbroker@vm-projectweb3'}).

%% List on stdout all of the potential games that the user could start a
%% room for and play.
-spec listGames() -> no_return().
listGames() ->
    Games = gen_server:call(?SERVERBROKER, {list_games}),
    io:format("Available Games:~n"),
    lists:map(fun (Game) -> io:format("\t~p~n", [Game]) end, Games),
    init:stop().

%% List the active rooms on stdout.
-spec listActiveGames() -> no_return().
listActiveGames() ->
    Reply = gen_server:call(?SERVERBROKER, {list_gameservers}),
    io:format("~p~n", [Reply]),
    init:stop().

% Send a user a message.
-spec sendMessage([string() | [string() | string() | []]]) -> no_return().
sendMessage([FromUser, ToUser, Message]) ->
    gen_server:cast(?SERVERBROKER, {message_user, FromUser, ToUser, Message}),
    init:stop().
    
% TODO: implement?
% addFriend(FriendID) ->
%     init:stop().

% TODO: implement?
% removeFriend(FriendID) ->
%     init:stop().