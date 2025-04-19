%% handleCLIRequest.erl
%% Class: Concurrent Programming, Spring 2025
%% Handles certain requests coming from the "hg" command line tool.

%% One weird thing to note is that every function needs to end with 
%% "init:stop()" to terminate the process.

-module(handleCLIRequest).
-export([listActiveGames/0, sendMessage/1]).

-define(SERVERBROKER, {serverbroker, 'serverbroker@vm-projectweb3'}).

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