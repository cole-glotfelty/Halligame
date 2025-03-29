%%%
%%% @author Liam Strand, Tufts University
%%% @copyright Spring 2024
%%%
%%% A simple server that starts an external program and communicates
%%% with it on a port.
%%% Edited by Will Cordray, Spring 2025
%%%
-module(communicationServer).
-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([send/1, dump_ports/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-record(client, {pid :: pid(), port :: port()}).
-record(state, {game_server_port :: port(), clients = [] :: [#client{}]}).
%% Message structure:
%%      Server (from port):
%%            {broadcast, NewState} --> Send {state, NewState} to All Clients
%%            {reply, {ClientPID, ErrorMessage}} --> Send {invalid, ErrorMessage} to ClientPort
%%            {_, Message} --> Send {other, Message} to All Clients
%%
%%      Client (from port):
%%            {event, Event} --> Send {event, Event} to Server
%%            {_, Message} --> Send {other, Message} to Server
%%
%%      Management (e.g. from other erlang nodes):
%%            Add Client --> Send {new_client, Pid} to Server
%%            Remove Client --> Send {remove_client, Pid} to Server


start_link(GamePathString) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [GamePathString], []).

stop() ->
    gen_server:stop(?MODULE).

send(Data) -> gen_server:cast(?MODULE, {send, Data}).
dump_ports() -> gen_server:call(?MODULE, dump_ports).

init(GamePathString) ->
    %% TODO: update the command so that it opens up a port to the server
    GameServerPort = open_port({spawn, GamePathString}, [binary, {packet, 4}, use_stdio]),
    {ok, {GameServerPort, []}}.

%% When we terminate, send the program on the port a message indicating that
%% it is about to close, then close the port.
terminate(_Reason, {GameServerPort, _ClientPorts}) ->
    GameServerPort ! {self(), command, term_to_binary(close)},
    GameServerPort ! {self(), close}.

% port must be opened by caller
handle_cast({add_client, {Pid, ClientPort}}, {GameServerPort, ClientPorts}) ->
    ClientPort ! {Pid, {connect, self()}}, % set the owner of the port to this genserver module
    sendPortMessage(GameServerPort, {new_client, Pid}),
    {noreply, {GameServerPort, [ClientPort | ClientPorts]}};
handle_cast({remove_client, {Pid, ClientPort}}, {GameServerPort, ClientPorts}) ->
    sendPortMessage(GameServerPort, {remove_client, Pid}),
    NewClientPorts = lists:filter(fun(Port) -> Port =/= ClientPort end, ClientPorts),
    {noreply, {GameServerPort, NewClientPorts}}.

%% for debugging --> should print all of the game server ports?
handle_call(dump_ports, _From, {GameServerPort, ClientPorts}) -> 
    {reply, ClientPorts, {GameServerPort, ClientPorts}}.

%% If we receive a message from the port, either accept the new state and notify all client ports or deny it
handle_info({Port, {data, Data}}, State) ->
    case Port of
        State#state.game_server_port ->
            {MessageType, {ClientPid, Message}} = binary_to_term(Data),
            case MessageType of
                broadcast -> broadcast(ClientPorts, {state, Message});
                reply -> ClientPort = none,
                        sendPortMessage(ClientPort, {reply, Message});
                % reply ->
                %         {ClientPid, Message} = Message,
                %         sendPortMessage(ClientPort, {invalid, ErrorMessage});
                _ -> broadcast(ClientPorts, {ClientPort, Message})
            end,
            {noreply, {GameServerPort, ClientPorts}};
        _ClientPort ->
            {MessageType, {ClientPort, Message}} = binary_to_term(Data),
            case MessageType of
                event -> sendPortMessage(GameServerPort, {event, {ClientPort, Message}});
                _ -> sendPortMessage(GameServerPort, {other, {ClientPort, Message}})
            end,
            {noreply, {GameServerPort, ClientPorts}}
    end.


sendPortMessage(Port, Message) ->
    Port ! {self(), command, term_to_binary(Message)}.


broadcast([], _Message) ->
    ok;
broadcast([ClientPort | Others], Message) ->
    sendPortMessage(ClientPort, Message),
    broadcast(Others, Message).
