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
%%            {broadcastState, NewState} --> Send {state, NewState} to All Clients
%%            {reply, {ClientPID, Message}} --> Send {reply, Message} to ClientPort
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
    GameServerPort = open_port({spawn, GamePathString}, [binary, {packet, 4}, nouse_stdio]),
    {ok, #state{game_server_port = GameServerPort, clients = []}}.

%% When we terminate, send the program on the port a message indicating that
%% it is about to close, then close the port.
terminate(_Reason, {GameServerPort, _ClientPorts}) ->
    GameServerPort ! {self(), command, term_to_binary(close)},
    GameServerPort ! {self(), close}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.

% port must be opened by caller
handle_cast({add_client, {Pid, ClientPort}}, State) ->
    ClientPort ! {Pid, {connect, self()}}, % set the owner of the port to this genserver module
    % port_connect(ClientPort, self()), % set the owner of the port to this genserver module
    sendPortMessage(State#state.game_server_port, {new_client, Pid}),
    NewClient = #client{pid = Pid, port = ClientPort},
    CurrClients = State#state.clients,
    {noreply, State#state{clients = [NewClient | CurrClients]}};

handle_cast({remove_client, {Pid, ClientPort}}, State) ->
    sendPortMessage(State#state.game_server_port, {remove_client, Pid}),
    FilterFun = fun(Client) -> Client#client.port =/= ClientPort end,
    FilteredClients = lists:filter(FilterFun, State#state.clients),
    {noreply, State#state{clients = FilteredClients}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.

%% for debugging --> should print all of the game server ports?
handle_call(dump_ports, _From, {GameServerPort, ClientPorts}) -> 
    {reply, ClientPorts, {GameServerPort, ClientPorts}};
handle_call(dump_state, _From, State) -> 
    {reply, State, State}.

%% If we receive a message from the port, either accept the new state and notify all client ports or deny it
handle_info({Port, {data, Data}}, State) ->
    GameServerPort = State#state.game_server_port,
    case Port of
        GameServerPort ->
            {MessageType, RawMessage} = binary_to_term(Data),
            case MessageType of
                broadcastState ->
                    broadcast(State#state.clients, {state, RawMessage});
                reply -> 
                    {ClientPid, Message} = RawMessage,
                    ClientPort = pidToPort(ClientPid, State),
                    sendPortMessage(ClientPort, {reply, Message});
                % reply ->
                %         {ClientPid, Message} = Message,
                %         sendPortMessage(ClientPort, {invalid, ErrorMessage});
                terminate ->
                    sendPortMessage(GameServerPort, stop),
                    broadcast(State#state.clients, stop),
                    stop();
                _ ->
                    {ClientPid, Message} = RawMessage,
                    broadcast(State#state.clients, {ClientPid, Message})
            end,
            {noreply, State};

        ClientPort ->
            ClientPid = portToPid(ClientPort, State),
            {MessageType, Message} = binary_to_term(Data),
            case MessageType of
                event -> sendPortMessage(GameServerPort, {event, {ClientPid, Message}});
                _ -> sendPortMessage(GameServerPort, {other, {ClientPid, Message}})
            end,
            {noreply, State}
    end.

portToPid(Port, State) -> 
    {value, Client} = lists:search(fun (X) -> X#client.port == Port end, State#state.clients),
    Client#client.pid.

pidToPort(Pid, State) -> 
    {value, Client} = lists:search(fun (X) -> X#client.pid == Pid end, State#state.clients),
    Client#client.port.

sendPortMessage(Port, Message) ->
    Port ! {self(), {command, term_to_binary(Message)}}.


broadcast([], _Message) ->
    ok;
broadcast([Client | Rest], Message) ->
    sendPortMessage(Client#client.port, Message),
    broadcast(Rest, Message).
