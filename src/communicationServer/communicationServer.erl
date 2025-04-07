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
% -export([send/1, dump_ports/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVERBROKER, {serverbroker, 'serverbroker@vm-projectweb3'}).
-define(ME, string:trim(os:cmd("whoami"))).

% -record(client, {pid :: pid(), port :: port()}).
-record(state, {game_name :: atom(), game_server :: pid(), clients = [] :: [pid()]}).
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


start_link([GameName, NodeName]) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [GameName, NodeName], []).

stop() ->
    gen_server:stop(?MODULE).

% send(Data) -> gen_server:cast(?MODULE, {send, Data}).
% dump_ports() -> gen_server:call(?MODULE, dump_ports). % TODO: rm?

init([GameName, NodeName]) ->
    %% TODO: update the command so that it opens up a port to the server
    % GameServerPort = open_port({spawn, GamePathString}, [binary, {packet, 4}, nouse_stdio]),
    % register gameserver with the serverbroker
    gen_server:call(?SERVERBROKER, {register_gameserver, {?ME ++ ":" ++ GameName, NodeName}}),

    {ok, #state{game_name = GameName, clients = []}}.

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

handle_cast({replace_server, Pid}, State) ->
    io:format("Replace server with ~p~n", [Pid]),
    {noreply, State#state{game_server = Pid}};
handle_cast({add_client, Pid}, State) ->
    % ClientPort ! {Pid, {connect, self()}}, % set the owner of the port to this genserver module
    % port_connect(ClientPort, self()), % set the owner of the port to this genserver module
    io:format("~p~n", [State#state.game_server]),
    State#state.game_server ! {new_client, Pid},
    % sendPortMessage(State#state.game_server_port, {new_client, Pid}),
    CurrClients = State#state.clients,
    {noreply, State#state{clients = [Pid | CurrClients]}};
handle_cast({remove_client, Pid}, State) ->
    State#state.game_server ! {remove_client, Pid},
    State#state.game_server ! {remove_client, Pid},
    FilterFun = fun(X) -> X =/= Pid end,
    FilteredClients = lists:filter(FilterFun, State#state.clients),
    {noreply, State#state{clients = FilteredClients}};
handle_cast(Any, State) ->
    io:format("Unrecognized cast ~p; current state ~p~n", [Any, State]),
    {noreply, State}.

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
    {reply, State, State};
handle_call(Any, From, State) ->
    io:format("Unrecognized call ~p from ~p; current state ~p~n", [Any, From, State]),
    {reply, error, State}.

%% If we receive a message from the port, either accept the new state and notify all client ports or deny it
handle_info({Pid, {data, Data}}, State) ->
    GameServerPid = State#state.game_server,
    case Pid of
        GameServerPid ->
            % {MessageType, RawMessage} = binary_to_term(Data),
            {MessageType, RawMessage} = Data,
            io:format("Got data ~p from server.~n", [Data]),
            case MessageType of
                broadcastState ->
                    broadcast(State#state.clients, {state, RawMessage});
                reply -> 
                    {ClientPid, Message} = RawMessage,
                    ClientPid ! {reply, Message};
                % reply ->
                %         {ClientPid, Message} = Message,
                %         sendPortMessage(ClientPort, {invalid, ErrorMessage});
                terminate ->
                    GameServerPid ! stop,
                    broadcast(State#state.clients, stop),
                    stop();
                _ ->
                    {ClientPid, Message} = RawMessage,
                    broadcast(State#state.clients, {ClientPid, Message})
            end,
            {noreply, State};

        ClientPid ->
            % ClientPid = portToPid(ClientPort, State),
            {MessageType, Message} = Data, %binary_to_term(Data),
            io:format("Got data ~p from client.~n", [Data]),
            case MessageType of
                event -> GameServerPid ! {event, {ClientPid, Message}};
                _ -> GameServerPid ! {other, {ClientPid, Message}}
            end,
            {noreply, State}
    end;
handle_info(Any, State) ->
    io:format("Unrecognized info ~p; current state ~p~n", [Any, State]),
    {noreply, State}.

% portToPid(Port, State) -> 
%     {value, Client} = lists:search(fun (X) -> X#client.port == Port end, State#state.clients),
%     Client#client.pid.

% pidToPort(Pid, State) -> 
%     {value, Client} = lists:search(fun (X) -> X#client.pid == Pid end, State#state.clients),
%     Client#client.port.

% sendPortMessage(Port, Message) ->
%     Port ! {self(), {command, term_to_binary(Message)}}.


broadcast([], _Message) ->
    ok;
broadcast([Client | Rest], Message) ->
    % sendPortMessage(Client#client.port, Message),
    Client ! Message,
    broadcast(Rest, Message).
