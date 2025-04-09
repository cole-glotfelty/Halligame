%%% @author Originally by Liam Strand, Spring 2024.
%%% Edited heavily by Will Cordray and Michael Daniels, spring 2025.
%%% 
%%% @doc Facilitates communication between a game server and its clients.
-module(communicationServer).
-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVERBROKER, {serverbroker, 'serverbroker@vm-projectweb3'}).
-define(ME, string:trim(os:cmd("whoami"))).

-record(state, {game_name :: atom(), game_server :: pid() | undefined,
                clients = [] :: [pid()]}).

-spec start_link([string() | string() | []]) ->
    ignore | {error, _} | {'ok',pid()}.
% start_link takes a two-element list and starts this gen_server.
start_link([GameName, NodeName]) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [GameName, NodeName], []).

-spec stop() -> ok.
% Stops this gen_server.
stop() ->
    gen_server:stop(?MODULE).


-spec init([string() | string() | []]) -> {ok, #state{}}.
init([GameName, NodeName]) ->
    % register gameserver with the serverbroker
    gen_server:call(?SERVERBROKER, {register_gameserver, {?ME ++ ":" ++ GameName, NodeName}}),
    {ok, #state{game_name = list_to_atom(GameName), clients = []}}.

-spec terminate(Reason, State :: #state{}) -> any() 
    when Reason :: normal | shutdown | {shutdown,term()} | term().
%% When we terminate, send the program a message indicating that
%% we are leaving.
terminate(_Reason, State) ->
    broadcast(State#state.clients, close),
    State#state.game_server ! close.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: {atom(), pid()}, State :: #state{}) ->
	  {noreply, NewState :: #state{}}.

handle_cast({replace_server, Pid}, State) ->
    % Replaces the current game server, if any, with a new one.
    % Note: this is used to set the game server at first.
    {noreply, State#state{game_server = Pid}};
handle_cast({add_client, Pid}, State) ->
    % Add a client to this game.
    io:format("~p~n", [State#state.game_server]),
    State#state.game_server ! {new_client, Pid},
    CurrClients = State#state.clients,
    {noreply, State#state{clients = [Pid | CurrClients]}};
handle_cast({remove_client, Pid}, State) ->
    % Remove a client from this game.
    State#state.game_server ! {remove_client, Pid},
    State#state.game_server ! {remove_client, Pid},
    FilterFun = fun(X) -> X =/= Pid end,
    FilteredClients = lists:filter(FilterFun, State#state.clients),
    {noreply, State#state{clients = FilteredClients}};
handle_cast(Any, State) ->
    % Catchall
    io:format("Unrecognized cast ~p; current state ~p~n", [Any, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: #state{}) ->
	  {reply, Reply :: term(), NewState :: #state{}} |
	  {reply, Reply :: term(), NewState :: #state{}, Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: #state{}, hibernate} |
	  {noreply, NewState :: #state{}} |
	  {noreply, NewState :: #state{}, Timeout :: timeout()} |
	  {noreply, NewState :: #state{}, hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	  {stop, Reason :: term(), NewState :: #state{}}.

handle_call(dump_state, _From, State) -> 
    % For debugging, dump the state
    {reply, State, State};
handle_call(Any, From, State) ->
    % Catchall
    io:format("Unrecognized call ~p from ~p; current state ~p~n",
              [Any, From, State]),
    {reply, error, State}.

-spec handle_info({Pid :: pid(), {data, Message :: term()}}, State :: #state{}) ->
                    {noreply, #state{}}.
%% If we receive a message from the port, either accept the new state and
%% notify all client ports or deny it.
handle_info({Pid, {data, {MessageType, RawMessage}}}, State) ->
    GameServerPid = State#state.game_server,
    case Pid of
        GameServerPid ->
            case MessageType of
                broadcastState ->
                    broadcast(State#state.clients, {state, RawMessage});
                reply -> 
                    {ClientPid, Message} = RawMessage,
                    ClientPid ! {reply, Message};
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
            case MessageType of
                event -> GameServerPid ! {event, {ClientPid, RawMessage}};
                _ -> GameServerPid ! {other, {ClientPid, RawMessage}}
            end,
            {noreply, State}
    end;
% Catchall
handle_info(Any, State) ->
    io:format("Unrecognized info ~p; current state ~p~n", [Any, State]),
    {noreply, State}.


-spec broadcast(SendTo :: [pid()], Message :: term()) -> ok.
% Send Message to all erlang processes in the list.
broadcast([], _Message) ->
    ok;
broadcast([Client | Rest], Message) ->
    Client ! Message,
    broadcast(Rest, Message).
