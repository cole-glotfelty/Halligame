%%%
%%% @author Liam Strand, Tufts University
%%% @copyright Spring 2024
%%%
%%% A simple server that starts an external program and communicates
%%% with it on a port.
%%% Edited by Will Cordray, Spring 2025
%%%
-module(gameServer).
-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([send/1, dump_ports/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

start_link(GamePathString) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [GamePathString], []).

stop() ->
    gen_server:stop(?MODULE).

send(Data) -> gen_server:cast(?MODULE, {send, Data}).
dump_ports() -> gen_server:call(?MODULE, dump_ports).

init(GamePathString) ->
    GamePort = open_port({spawn, GamePathString}, [binary, {packet, 4}, use_stdio]),
    {ok, {GamePort, []}}.

%% When we terminate, send the program on the port a message indicating that
%% it is about to close, then close the port.
terminate(_Reason, {GamePort, _ClientPorts}) ->
    GamePort ! {self(), command, term_to_binary(close)},
    GamePort ! {self(), close}.

handle_cast({make_move, Data}, {GamePort, ClientPorts}) ->
    GamePort ! {self(), {command, term_to_binary(Data)}},
    {noreply, {GamePort, ClientPorts}};
handle_cast({add_client, {Pid, ClientPort}}, {GamePort, ClientPorts}) ->
    % port must be opened by caller
    ClientPort ! {Pid, {connect, self()}}, % set the owner of the port to this genserver module
    {noreply, {GamePort, [ClientPort | ClientPorts]}}.

handle_call(dump_ports, _From, {GamePort, ClientPorts}) -> 
    {reply, ClientPorts, {GamePort, ClientPorts}}.


%% If we receive a message from the port, either accept the new state and notify all client ports or deny it
handle_info({Port, {data, Data}}, {_GamePort, ClientPorts}) ->
    {noreply, {Port, [binary_to_term(Data) | ClientPorts]}}.
