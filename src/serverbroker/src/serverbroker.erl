%%%-------------------------------------------------------------------
%%% @author 
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2025
%%% Last edited: Michael Daniels, 16 April 2025
%%%-------------------------------------------------------------------
-module(serverbroker).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3, format_status/1]).

-define(SERVER, ?MODULE).

-record(game, {name :: string(), min_players :: pos_integer(),
               max_players :: pos_integer() | inf}).

-record(user, {login :: string(), pids = [] :: [string()],
               playing = [] :: [#game{}]}).

-record(gameclient, {login :: string(), pid :: pid()}).

-record(gameserver, {game :: #game{}, pid :: pid(),
                     players = [] :: [#gameclient{}]}).
                    
-record(state, {users = [] :: [#user{}],
                gameservers = [] :: [#gameserver{}]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server (will die when paraent proceess dies)
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
      {error, Error :: {already_started, pid()}} |
      {error, Error :: term()} |
      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server (entirely independant of the parent)
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, Pid :: pid()} |
    {error, Error :: {already_started, pid()}} |
    {error, Error :: term()} |
    ignore.
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

% TODO doc
stop() ->
    gen_server:stop({local, ?SERVER}).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
      {ok, State :: term(), Timeout :: timeout()} |
      {ok, State :: term(), hibernate} |
      {stop, Reason :: term()} |
      ignore.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

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
handle_call(Request, {Pid, _FromTag}, State) ->
    case Request of
        {register_gameserver, Game} ->
            % Must be called by the game server, not a client.
            % TODO: monitor?
            CurrGS   = State#state.gameservers,
            NewGS    = #gameserver{game = Game, pid = Pid},
            Reply    = ok,
            NewState = State#state{gameservers = [NewGS | CurrGS]};
        {unregister_gameserver} ->
            % Must be called by the game server, not a client.
            CurrGS   = State#state.gameservers,
            IsRegistered = fun (X) -> Pid == X#gameserver.pid end,
            Filtered = lists:filter(IsRegistered, CurrGS),
            Reply = ok,
            NewState = State#state{gameservers = Filtered};
        {joined_gameserver, JoinedLogin, JoinedPid} ->
            % Must be called by the game server, not a client.
            % TODO: update user's currently playing games too.
            % TODO: monitor?
            CurrGS         = State#state.gameservers,
            IsRegistered   = fun (X) -> Pid == X#gameserver.pid end,
            {ThisGS, Rest} = lists:partition(IsRegistered, CurrGS),
            OldGCs         = ThisGS#gameserver.players,
            NewGC          = #gameclient{login = JoinedLogin, pid = JoinedPid},
            NewGS          = ThisGS#gameserver{players = [NewGC | OldGCs]},
            Reply          = ok,
            NewState       = State#state{gameservers = [NewGS | Rest]};
        {left_gameserver, LeftLogin, LeftPid} ->
            % Must be called by the game server, not a client.
            % TODO: update user's currently playing games too.
            CurrGS         = State#state.gameservers,
            IsRegistered   = fun (X) -> Pid == X#gameserver.pid end,
            {ThisGS, Rest} = lists:partition(IsRegistered, CurrGS),
            OldGCs         = ThisGS#gameserver.players,
            NewGCs         = lists:filter(neq(#gameclient{login = LeftLogin,
                                                          pid = LeftPid}),
                                          OldGCs),
            NewGS          = ThisGS#gameserver{players = NewGCs},
            Reply          = ok,
            NewState       = State#state{gameservers = [NewGS | Rest]};
        {list_users} ->
            Reply = State#state.users,
            NewState = State;
        {list_gameservers} ->
            Reply = State#state.gameservers,
            NewState = State;
        _ -> 
            io:format("Got request ~p, ignoring~n", [Request]),
            Reply = error,
            NewState = State
    end,
    % io:format("Reply = ~p, NewState = ~p~n", [Reply, NewState]),
    {reply, Reply, NewState}.


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
handle_cast(Request, State) ->
    case Request of
        % ShellPid is a string containing a Linux process ID, not an erlang one.
        {add_user, Login, ShellPid} ->
            io:format("in add_user, login ~p; shell pid ~p~n", [Login, ShellPid]),
            {ThisUser, Rest} = lists:partition(eq(Login), State#state.users),
            case ThisUser of
                [] ->
                    User = #user{login = Login, pids = [ShellPid]};
                #user{login = Login, pids = Pids, playing = Playing} ->
                    OtherPids = lists:filter(neq(ShellPid), Pids),
                    User = #user{login   = Login,
                                 pids    = [ShellPid | OtherPids],
                                 playing = Playing}
            end,
            {noreply, State#state{users = [User | Rest]}};
        {del_user, Login, ShellPid} ->
            Fun      = fun (X) -> Login == X#user.login end,
            {ThisUser, Rest} = lists:partition(Fun, State#state.users),
            case ThisUser of
                [] ->
                    % User not found.
                    Users = Rest;
                [#user{login = Login, pids = []}] ->
                    % User has no pids, delete.
                    Users = Rest;
                [#user{login = Login, pids = [ShellPid]}] ->
                    % The user has only this pid, so delete them.
                    Users = Rest;
                [#user{login = Login, pids = [_Other]}] ->
                    % This pid wasn't found, so ignore.
                    Users = State#state.users;
                [#user{login = Login, pids = Pids, playing = Playing}] ->
                    % User has 2+ pids, delete only this pid, keep the user.
                    User  = #user{login   = Login,
                                    pids    = lists:filter(neq(ShellPid), Pids),
                                    playing = Playing},
                    Users = [User | Rest]
            end,
            {noreply, State#state{users = Users}};
        Catchall ->
            io:format("Unrecognized call: ~p~n", [Catchall]),
            {noreply, State}
    end.
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
      {noreply, NewState :: term()} |
      {noreply, NewState :: term(), Timeout :: timeout()} |
      {noreply, NewState :: term(), hibernate} |
      {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
        State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
          State :: term(),
          Extra :: term()) -> {ok, NewState :: term()} |
      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Status :: gen_server:format_status()) ->
    NewStatus :: gen_server:format_status().
format_status(Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% eq/1, a utility higher-order function, takes a term A and returns a fun.
% That fun takes one argument, B, and returns whether A == B.
eq(A) ->
    fun (B) -> A == B end.

% neq/1, a utility higher-order function, takes a term A and returns a fun.
% That fun takes one argument, B, and returns whether A =/= B.
neq(A) ->
    fun (B) -> A =/= B end.


% Tests

% https://lookonmyworks.co.uk/2015/01/25/testing-a-gen_server-with-eunit/
server_broker_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun server_is_alive/1,
        fun add_list_user_1/1,
        fun add_list_user_2/1,
        fun add_list_del_list/1
    ]}.

setup() ->
    {ok, Pid} = serverbroker:start(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

server_is_alive(Pid) ->
    fun () ->
        ?assert(is_process_alive(Pid))
    end.

add_list_user_1(Pid) ->
    fun () ->
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "mdanie09", "123"})),
        ?assertMatch([#user{login = "mdanie09"}],
                     gen_server:call(Pid, {list_users}))
    end.

add_list_user_2(Pid) ->
    fun () ->
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "wcordr01", "111"})),
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "mdanie09", "222"})),
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "cglotf01", "333"})),
        ?assertMatch([#user{login = "cglotf01"}, #user{login = "mdanie09"},
                      #user{login = "wcordr01"}],
                     lists:sort(gen_server:call(Pid, {list_users})))
    end.

add_list_del_list(Pid) ->
    fun () ->
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "wcordr01", "111"})),
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "mdanie09", "222"})),
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "cglotf01", "333"})),
        ?assertMatch([#user{login = "cglotf01"}, #user{login = "mdanie09"},
                      #user{login = "wcordr01"}],
                     lists:sort(gen_server:call(Pid, {list_users}))),
        ?assertEqual(ok, gen_server:cast(Pid, {del_user, "mdanie09", "222"})),
        ?assertMatch([#user{login = "cglotf01"}, #user{login = "wcordr01"}],
                     lists:sort(gen_server:call(Pid, {list_users})))
    end.