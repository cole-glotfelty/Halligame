%%%-------------------------------------------------------------------
%%% @doc
%%% TODO
%%% @end
%%% Created : 22 Mar 2025
%%% Last edited: Michael Daniels, 19 April 2025
%%%-------------------------------------------------------------------
-module(serverbroker).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3, format_status/1]).

-export([add_list_del_list/1]).

-define(SERVER, ?MODULE).

-record(session, {linuxpid = "" :: string(), erlangpid :: pid()}).
-record(user, {login :: string(), sessions = [] :: [#session{}],
               playing = [] :: [{string(), pid()}]}).

-record(gameclient, {login :: string(), pid :: pid()}).

-record(gameserver, {game :: string(), pid :: pid(),
                     players = [] :: [#gameclient{}],
                     nodeName :: string()}).

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
-spec handle_call(Request :: term(), From :: {pid(), term()},
                  State :: term()) ->
      {reply, Reply :: term(), NewState :: term()} |
      {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
      {reply, Reply :: term(), NewState :: term(), hibernate} |
      {noreply, NewState :: term()} |
      {noreply, NewState :: term(), Timeout :: timeout()} |
      {noreply, NewState :: term(), hibernate} |
      {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
      {stop, Reason :: term(), NewState :: term()}.
handle_call({list_users}, _From, State) ->
    {reply, State#state.users, State};
handle_call({list_logins}, _From, State) ->
    {reply, lists:map(fun (X) -> X#user.login end, State#state.users), State};
handle_call({list_gameservers}, _From, State) ->
    {reply, State#state.gameservers, State};
handle_call({lookupGameServerID, ID}, _From, State) ->
    % ID is a list of six digits.
    FilterFun = fun (GS) -> lists:prefix(ID, GS#gameserver.nodeName) end,
    case lists:filter(FilterFun, State#state.gameservers) of
        [TheGS] ->
            {reply, {TheGS#gameserver.game, TheGS#gameserver.nodeName,
                     term_to_binary(TheGS#gameserver.pid)}, State};
        [] ->
            {reply, notfound, State}
    end;
handle_call(Catchall, From, State) ->
    io:fwrite("Unrecognized call ~p from ~p~n", [Catchall, From]),
    {reply, error, State}.


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
handle_cast({register_gameserver, GameName, NodeName, ServerPid}, State) ->
    % Must be called by the game server, not a client.
    CurrGS   = State#state.gameservers,
    NewGS    = #gameserver{game = GameName, pid = ServerPid,
                           nodeName = NodeName},
    monitor(process, ServerPid),
    {noreply, State#state{gameservers = [NewGS | CurrGS]}};
handle_cast({unregister_gameserver, ServerPid}, State) ->
    % Must be called by the game server, not a client.
    CurrGS     = State#state.gameservers,
    NotThisPid = fun (X) -> ServerPid =/= X#gameserver.pid end,
    Filtered   = lists:filter(NotThisPid, CurrGS),
    {noreply, State#state{gameservers = Filtered}};
handle_cast({joined_gameserver, JoinedLogin, JoinedPid, ServerPid}, State) ->
    % Must be called by the game server, not a client.
    CurrGS         = State#state.gameservers,
    HasThisPid     = fun (X) -> ServerPid == X#gameserver.pid end,
    {[ThisGS], Rest} = lists:partition(HasThisPid, CurrGS),
    OldGCs         = ThisGS#gameserver.players,
    NewGC          = #gameclient{login = JoinedLogin, pid = JoinedPid},
    NewGS          = ThisGS#gameserver{players = [NewGC | OldGCs]},

    FilterUser = fun (Usr) -> Usr#user.login == JoinedLogin end,
    {Matching, OtherUsers} = lists:partition(FilterUser, State#state.users),
    case Matching of
        [] ->
            NewUsers = OtherUsers;
        [ThisUser] ->
            NewPlaying = [{ThisGS#gameserver.game, JoinedPid}
                           | ThisUser#user.playing],
            NewUsers = [ThisUser#user{playing = NewPlaying} | OtherUsers]
    end,
    monitor(process, JoinedPid),
    {noreply, State#state{gameservers = [NewGS | Rest],
                          users = NewUsers}};
handle_cast({left_gameserver, LeftLogin, LeftPid, ServerPid}, State) ->
    % Must be called by the game server, not a client.
    % TODO: update user's currently playing games too.
    CurrGS         = State#state.gameservers,
    HasThisPid     = fun (X) -> ServerPid == X#gameserver.pid end,
    {[ThisGS], Rest} = lists:partition(HasThisPid, CurrGS),
    OldGCs         = ThisGS#gameserver.players,
    FilterFun      = fun (GC) -> ((GC#gameclient.login == LeftLogin) and
                                  (GC#gameclient.pid == LeftPid)) end,
    NewGCs         = lists:filter(FilterFun, OldGCs),
    NewGS          = ThisGS#gameserver{players = NewGCs},

    FilterUser = fun (Usr) -> Usr#user.login == LeftLogin end,
    {[ThisUser], OtherUsers} = lists:partition(FilterUser, State#state.users),
    FilterPlaying = fun ({GameName, Pid}) ->
                            ((GameName =/= CurrGS#gameserver.game) and
                            (Pid =/= LeftPid)) end,
    UpdatedUser = ThisUser#user{playing = lists:filter(FilterPlaying,
                                                       ThisUser#user.playing)},
    {noreply, State#state{gameservers = [NewGS | Rest],
                          users = [UpdatedUser | OtherUsers]}};
handle_cast({add_user, Login, LinuxPid, ErlangPid}, State) ->
    {ThisUser, Rest} = lists:partition(fun (Usr) -> Usr#user.login == Login end,
                                       State#state.users),
    NewSession = #session{erlangpid = ErlangPid, linuxpid = LinuxPid},
    monitor(process, ErlangPid),
    case ThisUser of
        [] ->
            User = #user{login = Login, sessions = [NewSession]};
        [#user{login = Login, sessions = OldSessions, playing = Playing}] ->
            User = #user{login    = Login,
                            sessions = [NewSession | OldSessions],
                            playing  = Playing}
    end,
    {noreply, State#state{users = [User | Rest]}};
handle_cast({del_user, Login, LinuxPid}, State) ->
    Fun      = fun (Usr) -> Login == Usr#user.login end,
    {ThisUser, Rest} = lists:partition(Fun, State#state.users),
    case ThisUser of
        [] ->
            % User not found.
            Users = Rest;
        [#user{login = Login, sessions = Sessions}] ->
            % Remove this session.
            Filter = fun (X) -> X#session.linuxpid =/= LinuxPid end,
            FilteredSessions = lists:filter(Filter, Sessions),
            [SingleUser] = ThisUser,
            Users = [SingleUser#user{sessions = FilteredSessions} | Rest]
    end,
    {noreply, State#state{users = Users}};
handle_cast({message_user, FromUser, ToUser, Message}, State) ->
    message_user(FromUser, ToUser, Message, State#state.users),
    {noreply, State};
handle_cast(Catchall, State) ->
    io:fwrite("Unrecognized cast: ~p~n", [Catchall]),
    {noreply, State}.


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
handle_info({'DOWN', _MonitorRef, process, ErlangPid, _Reason}, State) ->
    io:fwrite("Current state ~p~n", [State]),
    io:fwrite("Got down message for process ~p~n", [ErlangPid]),
    FilterSession = fun (Ses) -> Ses#session.erlangpid =/= ErlangPid end,
    FilterPlaying = fun ({_GameName, Pid}) -> (Pid =/= ErlangPid) end,
    UsrMapFun = fun (Usr) ->
        Usr#user{sessions = lists:filter(FilterSession, Usr#user.sessions),
                 playing = lists:filter(FilterPlaying, Usr#user.playing)} end,
    NewUsers = lists:map(UsrMapFun, State#state.users),

    FilterGameServer = fun (GS) -> GS#gameserver.pid =/= ErlangPid end,
    FilteredGSs = lists:filter(FilterGameServer, State#state.gameservers),
    io:fwrite("Filtered GSs: ~p~n", [FilteredGSs]),
    FilterPlayers = fun (P) -> P#gameclient.pid =/= ErlangPid end,
    PlayerMapFun = fun (GS) ->
        GS#gameserver{players =
                        lists:filter(FilterPlayers, GS#gameserver.players)} end,
    FinalGSs = lists:map(PlayerMapFun, FilteredGSs),
    io:fwrite("Final GSs: ~p~n", [FinalGSs]),
    {noreply, State#state{users = NewUsers, gameservers = FinalGSs}};
handle_info({getBrokerPid, FromPid}, State) ->
    io:fwrite("Got info: ~p~n", [{getBrokerPid, FromPid}]),
    FromPid ! {brokerPid, self()},
    {noreply, State};
handle_info(Info, State) ->
    io:fwrite("Unrecognized info: ~p~n", [Info]),
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

% Takes a message, its sender, its recipient, and a list of all users.
% Sends the message to all sessions the recipient has.
message_user(_FromUser, _ToUser, _Message, [])      ->
    ok;
message_user(FromUser, ToUser, Message, [H | T]) ->
    Fun = fun (X) -> X#session.erlangpid ! {message, FromUser, Message} end,
    case H#user.login of
        ToUser ->
            lists:map(Fun, H#user.sessions);
        _  ->
            ok
    end,
    message_user(FromUser, ToUser, Message, T).


%%%===================================================================
%%% Tests
%%%===================================================================

% Credit:
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
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "mdanie09",
                                               "123", self()})),
        ?assertMatch([#user{login = "mdanie09"}],
                     gen_server:call(Pid, {list_users}))
    end.

add_list_user_2(Pid) ->
    fun () ->
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "wcordr01",
                                              "111", self()})),
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "mdanie09",
                                               "222", self()})),
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "cglotf01",
                                               "333", self()})),
        ?assertMatch([#user{login = "cglotf01"}, #user{login = "mdanie09"},
                      #user{login = "wcordr01"}],
                     lists:sort(gen_server:call(Pid, {list_users})))
    end.

add_list_del_list(Pid) ->
    fun () ->
        Self = self(),
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "wcordr01",
                                               "111", Self})),
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "mdanie09",
                                               "222", self()})),
        ?assertEqual(ok, gen_server:cast(Pid, {add_user, "cglotf01",
                                               "333", self()})),
        ?assertMatch([#user{login = "cglotf01"}, #user{login = "mdanie09"},
                      #user{login = "wcordr01"}],
                     lists:sort(gen_server:call(Pid, {list_users}))),
        ?assertEqual(ok, gen_server:cast(Pid, {del_user, "mdanie09", "222"})),
        ?assertMatch([#user{login = "cglotf01",
                            sessions = [#session{erlangpid = Self,
                                                 linuxpid = "333"}]},
                      #user{login = "mdanie09", sessions = []},
                      #user{login = "wcordr01",
                            sessions = [#session{erlangpid = Self,
                                                 linuxpid = "111"}]}],
                     lists:sort(gen_server:call(Pid, {list_users})))
    end.