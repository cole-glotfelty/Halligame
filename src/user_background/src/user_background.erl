-module(user_background).

-export([start/1]).

-define(SERVERREF, {serverbroker, 'serverbroker@vm-projectweb3'}).
-define(WAIT_TIME_S, 30).

start([Username, ShellPid]) ->
    % io:format("Using ~p~n", [TtyName]),
    % {ok, Tty} = file:open(TtyName, [read, append]),
    % Tty = no,
    gen_server:cast(?SERVERREF, {add_user, Username, ShellPid, self()}),
    io:format("Connected to server; awaiting messages.~n"),
    loop(Username, ShellPid).

stop_if_shell_exited(Username, ShellPid) ->
    case filelib:is_dir("/proc/" ++ ShellPid) of
        true ->
            ok;
        false ->
            gen_server:cast(?SERVERREF, {del_user, Username, ShellPid}),
            init:stop()
    end.

loop(Username, ShellPid) ->
    receive
        {message, From, Message} ->
        io:format("Got message from ~p: ~p~n", [From, Message]),
        % io:format("Your reply? Press enter, then Control-D when done.~n", []),
        Line = io:get_line("Your reply? (Press enter when done.):"),
        % io:format(Tty, "Got message from ~p: ~p~n", [From, Message]),
        % io:format(Tty, "Your reply? Press enter, then Control-D when done.~n", []),
        % Line = file:read_line(Tty),
        case Line of
            "\n" -> ok;
            _ -> 
                io:format("Sent message ~p~n", [Line])
                % io:format(Tty, "Sent message ~p~n", [Line])
                % TODO: send to server
        end;
        {invite, From, Game, JoinCmd} ->
            io:format("You were invited to ~p by ~p.~n", [From, Game]),
            io:format("To join, run '~p'", [JoinCmd])
    after
        ?WAIT_TIME_S * 1000 ->
            [] % do nothing
    end,
    stop_if_shell_exited(Username, ShellPid),
    loop(Username, ShellPid).