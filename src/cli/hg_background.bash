#!/bin/bash
# Michael Daniels 2025-04-16
# TODO: doesn't work in background, needs to be foreground

PARENT_SHELL_PID=$(ps -o ppid= -p $$)
THISUSER=$(whoami)
TTY=$(tty)
SNAME=$(shuf -i 0-999999 -n 1)
export ERL_LIBS="$HG_ROOT/src/user_background/_build/default/lib:$HG_ROOT/src/serverbroker/_build/default/lib:$ERL_LIBS"
# SERVER="{serverbroker, 'serverbroker@vm-projectweb3'}"
# PRE_CMD_1="erl -detached -sname $(shuf -i 0-999999 -n 1) -setcookie Sh4rKM3ld0n"
# PRE_CMD_2="-eval \"gen_server:cast($SERVER, {"
# PRE_CMD="$PRE_CMD_1 $PRE_CMD_2"
# POST_CMD=", \\\"$THISUSER\\\", \\\"$PARENT_SHELL_PID\\\"}), init:stop().\""

# mkfifo $HOME/.halligame-$SNAME-pipe
# cat > $HOME/.halligame-$SNAME-pipe &

erl -noshell -sname $SNAME -setcookie Sh4rKM3ld0n \
    -run user_background start "$THISUSER" "$PARENT_SHELL_PID"

# coproc erl -noshell -sname $SNAME -setcookie Sh4rKM3ld0n \
#     -run user_background start "$THISUSER" "$PARENT_SHELL_PID" "$TTY"

# disown