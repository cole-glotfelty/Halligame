#!/bin/bash
# Michael Daniels 2025-04-16

PARENT_SHELL_PID=$(ps -o ppid= -p $$)
THISUSER=$(whoami)

SERVER="{serverbroker, 'serverbroker@vm-projectweb3'}"
PRE_CMD_1="erl -detached -sname $(shuf -i 0-999999 -n 1) -setcookie Sh4rKM3ld0n"
PRE_CMD_2="-eval \"gen_server:cast($SERVER, {"
PRE_CMD="$PRE_CMD_1 $PRE_CMD_2"
POST_CMD=", \\\"$THISUSER\\\", \\\"$PARENT_SHELL_PID\\\"}), init:stop().\""

eval "$PRE_CMD""add_user""$POST_CMD"

(
while true; do
    if [ ! -d /proc/$PARENT_SHELL_PID ]; then
        eval "$PRE_CMD""del_user""$POST_CMD"

        # echo "Remove client" | tee -a "/h/mdanie09/test.log"
        # sleep 10
        break
    else
        # echo "Parent still exists" | tee -a "/h/mdanie09/test.log"
        sleep 15
    fi
done
) &
disown