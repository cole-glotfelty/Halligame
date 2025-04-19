#!/bin/bash
# Michael Daniels 2025-04-16

export ERL_LIBS="$HG_ROOT/src/user_background/_build/default/lib:$HG_ROOT/src/serverbroker/_build/default/lib:$ERL_LIBS"

HGSHELLPROC=$(ps -o ppid= -p $$)
HGTTY=$(tty)

$HG_ROOT/.venv/bin/python $HG_ROOT/src/cli/background.py "$HGSHELLPROC" "$HGTTY" &
disown