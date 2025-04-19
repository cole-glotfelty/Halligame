#!/bin/bash
# Michael Daniels 2025-04-16

export ERL_LIBS="$HG_ROOT/src/user_background/_build/default/lib:$HG_ROOT/src/serverbroker/_build/default/lib:$ERL_LIBS"

$HG_ROOT/bin/python $HG_ROOT/src/cli/background.py $(ps -o ppid= -p $$) $(tty) &
disown