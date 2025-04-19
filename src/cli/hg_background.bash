#!/bin/bash
# Michael Daniels 2025-04-16

HGSHELLPROC=$(ps -o ppid= -p $$)
HGTTY=$(tty)

$HG_ROOT/.venv/bin/python $HG_ROOT/src/cli/background.py \
    "$HGSHELLPROC" "$HGTTY" &
disown