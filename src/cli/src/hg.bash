#!/bin/bash

# this command is the prefix to run the erlang script that handles requests
export ERL_LIBS="$HG_ROOT/src/cli/_build/default/lib:$HG_ROOT/src/communicationServer/_build/default/lib:$ERL_LIBS"
script="erl -noshell -sname cli -setcookie COOKIE -eval \"handleCLIRequest:"

src="$HG_ROOT/src"

# no arguments supplied, or asking for help
if [ "$#" == 0 ] || [ "$1" == "--help" ] || [ "$1" == "-h" ]; then
    eval "${script}help()\""
    exit 1
fi


if [ "$1" == "join" ]; then
    if [ "$#" != 2 ]; then
        echo "Expected a gameID to join"
        exit 1
    fi
    pgrep epmd
    if [ $? -ne 0 ]; then
        epmd -daemon
    fi
    # eval "${script}joinGame($2)\""
    eval "uv --project ${src}/.. --directory ${src}/halligame run ${src}/halligame/utils/ClientComms.py $2"
elif [ "$1" == "new" ]; then
    if [ "$#" != 2 ]; then
        echo "Expected a game name to be supplied"
        eval "${script}listGames()\""
        exit 1
    fi
    pgrep epmd
    if [ $? -ne 0 ]; then
        epmd -daemon
    fi
    node_name="$(shuf -i 0-999999 -n 1)@$HOST"
    eval "${script}newGame('$2', '${node_name}')\""
    if [ "$?" == 0 ]; then
        eval "uv --project ${src}/.. --directory ${src}/halligame run ${src}/halligame/utils/ServerComms.py -g $2 -n ${node_name} &"
    fi
elif [ "$1" == "games" ]; then
    eval "${script}listGames()\""
elif [ "$1" == "availableGames" ]; then
    eval "${script}listActiveGames()\""
else # no invalid second argument
    echo "Argument \"$1\" not recognized"
    eval "${script}help()\""
fi
