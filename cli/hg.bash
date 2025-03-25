#!/bin/bash

# this command is the prefix to run the erlang script that handles requests
script="erl -noshell -sname cli -eval \"handleCLIRequest:"

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
    eval "${script}joinGame($2)\""
elif [ "$1" == "new" ]; then
    if [ "$#" != 2 ]; then
        echo "Expected a game name to be supplied"
        eval "${script}listGames()\""
        exit 1
    fi
    eval "${script}newGame($2)\""
elif [ "$1" == "games" ]; then
    eval "${script}listGames()\""
else # no invalid second argument
    echo "Argument \"$1\" not recognized"
    eval "${script}help()\""
fi
