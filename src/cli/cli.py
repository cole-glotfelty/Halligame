#!/usr/bin/env python
# cli.py is the command-line interface for Halligame.
# Created by:  Michael Daniels, 2025-04-14
# Last edited: Michael Daniels, 2025-04-19
import os
import subprocess
from argparse import ArgumentParser
from random import randint
from socket import gethostname

import halligame.utils.ClientComms as ClientComms
import halligame.utils.ServerComms as ServerComms
from halligame.utils.common import ensure_epmd, whoami

GAMES_DIR = os.path.join(os.environ["HG_ROOT"], "src", "halligame", "games")

GAMES = list(
    filter(
        lambda elem: (
            os.path.isdir(os.path.join(GAMES_DIR, elem))
            and elem != "__pycache__"
            and elem != "ExampleGame"
        ),
        os.listdir(GAMES_DIR),
    )
)

SCRIPT = [
    "env",
    f"ERL_LIBS={os.environ['HG_ROOT']}/src/cli/_build/default/lib",
    "erl",
    "-noshell",
    "-sname",
    f"cli{randint(0, 999999):06d}",
    "-setcookie",
    "Sh4rKM3ld0n",
    "-run",
    "handleCLIRequest",
]


def join(args) -> None:
    ensure_epmd()
    inputGameID = str(args.gameID).replace("-", "")

    cli = SCRIPT.copy()
    cli.append("lookupGameServerID")
    cli.append(inputGameID)

    gameAndNode = (
        subprocess.run(cli, capture_output=True).stdout.decode().splitlines()
    )
    if gameAndNode[0].strip() == "notfound":
        print(f"Error: game with id {inputGameID} not found.")
    else:
        gameName = gameAndNode[0]
        nodeName = gameAndNode[1]
        ClientComms.start(nodeName, gameName)


def new(args) -> None:
    ensure_epmd()
    hostname = gethostname()
    gameID = f"{randint(0, 999999):06d}"
    server_node_name = f"{gameID}@{hostname}"
    print(f"Game ID: {gameID[:3]}-{gameID[3:]}")
    try:
        ServerComms.start(args.game, server_node_name)
    except KeyboardInterrupt:
        exit(0)


def listGames(_) -> None:
    print("Games available:")
    for game in GAMES:
        print("    * " + game)


def listActiveGames(_) -> None:
    cli = SCRIPT.copy()
    cli.append("listActiveGames")
    subprocess.run(cli)


def listOnline(_) -> None:
    cli = SCRIPT.copy()
    cli.append("listOnline")
    subprocess.run(cli)


def write(args) -> None:
    cli = SCRIPT.copy()
    cli.append("sendMessage")
    cli.append(whoami())
    cli.append(args.username)

    try:
        message = input("Enter your message here: ")
    except EOFError:
        message = ""

    if message != "":
        cli.append(message)
        subprocess.run(cli)


if __name__ == "__main__":
    parser = ArgumentParser()
    subparsers = parser.add_subparsers(required=True)

    join_parser = subparsers.add_parser("join", help="Join an existing game")
    join_parser.add_argument("gameID")
    join_parser.set_defaults(func=join)

    new_parser = subparsers.add_parser("new", help="Create a new game")
    new_parser.add_argument("-g", "--game", choices=GAMES, required=True)
    new_parser.set_defaults(func=new)

    games_parser = subparsers.add_parser("games", help="List all games")
    games_parser.set_defaults(func=listGames)

    active_games_parser = subparsers.add_parser(
        "active", help="List all current game sessions"
    )
    active_games_parser.set_defaults(func=listActiveGames)

    online_parser = subparsers.add_parser(
        "online", help="List all currently online players"
    )
    online_parser.set_defaults(func=listOnline)

    write_parser = subparsers.add_parser("write", help="Write a user a message")
    write_parser.add_argument("username")
    write_parser.set_defaults(func=write)

    # the_args = parser.parse_args()
    parsed = parser.parse_args()
    parsed.func(parsed)
