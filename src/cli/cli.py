#!/usr/bin/env python
import os
import socket
import subprocess
from argparse import ArgumentParser
from random import randint

import psutil

import halligame.utils.ClientComms as ClientComms
import halligame.utils.ServerComms as ServerComms

GAMES_DIR = os.path.join(os.environ["HG_ROOT"], "src", "halligame", "games")

GAMES = os.listdir(GAMES_DIR)
# TODO: fix
# GAMES=filter(lambda elem: os.path.isdir(os.path.join(GAMES_DIR, elem)), GAMES)

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


def ensure_epmd():
    epmd_running = False
    for proc in psutil.process_iter(["pid", "name"]):
        if proc.info["name"] == "epmd":
            epmd_running = True
            break
    if not epmd_running:
        subprocess.Popen(["epmd", "-daemon"])


def join(args) -> None:
    ensure_epmd()
    ClientComms.start(args.commServer, args.game)


def new(args) -> None:
    ensure_epmd()
    hostname = socket.gethostname()
    server_node_name = f"{randint(0, 999999):06d}@{hostname}"
    print(f"RoomName: {server_node_name}")
    try:
        ServerComms.start(args.game, server_node_name)
    except KeyboardInterrupt:
        exit(0)


def listGames(_) -> None:
    # TODO: insufficently pretty?
    print(GAMES)


def listActiveGames(_) -> None:
    cli = SCRIPT.copy()
    cli.append("listActiveGames")
    subprocess.run(cli)


def write(args) -> None:
    cli = SCRIPT.copy()
    cli.append("sendMessage")
    thisUser = subprocess.run(["whoami"], capture_output=True).stdout
    cli.append(thisUser.decode().strip())
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
    join_parser.add_argument("-g", "--game", choices=GAMES, required=True)
    join_parser.add_argument("-s", "--commServer", required=True)
    join_parser.set_defaults(func=join)

    new_parser = subparsers.add_parser("new", help="Create a new game")
    new_parser.add_argument("-g", "--game", choices=GAMES, required=True)
    new_parser.set_defaults(func=new)

    games_parser = subparsers.add_parser("games", help="List active games")
    games_parser.set_defaults(func=listGames)

    active_games_parser = subparsers.add_parser(
        "availableGames", help="List all playable games"
    )
    active_games_parser.set_defaults(func=listActiveGames)

    write_parser = subparsers.add_parser("write", help="Write a user a message")
    write_parser.add_argument("username")
    write_parser.set_defaults(func=write)

    # the_args = parser.parse_args()
    parsed = parser.parse_args()
    parsed.func(parsed)
