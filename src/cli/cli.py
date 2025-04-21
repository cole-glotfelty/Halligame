#!/usr/bin/env python
# cli.py is the command-line interface for Halligame.
# Created by:  Michael Daniels, 2025-04-14
# Last edited: Michael Daniels, 2025-04-19
import multiprocessing as mp
import os
import subprocess
from argparse import ArgumentParser, Namespace
from random import randint
from socket import gethostname

import psutil
from term import codec

import halligame.utils.ClientComms as ClientComms
import halligame.utils.ServerComms as ServerComms
from halligame.utils.common import ensure_epmd, whoami

#: The directory in which all games are contained.
GAMES_DIR: str = os.path.join(
    os.environ["HG_ROOT"], "src", "halligame", "games"
)

#: A list of all playable games.
GAMES: list[str] = list(
    filter(
        lambda elem: (
            os.path.isdir(os.path.join(GAMES_DIR, elem))
            and elem != "__pycache__"
            and elem != "ExampleGame"
        ),
        os.listdir(GAMES_DIR),
    )
)

#: The command to use for Erlang calls. The module handleCLIRequest is
#: pre-specified, but the function and any arguments need to be appended.
BASESCRIPT: list[str] = [
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


def join(args: Namespace) -> None:
    """Join the game with the ID stored in args.gameID, or print an error."""
    ensure_epmd()
    inputGameID = str(args.gameID).replace("-", "")

    cmd = BASESCRIPT.copy()
    cmd.append("lookupGameServerID")
    cmd.append(inputGameID)

    returned = subprocess.run(cmd, capture_output=True).stdout.splitlines()

    match returned:
        case [b"notfound"]:
            print(f"Error: game with id {inputGameID} not found.")
        case [gameName, nodeName, nodePid]:
            ClientComms.start(
                nodeName.decode(),
                gameName.decode(),
                codec.binary_to_term(nodePid[1:])[0],
            )
        case _:
            print(f"Error: cli got bad response from broker: {returned}")


def new(args: Namespace) -> None:
    """
    Create a new game of the game whose name is stored in args.game.
    Outputs the game ID to stdout.
    """
    ensure_epmd()
    hostname = gethostname()
    gameID = f"{randint(0, 999999):06d}"
    server_node_name = f"{gameID}@{hostname}"
    print("New game created.")
    print(f'Run "hg join {gameID[:3]}-{gameID[3:]}" to join!')

    thisProc = psutil.Process(os.getpid())
    parentPid = next(p for p in thisProc.parents() if p.name() != "uv")

    mp.Process(
        target=ServerComms.start,
        args=(args.game, server_node_name, parentPid.pid),
    ).start()


def listGames(_) -> None:
    """List on stdout all games that can be played."""
    print("Games available:")
    for game in GAMES:
        print("    * " + game)


def listActiveGames(_) -> None:
    """List on stdout all active game sessions."""
    cmd = BASESCRIPT.copy()
    cmd.append("listActiveGames")
    subprocess.run(cmd)


def listOnline(_) -> None:
    """List on stdout all online users' names."""
    cmd = BASESCRIPT.copy()
    cmd.append("listOnline")
    subprocess.run(cmd)


def write(args: Namespace) -> None:
    """
    Send a message to the user whose username is stored in args.username.
    Prints a prompt to stdout and gets input from stdin.
    """
    cmd = BASESCRIPT.copy()
    cmd.append("sendMessage")
    cmd.append(whoami())
    cmd.append(args.username)

    try:
        message = input("Enter your message here: ")
    except EOFError:
        message = ""

    if message != "":
        cmd.append(message)
        subprocess.run(cmd)


def invite(args: Namespace) -> None:
    """Invite a user to a game."""
    ensure_epmd()
    inputGameID = str(args.gameID).replace("-", "")

    cmd = BASESCRIPT.copy()
    cmd.append("lookupGameServerID")
    cmd.append(inputGameID)

    returned = subprocess.run(cmd, capture_output=True).stdout.splitlines()

    match returned:
        case [b"notfound"]:
            print(f"Error: game with id {inputGameID} not found.")
        case [gameName, nodeName, _nodePid]:
            sendInvite(args.username, gameName.decode(), nodeName.decode()[:6])
            print("Invited!")
        case _:
            print(f"Error: cli got bad response from broker: {returned}")


def sendInvite(toUser: str, gameName: str, gameID: str) -> None:
    """Helper function for invite(), actually sends the message.
    gameID is a string of six numbers."""
    cmd = BASESCRIPT.copy()
    cmd.append("sendInvite")
    cmd.append(whoami())
    cmd.append(toUser)
    cmd.append(gameName)
    cmd.append(f"hg join {gameID[:3]}-{gameID[3:]}")
    print(f"cmd = {cmd}")
    subprocess.run(cmd)


if __name__ == "__main__":
    mp.set_start_method("forkserver")
    parser = ArgumentParser(prog="hg")
    subparsers = parser.add_subparsers(dest="subcommand")

    join_parser = subparsers.add_parser("join", help="Join an existing game")
    join_parser.add_argument("gameID")
    join_parser.set_defaults(func=join)

    new_parser = subparsers.add_parser("new", help="Create a new game")
    new_parser.add_argument("game", choices=GAMES)
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

    invite_parser = subparsers.add_parser(
        "invite", help="Invite someone to a game"
    )
    invite_parser.add_argument("username")
    invite_parser.add_argument(
        "gameID", help="six digits, optionally dash-seperated"
    )
    invite_parser.set_defaults(func=invite)

    parsed = parser.parse_args()
    if not parsed.subcommand:
        parser.print_help()
        os._exit(1)
    else:
        parsed.func(parsed)
        os._exit(0)
