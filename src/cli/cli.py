#!/usr/bin/env python
from argparse import ArgumentParser
import halligame.utils.ClientComms as ClientComms
import halligame.utils.ServerComms as ServerComms
from random import randint
import subprocess
import socket
import psutil
import sys
import os

GAMES_DIR = os.path.join(os.environ['HG_ROOT'], "src", 
                                "halligame", "games")

GAMES = os.listdir(GAMES_DIR)
# TODO: fix - GAMES = filter(lambda elem: os.path.isdir(os.path.join(GAMES_DIR, elem)), GAMES)

SCRIPT = ['erl', '-noshell', '-sname', f'cli{randint(0, 999999):06d}',
                    '-setcookie', 'Sh4rKM3ld0n', '-eval', 'handleCLIRequest:']

def ensure_epmd():
    epmd_running = False
    for proc in psutil.process_iter(['pid', 'name']):
        if proc.info['name'] == 'epmd':
            epmd_running = True
            break
    if not epmd_running:
        subprocess.Popen(['epmd', '-daemon'])
    
def join(args) -> None:
    ensure_epmd()
    ClientComms.start(args.commServer, args.game)

def new(args) -> None:
    ensure_epmd()
    hostname = socket.gethostname()
    server_node_name = f'{randint(0, 999999):06d}@{hostname}'
    # cli = SCRIPT.copy()
    # cli[-1] += f'newGame(\'{args.game}\', \'{server_node_name}\').'
    # env = os.environ
    # env["ERL_LIBS"] = f"{env['HG_ROOT']}/src/cli/_build/default/lib"
    print(f"RoomName: {server_node_name}") # TODO: this is a hack because I cannot change the gameserver. Plz remove

    # subprocess.run(cli, stdout = sys.stdout, stderr = sys.stderr, env = env)
    try:
        ServerComms.start(args.game, server_node_name)
    except KeyboardInterrupt:
        exit(0)

def listGames(_) -> None:
    # TODO: insufficently pretty?
    print(GAMES)

def listActiveGames(_) -> None:
    cli = SCRIPT.copy()
    cli[-1] += 'listActiveGames()'
    subprocess.run(cli)

if __name__ == '__main__':
    parser = ArgumentParser()
    subparsers = parser.add_subparsers(required = True)

    join_parser = subparsers.add_parser('join')
    join_parser.add_argument('-g', '--game', choices=GAMES, required = True)
    join_parser.add_argument('-s', '--commServer', required = True)
    join_parser.set_defaults(func = join)

    new_parser = subparsers.add_parser('new')
    new_parser.add_argument('-g', '--game', choices = GAMES, required = True)
    new_parser.set_defaults(func = new)

    games_parser = subparsers.add_parser('games')
    games_parser.set_defaults(func = listGames)

    active_games_parser = subparsers.add_parser('availableGames')
    active_games_parser.set_defaults(func = listActiveGames)

    # the_args = parser.parse_args()    
    parsed = parser.parse_args()
    parsed.func(parsed)
