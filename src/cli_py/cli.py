#!/usr/bin/env python
from argparse import ArgumentParser
import halligame.utils.ClientComms as ClientComms
import halligame.utils.ServerComms as ServerComms
from random import randint
import subprocess
import socket
import psutil

GAMES = ['TicTacToe']
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
    node_name = f'{randint(0, 999999):06d}@{hostname}'
    cli = SCRIPT.copy()
    cli[-1] += f'newGame(\'{args.game}\', \'{node_name}\')'
    subprocess.run(cli)
    ServerComms.start(args.game, node_name)

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
    parser.parse_args()
