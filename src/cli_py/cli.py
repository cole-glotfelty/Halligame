# import argparse
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
    for proc in psutil.process_iter(['pid', 'name', 'username']):
        if proc.info['username'] == 
    
def join(args) -> None:
    # TODO: pgrep epmd, start it if not.
    ClientComms.start(args.commServer, args.game)

def new(args) -> None:
    # TODO: pgrep epmd, start it if not.
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

if __name__ == 'main':
    parser = ArgumentParser()
    subparsers = parser.add_subparsers(required=True)
    join_parser = subparsers.add_parser('join', aliases = ['j'])
    join_parser.add_argument('-g', '--game', choices=GAMES)
    join_parser.add_argument('-s', '--commServer')
    join_parser.set_defaults(func = join)

    new_parser = subparsers.add_parser('new', aliases = ['n'])
    new_parser.add_argument('-g', '--game', choices = GAMES)
    new_parser.add_argument('n')
    new_parser.set_defaults(func = new)

    games_parser = subparsers.add_parser('games', aliases = ['list-games'])
    games_parser.set_defaults(func = listGames)

    active_games_parser = subparsers.add_parser('availableGames', aliases = 'activeGames')
    active_games_parser.set_defaults(func = listActiveGames)

    args = parser.parse_args()

    