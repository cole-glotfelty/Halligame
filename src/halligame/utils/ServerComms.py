# ServerComms.py

# Communication from the game server to the communication server/client
# Written by: Will Cordray & Michael Daniels


# import importlib # allows us to import a module based on the name

import subprocess
from time import sleep
import argparse
import sys
import importlib

from pyrlang import Node
from pyrlang.process import Process
from pyrlang.gen.server import GenServerInterface

from term import Atom
from halligame.games import *
from halligame.utils.gameState import GameState
import socket
from random import randint
import os
 
class ServerCommunicate(Process):
    def __init__(self, gameName: str, NodeName: str):
        super().__init__()
        node.register_name(self, Atom("pyServer"))

        self.__connectedClients = set()

        gameModule = importlib.import_module("halligame.games." + gameName)
        self.__serverGameInstance = gameModule.Server(self)

    def handle_one_inbox_message(self, msg):
        print(f"DEBUG: ServerComms got message {msg}\n")
        if msg == "close":
            self.exit()
            exit(0)

        if (msg[0] == "new_client"):
            self.__serverGameInstance.addClient(msg[1]) # send them the client
        elif (msg[0] == "remove_client"):
            self.__connectedClients.remove(msg[1])
            self.__serverGameInstance.removeClient(msg[1])
        elif (msg[0] == "message"):
            # msg[1][0] = clientPid
            # msg[1][1] = Message
            self.__serverGameInstance.gotClientMessage(msg[1][0], msg[1][1])
        else:
            raise ValueError("Unknown Message ID in ServerComms: " + str(msg))

    # front end wrapper for sending a message with correct formatting
    def __sendMessage(self, ClientPid, Msg):
        print(f"DEBUG: Sending Message from ServerComms to Client {ClientPid}: {Msg}\n")

        node.send_nowait(sender = self.pid_,
                         receiver = ClientPid,
                         message = Msg)

    # State should have type halligame.utils.GameState
    def broadcastState(self, State : GameState):
        for ClientPid in self.__connectedClients:
            self.__sendMessage(ClientPid, (Atom("state"), State.serialize()))

    def confirmJoin(self, ClientPid, Message):
        self.__connectedClients.add(ClientPid)
        self.__sendMessage(ClientPid, (Atom("confirmed_join"), Message))

    def shutdown(self):
        node.destroy()

    def sendClientMessage(self, ClientPid, Message):
        self.__sendMessage(ClientPid, ("reply", Message))
        # node.send_nowait(sender = self.pid_,
        #                  receiver = ClientPid,
        #                  message = Message)

def start(game : str, node_name : str):
    global node
    node = Node(node_name, cookie = "Sh4rKM3ld0n")
    serverComms = ServerCommunicate(game, node_name)
    node.run()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-g", "--game", type=str, default="")
    parser.add_argument("-n", "--node_name", type=str, default="")
    args = parser.parse_args()

    if (args.game == ""):
        print("ERROR: No Game Supplied to ServerCommunicate", file=sys.stderr)
    elif (args.node_name == ""):
        print("ERROR: No Node Name Supplied", file=sys.stderr)
    else:
        start(args.game, args.node_name)
