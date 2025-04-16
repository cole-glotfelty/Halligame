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
            clientPid = msg[1]
            self.__serverGameInstance.addClient(clientPid) # send them the client
        elif (msg[0] == "remove_client"):
            clientPid = msg[1]
            self.__connectedClients.remove(clientPid)
            self.__serverGameInstance.removeClient(clientPid)
        elif (msg[0] == "message"):
            clientPid = msg[1][0]
            message = msg[1][1]
            self.__serverGameInstance.gotClientMessage(clientPid, message)
        else:
            raise ValueError("ServerComms Received an unknown message: " + str(msg))

    # front end wrapper for sending a message with correct formatting
    def __sendMessage(self, ClientPid, Msg):
        print(f"DEBUG: Sending Message from ServerComms to Client {ClientPid}: {Msg}\n")

        node.send_nowait(sender = self.pid_,
                         receiver = ClientPid,
                         message = Msg)

    # State should have type halligame.utils.GameState
    def broadcastState(self, State : GameState):
        for ClientPid in self.__connectedClients:
            self.__sendMessage(ClientPid, ("state", State.serialize()))

    def confirmJoin(self, ClientPid, Message):
        self.__connectedClients.add(ClientPid)
        self.__sendMessage(ClientPid, ("confirmed_join", Message))


    def sendClientMessage(self, ClientPid, Message):
        self.__sendMessage(ClientPid, ("message", Message))

    def shutdown(self):
        node.destroy()




def start(game : str, node_name : str):
    global node
    node = Node(node_name, cookie = "Sh4rKM3ld0n")
    serverComms = ServerCommunicate(game, node_name)
    node.run()
