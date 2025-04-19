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
 
class ServerCommunicate(Process):
    def __init__(self, gameName: str, NodeName: str):
        super().__init__()
        node.register_name(self, Atom("pyServer"))
        self.__gameName = gameName

        self.__connectedClients = set()

        gameModule = importlib.import_module("halligame.games." + gameName)
        self.__serverGameInstance = gameModule.Server(self)
        # Pyrlang has node name and registered name backwards
        self.__sendMessage((Atom("serverbroker@vm-projectweb3"), 
                            Atom("serverbroker")),
                           (Atom("getBrokerPid"), self.pid_))
        self.__serverBroker = None

    def handle_one_inbox_message(self, msg):
        print(f"DEBUG: ServerComms got message {msg}\n")
        if msg == "close":
            self.shutdown()
            exit(0)
        elif msg[0] == Atom("brokerPid"):
            self.__serverBroker = GenServerInterface(self, msg[1])
            self.__serverBroker.cast_nowait((Atom("register_gameserver"),
                                             self.__gameName, self.pid_))
        elif (msg[0] == "new_client"):
            clientPid = msg[1]
            self.__serverGameInstance.addClient(clientPid) # send them the client
        elif (msg[0] == "remove_client"):
            ClientPid = msg[1]
            self.__sendMessage(ClientPid, ("quit_confirm", self.pid_))

            self.__connectedClients.remove(ClientPid)
            self.__serverGameInstance.removeClient(ClientPid)
        elif (msg[0] == "message"):
            clientPid = msg[1][0]
            message = msg[1][1]
            self.__serverGameInstance.gotClientMessage(clientPid, message)
        else:
            raise ValueError("ServerComms Received an unknown message: " + str(msg))

    # wrapper for sending a message with correct formatting
    def __sendMessage(self, dest, msg):
        # print(f"DEBUG: Sending Message from ServerComms to {dest}: {msg}")
        node.send_nowait(sender = self.pid_,
                         receiver = dest,
                         message = msg)

    # State should have type halligame.utils.GameState
    def broadcastState(self, State : GameState):
        for ClientPid in self.__connectedClients:
            self.__sendMessage(ClientPid, ("state", State.serialize()))

    def broadcastMessage(self, Message):
        for ClientPid in self.__connectedClients:
            self.sendClientMessage(ClientPid, Message)

    def confirmJoin(self, ClientPid, Message):
        self.__connectedClients.add(ClientPid)
        self.__sendMessage(ClientPid, ("confirmed_join", Message))


    def sendClientMessage(self, ClientPid, Message):
        self.__sendMessage(ClientPid, ("message", Message))

    def shutdown(self):
        self.__serverBroker.cast_nowait((Atom("unregister_gameserver"),
                                         self.pid_))
        node.destroy()





def start(game : str, node_name : str):
    global node
    node = Node(node_name, cookie = "Sh4rKM3ld0n")
    serverComms = ServerCommunicate(game, node_name)
    node.run()
