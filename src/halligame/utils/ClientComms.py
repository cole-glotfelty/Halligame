# ClientComms.py

# Communication Module for clients to communicate with the game/communication
# server.
# Writen by: Will Cordray & Michael Daniels
# Last Edited by: Cole Glotfelty <2025-04-09>

# Changelog:
# Cole Glotfelty <2025-04-09> - Added documentation to functions

import importlib # allows us to import a module based on the name
import os
import sys
import asyncio

from pyrlang import Node
from pyrlang.process import Process
from pyrlang.gen.server import GenServerInterface
from term import Atom
from halligame.games import *
from random import randint
from time import sleep

from halligame.utils.screen import Screen

class ClientCommunicate(Process):
    # TODO: there are some serious shenanigans of imports going on here and I hate it
    def __init__(self, gameName, serverPid):
        super().__init__()
        node.register_name(self, f'pyClient')

        self.__serverPid = serverPid

        # "TicTacToe"
        #    - Import the tictactoe module
        #    - Call the init function of that tictactoe module
        gameModule = importlib.import_module("halligame.games." + gameName)
        self.__clientGameInstance = gameModule.Client(self)

        self.__backendSendMessage(("new_client", self.pid_))

    def handle_one_inbox_message(self, msg: tuple):
        """
        Await messages/check inbox of erlang/pyrlang node and call callback
        function when it receives a known message, other wise, informs the user
        there has been some sort of error and we've received an unknown message

        msg : message received
        """
        if msg == "close":
            exit(0)
        
        messageContents = msg[1]

        if (msg[0] == "state"):
            Message = msg[1]
            self.__clientGameInstance.updateState(messageContents)
        elif (msg[0] == "message"):
            self.__clientGameInstance.gotServerMessage(messageContents)
        elif (msg[0] == "confirmed_join"):
            self.__clientGameInstance.confirmedJoin(messageContents)
        else:
            raise ValueError("ClientComms Received an unknown message"  + str(msg))

    def sendMessage(self, Msg):
        """
        Given a message (msg) send it to the server

        msg : message to send
        """

        self.__backendSendMessage(("message", (self.pid_, Msg)))


    def __backendSendMessage(self, Msg):
        node.send_nowait(sender = self.pid_,
                         receiver = (Atom(self.__serverPid), Atom("pyServer")),
                         message = Msg)

    def shutdown(self):
        self.__backendSendMessage(("remove_client", self.pid_))
        sleep(0.5)
        node.destroy()
        sys.exit(0)




def start(commServerName, gameName):
    global name, node
    name = f'{randint(0, 999999) :06d}@{os.environ["HOST"]}'
    print("ClientComms NodeName: ", name)
    node = Node(node_name = name, cookie = "Sh4rKM3ld0n")
    clientComms = ClientCommunicate(gameName, commServerName)
    node.run()
